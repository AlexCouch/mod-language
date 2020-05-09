use crate::{
  // util::{ UnwrapUnchecked, },
  some,
  common::{ Identifier, Number, Operator, },
  source::{ SourceRegion, },
  ast::{ self, Item, ItemData, TypeExpression, TypeExpressionData, Path, ExportData, },
  // ast::{ Item, ItemData, ExportData, Path, TypeExpression, TypeExpressionData, Expression, ExpressionData, Block, Conditional, },
  ctx::{ Module, Global, Function, GlobalItem, GlobalKey, Type, TypeData, PrimitiveType, CoercibleType, LocalItem,  MultiKey, TypeDisplay, },
  ir,
};

use super::{ Analyzer, };


enum AliasData {
  Import { absolute: bool, relative_to: GlobalKey, chain: Vec<Identifier>, },
  Export { base: Identifier, },
}

struct Alias {
  destination_module: GlobalKey,
  data: AliasData,
  new_name: Identifier,
  origin: SourceRegion,
}

/// Binds top level items to their identifiers
#[inline]
fn pass_bind_top_level (analyzer: &mut Analyzer, items: &[Item], aliases: &mut Vec<Alias>) {
  fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item, aliases: &mut Vec<Alias>) -> (&'a Identifier, GlobalKey) {
    match &item.data {
      ItemData::Module { identifier, items, .. } => {
        let new_mod = analyzer.create_item(
          identifier.to_owned(),
          Module::new(
            Some(analyzer.get_active_module_key()),
            identifier.to_owned(),
            item.origin
          ),
          item.origin
        );

        analyzer.push_active_module(new_mod);

        pass_bind_top_level(analyzer, items, aliases);

        analyzer.pop_active_module();

        (identifier, new_mod)
      },

      ItemData::Global { identifier, .. } => (identifier, analyzer.create_item(
        identifier.to_owned(),
        Global::new(
          identifier.to_owned(),
          item.origin,
          None
        ),
        item.origin
      )),

      ItemData::Function { identifier, .. } => (identifier, analyzer.create_item(
        identifier.to_owned(),
        Function::new(
          identifier.to_owned(),
          item.origin,
          None
        ),
        item.origin
      )),

      | ItemData::Import { .. }
      | ItemData::Export { .. }
      => unreachable!("Internal error, export node contains invalid descendent")
    }
  }


  for item in items.iter() {
    match &item.data {
      ItemData::Import { data, .. } => {
        let destination_module = analyzer.get_active_module_key();

        for (base, new_name) in data.iter() {
          let new_name = if let Some(new_name) = new_name { new_name } else { base.last().expect("Internal error, empty import path with no alias") }.to_owned();
          
          let relative_to = if base.absolute { analyzer.context.lib_mod } else { destination_module };

          aliases.push(Alias {
            destination_module,
            data: AliasData::Import {
              absolute: base.absolute,
              relative_to,
              chain: base.chain.clone()
            },
            new_name,
            origin: item.origin,
          })
        }
      },

      ItemData::Export { data, .. } => {
        match data {
          ExportData::List(exports) => {
            let destination_module = analyzer.get_active_module_key();

            for (base, new_name) in exports.iter() {
              let new_name = if let Some(new_name) = new_name { new_name } else { base }.to_owned();
              
              aliases.push(Alias {
                destination_module,
                data: AliasData::Export { base: base.clone() },
                new_name,
                origin: item.origin,
              })
            }
          },

          ExportData::Inline(item) => {
            let (identifier, key) = bind_item(analyzer, item, aliases);

            analyzer.get_active_module_mut().export_bindings.set_entry_bound(identifier.to_owned(), key, item.origin);
          },
        }
      },

      | ItemData::Module   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => {
        bind_item(analyzer, item, aliases);
      }
    }
  }
}



fn pass_resolve_aliases (analyzer: &mut Analyzer, aliases: &mut Vec<Alias>) {
  fn resolve_alias (analyzer: &mut Analyzer, aliases: &mut Vec<Alias>, alias: Alias) -> Option<GlobalKey> {
    fn try_get_alias (aliases: &mut Vec<Alias>, in_module: GlobalKey, find_import: bool, identifier: &Identifier) -> Option<Alias> {
      for (index, alias) in aliases.iter().enumerate() {
        if alias.destination_module == in_module
        && (( find_import && matches!(alias.data, AliasData::Import { .. }))
        || (!find_import && matches!(alias.data, AliasData::Export { .. })))
        && &alias.new_name == identifier {
          return Some(aliases.remove(index))
        }
      }

      None
    }

    match alias.data {
      AliasData::Import { absolute, relative_to, chain } => {
        let dest_mod =
          analyzer.context.items
            .get(alias.destination_module)
            .expect("Internal error, alias has invalid destination module key")
            .ref_module()
            .expect("Internal error, alias destination key does not resolve to a module");
        
        if let Some(existing_key) = dest_mod.local_bindings.get_entry(&alias.new_name) {
          let existing_origin = 
            dest_mod.local_bindings
              .get_bind_location(existing_key)
              .expect("Internal error, local item has no binding source location");

          analyzer.error(alias.origin, format!(
            "Module import `{}` shadows an existing item, defined at [{}]",
            alias.new_name,
            existing_origin,
          ))
        }


        let mut base_name = Identifier::default();
                
        let mut resolved_key = relative_to;
        
        for ident in chain.iter() {
          let base = analyzer.context.items.get(resolved_key).expect("Internal error, invalid lowered key during import alias resolution");

          if let GlobalItem::Module(module) = base {
            base_name.set(&module.canonical_name);

            resolved_key = if !absolute && resolved_key == relative_to {
              if let Some(local) = module.local_bindings.get_entry(ident) {
                local
              } else if let Some(alias) = try_get_alias(aliases, resolved_key, true, ident) {
                // if this fails there has already been an error message and we can just bail
                // TODO should unresolved imports import an error item? (probably)
                resolve_alias(analyzer, aliases, alias)?
              } else if let Some(core) = analyzer.context.core_ns.get_entry(ident) {
                core
              } else {
                analyzer.error(alias.origin, format!("Module `{}` does not have access to an item named `{}`", base_name, ident));
                return None
              }
            } else if let Some(exported_key) = module.export_bindings.get_entry(ident) {
              exported_key
            } else if let Some(alias) = try_get_alias(aliases, resolved_key, false, ident) {
              // if this fails there has already been an error message and we can just bail
              // TODO should unresolved imports import an error item? (probably)
              resolve_alias(analyzer, aliases, alias)?
            } else {
              analyzer.error(alias.origin, format!("Module `{}` does not export an item named `{}`", base_name, ident));
              return None
            };
          } else {
            analyzer.error(alias.origin, format!("{} is not a Module and has no exports", ident));
            return None
          }
        }

        let dest_mod_mut = unsafe { analyzer.context.items.get_unchecked_mut(alias.destination_module).mut_module_unchecked() };

        dest_mod_mut.local_bindings.set_entry_bound(alias.new_name, resolved_key, alias.origin);

        Some(resolved_key)
      },
  
      AliasData::Export { base } => {
        let dest_mod =
          analyzer.context.items
            .get(alias.destination_module)
            .expect("Internal error, alias has invalid destination module key")
            .ref_module()
            .expect("Internal error, alias destination key does not resolve to a module");
        
        if let Some(existing_key) = dest_mod.export_bindings.get_entry(&alias.new_name) {
          let existing_origin = 
            dest_mod.export_bindings
              .get_bind_location(existing_key)
              .expect("Internal error, exported item has no binding source location");

          analyzer.error(alias.origin, format!(
            "Module export `{}` shadows an existing export, defined at [{}]",
            alias.new_name,
            existing_origin,
          ))
        }

        let base_key = if let Some(local_key) = dest_mod.local_bindings.get_entry(&base) {
          local_key
        } else if let Some(alias) = try_get_alias(aliases, alias.destination_module, true, &base) {
          // if this fails there has already been an error message and we can just bail
          // TODO should unresolved exports export an error item? (probably)
          resolve_alias(analyzer, aliases, alias)?
        } else if let Some(core_key) = analyzer.context.core_ns.get_entry(&base) {
          core_key
        } else {
          analyzer.error(alias.origin, format!("Cannot export undefined identifier `{}`", base));
          return None
        };

        let dest_mod_mut = unsafe { analyzer.context.items.get_unchecked_mut(alias.destination_module).mut_module_unchecked() };

        dest_mod_mut.export_bindings.set_entry_bound(alias.new_name, base_key, alias.origin);

        Some(base_key)
      },
    }
  }

  while let Some(alias) = aliases.pop() {
    resolve_alias(analyzer, aliases, alias);
  }
}


fn evaluate_global_ident (analyzer: &Analyzer, identifier: &Identifier, origin: SourceRegion) -> Option<GlobalKey> {
  let module = analyzer.get_active_module();

  Some(if let Some(local) = module.local_bindings.get_entry(identifier) {
    local
  } else if let Some(core) = analyzer.context.core_ns.get_entry(identifier) {
    core
  } else {
    analyzer.error(origin, format!("Module `{}` does not have access to an item named `{}`", module.canonical_name, identifier));
    return None
  })
}

fn evaluate_path (analyzer: &mut Analyzer, path: &Path, origin: SourceRegion) -> Option<GlobalKey> {
  let mut base_name = Identifier::default();
                
  let base_key = if path.absolute {
    analyzer.context.lib_mod
  } else {
    analyzer.get_active_module_key()
  };

  let mut resolved_key = base_key;
  
  for ident in path.iter() {
    let base = analyzer.context.items.get(resolved_key).expect("Internal error, invalid lowered key during path resolution");

    if let GlobalItem::Module(module) = base {
      base_name.set(&module.canonical_name);

      resolved_key = if !path.absolute && resolved_key == base_key {
        if let Some(local) = module.local_bindings.get_entry(ident) {
          local
        } else if let Some(core) = analyzer.context.core_ns.get_entry(ident) {
          core
        } else {
          analyzer.error(origin, format!("Module `{}` does not have access to an item named `{}`", base_name, ident));
          return None
        }
      } else if let Some(exported_key) = module.export_bindings.get_entry(ident) {
        exported_key
      } else {
        analyzer.error(origin, format!("Module `{}` does not export an item named `{}`", base_name, ident));
        return None
      };
    } else {
      analyzer.error(origin, format!("{} is not a Module and has no exports", ident));
      return None
    }
  }

  Some(resolved_key)
}


macro_rules! bubble_err_ty {
  ($analyzer:expr, $key:expr) => {
    if $key != $analyzer.context.err_ty { $key } else { $key }
  };
}

fn evaluate_anon_tdata (analyzer: &mut Analyzer, type_data: TypeData, origin: SourceRegion) -> GlobalKey {
  assert!(type_data.is_anon(), "Internal error, non-anonymous TypeData passed to evaluate_anon_tdata");

  if let Some(existing_key) = analyzer.context.anon_types.get(&type_data) {
    *existing_key
  } else {
    let new_key = analyzer.context.items.insert(Type::new(None, origin, Some(type_data.clone())).into());
    analyzer.context.anon_types.insert(type_data, new_key);
    new_key
  }
}


fn evaluate_texpr (analyzer: &mut Analyzer, texpr: &TypeExpression) -> GlobalKey {
  let err_ty = analyzer.context.err_ty;

  match &texpr.data {
    TypeExpressionData::Path(path) => {
      let key = some!(evaluate_path(analyzer, path, texpr.origin); err_ty);
      
      let item = analyzer.context.items.get(key).expect("Internal error, path evaluated to invalid key");

      if item.ref_type().is_some() {
        key
      } else {
        analyzer.error(texpr.origin, format!("Path `{}` does not evaluate to a type", path));

        err_ty
      }
    },

    TypeExpressionData::Identifier(identifier) => {
      let key = some!(evaluate_global_ident(analyzer, identifier, texpr.origin); err_ty);

      let item = analyzer.context.items.get(key).expect("Internal error, identifier evaluated to invalid key");

      if item.ref_type().is_some() {
        key
      } else {
        analyzer.error(texpr.origin, format!("Identifier `{}` does not evaluate to a type", identifier));

        err_ty
      }
    },

    TypeExpressionData::Function { parameter_types: parameter_texprs, return_type: return_texpr } => {
      let mut parameter_types = Vec::with_capacity(parameter_texprs.len());

      for texpr in parameter_texprs.iter() {
        parameter_types.push(bubble_err_ty!(analyzer, evaluate_texpr(analyzer, texpr)));
      }

      let return_type = if let box Some(return_texpr) = return_texpr { Some(bubble_err_ty!(analyzer, evaluate_texpr(analyzer, return_texpr))) } else { None };

      let fn_td = TypeData::Function { parameter_types, return_type };

      evaluate_anon_tdata(analyzer, fn_td, texpr.origin)
    },
  }
}


/// Binds top level globals and functions to their types
fn pass_type_link_top_level (analyzer: &mut Analyzer, items: &[Item]) {
  fn type_link_item (analyzer: &mut Analyzer, item: &Item) {
    match &item.data {
      ItemData::Module { identifier, items, .. } => {
        analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
        pass_type_link_top_level(analyzer, items);
        analyzer.pop_active_module();
      },

      ItemData::Global { identifier, explicit_type, .. } => {
        let global_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

        // its possible some shadowing error has overwritten this def and if so we just return
        some!(analyzer.context.items.get(global_key).unwrap().ref_global());

        let ty = evaluate_texpr(analyzer, explicit_type);

        unsafe { analyzer.context.items.get_unchecked_mut(global_key).mut_global_unchecked() }.ty.replace(ty);
      },

      ItemData::Function { identifier, parameters, return_type, .. } => {
        let function_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

        // its possible some shadowing error has overwritten this def and if so we just return
        some!(analyzer.context.items.get(function_key).unwrap().ref_function());

        let parameter_types = parameters.iter().map(|(_, texpr)| texpr.clone()).collect();
        let return_type = box return_type.as_ref().cloned();

        let fn_texpr = TypeExpression::new(TypeExpressionData::Function { parameter_types, return_type }, item.origin);

        let ty = evaluate_texpr(analyzer, &fn_texpr);

        unsafe { analyzer.context.items.get_unchecked_mut(function_key).mut_function_unchecked() }.ty.replace(ty);
      }
      
      | ItemData::Import { .. }
      | ItemData::Export { .. }
      => unreachable!()
    }
  }
  
  for item in items.iter() {
    match &item.data {
      | ItemData::Import { .. }
      | ItemData::Export { data: ExportData::List(_), .. } 
      => continue,

      ItemData::Export { data: ExportData::Inline(item), .. } => type_link_item(analyzer, item),

      | ItemData::Module   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => type_link_item(analyzer, item)
    }
  }
}


fn evaluate_local_ident (analyzer: &mut Analyzer, identifier: &Identifier, origin: SourceRegion) -> Option<MultiKey> {
  Some(if let Some(local) = analyzer.get_active_local_context().get_variable(identifier) {
    local
  } else {
    let module = analyzer.get_active_module();

    if let Some(global) = module.local_bindings.get_entry(identifier) {
      global
    } else if let Some(core) = analyzer.context.core_ns.get_entry(identifier) {
      core
    } else {
      analyzer.error(origin, format!("Cannot find item or local variable named `{}`", identifier));
      return None
    }.into()
  })
}


fn extract_global_item_ty (global_item: &GlobalItem) -> Option<GlobalKey> {
  match global_item {
    | &GlobalItem::Global(Global { ty, .. })
    | &GlobalItem::Function(Function { ty, .. })
    => ty,

    | GlobalItem::Module { .. }
    | GlobalItem::Type   { .. }
    => None
  }
}


fn ty_resolve_alias (analyzer: &mut Analyzer, mut alias: GlobalKey) -> Option<GlobalKey> {
  loop {
    let ty_data =
      &analyzer.context.items
        .get(alias)
        .expect("Internal error, type alias contained invalid key")
        .ref_type()
        .expect("Internal error, type alias does not reference a type")
        .data;
    
    match ty_data {
      Some(TypeData::Alias(tk)) => { alias = *tk },

      | Some(TypeData::Error)
      | None
      => break None,

      _ => break Some(alias)
    }
  }
}

fn ty_from_unary (analyzer: &mut Analyzer, operand_tk: GlobalKey, operator: Operator, origin: SourceRegion) -> Option<GlobalKey> {
  let operand_tk = ty_resolve_alias(analyzer, operand_tk)?;

  let operand_ty = analyzer.context.items.get(operand_tk).unwrap().ref_type().unwrap();

  let operand_td = operand_ty.data.as_ref()?;
  
  Some(match operand_td {
    TypeData::Pointer(tk) if operator == Operator::Dereference => *tk,
    
    | TypeData::Function { .. }
    | TypeData::Pointer  { .. }
    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Coercible(CoercibleType::FloatingPoint)
    | TypeData::Primitive(PrimitiveType::Bool)
    | TypeData::Primitive(PrimitiveType::Integer { .. })
    | TypeData::Primitive(PrimitiveType::FloatingPoint { .. })

    if operator == Operator::AddressOf
    => evaluate_anon_tdata(analyzer, TypeData::Pointer(operand_tk), origin),


    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Primitive(PrimitiveType::Bool)
    | TypeData::Primitive(PrimitiveType::Integer { .. })

    if matches!(operator, Operator::Not)
    => operand_tk,


    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Coercible(CoercibleType::FloatingPoint)
    | TypeData::Primitive(PrimitiveType::Integer { .. })
    | TypeData::Primitive(PrimitiveType::FloatingPoint { .. })

    if matches!(operator, Operator::Sub | Operator::Add)
    => operand_tk,
    
    
    TypeData::Error { .. } => return None,

    _ => {
      analyzer.error(origin, format!(
        "This expression type (`{}`) is not compatible with unary operator `{}`",
        TypeDisplay { ty_key: operand_tk, context: &analyzer.context },
        operator.value()
      ));
      return None
    }
  })
}

fn ty_production_unary (analyzer: &mut Analyzer, expr: &ast::Expression) -> Option<ir::Expression> {
  let (operand_expr, operator) =
    if let ast::ExpressionData::Unary { box operand, operator } = &expr.data { (operand, *operator) }
    else { panic!("Internal error, unary type production function called on non-unary expression") };

  let operand_ir = evaluate_expr(analyzer, operand_expr)?;

  let result_ty = ty_from_unary(analyzer, operand_ir.ty, operator, expr.origin)?;

  Some(ir::Expression::new(ir::ExpressionData::Unary { operand: box operand_ir, operator }, result_ty, expr.origin))
}

fn ty_will_coerce (analyzer: &mut Analyzer, allow_int_to_ptr: bool, from_tk: GlobalKey, into_tk: GlobalKey) -> bool {
  let from_tk = some!(ty_resolve_alias(analyzer, from_tk); false);
  let into_tk = some!(ty_resolve_alias(analyzer, into_tk); false);

  if from_tk == into_tk { return true }
  
  let from_ty = analyzer.context.items.get(from_tk).unwrap().ref_type().unwrap();
  let into_ty = analyzer.context.items.get(into_tk).unwrap().ref_type().unwrap();

  
  if let (Some(from_td), Some(into_td)) = (&from_ty.data, &into_ty.data) {
    match (from_td, into_td) {
      | (TypeData::Primitive(PrimitiveType::Integer { signed: true, bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::Integer { signed: true, bit_size: into_size }))

      | (TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: into_size }))

      | (TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: into_size }))

      if from_size <= into_size
      => true,


      | (TypeData::Coercible(CoercibleType::FloatingPoint)
      ,  TypeData::Primitive(PrimitiveType::FloatingPoint { .. }))

      | (TypeData::Coercible(CoercibleType::Integer)
      ,  TypeData::Primitive(PrimitiveType::Integer { .. }))

      => true,
      

      | (TypeData::Coercible(CoercibleType::Integer)
      , TypeData::Pointer(_))

      | (TypeData::Primitive(PrimitiveType::Integer { .. })
      ,  TypeData::Pointer(_))

      if allow_int_to_ptr
      => true,


      | (&TypeData::Pointer(from_p)
      ,  &TypeData::Pointer(into_p))

      if from_p == analyzer.context.void_ty
      => true,


      _ => false
    }
  } else {
    false
  }
}

fn ty_meet (analyzer: &mut Analyzer, allow_int_to_ptr: bool, a_tk: GlobalKey, b_tk: GlobalKey) -> Option<GlobalKey> {
  if ty_will_coerce(analyzer, allow_int_to_ptr, a_tk, b_tk) { Some(b_tk) }
  else if ty_will_coerce(analyzer, allow_int_to_ptr, b_tk, a_tk) { Some(a_tk) }
  else { None }
}

fn ty_from_binary (analyzer: &mut Analyzer, operand_tk: GlobalKey, operator: Operator, origin: SourceRegion) -> Option<GlobalKey> {
  let operand_ty = analyzer.context.items.get(operand_tk).unwrap().ref_type().unwrap();

  let operand_td = operand_ty.data.as_ref()?;

  Some(match (operand_td, operator) {
    | (TypeData::Primitive(PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::And | Operator::Or | Operator::Xor | Operator::Add | Operator::Sub | Operator::Mul | Operator::Div | Operator::Rem)   

    => operand_tk,
    
    
    | (TypeData::Primitive(PrimitiveType::Bool)
    ,  Operator::And | Operator::Or | Operator::Xor)

    | (TypeData::Primitive(PrimitiveType::Bool | PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::Equal | Operator::NotEqual)

    | (TypeData::Primitive(PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::Lesser | Operator::Greater | Operator::LesserOrEqual | Operator::GreaterOrEqual)

    => analyzer.context.bool_ty,


    _ => {
      analyzer.error(origin, format!(
        "The operand type of this binary expression (`{}`) does not support the operator `{}`",
        TypeDisplay { ty_key: operand_tk,  context: &analyzer.context },
        operator.value()
      ));

      return None
    }
  })
}

fn ty_production_binary (analyzer: &mut Analyzer, expr: &ast::Expression) -> Option<ir::Expression> {
  let (left_expr, right_expr, operator) =
    if let ast::ExpressionData::Binary { box left, box right, operator } = &expr.data { (left, right, *operator) }
    else { panic!("Internal error, binary type production function called on non-binary expression") };

  let irs = (evaluate_expr(analyzer, left_expr), evaluate_expr(analyzer, right_expr));

  let (mut left_ir, mut right_ir) = (irs.0?, irs.1?);
  
  let left_tk = ty_resolve_alias(analyzer, left_ir.ty)?;
  let right_tk = ty_resolve_alias(analyzer, right_ir.ty)?;

  let operand_tk =
    if let Some(tk) = ty_meet(analyzer, true, left_ir.ty, right_ir.ty) { tk }
    else {
      analyzer.error(expr.origin, format!(
        "The types of the subexpressions for this binary operator (left: `{}`, right: `{}`), \
         are not equal and cannot coerce to the same type",
         TypeDisplay { ty_key: left_tk,  context: &analyzer.context },
         TypeDisplay { ty_key: right_tk, context: &analyzer.context },
      ));

      return None
    };

  if left_tk != operand_tk {
    let origin = left_ir.origin;
    left_ir = ir::Expression::new(ir::ExpressionData::Coerce(box left_ir), operand_tk, origin)
  } else if right_tk != operand_tk {
    let origin = right_ir.origin;
    right_ir = ir::Expression::new(ir::ExpressionData::Coerce(box right_ir), operand_tk, origin)
  }

  let result_tk = ty_from_binary(analyzer, operand_tk, operator, expr.origin)?;

  Some(ir::Expression::new(ir::ExpressionData::Binary { left: box left_ir, right: box right_ir, operator }, result_tk, expr.origin))
}

/// Peforms type evaluation for expressions
fn evaluate_expr (analyzer: &mut Analyzer, expr: &ast::Expression) -> Option<ir::Expression> {
  match &expr.data {
    ast::ExpressionData::Path(path) => {
      let key = evaluate_path(analyzer, path, expr.origin)?;

      let item = analyzer.context.items.get(key).unwrap();

      let ty = extract_global_item_ty(item);

      if let Some(ty) = ty {
        Some(ir::Expression::new(ir::ExpressionData::Reference(key.into()), ty, expr.origin))
      } else {
        analyzer.error(expr.origin, "Path does not evaluate to a value".to_owned());

        None
      }
    },

    ast::ExpressionData::Identifier(ident) => {
      let multi_key = evaluate_local_ident(analyzer, ident, expr.origin)?;
      
      match multi_key {
        MultiKey::LocalKey(local_key) => {
          let local: &LocalItem = analyzer.get_active_local_context().variables.get(local_key).unwrap();

          Some(ir::Expression::new(ir::ExpressionData::Reference(multi_key), local.ty, expr.origin))
        },

        MultiKey::GlobalKey(global_key) => {
          let global: &GlobalItem = analyzer.context.items.get(global_key).unwrap();

          let ty = extract_global_item_ty(global);

          if let Some(ty) = ty {
            Some(ir::Expression::new(ir::ExpressionData::Reference(multi_key), ty, expr.origin))
          } else {
            analyzer.error(expr.origin, "Path does not evaluate to a value".to_owned());

            None
          }
        },
      }
    },

    &ast::ExpressionData::Number(number) => Some(ir::Expression::new(
      ir::ExpressionData::Number(number),
      match number {
        Number::Integer(_) => analyzer.context.int_ty,
        Number::FloatingPoint(_) => analyzer.context.float_ty,
      },
      expr.origin
    )),
  
    ast::ExpressionData::Unary { .. } => ty_production_unary(analyzer, expr),

    ast::ExpressionData::Binary { .. } => ty_production_binary(analyzer, expr),

    ast::ExpressionData::Call { box callee, arguments } => {
      let argument_irs: Vec<_> = arguments.iter().map(|arg| (evaluate_expr(analyzer, arg), arg.origin)).collect();
      
      let callee_ir = evaluate_expr(analyzer, callee)?;
      
      let callee_ty: &Type = analyzer.context.items.get(callee_ir.ty).unwrap().ref_type().unwrap();
      
      let callee_ty_data = callee_ty.data.as_ref().unwrap();

      let num_args = argument_irs.len();

      match callee_ty_data {
        TypeData::Function { parameter_types, return_type } => {
          let num_params = parameter_types.len();

          if num_params == num_args {
            let iter = argument_irs.into_iter().enumerate();
            let mut argument_irs = Some(Vec::with_capacity(num_args));

            for (i, (arg_ir, arg_origin)) in iter {
              if let Some(arg_ir) = arg_ir {
                let param_ty = unsafe { *parameter_types.get_unchecked(i) };

                if arg_ir.ty != param_ty {
                  analyzer.error(arg_origin, format!(
                    "Parameter {} is of type `{}`, but the argument provided is of type `{}`",
                    i,
                    TypeDisplay { ty_key: param_ty, context: &analyzer.context },
                    TypeDisplay { ty_key: arg_ir.ty, context: &analyzer.context },
                  ));

                  argument_irs = None;
                } else if let Some(argument_irs) = &mut argument_irs {
                  argument_irs.push(arg_ir);
                }
              }
            };

            if let Some(argument_irs) = argument_irs {
              return Some(ir::Expression::new(
                ir::ExpressionData::Call { callee: box callee_ir, arguments: argument_irs },
                return_type.unwrap_or(analyzer.context.void_ty),
                expr.origin
              ))
            }
          } else {
            analyzer.error(
              SourceRegion {
                source: callee.origin.source,
                start: callee.origin.end,
                end: expr.origin.end
              },
              format!(
                "Call contains {} arguments, but the callee function defines {} parameters",
                num_args, num_params
              )
            );
          }
        },

        _ => {
          analyzer.error(callee.origin, "Subexpression does not evaluate to a function and cannot be used as the callee of a call expression".to_owned());
        }
      }

      None
    },

    ast::ExpressionData::Block(box block) => {
      let block = evaluate_block(analyzer, true, block)?;

      let ty = block.trailing_expression.as_ref().unwrap().ty;

      Some(ir::Expression::new(ir::ExpressionData::Block(box block), ty, expr.origin))
    }

    ast::ExpressionData::Conditional(box conditional) => {
      let conditional = evaluate_conditional(analyzer, true, conditional)?;

      let ty = conditional.if_branch.body.trailing_expression.as_ref().unwrap().ty;

      Some(ir::Expression::new(ir::ExpressionData::Conditional(box conditional), ty, expr.origin))
    }
  }
}

fn evaluate_statement (analyzer: &mut Analyzer, statement: &ast::Statement) -> Option<ir::Statement> {
  unimplemented!()
}

fn evaluate_block (analyzer: &mut Analyzer, as_expression: bool, block: &ast::Block) -> Option<ir::Block> {
  unimplemented!()
}

fn evaluate_conditional (analyzer: &mut Analyzer, as_expression: bool, conditional: &ast::Conditional) -> Option<ir::Conditional> {
  unimplemented!()
}

/// Performs analysis at the statement & expression levels
fn pass_eval_bodies (analyzer: &mut Analyzer, items: &mut Vec<Item>) {
  fn evaluate_item (analyzer: &mut Analyzer, item: &mut Item) {
    match &mut item.data {
      ItemData::Module { identifier, items, .. } => {
        analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
        pass_eval_bodies(analyzer, items);
        analyzer.pop_active_module();
      },

      ItemData::Global { identifier, initializer, .. } => {
        let global_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

        // its possible some shadowing error has overwritten this def and if so we just return
        let global = some!(analyzer.context.items.get(global_key).unwrap().ref_global());
      
        
      },

      ItemData::Function { identifier, parameters, body, .. } => {
        let function_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

        // its possible some shadowing error has overwritten this def and if so we just return
        let function = some!(analyzer.context.items.get(function_key).unwrap().ref_function());


      },

      | ItemData::Import { .. }
      | ItemData::Export { .. }
      => unreachable!()
    }
  }

  for item in items.iter_mut() {
    match &mut item.data {
      | ItemData::Import { .. }
      | ItemData::Export { data: ExportData::List { .. }, .. }
      => continue,
      
      ItemData::Export { data: ExportData::Inline(item), .. } => evaluate_item(analyzer, item),
      
      | ItemData::Module   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => evaluate_item(analyzer, item)
    }
  }
}


impl Analyzer {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self, ast: &mut Vec<Item>) {
    {
      let mut aliases = Vec::new();

      pass_bind_top_level(self, ast, &mut aliases);

      pass_resolve_aliases(self, &mut aliases);
    }

    pass_type_link_top_level(self, ast);

    pass_eval_bodies(self, ast);

    assert!(self.get_active_module_key() == self.context.lib_mod, "Internal error, a pass did not pop an active module");
  }
}