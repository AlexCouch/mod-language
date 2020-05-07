use crate::{
  // util::{ UnwrapUnchecked, },
  some,
  common::{ Identifier, },
  source::{ SourceRegion, },
  ast::{ Item, ItemData, ExportData, Path, TypeExpression, TypeExpressionData, Statement, StatementData, Expression, ExpressionData, },
  ctx::{ Module, Global, Function, NamespaceItem, NamespaceKey, Type, TypeData, LocalKey, MultiKey, },
};

use super::{ Analyzer, };



/// Binds top level items to their identifiers
#[inline]
fn pass_bind_top_level (analyzer: &mut Analyzer) {
  enum AliasData {
    Import { absolute: bool, relative_to: NamespaceKey, chain: Vec<Identifier>, },
    Export { base: Identifier, },
  }

  struct Alias {
    destination_module: NamespaceKey,
    data: AliasData,
    new_name: Identifier,
    origin: SourceRegion,
  }


  let mut aliases = Vec::new();


  fn binder_impl (analyzer: &mut Analyzer, items: &[Item], aliases: &mut Vec<Alias>) {
    fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item, aliases: &mut Vec<Alias>) -> (&'a Identifier, NamespaceKey) {
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

          binder_impl(analyzer, items, aliases);

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

  binder_impl(analyzer, analyzer.ast, &mut aliases);


  fn resolve_alias (alias: Alias, aliases: &mut Vec<Alias>, analyzer: &mut Analyzer) -> Option<NamespaceKey> {
    fn try_get_alias (in_module: NamespaceKey, find_import: bool, identifier: &Identifier, aliases: &mut Vec<Alias>) -> Option<Alias> {
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

          if let NamespaceItem::Module(module) = base {
            base_name.set(&module.canonical_name);

            resolved_key = if !absolute && resolved_key == relative_to {
              if let Some(local) = module.local_bindings.get_entry(ident) {
                local
              } else if let Some(alias) = try_get_alias(resolved_key, true, ident, aliases) {
                // if this fails there has already been an error message and we can just bail
                // TODO should unresolved imports import an error item? (probably)
                resolve_alias(alias, aliases, analyzer)?
              } else if let Some(core) = analyzer.context.core_ns.get_entry(ident) {
                core
              } else {
                analyzer.error(alias.origin, format!("Module `{}` does not have access to an item named `{}`", base_name, ident));
                return None
              }
            } else if let Some(exported_key) = module.export_bindings.get_entry(ident) {
              exported_key
            } else if let Some(alias) = try_get_alias(resolved_key, false, ident, aliases) {
              // if this fails there has already been an error message and we can just bail
              // TODO should unresolved imports import an error item? (probably)
              resolve_alias(alias, aliases, analyzer)?
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
        } else if let Some(alias) = try_get_alias(alias.destination_module, true, &base, aliases) {
          // if this fails there has already been an error message and we can just bail
          // TODO should unresolved exports export an error item? (probably)
          resolve_alias(alias, aliases, analyzer)?
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
    resolve_alias(alias, &mut aliases, analyzer);
  }
}


fn evaluate_global_ident (analyzer: &Analyzer, identifier: &Identifier, origin: SourceRegion) -> Option<NamespaceKey> {
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

fn evaluate_global_path (analyzer: &mut Analyzer, path: &Path, origin: SourceRegion) -> Option<NamespaceKey> {
  let mut base_name = Identifier::default();
                
  let base_key = if path.absolute {
    analyzer.context.lib_mod
  } else {
    analyzer.get_active_module_key()
  };

  let mut resolved_key = base_key;
  
  for ident in path.iter() {
    let base = analyzer.context.items.get(resolved_key).expect("Internal error, invalid lowered key during path resolution");

    if let NamespaceItem::Module(module) = base {
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

fn evaluate_anon_tdata (analyzer: &mut Analyzer, type_data: TypeData, origin: SourceRegion) -> NamespaceKey {
  assert!(type_data.is_anon(), "Internal error, non-anonymous TypeData passed to evaluate_anon_tdata");

  if let Some(existing_key) = analyzer.context.anon_types.get(&type_data) {
    *existing_key
  } else {
    let new_key = analyzer.context.items.insert(Type::new(None, origin, Some(type_data.clone())).into());
    analyzer.context.anon_types.insert(type_data, new_key);
    new_key
  }
}


fn evaluate_texpr (analyzer: &mut Analyzer, texpr: &TypeExpression) -> NamespaceKey {
  let err_ty = analyzer.context.err_ty;

  match &texpr.data {
    TypeExpressionData::Path(path) => {
      let key = some!(evaluate_global_path(analyzer, path, texpr.origin); err_ty);
      
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
fn pass_type_link_top_level (analyzer: &mut Analyzer) {
  fn type_linker_impl (analyzer: &mut Analyzer, items: &[Item]) {
    fn type_link_item (analyzer: &mut Analyzer, item: &Item) {
      match &item.data {
        ItemData::Module { identifier, items, .. } => {
          analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
          type_linker_impl(analyzer, items);
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

  type_linker_impl(analyzer, analyzer.ast)
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

fn evaluate_local_path (analyzer: &mut Analyzer, path: &Path, origin: SourceRegion) -> Option<MultiKey> {
  let mut base_name = Identifier::default();
                
  let base_key = if path.absolute {
    analyzer.context.lib_mod
  } else {
    analyzer.get_active_module_key()
  };

  let mut resolved_key: MultiKey = base_key.into();
  
  for ident in path.iter() {
    let ns_key = if let MultiKey::NamespaceKey(ns_key) = resolved_key {
      ns_key
    } else {
      analyzer.error(origin, "Local variables have no viable path extension".to_owned());

      return None
    };

    let base = analyzer.context.items.get(ns_key).expect("Internal error, invalid lowered key during path resolution");

    if let NamespaceItem::Module(module) = base {
      base_name.set(&module.canonical_name);

      resolved_key = if !path.absolute && resolved_key == base_key {
        if let Some(local) = analyzer.get_active_local_context().get_variable(ident) {
          local
        } else if let Some(global) = module.local_bindings.get_entry(ident) {
          global.into()
        } else if let Some(core) = analyzer.context.core_ns.get_entry(ident) {
          core.into()
        } else {
          analyzer.error(origin, format!("Cannot find item or local variable named `{}`", ident));
          return None
        }
      } else if let Some(exported_key) = module.export_bindings.get_entry(ident) {
        exported_key.into()
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


/// Generates IR for an expression and returns the Type of an expression
fn evaluate_expr (analyzer: &mut Analyzer, expr: &Expression) -> MultiKey {
  /// /// TODO what do ??? /// ///
  match &expr.data {
    ExpressionData::Path(path) => {
      let multi_key = evaluate_local_path(analyzer, path, expr.origin);
      unimplemented!()
    },

    ExpressionData::Identifier(ident) => {
      let multi_key = evaluate_local_ident(analyzer, ident, expr.origin);
      unimplemented!()
    },

    ExpressionData::Number(number) => {
      unimplemented!()
    },
    
    ExpressionData::Unary { box operand, operator } => {
      unimplemented!()
    },

    ExpressionData::Binary { box left, box right, operator } => {
      unimplemented!()
    },

    ExpressionData::Call { callee, arguments } => {
      unimplemented!()
    },

    ExpressionData::Block(box block) => {
      unimplemented!()
    },

    ExpressionData::Conditional(box conditional) => {
      unimplemented!()
    }
  }
}

/// Performs analysis at the statement & expression levels
fn pass_eval_bodies (analyzer: &mut Analyzer) {
  fn evaluator_impl (analyzer: &mut Analyzer, items: &[Item]) {
    fn evaluate_item (analyzer: &mut Analyzer, item: &Item) {
      match &item.data {
        ItemData::Module { identifier, items, .. } => {
          analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
          evaluator_impl(analyzer, items);
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

    for item in items.iter() {
      match &item.data {
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

  evaluator_impl(analyzer, analyzer.ast)
}


impl<'a> Analyzer<'a> {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self) {
    pass_bind_top_level(self);

    pass_type_link_top_level(self);

    pass_eval_bodies(self);

    assert!(self.get_active_module_key() == self.context.lib_mod, "Internal error, a pass did not pop an active module");
  }
}