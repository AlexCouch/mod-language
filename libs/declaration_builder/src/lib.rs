//! A system which iterates over a processed Context and generates a declaration file of all exports

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::useless_let_if_seq)]

#![feature(box_syntax)]

use std::{
  collections::{ HashSet, },
};

use mod_common::{ Identifier, };
use mod_frontend::{ 
  ast,
  ctx::{ Context, ContextKey, ContextItem, Namespace, TypeData, },
};



/// Iterate over the exported items of a Context and build an ast of external declarations
pub fn generate_declarations (ctx: &Context) -> Vec<ast::Item> {
  let exported_items = collect_exported_items(ctx);

  decl_ns_items(ctx, &exported_items, ctx.main_ns)
}



fn collect_ns_exports (ctx: &Context, ns: &Namespace, set: &mut HashSet<ContextKey>) {
  for &exported_key in ns.export_bindings.key_iter() {
    set.insert(exported_key);

    if let ContextItem::Namespace(ns) = ctx.items.get(exported_key).unwrap() {
      collect_ns_exports(ctx, ns, set)
    }
  }
}


fn collect_exported_items (ctx: &Context) -> HashSet<ContextKey> {
  let mut set = HashSet::default();

  let module_ns = ctx.items.get(ctx.main_ns).unwrap().ref_namespace().unwrap();

  collect_ns_exports(ctx, module_ns, &mut set);

  set
}



fn ns_contains_exports (exported_items: &HashSet<ContextKey>, ns: &Namespace) -> bool {
  for export_key in ns.export_bindings.key_iter() {
    if exported_items.contains(export_key) {
      return true
    }
  }

  false
}

fn ns_exports (ns: &Namespace, key: ContextKey) -> bool {
  for &export_key in ns.export_bindings.key_iter() {
    if export_key == key {
      return true
    }
  }

  false
}



fn decl_ns_items (ctx: &Context, exported_items: &HashSet<ContextKey>, ns_key: ContextKey) -> Vec<ast::Item> {
  let ns: &Namespace = ctx.items.get(ns_key).unwrap().ref_namespace().unwrap();

  let mut items = Vec::new();

  for (local_ident, &local_key) in ns.local_bindings.entry_iter() {
    if !ns_exports(ns, local_key) {
      let local_item = ctx.items.get(local_key).unwrap();

      match local_item {
        ContextItem::Module(module) => {
          if ns_contains_exports(exported_items, ctx.items.get(module.namespace).unwrap().ref_namespace().unwrap()) {
            items.push(decl_import(ctx, None, local_key));
          }
        },

        ContextItem::Namespace(local_ns) => {
          if local_ns.parent_namespace == Some(ns_key)
          && ns_contains_exports(exported_items, local_ns) {
            items.push(decl_ns(ctx, exported_items, local_ident.clone(), local_key));
          }
        },

        _ => continue
      }
    }
  }

  for (export_ident, &export_key) in ns.export_bindings.entry_iter() {
    let export_item = ctx.items.get(export_key).unwrap();

    match export_item {
      ContextItem::Module(_) => {
        items.push(make_export_item(decl_import(ctx, Some(export_ident.clone()), export_key)));
      },

      ContextItem::Namespace(export_ns) => {
        if export_ns.parent_namespace == Some(ns_key) {
          items.push(make_export_item(decl_ns(ctx, exported_items, export_ident.clone(), export_key)));
        } else {
          items.push(decl_reexport(ctx, ns_key, export_ident.clone(), export_key));
        }
      },

      ContextItem::Type(ty) => {
        if ty.parent_namespace == Some(ns_key) {
          items.push(make_export_item(decl_ty(ctx, ns_key, export_ident.clone(), export_key)));
        } else if !ty.is_anon() {
          items.push(decl_reexport(ctx, ns_key, export_ident.clone(), export_key));
        } else {
          items.push(make_export_item(decl_ty_alias(ctx, ns_key, export_ident.clone(), export_key)));
        }
      },

      ContextItem::Global(global) => {
        if global.parent_namespace == ns_key {
          items.push(make_export_item(decl_global(ctx, ns_key, export_ident.clone(), export_key)));
        } else {
          items.push(decl_reexport(ctx, ns_key, export_ident.clone(), export_key));
        }
      },

      ContextItem::Function(function) => {
        if function.parent_namespace == ns_key {
          items.push(make_export_item(decl_function(ctx, ns_key, export_ident.clone(), export_key)));
        } else {
          items.push(decl_reexport(ctx, ns_key, export_ident.clone(), export_key));
        }
      },
    }
  }

  items
}


fn decl_import (ctx: &Context, mod_name: Option<Identifier>, mod_key: ContextKey) -> ast::Item {
  let module = ctx.items.get(mod_key).unwrap().ref_module().unwrap();

  ast::Item::no_src(ast::ItemData::Import {
    identifier: module.canonical_name.clone(),
    new_name: mod_name.and_then(|mod_name| if mod_name != module.canonical_name { Some(mod_name) } else { None }),
    ast_key: Default::default()
  })
}


fn decl_ns (ctx: &Context, exported_items: &HashSet<ContextKey>, ns_name: Identifier, ns_key: ContextKey) -> ast::Item {  
  ast::Item::no_src(ast::ItemData::Namespace {
    identifier: ns_name,
    items: decl_ns_items(ctx, exported_items, ns_key),
    inline: true,
  })
}


fn decl_ty (ctx: &Context, base_key: ContextKey, ty_name: Identifier, ty_key: ContextKey) -> ast::Item {
  let ty = ctx.items.get(ty_key).unwrap().ref_type().unwrap();

  if let Some(TypeData::Structure { field_names, field_types }) = ty.data.as_ref() {
    let fields =
      field_names.iter().zip(field_types.iter())
        .map(|(field_name, &field_type)| ast::LocalDeclaration::no_src(field_name.clone(), make_texpr(ctx, base_key, field_type)))
        .collect();

    ast::Item::no_src(ast::ItemData::Struct { identifier: ty_name, fields, terminal: true })
  } else {
    unreachable!()
  }
}


fn decl_ty_alias (ctx: &Context, base_key: ContextKey, ty_name: Identifier, ty_key: ContextKey) -> ast::Item {
  ast::Item::no_src(
    ast::ItemData::Type {
      identifier: ty_name,
      type_expression: make_texpr(ctx, base_key, ty_key)
    }
  )
}


fn decl_global (ctx: &Context, base_key: ContextKey, global_name: Identifier, global_key: ContextKey) -> ast::Item {
  let global = ctx.items.get(global_key).unwrap().ref_global().unwrap();

  ast::Item::no_src(
    ast::ItemData::Global {
      identifier: global_name,
      explicit_type: make_texpr(ctx, base_key, global.ty.unwrap()),
      initializer: None
    }
  )
}


fn decl_function (ctx: &Context, base_key: ContextKey, function_name: Identifier, function_key: ContextKey) -> ast::Item {
  let function = ctx.items.get(function_key).unwrap().ref_function().unwrap();

  let parameters =
    function.params.iter()
    .map(|&(ref ident, ty, _)| ast::LocalDeclaration::no_src(ident.clone(), make_texpr(ctx, base_key, ty)))
    .collect();

  ast::Item::no_src(
    ast::ItemData::Function {
      identifier: function_name,
      parameters,
      return_type: function.return_ty.map(|ty| make_texpr(ctx, base_key, ty)),
      body: None
    }
  )
}


fn decl_reexport (ctx: &Context, base_key: ContextKey, ns_name: Identifier, ns_key: ContextKey) -> ast::Item {
  let path = make_path(ctx, base_key, ns_key);
  let new_name = if path.last() == Some(&ns_name) { None } else { Some(ns_name) };

  ast::Item::no_src(
    ast::ItemData::Export {
      data: ast::ExportData::List(vec! [ ast::PseudonymData::no_src(path, new_name) ]),
      terminal: false
    }
  )
}





fn make_texpr (ctx: &Context, base_key: ContextKey, ty_key: ContextKey) -> ast::TypeExpression {
  let ty = ctx.items.get(ty_key).unwrap().ref_type().unwrap();

  let texpr_data = match ty.data.as_ref().unwrap() {
    | TypeData::Structure { .. }
    | TypeData::Primitive { .. }
    => ast::TypeExpressionData::Path(make_path(ctx, base_key, ty_key)),

    &TypeData::Pointer(value_ty) => ast::TypeExpressionData::Pointer(box make_texpr(ctx, base_key, value_ty)),
    
    TypeData::Function { parameter_types, return_type } => {
      let parameter_types = parameter_types.iter().map(|&ty| make_texpr(ctx, base_key, ty)).collect();
      let return_type = box return_type.map(|ty| make_texpr(ctx, base_key, ty));

      ast::TypeExpressionData::Function { parameter_types, return_type }
    },

    _ => unreachable!()
  };

  ast::TypeExpression::no_src(texpr_data)
}


fn make_export_item (item: ast::Item) -> ast::Item {
  let terminal = !item.requires_semi();
  ast::Item::no_src(ast::ItemData::Export { data: ast::ExportData::Inline(box item), terminal })
}


fn make_path (ctx: &Context, base_key: ContextKey, ns_key: ContextKey) -> ast::Path {
  let mut chain = Vec::new();
  let mut bs_chain: Vec<(bool, ContextKey)> = Vec::new();

  let mut active_key = ns_key;

  let mut absolute = true;

  'traversal: loop {
    if let Some(parent_key) = ctx.get_item_parent(active_key) {
      let parent_ns: &Namespace = ctx.items.get(parent_key).unwrap().ref_namespace().unwrap();

      for (export_ident, &export_key) in parent_ns.export_bindings.entry_iter() {
        if export_key == active_key {
          chain.insert(0, export_ident.to_owned());
          bs_chain.insert(0, (false, active_key));
          active_key = parent_key;
          continue 'traversal
        }
      }

      for (local_ident, &local_key) in parent_ns.local_bindings.entry_iter() {
        if local_key == active_key {
          active_key = parent_key;
          chain.insert(0, local_ident.to_owned());
          bs_chain.insert(0, (true, active_key));
          
          if active_key == base_key {
            absolute = false;
            break 'traversal
          } else {
            continue 'traversal
          }
        }
      }

      panic!("Could not find item named `{}` in `{}` [chain was @ {:?}]", ctx.get_item_canonical_name(active_key).map(|ident| ident.as_ref()).unwrap_or("[ERROR GETTING IDENT]"), parent_ns.canonical_name, chain);
    } else {
      break 'traversal
    }
  }

  fn resolve_path_chains (ctx: &Context, mut chain: Vec<Identifier>, mut bs_chain: Vec<(bool, ContextKey)>) -> Vec<Identifier> {
    let len = bs_chain.len();

    if len <= 2 { return chain }

    for i in 0 ..= len - 3 {
      let (is_local, _) = bs_chain[i + 1];
      
      if is_local {
        let (_, root_key)  = bs_chain[i];
        let (_, local_key) = bs_chain[i + 2];

        let root_ns: &Namespace = ctx.items.get(root_key).unwrap().ref_namespace().unwrap();

        for (export_ident, &export_key) in root_ns.export_bindings.entry_iter() {
          if export_key == local_key {
            bs_chain[i + 2].0 = false;
            chain[i + 2] = export_ident.clone();
            chain.remove(i + 1);
            bs_chain.remove(i + 1);

            return resolve_path_chains(ctx, chain, bs_chain)
          }
        }

        unreachable!();
      }
    }

    chain
  }

  let mut chain = resolve_path_chains(ctx, chain, bs_chain);

  if absolute {
    if let Some(module_key) = ctx.get_item_module(active_key) {
      if module_key != ctx.main_mod {
        let module = ctx.items.get(module_key).unwrap().ref_module().unwrap();
        // TODO this needs to get the name the module was imported as when module imports are resolved
        chain.insert(0, module.canonical_name.clone());
        absolute = false;
      }
    }
  }

  ast::Path::no_src(absolute, chain)
}