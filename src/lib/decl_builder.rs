#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]

//! A system which iterates over a processed Context and generates a declaration file of all exports

use crate::{
  common::{ Identifier, },
  ctx::{ Context, ContextKey, ContextItem, Namespace, Global, Function, Type, },
  ast::{ self, Path, },
};


fn generate_declarations (ctx: &Context) -> ast::Item {
  decl_namespace(ctx, ctx.main_ns)
}

fn generate_path_to (ctx: &Context, key: ContextKey) -> Path {
  unimplemented!()
}

fn decl_namespace (ctx: &Context, namespace_key: ContextKey) -> ast::Item {
  let curr_namespace = ctx.items.get(namespace_key).unwrap().ref_namespace().unwrap();

  let mut items: Vec<ast::Item> = Vec::new();

  for (export_ident, &export_key) in curr_namespace.export_bindings.entry_iter() {
    let export_item = ctx.items.get(export_key).unwrap();

    match export_item {
      ContextItem::Namespace(namespace)
      if namespace.parent_namespace == Some(namespace_key)
      => items.push(decl_namespace(ctx, export_key)),

      ContextItem::Type(ty)
      if ty.parent_namespace == Some(namespace_key)
      => items.push(decl_type(ctx, export_key)),

      ContextItem::Global(global)
      if global.parent_namespace == namespace_key
      => items.push(decl_global(ctx, export_key)),

      ContextItem::Function(function)
      if function.parent_namespace == namespace_key
      => items.push(decl_function(ctx, export_key)),

      ___________
      => continue
    }
  }

  ast::Item::new(
    ast::ItemData::Namespace {
      identifier: curr_namespace.canonical_name.clone(),
      items,
      inline: true,
    },
    curr_namespace.origin
  )
}

fn decl_type (ctx: &Context, type_key: ContextKey) -> ast::Item {
  unimplemented!()
}

fn decl_global (ctx: &Context, global_key: ContextKey) -> ast::Item {
  unimplemented!()
}

fn decl_function (ctx: &Context, function_key: ContextKey) -> ast::Item {
  unimplemented!()
}

