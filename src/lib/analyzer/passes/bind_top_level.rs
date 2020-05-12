use crate::{
  common::{ Identifier, },
  ast::{ self, Item, ItemData, ExportData, AliasData, },
  ctx::{ ContextKey, Namespace, Global, Function, },
};

use super::{
  Analyzer,
  support_structures::{ Alias, AliasKind, },
};



/// Binds top level items to their identifiers
pub fn bind_top_level (analyzer: &mut Analyzer, items: &[Item], aliases: &mut Vec<Alias>) {
  for item in items.iter() {
    match &item.data {
      ItemData::Import { data, .. } => {
        let destination_namespace = analyzer.get_active_namespace_key();

        for AliasData { path, new_name } in data.iter() {
          let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty import path with no alias") }.to_owned();
          
          let relative_to = if path.absolute { analyzer.context.lib_ns } else { destination_namespace };

          aliases.push(Alias {
            destination_namespace,
            kind: AliasKind::Import,
            absolute: path.absolute,
            relative_to,
            chain: path.chain.clone(),
            new_name,
            origin: item.origin,
          })
        }
      },

      ItemData::Export { data, .. } => {
        match data {
          ExportData::List(exports) => {
            let destination_namespace = analyzer.get_active_namespace_key();

            for ast::AliasData { path, new_name } in exports.iter() {
              let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty export path with no alias") }.to_owned();
              
              let relative_to = if path.absolute { analyzer.context.lib_ns } else { destination_namespace };

              aliases.push(Alias {
                destination_namespace,
                kind: AliasKind::Export,
                absolute: path.absolute,
                relative_to,
                chain: path.chain.clone(),
                new_name,
                origin: item.origin,
              })
            }
          },

          ExportData::Inline(item) => {
            let (identifier, key) = bind_item(analyzer, item, aliases);

            analyzer.get_active_namespace_mut().export_bindings.set_entry_bound(identifier.to_owned(), key, item.origin);
          },
        }
      },

      | ItemData::Namespace   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => {
        bind_item(analyzer, item, aliases);
      }
    }
  }
}


fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item, aliases: &mut Vec<Alias>) -> (&'a Identifier, ContextKey) {
  match &item.data {
    ItemData::Namespace { identifier, items, .. } => {
      let new_ns = analyzer.create_item(
        identifier.to_owned(),
        Namespace::new(
          Some(analyzer.get_active_namespace_key()),
          identifier.to_owned(),
          item.origin
        ),
        item.origin
      );

      analyzer.push_active_namespace(new_ns);

      bind_top_level(analyzer, items, aliases);

      analyzer.pop_active_namespace();

      (identifier, new_ns)
    },

    ItemData::Global { identifier, .. } => (identifier, analyzer.create_item(
      identifier.to_owned(),
      Global::new(
        analyzer.get_active_namespace_key(),
        identifier.to_owned(),
        item.origin,
        None
      ),
      item.origin
    )),

    ItemData::Function { identifier, .. } => (identifier, analyzer.create_item(
      identifier.to_owned(),
      Function::new(
        analyzer.get_active_namespace_key(),
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