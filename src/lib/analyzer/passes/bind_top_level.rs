use crate::{
  common::{ Identifier, },
  ast::{ self, Item, ItemData, ExportData, AliasData, },
  ctx::{ GlobalKey, Module, Global, Function, },
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
        let destination_module = analyzer.get_active_module_key();

        for AliasData { path, new_name } in data.iter() {
          let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty import path with no alias") }.to_owned();
          
          let relative_to = if path.absolute { analyzer.context.lib_mod } else { destination_module };

          aliases.push(Alias {
            destination_module,
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
            let destination_module = analyzer.get_active_module_key();

            for ast::AliasData { path, new_name } in exports.iter() {
              let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty export path with no alias") }.to_owned();
              
              let relative_to = if path.absolute { analyzer.context.lib_mod } else { destination_module };

              aliases.push(Alias {
                destination_module,
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

      bind_top_level(analyzer, items, aliases);

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

