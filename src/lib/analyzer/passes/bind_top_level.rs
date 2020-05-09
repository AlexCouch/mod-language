use crate::{
  common::{ Identifier, },
  ast::{ Item, ItemData, ExportData, },
  ctx::{ GlobalKey, Module, Global, Function, },
};

use super::{
  Analyzer,
  alias::{ Alias, AliasData, },
};


/// Binds top level items to their identifiers
pub fn bind_top_level (analyzer: &mut Analyzer, items: &[Item], aliases: &mut Vec<Alias>) {
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