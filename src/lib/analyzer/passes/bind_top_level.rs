use crate::{
  common::{ Identifier, },
  ast::{ self, Item, ItemData, ExportData, PseudonymData, },
  ctx::{ ContextKey, Namespace, Global, Function, },
};

use super::{
  Analyzer,
  support_structures::{ Pseudonym, PseudonymKind, },
};



/// Binds top level items to their identifiers
pub fn bind_top_level (analyzer: &mut Analyzer, items: &[Item], pseudonyms: &mut Vec<Pseudonym>) {
  for item in items.iter() {
    match &item.data {
      ItemData::Alias { data, .. } => {
        let destination_namespace = analyzer.get_active_namespace_key();

        for PseudonymData { path, new_name } in data.iter() {
          let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty alias path with no pseudonym") }.to_owned();
          
          let relative_to = if path.absolute { analyzer.context.lib_ns } else { destination_namespace };

          pseudonyms.push(Pseudonym {
            destination_namespace,
            kind: PseudonymKind::Alias,
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

            for ast::PseudonymData { path, new_name } in exports.iter() {
              let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty export path with no pseudonym") }.to_owned();
              
              let relative_to = if path.absolute { analyzer.context.lib_ns } else { destination_namespace };

              pseudonyms.push(Pseudonym {
                destination_namespace,
                kind: PseudonymKind::Export,
                absolute: path.absolute,
                relative_to,
                chain: path.chain.clone(),
                new_name,
                origin: item.origin,
              })
            }
          },

          ExportData::Inline(item) => {
            let (identifier, key) = bind_item(analyzer, item, pseudonyms);

            analyzer.get_active_namespace_mut().export_bindings.set_entry_bound(identifier.to_owned(), key, item.origin);
          },
        }
      },

      | ItemData::Namespace   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => {
        bind_item(analyzer, item, pseudonyms);
      }
    }
  }
}


fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item, pseudonyms: &mut Vec<Pseudonym>) -> (&'a Identifier, ContextKey) {
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

      bind_top_level(analyzer, items, pseudonyms);

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

    | ItemData::Alias { .. }
    | ItemData::Export { .. }
    => unreachable!("Internal error, export node contains invalid descendent")
  }
}