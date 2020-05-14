use crate::{
  common::{ Identifier, },
  source::{ SOURCE_MANAGER, },
  ast::{ Item, ItemData, ExportData, PseudonymData, Path, },
  ctx::{ ContextKey,  Namespace, Type, Global, Function, },
};

use super::{
  Analyzer,
  support_structures::{ Pseudonym, PseudonymKind, PseudonymPayload, },
};



/// Binds top level items to their identifiers
pub fn bind_top_level (analyzer: &mut Analyzer, items: &[Item], pseudonyms: &mut Vec<Pseudonym>) {
  for item in items.iter() {
    match &item.data {
      ItemData::Alias { data, .. } => {
        let destination_namespace = analyzer.get_active_namespace_key();

        for &PseudonymData { ref path, ref new_name, origin } in data.iter() {
          let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty alias path with no pseudonym") }.to_owned();
          
          let relative_to = if path.absolute { analyzer.get_active_module().namespace } else { destination_namespace };

          pseudonyms.push(Pseudonym {
            destination_namespace,
            kind: PseudonymKind::Alias,
            payload: PseudonymPayload::Path(path.clone()),
            relative_to,
            new_name,
            origin,
          })
        }
      },

      ItemData::Export { data, .. } => {
        let destination_namespace = analyzer.get_active_namespace_key();

        match data {
          ExportData::List(exports) => {
            for &PseudonymData { ref path, ref new_name, origin } in exports.iter() {
              let new_name = if let Some(new_name) = new_name { new_name } else { path.last().expect("Internal error, empty export path with no pseudonym") }.to_owned();
              
              let relative_to = if path.absolute { analyzer.get_active_module().namespace } else { destination_namespace };

              pseudonyms.push(Pseudonym {
                destination_namespace,
                kind: PseudonymKind::Export,
                payload: PseudonymPayload::Path(path.clone()),
                relative_to,
                new_name,
                origin,
              })
            }
          },

          ExportData::Inline(type_item @ box Item { data: ItemData::Type { identifier, .. }, .. }) => {
            bind_pseudo_type(analyzer, type_item, pseudonyms);

            pseudonyms.push(Pseudonym {
              destination_namespace,
              kind: PseudonymKind::Export,
              payload: PseudonymPayload::Path(Path::new(false, vec![ identifier.clone() ], type_item.origin)),
              relative_to: destination_namespace,
              new_name: identifier.clone(),
              origin: item.origin,
            })
          },

          ExportData::Inline(box item) => {
            let (identifier, key) = bind_item(analyzer, item, pseudonyms);

            analyzer.get_active_namespace_mut().export_bindings.set_entry_bound(identifier.to_owned(), key, item.origin);
          },
        }
      },

      ItemData::Type { .. } => bind_pseudo_type(analyzer, item, pseudonyms),

      | ItemData::Import    { .. }
      | ItemData::Namespace { .. }
      | ItemData::Struct    { .. }
      | ItemData::Global    { .. }
      | ItemData::Function  { .. }
      => {
        bind_item(analyzer, item, pseudonyms);
      }
    }
  }
}

fn bind_pseudo_type (analyzer: &mut Analyzer, item: &Item, pseudonyms: &mut Vec<Pseudonym>) {
  if let ItemData::Type { identifier, type_expression } = &item.data {
    let destination_namespace = analyzer.get_active_namespace_key();
    let relative_to = destination_namespace;

    pseudonyms.push(Pseudonym {
      destination_namespace,
      kind: PseudonymKind::Alias,
      payload: PseudonymPayload::TypeExpression(type_expression.clone()),
      relative_to,
      new_name: identifier.clone(),
      origin: item.origin,
    })
  } else {
    unreachable!("Internal error, bind_pseudo_type called on invalid item");
  }
}


fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item, pseudonyms: &mut Vec<Pseudonym>) -> (&'a Identifier, ContextKey) {
  match &item.data {
    ItemData::Import { identifier, new_name, ast_key } => {
      let module_key = if let Some(&module_key) = analyzer.context.modules.get(identifier) {
        module_key
      } else {
        let module_key = analyzer.create_module(identifier.clone(), item.origin);

        let ast = SOURCE_MANAGER.get_ast(*ast_key).expect("Internal error, invalid ASTKey in import node");

        analyzer.push_active_module(module_key);

        bind_top_level(analyzer, ast, pseudonyms);
        
        analyzer.pop_active_module();

        module_key
      };

      let new_name = if let Some(new_name) = new_name { new_name } else { identifier };

      let ns = analyzer.get_active_namespace_mut();

      ns.local_bindings.set_entry_bound(new_name.clone(), module_key, item.origin);

      (new_name, module_key)
    },

    ItemData::Namespace { identifier, items, .. } => {
      let new_ns = analyzer.create_item(
        identifier.to_owned(),
        Namespace::new(
          analyzer.get_active_module_key(),
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

    ItemData::Struct { identifier, .. } => (identifier, analyzer.create_item(
      identifier.to_owned(),
      Type::new(
        Some(analyzer.get_active_module_key()),
        Some(analyzer.get_active_namespace_key()),
        Some(identifier.to_owned()),
        item.origin,
        None
      ),
      item.origin
    )),

    ItemData::Global { identifier, .. } => (identifier, analyzer.create_item(
      identifier.to_owned(),
      Global::new(
        analyzer.get_active_module_key(),
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
        analyzer.get_active_module_key(),
        analyzer.get_active_namespace_key(),
        identifier.to_owned(),
        item.origin,
        None
      ),
      item.origin
    )),

    | ItemData::Alias { .. }
    | ItemData::Export { .. }
    | ItemData::Type { .. }
    => unreachable!("Internal error, export node contains invalid / uncaught descendent")
  }
}