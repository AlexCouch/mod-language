use std::{
  fmt::{ Display, Formatter, Result as FMTResult, },
};

use crate::{
  // util::{ UnwrapUnchecked, },
  common::{ Identifier, },
  // source::{ SourceRegion, },
  ast::{ Item, ItemData, ExportData, },
  ctx::{ Module, Global, Function, NamespaceItem, NamespaceKey, Alias as CTXAlias, AliasData, },
};

use super::{ Analyzer, };


struct RefPathDisplay<'a>(&'a [&'a Identifier]);

impl<'a> Display for RefPathDisplay<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    let mut iter = self.0.iter().peekable();
    while let Some(ident) = iter.next() {
      write!(f, "{}", ident)?;
      if iter.peek().is_some() {
        write!(f, "::")?;
      }
    }
    Ok(())
  }
}


/// Binds top level items to their identifiers
#[inline]
fn pass_bind_top_level (analyzer: &mut Analyzer) {
  fn binder_impl (analyzer: &mut Analyzer, items: &[Item]) {
    fn bind_item<'a> (analyzer: &mut Analyzer, item: &'a Item) -> (&'a Identifier, NamespaceKey) {
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

          binder_impl(analyzer, items);

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
          for (base, new_name) in data.iter() {
            let new_name = if let Some(new_name) = new_name { new_name } else { base.last().expect("Internal error, empty import path") };

            let relative_to = if base.absolute { None } else { Some(analyzer.get_active_module_key()) };

            analyzer.create_item(new_name.to_owned(), CTXAlias::new(relative_to, AliasData::Import(base.to_owned()), item.origin), item.origin);
          }
        },

        ItemData::Export { data, .. } => {
          match data {
            ExportData::List(aliases) => {
              for (base, new_name) in aliases.iter() {
                let new_name = if let Some(new_name) = new_name { new_name } else { base };
                
                let key = analyzer.context.items.insert(CTXAlias::new(Some(analyzer.get_active_module_key()), AliasData::Export(base.to_owned()), item.origin).into());
                
                analyzer.get_active_module_mut().export_bindings.set_entry_bound(new_name.to_owned(), key, item.origin);
              }
            },

            ExportData::Inline(item) => {
              let (identifier, key) = bind_item(analyzer, item);

              analyzer.get_active_module_mut().export_bindings.set_entry_bound(identifier.to_owned(), key, item.origin);
            },
          }
        },

        _ => {
          bind_item(analyzer, item);
        }
      }
    }
  }

  binder_impl(analyzer, analyzer.ast)
}


/// Traverses the top level items discovered in the first pass and attempts to resolve all of the aliases
#[inline]
pub fn pass_resolve_aliases (analyzer: &mut Analyzer) {
  fn resolver_impl (analyzer: &mut Analyzer, module_key: NamespaceKey) {
    if let NamespaceItem::Module(module) = analyzer.context.items.get(module_key).expect("Internal error, alias resolver impl called on undefined key") {
      let aliases: Vec<_> = module.local_bindings.key_iter().chain(module.export_bindings.key_iter()).copied().collect();

      for key in aliases.into_iter() {
        if let Some(item_key) = analyzer.context.lower_key(key) {
          resolver_impl(analyzer, item_key)
        }
      }
    }
  }

  resolver_impl(analyzer, analyzer.get_active_module_key())
}


impl<'a> Analyzer<'a> {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self) {
    pass_bind_top_level(self);

    pass_resolve_aliases(self);
  }
}