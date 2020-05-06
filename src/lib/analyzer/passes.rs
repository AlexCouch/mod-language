use std::{
  fmt::{ Display, Formatter, Result as FMTResult, },
};

use crate::{
  // util::{ UnwrapUnchecked, },
  common::{ Identifier, },
  source::{ SourceRegion, },
  ast::{ Item, ItemData, ExportData, },
  ctx::{ Module, Global, Function, NamespaceItem, NamespaceKey, },
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
                analyzer.error(alias.origin, format!("Module `{}` does not have an item named `{}` available for import", base_name, ident));
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




impl<'a> Analyzer<'a> {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self) {
    pass_bind_top_level(self);
  }
}