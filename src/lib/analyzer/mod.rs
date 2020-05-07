//! The semantic analyzer

use crate::{
  util::{ UnwrapUnchecked, },
  session::{ SESSION, MessageKind, },
  source::{ SourceRegion, },
  common::{ Identifier, },
  ast::{ Item, },
  ctx::{ Context, Module, NamespaceItem, NamespaceKey, },
};

mod passes;





/// The core interface structure for semantic analysis
pub struct Analyzer<'a> {
  /// The AST being analyzed
  pub ast: &'a [Item],

  /// All contextual information used by a semantic analyzer
  pub context: Context,

  /// A stack of active modules being analyzed
  pub active_modules: Vec<NamespaceKey>,
}


impl<'a> Analyzer<'a> {
  /// Create a new semantic analyzer
  pub fn new (ast: &'a [Item]) -> Self {
    let context = Context::default();
    let active_modules = vec![ context.lib_mod ];

    Self {
      ast,

      context,

      active_modules,
    }
  }

  /// Run a semantic analyzer on its ast,
  /// consuming the analyzer in the process
  pub fn analyze (mut self) -> Context {
    self.run_passes();

    self.context
  }

  
  /// Push a new active module key and namespace on an Analyzer's stack
  pub fn push_active_module (&mut self, key: NamespaceKey) {
    self.active_modules.push(key);
  }

  /// Pops and returns active module key and namespace from an Analyzer's stack
  ///
  /// Panics if there is only one (the root) active module and namespace left on the stack,
  /// or if the item namespaces and active modules counts are not identical
  pub fn pop_active_module (&mut self) -> NamespaceKey {
    assert!(
      self.active_modules.len() > 1,
      "Internal error, cannot pop lib module"
    );

    unsafe { self.active_modules.pop().unwrap_unchecked() }
  }


  /// Get they key of the active Module in an Analyzer
  pub fn get_active_module_key (&self) -> NamespaceKey {
    unsafe { *self.active_modules.last().unwrap_unchecked() }
  }

  /// Get an immutable reference to the active Module in an Analyzer
  pub fn get_active_module (&self) -> &Module {
    unsafe { self.context.items.get_unchecked(self.get_active_module_key()).ref_module_unchecked() }
  }
  
  /// Get a mutable reference to the active Module in an Analyzer
  pub fn get_active_module_mut (&mut self) -> &mut Module {
    unsafe { self.context.items.get_unchecked_mut(self.get_active_module_key()).mut_module_unchecked() }
  }


  /// Create a new top level item in the active module of an Analyzer
  /// 
  /// Creates an error if there is an existing item with the same identifier
  /// 
  /// Returns the NamespaceKey associated with the new item
  pub fn create_item<I: Into<NamespaceItem>> (&mut self, identifier: Identifier, new_item: I, origin: SourceRegion) -> NamespaceKey {
    let new_item = new_item.into();

    if let Some(shadowed_key) = self.get_active_module().local_bindings.get_entry(&identifier) {
      let shadowed_kind = self.context.items.get(shadowed_key).expect("Internal error, shadowed item does not exist").kind();
      let shadowed_location = self.get_active_module().local_bindings.get_bind_location(shadowed_key).expect("Internal error, shadowed item has no bind location");

      self.error(origin, format!(
        "{} `{}` shadows existing {} in `{}`, defined at [{}]",
        new_item.kind(), identifier, shadowed_kind, self.get_active_module().canonical_name, shadowed_location
      ));
    }

    let key = (|| {
      if let NamespaceItem::Type(ty) = &new_item {
        if let Some(td) = &ty.data {
          if td.is_anon() {
            return if let Some(existing_key) = self.context.anon_types.get(td) {
              *existing_key
            } else {
              let td_for_lookup = td.clone();
              let new_key = self.context.items.insert(new_item);
              self.context.anon_types.insert(td_for_lookup, new_key);
              new_key
            }
          }
        }
      }
      
      self.context.items.insert(new_item)
    })();

    self.get_active_module_mut().local_bindings.set_entry_bound(identifier, key, origin);

    key
  }


  /// Create a Message in the Source of the AST of an Analyzer
  pub fn message (&self, origin: SourceRegion, kind: MessageKind, message: String) {
    SESSION.message(Some(origin), kind, message)
  }

  /// Create a notice in the Source of the AST of an Analyzer
  pub fn notice (&self, origin: SourceRegion, message: String) {
    self.message(origin, MessageKind::Notice, message)
  }

  /// Create a warning in the Source of the AST of an Analyzer
  pub fn warning (&self, origin: SourceRegion, message: String) {
    self.message(origin, MessageKind::Warning, message)
  }

  /// Create an error in the Source of the AST of an Analyzer
  pub fn error (&self, origin: SourceRegion, message: String) {
    self.message(origin, MessageKind::Error, message)
  }
}