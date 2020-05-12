//! The semantic analyzer

use crate::{
  util::{ UnwrapUnchecked, },
  session::{ SESSION, MessageKind, },
  source::{ SourceRegion, },
  common::{ Identifier, },
  ast::{ Item, },
  ctx::{ Context, Namespace, ContextItem, ContextKey, LocalContext, },
};


pub mod passes;


/// The core interface structure for semantic analysis
pub struct Analyzer {
  /// All contextual information used by a semantic analyzer
  pub context: Context,
  /// A stack of active namespaces being analyzed
  pub active_namespaces: Vec<ContextKey>,
  /// The local context being analyzed, if any
  pub local_context: Option<LocalContext>,
}


impl Default for Analyzer {
  #[inline] fn default () -> Self { Self::new() }
}


impl Analyzer {
  /// Create a new semantic analyzer
  pub fn new () -> Self {
    let context = Context::default();
    let active_namespaces = vec![ context.lib_ns ];

    Self {
      context,
      active_namespaces,
      local_context: None,
    }
  }

  /// Run a semantic analyzer on its ast,
  /// consuming the analyzer in the process
  pub fn analyze (mut self, mut ast: Vec<Item>) -> (Context, Vec<Item>) {
    self.run_passes(&mut ast);

    (self.context, ast)
  }

  
  /// Push a new active namespace key on an Analyzer's stack
  pub fn push_active_namespace (&mut self, key: ContextKey) {
    self.active_namespaces.push(key);
  }

  /// Pops and returns active namespace key from an Analyzer's stack
  ///
  /// Panics if there is only one (the root) active namespace left on the stack,
  /// or if the active namespaces counts are not identical
  pub fn pop_active_namespace (&mut self) -> ContextKey {
    assert!(
      self.active_namespaces.len() > 1,
      "Internal error, cannot pop lib namespace"
    );

    unsafe { self.active_namespaces.pop().unwrap_unchecked() }
  }


  /// Get they key of the active Namespace in an Analyzer
  pub fn get_active_namespace_key (&self) -> ContextKey {
    unsafe { *self.active_namespaces.last().unwrap_unchecked() }
  }

  /// Get an immutable reference to the active Namespace in an Analyzer
  pub fn get_active_namespace (&self) -> &Namespace {
    unsafe { self.context.items.get_unchecked(self.get_active_namespace_key()).ref_namespace_unchecked() }
  }
  
  /// Get a mutable reference to the active Namespace in an Analyzer
  pub fn get_active_namespace_mut (&mut self) -> &mut Namespace {
    unsafe { self.context.items.get_unchecked_mut(self.get_active_namespace_key()).mut_namespace_unchecked() }
  }


  /// Create a new LocalContext
  /// 
  /// Panics if there is already a local context
  #[track_caller]
  pub fn create_local_context (&mut self) -> &mut LocalContext {
    self.local_context.replace(LocalContext::default()).expect_none("Internal error, cannot create LocalContext, one already exists");
    unsafe { self.local_context.as_mut().unwrap_unchecked() }
  }

  /// Remove the LocalContext
  /// 
  /// Panics if there is not a local context
  #[track_caller]
  pub fn remove_local_context (&mut self) -> LocalContext {
    self.local_context.take().expect("Internal error, cannot remove LocalContext, it does not exist")
  }

  /// Get an immutable reference to the active local context
  /// 
  /// Panics if there is no local context
  #[track_caller]
  pub fn get_local_context (&self) -> &LocalContext {
    self.local_context.as_ref().expect("Internal error, cannot get LocalContext")
  }

  /// Get a mutable reference to the active local context
  /// 
  /// Panics if there is no local context
  #[track_caller]
  pub fn get_local_context_mut (&mut self) -> &mut LocalContext {
    self.local_context.as_mut().expect("Internal error, cannot get LocalContext")
  }


  /// Create a new top level item in the active namespace of an Analyzer
  /// 
  /// Creates an error if there is an existing item with the same identifier
  /// 
  /// Returns the ContextKey associated with the new item
  pub fn create_item<I: Into<ContextItem>> (&mut self, identifier: Identifier, new_item: I, origin: SourceRegion) -> ContextKey {
    let new_item = new_item.into();

    if let Some(shadowed_key) = self.get_active_namespace().local_bindings.get_entry(&identifier) {
      let shadowed_kind = self.context.items.get(shadowed_key).expect("Internal error, shadowed item does not exist").kind();
      let shadowed_location = self.get_active_namespace().local_bindings.get_bind_location(shadowed_key).expect("Internal error, shadowed item has no bind location");

      self.error(origin, format!(
        "{} `{}` shadows existing {} in `{}`, defined at [{}]",
        new_item.kind(), identifier, shadowed_kind, self.get_active_namespace().canonical_name, shadowed_location
      ));
    }

    let key = (|| {
      if let ContextItem::Type(ty) = &new_item {
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

    self.get_active_namespace_mut().local_bindings.set_entry_bound(identifier, key, origin);

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