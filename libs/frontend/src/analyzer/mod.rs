//! The semantic analyzer

use mod_utils::{ UnwrapUnchecked, };
use mod_common::{ Identifier, };

use crate::{
  session::{ SESSION, MessageKind, Message, },
  source::{ SourceRegion, },
  ast::{ Item, },
  ctx::{ Context, Module, Namespace, ContextItem, ContextItemKind, ContextKey, LocalContext, },
};


pub mod passes;


/// The core interface structure for semantic analysis
pub struct Analyzer {
  /// All contextual information used by a semantic analyzer
  pub context: Context,
  /// A stack of active modules and namespaces
  pub active_mod_and_ns: Vec<(ContextKey, Vec<ContextKey>)>,
  /// The local context being analyzed, if any
  pub local_context: Option<LocalContext>,
  /// A counter used to track initialization order of Globals
  pub global_rank_counter: usize,
}


impl Default for Analyzer {
  #[inline] fn default () -> Self { Self::new() }
}


impl Analyzer {
  /// Create a new semantic analyzer
  pub fn new () -> Self {
    let context = Context::default();
    let active_mod_and_ns = vec![ (context.main_mod, vec![ context.main_ns ]) ];

    Self {
      context,
      active_mod_and_ns,
      local_context: None,
      global_rank_counter: 0,
    }
  }

  /// Run a semantic analyzer on its ast,
  /// consuming the analyzer in the process
  pub fn analyze (mut self, mut ast: Vec<Item>) -> (Context, Vec<Item>) {
    self.run_passes(&mut ast);

    (self.context, ast)
  }

  
  /// Get the current value of the global rank counter, and then increment it
  /// 
  /// If the active module is not the main module, global rank is always `usize::MAX`
  pub fn get_global_rank (&mut self) -> usize {
    if self.get_active_module_key() == self.context.main_mod {
      let rank = self.global_rank_counter;
      self.global_rank_counter += 1;
      rank
    } else {
      std::usize::MAX
    }
  }

  
  /// Push a new active module and its root namespace on an Analyzer's stack
  pub fn push_active_module (&mut self, mod_key: ContextKey) {
    let module = self.context.items.get(mod_key).unwrap().ref_module().unwrap();
    let ns_key = module.namespace;
    self.active_mod_and_ns.push((mod_key, vec![ ns_key ]));
  }

  /// Pops and returns active module key from an Analyzer's stack
  ///
  /// Panics if there is only one (the main module) active module left on the stack,
  /// or if there is more than one active namespace in the active module's stack
  pub fn pop_active_module (&mut self) -> ContextKey {
    assert!(
      self.active_mod_and_ns.len() > 1,
      "Internal error, cannot pop main module"
    );

    assert!(
      self.active_mod_and_ns.last().unwrap().1.len() == 1,
      "Internal error, cannot pop module with active namespaces"
    );

    let (mod_key, _ns_vec) = unsafe { self.active_mod_and_ns.pop().unwrap_unchecked() };

    mod_key
  }

  
  /// Push a new active namespace key on an Analyzer's stack
  pub fn push_active_namespace (&mut self, ns_key: ContextKey) {
    unsafe { self.active_mod_and_ns.last_mut().unwrap_unchecked().1.push(ns_key) };
  }

  /// Pops and returns active namespace key from an Analyzer's stack
  ///
  /// Panics if there is only one (the module root) active namespace left on the stack
  pub fn pop_active_namespace (&mut self) -> ContextKey {
    assert!(
      self.active_mod_and_ns.last().unwrap().1.len() > 1,
      "Internal error, cannot pop module root namespace"
    );

    unsafe { self.active_mod_and_ns.last_mut().unwrap_unchecked().1.pop().unwrap_unchecked() }
  }


  
  /// Get they key of the active Module in an Analyzer
  pub fn get_active_module_key (&self) -> ContextKey {
    unsafe { self.active_mod_and_ns.last().unwrap_unchecked().0 }
  }

  /// Get an immutable reference to the active Module in an Analyzer
  pub fn get_active_module (&self) -> &Module {
    unsafe { self.context.items.get_unchecked(self.get_active_module_key()).ref_module_unchecked() }
  }
  
  /// Get a mutable reference to the active Module in an Analyzer
  pub fn get_active_module_mut (&mut self) -> &mut Module {
    unsafe { self.context.items.get_unchecked_mut(self.get_active_module_key()).mut_module_unchecked() }
  }


  /// Get they key of the active Namespace in an Analyzer
  pub fn get_active_namespace_key (&self) -> ContextKey {
    unsafe { *self.active_mod_and_ns.last().unwrap_unchecked().1.last().unwrap_unchecked() }
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

    let kind = new_item.kind();

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

    match kind {
      ContextItemKind::Module => { },
      ContextItemKind::Namespace => self.context.namespaces.push(key),
      ContextItemKind::Type => self.context.types.push(key),
      ContextItemKind::Global => self.context.globals.push(key),
      ContextItemKind::Function => self.context.functions.push(key),
    }

    key
  }


  /// Create a new dependency declaration Module, and its namespace
  /// 
  /// Panics if there is an existing module with the same identifier
  /// 
  /// Returns the ContextKey associated with the new Module
  pub fn create_module (&mut self, identifier: Identifier, origin: SourceRegion) -> ContextKey {
    let module_key = self.context.items.insert(Module::new(identifier.clone(), false, ContextKey::NULL, Some(origin)).into());
    let namespace_key = self.context.items.insert(Namespace::new(module_key, None, identifier.clone(), origin).into());

    unsafe { self.context.items.get_unchecked_mut(module_key).mut_module_unchecked() }.namespace = namespace_key;

    self.context.modules.insert(identifier, module_key).expect_none("Internal error, overwrote existing Module");

    module_key
  }


  /// Create a Message in the Source of the AST of an Analyzer
  #[allow(clippy::mut_from_ref)]
  pub fn message (&self, origin: SourceRegion, kind: MessageKind, message: String) -> &mut Message {
    SESSION.message(origin, kind, message)
  }

  /// Create a notice in the Source of the AST of an Analyzer
  #[allow(clippy::mut_from_ref)]
  pub fn notice (&self, origin: SourceRegion, message: String) -> &mut Message {
    self.message(origin, MessageKind::Notice, message)
  }

  /// Create a warning in the Source of the AST of an Analyzer
  #[allow(clippy::mut_from_ref)]
  pub fn warning (&self, origin: SourceRegion, message: String) -> &mut Message {
    self.message(origin, MessageKind::Warning, message)
  }

  /// Create an error in the Source of the AST of an Analyzer
  #[allow(clippy::mut_from_ref)]
  pub fn error (&self, origin: SourceRegion, message: String) -> &mut Message {
    self.message(origin, MessageKind::Error, message)
  }
}