//! The semantic analyzer

use std::{
  collections::{ HashMap, },
};

use crate::{
  util::{ UnwrapUnchecked, Unref, },
  extras::{ AllowIf, },
  session::{ SESSION, MessageKind, },
  source::{ SourceRegion, },
  ast::{ Item, },
  ctx::{ Context, LocalContext, Namespace, Module, TypeData, ModuleKey, TypeKey, NamespaceKey, },
};

mod passes;





/// The core interface structure for semantic analysis
pub struct Analyzer<'a> {
  /// The AST being analyzed
  pub ast: &'a [Item],

  /// All contextual information used by a semantic analyzer
  pub context: Context,

  /// A stack of namespaces which identifier lookups traverse the top of
  pub item_namespaces: Vec<Namespace>,

  /// A stack of namespaces which identifier lookups traverse the full depth of
  pub local_namespaces: Vec<Namespace>,

  /// A stack of active modules being analyzed
  pub active_modules: Vec<ModuleKey>,

  /// The active local context being analyzed, if any
  pub local_context: Option<LocalContext>,
}


impl<'a> Analyzer<'a> {
  /// Create a new semantic analyzer
  pub fn new (ast: &'a [Item]) -> Self {
    let context = Context::default();
    let active_modules = vec![ context.lib ];

    Self {
      ast,

      context,
      item_namespaces: vec![ Namespace::default() ],
      local_namespaces: vec![ ],

      active_modules,
      local_context: None,
    }
  }


  /// Traverse the user defined namespace stack of an Analyzer,
  /// looking for a key binding matching an identifier
  /// 
  /// Returns a NamespaceKey if one is found
  /// 
  /// This is different from `lookup_ident` in that it does not traverse the core namespace;
  /// see `lookup_ident` docs for more information on traversal
  pub fn lookup_user_ident<S: AsRef<str>> (&self, ident: &S) -> Option<NamespaceKey> {
    let ident = ident.as_ref();

    for lns in self.local_namespaces.iter().rev() {
      let key = lns.get(ident);

      if key.is_some() { return key.unref() }
    }

    self.item_namespaces
      .last()
      .and_then(|ins| ins.get(ident).unref())
  }

  /// Traverse the namespace stack of an Analyzer,
  /// looking for a key binding matching an identifier
  /// 
  /// Returns a NamespaceKey if one is found
  /// 
  /// # Traversal order
  /// 
  /// `local_namespace N -> ... -> local_namespace 0 -> item_namespace N -> core`
  /// 
  /// When looking for an identifier, the local namespace stack is traversed in descending order.
  /// If the local namespace stack does not contain the identifier, the namespace top of the item namespace stack is traversed,
  /// after that the core namespace is tried, (if you want to skip the core namespace use `lookup_user_ident`),
  /// finally if no entry is found in core then None is returned
  pub fn lookup_ident<S: AsRef<str>> (&self, ident: &S) -> Option<NamespaceKey> {
    self.lookup_user_ident(ident).or_else(|| self.context.core.get(ident.as_ref()).unref())
  }

  /// Insert a new identifier binding into the namespace stack
  /// 
  /// If there is already a binding for the given identifier
  /// in the top frame of the namespace stack, a new namespace stack frame is generated
  pub fn bind_ident<I: Into<String>, K: Into<NamespaceKey>> (&mut self, ident: I, key: K) {
    let ident = ident.into();
    let key = key.into();
    
    if let Some(tns) = self.item_namespaces.last_mut().allow_if_not(|tns| tns.contains_key(&ident)) {
      tns.insert(ident, key);
    } else {
      let mut nns = HashMap::default();
      nns.insert(ident, key);
      self.item_namespaces.push(nns);
    }
  }

  /// Add a SourceRegion referencing an Item in an Analyzer's Context
  pub fn add_reference<K: Into<NamespaceKey>> (&mut self, key: K, region: SourceRegion) {
    self.context.reference_locations.entry(key.into()).or_insert_with(Vec::default).push(region)
  }

  /// Get a shared TypeKey from an anonymous Type such as a function type
  /// 
  /// If this is the first time the given Type has been generated, a new key is created,
  /// otherwise the key of the matching Type in the types slotmap is returned
  pub fn resolve_anon_type_key (&mut self, ty: TypeData) -> TypeKey {
    let ty = Some(ty.into());

    if let Some(existing_key) = self.context.types.find_key(&ty) {
      existing_key
    } else {
      self.context.types.insert(ty)
    }
  }

  
  /// Push a new active module key and namespace on an Analyzer's stack
  /// 
  /// Panics if there is any local context or namespace
  pub fn push_active_module (&mut self, key: ModuleKey) {
    assert!(self.local_context.is_none() && self.local_namespaces.is_empty(), "Internal error, cannot create new active module with active local context or namespace");
    self.active_modules.push(key);
    self.item_namespaces.push(Namespace::default());
  }

  /// Pop and returns active module key and namespace from an Analyzer's stack
  ///
  /// Panics if there is only one (the root) active module and namespace left on the stack,
  /// or if the item namespaces and active modules counts are not identical
  pub fn pop_active_module (&mut self) -> (ModuleKey, Namespace) {
    assert!(self.item_namespaces.len() == self.active_modules.len() && self.active_modules.len() > 1, "Internal error, unexpected active module / item namespace state while popping active module");
    unsafe { (
      self.active_modules.pop().unwrap_unchecked(),
      self.item_namespaces.pop().unwrap_unchecked()
    ) }
  }


  /// Get they key of the active Module in an Analyzer
  pub fn get_active_module_key (&self) -> ModuleKey {
    unsafe { self.active_modules.last().unref().unwrap_unchecked() }
  }

  /// Get an immutable reference to the active Module in an Analyzer
  pub fn get_active_module (&self) -> &Module {
    unsafe { self.context.modules.get_unchecked(self.get_active_module_key()).as_ref().unwrap_unchecked() }
  }
  
  /// Get a mutable reference to the active Module in an Analyzer
  pub fn get_active_module_mut (&mut self) -> &mut Module {
    unsafe { self.context.modules.get_unchecked_mut(self.get_active_module_key()).as_mut().unwrap_unchecked() }
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