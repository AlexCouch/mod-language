//! The semantic analyzer

use std::{
  collections::{ HashMap, },
};

use crate::{
  util::{ UnwrapUnchecked, Unref, },
  extras::{ AllowIf, },
  source::{ SourceRegion, MessageKind, },
  ast::{ AST, },
  ctx::{ Context, LocalContext, Namespace, Module, TypeData, ModuleKey, TypeKey, NamespaceKey, },
};

mod passes;





/// The core interface structure for semantic analysis
pub struct Analyzer<'a> {
  /// The AST being analyzed
  pub ast: &'a AST<'a>,

  /// All contextual information used by a semantic analyzer
  pub context: Context,

  /// A stack of namespaces which identifier lookups traverse down
  pub namespaces: Vec<Namespace>,
  /// A stack of placeholders to enable deep popping of namespaces
  pub namespace_markers: Vec<usize>,

  /// The active module being analyzed
  pub active_module: ModuleKey,
  /// The active local context being analyzed, if any
  pub local_context: Option<LocalContext>,
}


impl<'a> Analyzer<'a> {
  /// Create a new semantic analyzer
  pub fn new (ast: &'a AST) -> Self {
    let context = Context::default();
    let active_module = context.lib;

    Self {
      ast,

      context,
      namespaces: vec![ Namespace::default() ],
      namespace_markers: vec![ ],

      active_module,
      local_context: None,
    }
  }


  /// Traverse the namespace stack of an Analyzer,
  /// looking for a key binding matching an identifier
  /// 
  /// Returns a NamespaceKey if one is found
  /// 
  /// # Traversal order
  /// 
  /// `namespace N -> ... -> namespace 0 -> core`
  /// 
  /// When looking for an identifier, the namespace stack is traversed in descending order.
  /// If the last namespace on the stack does not have the required identifier,
  /// the core namespace is tried, finally if no entry is found in core then None is returned
  /// 
  /// This is different from `lookup_user_ident` in that it traverses the core namespace
  pub fn lookup_ident<S: AsRef<str>> (&self, ident: &S) -> Option<NamespaceKey> {
    let ident = ident.as_ref();

    for ns in self.namespaces.iter().rev() {
      let key = ns.get(ident);

      if key.is_some() { return key.unref() }
    }

    self.context.core.get(ident).unref()
  }

  /// Traverse the user defined namespace stack of an Analyzer,
  /// looking for a key binding matching an identifier
  /// 
  /// Returns a NamespaceKey if one is found
  /// 
  /// # Traversal order
  /// 
  /// `namespace N -> ... -> namespace 0`
  /// 
  /// When looking for an identifier, the namespace stack is traversed in descending order.
  /// If the last namespace on the stack does not have the required identifier, None is returned
  /// 
  /// This is different from `lookup_ident` in that it does not traverse the core namespace
  pub fn lookup_user_ident<S: AsRef<str>> (&self, ident: &S) -> Option<NamespaceKey> {
    let ident = ident.as_ref();

    for ns in self.namespaces.iter().rev() {
      let key = ns.get(ident);

      if key.is_some() { return key.unref() }
    }

    self.context.core.get(ident).unref()
  }

  /// Insert a new identifier binding into the namespace stack
  /// 
  /// If there is already a binding for the given identifier
  /// in the top frame of the namespace stack, a new namespace stack frame is generated
  pub fn bind_ident<I: Into<String>, K: Into<NamespaceKey>> (&mut self, ident: I, key: K) {
    let ident = ident.into();
    let key = key.into();
    
    if let Some(tns) = self.namespaces.last_mut().allow_if_not(|tns| tns.contains_key(&ident)) {
      tns.insert(ident, key);
    } else {
      let mut nns = HashMap::default();
      nns.insert(ident, key);
      self.namespaces.push(nns);
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

  /// Get an immutable reference to the active Module in an Analyzer
  pub fn get_active_module (&self) -> &Module {
    unsafe { self.context.modules.get_unchecked(self.active_module).as_ref().unwrap_unchecked() }
  }
  
  /// Get a mutable reference to the active Module in an Analyzer
  pub fn get_active_module_mut (&mut self) -> &mut Module {
    unsafe { self.context.modules.get_unchecked_mut(self.active_module).as_mut().unwrap_unchecked() }
  }


  /// Create a Message in the Source of the AST of an Analyzer
  pub fn message (&self, origin: SourceRegion, kind: MessageKind, message: String) {
    self.ast.stream.source.message(Some(origin), kind, message)
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