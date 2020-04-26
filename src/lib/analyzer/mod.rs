//! The semantic analyzer

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
  /// If the local namespace stack does not contain the identifier, the top namespace of the item namespace stack is traversed,
  /// after that if `traverse_core` is `true`, the core namespace is tried,
  /// and finally if no entry is found then None is returned
  pub fn lookup_ident<S: AsRef<str>> (&self, ident: &S, traverse_core: bool) -> Option<NamespaceKey> {
    let ident = ident.as_ref();

    for lns in self.local_namespaces.iter().rev() {
      let key = lns.get_entry(ident);

      if key.is_some() { return key }
    }

    let res = self.item_namespaces.last().and_then(|ins| ins.get_entry(ident));

    if traverse_core {
      res.or_else(|| self.context.core.get_entry(ident))
    } else {
      res
    }
  }

  /// Insert a new identifier binding into the item namespace stack
  /// 
  /// If there is already a binding for the given identifier
  /// in the top frame of the namespace stack, a new namespace stack frame is generated
  pub fn bind_item_ident<I: Into<String>, K: Into<NamespaceKey>> (&mut self, ident: I, key: K, origin: Option<SourceRegion>) {
    let ident = ident.into();
    let key = key.into();
    
    let tns = if let Some(tns) = self.item_namespaces.last_mut().allow_if_not(|tns| tns.has_entry(&ident)) {
      tns
    } else {
      self.item_namespaces.push(Namespace::default());
      self.item_namespaces.last_mut().unwrap()
    };
    
    tns.add_entry(ident, key);

    if let Some(origin) = origin {
      tns.set_bind_location(key, origin);
    }
  }

  /// Find the identifier associated with a given namespace key, if any
  pub fn find_ident_from_key<K: Into<NamespaceKey>> (&self, key: K, traverse_core: bool) -> Option<&str> {
    let key = &key.into();

    for lns in self.local_namespaces.iter() {
      for (i, k) in lns.entry_iter() {
        if k == key { return Some(i) }
      }
    }

    if let Some(ins) = self.item_namespaces.last() {
      for (i, k) in ins.entry_iter() {
        if k == key { return Some(i) }
      }
    }

    if traverse_core {
      for (i, k) in self.context.core.entry_iter() {
        if k == key { return Some(i) }
      }
    }

    None
  }

  /// Get the SourceRegion bound as the origin of an item in the active namespace, if there is any
  pub fn get_item_namespace_origin<K: Into<NamespaceKey>> (&self, key: K) -> Option<SourceRegion> {
    let key = key.into();

    for ins in self.item_namespaces.iter() {
      let loc = ins.get_bind_location(key);

      if loc.is_some() { return loc }
    }

    None
  }

  
  /// Determine if there is any SourceRegion bound as the origin of an item in the active namespace
  pub fn has_item_namespace_origin<K: Into<NamespaceKey>> (&self, key: K) -> bool {
    self.get_item_namespace_origin(key).is_some()
  }

  /// Bind an origin SourceRegion to an item in the active namespaces
  /// 
  /// Panics if there is no namespace entry for the given key, or if the entry already has an origin bound
  pub fn bind_item_namespace_origin<K: Into<NamespaceKey>> (&mut self, key: K, origin: SourceRegion) {
    let key = key.into();

    for ins in self.item_namespaces.iter_mut() {
      if ins.has_entry_key(key) {
        return ins.set_bind_location(key, origin)
      }
    }

    panic!("Internal error: Tried to bind origin for invalid namespace entry");
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