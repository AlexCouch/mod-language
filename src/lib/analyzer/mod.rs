//! The semantic analyzer

use crate::{
  util::{ UnwrapUnchecked, Unref, },
  session::{ SESSION, MessageKind, },
  source::{ SourceRegion, },
  common::{ Identifier, },
  ast::{ Item, },
  ctx::{ Context, LocalContext, Namespace, Module, Type, Global, Function, TypeData, ModuleKey, TypeKey, GlobalKey, FunctionKey, NamespaceKey, },
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


macro_rules! make_definer {
  (($plural:ident) pub fn $fn_name:ident (&mut self, origin: SourceRegion, identifier: &Identifier, $data:ident: $ty:ident) -> $key:ident; $($rest:tt)*) => {
    /// Defines an item of the given kind in an Analyzer's active module and namespace, and returns its key
    /// 
    /// Will produce session errors if the definition shadows:
    /// + An existing definition
    /// + An import
    /// + An identifier expected to refer to another kind of item
    /// 
    /// 
    /// Under these circumstances, the item is still created in order to prevent error spam
    pub fn $fn_name (&mut self, origin: SourceRegion, identifier: &Identifier, $data: $ty) -> $key {
      if let Some(existing_binding) = self.lookup_ident(identifier, false) {
        if let Some(&existing_key) = self.get_active_module().$plural.get(identifier.as_ref()) {
          // Found an item known to exist in this module
          let existing_data = unsafe { self.context.$plural.get_unchecked_mut(existing_key) };
  
          if let Some(existing_data) = existing_data {
            // Item was already defined in this module
            let message = format!(
              concat!("Duplicate definition for ", stringify!($ty), "`{}`, previous definition is at [{}]"),
              identifier.as_ref(),
              existing_data.origin
                .expect(concat!("Internal error: Found duplicate ", stringify!($ty), " definition but existing ", stringify!($ty), " had no source attribution"))
            );
            
            self.error(origin, message);
          } else {
            // Item was not defined but has been imported somewhere
            existing_data.replace($data);
            self.add_reference(existing_key, origin);
            self.bind_item_namespace_origin(existing_key, origin);
            return existing_key
          }
        } else if let Some(import_origin) = self.get_item_namespace_origin(existing_binding) {
          // Item shadows an import from another module
          self.error(origin, format!(
            concat!(stringify!($ty), " definition `{}` shadows an {} item imported at [{}]"),
            identifier.as_ref(), existing_binding.kind(), import_origin
          ));
        } else {
          // Item is something that has been referenced but not attributed to any location
          if let NamespaceKey::$ty(data_key) = existing_binding {
            // Item was expected to be this kind, this is fine
            let existing_data = unsafe { self.context.$plural.get_unchecked_mut(data_key) };
  
            // TODO should this be removed?
            // insanity check, this should never happen
            assert!(existing_data.is_none(), concat!("Internal error: Found unbound ", stringify!($ty), " with definition"));
  
            existing_data.replace($data);
            self.get_active_module_mut().$plural.insert(identifier.into(), data_key);
            self.add_reference(data_key, origin);
            self.bind_item_namespace_origin(data_key, origin);
            return data_key
          } else {
            // Item was expected to be some other kind
            self.error(self.get_first_reference(existing_binding), format!(
              concat!("Identifier `{}` refers to a ", stringify!($ty), ", defined at [{}]; expected a {}"),
              identifier,
              origin,
              existing_binding.kind()
            ));
          }
        }
      }
      
      // Either this is the first time we've encountered this identifier,
      // or we've fallen through from an error condition,
      // and this definition is shadowing to prevent error spam
      let key = self.context.$plural.insert(Some($data));
      self.get_active_module_mut().$plural.insert(identifier.into(), key);
      self.bind_item_ident(identifier, key, Some(origin));
      self.add_reference(key, origin);
      key
    }

    make_definer!{ $($rest)* }
  };

  () => {};
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
  pub fn bind_item_ident<I: Into<String>, K: Into<NamespaceKey>> (&mut self, ident: I, key: K, origin: Option<SourceRegion>) {
    let ident = ident.into();
    let key = key.into();
        
    let tns = self.item_namespaces.last_mut().expect("Internal error, no item namespace on the stack");
    
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

  /// Get the first location a namespacekey was referenced
  /// 
  /// Panics if there is not a reference
  pub fn get_first_reference<K: Into<NamespaceKey>> (&self, key: K) -> SourceRegion {
    *self.context.reference_locations
      .get(&key.into())
      .expect("Internal error: Item created with no reference locations")
      .first()
      .expect("Internal error: Item reference array was empty")
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

  
  make_definer! {
    (modules) pub fn define_module (&mut self, origin: SourceRegion, identifier: &Identifier, module: Module) -> ModuleKey;
    (types) pub fn define_type (&mut self, origin: SourceRegion, identifier: &Identifier, r#type: Type) -> TypeKey;
    (globals) pub fn define_global (&mut self, origin: SourceRegion, identifier: &Identifier, global: Global) -> GlobalKey;
    (functions) pub fn define_function (&mut self, origin: SourceRegion, identifier: &Identifier, function: Function) -> FunctionKey;
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