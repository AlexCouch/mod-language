//! Contextual types used by the semantic analysis system

use core::{
  hint::{ unreachable_unchecked, },
};

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  collections::{ HashMap, hash_map::{ Iter as HashMapIter, IterMut as HashMapIterMut, Keys as HashMapKeys, Values as HashMapValues, }, },
};

use crate::{
  util::{ make_key_type, Unref, UnwrapUnchecked, },
  collections::{ SlotMap, },
  source::{ SourceRegion, },
  common::{ Identifier, },
  ir,
};


/// The main layer of contextual information derived from a semantic analysis session
#[derive(Debug)]
pub struct Context {
  /// Storage for all top-level items discovered by a semantic analyzer
  pub items: SlotMap<GlobalKey, GlobalItem>,
  /// The namespace containing compiler intrinsics for a Context
  pub core_ns: GlobalNamespace,
  /// The module containing compiler intrinsics for a Context
  pub core_mod: GlobalKey,
  /// The root module for the library represented by a Context
  pub lib_mod: GlobalKey,
  /// The key given as a result of a type error
  pub err_ty: GlobalKey,
  /// The return type of functions that do not return a value
  pub void_ty: GlobalKey,
  /// The type of logical expressions
  pub bool_ty: GlobalKey,
  /// Coercible type representing integer literals
  pub int_ty: GlobalKey,
  /// Concrete type the coercible integer type becomes without inferrence
  pub concrete_int_ty: GlobalKey,
  /// Coercible type representing floating point literals
  pub float_ty: GlobalKey,
  /// Concrete type the coercible floating point type becomes without inferrence
  pub concrete_float_ty: GlobalKey,
  /// Anonymous type lookup helper
  pub anon_types: HashMap<TypeData, GlobalKey>,
}

impl Default for Context {
  #[inline] fn default () -> Self { Self::new() }
}

impl Context {
  /// Create a new semantic analysis context, and initialize it with the core primitives, module, and namespace, as well as an empty library root module
  pub fn new () -> Self {
    const PRIMITIVE_TYPES: &[(&str, TypeData)] = &[
      ("void", TypeData::Primitive(PrimitiveType::Void)),
      ("bool", TypeData::Primitive(PrimitiveType::Bool)),
      ("u8",   TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: 8 })),
      ("u16",  TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: 16 })),
      ("u32",  TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: 32 })),
      ("u64",  TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: 64 })),
      ("s8",   TypeData::Primitive(PrimitiveType::Integer { signed: true,  bit_size: 8 })),
      ("s16",  TypeData::Primitive(PrimitiveType::Integer { signed: true,  bit_size: 16 })),
      ("s32",  TypeData::Primitive(PrimitiveType::Integer { signed: true,  bit_size: 32 })),
      ("s64",  TypeData::Primitive(PrimitiveType::Integer { signed: true,  bit_size: 64 })),
      ("f32",  TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: 32 })),
      ("f64",  TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: 64 })),
    ];

    let mut items = SlotMap::default();
    let mut core_mod = Module::new(None, "core".into(), SourceRegion::ANONYMOUS);
    let mut core_ns  = Namespace::default();

    for &(name, ref td) in PRIMITIVE_TYPES.iter() {
      let key = items.insert(Type::new(Some(name.into()), SourceRegion::ANONYMOUS, Some(td.clone())).into());

      core_ns.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      core_mod.local_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      core_mod.export_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
    }

    
    let err_ty = items.insert(Type::new(Some("err_ty".into()), SourceRegion::ANONYMOUS, Some(TypeData::Error)).into());
    let void_ty = core_ns.get_entry("void").unwrap();
    let bool_ty = core_ns.get_entry("bool").unwrap();
    let int_ty = items.insert(Type::new(Some("int".into()), SourceRegion::ANONYMOUS, Some(TypeData::Coercible(CoercibleType::Integer))).into());
    let concrete_int_ty = core_ns.get_entry("s32").unwrap();
    let float_ty = items.insert(Type::new(Some("float".into()), SourceRegion::ANONYMOUS, Some(TypeData::Coercible(CoercibleType::FloatingPoint))).into());
    let concrete_float_ty = core_ns.get_entry("f32").unwrap();


    let core_key = items.insert(core_mod.into());
    core_ns.set_entry_bound("core", core_key, SourceRegion::ANONYMOUS);

    let lib_mod = Module::new(None, "lib".into(), SourceRegion::ANONYMOUS);
    let lib_key = items.insert(lib_mod.into());
    core_ns.set_entry_bound("lib", lib_key, SourceRegion::ANONYMOUS);

    Self {
      items,

      core_ns,
      core_mod: core_key,
      lib_mod: lib_key,

      err_ty,
      void_ty,
      bool_ty,
      int_ty,
      concrete_int_ty,
      float_ty,
      concrete_float_ty,

      anon_types: HashMap::default(),
    }
  }
}


/// A layer of semantic distinction for identifiers in a compilation context
#[derive(Debug, Clone)]
pub struct Namespace<K: std::hash::Hash + Eq + Copy> {
  entries: HashMap<Identifier, K>,
  bind_locations: HashMap<K, SourceRegion>,
}

impl<K: std::hash::Hash + Eq + Copy> Namespace<K> {
  /// Create a new empty Namespace
  pub fn new () -> Self {
    Self {
      entries: HashMap::default(),
      bind_locations: HashMap::default(),
    }
  }
}

impl<K: std::hash::Hash + Eq + Copy> Default for Namespace<K> { #[inline] fn default () -> Self { Self::new() } }


/// A variant of namespace using GlobalKey as its K type
pub type GlobalNamespace = Namespace<GlobalKey>;

/// A variant of namespace using MultiKey as its K type
pub type LocalNamespace = Namespace<MultiKey>;

impl<K: std::hash::Hash + Eq + Copy> Namespace<K> {
  /// Get the location, if any, a Namespace entry was bound at
  pub fn get_bind_location (&self, key: K) -> Option<SourceRegion> {
    self.bind_locations.get(&key).unref()
  }

  /// Determine if a Namespace entry has a binding location
  pub fn has_bind_location (&self, key: K) -> bool {
    self.bind_locations.contains_key(&key)
  }

  /// Register a binding location for a Namespace entry
  pub fn set_bind_location (&mut self, key: K, location: SourceRegion) {
    self.bind_locations.insert(key, location);
  }
  
  /// Get the entry, if any, associated with an identifier in a Namespace
  pub fn get_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> Option<K> {
    self.entries.get(ident.as_ref()).unref()
  }

  /// Get the entry associated with an identifier in a Namespace
  /// # Safety
  /// It is up to the caller to determine whether the identifier provided is bound
  pub unsafe fn get_entry_unchecked<I: AsRef<str> + ?Sized> (&self, ident: &I) -> K {
    *self.entries.get(ident.as_ref()).unwrap_unchecked()
  }

  /// Determine if a namespace entry has a binding location
  pub fn has_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> bool {
    self.entries.contains_key(ident.as_ref())
  }

  
  /// Get the identifier associated with an entry, if any
  pub fn get_entry_ident (&self, key: K) -> Option<&str> {
    for (i, k) in self.entries.iter() {
      if k == &key { return Some(i.as_str()) }
    }

    None
  }

  /// Determine if a Namespace contains a given key
  pub fn has_entry_key (&self, key: K) -> bool {
    self.get_entry_ident(key).is_some()
  }

  /// Register a new namespace entry
  pub fn set_entry<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: K) {
    self.entries.insert(ident.into(), key);
  }

  /// Register a new namespace entry, and immediately bind its source location
  pub fn set_entry_bound<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: K, location: SourceRegion) {
    self.set_entry(ident, key);
    self.set_bind_location(key, location);
  }

  /// Get a key/loc iterator over the bind pairs in a namespace
  pub fn bind_iter (&self) -> HashMapIter<K, SourceRegion> {
    self.bind_locations.iter()
  }

  /// Get a mutable key/loc iterator over the bind pairs in a namespace
  pub fn bind_iter_mut (&mut self) -> HashMapIterMut<K, SourceRegion> {
    self.bind_locations.iter_mut()
  }

  /// Get an iterator over the identifiers in a namespace
  pub fn ident_iter (&self) -> HashMapKeys<Identifier, K> {
    self.entries.keys()
  }

  /// Get an iterator over the keys in a namespace
  pub fn key_iter (&self) -> HashMapValues<Identifier, K> {
    self.entries.values()
  }

  /// Get a key/value iterator over the entry pairs in a namespace
  pub fn entry_iter (&self) -> HashMapIter<Identifier, K> {
    self.entries.iter()
  }

  /// Get a mutable key/value iterator over the entry pairs in a namespace
  pub fn entry_iter_mut (&mut self) -> HashMapIterMut<Identifier, K> {
    self.entries.iter_mut()
  }

  /// Get an immutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
  pub fn iter (&self) -> NamespaceIter<K> {
    NamespaceIter {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter()
    }
  }

  /// Get a mutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
  pub fn iter_mut (&mut self) -> NamespaceIterMut<K> {
    NamespaceIterMut {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter_mut()
    }
  }
}

/// An immutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
pub struct NamespaceIter<'a, K: std::hash::Hash + Eq + Copy> {
  bind_locations: &'a HashMap<K, SourceRegion>,
  entry_iter: HashMapIter<'a, Identifier, K>,
}

impl<'a, K: std::hash::Hash + Eq + Copy> Iterator for NamespaceIter<'a, K> {
  type Item = (&'a Identifier, &'a K, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;

    Some((ident, key, self.bind_locations.get(key)?))
  }
}

/// A mutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
pub struct NamespaceIterMut<'a, K: std::hash::Hash + Eq + Copy> {
  bind_locations: &'a HashMap<K, SourceRegion>,
  entry_iter: HashMapIterMut<'a, Identifier, K>,
}

impl<'a, K: std::hash::Hash + Eq + Copy> Iterator for NamespaceIterMut<'a, K> {
  type Item = (&'a Identifier, &'a mut K, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;
    let owned_key = *key;

    Some((ident, key, self.bind_locations.get(&owned_key)?))
  }
}



/// The basic unit of source for a semantic analyzer,
/// serves as a container for types, values, and other modules
#[derive(Debug, Clone)]
pub struct Module {
  /// Immediate hierarchical ancestor of a Module, if any
  pub parent: Option<GlobalKey>,
  /// The canonical, original name of a Module
  pub canonical_name: Identifier,
  /// Namespaced bindings available locally, inside a Module
  pub local_bindings: GlobalNamespace,
  /// Namespaced bindings available publicly, outside a Module
  pub export_bindings: GlobalNamespace,
  /// The SourceRegion at which a Module was defined
  pub origin: SourceRegion,
}

impl Module {
  /// Create a new semantic analysis Module, and initialize its parent key
  pub fn new (parent: Option<GlobalKey>, canonical_name: Identifier, origin: SourceRegion) -> Self {
    Self {
      parent,
      canonical_name,
      local_bindings: Namespace::default(),
      export_bindings: Namespace::default(),
      origin,
    }
  }
}


/// Data representation of the simplest kind of type,
/// built in primitives such as numbers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveType {
  /// An empty monostate representing nothing
  Void,
  /// A binary state, True or False
  Bool,
  /// An integer numeric value
  Integer {
    /// Determines whether an integer type has a sign bit
    signed: bool,
    /// The total number of bits used by an integer type
    bit_size: usize
  },
  /// A floating point numeric value
  FloatingPoint {
    /// The total number of bits used by a floating point type
    bit_size: usize
  }
}

/// Data representation of a type which is a placeholder for another type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CoercibleType {
  /// Becomes any kind of integer type
  Integer,
  /// Becomes any kind of floating point type
  FloatingPoint,
}

/// Unique data for a Type in a semantic analyzer
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
  /// The error type
  Error,
  /// Built in primitive type such as an integer
  Primitive(PrimitiveType),
  /// A type which can coerce to another type,
  /// these are used by literal values
  Coercible(CoercibleType),
  /// A pointer to another type
  Pointer(GlobalKey),
  /// A function pointer
  Function {
    /// The type(s) of any parameters accepted by a function
    parameter_types: Vec<GlobalKey>,
    /// The type of value returned by a function, if any
    return_type: Option<GlobalKey>
  },
}

impl TypeData {
  /// Determine if a Type is an anonymous Type that is not necessarily associated with a canonical name
  pub fn is_anon (&self) -> bool {
    match self {
      | TypeData::Error    { .. }
      | TypeData::Pointer  { .. }
      | TypeData::Function { .. }
      => true,
      
      | TypeData::Primitive { .. }
      | TypeData::Coercible { .. }
      => false,
    }
  }
}


/// Any Type known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Type {
  /// The canonical, original name of a Type
  pub canonical_name: Option<Identifier>,
  /// The unique data associated with a Type, if it has been defined
  pub data: Option<TypeData>,
  /// The SourceRegion at which a Type was defined
  pub origin: SourceRegion,
}

impl Type {
  /// Create a new Type and initialize its canonical name, origin, and optionally its data
  pub fn new (canonical_name: Option<Identifier>, origin: SourceRegion, data: Option<TypeData>) -> Self {
    Self {
      canonical_name,
      data,
      origin,
    }
  }

  /// Determine if a Type's TypeData is anonymous
  pub fn is_anon (&self) -> Option<bool> {
    self.data.as_ref().map(|td| td.is_anon())
  }
}


/// A wrapper structure for printing types for the user
pub struct TypeDisplay<'a> {
  /// The type being displayed
  pub ty_key: GlobalKey,
  /// The context the type is found in
  pub context: &'a Context,
}

impl<'a> TypeDisplay<'a> {
  /// Create a new TypeDisplay with the same Context as an existing one, but a new GlobalKey
  pub fn descend (&self, ty_key: GlobalKey) -> Self {
    Self {
      ty_key,
      context: self.context,
    }
  }
}

impl<'a> Display for TypeDisplay<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    let ty: &Type =
      self.context.items
        .get(self.ty_key)
        .expect("Internal error, tried to display invalid type")
        .ref_type()
        .expect("Internal error, cannot use TypeDisplay for other variants of GlobalItem");
    
    if let Some(canonical_name) = &ty.canonical_name {
      write!(f, "{}", canonical_name)?;
    } else if let Some(data) = &ty.data {
       match data {
        TypeData::Pointer(ty_key) => { write!(f, "^{}", self.descend(*ty_key))?; },
        TypeData::Error => { write!(f, "err ty")?; },
        TypeData::Coercible(CoercibleType::Integer) => { write!(f, "int")?; },
        TypeData::Coercible(CoercibleType::FloatingPoint) => { write!(f, "float")?; },
        TypeData::Primitive(prim) => match prim {
          PrimitiveType::Void => { write!(f, "void")?; },
          PrimitiveType::Bool => { write!(f, "bool")?; },
          PrimitiveType::Integer { bit_size, signed } => { write!(f, "{}{}", if *signed { "s" } else { "u" }, bit_size)?; },
          PrimitiveType::FloatingPoint { bit_size } => { write!(f, "f{}", bit_size)?; },
        }
        TypeData::Function { parameter_types, return_type } => {
          write!(f, "fn (")?;

          let mut iter = parameter_types.iter().peekable();

          while let Some(param_ty) = iter.next() {
            write!(f, "{}", self.descend(*param_ty))?;

            if iter.peek().is_some() { write!(f, ", ")?; }
          }

          write!(f, ")")?;

          if let Some(return_ty) = return_type {
            write!(f, " -> {}", self.descend(*return_ty))?;
          }
        }
      }
    } else {
      write!(f, "UndefinedType")?;
    }

    Ok(())
  }
}



/// Any Global known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Global {
  /// The canonical, original name of a Global
  pub canonical_name: Identifier,
  /// The Type associated with a Global, if it has been defined
  pub ty: Option<GlobalKey>,
  /// The SourceRegion at which a Global was defined
  pub origin: SourceRegion,
  /// The initializer IR expression for a Global if it has one
  pub initializer: Option<ir::Expression>
}

impl Global {
  /// Create a new Global and initialize its canonical name, origin, and optionally its type
  pub fn new (canonical_name: Identifier, origin: SourceRegion, ty: Option<GlobalKey>) -> Self {
    Self {
      canonical_name,
      ty,
      origin,
      initializer: None,
    }
  }
}

/// Any Function known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Function {
  /// The canonical, original name of a Function
  pub canonical_name: Identifier,
  /// The parameter identifiers and type keys associated with a Function, if it has been defined, and has any
  pub params: Vec<(Identifier, GlobalKey, SourceRegion)>,
  /// The return type key associated with a Function, if it has been defined, and has any
  pub return_ty: Option<GlobalKey>,
  /// The Type associated with a Function, if it has been defined
  pub ty: Option<GlobalKey>,
  /// The SourceRegion at which a Function was defined
  pub origin: SourceRegion,
  /// The IR associated with a Function's body, if it has one
  pub body: Option<ir::Block>
}

impl Function {
  /// Create a new Function and initialize its canonical name, origin, and optionally its type
  pub fn new (canonical_name: Identifier, origin: SourceRegion, ty: Option<GlobalKey>) -> Self {
    Self {
      canonical_name,
      params: Vec::new(),
      return_ty: None,
      ty,
      origin,
      body: None
    }
  }
}


/// A top-level item known by a semantic analyzer
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum GlobalItem {
  Module(Module),
  Type(Type),
  Global(Global),
  Function(Function),
}


/// A local item known by a semantic analyzer
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct LocalItem {
  pub canonical_name: Identifier,
  pub ty: GlobalKey,
  pub is_parameter: bool,
  pub index: usize,
}

/// A reference to either a GlobalItem or a LocalItem
#[allow(missing_docs)]
pub enum MultiRef<'a> {
  LocalItem(&'a LocalItem),
  GlobalItem(&'a GlobalItem),
}

impl<'a> From<&'a LocalItem> for MultiRef<'a> { #[inline] fn from (item: &'a LocalItem) -> Self { Self::LocalItem(item) } }
impl<'a> From<&'a GlobalItem> for MultiRef<'a> { #[inline] fn from (item: &'a GlobalItem) -> Self { Self::GlobalItem(item) } }

/// A mutable reference to either a GlobalItem or a LocalItem
#[allow(missing_docs)]
pub enum MultiMut<'a> {
  LocalItem(&'a mut LocalItem),
  GlobalItem(&'a mut GlobalItem),
}

impl<'a> From<&'a mut LocalItem> for MultiMut<'a> { #[inline] fn from (item: &'a mut LocalItem) -> Self { Self::LocalItem(item) } }
impl<'a> From<&'a mut GlobalItem> for MultiMut<'a> { #[inline] fn from (item: &'a mut GlobalItem) -> Self { Self::GlobalItem(item) } }

make_key_type! {
  /// A SlotMap Key representing a LocalItem
  pub struct LocalKey;
}

/// A Key to either a local variable or a global item
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MultiKey {
  LocalKey(LocalKey),
  GlobalKey(GlobalKey),
}

impl MultiKey {
  /// Convert a MultiKey to its variant
  pub fn local (self) -> Option<LocalKey> { if let Self::LocalKey(key) = self { Some(key) } else { None } }

  /// Convert a MultiKey to its variant
  /// 
  /// # Safety
  /// This function only checks the validity of this conversion if debug_assertions are enabled,
  /// it is up to the caller to determine the safety of this operation
  pub unsafe fn local_unchecked (self) -> LocalKey { if cfg!(debug_assertions) { self.local().unwrap() } else if let Self::LocalKey(key) = self { key } else { unreachable_unchecked() } }

  /// Convert a MultiKey to its variant
  pub fn namespace (self) -> Option<GlobalKey> { if let Self::GlobalKey(key) = self { Some(key) } else { None } }

  /// Convert a MultiKey to its variant
  /// 
  /// # Safety
  /// This function only checks the validity of this conversion if debug_assertions are enabled,
  /// it is up to the caller to determine the safety of this operation
  pub unsafe fn namespace_unchecked (self) -> GlobalKey { if cfg!(debug_assertions) { self.namespace().unwrap() } else if let Self::GlobalKey(key) = self { key } else { unreachable_unchecked() } }
}

impl PartialEq<LocalKey> for MultiKey { #[inline] fn eq (&self, other: &LocalKey) -> bool { if let Self::LocalKey(key) = self { key == other } else { false } } }
impl PartialEq<GlobalKey> for MultiKey { #[inline] fn eq (&self, other: &GlobalKey) -> bool { if let Self::GlobalKey(key) = self { key == other } else { false } } }

impl From<LocalKey> for MultiKey { #[inline] fn from (key: LocalKey) -> Self { Self::LocalKey(key) } }
impl From<GlobalKey> for MultiKey { #[inline] fn from (key: GlobalKey) -> Self { Self::GlobalKey(key) } }


/// A function or global initializer expression's contextual information
#[derive(Debug, Clone)]
pub struct LocalContext {
  /// All local variables known by a LocalContext,
  /// not all of which may be accessible from the namespace stack
  pub variables: SlotMap<LocalKey, LocalItem>,
  /// The number of function parameter variables registered in a LocalContext
  pub parameter_count: usize,
  /// The number of local variables registered in a LocalContext
  pub local_count: usize,
  /// The stack of namespaces in scope for a LocalContext,
  /// identifier lookup reverse iterates this stack to find variables,
  /// ending at the global namespace
  pub stack_frames: Vec<LocalNamespace>,
}

impl Default for LocalContext { #[inline] fn default () -> Self { Self::new() } }

impl LocalContext {
  /// Create a new LocalContext with a single empty stack frame
  pub fn new () -> Self {
    Self {
      variables: SlotMap::default(),
      parameter_count: 0,
      local_count: 0,
      stack_frames: vec![ Namespace::default() ]
    }
  }

  /// Create a local variable in a LocalContext
  pub fn create_variable (&mut self,
    canonical_name: Identifier,
    ty: GlobalKey,
    is_parameter: bool,
    origin: SourceRegion,
  ) -> LocalKey {
    let count = if is_parameter { &mut self.parameter_count } else { &mut self.local_count };

    let index = *count;

    *count += 1;

    let key = self.variables.insert(LocalItem {
      canonical_name: canonical_name.clone(),
      ty,
      is_parameter,
      index
    });

    self.set_variable(canonical_name, key.into(), origin);

    key
  }

  /// Set the value of a local variable in the current stack frame of a LocalContext
  pub fn set_variable<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, value: MultiKey, origin: SourceRegion) {
    self.stack_frames.last_mut().unwrap().set_entry_bound(ident, value, origin)
  }
  
  /// Try to lookup a local variable by traversing down the stack from a LocalContext's top namespace
  pub fn get_variable<I: AsRef<str>> (&self, ident: &I) -> Option<MultiKey> {
    for ns in self.stack_frames.iter().rev() {
      let ns_entry = ns.get_entry(ident);

      if ns_entry.is_some() { return ns_entry }
    }

    None
  }

  /// Create a new stack frame namespace in a LocalContext
  pub fn push_stack_frame (&mut self) {
    self.stack_frames.push(Namespace::default())
  }

  /// Pop a namespace off the frame stack in a LocalContext
  /// 
  /// Panics if there is only a single stack frame left
  pub fn pop_stack_frame (&mut self) -> LocalNamespace {
    assert!(self.stack_frames.len() > 1, "Internal error, cannot pop final stack frame of LocalContext");

    self.stack_frames.pop().unwrap()
  }
}

impl GlobalItem {
  /// Get the NamespaceKind of a GlobalItem
  pub fn kind (&self) -> NamespaceKind {
    match self {
      Self::Module(_)   => NamespaceKind::Module,
      Self::Type(_)     => NamespaceKind::Type,
      Self::Global(_)   => NamespaceKind::Global,
      Self::Function(_) => NamespaceKind::Function,
    }
  }


  /// Convert a reference to a GlobalItem into an optional reference to a Module
  #[inline] pub fn ref_module (&self) -> Option<&Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a reference to a GlobalItem into an optional reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_module_unchecked (&self) -> &Module { if cfg!(debug_assertions) { self.ref_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Module
  #[inline] pub fn mut_module (&mut self) -> Option<&mut Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_module_unchecked (&mut self) -> &mut Module { if cfg!(debug_assertions) { self.mut_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a GlobalItem into an optional reference to a Type
  #[inline] pub fn ref_type (&self) -> Option<&Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a reference to a GlobalItem into an optional reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_type_unchecked (&self) -> &Type { if cfg!(debug_assertions) { self.ref_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Type
  #[inline] pub fn mut_type (&mut self) -> Option<&mut Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_type_unchecked (&mut self) -> &mut Type { if cfg!(debug_assertions) { self.mut_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a GlobalItem into an optional reference to a Global
  #[inline] pub fn ref_global (&self) -> Option<&Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a reference to a GlobalItem into an optional reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_global_unchecked (&self) -> &Global { if cfg!(debug_assertions) { self.ref_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Global
  #[inline] pub fn mut_global (&mut self) -> Option<&mut Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_global_unchecked (&mut self) -> &mut Global { if cfg!(debug_assertions) { self.mut_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a GlobalItem into an optional reference to a Function
  #[inline] pub fn ref_function (&self) -> Option<&Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a reference to a GlobalItem into an optional reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_function_unchecked (&self) -> &Function { if cfg!(debug_assertions) { self.ref_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Function
  #[inline] pub fn mut_function (&mut self) -> Option<&mut Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a GlobalItem into an optional mutable reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_function_unchecked (&mut self) -> &mut Function { if cfg!(debug_assertions) { self.mut_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }
}

impl From<Module> for GlobalItem { #[inline] fn from (item: Module) -> GlobalItem { Self::Module(item) } }
impl From<Type> for GlobalItem { #[inline] fn from (item: Type) -> GlobalItem { Self::Type(item) } }
impl From<Global> for GlobalItem { #[inline] fn from (item: Global) -> GlobalItem { Self::Global(item) } }
impl From<Function> for GlobalItem { #[inline] fn from (item: Function) -> GlobalItem { Self::Function(item) } }

/// The kind of a top-level item known by a semantic analyzer
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NamespaceKind {
  Module,
  Type,
  Global,
  Function,
}

impl Display for NamespaceKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Module => "Module",
      Self::Type => "Type",
      Self::Global => "Global",
      Self::Function => "Function",
    })
  }
}

make_key_type! {
  /// A key referring to a top-level item in a semantic analyzer
  pub struct GlobalKey;
}