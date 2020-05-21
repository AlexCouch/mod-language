//! Contextual types used by the semantic analysis system

use core::{
  hint::{ unreachable_unchecked, },
};

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  collections::{ HashMap, },
};

use mod_utils::{
  make_key_type, Unref, UnwrapUnchecked,
  collections::{ SlotMap, Map, map::{ SliceIter, PairIter, PairIterMut, }, },
};

use mod_common::{ Identifier, };

use crate::{
  source::{ SourceRegion, },
  ir,
};


/// The main layer of contextual information derived from a semantic analysis session
#[derive(Debug)]
pub struct Context {
  /// Storage for all top-level items discovered by a semantic analyzer
  pub items: SlotMap<ContextKey, ContextItem>,

  /// The Bindspace containing compiler intrinsics for a Context, as well as Modules encountered during analysis
  pub core_bs: GlobalBindspace,
  /// The Namespace containing compiler intrinsics for a Context
  pub core_ns: ContextKey,
  /// The Module containing compiler intrinsics for a Context,
  pub core_mod: ContextKey,

  /// The root Namespace for the module represented by a Context
  pub main_ns: ContextKey,
  /// The root Module represented by a Context
  pub main_mod: ContextKey,

  /// The key given as a result of a type error
  pub err_ty: ContextKey,
  /// Coercible type representing integer literals
  pub int_ty: ContextKey,
  /// Coercible type representing floating point literals
  pub float_ty: ContextKey,

  /// The return type of functions that do not return a value
  pub void_ty: ContextKey,
  /// The type of logical expressions
  pub bool_ty: ContextKey,

  /// Concrete type the coercible integer type becomes without inferrence
  pub concrete_int_ty: ContextKey,
  /// Concrete type the coercible floating point type becomes without inferrence
  pub concrete_float_ty: ContextKey,

  /// Module lookup helper
  pub modules: HashMap<Identifier, ContextKey>,
  /// Anonymous type lookup helper
  pub anon_types: HashMap<TypeData, ContextKey>,

  /// A list of all types in a context
  pub types: Vec<ContextKey>,
  /// A list of all namespaces in a context
  pub namespaces: Vec<ContextKey>,
  /// A list of all globals in a context
  pub globals: Vec<ContextKey>,
  /// A list of all functions in a context
  pub functions: Vec<ContextKey>,
}

impl Default for Context {
  #[inline] fn default () -> Self { Self::new() }
}

impl Context {
  /// Create a new semantic analysis context, and initialize it with the core primitives,
  /// namespace, and bindspace, module, as well as a root module
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

    let mut modules = HashMap::default();
    let anon_types = HashMap::default();
    let mut namespaces = Vec::new();
    let mut types = Vec::new();
    let globals = Vec::new();
    let functions = Vec::new();

    let mut items: SlotMap<ContextKey, ContextItem> = SlotMap::default();
    let core_ns = items.insert(Namespace::new(ContextKey::default(), None, "core".into(), SourceRegion::ANONYMOUS).into());
    let core_mod = items.insert(Module::new("core".into(), false, core_ns, None).into());

    unsafe { items.get_unchecked_mut(core_ns).mut_namespace_unchecked() }.parent_module = core_mod;

    let mut core_bs = Bindspace::default();

    for &(name, ref td) in PRIMITIVE_TYPES.iter() {
      let key = items.insert(Type::new(Some(core_mod), Some(core_ns), Some(name.into()), SourceRegion::ANONYMOUS, Some(td.clone())).into());
      types.push(key);

      core_bs.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      
      let core_ns = unsafe { items.get_unchecked_mut(core_ns).mut_namespace_unchecked() };
      core_ns.local_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      core_ns.export_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
    }

    let err_ty = items.insert(Type::new(None, None, Some("{{ type error }}".into()), SourceRegion::ANONYMOUS, Some(TypeData::Error)).into());
    types.push(err_ty);
    let int_ty = items.insert(Type::new(None, None, Some("int".into()), SourceRegion::ANONYMOUS, Some(TypeData::Coercible(CoercibleType::Integer))).into());
    types.push(int_ty);
    let float_ty = items.insert(Type::new(None, None, Some("float".into()), SourceRegion::ANONYMOUS, Some(TypeData::Coercible(CoercibleType::FloatingPoint))).into());
    types.push(float_ty);

    let void_ty = core_bs.get_entry("void").unwrap();
    let bool_ty = core_bs.get_entry("bool").unwrap();

    let concrete_int_ty = core_bs.get_entry("s32").unwrap();
    let concrete_float_ty = core_bs.get_entry("f32").unwrap();

    core_bs.set_entry_bound("core", core_ns, SourceRegion::ANONYMOUS);

    let main_ns = items.insert(Namespace::new(ContextKey::default(), None, "module".into(), SourceRegion::ANONYMOUS).into());
    core_bs.set_entry_bound("module", main_ns, SourceRegion::ANONYMOUS);

    let main_mod = items.insert(Module::new("main".into(), true, main_ns, None).into());

    unsafe { items.get_unchecked_mut(main_ns).mut_namespace_unchecked() }.parent_module = main_mod;

    modules.insert("core".into(), core_mod);
    modules.insert("main".into(), main_mod);

    namespaces.push(core_ns);
    namespaces.push(main_ns);

    Self {
      items,

      core_bs,
      core_ns,
      core_mod,

      main_ns,
      main_mod,

      err_ty,
      int_ty,
      float_ty,

      void_ty,
      bool_ty,

      concrete_int_ty,
      concrete_float_ty,

      modules,
      anon_types,

      namespaces,
      types,
      globals,
      functions,
    }
  }

  /// Iterate a slice of identifiers and use it as a path to traverse and retrieve a key
  pub fn get_key_from_path<I: AsRef<str>> (&self, path: &[I]) -> Option<ContextKey> {
    let mut active_key = self.main_ns;

    for ident in path.iter() {
      let ns = self.items.get(active_key)?.ref_namespace()?;

      active_key = ns.export_bindings.get_entry(ident)?;
    }

    Some(active_key)
  }


  /// Get the ContextKey associated with an item's parent Namespace, if it has one
  pub fn get_item_parent (&self, key: ContextKey) -> Option<ContextKey> {
    match self.items.get(key)? {
      ContextItem::Module(_) => None,
  
      | &ContextItem::Namespace(Namespace { parent_namespace, .. })
      | &ContextItem::Type(Type { parent_namespace, .. })
      => parent_namespace,
  
      | &ContextItem::Global(Global { parent_namespace, .. })
      | &ContextItem::Function(Function { parent_namespace, .. })
      => Some(parent_namespace)
    }
  }
  
  /// Get the ContextKey associated with an item's parent Module, if it has one
  pub fn get_item_module (&self, key: ContextKey) -> Option<ContextKey> {
    match self.items.get(key)? {
      ContextItem::Module(_) => None,
  
      &ContextItem::Type(Type { parent_module, .. })
      => parent_module,
      
      | &ContextItem::Namespace(Namespace { parent_module, .. })
      | &ContextItem::Global(Global { parent_module, .. })
      | &ContextItem::Function(Function { parent_module, .. })
      => Some(parent_module)
    }
  }
  
  /// Get a reference to the Identifier containing the canonical name of an item, if it has one
  pub fn get_item_canonical_name (&self, key: ContextKey) -> Option<&Identifier> {
    match self.items.get(key)? {
      ContextItem::Module(Module { canonical_name, .. }) => Some(canonical_name),
  
      | ContextItem::Type(Type { canonical_name, .. })
      => canonical_name.as_ref(),
      
      | ContextItem::Namespace(Namespace { canonical_name, .. })
      | ContextItem::Global(Global { canonical_name, .. })
      | ContextItem::Function(Function { canonical_name, .. })
      => Some(canonical_name)
    }
  }
}


/// A layer of semantic distinction for identifiers in a compilation context
#[derive(Debug, Clone)]
pub struct Bindspace<K: std::hash::Hash + Eq + Copy> {
  entries: Map<Identifier, K>,
  bind_locations: Map<K, SourceRegion>,
}

impl<K: std::hash::Hash + Eq + Copy> Bindspace<K> {
  /// Create a new empty Bindspace
  pub fn new () -> Self {
    Self {
      entries: Map::new(),
      bind_locations: Map::new(),
    }
  }
}

impl<K: std::hash::Hash + Eq + Copy> Default for Bindspace<K> { #[inline] fn default () -> Self { Self::new() } }


/// A variant of Bindspace using ContextKey as its K type
pub type GlobalBindspace = Bindspace<ContextKey>;

/// A variant of Bindspace using MultiKey as its K type
pub type LocalBindspace = Bindspace<MultiKey>;

impl<K: std::hash::Hash + Eq + Copy> Bindspace<K> {
  /// Get the location, if any, a Bindspace entry was bound at
  pub fn get_bind_location (&self, key: K) -> Option<SourceRegion> {
    self.bind_locations.find_value(&key).unref()
  }

  /// Determine if a Bindspace entry has a binding location
  pub fn has_bind_location (&self, key: K) -> bool {
    self.bind_locations.contains_key(&key)
  }

  /// Register a binding location for a Bindspace entry
  pub fn set_bind_location (&mut self, key: K, location: SourceRegion) {
    self.bind_locations.insert(key, location);
  }
  
  /// Get the entry, if any, associated with an identifier in a Bindspace
  pub fn get_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> Option<K> {
    self.entries.find_value(ident.as_ref()).unref()
  }

  /// Get the entry associated with an identifier in a Bindspace
  /// # Safety
  /// It is up to the caller to determine whether the identifier provided is bound
  pub unsafe fn get_entry_unchecked<I: AsRef<str> + ?Sized> (&self, ident: &I) -> K {
    *self.entries.find_value(ident.as_ref()).unwrap_unchecked()
  }

  /// Determine if a Bindspace entry has a binding location
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

  /// Determine if a Bindspace contains a given key
  pub fn has_entry_key (&self, key: K) -> bool {
    self.get_entry_ident(key).is_some()
  }

  /// Register a new Bindspace entry
  pub fn set_entry<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: K) {
    self.entries.insert(ident.into(), key);
  }

  /// Register a new Bindspace entry, and immediately bind its source location
  pub fn set_entry_bound<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: K, location: SourceRegion) {
    self.set_entry(ident, key);
    self.set_bind_location(key, location);
  }

  /// Get a key/loc iterator over the bind pairs in a Bindspace
  pub fn bind_iter (&self) -> PairIter<K, SourceRegion> {
    self.bind_locations.iter()
  }

  /// Get a mutable key/loc iterator over the bind pairs in a Bindspace
  pub fn bind_iter_mut (&mut self) -> PairIterMut<K, SourceRegion> {
    self.bind_locations.iter_mut()
  }

  /// Get an iterator over the identifiers in a Bindspace
  pub fn ident_iter (&self) -> SliceIter<Identifier> {
    self.entries.key_iter()
  }

  /// Get an iterator over the keys in a Bindspace
  pub fn key_iter (&self) -> SliceIter<K> {
    self.entries.value_iter()
  }

  /// Get a key/value iterator over the entry pairs in a Bindspace
  pub fn entry_iter (&self) -> PairIter<Identifier, K> {
    self.entries.iter()
  }

  /// Get a mutable key/value iterator over the entry pairs in a Bindspace
  pub fn entry_iter_mut (&mut self) -> PairIterMut<Identifier, K> {
    self.entries.iter_mut()
  }

  /// Get an immutable key/value/sourceregion iterator over the entry pairs and bind locations in a Bindspace
  pub fn iter (&self) -> BindspaceIter<K> {
    BindspaceIter {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter()
    }
  }

  /// Get a mutable key/value/sourceregion iterator over the entry pairs and bind locations in a Bindspace
  pub fn iter_mut (&mut self) -> BindspaceIterMut<K> {
    BindspaceIterMut {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter_mut()
    }
  }
}

/// An immutable key/value/sourceregion iterator over the entry pairs and bind locations in a Bindspace
pub struct BindspaceIter<'a, K: std::hash::Hash + Eq + Copy> {
  bind_locations: &'a Map<K, SourceRegion>,
  entry_iter: PairIter<'a, Identifier, K>,
}

impl<'a, K: std::hash::Hash + Eq + Copy> Iterator for BindspaceIter<'a, K> {
  type Item = (&'a Identifier, &'a K, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;

    Some((ident, key, self.bind_locations.find_value(key)?))
  }
}

/// A mutable key/value/sourceregion iterator over the entry pairs and bind locations in a Bindspace
pub struct BindspaceIterMut<'a, K: std::hash::Hash + Eq + Copy> {
  bind_locations: &'a Map<K, SourceRegion>,
  entry_iter: PairIterMut<'a, Identifier, K>,
}

impl<'a, K: std::hash::Hash + Eq + Copy> Iterator for BindspaceIterMut<'a, K> {
  type Item = (&'a Identifier, &'a mut K, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;
    let owned_key = *key;

    Some((ident, key, self.bind_locations.find_value(&owned_key)?))
  }
}


/// The top level compilation unit of source for a semantic analyzer,
/// synonymous with packages, libraries, compilation unit etc
#[derive(Debug, Clone)]
pub struct Module {
  /// The canonical, original name of a Module
  pub canonical_name: Identifier,
  /// Indicates whether a Module is the main module being compiled,
  /// or a dependency compiled separately
  pub is_main: bool,
  /// The top level Namespace containing all of a Module's Items
  pub namespace: ContextKey,
  /// The location a Module was first imported (if it is an import)
  pub origin: Option<SourceRegion>,
}

impl Module {
  /// Create a new semantic analysis Module
  pub fn new (canonical_name: Identifier, is_main: bool, namespace: ContextKey, origin: Option<SourceRegion>) -> Self {
    Self {
      canonical_name,
      is_main,
      namespace,
      origin,
    }
  }
}


/// The basic unit of source for a Module in a semantic analyzer,
/// serves as a container for types, values, and other namespaces
#[derive(Debug, Clone)]
pub struct Namespace {
  /// The Module a namespace was created in
  pub parent_module: ContextKey,
  /// Immediate hierarchical ancestor of a Namespace, if any
  pub parent_namespace: Option<ContextKey>,
  /// The canonical, original name of a Namespace
  pub canonical_name: Identifier,
  /// Bindspace entries available locally, inside a Namespace
  pub local_bindings: GlobalBindspace,
  /// Bindspace entries available publicly, outside a Namespace
  pub export_bindings: GlobalBindspace,
  /// The SourceRegion at which a Namespace was defined
  pub origin: SourceRegion,
}

impl Namespace {
  /// Create a new semantic analysis Namespace, and initialize its parent key
  pub fn new (parent_module: ContextKey, parent_namespace: Option<ContextKey>, canonical_name: Identifier, origin: SourceRegion) -> Self {
    Self {
      parent_module,
      parent_namespace,
      canonical_name,
      local_bindings: Bindspace::default(),
      export_bindings: Bindspace::default(),
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
  Pointer(ContextKey),
  /// A function pointer
  Function {
    /// The type(s) of any parameters accepted by a function
    parameter_types: Vec<ContextKey>,
    /// The type of value returned by a function, if any
    return_type: Option<ContextKey>
  },
  /// A structural, aggregate type
  Structure {
    /// Names for each field of a structural type
    field_names: Vec<Identifier>,
    /// Types for each field of a structural type
    field_types: Vec<ContextKey>,
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
      | TypeData::Structure { .. }
      => false,
    }
  }
}


/// Any Type known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Type {
  /// The Module a Type was defined in, if it is not an anonymous Type
  pub parent_module: Option<ContextKey>,
  /// The Namespace a Type was defined in, if it is not an anonymous type
  pub parent_namespace: Option<ContextKey>,
  /// The canonical, original name of a Type
  pub canonical_name: Option<Identifier>,
  /// The unique data associated with a Type, if it has been defined
  pub data: Option<TypeData>,
  /// The SourceRegion at which a Type was defined
  pub origin: SourceRegion,
}

impl Type {
  /// Create a new Type and initialize its canonical name, origin, and optionally its data
  pub fn new (parent_module: Option<ContextKey>, parent_namespace: Option<ContextKey>, canonical_name: Option<Identifier>, origin: SourceRegion, data: Option<TypeData>) -> Self {
    Self {
      parent_module,
      parent_namespace,
      canonical_name,
      data,
      origin,
    }
  }

  /// Determine if a Type's TypeData is anonymous
  /// 
  /// Returns None if the type is undefined
  pub fn is_anon_opt (&self) -> Option<bool> {
    self.data.as_ref().map(|td| td.is_anon())
  }

  /// Determine if a Type's TypeData is anonymous
  /// 
  /// Returns true if the type is undefined
  pub fn is_anon (&self) -> bool {
    self.data.as_ref().map(|td| td.is_anon()).unwrap_or(true)
  }
}


/// A wrapper structure for printing types for the user
pub struct TypeDisplay<'a> {
  /// The type being displayed
  pub ty_key: ContextKey,
  /// The context the type is found in
  pub context: &'a Context,
}

impl<'a> TypeDisplay<'a> {
  /// Create a new TypeDisplay with the same Context as an existing one, but a new ContextKey
  pub fn descend (&self, ty_key: ContextKey) -> Self {
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
        .expect("Internal error, cannot use TypeDisplay for other variants of ContextItem");
    
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
        },
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
        },
        TypeData::Structure { field_names, field_types } => {
          writeln!(f, "struct {{")?;

          let mut iter = field_names.iter().zip(field_types.iter()).peekable();

          while let Some((field_name, &field_type)) = iter.next() {
            write!(f, " {}: {}", field_name, self.descend(field_type))?;

            if iter.peek().is_some() { write!(f, ", " )?; }
          }

          write!(f, " }}")?;
        },
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
  /// The Module a Global was defined in
  pub parent_module: ContextKey,
  /// The Namespace a Global was defined in
  pub parent_namespace: ContextKey,
  /// The canonical, original name of a Global
  pub canonical_name: Identifier,
  /// The Type associated with a Global, if it has been defined
  pub ty: Option<ContextKey>,
  /// The SourceRegion at which a Global was defined
  pub origin: SourceRegion,
  /// The initializer IR expression for a Global if it has one
  pub initializer: Option<ir::Expression>,
  /// The initialization order of a Global
  pub rank: usize,
}

impl Global {
  /// Create a new Global and initialize its canonical name, origin, and optionally its type
  pub fn new (parent_module: ContextKey, parent_namespace: ContextKey, rank: usize, canonical_name: Identifier, origin: SourceRegion, ty: Option<ContextKey>) -> Self {
    Self {
      parent_module,
      parent_namespace,
      canonical_name,
      ty,
      origin,
      initializer: None,
      rank,
    }
  }
}

/// Any Function known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Function {
  /// The Module a Function was defined in
  pub parent_module: ContextKey,
  /// The Namespace a Function was defined in
  pub parent_namespace: ContextKey,
  /// The canonical, original name of a Function
  pub canonical_name: Identifier,
  /// The parameter identifiers and type keys associated with a Function, if it has been defined, and has any
  pub params: Vec<(Identifier, ContextKey, SourceRegion)>,
  /// The return type key associated with a Function, if it has been defined, and has any
  pub return_ty: Option<ContextKey>,
  /// The Type associated with a Function, if it has been defined
  pub ty: Option<ContextKey>,
  /// The SourceRegion at which a Function was defined
  pub origin: SourceRegion,
  /// The IR associated with a Function's body, if it has one
  pub body: Option<ir::Block>
}

impl Function {
  /// Create a new Function and initialize its canonical name, origin, and optionally its type
  pub fn new (parent_module: ContextKey, parent_namespace: ContextKey, canonical_name: Identifier, origin: SourceRegion, ty: Option<ContextKey>) -> Self {
    Self {
      parent_module,
      parent_namespace,
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
#[allow(clippy::large_enum_variant)] // cant do anything about this under the current architecture
#[derive(Debug, Clone)]
pub enum ContextItem {
  Module(Module),
  Namespace(Namespace),
  Type(Type),
  Global(Global),
  Function(Function),
}


/// A local item known by a semantic analyzer
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct LocalItem {
  pub canonical_name: Identifier,
  pub ty: ContextKey,
  pub is_parameter: bool,
  pub index: usize,
}

/// A reference to either a ContextItem or a LocalItem
#[allow(missing_docs)]
pub enum MultiRef<'a> {
  LocalItem(&'a LocalItem),
  ContextItem(&'a ContextItem),
}

impl<'a> From<&'a LocalItem> for MultiRef<'a> { #[inline] fn from (item: &'a LocalItem) -> Self { Self::LocalItem(item) } }
impl<'a> From<&'a ContextItem> for MultiRef<'a> { #[inline] fn from (item: &'a ContextItem) -> Self { Self::ContextItem(item) } }

/// A mutable reference to either a ContextItem or a LocalItem
#[allow(missing_docs)]
pub enum MultiMut<'a> {
  LocalItem(&'a mut LocalItem),
  ContextItem(&'a mut ContextItem),
}

impl<'a> From<&'a mut LocalItem> for MultiMut<'a> { #[inline] fn from (item: &'a mut LocalItem) -> Self { Self::LocalItem(item) } }
impl<'a> From<&'a mut ContextItem> for MultiMut<'a> { #[inline] fn from (item: &'a mut ContextItem) -> Self { Self::ContextItem(item) } }

make_key_type! {
  /// A SlotMap Key representing a LocalItem
  pub struct LocalKey;
}

/// A Key to either a local variable or a global item
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MultiKey {
  LocalKey(LocalKey),
  ContextKey(ContextKey),
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
  pub fn bindspace (self) -> Option<ContextKey> { if let Self::ContextKey(key) = self { Some(key) } else { None } }

  /// Convert a MultiKey to its variant
  /// 
  /// # Safety
  /// This function only checks the validity of this conversion if debug_assertions are enabled,
  /// it is up to the caller to determine the safety of this operation
  pub unsafe fn bindspace_unchecked (self) -> ContextKey { if cfg!(debug_assertions) { self.bindspace().unwrap() } else if let Self::ContextKey(key) = self { key } else { unreachable_unchecked() } }
}

impl PartialEq<LocalKey> for MultiKey { #[inline] fn eq (&self, other: &LocalKey) -> bool { if let Self::LocalKey(key) = self { key == other } else { false } } }
impl PartialEq<ContextKey> for MultiKey { #[inline] fn eq (&self, other: &ContextKey) -> bool { if let Self::ContextKey(key) = self { key == other } else { false } } }

impl From<LocalKey> for MultiKey { #[inline] fn from (key: LocalKey) -> Self { Self::LocalKey(key) } }
impl From<ContextKey> for MultiKey { #[inline] fn from (key: ContextKey) -> Self { Self::ContextKey(key) } }


/// A function or global initializer expression's contextual information
#[derive(Debug, Clone)]
pub struct LocalContext {
  /// All local variables known by a LocalContext,
  /// not all of which may be accessible from the Bindspace stack
  pub variables: SlotMap<LocalKey, LocalItem>,
  /// The number of function parameter variables registered in a LocalContext
  pub parameter_count: usize,
  /// The number of local variables registered in a LocalContext
  pub local_count: usize,
  /// The stack of Bindspaces in scope for a LocalContext,
  /// identifier lookup reverse iterates this stack to find variables,
  /// ending at the global Bindspace
  pub stack_frames: Vec<LocalBindspace>,
}

impl Default for LocalContext { #[inline] fn default () -> Self { Self::new() } }

impl LocalContext {
  /// Create a new LocalContext with a single empty stack frame
  pub fn new () -> Self {
    Self {
      variables: SlotMap::default(),
      parameter_count: 0,
      local_count: 0,
      stack_frames: vec![ Bindspace::default() ]
    }
  }

  /// Create a local variable in a LocalContext
  pub fn create_variable (&mut self,
    canonical_name: Identifier,
    ty: ContextKey,
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
  
  /// Try to lookup a local variable by traversing down the stack from a LocalContext's top Bindspace
  pub fn get_variable<I: AsRef<str>> (&self, ident: &I) -> Option<MultiKey> {
    for bs in self.stack_frames.iter().rev() {
      let bs_entry = bs.get_entry(ident);

      if bs_entry.is_some() { return bs_entry }
    }

    None
  }

  /// Create a new stack frame Bindspace in a LocalContext
  pub fn push_stack_frame (&mut self) {
    self.stack_frames.push(Bindspace::default())
  }

  /// Pop a Bindspace off the frame stack in a LocalContext
  /// 
  /// Panics if there is only a single stack frame left
  pub fn pop_stack_frame (&mut self) -> LocalBindspace {
    assert!(self.stack_frames.len() > 1, "Internal error, cannot pop final stack frame of LocalContext");

    self.stack_frames.pop().unwrap()
  }
}

impl ContextItem {
  /// Get the ContextItemKind of a ContextItem
  pub fn kind (&self) -> ContextItemKind {
    match self {
      Self::Module(_)    => ContextItemKind::Module,
      Self::Namespace(_) => ContextItemKind::Namespace,
      Self::Type(_)      => ContextItemKind::Type,
      Self::Global(_)    => ContextItemKind::Global,
      Self::Function(_)  => ContextItemKind::Function,
    }
  }

  /// Convert a reference to a ContextItem into an optional reference to a Module
  #[inline] pub fn ref_module (&self) -> Option<&Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a reference to a ContextItem into an optional reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_module_unchecked (&self) -> &Module { if cfg!(debug_assertions) { self.ref_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Module
  #[inline] pub fn mut_module (&mut self) -> Option<&mut Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_module_unchecked (&mut self) -> &mut Module { if cfg!(debug_assertions) { self.mut_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }


  /// Convert a reference to a ContextItem into an optional reference to a Namespace
  #[inline] pub fn ref_namespace (&self) -> Option<&Namespace> { match self { Self::Namespace(item) => Some(item), _ => None } }

  /// Convert a reference to a ContextItem into an optional reference to a Namespace
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_namespace_unchecked (&self) -> &Namespace { if cfg!(debug_assertions) { self.ref_namespace().unwrap() } else if let Self::Namespace(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Namespace
  #[inline] pub fn mut_namespace (&mut self) -> Option<&mut Namespace> { match self { Self::Namespace(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Namespace
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_namespace_unchecked (&mut self) -> &mut Namespace { if cfg!(debug_assertions) { self.mut_namespace().unwrap() } else if let Self::Namespace(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a ContextItem into an optional reference to a Type
  #[inline] pub fn ref_type (&self) -> Option<&Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a reference to a ContextItem into an optional reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_type_unchecked (&self) -> &Type { if cfg!(debug_assertions) { self.ref_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Type
  #[inline] pub fn mut_type (&mut self) -> Option<&mut Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_type_unchecked (&mut self) -> &mut Type { if cfg!(debug_assertions) { self.mut_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a ContextItem into an optional reference to a Global
  #[inline] pub fn ref_global (&self) -> Option<&Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a reference to a ContextItem into an optional reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_global_unchecked (&self) -> &Global { if cfg!(debug_assertions) { self.ref_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Global
  #[inline] pub fn mut_global (&mut self) -> Option<&mut Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_global_unchecked (&mut self) -> &mut Global { if cfg!(debug_assertions) { self.mut_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a ContextItem into an optional reference to a Function
  #[inline] pub fn ref_function (&self) -> Option<&Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a reference to a ContextItem into an optional reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_function_unchecked (&self) -> &Function { if cfg!(debug_assertions) { self.ref_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Function
  #[inline] pub fn mut_function (&mut self) -> Option<&mut Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a ContextItem into an optional mutable reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_function_unchecked (&mut self) -> &mut Function { if cfg!(debug_assertions) { self.mut_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }
}

impl From<Module> for ContextItem { #[inline] fn from (item: Module) -> ContextItem { Self::Module(item) } }
impl From<Namespace> for ContextItem { #[inline] fn from (item: Namespace) -> ContextItem { Self::Namespace(item) } }
impl From<Type> for ContextItem { #[inline] fn from (item: Type) -> ContextItem { Self::Type(item) } }
impl From<Global> for ContextItem { #[inline] fn from (item: Global) -> ContextItem { Self::Global(item) } }
impl From<Function> for ContextItem { #[inline] fn from (item: Function) -> ContextItem { Self::Function(item) } }

/// The kind of a top-level item known by a semantic analyzer
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ContextItemKind {
  Module,
  Namespace,
  Type,
  Global,
  Function,
}

impl Display for ContextItemKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Module => "Module",
      Self::Namespace => "Namespace",
      Self::Type => "Type",
      Self::Global => "Global",
      Self::Function => "Function",
    })
  }
}

make_key_type! {
  /// A key referring to a top-level item in a semantic analyzer
  pub struct ContextKey;
}