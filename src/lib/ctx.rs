//! Contextual types used by the semantic analysis system

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  collections::{ HashMap, hash_map::{ Iter as HashMapIter, IterMut as HashMapIterMut, }, },
};

use crate::{
  util::{ make_key_type, Unref, },
  source::{ SourceRegion, },
  collections::{ SlotMap, },
};



/// A local context for semantic analysis inside a function
pub struct LocalContext {
  /// All function parameter variables available in a local context
  pub parameters: SlotMap<ParameterKey, Parameter>,
  /// All stack variables created in a local context,
  /// not all of them may be accessable by the namespace
  pub locals:     SlotMap<LocalKey, Local>,
}

/// The library-wide context for a semantic analyzer,
/// where all data is stored for reference by descendant structures
pub struct Context {
  /// All modules in a library being analyzed
  pub modules:   SlotMap<ModuleKey, Option<Module>>,
  /// All types in a library being analyzed
  pub types:     SlotMap<TypeKey, Option<Type>>,
  /// All globals in a library being analyzed
  pub globals:   SlotMap<GlobalKey, Option<Global>>,
  /// All functions in a library being analyzed
  pub functions: SlotMap<FunctionKey, Option<Function>>,

  /// A list of source locations referencing items in the context
  pub reference_locations: HashMap<NamespaceKey, Vec<SourceRegion>>,

  /// The universal namespace available to all code in a library,
  /// containing things like the built in primitive types
  pub core: Namespace,

  /// The ModuleKey associated with the root library module
  pub lib: ModuleKey,

  /// The TypeKey given as a result of a type error
  pub err_ty: TypeKey,
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
    ];

    let mut types = SlotMap::default();
    let mut core_mod = Module::default();
    let mut core_ns  = Namespace::default();

    for &(name, ref ty) in PRIMITIVE_TYPES.iter() {
      let key = types.insert(Some(ty.clone().into()));
      let ns_key = NamespaceKey::Type(key);

      core_ns.add_entry(name, ns_key);
      core_mod.types.insert(name.to_string(), key);
      core_mod.exports.insert(name.to_string(), ns_key);
    }

    let mut modules = SlotMap::default();
    let core_key = modules.insert(Some(core_mod));
    core_ns.add_entry("core", NamespaceKey::Module(core_key));

    let lib_mod = Module::default();
    let lib_key = modules.insert(Some(lib_mod));
    core_ns.add_entry("lib", NamespaceKey::Module(lib_key));

    let err_ty = types.insert(Some(TypeData::Alias(TypeKey::default()).into()));

    Self {
      modules,
      types,
      globals: SlotMap::default(),
      functions: SlotMap::default(),

      reference_locations: HashMap::default(),

      core: core_ns,
      lib: lib_key,

      err_ty,
    }
  }
}




/// The kind of an identifier in a Namespace
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NamespaceKind {
  /// An identifier known to refer to a module
  Module,
  /// An identifier known to refer to a type
  Type,
  /// An identifier known to refer to a global
  Global,
  /// An identifier known to refer to a function
  Function,
  /// An identifier known to refer to a local function parameter variable
  Parameter,
  /// An identifier known to refer to a local stack variable
  Local,
}

impl Display for NamespaceKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Module => "Module",
      Self::Type => "Type",
      Self::Global => "Global",
      Self::Function => "Function",
      Self::Parameter => "Parameter",
      Self::Local => "Local",
    })
  }
}


/// Any identifier in a Namespace
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NamespaceKey {
  /// A module identifier
  Module(ModuleKey),
  /// A type identifier
  Type(TypeKey),
  /// A global identifier
  Global(GlobalKey),
  /// A function identifier
  Function(FunctionKey),

  /// A function parameter local variable
  Parameter(ParameterKey),
  /// A stack local variable
  Local(LocalKey),
}

impl NamespaceKey {
  /// Get the NamespaceKind of a NamespaceKey
  pub fn kind (&self) -> NamespaceKind {
    match self {
      Self::Module(_) => NamespaceKind::Module,
      Self::Type(_) => NamespaceKind::Type,
      Self::Global(_) => NamespaceKind::Global,
      Self::Function(_) => NamespaceKind::Function,
      Self::Parameter(_) => NamespaceKind::Parameter,
      Self::Local(_) => NamespaceKind::Local,
    }
  }
}

impl From<ModuleKey> for NamespaceKey { #[inline] fn from (key: ModuleKey) -> Self { Self::Module(key) } }
impl From<TypeKey> for NamespaceKey { #[inline] fn from (key: TypeKey) -> Self { Self::Type(key) } }
impl From<GlobalKey> for NamespaceKey { #[inline] fn from (key: GlobalKey) -> Self { Self::Global(key) } }
impl From<FunctionKey> for NamespaceKey { #[inline] fn from (key: FunctionKey) -> Self { Self::Function(key) } }
impl From<ParameterKey> for NamespaceKey { #[inline] fn from (key: ParameterKey) -> Self { Self::Parameter(key) } }
impl From<LocalKey> for NamespaceKey { #[inline] fn from (key: LocalKey) -> Self { Self::Local(key) } }


/// A layer of semantic distinction for identifiers in a compilation context
#[derive(Debug, Clone, Default)]
pub struct Namespace {
  entries: HashMap<String, NamespaceKey>,
  bind_locations: HashMap<NamespaceKey, SourceRegion>,
}

impl Namespace {
  /// Get the location, if any, a Namespace entry was bound at
  pub fn get_bind_location (&self, key: NamespaceKey) -> Option<SourceRegion> {
    self.bind_locations.get(&key).unref()
  }

  /// Determine if a Namespace entry has a binding location
  pub fn has_bind_location (&self, key: NamespaceKey) -> bool {
    self.bind_locations.contains_key(&key)
  }

  /// Register a binding location for a Namespace entry
  /// 
  /// Panics if the Namespace entry is already bound
  pub fn set_bind_location (&mut self, key: NamespaceKey, location: SourceRegion) {
    assert!(!self.has_bind_location(key), "Internal error: Namespace entry bound to multiple source locations");

    self.bind_locations.insert(key, location);
  }
  
  /// Get the entry, if any, associated with an identifier in a Namespace
  pub fn get_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> Option<NamespaceKey> {
    self.entries.get(ident.as_ref()).unref()
  }

  /// Determine if a namespace entry has a binding location
  pub fn has_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> bool {
    self.entries.contains_key(ident.as_ref())
  }

  
  /// Get the identifier associated with an entry, if any
  pub fn get_entry_ident (&self, key: NamespaceKey) -> Option<&str> {
    for (i, k) in self.entries.iter() {
      if k == &key { return Some(i.as_str()) }
    }

    None
  }

  /// Determine if a Namespace contains a given key
  pub fn has_entry_key (&self, key: NamespaceKey) -> bool {
    self.get_entry_ident(key).is_some()
  }

  /// Register a new namespace entry
  ///
  /// Panics if the namespace entry is already bound
  pub fn add_entry<I: Into<String> + AsRef<str>> (&mut self, ident: I, key: NamespaceKey) {
    assert!(!self.has_entry(&ident), "Internal error: Namespace entry already exists");

    self.entries.insert(ident.into(), key);
  }

  /// Get a key/value iterator over the entry pairs in a namespace
  pub fn entry_iter (&self) -> HashMapIter<String, NamespaceKey> {
    self.entries.iter()
  }

  /// Get a mutable key/value iterator over the entry pairs in a namespace
  pub fn entry_iter_mut (&mut self) -> HashMapIterMut<String, NamespaceKey> {
    self.entries.iter_mut()
  }
}


/// The basic unit of source for a semantic analyzer,
/// serves as a container for types, values, and other modules
pub struct Module {
  /// The hierarchical predecessor of a module if one exists.
  /// All modules other than the root library module and core module have a parent
  /// Later, external dependency modules may be possible, which would also have no parent
  pub parent: Option<ModuleKey>,
  
  /// Hierarchical descedants of a module
  pub modules:   HashMap<String, ModuleKey>,
  /// Types associated with a module
  pub types:     HashMap<String, TypeKey>,
  /// Globals associated with a module
  pub globals:   HashMap<String, GlobalKey>,
  /// Functions associated with a module
  pub functions: HashMap<String, FunctionKey>,

  /// A list of all items publicly accessable from outside a module
  pub exports: HashMap<String, NamespaceKey>,

  /// The source location at which a Module was defined
  pub origin: Option<SourceRegion>,
}

impl Default for Module {
  #[inline] fn default () -> Self { Self::new(None, None) }
}

impl Module {
  /// Create a new semantic analysis Module, and initialize its parent key
  pub fn new (parent: Option<ModuleKey>, origin: Option<SourceRegion>) -> Self {
    Self {
      parent,

      modules: HashMap::default(),
      types: HashMap::default(),
      globals: HashMap::default(),
      functions: HashMap::default(),
      
      exports: HashMap::default(),

      origin,
    }
  }
}

/// Data representation of the simplest kind of type,
/// built in primitives such as numbers
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
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
  },
}

/// Unique data for a Type in a semantic analyzer
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum TypeData {
  /// A reference to another type
  Alias(TypeKey),
  /// Built in primitive type such as an integer
  Primitive(PrimitiveType),
  /// A pointer to another type
  Pointer(TypeKey),
  /// A function pointer
  Function {
    /// The type(s) of any parameters accepted by a function
    parameter_types: Vec<TypeKey>,
    /// The type of value returned by a function, if any
    return_type: Option<TypeKey>
  },
}

/// Any Type known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Type {
  /// The unique data associated with a type
  pub data: TypeData,
  /// The source location at which a global was defined
  pub origin: Option<SourceRegion>,
}

impl PartialEq for Type {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl std::hash::Hash for Type {
  #[inline] fn hash<H: std::hash::Hasher> (&self, hasher: &mut H) { self.data.hash(hasher) }
}

impl From<TypeData> for Type {
  fn from (data: TypeData) -> Self { Self { data, origin: None } }
}

/// A global variable in a semantic analysis context
pub struct Global {
  /// The type of value represented by a global
  pub ty: TypeKey,
  /// The source location at which a global was defined
  pub origin: Option<SourceRegion>,
}

impl From<TypeKey> for Global {
  fn from (ty: TypeKey) -> Self { Self { ty, origin: None } }
}

/// A function in a semantic analysis context
pub struct Function {
  /// The type of the interface presented by a function
  pub ty: TypeKey,
  /// The source location at which a function was defined
  pub origin: Option<SourceRegion>,
}

impl From<TypeKey> for Function {
  fn from (ty: TypeKey) -> Self { Self { ty, origin: None } }
}

/// A function parameter variable in a semantic analysis local context
pub struct Parameter {
  /// The type of value represented by a parameter
  pub ty: TypeKey,
  /// The source location at which a parameter was defined
  pub origin: Option<SourceRegion>,
}

impl From<TypeKey> for Parameter {
  fn from (ty: TypeKey) -> Self { Self { ty, origin: None } }
}

/// A stack variable in a semantic analysis local context
pub struct Local {
  /// The type of value represented by a local
  pub ty: TypeKey,
  /// The source location at which a local was defined
  pub origin: Option<SourceRegion>,
}

impl From<TypeKey> for Local {
  fn from (ty: TypeKey) -> Self { Self { ty, origin: None } }
}

make_key_type! {
  /// An addressable key representing a module in a semantic analysis context
  pub struct ModuleKey;
  /// An addressable key representing a type in a semantic analysis context
  pub struct TypeKey;
  /// An addressable key representing a global in a semantic analysis context
  pub struct GlobalKey;
  /// An addressable key representing a function in a semantic analysis context
  pub struct FunctionKey;
  /// An addressable key representing a function's local parameter in a semantic analysis context
  pub struct ParameterKey;
  /// An addressable key representing a function's local stack variable in a semantic analysis context
  pub struct LocalKey;
}