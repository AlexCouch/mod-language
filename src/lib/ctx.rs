

#![allow(dead_code)]

//! Contextual types used by the semantic analysis system

use core::{
  hint::{ unreachable_unchecked, },
};

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  collections::{ HashMap, hash_map::{ Iter as HashMapIter, IterMut as HashMapIterMut, }, },
};

use crate::{
  util::{ make_key_type, Unref, UnwrapUnchecked, },
  collections::{ SlotMap, },
  session::{ SESSION, },
  source::{ SourceRegion, },
  common::{ Identifier, },
  ast::{ Path, },
};


/// The main layer of contextual information derived from a semantic analysis session
#[derive(Debug)]
pub struct Context {
  /// Storage for all top-level items discovered by a semantic analyzer
  pub items: SlotMap<NamespaceKey, NamespaceItem>,
  /// The namespace containing compiler intrinsics for a Context
  pub core_ns: Namespace,
  /// The module containing compiler intrinsics for a Context
  pub core_mod: NamespaceKey,
  /// The root module for the library represented by a Context
  pub lib_mod: NamespaceKey,
  /// A list of source locations referencing items in a Context
  pub reference_locations: HashMap<NamespaceKey, Vec<SourceRegion>>,
  /// The key given as a result of a type error
  pub err_ty: NamespaceKey,
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

    let mut items = SlotMap::default();
    let mut core_mod = Module::new(None, "core".into(), SourceRegion::ANONYMOUS);
    let mut core_ns  = Namespace::default();

    for &(name, ref td) in PRIMITIVE_TYPES.iter() {
      let key = items.insert(Type::new(name.into(), SourceRegion::ANONYMOUS, Some(td.clone())).into());

      core_ns.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      core_mod.local_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
      core_mod.export_bindings.set_entry_bound(name, key, SourceRegion::ANONYMOUS);
    }

    let core_key = items.insert(core_mod.into());
    core_ns.set_entry_bound("core", core_key, SourceRegion::ANONYMOUS);

    let lib_mod = Module::new(None, "lib".into(), SourceRegion::ANONYMOUS);
    let lib_key = items.insert(lib_mod.into());
    core_ns.set_entry_bound("lib", lib_key, SourceRegion::ANONYMOUS);

    let err_ty = items.insert(Type::new("err_ty".into(), SourceRegion::ANONYMOUS, Some(TypeData::Alias(NamespaceKey::default()))).into());

    Self {
      items,

      reference_locations: HashMap::default(),

      core_ns,
      core_mod: core_key,
      lib_mod: lib_key,

      err_ty,
    }
  }


  /// Get the bottom level of NamespaceKey using alias traversal
  pub fn lower_key (&mut self, key: NamespaceKey) -> Option<NamespaceKey> {
    let item = self.items.get(key)?;

    macro_rules! replace_cache {
      ($new_res:expr) => {
        unsafe { self.items.get_unchecked_mut(key).mut_alias_unchecked() }.cached_key.replace($new_res);
      };
    }

    macro_rules! error {
      ($origin:expr, $($format_args:expr),+) => {
        {
          SESSION.error(Some($origin), format!($($format_args),+));

          replace_cache!(Err(()));

          return None
        }
      };
    }

    Some(if let NamespaceItem::Alias(alias) = item {
      if let Some(res) = alias.cached_key {
        if let Ok(res) = res {
          return Some(res)
        } else {
          return None
        }
      }

      let alias = alias.clone();

      let lowered_key = match &alias.data {
        AliasData::Import(path) => {
          let base_key = if path.absolute {
            self.lib_mod
          } else {
            alias.relative_to.expect("Internal error, no base key for relative import alias")
          };

          let mut base_name = Identifier::default();

          base_name.set(
            &self.items
              .get(base_key)
              .expect("Internal error, base key for import alias is not valid")
              .ref_module()
              .expect("Internal error, base key for import alias does not resolve to a module")
              .canonical_name
          );

          let mut lowered_key = base_key;

          for ident in path.iter() {
            let base = self.resolve_key(lowered_key)?;

            if let NamespaceItem::Module(module) = base {
              let entry_source = if lowered_key == base_key && alias.relative_to.is_some() {
                &module.local_bindings
              } else {
                &module.export_bindings
              };

              if let Some(exported_key) = entry_source.get_entry(ident) {
                base_name.set(ident);

                lowered_key = self.lower_key(exported_key)?;
              } else {
                error!(alias.origin, "Module `{}` does not export an item named `{}`", module.canonical_name, ident);
              }
            } else {
              error!(alias.origin, "`{}` is not a Module and has no exports", base_name);
            }
          }

          lowered_key
        },

        AliasData::Export(ident) => {
          let base_key = alias.relative_to.expect("Internal error, no base key for export alias");

          let base: &Module = 
            self.items
              .get(base_key)
              .expect("Internal error, base key for export alias is not valid")
              .ref_module()
              .expect("Internal error, base key for export alias does not resolve to a module");
          
          if let Some(local_key) = base.local_bindings.get_entry(ident) {
            self.lower_key(local_key)?
          } else {
            error!(alias.origin, "Module `{}` does not have an item named `{}` to export", base.canonical_name, ident);
          }
        },
      };

      replace_cache!(Ok(lowered_key));

      lowered_key
    } else {
      key
    })
  }

  /// Resolve a NamespaceKey, traversing imports as deeply as possible
  pub fn resolve_key (&mut self, key: NamespaceKey) -> Option<&NamespaceItem> {
    let key = self.lower_key(key)?;
    self.items.get(key)
  }

  /// Resolve a NamespaceKey, traversing imports as deeply as possible
  pub fn resolve_key_mut (&mut self, key: NamespaceKey) -> Option<&mut NamespaceItem> {
    let key = self.lower_key(key)?;
    self.items.get_mut(key)
  }
}


/// A layer of semantic distinction for identifiers in a compilation context
#[derive(Debug, Clone, Default)]
pub struct Namespace {
  entries: HashMap<Identifier, NamespaceKey>,
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
  pub fn set_bind_location (&mut self, key: NamespaceKey, location: SourceRegion) {
    self.bind_locations.insert(key, location);
  }
  
  /// Get the entry, if any, associated with an identifier in a Namespace
  pub fn get_entry<I: AsRef<str> + ?Sized> (&self, ident: &I) -> Option<NamespaceKey> {
    self.entries.get(ident.as_ref()).unref()
  }

  /// Get the entry associated with an identifier in a Namespace
  /// # Safety
  /// It is up to the caller to determine whether the identifier provided is bound
  pub unsafe fn get_entry_unchecked<I: AsRef<str> + ?Sized> (&self, ident: &I) -> NamespaceKey {
    *self.entries.get(ident.as_ref()).unwrap_unchecked()
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
  pub fn set_entry<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: NamespaceKey) {
    self.entries.insert(ident.into(), key);
  }

  /// Register a new namespace entry, and immediately bind its source location
  pub fn set_entry_bound<I: Into<Identifier> + AsRef<str>> (&mut self, ident: I, key: NamespaceKey, location: SourceRegion) {
    self.set_entry(ident, key);
    self.set_bind_location(key, location);
  }

  /// Get a key/loc iterator over the bind pairs in a namespace
  pub fn bind_iter (&self) -> HashMapIter<NamespaceKey, SourceRegion> {
    self.bind_locations.iter()
  }

  /// Get a mutable key/loc iterator over the bind pairs in a namespace
  pub fn bind_iter_mut (&mut self) -> HashMapIterMut<NamespaceKey, SourceRegion> {
    self.bind_locations.iter_mut()
  }

  /// Get a key/value iterator over the entry pairs in a namespace
  pub fn entry_iter (&self) -> HashMapIter<Identifier, NamespaceKey> {
    self.entries.iter()
  }

  /// Get a mutable key/value iterator over the entry pairs in a namespace
  pub fn entry_iter_mut (&mut self) -> HashMapIterMut<Identifier, NamespaceKey> {
    self.entries.iter_mut()
  }

  /// Get an immutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
  pub fn iter (&self) -> NamespaceIter {
    NamespaceIter {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter()
    }
  }

  /// Get a mutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
  pub fn iter_mut (&mut self) -> NamespaceIterMut {
    NamespaceIterMut {
      bind_locations: &self.bind_locations,
      entry_iter: self.entries.iter_mut()
    }
  }
}

/// An immutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
pub struct NamespaceIter<'a> {
  bind_locations: &'a HashMap<NamespaceKey, SourceRegion>,
  entry_iter: HashMapIter<'a, Identifier, NamespaceKey>,
}

impl<'a> Iterator for NamespaceIter<'a> {
  type Item = (&'a Identifier, &'a NamespaceKey, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;

    Some((ident, key, self.bind_locations.get(key)?))
  }
}

/// A mutable key/value/sourceregion iterator over the entry pairs and bind locations in a namespace
pub struct NamespaceIterMut<'a> {
  bind_locations: &'a HashMap<NamespaceKey, SourceRegion>,
  entry_iter: HashMapIterMut<'a, Identifier, NamespaceKey>,
}

impl<'a> Iterator for NamespaceIterMut<'a> {
  type Item = (&'a Identifier, &'a mut NamespaceKey, &'a SourceRegion);

  fn next (&mut self) -> Option<Self::Item> {
    let (ident, key) = self.entry_iter.next()?;
    let owned_key = *key;

    Some((ident, key, self.bind_locations.get(&owned_key)?))
  }
}

/// The data associated with an alias, either import or export
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum AliasData {
  Import(Path),
  Export(Identifier),
}

/// A lazy evaluated, cached alias to a path
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct Alias {
  pub data: AliasData,
  pub relative_to: Option<NamespaceKey>,
  pub cached_key: Option<Result<NamespaceKey, ()>>,
  pub origin: SourceRegion,
}

impl Alias {
  /// Create a new unresolved Alias
  pub fn new (relative_to: Option<NamespaceKey>, data: AliasData, origin: SourceRegion) -> Self {
    Self {
      data,
      relative_to,
      cached_key: None,
      origin,
    }
  }
}


/// The basic unit of source for a semantic analyzer,
/// serves as a container for types, values, and other modules
#[derive(Debug, Clone)]
pub struct Module {
  /// Immediate hierarchical ancestor of a Module, if any
  pub parent: Option<NamespaceKey>,
  /// The canonical, original name of a Module
  pub canonical_name: Identifier,
  /// Namespaced bindings available locally, inside a Module
  pub local_bindings: Namespace,
  /// Namespaced bindings available publicly, outside a Module
  pub export_bindings: Namespace,
  /// The SourceRegion at which a Module was defined
  pub origin: SourceRegion,
}

impl Module {
  /// Create a new semantic analysis Module, and initialize its parent key
  pub fn new (parent: Option<NamespaceKey>, canonical_name: Identifier, origin: SourceRegion) -> Self {
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
  Alias(NamespaceKey),
  /// Built in primitive type such as an integer
  Primitive(PrimitiveType),
  /// A pointer to another type
  Pointer(NamespaceKey),
  /// A function pointer
  Function {
    /// The type(s) of any parameters accepted by a function
    parameter_types: Vec<NamespaceKey>,
    /// The type of value returned by a function, if any
    return_type: Option<NamespaceKey>
  },
}

/// Any Type known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Type {
  /// The canonical, original name of a Type
  pub canonical_name: Identifier,
  /// The unique data associated with a Type, if it has been defined
  pub data: Option<TypeData>,
  /// The SourceRegion at which a Type was defined
  pub origin: SourceRegion,
}

impl Type {
  /// Create a new Type and initialize its canonical name, origin, and optionally its data
  pub fn new (canonical_name: Identifier, origin: SourceRegion, data: Option<TypeData>) -> Self {
    Self {
      canonical_name,
      data,
      origin,
    }
  }
}


/// Any Global known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Global {
  /// The canonical, original name of a Global
  pub canonical_name: Identifier,
  /// The Type associated with a Global, if it has been defined
  pub ty: Option<NamespaceKey>,
  /// The SourceRegion at which a Global was defined
  pub origin: SourceRegion,
}

impl Global {
  /// Create a new Global and initialize its canonical name, origin, and optionally its type
  pub fn new (canonical_name: Identifier, origin: SourceRegion, ty: Option<NamespaceKey>) -> Self {
    Self {
      canonical_name,
      ty,
      origin,
    }
  }
}

/// Any Function known by a semantic analyzer
#[derive(Debug, Clone)]
pub struct Function {
  /// The canonical, original name of a Function
  pub canonical_name: Identifier,
  /// The Type associated with a Function, if it has been defined
  pub ty: Option<NamespaceKey>,
  /// The SourceRegion at which a Function was defined
  pub origin: SourceRegion,
}

impl Function {
  /// Create a new Function and initialize its canonical name, origin, and optionally its type
  pub fn new (canonical_name: Identifier, origin: SourceRegion, ty: Option<NamespaceKey>) -> Self {
    Self {
      canonical_name,
      ty,
      origin,
    }
  }
}


/// A top-level item known by a semantic analyzer
#[allow(missing_docs)]
#[allow(clippy::large_enum_variant)] // This is unavoidable in this instance
#[derive(Debug, Clone)]
pub enum NamespaceItem {
  Alias(Alias),
  Module(Module),
  Type(Type),
  Global(Global),
  Function(Function),
}

impl NamespaceItem {
  /// Get the NamespaceKind of a NamespaceItem
  pub fn kind (&self) -> NamespaceKind {
    match self {
      Self::Alias(_)    => NamespaceKind::Alias,
      Self::Module(_)   => NamespaceKind::Module,
      Self::Type(_)     => NamespaceKind::Type,
      Self::Global(_)   => NamespaceKind::Global,
      Self::Function(_) => NamespaceKind::Function,
    }
  }


  /// Convert a reference to a NamespaceItem into an optional reference to a Alias
  #[inline] pub fn ref_alias (&self) -> Option<&Alias> { match self { Self::Alias(item) => Some(item), _ => None } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Alias
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_alias_unchecked (&self) -> &Alias { if cfg!(debug_assertions) { self.ref_alias().unwrap() } else if let Self::Alias(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Alias
  #[inline] pub fn mut_alias (&mut self) -> Option<&mut Alias> { match self { Self::Alias(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Alias
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_alias_unchecked (&mut self) -> &mut Alias { if cfg!(debug_assertions) { self.mut_alias().unwrap() } else if let Self::Alias(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Module
  #[inline] pub fn ref_module (&self) -> Option<&Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_module_unchecked (&self) -> &Module { if cfg!(debug_assertions) { self.ref_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Module
  #[inline] pub fn mut_module (&mut self) -> Option<&mut Module> { match self { Self::Module(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Module
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_module_unchecked (&mut self) -> &mut Module { if cfg!(debug_assertions) { self.mut_module().unwrap() } else if let Self::Module(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a NamespaceItem into an optional reference to a Type
  #[inline] pub fn ref_type (&self) -> Option<&Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_type_unchecked (&self) -> &Type { if cfg!(debug_assertions) { self.ref_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Type
  #[inline] pub fn mut_type (&mut self) -> Option<&mut Type> { match self { Self::Type(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Type
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_type_unchecked (&mut self) -> &mut Type { if cfg!(debug_assertions) { self.mut_type().unwrap() } else if let Self::Type(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a NamespaceItem into an optional reference to a Global
  #[inline] pub fn ref_global (&self) -> Option<&Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_global_unchecked (&self) -> &Global { if cfg!(debug_assertions) { self.ref_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Global
  #[inline] pub fn mut_global (&mut self) -> Option<&mut Global> { match self { Self::Global(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Global
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_global_unchecked (&mut self) -> &mut Global { if cfg!(debug_assertions) { self.mut_global().unwrap() } else if let Self::Global(item) = self { item } else { unreachable_unchecked() } }
  
  /// Convert a reference to a NamespaceItem into an optional reference to a Function
  #[inline] pub fn ref_function (&self) -> Option<&Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a reference to a NamespaceItem into an optional reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn ref_function_unchecked (&self) -> &Function { if cfg!(debug_assertions) { self.ref_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Function
  #[inline] pub fn mut_function (&mut self) -> Option<&mut Function> { match self { Self::Function(item) => Some(item), _ => None } }

  /// Convert a mutable reference to a NamespaceItem into an optional mutable reference to a Function
  /// # Safety
  /// This only performs an assertion on the variant if debug_assertions is enabled,
  /// it is up to the caller to determine the safety of this transformation
  #[inline] pub unsafe fn mut_function_unchecked (&mut self) -> &mut Function { if cfg!(debug_assertions) { self.mut_function().unwrap() } else if let Self::Function(item) = self { item } else { unreachable_unchecked() } }
}

impl From<Alias> for NamespaceItem { #[inline] fn from (item: Alias) -> NamespaceItem { Self::Alias(item) } }
impl From<Module> for NamespaceItem { #[inline] fn from (item: Module) -> NamespaceItem { Self::Module(item) } }
impl From<Type> for NamespaceItem { #[inline] fn from (item: Type) -> NamespaceItem { Self::Type(item) } }
impl From<Global> for NamespaceItem { #[inline] fn from (item: Global) -> NamespaceItem { Self::Global(item) } }
impl From<Function> for NamespaceItem { #[inline] fn from (item: Function) -> NamespaceItem { Self::Function(item) } }

/// The kind of a top-level item known by a semantic analyzer
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NamespaceKind {
  Alias,
  Module,
  Type,
  Global,
  Function,
}

impl Display for NamespaceKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Alias => "Alias",
      Self::Module => "Module",
      Self::Type => "Type",
      Self::Global => "Global",
      Self::Function => "Function",
    })
  }
}

make_key_type! {
  /// A key referring to a top-level item in a semantic analyzer
  pub struct NamespaceKey;
}