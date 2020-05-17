//! The bytecode form and component structures

#![allow(unused_imports)]
#![allow(dead_code)]

use std::{
  mem::{ transmute, },
};



/// Interface trait for encoding a Bytecode value into a byte buffer
pub trait Encode {
  /// Encodes a Bytecode value into a byte buffer
  fn encode (&self, buff: &mut Vec<u8>);
}

/// Interface trait for decoding a Bytecode value from a byte buffer
pub trait Decode: Sized {
  /// Decodes a Bytecode value from a byte buffer
  fn decode (buff: &mut &[u8]) -> Result<Self, DecodeError>;
}

/// An error resulting from attempting to decode a Bytecode value from an improperly formed byte buffer
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DecodeError {
  /// The Decoder hit the end of the byte buffer without getting enough data to complete a value
  EOF,
  /// The Decoder read a badly encoded String
  InvalidString,
  /// The Decoder encountered an unexpected value
  UnexpectedValue,
}



/// The main container of the in-memory representation of bytecode for a compilation
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
  /// The uniquely identifying name of a Module
  pub name: String,
  /// The semantic Version number of a Module
  pub version: Version,

  /// All Type definitions used by a Module
  pub types:     Vec<Type>,
  /// All Modules a Module depends on
  pub imports:   Vec<ImportModule>,
  /// All Global definitions of a Module, in initialization order
  pub globals:   Vec<Global>,
  /// All Function defintions of a Module
  pub functions: Vec<Function>,
  /// All Exports a Module exposes
  pub exports:   Vec<Export>,
}

impl Module {
  /// Create a new, empty Module
  pub fn empty (name: String, version: Version) -> Self {
    Self {
      name,
      version,

      types: Vec::default(),
      imports: Vec::default(),
      globals: Vec::default(),
      functions: Vec::default(),
      exports: Vec::default(),
    }
  }
}

impl Encode for Module {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.name.encode(buff);
    self.version.encode(buff);
    self.types.encode(buff);
    self.imports.encode(buff);
    self.globals.encode(buff);
    self.functions.encode(buff);
    self.exports.encode(buff);
  }
}

impl Decode for Module {
  fn decode (buff: &mut &[u8]) -> Result<Module, DecodeError> {
    Ok(Module {
      name: String::decode(buff)?,
      version: Version::decode(buff)?,
      types: Vec::decode(buff)?,
      imports: Vec::decode(buff)?,
      globals: Vec::decode(buff)?,
      functions: Vec::decode(buff)?,
      exports: Vec::decode(buff)?,
    })
  }
}



/// Represents a Module's semver2 version number
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Version {
  /// Bumped for incompatible API changes
  pub major: u8,
  /// Bumped for added functionality in a backwards compatible manner
  pub minor: u8,
  /// Bumped for backwards compatible bug fixes
  pub patch: u8,
}

impl Version {
  /// Create a new Version and initialize its values
  pub fn new (major: u8, minor: u8, patch: u8) -> Self {
    Self { major, minor, patch }
  }
}

impl Encode for Version {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.major.encode(buff);
    self.minor.encode(buff);
    self.patch.encode(buff);
  }
}

impl Decode for Version {
  fn decode (buff: &mut &[u8]) -> Result<Version, DecodeError> {
    Ok(Version {
      major: u8::decode(buff)?,
      minor: u8::decode(buff)?,
      patch: u8::decode(buff)?,
    })
  }
}



/// A unique (per-item-kind, per-module) index for an item in a Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Index(pub u64);

impl From<u64> for Index { fn from (i: u64) -> Self { Self(i) } }
impl From<Index> for u64 { fn from (i: Index) -> Self { i.0 } }

impl From<NamespaceIndex> for Index { fn from (i: NamespaceIndex) -> Self { Self(i.0) } }
impl From<TypeIndex> for Index { fn from (i: TypeIndex) -> Self { Self(i.0) } }
impl From<GlobalIndex> for Index { fn from (i: GlobalIndex) -> Self { Self(i.0) } }
impl From<FunctionIndex> for Index { fn from (i: FunctionIndex) -> Self { Self(i.0) } }
impl From<LocalIndex> for Index { fn from (i: LocalIndex) -> Self { Self(i.0) } }
impl From<ElementIndex> for Index { fn from (i: ElementIndex) -> Self { Self(i.0) } }

impl Encode for Index { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }

impl Decode for Index { fn decode (buff: &mut &[u8]) -> Result<Index, DecodeError> { Ok(Self(u64::decode(buff)?)) }}



/// A unique (per-module) index for a Namespace in a Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NamespaceIndex(pub u64);

/// A unique (per-module) index for a Type in a Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct TypeIndex(pub u64);

/// A unique (per-module) index for a Global in a Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalIndex(pub u64);

/// A unique (per-module) index for a Function in a Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FunctionIndex(pub u64);

/// A unique (per-function) index for a Local in a Function
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LocalIndex(pub u64);

/// A unique (per-struct) index for an element in a Type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ElementIndex(pub u64);

impl From<u64> for NamespaceIndex { fn from (i: u64) -> Self { Self(i) } }
impl From<u64> for TypeIndex { fn from (i: u64) -> Self { Self(i) } }
impl From<u64> for GlobalIndex { fn from (i: u64) -> Self { Self(i) } }
impl From<u64> for FunctionIndex { fn from (i: u64) -> Self { Self(i) } }
impl From<u64> for LocalIndex { fn from (i: u64) -> Self { Self(i) } }
impl From<u64> for ElementIndex { fn from (i: u64) -> Self { Self(i) } }

impl From<NamespaceIndex> for u64 { fn from (i: NamespaceIndex) -> Self { i.0 } }
impl From<TypeIndex> for u64 { fn from (i: TypeIndex) -> Self { i.0 } }
impl From<GlobalIndex> for u64 { fn from (i: GlobalIndex) -> Self { i.0 } }
impl From<FunctionIndex> for u64 { fn from (i: FunctionIndex) -> Self { i.0 } }
impl From<LocalIndex> for u64 { fn from (i: LocalIndex) -> Self { i.0 } }
impl From<ElementIndex> for u64 { fn from (i: ElementIndex) -> Self { i.0 } }

impl From<Index> for NamespaceIndex { fn from (i: Index) -> Self { Self(i.0) } }
impl From<Index> for TypeIndex { fn from (i: Index) -> Self { Self(i.0) } }
impl From<Index> for GlobalIndex { fn from (i: Index) -> Self { Self(i.0) } }
impl From<Index> for FunctionIndex { fn from (i: Index) -> Self { Self(i.0) } }
impl From<Index> for LocalIndex { fn from (i: Index) -> Self { Self(i.0) } }
impl From<Index> for ElementIndex { fn from (i: Index) -> Self { Self(i.0) } }

impl Encode for NamespaceIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }
impl Encode for TypeIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }
impl Encode for GlobalIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }
impl Encode for FunctionIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }
impl Encode for LocalIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }
impl Encode for ElementIndex { fn encode (&self, buff: &mut Vec<u8>) { self.0.encode(buff) } }

impl Decode for NamespaceIndex { fn decode (buff: &mut &[u8]) -> Result<NamespaceIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}
impl Decode for TypeIndex { fn decode (buff: &mut &[u8]) -> Result<TypeIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}
impl Decode for GlobalIndex { fn decode (buff: &mut &[u8]) -> Result<GlobalIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}
impl Decode for FunctionIndex { fn decode (buff: &mut &[u8]) -> Result<FunctionIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}
impl Decode for LocalIndex { fn decode (buff: &mut &[u8]) -> Result<LocalIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}
impl Decode for ElementIndex { fn decode (buff: &mut &[u8]) -> Result<ElementIndex, DecodeError> { Ok(Self(u64::decode(buff)?)) }}



/// Represents a Type definition in a Module
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
  /// The index of a Type in a Module's types list
  pub index: TypeIndex,
  /// The variant data associated with a Type
  pub data: TypeData,
}

impl Type {
  /// Create a new Type
  pub fn new (index: TypeIndex, data: TypeData) -> Self {
    Self { index, data }
  }
}

impl Encode for Type {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.index.encode(buff);
    self.data.encode(buff);
  }
}

impl Decode for Type {
  fn decode (buff: &mut &[u8]) -> Result<Type, DecodeError> {
    Ok(Type {
      index: TypeIndex::decode(buff)?,
      data: TypeData::decode(buff)?,
    })
  }
}



/// Variant data associated with a Type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
  /// A built in Type defined by the compiler
  Intrinsic(IntrinsicType),
  /// The address of a value of another type
  Pointer(TypeIndex),
  /// An aggregate containing a list of values of other types
  Struct(Vec<TypeIndex>),
  /// A functional interface signature
  Function {
    /// Types of values provided to a function
    parameters: Vec<TypeIndex>,
    /// Type of value returned by a function
    result: Option<TypeIndex>
  },
}

impl TypeData {
  /// Get the TypeDataType of a TypeData
  pub fn get_type (&self) -> TypeDataType {
    match self {
      TypeData::Intrinsic { .. } => TypeDataType::Intrinsic,
      TypeData::Pointer { .. } => TypeDataType::Pointer,
      TypeData::Struct { .. } => TypeDataType::Struct,
      TypeData::Function { .. } => TypeDataType::Function,
    }
  }
}

impl Default for TypeData { fn default () -> Self { Self::Intrinsic(IntrinsicType::default()) } }

impl Encode for TypeData {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.get_type().encode(buff);

    use TypeData::*;

    match self {
      Intrinsic(ity) => ity.encode(buff),
      Pointer(idx) => idx.encode(buff),
      Struct(fields) => fields.encode(buff),
      Function { parameters, result } => {
        parameters.encode(buff);
        result.encode(buff);
      },
    }
  }
}

impl Decode for TypeData {
  fn decode (buff: &mut &[u8]) -> Result<TypeData, DecodeError> {
    Ok(match TypeDataType::decode(buff)? {
      TypeDataType::Intrinsic => TypeData::Intrinsic(IntrinsicType::decode(buff)?),
      TypeDataType::Pointer => TypeData::Pointer(TypeIndex::decode(buff)?),
      TypeDataType::Struct => TypeData::Struct(Vec::decode(buff)?),
      TypeDataType::Function => TypeData::Function {
        parameters: Vec::decode(buff)?,
        result: Option::decode(buff)?,
      },
    })
  }
}



/// A data-less variant only version of ImportData/ExportData
/// 
/// See ImportData and ExportData for docs on each variant
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDataType {
  Intrinsic,
  Pointer,
  Struct,
  Function,
}

impl Encode for TypeDataType {
  fn encode (&self, buff: &mut Vec<u8>) {
    buff.push(*self as _)
  }
}

impl Decode for TypeDataType {
  fn decode (buff: &mut &[u8]) -> Result<TypeDataType, DecodeError> {
    let byte = u8::decode(buff)?;
    
    if byte >= TypeDataType::Intrinsic as _
    && byte <= TypeDataType::Function  as _ {
      Ok(unsafe { transmute(byte) })
    } else {
      Err(DecodeError::UnexpectedValue)
    }
  }
}



/// Variant data associated with a built in Type defined by the compiler
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum IntrinsicType {
  /// An empty type, with no associated values
  Void,
  /// A binary state
  Bool,
  /// 8 bit unsigned integer
  U8,
  /// 16 bit unsigned integer
  U16,
  /// 32 bit unsigned integer
  U32,
  /// 64 bit unsigned integer
  U64,
  /// 8 bit signed integer
  S8,
  /// 16 bit signed integer
  S16,
  /// 32 bit signed integer
  S32,
  /// 64 bit signed integer
  S64,
  /// 32 bit real
  F32,
  /// 64 bit real
  F64,
}

impl Default for IntrinsicType { fn default () -> Self { Self::Void } }

impl Encode for IntrinsicType {
  fn encode (&self, buff: &mut Vec<u8>) {
    buff.push(*self as _)
  }
}

impl Decode for IntrinsicType {
  fn decode (buff: &mut &[u8]) -> Result<IntrinsicType, DecodeError> {
    let byte = u8::decode(buff)?;
    
    if byte >= IntrinsicType::Void as _
    && byte <= IntrinsicType::F64  as _ {
      Ok(unsafe { transmute(byte) })
    } else {
      Err(DecodeError::UnexpectedValue)
    }
  }
}



/// Binds another Module imported by a Module
#[derive(Debug, Clone, PartialEq)]
pub struct ImportModule {
  /// The uniquely identifying name of an ImportModule
  pub name: String,
  /// The semantic Version number of an ImportModule
  pub version: Version,
  /// List of items bound from the ImportModule
  pub items: Vec<Import>,
}

impl ImportModule {
  /// Create a new, empty ImportModule
  pub fn empty (name: String, version: Version) -> Self {
    Self {
      name,
      version,
      items: Vec::default(),
    }
  }
}

impl Encode for ImportModule {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.name.encode(buff);
    self.version.encode(buff);
    self.items.encode(buff);
  }
}

impl Decode for ImportModule {
  fn decode (buff: &mut &[u8]) -> Result<ImportModule, DecodeError> {
    Ok(ImportModule {
      name: String::decode(buff)?,
      version: Version::decode(buff)?,
      items: Vec::decode(buff)?
    })
  }
}



/// Binds an item from an imported Module to a unique Index a Module
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
  /// The uniquely identifying name of an Import binding
  pub name: String,
  /// The variant data associated with an Import binding
  pub data: ImportData,
}

impl Import {
  /// Create a new Import
  pub fn new (name: String, data: ImportData) -> Self {
    Self { name, data }
  }
}

impl Encode for Import {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.name.encode(buff);
    self.data.encode(buff);
  }
}

impl Decode for Import {
  fn decode (buff: &mut &[u8]) -> Result<Import, DecodeError> {
    Ok(Import {
      name: String::decode(buff)?,
      data: ImportData::decode(buff)?,
    })
  }
}



/// Variant data associated with an Import
#[derive(Debug, Clone, PartialEq)]
pub enum ImportData {
  /// An imported Namespace; contains other Import bindings
  Namespace(Vec<Import>),
  /// An imported Global; contains a Global index and a Type index
  Global(GlobalIndex, TypeIndex),
  /// An imported Function; contains a Function index and a Type index
  Function(FunctionIndex, TypeIndex),
}

impl ImportData {
  /// Get the AliasDataType of an ImportData
  pub fn get_type (&self) -> AliasDataType {
    match self {
      ImportData::Namespace { .. } => AliasDataType::Namespace,
      ImportData::Global { .. } => AliasDataType::Global,
      ImportData::Function { .. } => AliasDataType::Function,
    }
  }
}

impl Encode for ImportData {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.get_type().encode(buff);

    use ImportData::*;

    match self {
      Namespace(items) => {
        items.encode(buff);
      },

      Global(g_idx, t_idx) => {
        g_idx.encode(buff);
        t_idx.encode(buff);
      },

      Function(f_idx, t_idx) => {
        f_idx.encode(buff);
        t_idx.encode(buff);
      },
    }
  }
}

impl Decode for ImportData {
  fn decode (buff: &mut &[u8]) -> Result<ImportData, DecodeError> {
    Ok(match AliasDataType::decode(buff)? {
      AliasDataType::Namespace => ImportData::Namespace(Vec::decode(buff)?),
      AliasDataType::Global    => ImportData::Global(GlobalIndex::decode(buff)?, TypeIndex::decode(buff)?),
      AliasDataType::Function  => ImportData::Function(FunctionIndex::decode(buff)?, TypeIndex::decode(buff)?),
    })
  }
}



/// Represents a Global variable definition in a Module
#[derive(Debug, Clone, PartialEq)]
pub struct Global {
  /// The index of a Global in a Module's globals list
  pub index: GlobalIndex,
  /// The index of a Global's Type
  pub ty: TypeIndex,
  /// The instructions used to initialize a Global
  pub initializer: Vec<Instruction>,
}

impl Global {
  /// Create a new, empty Global with no initializer
  pub fn empty (index: GlobalIndex, ty: TypeIndex) -> Self {
    Self {
      index,
      ty,
      initializer: Vec::default(),
    }
  }
}

impl Encode for Global {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.index.encode(buff);
    self.ty.encode(buff);
    self.initializer.encode(buff);
  }
}

impl Decode for Global {
  fn decode (buff: &mut &[u8]) -> Result<Global, DecodeError> {
    Ok(Global {
      index: GlobalIndex::decode(buff)?,
      ty: TypeIndex::decode(buff)?,
      initializer: Vec::decode(buff)?,
    })
  }
}



/// Represents a Function definition in a Module
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
  /// The index of a Function in a Module's globals list
  pub index: FunctionIndex,
  /// The index of a Function's Type
  pub ty: TypeIndex,
  /// The instructions used to execute a Function
  pub body: Vec<Instruction>,
}

impl Function {
  /// Create a new, empty Function with no body
  pub fn empty (index: FunctionIndex, ty: TypeIndex) -> Self {
    Self {
      index,
      ty,
      body: Vec::default(),
    }
  }
}

impl Encode for Function {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.index.encode(buff);
    self.ty.encode(buff);
    self.body.encode(buff);
  }
}

impl Decode for Function {
  fn decode (buff: &mut &[u8]) -> Result<Function, DecodeError> {
    Ok(Function {
      index: FunctionIndex::decode(buff)?,
      ty: TypeIndex::decode(buff)?,
      body: Vec::decode(buff)?,
    })
  }
}



/// Binds an item exported by a Module
#[derive(Debug, Clone, PartialEq)]
pub struct Export {
  /// The uniquely identifying name of an Export binding
  pub name: String,
  /// The variant data associated with an Export binding
  pub data: ExportData,
}

impl Export {
  /// Create a new Export
  pub fn new (name: String, data: ExportData) -> Self {
    Self { name, data }
  }
}

impl Encode for Export {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.name.encode(buff);
    self.data.encode(buff);
  }
}

impl Decode for Export {
  fn decode (buff: &mut &[u8]) -> Result<Export, DecodeError> {
    Ok(Export {
      name: String::decode(buff)?,
      data: ExportData::decode(buff)?,
    })
  }
}



/// Variant data associated with an Export
#[derive(Debug, Clone, PartialEq)]
pub enum ExportData {
  /// An exported Namespace; contains other Export bindings
  Namespace(Vec<Export>),
  /// An exported Global; contains a Global index
  Global(GlobalIndex),
  /// An exported Function; contains a Function index
  Function(FunctionIndex),
}

impl ExportData {
  /// Get the AliasDataType of an ExportData
  pub fn get_type (&self) -> AliasDataType {
    match self {
      ExportData::Namespace { .. } => AliasDataType::Namespace,
      ExportData::Global { .. } => AliasDataType::Global,
      ExportData::Function { .. } => AliasDataType::Function,
    }
  }
}

impl Encode for ExportData {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.get_type().encode(buff);
    
    use ExportData::*;

    match self {
      Namespace(items) => {
        items.encode(buff);
      },

      Global(g_idx) => g_idx.encode(buff),
      Function(f_idx) => f_idx.encode(buff),
    }
  }
}

impl Decode for ExportData {
  fn decode (buff: &mut &[u8]) -> Result<ExportData, DecodeError> {
    Ok(match AliasDataType::decode(buff)? {
      AliasDataType::Namespace => ExportData::Namespace(Vec::decode(buff)?),
      AliasDataType::Global    => ExportData::Global(GlobalIndex::decode(buff)?),
      AliasDataType::Function  => ExportData::Function(FunctionIndex::decode(buff)?),
    })
  }
}



/// A data-less variant only version of ImportData/ExportData
/// 
/// See ImportData and ExportData for docs on each variant
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AliasDataType {
  Namespace,
  Global,
  Function,
}

impl Encode for AliasDataType {
  fn encode (&self, buff: &mut Vec<u8>) {
    buff.push(*self as _);
  }
}

impl Decode for AliasDataType {
  fn decode (buff: &mut &[u8]) -> Result<AliasDataType, DecodeError> {
    let byte = u8::decode(buff)?;
    
    if byte >= AliasDataType::Namespace as _
    && byte <= AliasDataType::Function  as _ {
      Ok(unsafe { transmute(byte) })
    } else {
      Err(DecodeError::UnexpectedValue)
    }
  }
}



/// Bytecode instructions used by Global initializers and Function bodies
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Instruction {
  /// Does nothing, filler data
  NoOp,

  /// Pushes a constant value on the stack
  ImmediateValue(ImmediateValue),

  /// Creates a local variable in the current stack frame, of the type given by an index
  CreateLocal(TypeIndex),

  /// Gets the address of a local variable,
  /// then pushes it on the stack
  LocalAddress(LocalIndex),
  /// Gets the address of a global variable,
  /// then pushes it on the stack
  GlobalAddress(GlobalIndex),
  /// Gets the address of a function,
  /// then pushes it on the stack
  FunctionAddress(FunctionIndex),
  
  /// Pops an address off the stack and offsets it to the specified struct element index,
  /// then pushes it back on the stack with the type of the struct element
  GetElement(ElementIndex),

  /// Pops a value off the stack and casts it to the type given by an index,
  /// then pushes the newly typed value back on the stack
  Cast(TypeIndex),
  
  /// Pops a value off the stack and uses it as an address for a dereference,
  /// then pushes the loaded data back on the stack
  Load,
  /// Pops an address and a value off the stack (Addr, Val),
  /// and stores the value to the address
  Store,

  /// Pops a value off the stack and discards it
  Discard,

  /// Pops two values off the stack (A, B), performs addition (A + B),
  /// and pushes the result back on the stack
  Add,
  /// Pops two values off the stack (A, B), performs subtraction (A - B),
  /// and pushes the result back on the stack
  Sub,
  /// Pops two values off the stack (A, B), performs multiplication (A * B),
  /// and pushes the result back on the stack
  Mul,
  /// Pops two values off the stack (A, B), performs division (A / B),
  /// and pushes the result back on the stack
  Div,
  /// Pops two values off the stack (A, B), performs remainder division (A % B),
  /// and pushes the result back on the stack
  Rem,
  /// Pops a single value off the stack and negates its sign (-Val),
  /// and pushes the result back on the stack
  Neg,

  /// Pops two values off the stack (A, B), performs bitwise AND (A & B),
  /// and pushes the result back on the stack
  And,
  /// Pops two values off the stack (A, B), performs bitwise OR (A | B),
  /// and pushes the result back on the stack
  Or,
  /// Pops two values off the stack (A, B), performs bitwise XOR (A ~ B),
  /// and pushes the result back on the stack
  Xor,
  /// Pops two values off the stack (A, B), performs bitwise LSHIFT (A << B),
  /// and pushes the result back on the stack
  LShift,
  /// Pops two values off the stack (A, B), performs bitwise RSHIFT (A >> B),
  /// and pushes the result back on the stack
  RShift,
  /// Pops a single value off the stack, performs bitwise NOT,
  /// and pushes the result back on the stack
  Not,

  /// Pops two values off the stack (A, B), performs equality comparison (A == B),
  /// and pushes the result back on the stack
  EQ,
  /// Pops two values off the stack (A, B), performs inequality comparison (A != B),
  /// and pushes the result back on the stack
  NEQ,
  /// Pops two values off the stack (A, B), performs less than comparison (A < B),
  /// and pushes the result back on the stack
  LT,
  /// Pops two values off the stack (A, B), performs greater than comparison (A > B),
  /// and pushes the result back on the stack
  GT,
  /// Pops two values off the stack (A, B), performs less than or equal comparison (A <= B),
  /// and pushes the result back on the stack
  LEQ,
  /// Pops two values off the stack (A, B), performs greater than or equal comparison (A >= B),
  /// and pushes the result back on the stack
  GEQ,

  /// Calls a Module-local function by index,
  /// after popping a type-dependant amount of arguments off the stack.
  /// After the call is complete, pushes the function's return value back on the stack (if one was given)
  CallDirect(FunctionIndex),

  /// Pops a function address off the stack,
  /// then pops a type-dependant amount of arguments off the stack.
  /// After the call is complete, pushes the function's return value back on the stack (if one was given)
  CallIndirect,

  /// Pops a boolean value off the stack and uses it as a branch predicate,
  /// either executing its then branch or else branch depending on the boolean value
  /// 
  /// Conceptually contains other instructions,
  /// but in serialized form it is a sequence
  IfBlock(Vec<Instruction>, Vec<Instruction>),

  /// Continues executing its branch until a Break or Return instruction is reached
  /// 
  /// Conceptually contains other instructions,
  /// but in serialized form it is a sequence
  LoopBlock(Vec<Instruction>),

  /// Jumps to the end of a LoopBlock and ends the loop
  Break,
  /// Jumps back to the entry point of a LoopBlock and continues the loop
  Continue,

  /// Pops a value off the stack, and returns it from a function
  Ret,
  /// Returns from a function
  RetVoid,
}

impl Instruction {
  /// Get the InstructionType of an Instruction
  pub fn get_type (&self) -> InstructionType {
    match self {
      Instruction::NoOp { .. } => InstructionType::NoOp,
      Instruction::ImmediateValue { .. } => InstructionType::ImmediateValue,
      Instruction::CreateLocal { .. } => InstructionType::CreateLocal,
      Instruction::LocalAddress { .. } => InstructionType::LocalAddress,
      Instruction::GlobalAddress { .. } => InstructionType::GlobalAddress,
      Instruction::FunctionAddress { .. } => InstructionType::FunctionAddress,
      Instruction::GetElement { .. } => InstructionType::GetElement,
      Instruction::Cast { .. } => InstructionType::Cast,
      Instruction::Load { .. } => InstructionType::Load,
      Instruction::Store { .. } => InstructionType::Store,
      Instruction::Discard { .. } => InstructionType::Discard,
      Instruction::Add { .. } => InstructionType::Add,
      Instruction::Sub { .. } => InstructionType::Sub,
      Instruction::Mul { .. } => InstructionType::Mul,
      Instruction::Div { .. } => InstructionType::Div,
      Instruction::Rem { .. } => InstructionType::Rem,
      Instruction::Neg { .. } => InstructionType::Neg,
      Instruction::And { .. } => InstructionType::And,
      Instruction::Or { .. } => InstructionType::Or,
      Instruction::Xor { .. } => InstructionType::Xor,
      Instruction::LShift { .. } => InstructionType::LShift,
      Instruction::RShift { .. } => InstructionType::RShift,
      Instruction::Not { .. } => InstructionType::Not,
      Instruction::EQ { .. } => InstructionType::EQ,
      Instruction::NEQ { .. } => InstructionType::NEQ,
      Instruction::LT { .. } => InstructionType::LT,
      Instruction::GT { .. } => InstructionType::GT,
      Instruction::LEQ { .. } => InstructionType::LEQ,
      Instruction::GEQ { .. } => InstructionType::GEQ,
      Instruction::CallDirect { .. } => InstructionType::CallDirect,
      Instruction::CallIndirect { .. } => InstructionType::CallIndirect,
      Instruction::IfBlock { .. } => InstructionType::IfBlock,
      Instruction::LoopBlock { .. } => InstructionType::LoopBlock,
      Instruction::Break { .. } => InstructionType::Break,
      Instruction::Continue { .. } => InstructionType::Continue,
      Instruction::Ret { .. } => InstructionType::Ret,
      Instruction::RetVoid { .. } => InstructionType::RetVoid,
    }
  }
}

impl Encode for Instruction {
  fn encode (&self, buff: &mut Vec<u8>) {
    use Instruction::*;

    self.get_type().encode(buff);

    match self {
      | NoOp
      | Load
      | Store
      | Discard
      | Add
      | Sub
      | Mul
      | Div
      | Rem
      | Neg
      | And
      | Or
      | Xor
      | LShift
      | RShift
      | Not
      | EQ
      | NEQ
      | LT
      | GT
      | LEQ
      | GEQ
      | CallIndirect
      | Break
      | Continue
      | Ret
      | RetVoid
      => { },

      ImmediateValue(imm) => imm.encode(buff),

      CreateLocal(t_idx) => t_idx.encode(buff),
      LocalAddress(l_idx) => l_idx.encode(buff),
      GlobalAddress(g_idx) => g_idx.encode(buff),
      FunctionAddress(f_idx) => f_idx.encode(buff),
      GetElement(e_idx) => e_idx.encode(buff),
      Cast(t_idx) => t_idx.encode(buff),
      CallDirect(f_idx) => f_idx.encode(buff),

      IfBlock(then_instrs, else_instrs) => {
        then_instrs.encode(buff);
        else_instrs.encode(buff);
      },

      LoopBlock(loop_instrs) => loop_instrs.encode(buff),
    }
  }
}

impl Decode for Instruction {
  fn decode (buff: &mut &[u8]) -> Result<Instruction, DecodeError> {
    Ok(match InstructionType::decode(buff)? {
      InstructionType::NoOp => Instruction::NoOp,
      InstructionType::Load => Instruction::Load,
      InstructionType::Store => Instruction::Store,
      InstructionType::Discard => Instruction::Discard,
      InstructionType::Add => Instruction::Add,
      InstructionType::Sub => Instruction::Sub,
      InstructionType::Mul => Instruction::Mul,
      InstructionType::Div => Instruction::Div,
      InstructionType::Rem => Instruction::Rem,
      InstructionType::Neg => Instruction::Neg,
      InstructionType::And => Instruction::And,
      InstructionType::Or => Instruction::Or,
      InstructionType::Xor => Instruction::Xor,
      InstructionType::LShift => Instruction::LShift,
      InstructionType::RShift => Instruction::RShift,
      InstructionType::Not => Instruction::Not,
      InstructionType::EQ => Instruction::EQ,
      InstructionType::NEQ => Instruction::NEQ,
      InstructionType::LT => Instruction::LT,
      InstructionType::GT => Instruction::GT,
      InstructionType::LEQ => Instruction::LEQ,
      InstructionType::GEQ => Instruction::GEQ,
      InstructionType::CallIndirect => Instruction::CallIndirect,
      InstructionType::Break => Instruction::Break,
      InstructionType::Continue => Instruction::Continue,
      InstructionType::Ret => Instruction::Ret,
      InstructionType::RetVoid => Instruction::RetVoid,

      InstructionType::ImmediateValue => Instruction::ImmediateValue(ImmediateValue::decode(buff)?),

      InstructionType::CreateLocal => Instruction::CreateLocal(TypeIndex::decode(buff)?),
      InstructionType::LocalAddress => Instruction::LocalAddress(LocalIndex::decode(buff)?),
      InstructionType::GlobalAddress => Instruction::GlobalAddress(GlobalIndex::decode(buff)?),
      InstructionType::FunctionAddress => Instruction::FunctionAddress(FunctionIndex::decode(buff)?),
      InstructionType::GetElement => Instruction::GetElement(ElementIndex::decode(buff)?),
      InstructionType::Cast => Instruction::Cast(TypeIndex::decode(buff)?),
      InstructionType::CallDirect => Instruction::CallDirect(FunctionIndex::decode(buff)?),

      InstructionType::IfBlock => Instruction::IfBlock(Vec::decode(buff)?, Vec::decode(buff)?),
      InstructionType::LoopBlock => Instruction::LoopBlock(Vec::decode(buff)?),
    })
  }
}



/// A data-less variant only version of Instruction
/// 
/// See Instruction for docs on each variant
/// 
/// Contains some InstructionTypes which do not correlate to a variant of Instruction,
/// for example Else, and End, which mark the ends of IfBlock branches and LoopBlocks
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InstructionType {
  NoOp,

  ImmediateValue,

  CreateLocal,

  LocalAddress,
  GlobalAddress,
  FunctionAddress,
  
  GetElement,

  Cast,
  
  Load,
  Store,

  Discard,

  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Neg,

  And,
  Or,
  Xor,
  LShift,
  RShift,
  Not,

  EQ,
  NEQ,
  LT,
  GT,
  LEQ,
  GEQ,

  CallDirect,

  CallIndirect,

  IfBlock,

  LoopBlock,

  Break,
  Continue,

  Ret,
  RetVoid,
}

impl Default for InstructionType { fn default () -> Self { Self::NoOp } }

impl Encode for InstructionType {
  fn encode (&self, buff: &mut Vec<u8>) {
    buff.push(*self as _)
  }
}

impl Decode for InstructionType {
  fn decode (buff: &mut &[u8]) -> Result<InstructionType, DecodeError> {
    let byte = u8::decode(buff)?;
    
    if byte >= InstructionType::NoOp as _
    && byte <= InstructionType::RetVoid  as _ {
      Ok(unsafe { transmute(byte) })
    } else {
      Err(DecodeError::UnexpectedValue)
    }
  }
}



/// A literal value encoded directly into a bytecode instruction
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ImmediateValue {
  /// A binary state
  Bool(bool),
  /// 8 bit unsigned integer
  U8(u8),
  /// 16 bit unsigned integer
  U16(u16),
  /// 32 bit unsigned integer
  U32(u32),
  /// 64 bit unsigned integer
  U64(u64),
  /// 8 bit signed integer
  S8(i8),
  /// 16 bit signed integer
  S16(i16),
  /// 32 bit signed integer
  S32(i32),
  /// 64 bit signed integer
  S64(i64),
  /// 32 bit real
  F32(f32),
  /// 64 bit real
  F64(f64),
}

impl ImmediateValue {
  /// Get the IntrinsicType of an ImmediateValue
  pub fn get_intrinsic_type (&self) -> IntrinsicType {
    match self {
      Self::Bool(_) => IntrinsicType::Bool,
      Self::U8(_) => IntrinsicType::U8,
      Self::U16(_) => IntrinsicType::U16,
      Self::U32(_) => IntrinsicType::U32,
      Self::U64(_) => IntrinsicType::U64,
      Self::S8(_) => IntrinsicType::S8,
      Self::S16(_) => IntrinsicType::S16,
      Self::S32(_) => IntrinsicType::S32,
      Self::S64(_) => IntrinsicType::S64,
      Self::F32(_) => IntrinsicType::F32,
      Self::F64(_) => IntrinsicType::F64,
    }
  }
}

impl Encode for ImmediateValue {
  fn encode (&self, buff: &mut Vec<u8>) {
    use ImmediateValue::*;

    self.get_intrinsic_type().encode(buff);

    match self {
      &Bool(x) => x.encode(buff),

      &U8(x) => x.encode(buff),
      U16(x) => x.encode(buff),
      U32(x) => x.encode(buff),
      U64(x) => x.encode(buff),
      &S8(x) => x.encode(buff),
      S16(x) => x.encode(buff),
      S32(x) => x.encode(buff),
      S64(x) => x.encode(buff),
      F32(x) => x.encode(buff),
      F64(x) => x.encode(buff),
    }
  }
}

impl Decode for ImmediateValue {
  fn decode (buff: &mut &[u8]) -> Result<ImmediateValue, DecodeError> {
    Ok(match IntrinsicType::decode(buff)? {
      IntrinsicType::Bool => bool::decode(buff)?.into(),

      IntrinsicType::U8  => u8 ::decode(buff)?.into(),
      IntrinsicType::U16 => u16::decode(buff)?.into(),
      IntrinsicType::U32 => u32::decode(buff)?.into(),
      IntrinsicType::U64 => u64::decode(buff)?.into(),
      IntrinsicType::S8  => i8 ::decode(buff)?.into(),
      IntrinsicType::S16 => i16::decode(buff)?.into(),
      IntrinsicType::S32 => i32::decode(buff)?.into(),
      IntrinsicType::S64 => i64::decode(buff)?.into(),
      IntrinsicType::F32 => f32::decode(buff)?.into(),
      IntrinsicType::F64 => f64::decode(buff)?.into(),
      
      IntrinsicType::Void => return Err(DecodeError::UnexpectedValue),
    })
  }
}

impl From<bool> for ImmediateValue { fn from (u: bool) -> Self { Self::Bool (u) } }

impl From<u8>  for ImmediateValue { fn from (u: u8)  -> Self { Self::U8  (u) } }
impl From<u16> for ImmediateValue { fn from (u: u16) -> Self { Self::U16 (u) } }
impl From<u32> for ImmediateValue { fn from (u: u32) -> Self { Self::U32 (u) } }
impl From<u64> for ImmediateValue { fn from (u: u64) -> Self { Self::U64 (u) } }
impl From<i8>  for ImmediateValue { fn from (s: i8)  -> Self { Self::S8  (s) } }
impl From<i16> for ImmediateValue { fn from (s: i16) -> Self { Self::S16 (s) } }
impl From<i32> for ImmediateValue { fn from (s: i32) -> Self { Self::S32 (s) } }
impl From<i64> for ImmediateValue { fn from (s: i64) -> Self { Self::S64 (s) } }
impl From<f32> for ImmediateValue { fn from (f: f32) -> Self { Self::F32 (f) } }
impl From<f64> for ImmediateValue { fn from (f: f64) -> Self { Self::F64 (f) } }

impl From<usize> for ImmediateValue { fn from (u: usize) -> Self { Self::U64 (u as _) } }
impl From<isize> for ImmediateValue { fn from (s: isize) -> Self { Self::S64 (s as _) } }



impl Encode for str {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.len().encode(buff);
    self.as_bytes().encode(buff);
  }
}

impl Encode for bool { fn encode (&self, buff: &mut Vec<u8>) { buff.push(*self as _); } }
impl Encode for u8   { fn encode (&self, buff: &mut Vec<u8>) { buff.push(*self as _); } }
impl Encode for i8   { fn encode (&self, buff: &mut Vec<u8>) { buff.push(*self as _); } }

impl Encode for u16 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for u32 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for u64 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for i16 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for i32 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for i64 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for f32 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }
impl Encode for f64 { fn encode (&self, buff: &mut Vec<u8>) { self.to_le_bytes().encode(buff) } }

impl Encode for usize { fn encode (&self, buff: &mut Vec<u8>) { (*self as u64).encode(buff) } }
impl Encode for isize { fn encode (&self, buff: &mut Vec<u8>) { (*self as i64).encode(buff) } }

impl<E: Encode> Encode for [E] {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.iter().for_each(|e| e.encode(buff))
  }
}

impl<E: Encode> Encode for Vec<E> {
  fn encode (&self, buff: &mut Vec<u8>) {
    self.len().encode(buff);
    self.iter().for_each(|e| e.encode(buff))
  }
}

impl<E: Encode> Encode for Option<E> {
  fn encode (&self, buff: &mut Vec<u8>) {
    match self {
      Some(e) => {
        true.encode(buff);
        e.encode(buff);
      },
      None => false.encode(buff)
    }
  }
}


impl Decode for u8 {
  fn decode (buff: &mut &[u8]) -> Result<u8, DecodeError> {
    match buff.get(0) {
      Some(&b) => { *buff = &buff[1..]; Ok(b) },
      None =>  Err(DecodeError::EOF)
    }
  }
}


impl Decode for bool { fn decode (buff: &mut &[u8]) -> Result<bool, DecodeError> { Ok(u8::decode(buff)? == 1 ) } }
impl Decode for i8   { fn decode (buff: &mut &[u8]) -> Result<i8,   DecodeError> { Ok(u8::decode(buff)? as i8) } }

type BytePair  = [u8; 2];

impl Decode for BytePair  {
  fn decode (buff: &mut &[u8]) -> Result<BytePair,  DecodeError> {
    if buff.len() >= 2 {
      let arr = unsafe { * (buff.as_ptr() as *const BytePair) };
      *buff = &buff[2..];
      Ok(arr)
    } else {
      Err(DecodeError::EOF)
    }
  }
}

type ByteQuad  = [u8; 4];

impl Decode for ByteQuad  {
  fn decode (buff: &mut &[u8]) -> Result<ByteQuad,  DecodeError> {
    if buff.len() >= 4 {
      let arr = unsafe { * (buff.as_ptr() as *const ByteQuad) };
      *buff = &buff[4..];
      Ok(arr)
    } else {
      Err(DecodeError::EOF)
    }
  }
}

type ByteOctet = [u8; 8];

impl Decode for ByteOctet  {
  fn decode (buff: &mut &[u8]) -> Result<ByteOctet,  DecodeError> {
    if buff.len() >= 8 {
      let arr = unsafe { *(buff.as_ptr() as *const ByteOctet) };
      *buff = &buff[8..];
      Ok(arr)
    } else {
      Err(DecodeError::EOF)
    }
  }
}

impl Decode for u16 { fn decode (buff: &mut &[u8]) -> Result<u16, DecodeError> { Ok(u16::from_le_bytes( BytePair::decode(buff)?)) }}
impl Decode for u32 { fn decode (buff: &mut &[u8]) -> Result<u32, DecodeError> { Ok(u32::from_le_bytes( ByteQuad::decode(buff)?)) }}
impl Decode for u64 { fn decode (buff: &mut &[u8]) -> Result<u64, DecodeError> { Ok(u64::from_le_bytes(ByteOctet::decode(buff)?)) }}

impl Decode for i16 { fn decode (buff: &mut &[u8]) -> Result<i16, DecodeError> { Ok(i16::from_le_bytes( BytePair::decode(buff)?)) }}
impl Decode for i32 { fn decode (buff: &mut &[u8]) -> Result<i32, DecodeError> { Ok(i32::from_le_bytes( ByteQuad::decode(buff)?)) }}
impl Decode for i64 { fn decode (buff: &mut &[u8]) -> Result<i64, DecodeError> { Ok(i64::from_le_bytes(ByteOctet::decode(buff)?)) }}

impl Decode for f32 { fn decode (buff: &mut &[u8]) -> Result<f32, DecodeError> { Ok(f32::from_le_bytes( ByteQuad::decode(buff)?)) }}
impl Decode for f64 { fn decode (buff: &mut &[u8]) -> Result<f64, DecodeError> { Ok(f64::from_le_bytes(ByteOctet::decode(buff)?)) }}

impl Decode for usize { fn decode (buff: &mut &[u8]) -> Result<usize, DecodeError> { Ok(u64::decode(buff)? as _) } }
impl Decode for isize { fn decode (buff: &mut &[u8]) -> Result<isize, DecodeError> { Ok(i64::decode(buff)? as _) } }


impl Decode for String {
  fn decode (buff: &mut &[u8]) -> Result<String, DecodeError> {
    let length = u64::decode(buff)? as usize;

    if buff.len() < length { return Err(DecodeError::EOF) }

    match std::str::from_utf8(&buff[..length]) {
      Ok(str) => {
        let string = str.to_string();

        *buff = &buff[length..];
        
        Ok(string)
      },
      Err(_) => Err(DecodeError::InvalidString)
    }
  }
}

impl<D: Decode> Decode for Vec<D> {
  fn decode (buff: &mut &[u8]) -> Result<Vec<D>, DecodeError> {
    let length = u64::decode(buff)? as usize;

    let mut vec = Vec::with_capacity(length);

    for _ in 0..length {
      vec.push(D::decode(buff)?)
    }

    Ok(vec)
  }
}

impl<D: Decode> Decode for Option<D> {
  fn decode (buff: &mut &[u8]) -> Result<Option<D>, DecodeError> {
    let is_some = bool::decode(buff)?;
    if is_some {
      Ok(Some(D::decode(buff)?))
    } else {
      Ok(None)
    }
  }
}



#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_bytecode_encode_decode () {
    let module = Module {
      name: "test_module".to_owned(),
      version: Version::new(0, 0, 1),
      types: vec! [
        Type::new(0.into(), TypeData::Intrinsic(IntrinsicType::S64)),
        Type::new(1.into(), TypeData::Function { parameters: vec! [ 0.into(), 0.into() ], result: Some(0.into()) }),
        Type::new(2.into(), TypeData::Function { parameters: vec! [ ], result: Some(0.into()) }),
      ],
      imports: vec! [
        ImportModule {
          name: "test_import_module".to_owned(),
          version: Version::new(1, 2, 0),
          items: vec! [
            Import::new("test_import_namespace".to_owned(), ImportData::Namespace(vec! [
              Import::new("test_import_global".to_owned(), ImportData::Global(0.into(), 0.into()))
            ])),
            Import::new("test_import_function".to_owned(), ImportData::Function(0.into(), 1.into())),
          ]
        }
      ],
      globals: vec! [
        Global {
          index: 1.into(),
          ty: 0.into(),
          initializer: vec! [
            Instruction::ImmediateValue(ImmediateValue::S64(99))
          ]
        },
        Global {
          index: 2.into(),
          ty: 0.into(),
          initializer: vec! [
            Instruction::CallDirect(1.into())
          ]
        }
      ],
      functions: vec! [
        Function {
          index: 1.into(),
          ty: 2.into(),
          body: vec! [
            Instruction::GlobalAddress(1.into()),
            Instruction::Load,
            Instruction::ImmediateValue(ImmediateValue::S64(1)),
            Instruction::CallDirect(2.into()),
            Instruction::Ret,
          ]
        },
        Function {
          index: 2.into(),
          ty: 1.into(),
          body: vec! [
            Instruction::LocalAddress(0.into()),
            Instruction::LocalAddress(1.into()),
            Instruction::Sub,
            Instruction::Ret,
          ]
        }
      ],
      exports: vec! [
        Export::new("test_export_namespace".to_owned(), ExportData::Namespace(vec! [
          Export::new("test_export_function".to_owned(), ExportData::Function(1.into())),
        ])),
        Export::new("test_export_global".to_owned(), ExportData::Global(1.into())),
        Export::new("test_reexport".to_owned(), ExportData::Global(0.into())),
      ]
    };

    let mut encoded = Vec::default();
    module.encode(&mut encoded);

    let mut decoder = encoded.as_slice();
    let decoded = Module::decode(&mut decoder).expect("Failed to decode module");

    assert_eq!(module, decoded)
  }

  #[test]
  fn test_all_instruction_encode_decode () {
    use Instruction::*;
    
    let instructions = vec! [
      NoOp,

      ImmediateValue(99i32.into()),

      CreateLocal(64.into()),

      LocalAddress(12.into()),
      GlobalAddress(13.into()),
      FunctionAddress(14.into()),
      
      GetElement(55.into()),

      Cast(11.into()),
      
      Load,
      Store,

      Discard,

      Add,
      Sub,
      Mul,
      Div,
      Rem,
      Neg,

      And,
      Or,
      Xor,
      LShift,
      RShift,
      Not,

      EQ,
      NEQ,
      LT,
      GT,
      LEQ,
      GEQ,

      CallDirect(4.into()),

      CallIndirect,

      IfBlock(vec! [ // random instructions
        GetElement(55.into()),
        Cast(11.into()),
        Ret,
        RetVoid,
        Load,
        Store,
        IfBlock(vec! [ //nesting
          Div,
          Rem,
          Ret,
          RetVoid,
          NoOp,
          Neg,
        ], vec! [
          EQ,
          NoOp,
          Ret,
          RetVoid,
          ImmediateValue(99i32.into()),
          NEQ,
          LT,
        ])
      ], vec![
        LShift,
        RShift,
        Not,
        FunctionAddress(14.into()),
        Ret,
        RetVoid,
        GetElement(55.into()),
        Cast(11.into()),
        LoopBlock(vec! [ //nesting
          NoOp,
          Ret,
          RetVoid,
          ImmediateValue(99i32.into()),
          CreateLocal(64.into()),
          IfBlock(vec! [
            Div,
            Rem,
            Neg,
          ], vec! [
            EQ,
            NEQ,
            LT,
          ])
        ]),
      ]),

      LoopBlock(vec! [ //random instructions
        LShift,
        RShift,
        Not,
        FunctionAddress(14.into()),
        IfBlock(vec! [ //nesting
          Div,
          Neg,
          Rem,
        ], vec! [
          NEQ,
          EQ,
          Break,
          LT,
        ]),
        Break,
      ]),

      Break,
      Continue,

      Ret,
      RetVoid,
    ];

    let mut encoded = Vec::default();
    instructions.encode(&mut encoded);

    let mut decoder = encoded.as_slice();
    let decoded = Vec::decode(&mut decoder).expect("Failed to decode instructions");

    assert_eq!(instructions, decoded)
  }

  #[test]
  fn test_enum_encode_decode () {
    let bad_data = vec! [ 255 ];


    let type_data_types = vec! [
      TypeDataType::Function,
      TypeDataType::Intrinsic,
      TypeDataType::Pointer,
      TypeDataType::Struct,
    ];

    let mut encoded = Vec::default();
    type_data_types.encode(&mut encoded);

    let mut decoder = encoded.as_slice();
    let decoded = Vec::decode(&mut decoder).expect("Failed to decode type_data_types");

    assert_eq!(type_data_types, decoded);

    let mut decoder = bad_data.as_slice();
    TypeDataType::decode(&mut decoder).expect_err("TypeDataType decoder failed to reject out of range value");


    let intrinsic_types = vec! [
      IntrinsicType::Void,
      IntrinsicType::Bool,
      IntrinsicType::U8,
      IntrinsicType::U16,
      IntrinsicType::U32,
      IntrinsicType::U64,
      IntrinsicType::S8,
      IntrinsicType::S16,
      IntrinsicType::S32,
      IntrinsicType::S64,
      IntrinsicType::F32,
      IntrinsicType::F64,
    ];

    let mut encoded = Vec::default();
    intrinsic_types.encode(&mut encoded);
    
    let mut decoder = encoded.as_slice();
    let decoded = Vec::decode(&mut decoder).expect("Failed to decode intrinsic_types");

    assert_eq!(intrinsic_types, decoded);

    let mut decoder = bad_data.as_slice();
    IntrinsicType::decode(&mut decoder).expect_err("IntrinsicType decoder failed to reject out of range value");


    let alias_data_types = vec! [
      AliasDataType::Function,
      AliasDataType::Global,
      AliasDataType::Namespace,
    ];

    let mut encoded = Vec::default();
    alias_data_types.encode(&mut encoded);
    
    let mut decoder = encoded.as_slice();
    let decoded = Vec::decode(&mut decoder).expect("Failed to decode alias_data_types");

    assert_eq!(alias_data_types, decoded);

    let mut decoder = bad_data.as_slice();
    AliasDataType::decode(&mut decoder).expect_err("AliasDataType decoder failed to reject out of range value");


    let instruction_types = vec! [
      InstructionType::NoOp,
      InstructionType::ImmediateValue,
      InstructionType::CreateLocal,
      InstructionType::LocalAddress,
      InstructionType::GlobalAddress,
      InstructionType::FunctionAddress,
      InstructionType::GetElement,
      InstructionType::Cast,
      InstructionType::Load,
      InstructionType::Store,
      InstructionType::Discard,
      InstructionType::Add,
      InstructionType::Sub,
      InstructionType::Mul,
      InstructionType::Div,
      InstructionType::Rem,
      InstructionType::Neg,
      InstructionType::And,
      InstructionType::Or,
      InstructionType::Xor,
      InstructionType::LShift,
      InstructionType::RShift,
      InstructionType::Not,
      InstructionType::EQ,
      InstructionType::NEQ,
      InstructionType::LT,
      InstructionType::GT,
      InstructionType::LEQ,
      InstructionType::GEQ,
      InstructionType::CallDirect,
      InstructionType::CallIndirect,
      InstructionType::IfBlock,
      InstructionType::LoopBlock,
      InstructionType::Break,
      InstructionType::Continue,
      InstructionType::Ret,
      InstructionType::RetVoid,
    ];

    let mut encoded = Vec::default();
    instruction_types.encode(&mut encoded);
    
    let mut decoder = encoded.as_slice();
    let decoded = Vec::decode(&mut decoder).expect("Failed to decode instruction_types");

    assert_eq!(instruction_types, decoded);

    let mut decoder = bad_data.as_slice();
    InstructionType::decode(&mut decoder).expect_err("InstructionType decoder failed to reject out of range value");
  }
}