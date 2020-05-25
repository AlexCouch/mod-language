//! The execution context and other core structures

use std::{
  mem::{ size_of, align_of, zeroed, },
  ffi::{ CStr, },
};

type PeekableSliceIter<'a, T> = std::iter::Peekable<std::slice::Iter<'a, T>>;

use llvm_sys::{
  prelude::{
    LLVMContextRef,
    LLVMModuleRef,
    LLVMBuilderRef,
    LLVMPassManagerRef,
    LLVMTypeRef,
    LLVMValueRef,
  },
  core::{
    LLVMContextCreate,
    LLVMContextDispose,
    LLVMModuleCreateWithNameInContext,
    // LLVMDisposeModule,
    LLVMCreateBuilderInContext,
    LLVMDisposeBuilder,
    LLVMCreateFunctionPassManagerForModule,
    LLVMDisposePassManager,
    LLVMInitializeFunctionPassManager,
    LLVMFinalizeFunctionPassManager,
    LLVMVoidTypeInContext,
    LLVMPointerType,
    LLVMInt1TypeInContext,
    LLVMInt8TypeInContext,
    LLVMInt16TypeInContext,
    LLVMInt32TypeInContext,
    LLVMInt64TypeInContext,
    LLVMFloatTypeInContext,
    LLVMDoubleTypeInContext,
    LLVMStructTypeInContext,
    LLVMFunctionType,
    LLVMDisposeMessage,
    LLVMAddGlobal,
    LLVMDeleteGlobal,
    LLVMAddFunction,
    LLVMDeleteFunction,
    LLVMSetFunctionCallConv,
  },
  target::{
    LLVM_InitializeNativeTarget,
    LLVM_InitializeNativeAsmPrinter,
    LLVM_InitializeNativeAsmParser,
    LLVMGetModuleDataLayout,
    // LLVMDisposeTargetData,
    LLVMTargetDataRef,
    LLVMABISizeOfType,
    LLVMABIAlignmentOfType,
    LLVMOffsetOfElement,
    // LLVMCopyStringRepOfTargetData,
  },
  execution_engine::{
    LLVMCreateExecutionEngineForModule,
    LLVMDisposeExecutionEngine,
    LLVMExecutionEngineRef,
    LLVMLinkInMCJIT,
    LLVMFreeMachineCodeForFunction,
  },
  transforms::pass_manager_builder::{
    LLVMPassManagerBuilderCreate,
    LLVMPassManagerBuilderSetOptLevel,
    LLVMPassManagerBuilderPopulateFunctionPassManager,
    LLVMPassManagerBuilderDispose,
  },
};

use mod_utils::{
  UnwrapUnchecked,
  into_decimal,
};

use mod_bytecode::{
  self as bc,
  Version,
};

pub use mod_bytecode::IntrinsicType;


use crate::{
  hash,
  c_lit,
  llvm_extras::{ LLVM_SUCCESS, ToLLVMInContext, },
  module_loader::{ load_module, load_modules, CompilationResult, },
};


/// Contains llvm Refs for a context
pub(crate) struct LLVMContext {
  pub(crate) ctx: LLVMContextRef,
  pub(crate) module: LLVMModuleRef,
  pub(crate) td: LLVMTargetDataRef,
  pub(crate) ee:  LLVMExecutionEngineRef,
  pub(crate) builder: LLVMBuilderRef,
  pub(crate) fpm: LLVMPassManagerRef,
}

impl Drop for LLVMContext {
  fn drop (&mut self) { unsafe {
    LLVMDisposePassManager(self.fpm);
    LLVMDisposeBuilder(self.builder);
    LLVMDisposeExecutionEngine(self.ee);
    // LLVMDisposeTargetData(self.td); the module owns this
    // LLVMDisposeModule(self.module); the execution engine owns this
    LLVMContextDispose(self.ctx);
  } }
}


/// The environment which stores all known
/// types, functions, and global
/// contributed by modules,
/// and provides a link between modules
pub struct Context {
  /// All modules known by a context
  modules: Vec<Module>,


  /// All types known by a Context
  types: Vec<Type>,


  /// TypeLinks to all Pointer types known by a Context
  pointer_types: Vec<TypeLink>,

  /// TypeLinks to all Structure types known by a Context
  structure_types: Vec<TypeLink>,
  /// Hash codes for all Structure types known by a Context
  structure_type_hashes: Vec<u64>,

  /// TypeLinks to all Function types known by a Context
  function_types: Vec<TypeLink>,
  /// Hash codes for all Function types known by a Context
  function_type_hashes: Vec<u64>,


  /// All globals known by a Context
  globals: Vec<Global>,
  

  /// All functions known by a Context
  functions: Vec<Function>,


  pub(crate) llvm: LLVMContext,
}


impl Default for Context { fn default () -> Self { Self::new(2).unwrap() } }

impl Context {
  const INTRINSIC_TYPES: &'static [(IntrinsicType, usize, usize)] = {
    use IntrinsicType::*;

    &[
      (Void, 0, 0),
      (Null, 8, 8),
      (Bool, size_of::<bool>(), align_of::<bool>()),
      (U8,   size_of::<u8>(),   align_of::<u8>()),
      (U16,  size_of::<u16>(),  align_of::<u16>()),
      (U32,  size_of::<u32>(),  align_of::<u32>()),
      (U64,  size_of::<u64>(),  align_of::<u64>()),
      (S8,   size_of::<i8>(),   align_of::<i8>()),
      (S16,  size_of::<i16>(),  align_of::<i16>()),
      (S32,  size_of::<i32>(),  align_of::<i32>()),
      (S64,  size_of::<i64>(),  align_of::<i64>()),
      (F32,  size_of::<f32>(),  align_of::<f32>()),
      (F64,  size_of::<f64>(),  align_of::<f64>()),
    ]
  };

  /// Create a new Context and initialize its internal data
  pub fn new (opt_level: u8) -> Result<Self, String> {
    if opt_level > 3 { return Err(format!("Invalid optimization level: {}, expected 0, 1, 2, or 3", opt_level)) }

    let mut llvm: LLVMContext;
    
    unsafe {
      llvm = zeroed();

      macro_rules! llvm_try {
        ($expr:expr, $err_msg:literal) => { if $expr != LLVM_SUCCESS { return Err($err_msg.to_owned()) } };
        ($expr:expr, $err_fmt:literal, $($err_args:expr),*) => { if $expr != LLVM_SUCCESS { return Err(format!($err_fmt, $($err_args),*)) } };
      }

      // initialize LLVM
      llvm_try!(LLVM_InitializeNativeTarget(), "Failed to initialize LLVM native target");
      llvm_try!(LLVM_InitializeNativeAsmPrinter(), "Failed to initialize LLVM assembly printer");
      llvm_try!(LLVM_InitializeNativeAsmParser(), "Failed to initialize LLVM assembly parser");
      
      LLVMLinkInMCJIT(); // NOTE afaik, this doesnt do anything but its required in order to link properly

      // create context
      llvm.ctx = LLVMContextCreate();

      // create module
      llvm.module = LLVMModuleCreateWithNameInContext(c_lit!("mod_rc context"), llvm.ctx);

      // create module target data
      llvm.td = LLVMGetModuleDataLayout(llvm.module);

      // create execution engine
      llvm.ee = zeroed();
      let mut err_msg = zeroed();

      llvm_try!(
        LLVMCreateExecutionEngineForModule(&mut llvm.ee, llvm.module, &mut err_msg),
        "Failed to create LLVM execution engine: {}",
        { let msg = CStr::from_ptr(err_msg).to_str().unwrap_or("[Error retrieiving error message]"); LLVMDisposeMessage(err_msg); msg }
      );

      // create instruction builder
      llvm.builder = LLVMCreateBuilderInContext(llvm.ctx);

      // create function pass manager
      let pmb = LLVMPassManagerBuilderCreate();

      LLVMPassManagerBuilderSetOptLevel(pmb, opt_level as _);
    
      llvm.fpm = LLVMCreateFunctionPassManagerForModule(llvm.module);
    
      llvm_try!(LLVMInitializeFunctionPassManager(llvm.fpm), "Failed to initialize LLVM function optimization pass");
    
      LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, llvm.fpm);
    
      llvm_try!(LLVMFinalizeFunctionPassManager(llvm.fpm), "Failed to finalize LLVM function optimization pass");
    
      LLVMPassManagerBuilderDispose(pmb);
    }


    let mut context = Self {
      modules: Vec::default(),

      types: Vec::default(),

      pointer_types: Vec::default(),
      
      structure_types: Vec::default(),
      structure_type_hashes: Vec::default(),

      function_types: Vec::default(),
      function_type_hashes: Vec::default(),

      globals: Vec::default(),

      functions: Vec::default(),

      llvm,
    };

    Self::INTRINSIC_TYPES
      .iter()
      .for_each(|&(ity, size, align)| context.types.push(Type {
        data: TypeData::Intrinsic(ity),
        size,
        align,
        llvm: ity.to_llvm_in_context(&context)
      }));

    Ok(context)
  }


  /// Get a reference to a Module from a ModuleLink
  /// 
  /// # Safety
  /// Because the only way to get a TypeLink is by using the Context's safe methods,
  /// this uses get_unsafe on it's internal array. If for some reason you have created a TypeLink
  /// by transmuting, it is up to you to determine the safety of this call
  pub fn get_module (&self, ml: ModuleLink) -> &Module {
    unsafe { self.modules.get_unchecked(ml.0) }
  }

  /// Get a ModuleLink from a module name
  pub fn find_module<S: AsRef<str>> (&self, name: S) -> Option<ModuleLink> {
    let name = name.as_ref();

    for (i, module) in self.modules.iter().enumerate() {
      if module.name == name {
        return Some(ModuleLink(i))
      }
    }

    None
  }

  /// Resolve a series of identifiers as a path to a Module Export
  pub fn resolve_path (&self, p: &[&str]) -> Option<GenericLink> {
    let mut iter = p.iter().peekable();

    let module_name = iter.next()?;
    let module_link = self.find_module(module_name)?;
    let module = self.get_module(module_link);

    fn traverse (exports: &[Export], idents: &mut PeekableSliceIter<&str>) -> Option<GenericLink> {
      let next_ident = *idents.next()?;

      for export in exports.iter() {
        if export.name == next_ident {
          match &export.data {
            ExportData::Namespace(exports) => return traverse(exports, idents),

            ExportData::Global(gl) => if idents.peek().is_none() {
              return Some(GenericLink::Global(*gl))
            } else {
              return None
            },

            ExportData::Function(fl) => if idents.peek().is_none() {
              return Some(GenericLink::Function(*fl))
            } else {
              return None
            },
          }
        }
      }

      None
    }

    traverse(module.exports.as_slice(), &mut iter)
  }

  /// Get a native address from a path into a context
  pub fn get_address (&self, p: &[&str]) -> Option<u64> {
    match self.resolve_path(p)? {
      GenericLink::Function(f_link) => {
        let func = self.get_function(f_link);

        if func.address != 0 {
          return Some(func.address)
        }
      },
      GenericLink::Global(g_link) => {
        let global = self.get_global(g_link);

        if global.address != 0 {
          return Some(global.address);
        }
      }
    }

    None
  }


  /// Compile and load a single bytecode Module into a Context
  pub fn load_module (&mut self, bc_module: bc::Module) -> CompilationResult {
    load_module(self, bc_module)
  }

  /// Compile and load multiple bytecode Modules into a Context, allowing cross-linking
  pub fn load_modules (&mut self, bc_modules: &[bc::Module]) -> CompilationResult {
    load_modules(self, bc_modules)
  }


  /// Add a new Module to a Context
  pub(crate) fn add_module (&mut self, m: Module) -> ModuleLink {
    let i = self.modules.len();
    self.modules.push(m);
    ModuleLink(i)
  }

  /// Determine how many Modules are contained in a Context
  pub fn module_count (&self) -> usize {
    self.modules.len()
  }


  /// Get a reference to a Type from a TypeLink
  /// 
  /// # Safety
  /// Because the only way to get a TypeLink is by using the Context's safe methods,
  /// this uses get_unsafe on it's internal array. If for some reason you have created a TypeLink
  /// by transmuting, it is up to you to determine the safety of this call
  pub fn get_type (&self, tl: TypeLink) -> &Type {
    unsafe { self.types.get_unchecked(tl.0) }
  }

  /// Add a new Type to a Context
  fn add_type (&mut self, ty: Type) -> TypeLink {
    let i = self.types.len();
    self.types.push(ty);
    TypeLink(i)
  }

  /// Determine how many Types are contained in a Context
  pub fn type_count (&self) -> usize {
    self.types.len()
  }
  
  /// Get a TypeLink from an IntrinsicType
  pub fn tl_intrinsic (&self, i: IntrinsicType) -> TypeLink {
    TypeLink(i as _)
  }

  /// Get a TypeLink for a Pointer from a value TypeLink
  pub fn tl_pointer (&mut self, value_tl: TypeLink) -> TypeLink {
    for &existing_tl in self.pointer_types.iter() {
      let existing_ty = self.get_type(existing_tl);

      if matches!(&existing_ty.data, &TypeData::Pointer(tl) if tl == value_tl) {
        return existing_tl
      }
    }

    let llvm = unsafe { LLVMPointerType(self.get_type(value_tl).llvm, 0) };

    let new_tl = self.add_type(Type { data: TypeData::Pointer(value_tl), size: 8, align: 8, llvm });
  
    self.pointer_types.push(new_tl);

    new_tl
  }

  /// Get a TypeLink for a Structure Type from a slice of field TypeLinks and a packing setting
  pub fn tl_structure (&mut self, field_tls: &[TypeLink], is_packed: bool) -> TypeLink {
    let new_hash = hash![ field_tls, is_packed ];

    for (&existing_hash, &existing_tl) in self.structure_type_hashes.iter().zip(self.structure_types.iter()) {
      if existing_hash == new_hash
      && self.get_type(existing_tl).data == (field_tls, is_packed)
      {
        return existing_tl
      }
    }

    let types = field_tls.to_owned();
    let mut llvm_types = Vec::new();
    let mut offsets = Vec::new();
    let mut aligns = Vec::new();
    let mut offset = 0usize;
    let mut align = if is_packed { 1usize } else { 0usize };

    for field_ty in types.iter().map(|&tl| self.get_type(tl)) {
      llvm_types.push(field_ty.llvm);

      if !is_packed {
        if field_ty.align > align { align = field_ty.align }

        let padding = (field_ty.align - (offset % field_ty.align)) % field_ty.align;

        offset += padding;
      }

      offsets.push(offset);
      aligns.push(field_ty.align);
        
      offset += field_ty.size;
    }

    let size = if align < 2 { offset } else { ((offset + align - 1) / align) * align };

    let llvm = unsafe { LLVMStructTypeInContext(self.llvm.ctx, llvm_types.as_mut_ptr(), llvm_types.len() as _, is_packed as _) };

    #[cfg(debug_assertions)]
    unsafe {
      debug_assert_eq!(LLVMABISizeOfType(self.llvm.td, llvm), size as _);
      debug_assert_eq!(LLVMABIAlignmentOfType(self.llvm.td, llvm), align as _);
      for (i, &offset) in offsets.iter().enumerate() {
        debug_assert_eq!(LLVMOffsetOfElement(self.llvm.td, llvm, i as _), offset as _);
      }
    }

    let new_tl = self.add_type(Type { data: TypeData::Structure { types, offsets, aligns, is_packed }, size, align, llvm });

    self.structure_types.push(new_tl);
    self.structure_type_hashes.push(new_hash);

    new_tl
  }

  /// Get a TypeLink for a Function Type from a slice of parameter TypeLinks and an optional result TypeLink
  pub fn tl_function (&mut self, param_tls: &[TypeLink], result: Option<TypeLink>, is_var_arg: bool) -> TypeLink {
    let new_hash = hash![ param_tls, result, is_var_arg ];

    for (&existing_hash, &existing_tl) in self.function_type_hashes.iter().zip(self.function_types.iter()) {
      if existing_hash == new_hash
      && self.get_type(existing_tl).data == (param_tls, result, is_var_arg)
      {
        return existing_tl
      }
    }

    let parameters = param_tls.to_owned();
    let mut parameters_llvm: Vec<_> = parameters.iter().map(|&tl| self.get_type(tl).llvm).collect();

    let result_llvm = if let Some(result_tl) = result { self.get_type(result_tl).llvm } else { unsafe { LLVMVoidTypeInContext(self.llvm.ctx) } };

    let llvm = unsafe { LLVMFunctionType(result_llvm, parameters_llvm.as_mut_ptr(), parameters_llvm.len() as _, is_var_arg as _) };

    let new_tl = self.add_type(Type { data: TypeData::Function { parameters, result, is_var_arg }, size: 8, align: 8, llvm });

    self.function_types.push(new_tl);
    self.function_type_hashes.push(new_hash);

    new_tl
  }


  /// Get a reference to a Global from a GlobalLink
  /// 
  /// # Safety
  /// Because the only way to get a GlobalLink is by using the Context's safe methods,
  /// this uses get_unsafe on it's internal array. If for some reason you have created a GlobalLink
  /// by transmuting, it is up to you to determine the safety of this call
  pub fn get_global (&self, gl: GlobalLink) -> &Global {
    unsafe { self.globals.get_unchecked(gl.0) }
  }

  /// Determine how many Globals are contained in a Context
  pub fn global_count (&self) -> usize {
    self.globals.len()
  }

  /// Purge all Globals created after a given index
  /// 
  /// # Safety
  /// This will invalidate any GlobalLinks,
  /// after (but not including) the given index.
  /// This is intended for error recovery, where the purged
  /// Globals will by definition not be referenced again;
  /// any other use is completely unsafe
  pub(crate) unsafe fn purge_globals (&mut self, base: usize) {
    while self.globals.len() > base {
      let global = self.globals.pop().unwrap_unchecked();

      LLVMDeleteGlobal(global.llvm)
    }
  }


  /// Create a new Global of the given type
  pub fn create_global (&mut self, ty: TypeLink) -> GlobalLink {
    let i = self.globals.len();

    let link = GlobalLink(i);
    let id: LLVMIdentifier = link.into();

    let llvm_ty = self.get_type(ty).llvm;

    let llvm = unsafe { LLVMAddGlobal(self.llvm.module, llvm_ty, id.as_ptr()) };

    self.globals.push(Global { id, ty, address: 0, initializer: None, llvm });

    link
  }

  /// Set the initializer function of a Global
  /// 
  /// Panics if the global already has an initializer
  pub fn set_global_initializer (&mut self, gl: GlobalLink, fl: FunctionLink) {
    unsafe { self.globals.get_unchecked_mut(gl.0) }.initializer.replace(fl).unwrap_none()
  }

  /// Set the address of a Global
  pub fn set_global_address (&mut self, gl: GlobalLink, address: u64) {
    unsafe { self.globals.get_unchecked_mut(gl.0) }.address = address
  }


  /// Get a reference to a Function from a FunctionLink
  /// 
  /// # Safety
  /// Because the only way to get a FunctionLink is by using the Context's safe methods,
  /// this uses get_unsafe on it's internal array. If for some reason you have created a FunctionLink
  /// by transmuting, it is up to you to determine the safety of this call
  pub fn get_function (&self, fl: FunctionLink) -> &Function {
    unsafe { self.functions.get_unchecked(fl.0) }
  }

  /// Determine how many Functions are contained in a Context
  pub fn function_count (&self) -> usize {
    self.functions.len()
  }

  /// Purge all Globals created after a given index
  /// 
  /// # Safety
  /// This will invalidate any GlobalLinks,
  /// after (but not including) the given index.
  /// This is intended for error recovery, where the purged
  /// Globals will by definition not be referenced again;
  /// any other use is completely unsafe
  pub(crate) unsafe fn purge_functions (&mut self, base: usize) {
    while self.functions.len() > base {
      let function = self.functions.pop().unwrap_unchecked();
      
      if function.address != 0 {
        LLVMFreeMachineCodeForFunction(self.llvm.ee, function.llvm)
      }
      
      LLVMDeleteFunction(function.llvm)
    }
  }

  /// Create a new Function of the given type
  /// 
  /// Panics if the given TypeLink is not a Function type
  pub fn create_function (&mut self, ty: TypeLink) -> FunctionLink {
    let i = self.functions.len();

    let link = FunctionLink(i);
    let id: LLVMIdentifier = link.into();

    let llvm_ty = {
      let ty = self.get_type(ty);
      
      assert!(matches!(&ty.data, TypeData::Function { .. }));

      ty.llvm
    };

    let llvm = unsafe { LLVMAddFunction(self.llvm.module, id.as_ptr(), llvm_ty) };

    // TODO support other CC's
    unsafe { LLVMSetFunctionCallConv(llvm, 0) }; // 0 -> C style CC

    self.functions.push(Function { id, ty, address: 0, llvm });

    link
  }

  /// Set the address of a Function
  pub fn set_function_address (&mut self, fl: FunctionLink, address: u64) {
    unsafe { self.functions.get_unchecked_mut(fl.0) }.address = address
  }
}



/// Represents a reference to a Module in the Context
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleLink(usize);

/// Represents a reference to a Type in the Context
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeLink(usize);

/// Represents a reference to a Global in the Context
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalLink(usize);

/// Represents a reference to a Function in the Context
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionLink(usize);

/// Represents a reference to either a Global or a Function in the Context
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericLink {
  Global(GlobalLink),
  Function(FunctionLink),
}



/// Represents a Module known by the Context
#[derive(Debug, PartialEq, Eq)]
pub struct Module {
  /// The unique name of a Module
  pub name: String,
  /// The version number of a Module
  pub version: Version,
  /// The export bindings of a Module
  pub exports: Vec<Export>,
}

impl Module {
  /// Create a new Module with no Exports
  pub fn empty (name: String, version: Version) -> Self {
    Self {
      name,
      version,
      exports: Vec::default(),
    }
  }
}

/// Represents an exported item in a Module
#[derive(Debug, PartialEq, Eq)]
pub struct Export {
  /// The name of an Export
  pub name: String,
  /// The variant data associated with an Export
  pub data: ExportData,
}

/// Data for an Export in a Module
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq)]
pub enum ExportData {
  Namespace(Vec<Export>),
  Global(GlobalLink),
  Function(FunctionLink),
}


/// Represents a Type known by the Context
#[derive(Debug, PartialEq, Eq)]
pub struct Type {
  /// The unique variant data of a Type
  pub data: TypeData,
  /// The size (in bytes) of a Type
  pub size: usize,
  /// The alignment (in bytes) of a Type
  pub align: usize,
  /// The LLVM representation of a Type
  llvm: LLVMTypeRef,
}

impl Type {
  pub(crate) fn llvm_t (&self, context: &Context) -> LLVMTypeRef {
    match &self.data {
      | TypeData::Intrinsic { .. }
      | TypeData::Pointer { .. }
      | TypeData::Structure { .. }
      => self.llvm,

      TypeData::Function { .. } => unsafe { LLVMPointerType(LLVMInt8TypeInContext(context.llvm.ctx), 0) }
    }
  }

  pub(crate) fn llvm_real_t (&self) -> LLVMTypeRef {
    self.llvm
  }
}


/// Unique variant data of a Type
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeData {
  /// An intrinsic type created by the compiler
  Intrinsic(IntrinsicType),

  /// The type of addresses
  Pointer(TypeLink),

  /// The basic aggregate type
  Structure {
    /// The types of each field of a Structure
    types: Vec<TypeLink>,
    /// The offset (in bytes) of each field of a Structure
    offsets: Vec<usize>,
    /// The alignments (in bytes) of each field of a Structure
    aligns: Vec<usize>,
    /// Whether or not a Structure type is packed
    is_packed: bool,
  },

  /// Functional signature type
  Function {
    /// The types of each parameter of a Function
    parameters: Vec<TypeLink>,
    /// The result type, if any, of a Function
    result: Option<TypeLink>,
    /// Whether or not a Function accepts a variable number of arguments
    is_var_arg: bool,
  },
}

impl PartialEq<(&[TypeLink], bool)> for TypeData {
  fn eq (&self, &(other_types, other_is_packed): &(&[TypeLink], bool)) -> bool {
    return matches!(
      self, &Self::Structure { ref types, is_packed, .. }
      if types.as_slice() == other_types
      && is_packed == other_is_packed
    )
  }
}

impl PartialEq<(&[TypeLink], Option<TypeLink>, bool)> for TypeData {
  fn eq (&self, &(other_parameters, other_result, other_is_var_arg): &(&[TypeLink], Option<TypeLink>, bool)) -> bool {
    return matches!(
      self, &Self::Function { ref parameters, result, is_var_arg }
      if parameters.as_slice() == other_parameters
      && result == other_result
      && is_var_arg == other_is_var_arg
    )
  }
}


// TODO get rid of this trait
impl ToLLVMInContext for IntrinsicType {
  type TargetInContext = LLVMTypeRef;

  fn to_llvm_in_context (&self, context: &Context) -> LLVMTypeRef {
    use IntrinsicType::*;

    unsafe { match self {
      Void => LLVMVoidTypeInContext(context.llvm.ctx),
      Null => LLVMPointerType(LLVMInt8TypeInContext(context.llvm.ctx), 0),
      Bool => LLVMInt1TypeInContext(context.llvm.ctx),
      U8  | S8  => LLVMInt8TypeInContext(context.llvm.ctx),
      U16 | S16 => LLVMInt16TypeInContext(context.llvm.ctx),
      U32 | S32 => LLVMInt32TypeInContext(context.llvm.ctx),
      U64 | S64 => LLVMInt64TypeInContext(context.llvm.ctx),
      F32 => LLVMFloatTypeInContext(context.llvm.ctx),
      F64 => LLVMDoubleTypeInContext(context.llvm.ctx),
    } }
  }
}



/// Contains a c-compatible string representing an LLVM value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LLVMIdentifier {
  data: [u8; 22],
  length: usize,
}

impl LLVMIdentifier {
  /// Create a new LLVMIdentifier from a kind byte and an index
  pub fn new (kind: u8, index: u64) -> Self {
    let mut id = Self {
      data: [0u8; 22],
      length: 0usize,
    };

    id.data[0] = kind;

    id.length = into_decimal(index, &mut id.data[1..]);

    id
  }

  /// Get a slice of the used bytes in an LLVMIdentifier
  pub fn as_slice (&self) -> &[u8] { self.as_ref() }
  /// Get a str of the used bytes in an LLVMIdentifier
  pub fn as_str (&self) -> &str { self.as_ref() }
  /// Get a cstr of the used bytes in an LLVMIdentifier
  pub fn as_cstr (&self) -> &CStr { self.as_ref() }
  /// Get a c-compattible pointer to the bytes in an LLVMIdentifier
  pub fn as_ptr (&self) -> *const i8 { self.data.as_ptr() as _ }
}

impl From<GlobalLink> for LLVMIdentifier { fn from (link: GlobalLink) -> Self { Self::new(b'G', link.0 as u64) } }
impl From<FunctionLink> for LLVMIdentifier { fn from (link: FunctionLink) -> Self { Self::new(b'F', link.0 as u64) } }

impl AsRef<[u8]> for LLVMIdentifier { fn as_ref (&self) -> &[u8] { &self.data[..self.length] } }
impl AsRef<str> for LLVMIdentifier { fn as_ref (&self) -> &str { unsafe { std::str::from_utf8_unchecked(self.as_ref()) } } }
impl AsRef<CStr> for LLVMIdentifier { fn as_ref (&self) -> &CStr { unsafe { CStr::from_bytes_with_nul_unchecked(&self.data[..self.length + 1]) } } }


/// Represents a Global variable known by the Context
#[derive(Debug, PartialEq, Eq)]
pub struct Global {
  /// The LLVM-compatible identifier associated with a Global
  pub id: LLVMIdentifier,
  /// A link to the Type of value a Global contains
  pub ty: TypeLink,
  /// The machine code address of a Global
  pub address: u64,
  /// The FunctionLink of an initializer function associated with a Global, if it has one
  pub initializer: Option<FunctionLink>,
  /// The LLVM representation of a Global
  pub(crate) llvm: LLVMValueRef,
}


/// Represents a Function variable known by the Context
#[derive(Debug, PartialEq, Eq)]
pub struct Function {
  /// The LLVM-compatible identifier associated with a Function
  pub id: LLVMIdentifier,
  /// A link to the Type of a Function
  pub ty: TypeLink,
  /// The machine code address of a Function
  pub address: u64,
  /// The LLVM representation of a Function
  pub(crate) llvm: LLVMValueRef,
}