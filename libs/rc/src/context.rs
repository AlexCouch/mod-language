//! The execution context and other core structures

use std::{
  mem::{ size_of, align_of, zeroed, },
  ffi::{ CStr, },
};

use llvm_sys::{
  prelude::{
    LLVMContextRef,
    LLVMModuleRef,
    LLVMBuilderRef,
    LLVMPassManagerRef,
    LLVMTypeRef,
    // LLVMValueRef,
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
    LLVMCopyStringRepOfTargetData,
  },
  execution_engine::{
    LLVMCreateExecutionEngineForModule,
    LLVMDisposeExecutionEngine,
    LLVMExecutionEngineRef,
    LLVMLinkInMCJIT,
  },
  transforms::pass_manager_builder::{
    LLVMPassManagerBuilderCreate,
    LLVMPassManagerBuilderSetOptLevel,
    LLVMPassManagerBuilderPopulateFunctionPassManager,
    LLVMPassManagerBuilderDispose,
  },
};

use mod_bytecode::{
  IntrinsicType,
};

use crate::{
  hash,
  c_lit,
  llvm_extras::{ LLVM_SUCCESS, ToLLVMInContext, },
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

      // TODO: Do I need to specify a layout myself, since this is empty?
      #[cfg(debug_assertions)]
      {
        let layout_str = LLVMCopyStringRepOfTargetData(llvm.td);

        println!("Created context module with data layout {}", CStr::from_ptr(layout_str).to_str().unwrap_or("[Error retrieving layout str]"));

        LLVMDisposeMessage(layout_str);
      }

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
      types: Vec::default(),

      pointer_types: Vec::default(),
      
      structure_types: Vec::default(),
      structure_type_hashes: Vec::default(),

      function_types: Vec::default(),
      function_type_hashes: Vec::default(),

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
}



/// Represents a reference to a Type in the Context
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct TypeLink(usize);

/// Represents a reference to a Global in the Context
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalLink(usize);

/// Represents a reference to a Function in the Context
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FunctionLink(usize);



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
  pub(crate) llvm: LLVMTypeRef,
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
  fn eq (&self, struct_data: &(&[TypeLink], bool)) -> bool {
    return matches!(self, Self::Structure { types, is_packed, .. } if struct_data.0 == types.as_slice() && *is_packed == struct_data.1)
  }
}

impl PartialEq<(&[TypeLink], Option<TypeLink>, bool)> for TypeData {
  fn eq (&self, func_data: &(&[TypeLink], Option<TypeLink>, bool)) -> bool {
    return matches!(self, Self::Function { parameters, result, is_var_arg } if func_data.0 == parameters.as_slice() && *result == func_data.1 && *is_var_arg == func_data.2)
  }
}


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