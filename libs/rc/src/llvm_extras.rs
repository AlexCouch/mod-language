use llvm_sys::{
  prelude::{ LLVMBool, },
};

use crate::{
  context::{ Context, },
};


/// LLVMBool true value
// pub const LLVM_TRUE: LLVMBool = 1;
/// LLVMBool false value
// pub const LLVM_FALSE: LLVMBool = 0;
/// LLVMBool success value
pub const LLVM_SUCCESS: LLVMBool = 0;


/// Create an LLVM compatible value from an internal value
pub trait ToLLVM {
  /// The type of LLVM compatible value created by to_llvm calls on this type
  type Target;

  /// Create an LLVM compatible value from an internal value
  fn to_llvm (&self) -> Self::Target;
}

/// Create an LLVM compatible value from an internal value
pub trait ToLLVMInContext {
  /// The type of LLVM compatible value created by to_llvm_in_context calls on this type
  type TargetInContext;

  /// Create an LLVM compatible value from an internal value
  fn to_llvm_in_context (&self, context: &Context) -> Self::TargetInContext;
}