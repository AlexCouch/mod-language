//! The runtime bytecode compiler for mod_language

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::match_ref_pats)]

#![feature(option_unwrap_none)]
#![feature(or_patterns)]

#[cfg(not(target_pointer_width = "64"))]
compile_error!("This crate can only be used on 64-bit targets");


pub mod utils;
pub(crate) mod llvm_extras;
pub mod context;
pub mod module_loader;
pub(crate) mod generator;


#[cfg(test)]
mod test {
  use super::*;
  use mod_bytecode::IntrinsicType;

  #[test]
  fn context_create_and_drop () {
    context::Context::new(3).expect("context creation works");
  }

  #[test]
  fn struct_creation () {
    let mut ctx = context::Context::default();

    let field_tys = &[ ctx.tl_intrinsic(IntrinsicType::Bool), ctx.tl_intrinsic(IntrinsicType::S64), ctx.tl_intrinsic(IntrinsicType::Bool) ];

    assert_ne!(
      ctx.tl_structure(field_tys, false),
      ctx.tl_structure(field_tys, true)
    );
  }
}