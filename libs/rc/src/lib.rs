//! The runtime bytecode compiler for mod_language

#![warn(missing_docs)]
#![warn(clippy::all)]


#[cfg(not(target_pointer_width = "64"))]
compile_error!("This crate can only be used on 64-bit systems");


pub mod utils;
pub mod context;
mod llvm_extras;


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