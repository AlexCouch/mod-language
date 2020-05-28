//! The runtime bytecode compiler for mod_language

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::match_ref_pats)]

#![feature(option_unwrap_none)]
#![feature(or_patterns)]
#![feature(track_caller)]

#[cfg(not(target_pointer_width = "64"))]
compile_error!("This crate can only be used on 64-bit targets");


pub mod utils;
pub(crate) mod llvm_extras;
pub mod context;
pub mod module_loader;
pub(crate) mod generator;


pub use context::Context;
pub use module_loader::{ CompilationResult, CompilationError, CompilationErrorData, };