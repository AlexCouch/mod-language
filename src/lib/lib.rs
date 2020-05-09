//! WIP wasm scripting language

#![warn(missing_docs)]
#![warn(clippy::all)]

#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(track_caller)]
#![feature(stmt_expr_attributes)]
#![feature(or_patterns)]

extern crate self as mod_language;

pub extern crate mod_utilities;
pub use mod_utilities as util;
pub use util::collections as collections;

mod extras;
pub mod ansi;
pub mod source;
pub mod session;
pub mod common;
pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod ctx;
pub mod ir;
pub mod analyzer;