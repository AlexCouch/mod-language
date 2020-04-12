//! WIP wasm scripting language

#![warn(missing_docs)]
#![warn(clippy::all)]

#![feature(box_syntax)]
#![feature(track_caller)]

extern crate self as mod_language;

pub extern crate mod_utilities;
pub use mod_utilities as util;
pub use util::collections as collections;

mod macros;
pub mod ansi;
pub mod source;
pub mod common;
pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod analyzer;