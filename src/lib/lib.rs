//! WIP wasm scripting language

#![warn(missing_docs)]

extern crate self as mod_language;

pub mod util;
pub mod ansi;
pub mod source;
pub mod token;
pub mod lexer;