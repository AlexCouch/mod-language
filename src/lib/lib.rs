//! WIP wasm scripting language

#![warn(missing_docs)]

#![feature(box_syntax)]
#![feature(track_caller)]

extern crate self as mod_language;

pub mod util;
pub mod ansi;
pub mod source;
pub mod common;
pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;