//! WIP wasm scripting language

#![warn(missing_docs)]
#![warn(clippy::all)]

#![feature(try_trait)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(track_caller)]
#![feature(stmt_expr_attributes)]
#![feature(or_patterns)]
#![feature(bindings_after_at)]
#![feature(option_expect_none)]
#![feature(const_fn)]
#![feature(const_loop)]
#![feature(const_if_match)]

extern crate self as mod_language;

pub extern crate mod_utilities;
pub use mod_utilities as util;
pub use util::collections as collections;

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