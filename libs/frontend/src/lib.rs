//! The front end lexer, parser, and analyzer for mod_language

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::useless_let_if_seq)]

#![feature(or_patterns)]
#![feature(track_caller)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(option_expect_none)]
#![feature(option_unwrap_none)]
#![feature(bindings_after_at)]

pub mod source;
pub mod session;
pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod ctx;
pub mod ir;
pub mod analyzer;