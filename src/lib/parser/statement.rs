//! The Statement Parser function and its dependencies

use crate::ast::{ Statement, };

use super::Parser;


/// Parse a single Statement
pub fn statement (parser: &mut Parser) -> Option<Statement> {
  unimplemented!("{:?}", parser.curr_tok())
}