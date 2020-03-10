//! The TypeExpression Parser function and its dependencies

use crate::ast::{ TypeExpression };

use super::Parser;


/// Parse a single TypeExpression
pub fn type_expression (parser: &mut Parser) -> Option<TypeExpression> {
  unimplemented!("{:?}", parser.curr_tok())
}