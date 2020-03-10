//! The Item Parser function and its dependencies

use crate::ast::{ Item };

use super::Parser;


/// Parse a single Item
pub fn item (parser: &mut Parser) -> Option<Item> {
  unimplemented!("{:?}", parser.curr_tok())
}