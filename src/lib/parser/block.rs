//! The Block Parser function and its dependencies

use crate::ast::{ Block, };

use super::Parser;


/// Parse a single Block
pub fn block (parser: &mut Parser) -> Option<Block> {
  unimplemented!("{:?}", parser.curr_tok())
}