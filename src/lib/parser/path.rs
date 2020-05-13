//! The Path Parser function and its dependencies

use crate::{
  util::{ Either, IntoEither, },
  common::{ Identifier, Operator::*, },
  source::{ SourceRegion, },
  token::{ Token, TokenData, },
  ast::{ Path, },
};

use super::{ Parser, };


/// Parse either a single Path or Identifier
pub fn path (parser: &mut Parser) -> Option<Either<Path, (Identifier, SourceRegion)>> {
  if let Some(token) = parser.curr_tok() {
    let start_region = token.origin;

    let absolute = if let TokenData::Operator(DoubleColon) = token.data {
      parser.advance();
      if !matches!(parser.curr_tok(), Some(Token { data: TokenData::Identifier(_), .. })) {
        return Some(Path::new(true, vec![], start_region).into_a());
      }
      true
    } else {
      if !matches!(token.data, TokenData::Identifier(_)) {
        parser.error("Expected :: or identifier".to_owned());
        return None
      }

      false
    };

    let mut chain = Vec::new();
    let mut end_region;

    loop {
      if let Some(&Token { data: TokenData::Identifier(ref ident), origin }) = parser.curr_tok() {
        chain.push(ident.clone());
        end_region = origin;

        parser.advance();

        if let Some(&Token { data: TokenData::Operator(DoubleColon), .. }) = parser.curr_tok() {
          parser.advance();
        } else {
          let origin = SourceRegion::merge(start_region, end_region);

          break Some(if absolute || chain.len() > 1 {
            Path::new(absolute, chain, origin).into_a()
          } else {
            (chain.pop().unwrap(), origin).into_b()
          })
        }
      } else {
        parser.error("Expected identifier to follow :: in path".to_owned());
        break None
      }
    }
  } else {
    parser.error("Expected :: or identifier".to_owned());

    None
  }
}