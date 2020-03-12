//! The TypeExpression Parser function and its dependencies

use crate::{
  token::{ Token, TokenKind, TokenData, },
  ast::{ TypeExpression, TypeExpressionData, },
};

use super::Parser;



// Public API //


/// Parse a single TypeExpression
pub fn type_expression (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(parselet) = TypeParselet::get(parser.curr_tok()?) {
    parselet.parse(parser)
  } else {
    parser.error("No semantic match for this token in the context of a type expression".to_owned());

    None
  }
}



// Parselets //


fn identifier (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(Token { data: TokenData::Identifier(ident), origin }) = parser.curr_tok() {
    let result = Some(TypeExpression::new(TypeExpressionData::Identifier(*ident), *origin));
    parser.advance();
    return result
  }

  unreachable!("Internal error, type expression identifier parselet called on non-identifier token");
}


struct TypeParselet {
  predicate: fn (&Token) -> bool,
  parser: fn (&mut Parser) -> Option<TypeExpression>,
}

impl TypeParselet {
  #[inline]
  fn predicate (&self, token: &Token) -> bool {
    (self.predicate)(token)
  }
  
  #[inline]
  fn parse (&self, parser: &mut Parser) -> Option<TypeExpression> {
    (self.parser)(parser)
  }
  

  const PARSELETS: &'static [Self] = {
    macro_rules! tpx { ($( $predicate: expr => $parser: expr ),* $(,)?) => { &[ $( TypeParselet { predicate: $predicate, parser: $parser } ),* ] } }

    tpx! [
      |token| token.kind() == TokenKind::Identifier => identifier,
    ]
  };

  fn get (token: &Token) -> Option<&'static TypeParselet> {
    for parselet in Self::PARSELETS.iter() {
      if parselet.predicate(token) {
        return Some(parselet)
      }
    }
  
    None
  }
}