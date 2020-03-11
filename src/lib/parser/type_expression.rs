//! The TypeExpression Parser function and its dependencies

use crate::{
  token::{ Token, TokenKind, TokenData },
  ast::{ TypeExpression, TypeExpressionData },
};

use super::Parser;



struct TypeParselet {
  predicate: fn (&Token) -> bool,
  parse: fn (&mut Parser) -> Option<TypeExpression>,
}

impl TypeParselet {
  #[inline]
  fn predicate (&self, token: &Token) -> bool {
    (self.predicate)(token)
  }

  #[inline]
  fn parse (&self, parser: &mut Parser) -> Option<TypeExpression> {
    (self.parse)(parser)
  }
}


/// Parse a single TypeExpression
pub fn type_expression (parser: &mut Parser) -> Option<TypeExpression> {
  let token = parser.curr_tok()?;

  for parselet in TYPE_PARSELETS.iter() {
    if parselet.predicate(token) {
      return parselet.parse(parser)
    }
  }

  parser.error("No semantic match for this token in the context of a type expression".to_owned());

  None
}


fn identifier (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(Token { data: TokenData::Identifier(ident), origin }) = parser.curr_tok() {
    let result = Some(TypeExpression::new(TypeExpressionData::Identifier(*ident), *origin));
    parser.advance();
    result
  } else {
    None
  }
}


const TYPE_PARSELETS: &[TypeParselet] = &[
  TypeParselet { predicate: |token| token.kind() == TokenKind::Identifier, parse: identifier },
];