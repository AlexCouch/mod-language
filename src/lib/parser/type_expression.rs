//! The TypeExpression Parser function and its dependencies

use crate::{
  token::{ Token, TokenKind, TokenData, },
  ast::{ TypeExpression, TypeExpressionData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, };



// Public API //


/// Parse a single TypeExpression
pub fn type_expression (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(parselet_function) = TypeParselet::get_function(parser.curr_tok()?) {
    parselet_function(parser)
  } else {
    parser.error("No semantic match for this token in the context of a type expression".to_owned());

    None
  }
}



// Parselets //


fn identifier (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(&Token { data: TokenData::Identifier(ref ident), origin }) = parser.curr_tok() {
    let result = Some(TypeExpression::new(TypeExpressionData::Identifier(ident.clone()), origin));
    parser.advance();
    return result
  }

  unreachable!("Internal error, type expression identifier parselet called on non-identifier token");
}


struct TypeParselet {
  predicate: ParseletPredicate,
  parser: ParseletFunction<TypeExpression>,
}

impl TypeParselet {
  const PARSELETS: &'static [Self] = {
    macro_rules! tpx { ($( $predicate: expr => $parser: expr ),* $(,)?) => { &[ $( TypeParselet { predicate: $predicate, parser: $parser } ),* ] } }

    tpx! [
      |token| token.kind() == TokenKind::Identifier => identifier,
    ]
  };

  fn get_function (token: &Token) -> Option<ParseletFunction<TypeExpression>> {
    for parselet in Self::PARSELETS.iter() {
      if (parselet.predicate)(token) {
        return Some(parselet.parser)
      }
    }
  
    None
  }
}