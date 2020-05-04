//! The TypeExpression Parser function and its dependencies

use crate::{
  util::{ Either, },
  common::{ Operator::*, },
  token::{ Token, TokenData, },
  ast::{ TypeExpression, TypeExpressionData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, path, };



// Public API //


/// Parse a single TypeExpression
pub fn type_expression (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(parselet_function) = TypeExpressionParselet::get_function(parser.curr_tok()?) {
    parselet_function(parser)
  } else {
    parser.error("No syntactic match for this token in the context of a type expression".to_owned());

    None
  }
}



// Parselets //


fn tpx_path_or_ident (parser: &mut Parser) -> Option<TypeExpression> {
  let (path_or_ident, origin) = path(parser)?;

  Some(TypeExpression::new(
    match path_or_ident {
      Either::A(path)  => TypeExpressionData::Path(path),
      Either::B(ident) => TypeExpressionData::Identifier(ident),
    },
    origin
  ))
}


struct TypeExpressionParselet {
  predicate: ParseletPredicate,
  function: ParseletFunction<TypeExpression>,
}

impl TypeExpressionParselet {
  const PARSELETS: &'static [Self] = {
    macro_rules! tpx { ($( $predicate: expr => $function: expr ),* $(,)?) => { &[ $( TypeExpressionParselet { predicate: $predicate, function: $function } ),* ] } }

    tpx! [
      |token| matches!(token.data, TokenData::Identifier(_) | TokenData::Operator(DoubleColon)) => tpx_path_or_ident,
    ]
  };

  fn get_function (token: &Token) -> Option<ParseletFunction<TypeExpression>> {
    for parselet in Self::PARSELETS.iter() {
      if (parselet.predicate)(token) {
        return Some(parselet.function)
      }
    }
  
    None
  }
}