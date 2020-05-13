//! The TypeExpression Parser function and its dependencies

use crate::{
  util::{ Either, },
  common::{ Operator::*, Keyword::*, },
  source::{ SourceRegion, },
  token::{ Token, TokenData, },
  ast::{ TypeExpression, TypeExpressionData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, path, sync, };



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

fn tpx_pointer (parser: &mut Parser) -> Option<TypeExpression> {
  if let Some(&Token { data: TokenData::Operator(AddressOf), origin }) = parser.curr_tok() {
    parser.advance();

    let value_texpr = type_expression(parser)?;

    let origin = SourceRegion::merge(origin, value_texpr.origin);

    return Some(TypeExpression::new(TypeExpressionData::Pointer(box value_texpr), origin))
  }

  unreachable!("Internal error, pointer type expression parselet called on non-address-of token");
}

fn tpx_function (parser: &mut Parser) -> Option<TypeExpression> {
  let (start_region, mut end_region) = if let Some(&Token { data: TokenData::Keyword(Function), origin }) = parser.curr_tok() {
    parser.advance();
    (origin, origin)
  } else {
    unreachable!("Internal error, function parselet called on non-fn token");
  };


  let mut parameter_types = Vec::new();

  if let Some(&Token { data: TokenData::Operator(LeftParen), .. }) = parser.curr_tok() {
    parser.advance();

    loop {
      if let Some(parameter_type) = type_expression(parser) {
        if let Some(&Token { data: TokenData::Operator(op), origin: param_end }) = parser.curr_tok() {
          parameter_types.push(parameter_type);
          
          if op == Comma {
            parser.advance();
            
            continue
          } else if op == RightParen {
            parser.advance();

            end_region = param_end;

            break;
          }
        }

        parser.error("Expected , to separate parameters or ) to end parameter list".to_owned());
      } // else { Error has already been issued by type_expression, fall through to synchronization }
          
      if parser.synchronize(sync::close_pair_or(sync::operator(LeftParen), sync::operator(RightParen), sync::operator(Comma))) {
        if let Some(&Token { data: TokenData::Operator(op), .. }) = parser.curr_tok() {
          if op == Comma {
            parser.advance();
            continue;
          } else {
            parser.advance();
            break;
          }
        }
      }

      // Could not recover
      return None
    }
  }

  let return_type = box if let Some(&Token { data: TokenData::Operator(RightArrow), .. }) = parser.curr_tok() {
    parser.advance();

    if let Some(texpr) = type_expression(parser) {
      end_region = texpr.origin;
      Some(texpr)
    } else {
      // type_expression should have already provided an error message
      // Synchronization should be handled by higher level parselet
      return None
    }
  } else {
    None
  };

  Some(TypeExpression::new(
    TypeExpressionData::Function { parameter_types, return_type },
    SourceRegion::merge(start_region, end_region)
  ))
}


struct TypeExpressionParselet {
  predicate: ParseletPredicate,
  function: ParseletFunction<TypeExpression>,
}

impl TypeExpressionParselet {
  const PARSELETS: &'static [Self] = {
    macro_rules! tpx { ($( $($predicate: pat)|* => $function: expr ),* $(,)?) => { &[ $( TypeExpressionParselet { predicate: |token | matches!(token.data, $($predicate)|*), function: $function } ),* ] } }

    use TokenData::*;
    
    tpx! [
      Identifier(_) | Operator(DoubleColon) => tpx_path_or_ident,
      Operator(AddressOf) => tpx_pointer,
      Keyword(Function) => tpx_function,
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