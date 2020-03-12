//! The Statement Parser function and its dependencies

use crate::{
  source::{ SourceRegion, },
  token::{ Token, TokenData, TokenKind, Operator, Operator::*, },
  ast::{ Expression, ExpressionData, },
};

use super::{ Parser, sync };



// Public API //


/// Check for and utilize infix operators in order to complete an Expression
pub fn complete_partial_expression (precedence: usize, mut left: Expression, parser: &mut Parser) -> Option<Expression> {
  while let Some(token) = parser.curr_tok() {
    if let Some(parselet) = InfixParselet::get(token) {
      if precedence >= parselet.precedence { break }

      left = parselet.parse(left, parser)?;
    } else {
      break
    }
  }

  Some(left)
}

/// Parse a single Expression starting with a given pratt precedence
/// 
/// `expression` calls this with precedence 0 to build the longest expression available
pub fn pratt (precedence: usize, parser: &mut Parser) -> Option<Expression> {
  if let Some(parselet) = PrefixParselet::get(parser.curr_tok()?) {
    complete_partial_expression(precedence, parselet.parse(parser)?, parser)
  } else {
    parser.error("No semantic match for this token in the context of an expression".to_owned());

    None
  }
}

/// Parse a single Expression
#[inline]
pub fn expression (parser: &mut Parser) -> Option<Expression> {
  pratt(0, parser)
}



// Prefix parselets //


fn pfx_identifier (parser: &mut Parser) -> Option<Expression> {
  if let Some(Token { data: TokenData::Identifier(ident), origin }) = parser.curr_tok() {
    let result = Some(Expression::new(ExpressionData::Identifier(*ident), *origin));
    parser.advance();
    return result
  }

  unreachable!("Internal error, identifier expression parselet called on non-identifier token");
}

fn pfx_number (parser: &mut Parser) -> Option<Expression> {
  if let Some(Token { data: TokenData::Number(num), origin }) = parser.curr_tok() {
    let result = Some(Expression::new(ExpressionData::Number(*num), *origin));
    parser.advance();
    return result
  }

  unreachable!("Internal error, number expression parselet called on non-number token");
}

fn pfx_semantic_group (parser: &mut Parser) -> Option<Expression> {
  if let Some(&Token { data: TokenData::Operator(LeftParenthesis), origin: SourceRegion { start, .. } }) = parser.curr_tok() {
    parser.advance();
    
    if let Some(expr) = expression(parser) {
      if let Some(&Token { data: TokenData::Operator(RightParenthesis), origin: SourceRegion { end, .. } }) = parser.curr_tok() {
        parser.advance();

        return Some(Expression::new(expr.data, SourceRegion { start, end }))
      } else {
        parser.error("Expected ) to close semantic group".to_owned());
      }
    } // else { Do not need to give an error message here as one should have already been issued inside the `expression` call, but we do need to sync }

    // If we reach this point there was some sort of error and we need to try to synchronize to the end of the (sem group),
    // but there isnt much we can do beyond this since our inner value is probably invalid and we may as well discard it
    parser.synchronize_unchecked(sync::close_pair(sync::operator(LeftParenthesis), sync::operator(RightParenthesis)));

    return None
  }

  unreachable!("Internal error, semantic group expression parselet called on non-parenthesis token");
}


struct PrefixParselet {
  predicate: fn (&Token) -> bool,
  parser: fn (&mut Parser) -> Option<Expression>,
}

impl PrefixParselet {
  #[inline]
  fn predicate (&self, token: &Token) -> bool {
    (self.predicate)(token)
  }

  #[inline]
  fn parse (&self, parser: &mut Parser) -> Option<Expression> {
    (self.parser)(parser)
  }


  const PARSELETS: &'static [Self] = {
    macro_rules! pfx { ($( $predicate: expr => $parser: expr ),* $(,)?) => { &[ $( PrefixParselet { predicate: $predicate, parser: $parser } ),* ] } }

    pfx! [
      |token| token.kind() == TokenKind::Identifier => pfx_identifier,
      |token| token.kind() == TokenKind::Number => pfx_number,
      |token| token.is_operator(LeftParenthesis) => pfx_semantic_group,
    ]
  };

  fn get (token: &Token) -> Option<&'static Self> {
    for parselet in Self::PARSELETS.iter() {
      if parselet.predicate(token) {
        return Some(parselet)
      }
    }

    None
  }
}



// Infix parselets //


fn ifx_binary_operator (left: Expression, parser: &mut Parser) -> Option<Expression> {
  if let Some(&Token { data: TokenData::Operator(operator), .. }) = parser.curr_tok() {
    parser.advance();

    if let Some(right) = pratt(InfixParselet::get_binary_precedence(operator), parser) {
      let origin = SourceRegion {
        start: left.origin.start,
        end: right.origin.end
      };

      return Some(Expression::new(
        ExpressionData::Binary {
          left: box left,
          right: box right,
          operator
        },
        origin
      ))
    } // else { Do not need to give an error message here as one should have already been issued inside the `pratt` call }

    // If we reached this point something was wrong with the right hand operand expression, or we ran out of tokens,
    // but we don't have any contextual information to use in synchronization so it must be handled by the callee and/or caller

    return None
  }

  unreachable!("Internal error, binary operator expression parselet called on non-operator token");
}


fn ifx_call (left: Expression, parser: &mut Parser) -> Option<Expression> {
  if let Some(&Token { data: TokenData::Operator(LeftParenthesis), .. }) = parser.curr_tok() {
    parser.advance();

    let mut arguments = Vec::new();

    let mut expr_ok = true;

    loop {
      match parser.curr_tok() {
        // Unexpected end of input
        None => {
          parser.error_at(SourceRegion { start: left.origin.start, end: parser.curr_location() }, "Unexpected end of input, expected ) to close call expression".to_owned());
          return None
        },

        // The end of the argument list
        Some(&Token { data: TokenData::Operator(RightParenthesis), origin: SourceRegion { end, .. } }) => {
          parser.advance();
          
          let origin = SourceRegion { start: left.origin.start, end };

          return Some(Expression::new(ExpressionData::Call { callee: box left, arguments }, origin))
        },

        // Argument expressions
        _ => {
          if expr_ok {
            if let Some(argument) = expression(parser) {
              arguments.push(argument);

              if let Some(&Token { data: TokenData::Operator(Comma), .. }) = parser.curr_tok() {
                parser.advance();
                expr_ok = true;
              } else {
                expr_ok = false;
              }

              continue
            } else {
              parser.error("Expected an argument expression or ) to end call expression".to_owned());
            }
          } else {
            parser.error("Expected a comma to separate arguments or ) to end call expression".to_owned());
          }

          // If we reach here there was some kind of error, either we didnt have a comma after the last expression, or our expression call had an error,
          // so we need to try and synchronize to the end of the call(expression) or the next comma
          
          if parser.synchronize(sync::close_pair_or(sync::operator(LeftParenthesis), sync::operator(RightParenthesis), sync::operator(Comma))) {
            if let Some(&Token { data: TokenData::Operator(Comma), .. }) = parser.curr_tok() {
              parser.advance();
              expr_ok = true;
            } // else { The next iteration will handle the parenthesis, so no need to do anything here }
          } else {
            // Cannot recover state locally
            return None
          }
        }
      }
    }
  }

  unreachable!("Internal error, call expression parselet called on non-parenthesis token");
}


struct InfixParselet {
  precedence: usize,
  predicate: fn (&Token) -> bool,
  parser: fn (Expression, &mut Parser) -> Option<Expression>,
}

impl InfixParselet {
  #[inline]
  fn predicate (&self, token: &Token) -> bool {
    (self.predicate)(token)
  }

  #[inline]
  fn parse (&self, expression: Expression, parser: &mut Parser) -> Option<Expression> {
    (self.parser)(expression, parser)
  }


  const BINARY_PRECEDENCES: &'static [(Operator, usize)] = {
    &[
      (Plus, 50),
      (Minus, 50),
      
      (Asterisk, 60),
      (ForwardSlash, 60),
    ]
  };

  fn get_binary_precedence (operator: Operator) -> usize {
    let mut iter = Self::BINARY_PRECEDENCES.iter();
    
    loop {
      let (precedent_op, precedence) = iter.next().unwrap();
      
      if *precedent_op == operator {
        break *precedence
      }
    }
  }
  

  const PARSELETS: &'static [Self] = {
    macro_rules! ifx { ($( [$precedence: expr] $predicate: expr => $parser: expr ),* $(,)?) => { &[ $( InfixParselet { precedence: $precedence, predicate: $predicate, parser: $parser } ),* ] } }

    ifx! [
      [10] |token| token.is_operator(LeftParenthesis) => ifx_call,
      [50] |token| token.is_any_operator_of(&[ Plus, Minus ]).is_some() => ifx_binary_operator,
      [60] |token| token.is_any_operator_of(&[ Asterisk, ForwardSlash ]).is_some() => ifx_binary_operator,
    ]
  };

  fn get (token: &Token) -> Option<&'static InfixParselet> {
    for parselet in Self::PARSELETS.iter() {
      if parselet.predicate(token) {
        return Some(parselet)
      }
    }
  
    None
  }
}