//! The Statement Parser function and its dependencies

use mod_common::{ Operator, Operator::*, Keyword::*, };

use crate::{
  source::{ SourceRegion, },
  token::{ Token, TokenData, },
  ast::{ Statement, StatementData, Expression, ExpressionData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, expression, block, type_expression, conditional, };



// Public API //


/// Parse a single Statement
pub fn statement (parser: &mut Parser) -> Option<Statement> {
  complete_assignment_statement(StatementParselet::get_function(parser.curr_tok()?)(parser)?, parser)
}


/// Tries to convert an Expression statement into an assignment Statement by checking for appropriate Tokens
pub fn complete_assignment_statement (target: Statement, parser: &mut Parser) -> Option<Statement> {
  const ASSIGN_OPERATORS: &[Operator] = &[
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignRem,
  ];

  if let Statement { data: StatementData::Expression(target), .. } = target {
    if let Some(operator) = parser.curr_tok().and_then(|token| token.is_any_operator_of(ASSIGN_OPERATORS)) {
      parser.advance();

      // Synchronization in the event of an error in expression will be taken care of by higher level parselet
      let value = expression(parser)?;

      let origin = SourceRegion::merge(target.origin, value.origin);
      
      Some(Statement::new(
        if operator == Assign {
          StatementData::Assignment { target, value }
        } else {
          StatementData::ModAssignment { target, value, operator }
        },
        origin
      ))
    } else {
      Some(Statement::from(target))
    }
  } else {
    Some(target)
  }
}



// Parselets //


fn stm_block (parser: &mut Parser) -> Option<Statement> {
  // Synchronization should be handled by higher level parselet

  let block = box block(parser)?;
  let origin = block.origin;

  Some(Statement::new(
    if block.is_expression() {
      StatementData::Expression(Expression::new(ExpressionData::Block(block), origin))
    } else {
      StatementData::Block(block)
    },
    origin
  ))
}


fn stm_declaration (parser: &mut Parser) -> Option<Statement> {
  // Synchronization should be handled by higher level parselet

  if let Some(&Token { data: TokenData::Keyword(Let), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), origin: mut end_region }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();
      
      let explicit_type = if let Some(&Token { data: TokenData::Operator(Colon), .. }) = parser.curr_tok() {
        parser.advance();

        let texpr = type_expression(parser)?;

        end_region = texpr.origin;

        Some(texpr)
      } else {
        None
      };

      let initializer = if let Some(&Token { data: TokenData::Operator(Assign), .. }) = parser.curr_tok() {
        parser.advance();

        let expr = expression(parser)?;

        end_region = expr.origin;

        Some(expr)
      } else {
        None
      };

      return Some(Statement::new(
        StatementData::Declaration { identifier, explicit_type, initializer },
        SourceRegion::merge(start_region, end_region)
      ))
    } else {
      parser.error("Expected identifier for variable to follow let keyword".to_owned());
    }
  }

  unreachable!("Internal error, declaration statement parselet called on non-let token");
}

fn stm_conditional (parser: &mut Parser) -> Option<Statement> {
  // Synchronization should be handled by higher level parselet
  let conditional = box conditional(parser)?;

  let origin = conditional.origin;

  Some(Statement::new(
    if conditional.is_expression() {
      StatementData::Expression(Expression::new(ExpressionData::Conditional(conditional), origin))
    } else {
      StatementData::Conditional(conditional)
    },
    origin
  ))
}

fn stm_return (parser: &mut Parser) -> Option<Statement> {
  if let Some(&Token { data: TokenData::Keyword(Return), origin }) = parser.curr_tok() {
    parser.advance();

    let (expr, origin) = if !matches!(parser.curr_tok(), Some(Token { data: TokenData::Operator(Semi | RightBracket), .. })) {
      let expr = expression(parser)?;
      let origin = SourceRegion::merge(origin, expr.origin);
      (Some(expr), origin)
    } else {
      (None, origin)
    };

    Some(Statement::new(StatementData::Return(expr), origin))
  } else {
    unreachable!("Internal error, return statement parser called on non-return token")
  }
}

fn stm_expression (parser: &mut Parser) -> Option<Statement> {
  // Synchronization should be handled by higher level parselet
  Some(expression(parser)?.into())
}


struct StatementParselet  {
  predicate: ParseletPredicate,
  function: ParseletFunction<Statement>,
}

impl StatementParselet {
  const PARSELETS: &'static [Self] = {
    macro_rules! stm { ($( $($predicate: pat)|* => $function: expr ),* $(,)?) => { &[ $( StatementParselet { predicate: |token| matches!(token.data, $($predicate)|*), function: $function } ),* ] } }

    use TokenData::*;
    stm! [
      Operator(LeftBracket) => stm_block,
      Keyword(Let) => stm_declaration,
      Keyword(If) => stm_conditional,
      Keyword(Return) => stm_return,
    ]
  };

  fn get_function (token: &Token) -> ParseletFunction<Statement> {
    for parselet in Self::PARSELETS.iter() {
      if (parselet.predicate)(token) {
        return parselet.function
      }
    }

    stm_expression
  }
}