//! The Block Parser function and its dependencies

use mod_common::{ Operator::*, Keyword::*, STATEMENT_KEYWORDS, };

use crate::{
  source::{ SourceRegion, },
  token::{ Token, TokenData, },
  ast::{ Block, StatementData,  ConditionalBranch, Conditional, },
};

use super::{ Parser, statement, expression, sync };


/// Parse a single Block
pub fn block (parser: &mut Parser) -> Option<Block> {
  if let Some(&Token { data: TokenData::Operator(LeftBracket), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    let mut statements = Vec::new();

    let mut trailing_expression = None;
    let mut stmt_ok = true;

    loop {
      match parser.curr_tok() {
        // Unexpected end of input
        None => {
          parser.error_at(SourceRegion::merge(start_region, parser.curr_region()), "Unexpected end of input, expected } to close block".to_owned());
          return None
        },

        // The end of the block
        Some(&Token { data: TokenData::Operator(RightBracket), origin: end_region }) => {
          parser.advance();

          return Some(Block::new(statements, trailing_expression, SourceRegion::merge(start_region, end_region)))
        },

        // Statements/Expressions
        _ => {
          if stmt_ok {
            if let Some(stmt) = statement(parser) {
              if stmt.requires_semi() {
                if let Some(&Token { data: TokenData::Operator(Semi), .. }) = parser.curr_tok() {
                  parser.advance();
                  statements.push(stmt);
                } else if let StatementData::Expression(expr) = stmt.data {
                  trailing_expression = Some(expr);
                  stmt_ok = false;
                } else {
                  statements.push(stmt);
                  stmt_ok = false;
                }
              } else {
                statements.push(stmt);
              }

              continue
            } // else { Error message already provided by statement }
          } else {
            parser.error("Expected a ; to separate statements or } to end block".to_owned());
          }

          // If we reach here there was some kind of error, either we didnt have a semi after the last statement, or our statement call had an error,
          // so we need to try and synchronize to the end of the {block} or the next semi or keyword
          
          if parser.synchronize(sync::close_pair_or(sync::operator(LeftBracket), sync::operator(RightBracket), sync::or(sync::operator(Semi), sync::any_keyword_of(STATEMENT_KEYWORDS)))) {
            match parser.curr_tok().unwrap() {
              Token { data: TokenData::Operator(Semi), .. } => {
                parser.advance();
                trailing_expression = None;
                stmt_ok = true;
              },
              Token { data: TokenData::Keyword(_), .. } => {
                trailing_expression = None;
                stmt_ok = true;
              },
              Token { data: TokenData::Operator(RightBracket), .. } => continue, // The next iteration will handle the closing bracket
              _ => unreachable!("Internal error, unexpected block parselet state post-synchronization")
            }
          } else {
            // Cannot recover state locally
            return None
          }
        }
      }
    }
  }

  unreachable!("Internal error, block parselet called on non-bracket token");
}


/// Parse a single ConditionalBranch
pub fn conditional_branch (parser: &mut Parser) -> Option<ConditionalBranch> {
  if let Some(&Token { data: TokenData::Keyword(If), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    // Synchronization must be handled by higher level parselet
    // If we try to synchronize on the next { we could really go off in the weeds
    let condition = expression(parser)?;

    if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
      let body = block(parser)?;

      let origin = SourceRegion::merge(start_region, body.origin);

      return Some(ConditionalBranch::new(condition, body, origin))
    } else {
      parser.error("Expected body block for condtional branch".to_owned());
      return None
    }
  }

  unreachable!("Internal error, conditional branch parselet called on non-if token");
}


/// Parse a Conditional chain
pub fn conditional (parser: &mut Parser) -> Option<Conditional> {
  // Synchronization must be handled by higher level parselet

  let if_branch = conditional_branch(parser)?;

  let start_region = if_branch.origin;
  let mut end_region = if_branch.origin;

  let mut else_if_branches = Vec::new();
  let mut else_block = None;

  while let Some(&Token { data: TokenData::Keyword(Else), .. }) = parser.curr_tok() {
    parser.advance();

    match parser.curr_tok() {
      // Unexpected end of input
      None => {
        parser.error_at(SourceRegion::merge(start_region, parser.curr_region()), "Unexpected end of input, expected if or { to continue else conditional branch".to_owned());

        return None
      },

      // else if branch
      Some(&Token { data: TokenData::Keyword(If), .. }) => {
        let branch = conditional_branch(parser)?;

        if if_branch.is_expression() != branch.is_expression() {
          parser.error_at(branch.origin, format!(
            "This branch's block {} a trailing expression, while the originating if branch's block {}",
            if branch.is_expression() { "has" } else { "does not have" },
            if if_branch.is_expression() { "does" } else { "does not" }
          ));
        }

        end_region = branch.origin;

        else_if_branches.push(branch);
      },

      // else block
      Some(&Token { data: TokenData::Operator(LeftBracket), .. }) => {
        let block = block(parser)?;

        end_region = block.origin;

        if if_branch.is_expression() != block.is_expression() {
          parser.error_at(block.origin, format!(
            "This else block {} a trailing expression, while the originating if branch's block {}",
            if block.is_expression() { "has" } else { "does not have" },
            if if_branch.is_expression() { "does" } else { "does not" }
          ));
        }

        else_block = Some(block);

        break
      },

      // Unexpected token
      _ => {
        parser.error("Expected if or { to continue else conditional branch".to_owned());
        return None
      }
    }
  }

  Some(Conditional::new(if_branch, else_if_branches, else_block, SourceRegion::merge(start_region, end_region)))
}