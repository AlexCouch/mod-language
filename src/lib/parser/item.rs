//! The Item Parser function and its dependencies

use crate::{
  source::{ SourceRegion, },
  common::{ Keyword::*, Operator::*, },
  token::{ Token, TokenData, },
  ast::{ Item, ItemData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, type_expression, expression, block, sync, };


/// Parse a single Item
pub fn item (parser: &mut Parser) -> Option<Item> {
  if let Some(parselet_function) = ItemParselet::get_function(parser.curr_tok()?) {
    parselet_function(parser)
  } else {
    parser.error("No semantic match for this token in the context of a top level item".to_owned());

    None
  }
}


fn itm_global (parser: &mut Parser) -> Option<Item> {
  // Synchronization should be handled by higher level parselet

  if let Some(&Token { data: TokenData::Keyword(Global), origin: SourceRegion { start, .. } }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), origin: SourceRegion { mut end, .. } }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();
      
      let explicit_type = if let Some(&Token { data: TokenData::Operator(Colon), .. }) = parser.curr_tok() {
        parser.advance();

        let texpr = type_expression(parser)?;

        end = texpr.origin.end;

        texpr
      } else {
        parser.error("Expected : and a type expression to follow global identifier".to_owned());
        return None
      };

      let initializer = if let Some(&Token { data: TokenData::Operator(Assign), .. }) = parser.curr_tok() {
        parser.advance();

        let expr = expression(parser)?;

        end = expr.origin.end;

        Some(expr)
      } else {
        None
      };

      return Some(Item::new(
        ItemData::Global { identifier, explicit_type, initializer },
        SourceRegion { start, end }
      ))
    } else {
      parser.error("Expected identifier for variable to follow let keyword".to_owned());
    }
  }
  unreachable!("Internal error, global item parselet called on non-global token");
}

fn itm_function (parser: &mut Parser) -> Option<Item> {
  // Synchronization should be handled by higher level parselet

  if let Some(&Token { data: TokenData::Keyword(Function), origin: SourceRegion { start, mut end } }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), .. }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();

      if let Some(&Token { data: TokenData::Operator(LeftParen), .. }) = parser.curr_tok() {
        parser.advance();

        let mut parameters = Vec::new();

        loop {
          if let Some(&Token { data: TokenData::Identifier(ref param_ident), .. }) = parser.curr_tok() {
            let parameter_name = param_ident.clone();

            parser.advance();

            if let Some(&Token { data: TokenData::Operator(Colon), .. }) = parser.curr_tok() {
              parser.advance();

              if let Some(parameter_type) = type_expression(parser) {
                if let Some(&Token { data: TokenData::Operator(op), origin: SourceRegion { end: params_end, .. } }) = parser.curr_tok() {
                  if op == Comma {
                    parser.advance();
                    
                    parameters.push((parameter_name, parameter_type));

                    continue
                  } else if op == RightParen {
                    parser.advance();

                    parameters.push((parameter_name, parameter_type));

                    end = params_end;

                    break;
                  }
                }

                parser.error("Expected , to separate parameters or ) to end parameter list".to_owned());
              } // else { Error has already been issued by type_expression, fall through to synchronization }
            } else {
              parser.error("Expected : and a type expression to follow parameter name".to_owned());
            }
          }

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

        let return_type = if let Some(&Token { data: TokenData::Operator(RightArrow), .. }) = parser.curr_tok() {
          parser.advance();

          if let Some(texpr) = type_expression(parser) {
            end = texpr.origin.end;
            Some(texpr)
          } else {
            // type_expression should have already provided an error message
            // Synchronization should be handled by higher level parselet
            return None
          }
        } else {
          None
        };

        let body = if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
          if let Some(blk) = block(parser) {
            end = blk.origin.end;
            Some(blk)
          } else {
            // block should have already provided an error message
            // Synchronization should be handled by higher level parselet
            return None
          }
        } else {
          None
        };

        return Some(Item::new(
          ItemData::Function { identifier, parameters, return_type, body },
          SourceRegion { start, end }
        ))
      } else {
        parser.error("Expected parameter list to follow identifier in function declaration".to_owned());
      }
    } else {
      parser.error("Expected identifier for function to follow fn keyword".to_owned());
    }

    return None;
  }

  unreachable!("Internal error, function parselet called on non-fn token");
}


struct ItemParselet {
  predicate: ParseletPredicate,
  function: ParseletFunction<Item>,
}

impl ItemParselet {
  const PARSELETS: &'static [Self] = {
    macro_rules! itm { ($( $predicate: expr => $function: expr ),* $(,)?) => { &[ $( ItemParselet { predicate: $predicate, function: $function } ),* ] } }

    itm! [
      |token| token.is_keyword(Global) => itm_global,
      |token| token.is_keyword(Function) => itm_function,
    ]
  };

  fn get_function (token: &Token) -> Option<ParseletFunction<Item>> {
    for parselet in Self::PARSELETS.iter() {
      if (parselet.predicate)(token) {
        return Some(parselet.function)
      }
    }
  
    None
  }
}