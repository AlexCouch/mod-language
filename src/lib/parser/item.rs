//! The Item Parser function and its dependencies

use crate::{
  source::{ SourceRegion, },
  common::{ Keyword::*, Operator::*, ITEM_KEYWORDS, },
  token::{ Token, TokenData, },
  ast::{ Item, ItemData, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, type_expression, expression, block, sync, };


/// Parse a single Item
pub fn item (parser: &mut Parser) -> Option<Item> {
  if let Some(parselet_function) = ItemParselet::get_function(parser.curr_tok()?) {
    parselet_function(parser)
  } else {
    parser.error("No syntactic match for this token in the context of a top level item".to_owned());

    None
  }
}


fn itm_module (parser: &mut Parser) -> Option<Item> {
  if let Some(&Token { data: TokenData::Keyword(Module), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), .. }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();

      // TODO: multiple source files
      if let Some(&Token { data: TokenData::Operator(Semi), origin: end_region }) = parser.curr_tok() {
        parser.error_at(SourceRegion::merge(start_region, end_region), format!("Cannot import source module file `{}`, multiple source files NYI", identifier.as_ref()));
        return None
      } else if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
        let mut items = Vec::new();

        let mut itm_ok = true;

        loop {
          match parser.curr_tok() {
            // The end of the stream
            None => {
              parser.error("Unexpected end of input while parsing module".to_owned());
            },

            // The end of the block
            Some(&Token { data: TokenData::Operator(RightBracket), origin: end_region }) => {
              return Some(Item::new(ItemData::Module { identifier, items }, SourceRegion::merge(start_region, end_region)));
            },

            // Items
            _ => {
              if itm_ok {
                if let Some(item) = item(parser) {
                  if item.requires_semi() {
                    if let Some(&Token { data: TokenData::Operator(Semi), .. }) = parser.curr_tok() {
                      parser.advance();
                      itm_ok = true;
                    } else {
                      itm_ok = false;
                    }
                  }
                  
                  items.push(item);

                  continue
                } else {
                  parser.error("Expected an item or end of input".to_owned());
                }
              } else {
                parser.error("Expected a ; to separate items or end of input".to_owned());
              }

              // If we reach here there was some kind of error, either we didnt have a semi after the last item, or our item call had an error,
              // so we need to try and synchronize to the end of the block or the next semi or keyword
              
              if parser.synchronize(sync::close_pair_or(sync::operator(LeftBracket), sync::operator(RightBracket), sync::or(sync::operator(Semi), sync::any_keyword_of(ITEM_KEYWORDS)))) {
                match parser.curr_tok().unwrap() {
                  Token { data: TokenData::Operator(Semi), .. } => {
                    parser.advance();
                    itm_ok = true;
                  },
                  Token { data: TokenData::Keyword(_), .. } => {
                    itm_ok = true;
                  },
                  _ => unreachable!("Internal error, unexpected parser state post synchronization")
                }
              } else {
                // Cannot recover state locally
                return None
              }
            }
          }
        }
      }
    }
  }

  None
}


fn itm_global (parser: &mut Parser) -> Option<Item> {
  // Synchronization should be handled by higher level parselet

  if let Some(&Token { data: TokenData::Keyword(Global), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), origin: mut end_region }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();
      
      let explicit_type = if let Some(&Token { data: TokenData::Operator(Colon), .. }) = parser.curr_tok() {
        parser.advance();

        let texpr = type_expression(parser)?;

        end_region = texpr.origin;

        texpr
      } else {
        parser.error("Expected : and a type expression to follow global identifier".to_owned());
        return None
      };

      let initializer = if let Some(&Token { data: TokenData::Operator(Assign), .. }) = parser.curr_tok() {
        parser.advance();

        let expr = expression(parser)?;

        end_region = expr.origin;

        Some(expr)
      } else {
        None
      };

      return Some(Item::new(
        ItemData::Global { identifier, explicit_type, initializer },
        SourceRegion::merge(start_region, end_region)
      ))
    } else {
      parser.error("Expected identifier for variable to follow let keyword".to_owned());
    }
  }
  unreachable!("Internal error, global item parselet called on non-global token");
}


fn itm_function (parser: &mut Parser) -> Option<Item> {
  // Synchronization should be handled by higher level parselet

  if let Some(&Token { data: TokenData::Keyword(Function), origin: start_region }) = parser.curr_tok() {
    let mut end_region = start_region;

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
                if let Some(&Token { data: TokenData::Operator(op), origin: params_end }) = parser.curr_tok() {
                  if op == Comma {
                    parser.advance();
                    
                    parameters.push((parameter_name, parameter_type));

                    continue
                  } else if op == RightParen {
                    parser.advance();

                    parameters.push((parameter_name, parameter_type));

                    end_region = params_end;

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

        let body = if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
          if let Some(blk) = block(parser) {
            end_region = blk.origin;
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
          SourceRegion::merge(start_region, end_region)
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
      |token| token.is_keyword(Module) => itm_module,
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