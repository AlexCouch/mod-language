//! The Item Parser function and its dependencies

use crate::{
  util::{ Either, },
  source::{ SourceRegion, SOURCE_MANAGER, },
  common::{ Keyword::*, Operator::*, ITEM_KEYWORDS, Identifier, },
  token::{ Token, TokenData, },
  ast::{ Item, ItemData, Alias, Export, },
  lexer::{ Lexer, },
};

use super::{ Parser, ParseletPredicate, ParseletFunction, type_expression, expression, block, path, sync, };


/// Parse a single Item
pub fn item (parser: &mut Parser) -> Option<Item> {
  let curr_tok = parser.curr_tok()?;

  let parselet: Option<ParseletFunction<Item>> = match curr_tok.data {
    TokenData::Keyword(Import) => Some(itm_import),
    TokenData::Keyword(Export) => Some(itm_export),
    _ => ItemParselet::get_function(curr_tok)
  };

  if let Some(parselet_function) = parselet {
    parselet_function(parser)
  } else {
    parser.error("No syntactic match for this token in the context of a top level item".to_owned());

    None
  }
}


fn get_new_name_and_origin (parser: &mut Parser) -> Result<Option<(Identifier, SourceRegion)>, ()> {
  if let Some(Token { data: TokenData::Operator(As), .. }) = parser.curr_tok() {
    parser.advance();
    
    if let Some(&Token { data: TokenData::Identifier(ref new_name), origin }) = parser.curr_tok() {
      let new_name = new_name.to_owned();
      parser.advance();

      Ok(Some((new_name, origin)))
    } else {
      parser.error("Expected identifier to follow aliasing keyword `as`".to_owned());

      Err(())
    }
  } else {
    Ok(None)
  }
}

fn get_new_name (parser: &mut Parser) -> Result<Option<Identifier>, ()> {
  match get_new_name_and_origin(parser) {
    Ok(opt) => Ok(opt.map(|(new_name, _)| new_name)),
    Err(()) => Err(())
  }
}


fn itm_import (parser: &mut Parser) -> Option<Item> {
  if let Some(&Token { data: TokenData::Keyword(Import), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    let refs = if let Some(Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
      parser.advance();

      let mut refs = Vec::new();

      let mut ref_ok = true;

      loop {
        match parser.curr_tok() {
          // Unexpected end of input
          None => {
            parser.error_at(SourceRegion::merge(start_region, parser.curr_region()), "Unexpected end of input, expected } to close block".to_owned());
            return None
          },
  
          // The end of the block
          Some(&Token { data: TokenData::Operator(RightBracket), .. }) => {
            parser.advance();
            break
          },
  
          // Refs
          _ => {
            if ref_ok {
              if let Some(Token { data: TokenData::Identifier(base), .. }) = parser.curr_tok() {
                let base = base.clone();
                
                parser.advance();
                                
                if let Ok(new_name) = get_new_name(parser) {
                  refs.push(Alias { base, new_name });

                  if let Some(Token { data: TokenData::Operator(Comma), .. }) = parser.curr_tok() {
                    parser.advance();
                    ref_ok = true;
                  }

                  continue
                }
              }
            } else {
              parser.error("Expected , to separate import references or } to end block".to_owned());
            }
    
            if parser.synchronize(sync::close_pair_or(sync::operator(LeftBracket), sync::operator(RightBracket), sync::operator(Comma))) {
              if let Some(&Token { data: TokenData::Operator(op), .. }) = parser.curr_tok() {
                parser.advance();

                if op == Comma { continue }
                else { break }
              }
            }
      
            // Could not recover
            return None
          }
        }
      }

      refs
    } else if let Some((path_or_ident, path_end)) = path(parser) {
      match path_or_ident {
        Either::A(path) => {
          let mut path = path;

          let new_name_and_origin = if let Ok(new_name_and_origin) = get_new_name_and_origin(parser) { new_name_and_origin } else { return None };
          
          let (new_name, end) = if let Some((new_name, new_end)) = new_name_and_origin {
            (Some(new_name), new_end)
          } else {
            (None, path_end)
          };

          let refs = vec![ Alias { base: path.pop(), new_name } ];
          return Some(Item::new(ItemData::Import { refs, path }, SourceRegion::merge(start_region, end)))
        },
        Either::B(base) => {
          let new_name = if let Ok(new_name) = get_new_name(parser) { new_name } else { return None };

          vec![ Alias { base, new_name } ]
        }
      }
      
    } else {
      parser.error("Expected identifier or path to follow `import` keyword".to_owned());

      return None
    };

    if let Some(Token { data: TokenData::Keyword(From), .. }) = parser.curr_tok() {
      parser.advance();

      if let Some((path_or_ident, path_end)) = path(parser) {
        let path = match path_or_ident {
          Either::A(path) => path,
          Either::B(ident) => ident.into()
        };

        return Some(Item::new(ItemData::Import { refs, path }, SourceRegion::merge(start_region, path_end)))
      } else {
        return None
      }
    } else {
      parser.error("Expected `from` keyword to separate imported identifier from source path".to_owned());

      return None
    }
  }

  unreachable!("Internal error, import item parselet called on non-import token");
}

fn itm_export (parser: &mut Parser) -> Option<Item> {
  if let Some(&Token { data: TokenData::Keyword(Export), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
      parser.advance();

      let mut refs = Vec::new();

      let mut ref_ok = true;

      let end_region;

      loop {
        match parser.curr_tok() {
          // Unexpected end of input
          None => {
            parser.error_at(SourceRegion::merge(start_region, parser.curr_region()), "Unexpected end of input, expected } to close block".to_owned());
            return None
          },
  
          // The end of the block
          Some(&Token { data: TokenData::Operator(RightBracket), origin }) => {
            parser.advance();
            end_region = origin;
            break
          },
  
          // Refs
          _ => {
            if ref_ok {
              if let Some(Token { data: TokenData::Identifier(base), .. }) = parser.curr_tok() {
                let base = base.clone();
                
                parser.advance();
                                
                if let Ok(new_name) = get_new_name(parser) {
                  refs.push(Alias { base, new_name });

                  if let Some(Token { data: TokenData::Operator(Comma), .. }) = parser.curr_tok() {
                    parser.advance();
                    ref_ok = true;
                  }

                  continue
                }
              }
            } else {
              parser.error("Expected , to separate export references or } to end block".to_owned());
            }
    
            if parser.synchronize(sync::close_pair_or(sync::operator(LeftBracket), sync::operator(RightBracket), sync::operator(Comma))) {
              if let Some(&Token { data: TokenData::Operator(op), origin }) = parser.curr_tok() {
                parser.advance();

                if op == Comma {
                  continue
                } else { 
                  end_region = origin;
                  break
                }
              }
            }
      
            // Could not recover
            return None
          }
        }
      }

      return Some(Item::new(ItemData::Export(Export::List(refs)), SourceRegion::merge(start_region, end_region)))
    } else if let Some(&Token { data: TokenData::Identifier(ref base), origin: base_end }) = parser.curr_tok() {
      let base = base.to_owned();

      parser.advance();

      let new_name_and_origin = if let Ok(new_name_and_origin) = get_new_name_and_origin(parser) { new_name_and_origin } else { return None };

      let (new_name, end) = if let Some((new_name, end)) = new_name_and_origin {
        (Some(new_name), end)
      } else {
        (None, base_end)
      };

      return Some(Item::new(ItemData::Export(Export::List(vec![ Alias { base, new_name }])), SourceRegion::merge(start_region, end)))
    } else {
      let curr_tok = if let Some(tok) = parser.curr_tok() { tok } else {
        parser.error("Expected a list, alias, or inline item to follow `export` keyword".to_owned());
        return None
      };

      let inline = if let Some(parselet_function) = ItemParselet::get_function(curr_tok) {
        parselet_function(parser)
      } else {
        parser.error("No syntactic match for this token in the context of a top level item".to_owned());
    
        return None
      }?;

      let region = SourceRegion::merge(start_region, inline.origin);

      return Some(Item::new(ItemData::Export(Export::Inline(box inline)), region))
    }
  }

  unreachable!("Internal error, export item parselet called on non-export token");
}


fn itm_module (parser: &mut Parser) -> Option<Item> {
  if let Some(&Token { data: TokenData::Keyword(Module), origin: start_region }) = parser.curr_tok() {
    parser.advance();

    if let Some(&Token { data: TokenData::Identifier(ref identifier), origin: end_region }) = parser.curr_tok() {
      let identifier = identifier.clone();

      parser.advance();

      if let Some(&Token { data: TokenData::Operator(LeftBracket), .. }) = parser.curr_tok() {
        parser.advance();

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
              parser.advance();
              return Some(Item::new(ItemData::Module { identifier, items, inline: true }, SourceRegion::merge(start_region, end_region)));
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
                } // else { Error message already provided by item }
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
                  Token { data: TokenData::Operator(RightBracket), .. } => continue, // The next iteration will handle the closing bracket
                  _ => unreachable!("Internal error, unexpected parser state post synchronization")
                }
              } else {
                // Cannot recover state locally
                return None
              }
            }
          }
        }
      } else {
        let curr_source_key = start_region.source.expect("Internal error: Module item has no source origin");
        let curr_source = SOURCE_MANAGER.get(curr_source_key).expect("Internal error: Module item has invalid source origin");

        let curr_path = &curr_source.path;
        let curr_dir = curr_path.parent().expect("Internal error: Source file path has no directory");

        let sub_mod_path: std::path::PathBuf = [ curr_dir, identifier.as_ref().as_ref() ].iter().collect::<std::path::PathBuf>();
        let sub_dir_mod_path: std::path::PathBuf = sub_mod_path.join("mod.ms");
        let sub_file_mod_path: std::path::PathBuf = sub_mod_path.with_extension("ms");

        let dir_exists = sub_dir_mod_path.exists();
        let file_exists = sub_file_mod_path.exists();

        let local_region = SourceRegion::merge(start_region, end_region);

        let local_error = |msg| parser.error_at(local_region, format!("Cannot import submodule `{}`: {}", identifier, msg));

        let sub_mod_path = if dir_exists && !file_exists {
          sub_dir_mod_path
        } else if file_exists && !dir_exists {
          sub_file_mod_path
        } else {
          if file_exists && dir_exists {
            local_error(format!(
              "A file exists at both [{}] and [{}], please remove one to resolve the ambiguity",
              sub_dir_mod_path.display(), sub_file_mod_path.display()
            ))
          } else {
            local_error(format!(
              "Expected a file at either [{}] or [{}], but neither exists",
              sub_dir_mod_path.display(), sub_file_mod_path.display()
            ))
          }

          return None
        };

        let sub_source_key = match SOURCE_MANAGER.load(&sub_mod_path) {
          Ok(key) => key,
          Err(e) => {
            if e.kind() == std::io::ErrorKind::AlreadyExists {
              local_error(format!(
                "File [{}] has already been loaded during this session, it cannot be imported twice",
                sub_mod_path.display()
              ))
            } else {
              local_error(format!(
                "Unexpected error loading file [{}] from disk: {}",
                sub_mod_path.display(), e
              ))
            }

            return None
          }
        };

        let mut sub_lexer = Lexer::new(sub_source_key);
        let sub_stream = sub_lexer.lex_stream();
        let mut sub_parser = Parser::new(&sub_stream);

        return Some(Item::new(ItemData::Module { identifier, items: sub_parser.parse_ast(), inline: false }, local_region));
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
  let (start_region, mut end_region) = if let Some(&Token { data: TokenData::Keyword(Function), origin }) = parser.curr_tok() {
    parser.advance();
    (origin, origin)
  } else {
    unreachable!("Internal error, function parselet called on non-fn token");
  };

  let identifier = if let Some(&Token { data: TokenData::Identifier(ref identifier), .. }) = parser.curr_tok() {
    let identifier = identifier.clone();

    parser.advance();

    identifier
  } else {
    parser.error("Expected identifier for function to follow fn keyword".to_owned());
    return None;
  };

  let mut parameters = Vec::new();

  if let Some(&Token { data: TokenData::Operator(LeftParen), .. }) = parser.curr_tok() {
    parser.advance();

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

  Some(Item::new(
    ItemData::Function { identifier, parameters, return_type, body },
    SourceRegion::merge(start_region, end_region)
  ))
}


struct ItemParselet {
  predicate: ParseletPredicate,
  function: ParseletFunction<Item>,
}

macro_rules! itm { ($( $predicate: expr => $function: expr ),* $(,)?) => { &[ $( ItemParselet { predicate: $predicate, function: $function } ),* ] } }

impl ItemParselet {
  const PARSELETS: &'static [Self] = itm! [
    |token| token.is_keyword(Module)   => itm_module,
    |token| token.is_keyword(Global)   => itm_global,
    |token| token.is_keyword(Function) => itm_function,
  ];

  fn get_function (token: &Token) -> Option<ParseletFunction<Item>> {
    for parselet in Self::PARSELETS.iter() {
      if (parselet.predicate)(token) {
        return Some(parselet.function)
      }
    }
  
    None
  }
}