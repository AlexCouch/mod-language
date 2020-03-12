use crate::source::SourceRegion;
use crate::token::{ Token, TokenData, Identifier, Number, IDENTIFIER_VALUES, SYM_OPERATOR_VALUES, };
use crate::util::{ Either, };

use super::Lexer;


/// An unrecognized lexical symbol
pub struct InvalidLexicalSymbol {
  /// The invalid symbol
  pub symbol: char,
  /// The area of a Source an invalid symbol was found
  pub origin: SourceRegion,
}

/// The result of a single step of lexical analysis in a Lexer
pub enum LexletResult {
  Some(Token),
  Err(InvalidLexicalSymbol),
  None
}


struct NumberBuilder {
  digits: [u8; Self::MAX_LENGTH],
  length: usize,
}

impl Default for NumberBuilder {
  #[inline] fn default () -> Self { Self::new() }
}

impl NumberBuilder {
  const MAX_LENGTH: usize = 256;

  fn new () -> Self {
    Self {
      digits: [0u8; Self::MAX_LENGTH],
      length: 0
    }
  }

  fn push (&mut self, d: char) {
    if self.length < Self::MAX_LENGTH {
      self.digits[self.length] = d as _;
    }

    self.length += 1;
  }
}

impl AsRef<str> for NumberBuilder {
  fn as_ref (&self) -> &str { unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.digits.as_ptr(), self.length.min(Self::MAX_LENGTH))) } }
}


fn lex_whitespace (lexer: &mut Lexer) -> LexletResult {
  loop {
    match lexer.curr_char() {
      Some(ch) if ch.is_whitespace() => { lexer.advance(); },
      _ => { return LexletResult::None }
    }
  }
}


fn lex_identifier (lexer: &mut Lexer) -> LexletResult {
  match lexer.curr_char() {
    Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => {
      lexer.push_marker();

      let mut ident = Identifier::default();

      ident.append(ch);

      lexer.advance();

      loop {
        match lexer.curr_char() {
          Some(ch) if ch.is_ascii_alphanumeric() || ch == '_' => {
            ident.append(ch);
            lexer.advance();
          },
          _ => break
        }
      }

      'id_loop: for (substr, either) in IDENTIFIER_VALUES {
        for (i, op_char) in substr.chars().enumerate() {  
          if ident.get(i) != Some(op_char) {
            continue 'id_loop;
          }
        }
    
        // If this point has been reached, the current identifier is a full match
        // Assuming identifiers are sorted from longest to shortest then we can accept this identifier
    
        return LexletResult::Some(Token::new(
          match either {
            Either::A(keyword) => TokenData::Keyword(*keyword),
            Either::B(operator) => TokenData::Operator(*operator),
          },
          lexer.pop_marker_region().unwrap()
        ))
      }

      LexletResult::Some(Token::new(TokenData::Identifier(ident), lexer.pop_marker_region().unwrap()))
    },
    _ => LexletResult::None
  }
}


fn lex_decimal_number (lexer: &mut Lexer) -> LexletResult {
  match lexer.curr_char() {
    Some(ch)
    if ch.is_ascii_digit()
    || (ch == '.' && if let Some(nx) = lexer.peek_char() { nx.is_ascii_digit() } else { false })
    => {
      lexer.push_marker();

      let mut builder = NumberBuilder::new();

      builder.push(ch);
      lexer.advance();

      let mut float = ch == '.';
      let mut allow_underscore = !float;
      let mut alphabetic_err_start = None;

      if float {
        builder.push(lexer.curr_char().unwrap());
        lexer.advance();
      }

      while let Some(ch) = lexer.curr_char() {
        if ch.is_ascii_digit() {
          if alphabetic_err_start.is_none() {
            allow_underscore = true;
            builder.push(ch);
          }
          lexer.advance();
        } else if ch == '.' && !float && if let Some(nx) = lexer.peek_char() { nx.is_ascii_digit() } else { false } && alphabetic_err_start.is_none() {
          allow_underscore = true;
          float = true;
          builder.push(ch);
          lexer.advance();
          builder.push(lexer.curr_char().unwrap());
          lexer.advance();
        } else if ch == '_' && allow_underscore {
          lexer.advance();
        } else if ch.is_ascii_alphabetic() { // TODO exponent notation
          if alphabetic_err_start.is_none() {
            alphabetic_err_start = Some(lexer.curr_location());
          }
          lexer.advance();
        } else {
          break
        }
      }

      if builder.length > NumberBuilder::MAX_LENGTH {
        lexer.error(format!("Decimal literal is too long at {} characters, the max length is {}", builder.length, NumberBuilder::MAX_LENGTH));
      }

      if let Some(start) = alphabetic_err_start {
        lexer.error_at(SourceRegion { start, end: lexer.curr_location() }, "Unexpected characters in decimal literal".to_owned());
      }

      LexletResult::Some(Token::new(
        TokenData::Number(if float {
          Number::FloatingPoint(match builder.as_ref().parse::<f64>() {
            Ok(f) => f,
            Err(e) => {
              lexer.error(format!("Failed to parse floating point decimal literal: {}", e));
              0.0
            }
          })
        } else {
          Number::Integer(match builder.as_ref().parse::<u64>() {
            Ok(i) => i,
            Err(e) => {
              lexer.error(format!("Failed to parse integer decimal literal: {}", e));
              0
            }
          })
        }),
        lexer.pop_marker_region().unwrap()
      ))
    },

    _ => LexletResult::None
  }
}


fn lex_operator (lexer: &mut Lexer) -> LexletResult {
  lexer.push_marker();

  'op_loop: for (substr, operator) in SYM_OPERATOR_VALUES {
    lexer.save_locale();

    for op_char in substr.chars() {  
      if lexer.curr_char() != Some(op_char) {
        lexer.load_locale();
        continue 'op_loop;
      } else {
        lexer.advance();
      }
    }

    lexer.discard_saved_locale();

    // If this point has been reached, the current operator is a full match
    // Assuming operators are sorted from longest to shortest then we can accept this operator

    return LexletResult::Some(Token::new(
      TokenData::Operator(*operator),
      lexer.pop_marker_region().unwrap()
    ))
  }

  lexer.pop_marker();

  LexletResult::None
}


pub const LEXLETS: &[fn (&mut Lexer) -> LexletResult] = &[
  lex_whitespace,
  lex_identifier,
  lex_decimal_number,
  lex_operator,
  |lexer: &mut Lexer| -> LexletResult {
    if let Some(ch) = lexer.curr_char() {
      lexer.push_marker();
      lexer.advance();
      LexletResult::Err(InvalidLexicalSymbol { symbol: ch, origin: lexer.pop_marker_region().unwrap() })
    } else {
      LexletResult::None
    }
  }
];