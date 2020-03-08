//! Contains Lexer and supporting structures and functions

use super::source::*;
use super::token::*;
use super::util::{ Either, DerefWrapper };

/// An unrecognized lexical symbol
pub struct InvalidLexicalSymbol {
  /// The invalid symbol
  pub symbol: char,
  /// The area of a Source an invalid symbol was found
  pub origin: SourceRegion,
}

/// The result of a single step of lexical analysis in a Lexer
enum LexletResult {
  Some(Token),
  Err(InvalidLexicalSymbol),
  None
}


/// Contains state information about the location of a Lexer in a Source
#[derive(Debug, Clone, Copy)]
pub struct LexerLocale {
  location: SourceLocation,
  prev: Option<char>,
  curr: Option<char>,
  next: Option<char>,
}

/// State information for a lexical analysis session
pub struct Lexer<'a> {
  source: &'a Source,
  chars: &'a [char],
  length: usize,
  stored_locale: Option<LexerLocale>,
  locale: LexerLocale,
  markers: Vec<SourceLocation>,
}

impl<'a> Lexer<'a> {
  /// Create a new Lexer for a given source string
  pub fn new (source: &'a Source) -> Option<Self> {
    let chars = source.chars();
    let length = chars.len();
    let curr = chars.get(0).deref_wrapper();
    let next = chars.get(1).deref_wrapper();

    Some(Self {
      source,
      chars,
      length,
      stored_locale: None,
      locale: LexerLocale {
        location: SourceLocation { index: 0, line: 0, column: 0 },
        prev: None,
        curr,
        next,
      },
      markers: Vec::new(),
    })
  }


  /// Save the locale state of a Lexer
  /// 
  /// Panics if there is already a saved locale
  pub fn save_locale (&mut self) {
    assert!(self.stored_locale.is_none());
    self.stored_locale.replace(self.locale);
  }

  /// Restore a saved locale state of a Lexer
  /// 
  /// Panics if there is not a saved locale
  pub fn load_locale (&mut self) {
    self.locale = self.stored_locale.take().unwrap();
  }

  /// Delete the saved locale state of a Lexer
  /// 
  /// Panics if there is not a saved locale
  pub fn discard_saved_locale (&mut self) {
    self.stored_locale.take().unwrap();
  }


  /// Create a SourceLocation bookmark in a Lexer
  pub fn push_marker (&mut self) {
    self.markers.push(self.locale.location);
  }

  /// Get a SourceLocation marker saved onto the Lexer's internal stack
  /// 
  /// Returns None if there was no SourceLocation marker on the stack
  pub fn pop_marker (&mut self) -> Option<SourceLocation> {
    self.markers.pop()
  }

  /// Get a SourceRegion by popping a SourceLocation marker off of the Lexer's stack
  /// and combine it with the Lexer's current SourceLocation
  pub fn pop_marker_region (&mut self) -> Option<SourceRegion> {
    self.pop_marker().map(|start| SourceRegion { start, end: self.locale.location })
  }

  /// Get the current SourceLocation of a Lexer
  pub fn curr_location (&self) -> SourceLocation {
    self.locale.location
  }
  
  /// Get a SourceRegion based on the current SourceLocation of a Lexer,
  /// and the top marker on the stack if one exists
  pub fn curr_region (&self) -> SourceRegion {
    SourceRegion {
      start: self.markers.last().deref_wrapper().unwrap_or(self.locale.location),
      end: self.locale.location
    }
  }


  /// Create a user-directed Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will use it to create a SourceRegion, without popping it from the stack
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn message (&mut self, kind: MessageKind, content: String) {
    self.source.message(
      Some(self.curr_region()),
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn message_pop (&mut self, kind: MessageKind, content: String) {
    self.source.message(
     Some(SourceRegion {
        start: self.pop_marker().unwrap_or(self.locale.location),
        end: self.locale.location
      }),
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of a Lexer, with a custom line and column origin
  pub fn message_at (&self, origin: SourceRegion, kind: MessageKind, content: String) {
    self.source.message(
      Some(origin),
      kind,
      content
    )
  }


  /// Create a user-directed Error Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will use it to create a SourceRegion, without popping it from the stack
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn error (&mut self, content: String) {
    self.message(
      MessageKind::Error,
      content
    )
  }

  /// Create a user-directed Error Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn error_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Error, content)
  }

  /// Create a user-directed Error Message in the Source of a Lexer, with a custom line and column origin
  pub fn error_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Error, content)
  }

  
  /// Create a user-directed Warning Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will use it to create a SourceRegion, without popping it from the stack
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn warning (&mut self, content: String) {
    self.message(
      MessageKind::Warning,
      content
    )
  }

  /// Create a user-directed Warning Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn warning_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Warning, content)
  }

  /// Create a user-directed Warning Message in the Source of a Lexer, with a custom line and column origin
  pub fn warning_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Warning, content)
  }

  
  /// Create a user-directed Notice Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will use it to create a SourceRegion, without popping it from the stack
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn notice (&mut self, content: String) {
    self.message(
      MessageKind::Notice,
      content
    )
  }

  /// Create a user-directed Notice Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn notice_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Notice, content)
  }

  /// Create a user-directed Notice Message in the Source of a Lexer, with a custom line and column origin
  pub fn notice_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Notice, content)
  }


  /// Get the previous character from the source of a Lexer
  pub fn prev_char (&self) -> Option<char> {
    self.locale.prev
  }

  /// Get the current character from the source of a Lexer
  pub fn curr_char (&self) -> Option<char> {
    self.locale.curr
  }

  /// Get the next character from the source of a Lexer
  pub fn peek_char (&self) -> Option<char> {
    self.locale.next
  }

  /// Advance the offset of a Lexer
  pub fn advance (&mut self) -> Option<char> {
    if self.locale.location.index <= self.length {
      self.locale.location.index += 1;

      if self.locale.curr.unwrap() == '\n' {
        self.locale.location.line += 1;
        self.locale.location.column = 0;
      } else {
        self.locale.location.column += 1;
      }
    }

    self.locale.prev = self.locale.curr;
    self.locale.curr = self.locale.next;
    self.locale.next = self.chars.get(self.locale.location.index).deref_wrapper();

    self.locale.curr
  }


  /// Advance by one Token of a Lexer's source and return the Token
  pub fn lex_token (&mut self) -> Result<Option<Token>, InvalidLexicalSymbol> {
    if self.curr_char().is_none() { return Ok(None) }
    
    for lexlet in LEXLETS.iter() {
      match lexlet(self) {
        LexletResult::Some(token) => return Ok(Some(token)),
        LexletResult::Err(sym) => return Err(sym),
        LexletResult::None => continue
      } 
    }

    Ok(None)
  }

  /// Convert an entire Source's content into a TokenStream
  pub fn lex_stream (&mut self) -> TokenStream {
    let mut tokens = Vec::new();
  
    loop {
      match self.lex_token() {
        Ok(tok_or_eof) => if let Some(token) = tok_or_eof { tokens.push(token) } else { break TokenStream::new(tokens, self.source) },
        Err(InvalidLexicalSymbol { symbol, origin }) => self.error_at(origin, format!("Unexpected lexical symbol {:?}", symbol))
      }
    }
  }
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
        } else if ch.is_ascii_alphabetic() {
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


const LEXLETS: &[fn (&mut Lexer) -> LexletResult] = &[
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