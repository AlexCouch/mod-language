//! Contains Lexer and supporting structures and functions

use super::source::*;
use super::token::*;

/// The result of a single step of lexical analysis in a Lexer
enum LexletResult {
  Some(Token),
  Err(char),
  None
}

/// The type of function used by a Lexer to analyze a particular variant of syntax
type Lexlet = fn (&mut Lexer) -> LexletResult;


/// State information for a lexical analysis session
pub struct Lexer<'a> {
  source: std::str::Chars<'a>,
  offset: usize,
  location: SourceLocation,
  prev: Option<char>,
  curr: Option<char>,
  next: Option<char>,
  marker: Option<SourceLocation>,
  source_id: u32,
}

impl<'a> Lexer<'a> {
  /// Create a new Lexer for a given source string
  pub fn new (source_id: u32) -> Option<Self> {
    let source = get_source_content(source_id)?;
    let mut source = source.chars();
    let curr = source.next();
    let next = source.next();

    Some(Self {
      source,
      offset: 0,
      location: SourceLocation { line: 0, column: 0 },
      prev: None,
      curr,
      next,
      marker: None,
      source_id,
    })
  }

  /// Create a SourceLocation bookmark in a Lexer
  pub fn mark (&mut self) {
    debug_assert!(self.marker.is_none());
    self.marker = Some(self.location);
  }

  /// Create a SourceRegion from a SourceLocation marker saved previously and the current SourceLocation of a Lexer
  pub fn pop_mark (&mut self) -> SourceOrigin {
    if let Some(start) = self.marker.take() {
      SourceOrigin {
        region: SourceRegion {
          start,
          end: self.location
        },
        id: self.source_id
      }
    } else {
      panic!("No marker was found to create SourceRegion");
    }
  }

  /// Get the previous character from the source of a Lexer
  pub fn prev_char (&self) -> Option<char> {
    self.prev
  }

  /// Get the current character from the source of a Lexer
  pub fn curr_char (&self) -> Option<char> {
    self.curr
  }

  /// Get the next character from the source of a Lexer
  pub fn peek_char (&self) -> Option<char> {
    self.next
  }

  /// Advance the offset of a Lexer
  pub fn advance (&mut self) -> Option<char> {
    if let Some(curr) = self.curr {
      if curr == '\n' {
        self.location.line += 1;
        self.location.column = 0;
      } else {
        self.location.column += 1;
      }

      self.offset += 1;
    }

    self.prev = self.curr;
    self.curr = self.next;
    self.next = self.source.next();

    self.curr
  }

  /// Advance by one Token of a Lexer's source and return the Token
  pub fn lex_token (&mut self) -> Result<Option<Token>, char> {
    if self.curr_char().is_none() { return Ok(None) }

    dbg!(self.curr_char());

    for lexlet in LEXLETS.iter() {
      match lexlet(self) {
        LexletResult::Some(token) => return Ok(Some(token)),
        LexletResult::Err(ch) => return Err(ch),
        LexletResult::None => continue
      } 
    }

    Ok(None)
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
      lexer.mark();

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

      LexletResult::Some(Token::new(TokenData::Identifier(ident), lexer.pop_mark()))
    },
    _ => LexletResult::None
  }
}


const LEXLETS: &[Lexlet] = &[
  lex_whitespace,
  lex_identifier,
  |lexer: &mut Lexer| -> LexletResult {
    if let Some(ch) = lexer.curr_char() {
      lexer.advance();
      LexletResult::Err(ch)
    } else {
      LexletResult::None
    }
  }
];