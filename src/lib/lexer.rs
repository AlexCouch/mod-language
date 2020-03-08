//! Contains Lexer and supporting structures and functions

use super::source::*;
use super::token::*;

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

/// The type of function used by a Lexer to analyze a particular variant of syntax
type Lexlet = fn (&mut Lexer) -> LexletResult;


/// State information for a lexical analysis session
pub struct Lexer<'a> {
  source: &'a Source,
  chars: std::str::Chars<'a>,
  offset: usize,
  location: SourceLocation,
  prev: Option<char>,
  curr: Option<char>,
  next: Option<char>,
  markers: Vec<SourceLocation>,
}

impl<'a> Lexer<'a> {
  /// Create a new Lexer for a given source string
  pub fn new (source: &'a Source) -> Option<Self> {
    let mut chars = source.chars();
    let curr = chars.next();
    let next = chars.next();

    Some(Self {
      source,
      chars,
      offset: 0,
      location: SourceLocation { line: 0, column: 0 },
      prev: None,
      curr,
      next,
      markers: Vec::new(),
    })
  }


  /// Create a SourceLocation bookmark in a Lexer
  pub fn push_marker (&mut self) {
    self.markers.push(self.location);
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
    self.pop_marker().map(|start| SourceRegion { start, end: self.location })
  }

  /// Get the current SourceLocation of a Lexer
  pub fn curr_location (&self) -> SourceLocation {
    self.location
  }
  
  /// Get a SourceRegion based on the current SourceLocation of a Lexer (zero-width)
  pub fn curr_region (&self) -> SourceRegion {
    self.location.to_region()
  }


  /// Create a user-directed Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn message_pop (&mut self, kind: MessageKind, content: Option<String>) {
    self.source.message(
     Some(SourceRegion {
        start: self.pop_marker().unwrap_or(self.location),
        end: self.location
      }),
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of a Lexer, with a custom line and column origin
  pub fn message_at (&self, origin: SourceRegion, kind: MessageKind, content: Option<String>) {
    self.source.message(
      Some(origin),
      kind,
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
  pub fn error_pop (&mut self, content: Option<String>) {
    self.message_pop(MessageKind::Error, content)
  }

  /// Create a user-directed Error Message in the Source of a Lexer, with a custom line and column origin
  pub fn error_at (&self, origin: SourceRegion, content: Option<String>) {
    self.message_at(origin, MessageKind::Error, content)
  }

  
  /// Create a user-directed Warning Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn warning_pop (&mut self, content: Option<String>) {
    self.message_pop(MessageKind::Warning, content)
  }

  /// Create a user-directed Warning Message in the Source of a Lexer, with a custom line and column origin
  pub fn warning_at (&self, origin: SourceRegion, content: Option<String>) {
    self.message_at(origin, MessageKind::Warning, content)
  }

  
  /// Create a user-directed Notice Message in the Source of a Lexer
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be zero-width,
  /// and will originate at the Lexer's current location
  pub fn notice_pop (&mut self, content: Option<String>) {
    self.message_pop(MessageKind::Notice, content)
  }

  /// Create a user-directed Notice Message in the Source of a Lexer, with a custom line and column origin
  pub fn notice_at (&self, origin: SourceRegion, content: Option<String>) {
    self.message_at(origin, MessageKind::Notice, content)
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
    self.next = self.chars.next();

    self.curr
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

      LexletResult::Some(Token::new(TokenData::Identifier(ident), lexer.pop_marker_region().unwrap()))
    },
    _ => LexletResult::None
  }
}


const LEXLETS: &[Lexlet] = &[
  lex_whitespace,
  lex_identifier,
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