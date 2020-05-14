//! Contains Lexer and supporting structures and functions

use crate::{
  session::{ SESSION, MessageKind, Message, },
  source::{ SOURCE_MANAGER, SourceLocation, SourceRegion, SourceKey, },
  token::{ Token, TokenData, },
  util::{ Unref, },
};

mod lexlets;
use lexlets::LexletResult;

pub use lexlets::InvalidLexicalSymbol;



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
  source_key: SourceKey,
  chars: &'a [char],
  length: usize,
  stored_locale: Option<LexerLocale>,
  locale: LexerLocale,
  markers: Vec<SourceLocation>,
}

impl<'a> Lexer<'a> {
  /// Create a new Lexer for a given Source
  pub fn new (source_key: SourceKey) -> Self {
    let source = SOURCE_MANAGER.get_source(source_key).expect("Internal error, invalid SourceKey passed to Lexer");
    let chars = source.chars();
    let length = chars.len();
    let curr = chars.get(0).unref();
    let next = chars.get(1).unref();

    Self {
      source_key,
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
    }
  }


  /// Get the previous char from the Source of a Lexer
  pub fn prev_char (&self) -> Option<char> {
    self.locale.prev
  }

  /// Get the current char from the Source of a Lexer
  pub fn curr_char (&self) -> Option<char> {
    self.locale.curr
  }

  /// Get the next char from the Source of a Lexer
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
    self.locale.next = self.chars.get(self.locale.location.index + 1).unref();

    self.locale.curr
  }


  /// Save the locale state of a Lexer
  /// 
  /// Panics if there is already a saved locale
  #[track_caller]
  pub fn save_locale (&mut self) {
    assert!(self.stored_locale.is_none());
    self.stored_locale.replace(self.locale);
  }

  /// Restore a saved locale state of a Lexer
  /// 
  /// Panics if there is not a saved locale
  #[track_caller]
  pub fn load_locale (&mut self) {
    self.locale = self.stored_locale.take().unwrap();
  }

  /// Delete the saved locale state of a Lexer
  /// 
  /// Panics if there is not a saved locale
  #[track_caller]
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
    self.pop_marker().map(|start| SourceRegion { source: self.source_key, start, end: self.locale.location })
  }

  /// Get the current SourceLocation of a Lexer
  pub fn curr_location (&self) -> SourceLocation {
    self.locale.location
  }
  
  /// Get a SourceRegion based on the current SourceLocation of a Lexer,
  /// and the top marker on the stack if one exists
  pub fn curr_region (&self) -> SourceRegion {
    SourceRegion {
      source: self.source_key,
      start: self.markers.last().unref().unwrap_or(self.locale.location),
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
  pub fn message (&mut self, kind: MessageKind, content: String) -> &mut Message {
    SESSION.message(
      self.curr_region(),
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
  pub fn message_pop (&mut self, kind: MessageKind, content: String) -> &mut Message {
    SESSION.message(
     SourceRegion {
        source: self.source_key,
        start: self.pop_marker().unwrap_or(self.locale.location),
        end: self.locale.location
      },
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of a Lexer, with a custom line and column origin
  #[allow(clippy::mut_from_ref)]
  pub fn message_at (&self, origin: SourceRegion, kind: MessageKind, content: String) -> &mut Message {
    SESSION.message(
      origin,
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
  pub fn error (&mut self, content: String) -> &mut Message {
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
  pub fn error_pop (&mut self, content: String) -> &mut Message {
    self.message_pop(MessageKind::Error, content)
  }

  /// Create a user-directed Error Message in the Source of a Lexer, with a custom line and column origin
  #[allow(clippy::mut_from_ref)]
  pub fn error_at (&self, origin: SourceRegion, content: String) -> &mut Message {
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
  pub fn warning (&mut self, content: String) -> &mut Message {
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
  pub fn warning_pop (&mut self, content: String) -> &mut Message {
    self.message_pop(MessageKind::Warning, content)
  }

  /// Create a user-directed Warning Message in the Source of a Lexer, with a custom line and column origin
  #[allow(clippy::mut_from_ref)]
  pub fn warning_at (&self, origin: SourceRegion, content: String) -> &mut Message {
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
  pub fn notice (&mut self, content: String) -> &mut Message {
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
  pub fn notice_pop (&mut self, content: String) -> &mut Message {
    self.message_pop(MessageKind::Notice, content)
  }

  /// Create a user-directed Notice Message in the Source of a Lexer, with a custom line and column origin
  #[allow(clippy::mut_from_ref)]
  pub fn notice_at (&self, origin: SourceRegion, content: String) -> &mut Message {
    self.message_at(origin, MessageKind::Notice, content)
  }


  /// Advance by one Token of a Lexer's source and return the Token
  pub fn lex_token (&mut self) -> Result<Option<Token>, InvalidLexicalSymbol> {
    if self.curr_char().is_none() { return Ok(None) }
    
    for lexlet in Self::LEXLETS.iter() {
      match lexlet(self) {
        LexletResult::Some(token) => return Ok(Some(token)),
        LexletResult::Err(sym) => return Err(sym),
        LexletResult::None => continue
      } 
    }

    Ok(None)
  }

  /// Convert an entire Source's content into a TokenStream
  pub fn lex_stream (&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();
  
    loop {
      match self.lex_token() {
        Ok(tok_or_eof) => if let Some(token) = tok_or_eof {
          tokens.push(token)
        } else {
          break tokens
        },
        Err(InvalidLexicalSymbol { symbol, origin }) => {
          tokens.push(Token::new(TokenData::Invalid, origin));
          self.error_at(origin, format!("Unexpected lexical symbol {:?}", symbol));
        }
      }
    }
  }
}
