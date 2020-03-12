//! Contains Parser and supporting structures and functions

use crate::{
  token::{ Token, TokenStream },
  source::{ SourceLocation, SourceRegion, MessageKind }
};


pub mod sync;
pub mod type_expression;
pub mod expression;
pub mod statement;
pub mod block;
pub mod item;

use sync::SyncPredicate;


/// Contains state information about the location of a Parser in a TokenStream
#[derive(Debug, Clone, Copy)]
pub struct ParserLocale<'a> {
  index: usize,
  prev: Option<&'a Token>,
  curr: Option<&'a Token>,
  next: Option<&'a Token>,
}

/// State information for a semantic analysis session
pub struct Parser<'a> {
  stream: &'a TokenStream<'a>,
  tokens: &'a [Token],
  length: usize,
  stored_locale: Option<ParserLocale<'a>>,
  locale: ParserLocale<'a>,
  markers: Vec<SourceLocation>,
}

impl<'a> Parser<'a> {
  /// Create a new Parser for a given TokenStream
  pub fn new (stream: &'a TokenStream) -> Self {
    let tokens = stream.tokens();
    let length = tokens.len();
    let curr = tokens.get(0);
    let next = tokens.get(1);

    Self {
      stream,
      tokens,
      length,
      stored_locale: None,
      locale: ParserLocale {
        index: 0,
        prev: None,
        curr,
        next,
      },
      markers: Vec::new()
    }
  }


  /// Get the previous Token from the TokenStream of a Parser
  pub fn prev_tok (&self) -> Option<&Token> {
    self.locale.prev
  }

  /// Get the current Token from the TokenStream of a Parser
  pub fn curr_tok (&self) -> Option<&Token> {
    self.locale.curr
  }

  /// Get the next Token from the TokenStream of a Parser
  pub fn peek_tok (&self) -> Option<&Token> {
    self.locale.next
  }

  /// Advance the offset of a Parser
  pub fn advance (&mut self) -> Option<&Token> {
    if self.locale.index <= self.length {
      self.locale.index += 1;
    }

    self.locale.prev = self.locale.curr;
    self.locale.curr = self.locale.next;
    self.locale.next = self.tokens.get(self.locale.index);

    self.locale.curr
  }

  /// Determine if the Parser is still capable of providing more Tokens to Parselets
  pub fn valid (&self) -> bool {
    self.locale.curr.is_some()
  }


  /// Save the locale state of a Parser
  /// 
  /// Panics if there is already a saved locale
  pub fn save_locale (&mut self) {
    assert!(self.stored_locale.is_none());
    self.stored_locale.replace(self.locale);
  }

  /// Restore a saved locale state of a Parser
  /// 
  /// Panics if there is not a saved locale
  pub fn load_locale (&mut self) {
    self.locale = self.stored_locale.take().unwrap();
  }

  /// Delete the saved locale state of a Parser
  /// 
  /// Panics if there is not a saved locale
  pub fn discard_saved_locale (&mut self) {
    self.stored_locale.take().unwrap();
  }


  /// Create a SourceLocation bookmark in a Parser
  /// 
  /// Panics if there is no active Token
  pub fn push_marker (&mut self) {
    self.markers.push(self.locale.curr.unwrap().origin.start);
  }

  /// Get a SourceLocation marker saved onto the Parser's internal stack
  /// 
  /// Returns None if there was no SourceLocation marker on the stack
  pub fn pop_marker (&mut self) -> Option<SourceLocation> {
    self.markers.pop()
  }

  /// Get a SourceRegion by popping a SourceLocation marker off of the Parser's stack
  /// and combine it with the Parser's previous Token's end SourceLocation
  /// 
  /// Panics if there is no previous Token
  pub fn pop_marker_region (&mut self) -> Option<SourceRegion> {
    self.pop_marker().map(|start| SourceRegion { start, end: self.locale.prev.unwrap().origin.end })
  }

  /// Get the Parser's current Token's starting SourceLocation
  /// 
  /// Panics if there is no active Token
  pub fn curr_location (&self) -> SourceLocation {
    self.locale.curr.unwrap().origin.start
  }
  
  /// Get the Parser's current Token's SourceRegion
  /// 
  /// Panics if there is no active Token
  pub fn curr_region (&self) -> SourceRegion {
    self.locale.curr.unwrap().origin
  }


  // TODO should synchronization leave a saved locale in the failure state?
  // TODO should synchronization's returned bool be must_use?

  
  /// Synchronize a Parser after an error is encountered
  /// 
  /// Uses a SyncPredicate to determine when the Parser has advanced to a valid state
  ///
  /// + If no valid state is reached by the end of the Parser's TokenStream, `false` is returned
  /// + Otherwise 'true` is returned and the Parser's locale remains on the Token that was accepted by the SyncPredicate
  #[must_use]
  pub fn synchronize (&mut self, mut predicate: impl SyncPredicate) -> bool {
    while let Some(token) = self.curr_tok() {
      if unsafe { predicate.sync(token) } {
        return true
      }
    }

    false
  }


  /// Synchronize a Parser after an error is encountered,
  /// while staying within a maximum number of Tokens from the error's origin
  /// 
  /// Uses a SyncPredicate to determine when the Parser has advanced to a valid state
  ///
  /// + If no valid state is reached by the time the given offset is reached, the Parser's locale is reset and 'false' is returned
  /// + If no valid state is reached by the end of the Parser's TokenStream, `false` is returned and the locale remains at the end of the Stream
  /// + Otherwise 'true` is returned and the Parser's locale remains on the Token that was accepted by the SyncPredicate
  #[must_use]
  pub fn synchronize_max_offset (&mut self, max_offset: usize, mut predicate: impl SyncPredicate) -> bool {
    let mut offset = 0;

    self.save_locale();

    while let Some(token) = self.curr_tok() {
      if offset >= max_offset {
        self.load_locale();
        break
      } else if unsafe { predicate.sync(token) } {
        self.discard_saved_locale();
        return true
      } else {
        offset += 1;
      }
    }

    self.discard_saved_locale();

    false
  }


  /// This is the same as the `synchronize` method,
  /// but it advances past the Token that was accepted by the SyncPredicate,
  /// and the caller is not required to know whether it was successful
  /// 
  /// This is useful for best-effort synchronization,
  /// e.g. in expressions where there is only so much that can be done to recover state before the next `;`
  pub fn synchronize_unchecked (&mut self, predicate: impl SyncPredicate) {
    if self.synchronize(predicate) {
      self.advance();
    }
  }

  /// This is the same as the `synchronize_max_offset` method,
  /// but it advances past the Token that was accepted by the SyncPredicate,
  /// and the caller is not required to know whether it was successful
  /// 
  /// This is useful for best-effort synchronization,
  /// e.g. in expressions where there is only so much that can be done to recover state before the next `;`
  pub fn synchronize_max_offset_unchecked (&mut self, max_offset: usize, predicate: impl SyncPredicate) {
    if self.synchronize_max_offset(max_offset, predicate) {
      self.advance();
    }
  }
  



  /// Create a user-directed Message in the Source of the TokenStream of a Parser
  /// 
  /// This will use the current Token's SourceRegion, and panics if one is not available
  pub fn message (&mut self, kind: MessageKind, content: String) {
    self.stream.source.message(
      Some(self.curr_region()),
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of the TokenStream of a Parser
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be taken from Parser's current Token
  pub fn message_pop (&mut self, kind: MessageKind, content: String) {
    self.stream.source.message(
      Some(self.pop_marker_region().or_else(|| Some(self.curr_region())).unwrap()),
      kind,
      content
    )
  }

  /// Create a user-directed Message in the Source of the TokenStream of a Parser,
  /// with a custom line and column origin
  pub fn message_at (&self, origin: SourceRegion, kind: MessageKind, content: String) {
    self.stream.source.message(
      Some(origin),
      kind,
      content
    )
  }


  /// Create a user-directed Error Message in the Source of the TokenStream of a Parser
  /// 
  /// This will use the current Token's SourceRegion, and panics if one is not available
  pub fn error (&mut self, content: String) {
    self.message(
      MessageKind::Error,
      content
    )
  }

  /// Create a user-directed Error Message in the Source of the TokenStream of a Parser
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be taken from Parser's current Token
  pub fn error_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Error, content)
  }

  /// Create a user-directed Error Message in the Source of the TokenStream of a Parser,
  /// with a custom line and column origin
  pub fn error_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Error, content)
  }

  
  /// Create a user-directed Warning Message in the Source of the TokenStream of a Parser
  /// 
  /// This will use the current Token's SourceRegion, and panics if one is not available
  pub fn warning (&mut self, content: String) {
    self.message(
      MessageKind::Warning,
      content
    )
  }

  /// Create a user-directed Warning Message in the Source of the TokenStream of a Parser
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be taken from Parser's current Token
  pub fn warning_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Warning, content)
  }

  /// Create a user-directed Warning Message in the Source of the TokenStream of a Parser,
  /// with a custom line and column origin
  pub fn warning_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Warning, content)
  }

  
  /// Create a user-directed Notice Message in the Source of the TokenStream of a Parser
  /// 
  /// This will use the current Token's SourceRegion, and panics if one is not available
  pub fn notice (&mut self, content: String) {
    self.message(
      MessageKind::Notice,
      content
    )
  }

  /// Create a user-directed Notice Message in the Source of the TokenStream of a Parser
  /// 
  /// If there is an active marker on the top of the marker stack,
  /// this will pop it and create a SourceRegion from it.
  /// 
  /// If there are no markers in the stack,
  /// the SourceRegion generated will be taken from Parser's current Token
  pub fn notice_pop (&mut self, content: String) {
    self.message_pop(MessageKind::Notice, content)
  }

  /// Create a user-directed Notice Message in the Source of the TokenStream of a Parser,
  /// with a custom line and column origin
  pub fn notice_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Notice, content)
  }
}