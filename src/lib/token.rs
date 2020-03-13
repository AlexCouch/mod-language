//! Contains Token and its component structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  str::from_utf8_unchecked as str_from_utf8_unchecked,
  slice::Iter as SliceIter,
  ops::{ Deref, },
};

use crate::{
  source::{ Source, SourceLocation, SourceRegion, },
  ansi,
  util::Either,
};


/// A value identifying a particular language variable or type
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier {
  vec: Vec<u8>,
}

impl Display for Identifier {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", self.as_ref())
  }
}

impl Debug for Identifier {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Display::fmt(self, f)
  }
}

impl Default for Identifier {
  #[inline] fn default () -> Self { Self::new() }
}

/// An iterator over the bytes of an Identifier as chars
pub struct IdentifierChars<'a> {
  identifier: &'a Identifier,
  index: usize,
}

impl<'a> Iterator for IdentifierChars<'a> {
  type Item = char;
  fn next (&mut self) -> Option<Self::Item> {
    let ch = self.identifier.get(self.index);
    if ch.is_some() { self.index += 1 }
    ch
  }
}

impl Identifier {
  /// The maximum length of bytes an identifier can contain
  pub const MAX_LENGTH: usize = 64;

  /// Create a new, empty Identifier
  pub fn new () -> Self {
    Self { vec: Vec::new() }
  }

  /// Get the length in chars/bytes of an Identifier
  #[inline]
  pub fn len (&self) -> usize {
    self.vec.len()
  }

  /// Determine if an Identifier contains no bytes/chars
  #[inline]
  pub fn is_empty (&self) -> bool {
    self.len() == 0
  }

  /// Set the value of an Identifier
  pub fn set (&mut self, s: &str) -> bool {
    if s.len() > Self::MAX_LENGTH { return false }

    for ch in s.chars() {
      if !ch.is_ascii() { return false }
    }

    self.vec.clear();

    for ch in s.chars() {
      self.vec.push(ch as u8);
    }

    true
  }

  /// Append a char to the end of an Identifier if it will fit and is ASCII
  pub fn append (&mut self, c: char) -> bool {
    if self.len() < Self::MAX_LENGTH && c.is_ascii() {
      self.vec.push(c as u8);
      
      true
    } else {
      false
    }
  }

  /// Get a specific byte at an index in an Identifier and convert it to `char`
  pub fn get (&self, index: usize) -> Option<char> {
    self.vec.get(index).map(|ch| *ch as _)
  }

  /// Get an iterator over the chars of an Identifier
  pub fn char_iter (&self) -> IdentifierChars<'_> {
    IdentifierChars { identifier: self, index: 0 }
  }

  /// Get an iterator over the bytes of an Identifier
  pub fn byte_iter (&self) -> SliceIter<u8> {
    self.vec.iter()
  }
}

impl AsRef<str> for Identifier {
  fn as_ref (&self) -> &str {
    unsafe { str_from_utf8_unchecked(self.vec.as_slice()) }
  }
}

impl From<&str> for Identifier {
  fn from (s: &str) -> Self {
    let mut i = Self::new();
    i.set(s);
    i
  }
}


/// An enum containing either an Integer or FloatingPoint numeric value
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[allow(missing_docs)]
pub enum Number {
  Integer(u64),
  FloatingPoint(f64),
}

impl From<u64> for Number {
  #[inline]
  fn from (i: u64) -> Self {
    Number::Integer(i)
  }
}

impl From<f64> for Number {
  #[inline]
  fn from (f: f64) -> Self {
    Number::FloatingPoint(f)
  }
}


/// An enum representing a language control word such as `fn` or `let`
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Keyword {
  Global,
  Else,
  Let,
  Function,
  If,
}

impl Keyword {
  /// Get the textual value of a Keyword
  pub fn value (self) -> &'static str {
    use Keyword::*;

    match self {
      Global   => "global",
      Else     => "else",
      Let      => "let",
      Function => "fn",
      If       => "if",
    }
  }
}


/// An enum representing a language operator symbol such as `+` or `-`
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Operator {
  Not,
  And,
  Xor,
  Or,
  As,

  RightArrow,

  AssignAdd,
  AssignSub,
  AssignMul,
  AssignDiv,
  AssignRem,

  Equal,

  Assign,

  Add,
  Sub,
  Mul,
  Div,
  Rem,

  Comma,
  Colon,
  Semi,

  LeftParen,
  RightParen,

  LeftBracket,
  RightBracket,
}

impl Operator {
  /// Get the textual value of an Operator
  pub fn value (self) -> &'static str {
    use Operator::*;

    match self {
      Not => "not",
      And => "and",
      Xor => "xor",
      Or  => "or",
      As  => "as",

      RightArrow => "->",
    
      AssignAdd => "+=",
      AssignSub => "-=",
      AssignMul => "*=",
      AssignDiv => "/=",
      AssignRem => "%=",
  
      Equal => "==",
  
      Assign => "=", 
  
      Add => "+", 
      Sub => "-", 
      Mul => "*", 
      Div => "/", 
      Rem => "%", 
  
      Comma => ",", 
      Colon => ":", 
      Semi => ";", 
  
      LeftParen => "(", 
      RightParen => ")", 
      
      LeftBracket => "{", 
      RightBracket => "}", 
    }
  }
}


/// A lookup table from substrings to their associated Keyword or Operator
/// 
/// Note that values are stored in order of longest to shortest in order to facilitate the lexer's matching system
pub const IDENTIFIER_VALUES: &[(&str, Either<Keyword, Operator>)] = {
  &[
    ("global", Either::A(Keyword::Global)),
    ("else",   Either::A(Keyword::Else)),
    ("let",    Either::A(Keyword::Let)),
    ("not",    Either::B(Operator::Not)),
    ("and",    Either::B(Operator::And)),
    ("xor",    Either::B(Operator::Xor)),
    ("fn",     Either::A(Keyword::Function)),
    ("if",     Either::A(Keyword::If)),
    ("or",     Either::B(Operator::Or)),
    ("as",     Either::B(Operator::As)),
  ]
};


/// A lookup table from substrings of symbols to their associated Operator
/// 
/// E.g. only contains operators which cannot be interpreted as an identifer
/// 
/// Note that values are stored in order of longest to shortest in order to facilitate the lexer's matching system
pub const SYM_OPERATOR_VALUES: &[(&str, Operator)] = {
  use Operator::*;
  &[
    ("->", RightArrow),
    
    ("+=", AssignAdd),
    ("-=", AssignSub),
    ("*=", AssignMul),
    ("/=", AssignDiv),
    ("%=", AssignRem),

    ("==", Equal),

    ("=",  Assign),

    ("+",  Add),
    ("-",  Sub),
    ("*",  Mul),
    ("/",  Div),
    ("%",  Rem),

    (",",  Comma),
    (":",  Colon),
    (";",  Semi),

    ("(",  LeftParen),
    (")",  RightParen),

    ("{",  LeftBracket),
    ("}",  RightBracket),
  ]
};


/// An enum indicating a specific variant of interior data in a Token, but not containing any itself
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum TokenKind {
  Identifier,
  Number,
  String,
  Keyword,
  Operator
}


/// An enum containing the interior data of a Token, such as an Identifier, Number, String, Keyword, or other variant
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum TokenData {
  Identifier(Identifier),
  Number(Number),
  String(String),
  Keyword(Keyword),
  Operator(Operator),
}

impl TokenData {
  /// Get the TokenKind of a TokenData
  pub fn kind (&self) -> TokenKind {
    match self {
      TokenData::Identifier(_) => TokenKind::Identifier,
      TokenData::Number(_) => TokenKind::Number,
      TokenData::String(_) => TokenKind::String,
      TokenData::Keyword(_) => TokenKind::Keyword,
      TokenData::Operator(_) => TokenKind::Operator,
    }
  }

  /// Determine if a TokenData is an Operator of a specific kind
  pub fn is_operator (&self, operator: Operator) -> bool {
    *self == TokenData::Operator(operator)
  }

  /// Determine if a TokenData is a Keyword of a specific kind
  pub fn is_keyword (&self, keyword: Keyword) -> bool {
    *self == TokenData::Keyword(keyword)
  }

  /// Determine if a TokenData is any Operator of a given set
  /// 
  /// Returns the Operator that matched or None
  pub fn is_any_operator_of (&self, operators: &[Operator]) -> Option<Operator> {
    if let TokenData::Operator(operator) = self {
      for matched_operator in operators.iter() {
        if matched_operator == operator {
          return Some(*operator)
        }
      }
    }

    None
  }

  /// Determine if a TokenData is any Keyword of a given set
  /// 
  /// Returns the Keyword that matched or None
  pub fn is_any_keyword_of (&self, keywords: &[Keyword]) -> Option<Keyword> {
    if let TokenData::Keyword(keyword) = self {
      for matched_keyword in keywords.iter() {
        if matched_keyword == keyword {
          return Some(*keyword)
        }
      }
    }

    None
  }
}



/// A single unit of language syntax, such as an identifier, a number, or an operator
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Token {
  pub data: TokenData,
  pub origin: SourceRegion,
}

impl PartialEq for Token {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Token {
  /// Create a new Token
  pub fn new (data: TokenData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Token with no SourceRegion origin
  pub fn no_src (data: TokenData) -> Self {
    Self { data, origin: SourceLocation::ZERO.to_region() }
  }
}

impl Deref for Token {
  type Target = TokenData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}


/// Contains a Vec of Tokens and a reference to the Source they were generated from
pub struct TokenStream<'a> {
  tokens: Vec<Token>,
  /// The source the Tokens in a TokenStream originated from
  pub source: &'a Source
}

impl<'a> TokenStream<'a> {
  /// Create a new TokenStream
  pub fn new (tokens: Vec<Token>, source: &'a Source) -> Self {
    Self {
      tokens,
      source,
    }
  }

  /// Get a slice of the Tokens in a TokenStream
  pub fn tokens (&self) -> &[Token] {
    self.tokens.as_slice()
  }
}

impl<'a> Display for TokenStream<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    writeln!(f, "TokenStream({}) [", self.tokens.len())?;

    for (i, token) in self.tokens.iter().enumerate() {
      writeln!(
        f,
        "  {} @ [{}{}:{:?}{} to {}{}:{:?}{}]: {:?}",
        i,
        ansi::Foreground::Cyan,
        self.source.path,
        token.origin.start,
        ansi::Foreground::Reset,
        ansi::Foreground::Cyan,
        self.source.path,
        token.origin.end,
        ansi::Foreground::Reset,
        token.data
      )?;
    }

    writeln!(f, "]")
  }
}