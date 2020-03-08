//! Contains Token and its component structures

use std::{
  fmt::{ Debug, Formatter, Result as FMTResult, },
  str::from_utf8_unchecked as str_from_utf8_unchecked,
  slice::from_raw_parts as slice_from_raw_parts,
};

use super::source::*;


/// A value identifying a particular language variable or type
pub struct Identifier {
  value: [u8; Self::MAX_LENGTH],
  length: usize,
}

impl Debug for Identifier {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", self.as_ref())
  }
}

impl PartialEq for Identifier {
  fn eq (&self, other: &Self) -> bool {
    if self.length != other.length { return false }

    for i in 0..self.length {
      if self.value[i] != other.value[i] { return false }
    }
    
    true
  }
}

impl Eq for Identifier { }

impl Clone for Identifier {
  fn clone (&self) -> Self {
    Self {
      value: self.value,
      length: self.length,
    }
  }
}

impl Copy for Identifier { }


impl Default for Identifier {
  #[inline] fn default () -> Self { Self::new() }
}

impl Identifier {
  /// The maximum length of bytes an identifier can contain
  pub const MAX_LENGTH: usize = 64;

  /// Create a new, empty Identifier
  pub fn new () -> Self {
    Self { value: [0u8; 64], length: 0 }
  }

  /// Set the value of an Identifier
  pub fn set (&mut self, s: &str) -> bool {
    self.length = 0;

    for ch in s.chars() {
      if ch.is_ascii() {
        self.value[self.length] = ch as u8;
        self.length += 1;
      } else {
        return false;
      }
    }

    true
  }

  /// Append a char to the end of an Identifier if it will fit and is ASCII
  pub fn append (&mut self, c: char) -> bool {
    if self.length < Self::MAX_LENGTH && c.is_ascii() {
      self.value[self.length] = c as u8;
      self.length += 1;
      
      true
    } else {
      false
    }
  }
}

impl AsRef<str> for Identifier {
  fn as_ref (&self) -> &str {
    unsafe { str_from_utf8_unchecked(slice_from_raw_parts(self.value.as_ptr(), self.length)) }
  }
}


/// An enum containing either an Integer or FloatingPoint numeric value
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[allow(missing_docs)]
pub enum Number {
  Integer(u64),
  FloatingPoint(f64),
}


/// An enum representing a language control word such as `fn` or `let`
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Keyword {
  Function,
  Let,
}

/// A lookup table from substrings to their associated Keyword
pub const KEYWORD_VALUES: &[(&str, Keyword)] = &[
  ("fn",  Keyword::Function),
  ("let", Keyword::Let),
];


/// An enum representing a language operator symbol such as `+` or `-`
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Operator {
  Plus,
  Minus,
  Asterisk,
  ForwardSlash,
  Comma,
  Colon,
  SemiColon,
  LeftParenthesis,
  RightParenthesis,
  LeftBracket,
  RightBracket,
}

/// A lookup table from substrings to their associated Operator
pub const OPERATOR_VALUES: &[(&str, Operator)] = &[
  ("+", Operator::Plus),
  ("-", Operator::Minus),
  ("*", Operator::Asterisk),
  ("/", Operator::ForwardSlash),
  (",", Operator::Comma),
  (":", Operator::Colon),
  (";", Operator::SemiColon),
  ("(", Operator::LeftParenthesis),
  (")", Operator::RightParenthesis),
  ("{", Operator::LeftBracket),
  ("}", Operator::RightBracket),
];


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




/// A single unit of language syntax, such as an identifier, a number, or an operator
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Token {
  pub data: TokenData,
  pub origin: SourceOrigin,
}

impl Token {
  /// Create a new Token
  pub fn new (data: TokenData, origin: SourceOrigin) -> Self {
    Self { data, origin }
  }
}