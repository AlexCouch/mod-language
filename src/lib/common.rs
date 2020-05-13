//! Implementation details common to both lexer and parser

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  str::from_utf8_unchecked as str_from_utf8_unchecked,
  slice::Iter as SliceIter,
  cmp::Ordering,
  hash::{ Hash, Hasher, },
  borrow::{ Borrow, },
};


use crate::{
  util::Either,
};


/// A value identifying a particular language variable or type
#[derive(Clone)]
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

impl PartialEq for Identifier {
  #[inline] fn eq (&self, other: &Self) -> bool { self.as_ref() == other.as_ref() }
}

impl Eq for Identifier { }

impl PartialOrd for Identifier {
  #[inline] fn partial_cmp (&self, other: &Self) -> Option<Ordering> { self.as_ref().partial_cmp(other.as_ref()) }
}

impl Ord for Identifier {
  #[inline] fn cmp (&self, other: &Self) -> Ordering { self.as_ref().cmp(other.as_ref()) }
}


impl Hash for Identifier {
  #[inline] fn hash<H: Hasher> (&self, hasher: &mut H) { self.as_ref().hash(hasher) }
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
  pub fn set<S: AsRef<str>> (&mut self, s: &S) -> bool {
    let s = s.as_ref();

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

  /// Get an &str pointing into an Identifier
  pub fn as_str (&self) -> &str {
    self.as_ref()
  }
}

impl AsRef<str> for Identifier {
  #[inline] fn as_ref (&self) -> &str { unsafe { str_from_utf8_unchecked(self.vec.as_slice()) } }
}

impl From<&str> for Identifier {
  #[inline]
  fn from (s: &str) -> Self {
    let mut i = Self::new();
    i.set(&s);
    i
  }
}

impl Into<String> for Identifier {
  #[inline] fn into (self) -> String { self.as_ref().to_owned() }
}

impl Into<String> for &Identifier {
  #[inline] fn into (self) -> String { self.as_ref().to_owned() }
}

impl Borrow<str> for Identifier {
  #[inline] fn borrow (&self) -> &str { self.as_ref() }
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
  Alias,
  Export,
  Namespace,
  Global,
  Struct,
  Type,
  Function,
  If,
  Else,
  Let,
}

impl Keyword {
  /// Get the textual value of a Keyword
  pub fn value (self) -> &'static str {
    use Keyword::*;

    match self {
      Alias     => "alias",
      Export    => "export",
      Namespace => "ns",
      Struct    => "struct",
      Type      => "type",
      Global    => "global",
      Function  => "fn",
      If        => "if",
      Else      => "else",
      Let       => "let",
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

  DoubleColon,
  RightArrow,

  AssignAdd,
  AssignSub,
  AssignMul,
  AssignDiv,
  AssignRem,

  Equal,
  NotEqual,
  GreaterOrEqual,
  LesserOrEqual,
  Greater,
  Lesser,

  Assign,

  Add,
  Sub,
  Mul,
  Div,
  Rem,

  AddressOf,
  Dereference,

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

      DoubleColon => "::",
      RightArrow => "->",
    
      AssignAdd => "+=",
      AssignSub => "-=",
      AssignMul => "*=",
      AssignDiv => "/=",
      AssignRem => "%=",
  
      Equal => "==",
      NotEqual => "!=",
      GreaterOrEqual => ">=",
      LesserOrEqual => "<=",
      Greater => ">",
      Lesser => "<",
  
      Assign => "=", 
  
      Add => "+", 
      Sub => "-", 
      Mul => "*", 
      Div => "/", 
      Rem => "%", 

      AddressOf => "^",
      Dereference => "@",
  
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
    ("export", Either::A(Keyword::Export)),
    ("global", Either::A(Keyword::Global)),
    ("struct", Either::A(Keyword::Struct)),
    ("alias",  Either::A(Keyword::Alias)),
    ("type",   Either::A(Keyword::Type)),
    ("else",   Either::A(Keyword::Else)),
    ("let",    Either::A(Keyword::Let)),
    ("not",    Either::B(Operator::Not)),
    ("and",    Either::B(Operator::And)),
    ("xor",    Either::B(Operator::Xor)),
    ("ns",     Either::A(Keyword::Namespace)),
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
    ("::", DoubleColon),
    ("->", RightArrow),
    
    ("+=", AssignAdd),
    ("-=", AssignSub),
    ("*=", AssignMul),
    ("/=", AssignDiv),
    ("%=", AssignRem),

    ("==", Equal),
    ("!=", NotEqual),
    (">=", GreaterOrEqual),
    ("<=", LesserOrEqual),
    (">", Greater),
    ("<", Lesser),

    ("=",  Assign),

    ("+",  Add),
    ("-",  Sub),
    ("*",  Mul),
    ("/",  Div),
    ("%",  Rem),
    
    ("^", AddressOf),
    ("@", Dereference),

    (",",  Comma),
    (":",  Colon),
    (";",  Semi),

    ("(",  LeftParen),
    (")",  RightParen),

    ("{",  LeftBracket),
    ("}",  RightBracket),
  ]
};

// TODO relocate this
/// Keywords that define the start of a Statement
pub const STATEMENT_KEYWORDS: &[Keyword] = {
  use Keyword::*;

  &[
    Let,
    If,
  ]
};

/// Keywords that define the start of an Item
pub const ITEM_KEYWORDS: &[Keyword] = {
  use Keyword::*;

  &[
    Namespace,
    Alias,
    Export,
    Struct,
    Type,
    Global,
    Function,
  ]
};

/// A lookup table for Pratt operator precedences of binary operators
pub const BINARY_PRECEDENCES: &[(Operator, usize)] = {
  use Operator::*;
  
  &[
    (And, 20),
    (Or, 20),
    (Xor, 20),
    
    (Equal, 30),
    (NotEqual, 30),
    (Lesser, 30),
    (Greater, 30),
    (LesserOrEqual, 30),
    (GreaterOrEqual, 30),

    (Add, 50),
    (Sub, 50),
    
    (Mul, 60),
    (Div, 60),
    (Rem, 60),

    (LeftParen, 70),
  ]
};

/// Use a lookup table to get the Pratt operator precedence of a binary operator
pub const fn get_binary_precedence (operator: Operator) -> usize {
  let mut i = 0;

  loop {
    let (op, prec) = BINARY_PRECEDENCES[i];

    if op as u8 == operator as u8 { return prec }

    i += 1;
  }
}