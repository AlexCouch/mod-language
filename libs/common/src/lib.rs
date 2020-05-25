//! Implementation details common to multiple mod_language components

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(clippy::useless_let_if_seq)]

#![feature(const_loop)]
#![feature(const_if_match)]

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  str::{ from_utf8_unchecked as str_from_utf8_unchecked, },
  slice::{ Iter as SliceIter, },
  cmp::{ Ordering, },
  hash::{ Hash, Hasher, },
  borrow::{ Borrow, },
};


/// A trait like `Display` for use with items that need to track hierarchical level (eg for indentation (see `Padding`))
pub trait HierarchicalDisplay {
  /// Display a value while tracking hierarchy level (eg for indentation)
  fn fmt_hierarchical (&self, f: &mut Formatter, level: &mut usize) -> FMTResult;
}

/// A simple interface unit structure for printing indentation in a HierarchicalDisplay implementation
pub struct Padding;

impl HierarchicalDisplay for Padding {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: &mut usize) -> FMTResult {
    for _ in 0..*level {
      write!(f, "  ")?;
    }

    Ok(())
  }
}

/// Represents a `Module`'s semver2 version number
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Version {
  /// Bumped for incompatible API changes
  pub major: u8,
  /// Bumped for added functionality in a backwards compatible manner
  pub minor: u8,
  /// Bumped for backwards compatible bug fixes
  pub patch: u8,
}

impl Version {
  /// Create a new `Version` and initialize its values
  pub fn new (major: u8, minor: u8, patch: u8) -> Self {
    Self { major, minor, patch }
  }
}

impl From<(u8, u8, u8)> for Version { fn from ((major, minor, patch): (u8, u8, u8)) -> Self { Self { major, minor, patch } } }
impl From<Version> for (u8, u8, u8) { fn from (Version { major, minor, patch }: Version) -> Self { (major, minor, patch) } }

impl HierarchicalDisplay for Version {
  fn fmt_hierarchical (&self, f: &mut Formatter, _level: &mut usize) -> FMTResult {
    write!(f, "(version {} {} {})", self.major, self.minor, self.patch)
  }
}

impl Display for Version { fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, &mut 0) } }



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

impl PartialEq<str> for Identifier {
  #[inline] fn eq (&self, other: &str) -> bool { self.as_ref() == other }
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

/// An enum representing a floating point constant
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum FloatingPoint {
  /// Not a Number
  NaN,
  /// A normal number value
  Norm(f64),
  /// Infinity
  Inf,
}

impl Display for FloatingPoint {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    match self {
      FloatingPoint::NaN => write!(f, "nan"),
      FloatingPoint::Norm(norm) => write!(f, "{}", norm),
      FloatingPoint::Inf => write!(f, "inf"),
    }
  }
}

impl From<f64> for FloatingPoint {
  fn from (f: f64) -> FloatingPoint {
    if f.is_infinite() {
      FloatingPoint::Inf
    } else if f.is_nan() {
      FloatingPoint::NaN
    } else {
      FloatingPoint::Norm(f)
    }
  }
}

/// An enum containing either an Integer or FloatingPoint numeric value
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
  /// An integer whole number
  Integer(u64),
  /// A floating point real or non-normal number
  FloatingPoint(FloatingPoint),
}

impl Display for Number {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    match self {
      Number::Integer(int) => Display::fmt(int, f),
      Number::FloatingPoint(float) => Display::fmt(float, f),
    }
  }
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
    Number::FloatingPoint(f.into())
  }
}

/// A constant literal value
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Constant {
  /// The 0-address pointer
  NullPointer,
  /// A numeric literal
  Number(Number),
  /// A boolean literal
  Bool(bool),
  /// A string literal
  String(String),
}

impl Display for Constant {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    match self {
      Constant::NullPointer => write!(f, "null"),
      Constant::Number(number) => Display::fmt(number, f),
      Constant::Bool(bool) => Display::fmt(bool, f),
      Constant::String(string) => write!(f, "\"{}\"", string),
    }
  }
}

impl<T: Into<Number>> From<T> for Constant {
  fn from (num: T) -> Constant { Constant::Number(num.into()) }
}

impl From<bool> for Constant {
  fn from (bool: bool) -> Constant { Constant::Bool(bool) }
}

impl From<String> for Constant {
  fn from (string: String) -> Constant { Constant::String(string) }
}


/// An enum representing a language control word such as `fn` or `let`
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Keyword {
  Import,
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
  Return,
}

impl Keyword {
  /// Get the textual value of a Keyword
  pub fn value (self) -> &'static str {
    use Keyword::*;

    match self {
      Import    => "import",
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
      Return    => "return",
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

/// A wrapper enum for built in identifier values
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum IdentifierValue {
  Keyword(Keyword),
  Operator(Operator),
  Constant(Constant),
}

/// A lookup table from substrings to their associated Keyword or Operator
/// 
/// Note that values are stored in order of longest to shortest in order to facilitate the lexer's matching system
pub const IDENTIFIER_VALUES: &[(&str, IdentifierValue)] = {
  &[
    ("return", IdentifierValue::Keyword(Keyword::Return)),
    ("import", IdentifierValue::Keyword(Keyword::Import)),
    ("export", IdentifierValue::Keyword(Keyword::Export)),
    ("global", IdentifierValue::Keyword(Keyword::Global)),
    ("struct", IdentifierValue::Keyword(Keyword::Struct)),
    ("alias",  IdentifierValue::Keyword(Keyword::Alias)),
    ("false",  IdentifierValue::Constant(Constant::Bool(false))),
    ("true",   IdentifierValue::Constant(Constant::Bool(true))),
    ("type",   IdentifierValue::Keyword(Keyword::Type)),
    ("else",   IdentifierValue::Keyword(Keyword::Else)),
    ("null",   IdentifierValue::Constant(Constant::NullPointer)),
    ("nan",    IdentifierValue::Constant(Constant::Number(Number::FloatingPoint(FloatingPoint::NaN)))),
    ("inf",    IdentifierValue::Constant(Constant::Number(Number::FloatingPoint(FloatingPoint::Inf)))),
    ("let",    IdentifierValue::Keyword(Keyword::Let)),
    ("not",    IdentifierValue::Operator(Operator::Not)),
    ("and",    IdentifierValue::Operator(Operator::And)),
    ("xor",    IdentifierValue::Operator(Operator::Xor)),
    ("ns",     IdentifierValue::Keyword(Keyword::Namespace)),
    ("fn",     IdentifierValue::Keyword(Keyword::Function)),
    ("if",     IdentifierValue::Keyword(Keyword::If)),
    ("or",     IdentifierValue::Operator(Operator::Or)),
    ("as",     IdentifierValue::Operator(Operator::As)),
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
    Import,
    Namespace,
    Alias,
    Export,
    Struct,
    Type,
    Global,
    Function,
  ]
};

/// The operator precedence used by all unary operators consuming an unbounded subexpression
pub const UNARY_PRECEDENCE: usize = 70;

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