//! Contains Token and its component structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  ops::{ Deref, },
};

use mod_common::{ Identifier, Constant, Keyword, Operator, IdentifierValue, };

use crate::{
  source::{ SourceRegion, },
};


/// An enum indicating a specific variant of interior data in a Token, but not containing any itself
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum TokenKind {
  Invalid,
  Identifier,
  Constant,
  Keyword,
  Operator
}


/// An enum containing the interior data of a Token, such as an Identifier, Number, String, Keyword, or other variant
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum TokenData {
  Invalid,

  Identifier(Identifier),
  Constant(Constant),
  Keyword(Keyword),
  Operator(Operator),
}

impl TokenData {
  /// Get the TokenKind of a TokenData
  pub fn kind (&self) -> TokenKind {
    match self {
      TokenData::Invalid => TokenKind::Invalid,
      TokenData::Identifier(_) => TokenKind::Identifier,
      TokenData::Constant(_) => TokenKind::Constant,
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

impl From<&IdentifierValue> for TokenData {
  fn from (iv: &IdentifierValue) -> TokenData {
    match iv {
      IdentifierValue::Constant(c) => TokenData::Constant(c.clone()),
      IdentifierValue::Keyword(k) => TokenData::Keyword(*k),
      IdentifierValue::Operator(o) => TokenData::Operator(*o),
    }
  }
}



/// A single unit of language syntax, such as an identifier, a number, or an operator
#[derive(Clone)]
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
    Self { data, origin: SourceRegion::ANONYMOUS }
  }
}

impl Deref for Token {
  type Target = TokenData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl Debug for Token {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "[{:?}]: {:?}",
      self.origin,
      self.data
    )
  }
}

impl Display for Token {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "[{}]: {:?}",
      self.origin,
      self.data
    )
  }
}