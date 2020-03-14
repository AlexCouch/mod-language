//! Contains Token and its component structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  ops::{ Deref, },
};

use crate::{
  source::{ Source, SourceLocation, SourceRegion, },
  ansi,
  common::{ Identifier, Number, Keyword, Operator, },
};


/// An enum indicating a specific variant of interior data in a Token, but not containing any itself
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum TokenKind {
  Invalid,
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
  Invalid,

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
      TokenData::Invalid => TokenKind::Invalid,
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