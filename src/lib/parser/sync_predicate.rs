//! The SyncPredicate trait and various SyncPredicates and builders

use crate::{
  token::{ Token, TokenKind, TokenData, Operator, Keyword },
};


/// Marks a predicate function or closure for use with a Parser's error re-synchronization method
pub trait SyncPredicate {
  /// Determine if a Token is an acceptable point of synchronization after an error is encountered in a Parser
  fn sync (&mut self, token: &Token) -> bool;
}

impl<F> SyncPredicate for F
where F: FnMut (&Token) -> bool
{
  #[inline] fn sync (&mut self, token: &Token) -> bool { self(token) }
}


/// Construct a SyncPredicate that looks for any TokenData matching a specific TokenKind
pub fn token_kind (kind: TokenKind) -> impl SyncPredicate {
  move |token: &Token| token.kind() == kind
}

/// Construct a SyncPredicate that looks for a particular TokenData value
pub fn token_data (data: TokenData) -> impl SyncPredicate {
  move |token: &Token| token.data == data
}

/// Construct a SyncPredicate that looks for any value from a set of TokenData
pub fn any_token_data_of (data: &'static [TokenData]) -> impl SyncPredicate {
  move |token: &Token| data.contains(&token.data)
}


/// Construct a SyncPredicate that looks for a particular Operator Token
pub fn operator (op: Operator) -> impl SyncPredicate {
  token_data(TokenData::Operator(op))
}

/// Construct a SyncPredicate that looks for any Operator from a set
pub fn any_operator_of (operators: &'static [Operator]) -> impl SyncPredicate {
  move |token: &Token| if let TokenData::Operator(operator) = &token.data { operators.contains(operator) } else { false }
}


/// Construct a SyncPredicate that looks for a particular Keyword Token
pub fn keyword (kw: Keyword) -> impl SyncPredicate {
  token_data(TokenData::Keyword(kw))
}

/// Construct a SyncPredicate that looks for any Keyword from a set
pub fn any_keyword_of (keywords: &'static [Keyword]) -> impl SyncPredicate {
  move |token: &Token| if let TokenData::Keyword(keyword) = &token.data { keywords.contains(keyword) } else { false }
}


/// Construct a SyncPredicate that combines two other SyncPredicates,
/// in order to match pairs of values until a counter of pairs reaches 0
/// 
/// The counter starts at 1, implicitly assuming that the first pair is already open
/// 
/// This is useful for e.g. sychronizing on the end of a pair of brackets
pub fn close_pair<Pl, Pr> (mut left: Pl, mut right: Pr) -> impl SyncPredicate
where Pl: SyncPredicate,
      Pr: SyncPredicate,
{
  let mut open_pair_count = 1;

  move |token: &Token| {
    if left.sync(token) { open_pair_count += 1 }
    else if right.sync(token) { open_pair_count -= 1 }

    open_pair_count == 0
  }
}

/// Construct a SyncPredicate that combines two other SyncPredicates,
/// in order to match pairs of values until a counter of pairs reaches 0,
/// and unlike `close_pair` this function utilizes a third subordinate SyncPredicate,
/// and will accept its choice for synchronization if there is only the original pair open
/// 
/// The counter starts at 1, implicitly assuming that the first pair is already open
/// 
/// This is useful for e.g. synchronizing on either the end of a bracket pair,
/// or a comma separator inside them
pub fn close_pair_or<Pl, Pr, Pi> (mut left: Pl, mut right: Pr, mut interior: Pi) -> impl SyncPredicate
where Pl: SyncPredicate,
      Pr: SyncPredicate,
      Pi: SyncPredicate,
{
  let mut open_pair_count = 1;

  move |token: &Token| {
    if left.sync(token) { open_pair_count += 1 }
    else if right.sync(token) { open_pair_count -= 1 }

    open_pair_count == 0 || (open_pair_count == 1 && interior.sync(token))
  }
}