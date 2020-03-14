//! The SyncPredicate trait and various SyncPredicates and builders

use crate::{
  token::{ Token, TokenKind, TokenData, Operator, Keyword, },
};


/// Marks a predicate function or closure for use with a Parser's error re-synchronization method
pub unsafe trait SyncPredicate {
  /// Determine if a Token is an acceptable point of synchronization after an error is encountered in a Parser
  /// # Safety
  /// This should not be called directly; instead, pass this SyncPredicate to a Parser synchronization method call
  unsafe fn sync (&mut self, token: &Token) -> bool;
}

unsafe impl<F> SyncPredicate for F
where F: FnMut (&Token) -> bool
{
  #[inline]
  unsafe fn sync (&mut self, token: &Token) -> bool { self(token) }
}


/// Construct a SyncPredicate that looks for any TokenData matching a specific TokenKind
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn token_kind (kind: TokenKind) -> impl SyncPredicate {
  move |token: &Token| token.kind() == kind
}

/// Construct a SyncPredicate that looks for a particular TokenData value
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn token_data (data: TokenData) -> impl SyncPredicate {
  move |token: &Token| token.data == data
}

/// Construct a SyncPredicate that looks for any value from a set of TokenData
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn any_token_data_of (data: &'static [TokenData]) -> impl SyncPredicate {
  move |token: &Token| data.contains(&token.data)
}


/// Construct a SyncPredicate that looks for a particular Operator Token
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn operator (op: Operator) -> impl SyncPredicate {
  token_data(TokenData::Operator(op))
}

/// Construct a SyncPredicate that looks for any Keyword
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn any_operator () -> impl SyncPredicate {
  move |token: &Token| matches!(token, Token { data: TokenData::Operator(_), .. })
}

/// Construct a SyncPredicate that looks for any Operator from a set
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn any_operator_of (operators: &'static [Operator]) -> impl SyncPredicate {
  move |token: &Token| token.is_any_operator_of(operators).is_some()
}


/// Construct a SyncPredicate that looks for a particular Keyword Token
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn keyword (kw: Keyword) -> impl SyncPredicate {
  token_data(TokenData::Keyword(kw))
}

/// Construct a SyncPredicate that looks for any Keyword
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn any_keyword () -> impl SyncPredicate {
  move |token: &Token| matches!(token, Token { data: TokenData::Keyword(_), .. })
}

/// Construct a SyncPredicate that looks for any Keyword from a set
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn any_keyword_of (keywords: &'static [Keyword]) -> impl SyncPredicate {
  move |token: &Token| token.is_any_keyword_of(keywords).is_some()
}


/// Construct a SyncPredicate that implements the logical or of two subordinate SyncPredicates
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn or<Pl, Pr> (mut left: Pl, mut right: Pr) -> impl SyncPredicate 
where Pl: SyncPredicate,
      Pr: SyncPredicate,
{
  move |token: &Token| unsafe { left.sync(token) || right.sync(token) }
}

/// Construct a SyncPredicate that implements the logical and of two subordinate SyncPredicates
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn and<Pl, Pr> (mut left: Pl, mut right: Pr) -> impl SyncPredicate 
where Pl: SyncPredicate,
      Pr: SyncPredicate,
{
  move |token: &Token| unsafe { left.sync(token) && right.sync(token) }
}


/// Construct a SyncPredicate that combines two other SyncPredicates,
/// in order to match pairs of values until a counter of pairs reaches 0
/// 
/// The counter starts at 1, implicitly assuming that the first pair is already open
/// 
/// This is useful for e.g. sychronizing on the end of a pair of brackets
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn close_pair<Pl, Pr> (mut left: Pl, mut right: Pr) -> impl SyncPredicate
where Pl: SyncPredicate,
      Pr: SyncPredicate,
{
  let mut open_pair_count = 1;

  move |token: &Token| {
    if unsafe { left.sync(token) } { open_pair_count += 1 }
    else if unsafe { right.sync(token) } { open_pair_count -= 1 }

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
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn close_pair_or<Pl, Pr, Pi> (mut left: Pl, mut right: Pr, mut interior: Pi) -> impl SyncPredicate
where Pl: SyncPredicate,
      Pr: SyncPredicate,
      Pi: SyncPredicate,
{
  let mut open_pair_count = 1;

  move |token: &Token| {
    if unsafe { left.sync(token) } { open_pair_count += 1 }
    else if unsafe { right.sync(token) } { open_pair_count -= 1 }

    open_pair_count == 0 || (open_pair_count == 1 && unsafe { interior.sync(token) })
  }
}


/// Construct a SyncPredicate that combines two other SyncPredicates,
/// in order to produce an interior region where a third SyncPredicate will not be used
/// 
/// This is the opposite of `close_pair_or` in that it's internal pair counter starts at 0,
/// and the third exterior SyncPredicate is only called while it is at 0
/// 
/// This is useful for e.g. synchronizing on symbols while not inside a pair, where
/// you do not want symbols inside subsequent pairs to be accepted
/// `we are here: (and dont want to synchronize here: ,) but do want to synchronize here: ,`
#[must_use = "sync_predicate::* functions return a synchronization predicate that must be passed to the Parser in a synchronization call, and do not perform the synchronization themselves"]
pub fn external<Pl, Pr, Pe> (mut left: Pl, mut right: Pr, mut interior: Pe) -> impl SyncPredicate
where Pl: SyncPredicate,
      Pr: SyncPredicate,
      Pe: SyncPredicate,
{
  let mut open_pair_count = 0;

  move |token: &Token| {
    if unsafe { left.sync(token) } { open_pair_count += 1 }
    else if unsafe { right.sync(token) } { open_pair_count -= 1 }

    open_pair_count == 0 && unsafe { interior.sync(token) }
  }
}