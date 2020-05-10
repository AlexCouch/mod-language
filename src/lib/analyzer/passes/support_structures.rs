//! Types used in resolution of Aliases: Imports, Exports and (TODO) Typealiases

use crate::{
  common::{ Identifier, },
  source::{ SourceRegion, },
  ctx::{ GlobalKey, },
};

/// Variant data for an Alias
#[allow(missing_docs)]
pub enum AliasData {
  Import { absolute: bool, relative_to: GlobalKey, chain: Vec<Identifier>, },
  Export { base: Identifier, },
}
  
/// A placeholder structure for delayed evaluation of imports and exports
#[allow(missing_docs)]
pub struct Alias {
  pub destination_module: GlobalKey,
  pub data: AliasData,
  pub new_name: Identifier,
  pub origin: SourceRegion,
}

/// Defines an expectation of some aspect of an analysis action
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expect {
  Require,
  Allow,
  Deny,
}

impl Default for Expect { #[inline] fn default () -> Self { Self::Allow } }

/// The result type given by `ty_helpers::ty_meet_n`
pub enum TyMeetResult {
  /// There was a single, viable type which all types could coerce to
  Ok(GlobalKey),
  /// There were multiple viable types which all types could coerce to,
  /// which is unresolvable in the current type system
  Unresolvable,
  /// There was no viable type which all types could coerce to
  None,
}
