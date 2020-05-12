//! Types used in resolution of Aliases: Imports, Exports and (TODO) Typealiases

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
};

use crate::{
  common::{ Identifier, },
  source::{ SourceRegion, },
  ctx::{ ContextKey, },
};

/// Variant data for an Alias
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AliasKind {
  Import,
  Export,
}

impl Display for AliasKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self { AliasKind::Import => "Import", AliasKind::Export => "Export" })
  }
}
  
/// A placeholder structure for delayed evaluation of imports and exports
#[allow(missing_docs)]
pub struct Alias {
  pub destination_namespace: ContextKey,
  pub kind: AliasKind,
  pub new_name: Identifier,
  pub absolute: bool,
  pub relative_to: ContextKey,
  pub chain: Vec<Identifier>,
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
  Ok(ContextKey),
  /// There were multiple viable types which all types could coerce to,
  /// which is unresolvable in the current type system
  Unresolvable,
  /// There was no viable type which all types could coerce to
  None,
}
