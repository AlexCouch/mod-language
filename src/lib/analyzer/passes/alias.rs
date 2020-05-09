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