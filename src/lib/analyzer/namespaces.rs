//! Structures containing recursive namespaces

use std::{
  collections::{ HashMap, },
  ops::{ Deref, DerefMut, },
};

use super::{
  AnalysisKey,
};



/// A recursive table used to associate identifiers with specific items during semantic analysis
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub struct Namespace (HashMap<String, AnalysisKey>);


impl Default for Namespace {
  #[inline] fn default () -> Self { Self::new() }
}

impl Namespace {
  /// Create a new Namespace
  pub fn new () -> Self {
    Self(HashMap::default())
  }
}

impl Deref for Namespace {
  type Target = HashMap<String, AnalysisKey>;
  fn deref (&self) -> &Self::Target { &self.0 }
}

impl DerefMut for Namespace {
  fn deref_mut (&mut self) -> &mut Self::Target { &mut self.0 }
}