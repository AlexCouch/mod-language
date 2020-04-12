//! Structures containing module hierarchy for semantic analysis

use std::{
  collections::{ HashMap, },
  ops::{ Deref, DerefMut, },
};

use super::{
  items::{ AnalysisKey, },
};


/// A wrapper for an Item in a Module, binding an AnalysisKey with an optional import source key
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ModuleItem {
  pub import: Option<AnalysisKey>,
  pub key: AnalysisKey,
}

/// The structure used by the semantic analyser to represent a module
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub struct Module {
  pub parent: Option<AnalysisKey>,
  pub name: String,
  pub items: HashMap<String, ModuleItem>,
}

impl Module {
  /// Create a new Module
  pub fn new (parent: Option<AnalysisKey>, name: String) -> Self {
    Self {
      parent,
      name,
      items: HashMap::new(),
    }
  }
}

impl Deref for Module {
  type Target = HashMap<String, ModuleItem>;
  #[inline] fn deref (&self) -> &Self::Target { &self.items }
}

impl DerefMut for Module {
  #[inline] fn deref_mut (&mut self) -> &mut Self::Target { &mut self.items }
}