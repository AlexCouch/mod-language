//! Structures containing recursive namespaces

use std::{
  collections::{ HashMap, },
};

use super::{
  types::{ TypeKey, },
  values::{ GlobalKey, FunctionKey, },
  // modules::{ ModuleKey, },
};


/// A recursive table used to associate identifiers with specific items during semantic analysis
#[allow(missing_docs)]
pub struct Namespace {
  // pub modules:   HashMap<String, ModuleKey>,
  pub types:     HashMap<String, TypeKey>,
  pub globals:   HashMap<String, GlobalKey>,
  pub functions: HashMap<String, FunctionKey>,
}

impl Default for Namespace {
  #[inline] fn default () -> Self { Self::new() }
}

impl Namespace {
  /// Create a new Namespace
  pub fn new () -> Self {
    Self {
      // modules: HashMap::new(),
      types: HashMap::new(),
      globals: HashMap::new(),
      functions: HashMap::new(),
    }
  }
}