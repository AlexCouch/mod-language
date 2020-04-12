//! Structures containing module hierarchy for semantic analysis

use std::{
  ops::{ Deref, DerefMut, },
};

use crate::{
  util::{ make_key_type },
};

use super::{
  namespaces::{ Namespace, },
};


/// The structure used by the semantic analyser to represent a module
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub struct Module {
  pub parent: Option<ModuleKey>,
  pub namespace: Namespace,
}

impl Default for Module {
  fn default () -> Self {
    Self {
      parent: None,
      namespace: Namespace::default(),
    }
  }
}

impl Module {
  /// Create a new Module
  pub fn new (parent: Option<ModuleKey>) -> Self {
    Self {
      parent,
      namespace: Namespace::new(),
    }
  }
}

impl Deref for Module {
  type Target = Namespace;
  #[inline] fn deref (&self) -> &Self::Target { &self.namespace }
}

impl DerefMut for Module {
  #[inline] fn deref_mut (&mut self) -> &mut Self::Target { &mut self.namespace }
}


make_key_type! {
  /// A SlotMap Key represetning a Module
  pub struct ModuleKey;
}