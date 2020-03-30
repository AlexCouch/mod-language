//! The trait allowing traversal of AST nodes

use crate::{
  ast::{ Item, },
};

use super::{
  Analyzer,
};

/// Allows semantic analysis by an Analyzer
pub trait Analysis {
  /// Perform semantic anaylsis of a node and its descendants
  fn analyze (&self, analyzer: &mut Analyzer);
}


impl Analysis for Item {
  fn analyze (&self, _analyzer: &mut Analyzer) {

  }
}