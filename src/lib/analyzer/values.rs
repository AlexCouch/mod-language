//! Structures involved in semantic analysis of top level items

use crate::{
  util::{ make_key_type, },
};

use super::types::{ TypeKey, };


/// The structure used by the semantic analyser to represent a global
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Global {
  pub(super) ty: TypeKey,
}

/// The structure used by the semantic analyser to represent a function
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Function {
  pub(super) ty: TypeKey,
}


make_key_type! {
  /// A SlotMap Key representing a Global variable
  pub struct GlobalKey;
  
  /// A SlotMap Key representing a Function
  pub struct FunctionKey;
}