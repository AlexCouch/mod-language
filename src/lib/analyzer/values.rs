//! Structures involved in semantic analysis of top level items

use super::AnalysisKey;


/// The structure used by the semantic analyser to represent a global
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Global {
  pub(super) ty: AnalysisKey,
}

/// The structure used by the semantic analyser to represent a function
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
pub struct Function {
  pub(super) ty: AnalysisKey,
}