//! Structures involved in type checking

use std::{
  hash::{ Hash, Hasher, },
  collections::hash_map::{ DefaultHasher, },
  ops::{ Deref, },
};

use super::AnalysisKey;


/// A representation for the type of a primitive value
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Hash)]
pub enum PrimitiveType {
  Bool,
  Integer { signed: bool, bit_size: usize },
  FloatingPoint { signed: bool, bit_size: usize },
}

/// Data enum for Type
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Hash)]
pub enum TypeData {
  Undefined,
  Alias(AnalysisKey),
  Primitive(PrimitiveType),
  Pointer(AnalysisKey),
  Function { parameter_types: Vec<AnalysisKey>, return_type: Option<AnalysisKey> },
}

/// The structure used by the semantic analyser to represent a type
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Type {
  pub(super) data: TypeData,
  pub(super) hash: u64,
}

impl Type {
  /// Get a hash code for a given TypeData
  fn hash_data (data: &TypeData) -> u64 {
    let mut hasher = DefaultHasher::default();

    data.hash(&mut hasher);

    hasher.finish()
  }

  /// Create a new Type from TypeData
  pub fn new (data: TypeData) -> Self {
    let hash = Self::hash_data(&data);
    Self { data, hash }
  }
}

impl From<TypeData> for Type {
  fn from (data: TypeData) -> Self { Self::new(data) }
}

impl Deref for Type {
  type Target = TypeData;

  fn deref (&self) -> &Self::Target {
    &self.data
  }
}

impl PartialEq for Type {
  fn eq (&self, other: &Self) -> bool {
    self.hash == other.hash && self.data == other.data
  }
}