//! The intermediate representation produced by the semantic Analyzer

use std::{
  fmt::{ Debug, Formatter, Result as FMTResult, },
  ops::{ Deref, },
};

use crate::{
  common::{ Operator, Number, },
  source::{ SourceRegion, },
  ctx::{ ContextKey, LocalItem, },
};


// IR only represents up to the block level, representation of things like Namespace, Alias, etc are left to Context

/// The top level IR item
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Block {
  pub statements: Vec<Statement>,
  pub trailing_expression: Option<Expression>,
  pub origin: SourceRegion,
}

impl Debug for Block {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult {
    f.debug_struct("Block")
      .field("statements", &self.statements)
      .field("trailing_expression", &self.trailing_expression)
      .finish()
  }
}

impl PartialEq for Block {
  #[inline] fn eq (&self, other: &Self) -> bool { self.statements == other.statements && self.trailing_expression == other.trailing_expression }
}

impl Block {
  /// Create a new Block
  pub fn new (statements: Vec<Statement>, trailing_expression: Option<Expression>, origin: SourceRegion) -> Self {
    Self { statements, trailing_expression, origin }
  }

  /// Create a new Block with no SourceRegion
  pub fn no_src (statements: Vec<Statement>, trailing_expression: Option<Expression>) -> Self {
    Self { statements, trailing_expression, origin: SourceRegion::ANONYMOUS }
  }

  /// Determine if a Block has a trailing Expression, making the Block itself an Expression
  pub fn is_expression (&self) -> bool {
    self.trailing_expression.is_some()
  }
}


/// An individual conditional Block and its predicate Expression
#[derive(Clone)]
#[allow(missing_docs)]
pub struct ConditionalBranch {
  pub condition: Expression,
  pub body: Block,
  pub origin: SourceRegion,
}

impl Debug for ConditionalBranch {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult {
    f.debug_struct("ConditionalBranch")
      .field("condition", &self.condition)
      .field("body", &self.body)
      .finish()
  }
}

impl PartialEq for ConditionalBranch { #[inline] fn eq (&self, other: &Self) -> bool { self.condition == other.condition && self.body == other.body } }

impl ConditionalBranch {
  /// Create a new ConditionalBranch
  pub fn new (condition: Expression, body: Block, origin: SourceRegion) -> Self {
    Self { condition, body, origin }
  }

  /// Create a new ConditionalBranch with no SourceRegion
  pub fn no_src (condition: Expression, body: Block) -> Self {
    Self { condition, body, origin: SourceRegion::ANONYMOUS }
  }

  /// Determine if the Block of a ConditionalBranch has a trailing Expression, making the ConditionalBranch itself an Expression
  pub fn is_expression (&self) -> bool {
    self.body.is_expression()
  }
}


/// A set of 1 or more sequenced ConditionalBranches and an optional else Block
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Conditional {
  pub if_branch: ConditionalBranch,
  pub else_if_branches: Vec<ConditionalBranch>,
  pub else_block: Option<Block>,
  pub origin: SourceRegion,
}

impl Debug for Conditional {
  #[inline]
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    f.debug_struct("Conditional")
      .field("if_branch", &self.if_branch)
      .field("else_if_branches", &self.else_if_branches)
      .field("else_block", &self.else_block)
      .finish()
  }
}

impl PartialEq for Conditional {
  #[inline] fn eq (&self, other: &Self) -> bool { self.if_branch == other.if_branch && self.else_if_branches == other.else_if_branches && self.else_block == other.else_block }
}

impl Conditional {
  /// Create a new Condtional
  pub fn new (if_branch: ConditionalBranch, else_if_branches: Vec<ConditionalBranch>, else_block: Option<Block>, origin: SourceRegion) -> Self {
    Self { if_branch, else_if_branches, else_block, origin }
  }

  /// Create a new Condtional with no SourceRegion
  pub fn no_src (if_branch: ConditionalBranch, else_if_branches: Vec<ConditionalBranch>, else_block: Option<Block>) -> Self {
    Self { if_branch, else_if_branches, else_block, origin: SourceRegion::ANONYMOUS }
  }

  /// Determine if a Conditional's if Branch Block has a trailing Expression, making the Conditional itself an Expression
  pub fn is_expression (&self) -> bool {
    self.if_branch.is_expression()
  }
}


/// Data enum for an IR Statement
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum StatementData {
  Expression(Expression),
  Declaration { ty: ContextKey, initializer: Option<Expression> },
  Assignment { target: Expression, value: Expression },
  ModAssignment { target: Expression, value: Expression, operator: Operator },

  Return(Option<Expression>),

  Block(Box<Block>),
  Conditional(Box<Conditional>),
}

impl StatementData {
  /// Determine if StatementData can be converted to ExpressionData
  pub fn is_expression (&self) -> bool {
    matches!(self, StatementData::Expression(_))
  }
}

/// Mid level IR item, semantic statements
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Statement {
  pub data: StatementData,
  pub origin: SourceRegion,
}

impl Debug for Statement { #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.data.fmt(f) } }
impl PartialEq for Statement { #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data } }
impl Deref for Statement {
  type Target = StatementData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl Statement {
  /// Create a new Statement
  pub fn new (data: StatementData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Statement with no SourceRegion origin
  pub fn no_src (data: StatementData) -> Self {
    Self { data, origin: SourceRegion::ANONYMOUS }
  }
}


/// Enum for reference expressions in an IR
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
  Global(ContextKey),
  Local { is_parameter: bool, index: usize },
}

impl From<ContextKey> for Reference { #[inline] fn from (key: ContextKey) -> Self { Self::Global(key) } }
impl From<&LocalItem> for Reference { #[inline] fn from (local: &LocalItem) -> Self { Self::Local { is_parameter: local.is_parameter, index: local.index } } }

/// Data enum for an IR Expression
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionData {
  Coerce(Box<Expression>),

  Reference(Reference),

  Number(Number),

  Unary {
    operand: Box<Expression>,
    operator: Operator,
  },

  Binary {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: Operator,
  },

  Call { callee: Box<Expression>, arguments: Vec<Expression> },

  Block(Box<Block>),
  Conditional(Box<Conditional>),
}

/// Lower level IR item, semantic expressions
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Expression {
  pub data: ExpressionData,
  pub ty: ContextKey,
  pub origin: SourceRegion,
}

impl Debug for Expression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult {
    f.debug_struct("")
      .field("data", &self.data)
      .field("ty", &self.ty)
      .finish()
  }
}
impl PartialEq for Expression { #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data } }
impl Deref for Expression {
  type Target = ExpressionData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl Expression {
  /// Create a new Expression
  pub fn new (data: ExpressionData, ty: ContextKey, origin: SourceRegion) -> Self {
    Self { data, ty, origin }
  }

  /// Create a new Expression with no SourceRegion origin
  pub fn no_src (data: ExpressionData, ty: ContextKey) -> Self {
    Self { data, ty, origin: SourceRegion::ANONYMOUS }
  }
}