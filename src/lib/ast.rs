//! Contains the AST structure and its subordinate structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  ops::{ Deref, },
};

use crate::{
  source::{ SourceRegion, SourceLocation, },
  common::{ Number, Identifier, Operator, },
  token::{ TokenStream, },
};


/// An enum containing the particular variant of an expression referencing a type
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum TypeExpressionData {
  Identifier(Identifier),
}

impl From<Identifier> for TypeExpressionData {
  #[inline]
  fn from (ident: Identifier) -> Self {
    Self::Identifier(ident)
  }
}

impl From<&str> for TypeExpressionData {
  #[inline]
  fn from (ident: &str) -> Self {
    Self::Identifier(ident.into())
  }
}

/// A semantic element referencing or describing a type
#[allow(missing_docs)]
pub struct TypeExpression {
  pub data: TypeExpressionData,
  pub origin: SourceRegion,
}

impl Debug for TypeExpression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.data.fmt(f) }
}

impl PartialEq for TypeExpression {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for TypeExpression {
  type Target = TypeExpressionData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<TypeExpressionData>> From<T> for TypeExpression {
  #[inline]
  fn from (value: T) -> Self { Self::no_src(value.into()) }
}

impl TypeExpression {
  /// Create a new TypeExpression
  pub fn new (data: TypeExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new TypeExpression with no SourceRegion origin
  pub fn no_src (data: TypeExpressionData) -> Self {
    Self { data, origin: SourceLocation::ZERO.to_region() }
  }
}


/// An enum containing the particular variant of an expression
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum ExpressionData {
  Identifier(Identifier),
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

impl From<Identifier> for ExpressionData {
  #[inline]
  fn from (ident: Identifier) -> Self {
    Self::Identifier(ident)
  }
}

impl From<&str> for ExpressionData {
  #[inline]
  fn from (ident: &str) -> Self {
    Self::Identifier(ident.into())
  }
}

impl From<Number> for ExpressionData {
  #[inline]
  fn from (num: Number) -> Self {
    Self::Number(num)
  }
}

impl From<u64> for ExpressionData {
  #[inline]
  fn from (num: u64) -> Self {
    Self::Number(num.into())
  }
}

impl From<f64> for ExpressionData {
  #[inline]
  fn from (num: f64) -> Self {
    Self::Number(num.into())
  }
}

/// A semantic element forming a sequence of actions or a reference
#[allow(missing_docs)]
pub struct Expression {
  pub data: ExpressionData,
  pub origin: SourceRegion,
}

impl Debug for Expression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.data.fmt(f) }
}

impl PartialEq for Expression {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for Expression {
  type Target = ExpressionData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<ExpressionData>> From<T> for Expression {
  #[inline]
  fn from (data: T) -> Self { Self::no_src(data.into()) }
}

impl Expression {
  /// Create a new Expression
  pub fn new (data: ExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Expression with no SourceRegion origin
  pub fn no_src (data: ExpressionData) -> Self {
    Self { data, origin: SourceLocation::ZERO.to_region() }
  }
}



/// An enum containing the particular variant of a statement
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum StatementData {
  Expression(Expression),
  Declaration { identifier: Identifier, explicit_type: Option<TypeExpression>, initializer: Option<Expression> },
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

  /// Determine if a particular variant of StatementData
  /// requires a semicolon to separate it from other Statements
  pub fn requires_semi (&self) -> bool {
    match self {
      StatementData::Expression { .. } |
      StatementData::Declaration { .. } |
      StatementData::Assignment { .. } |
      StatementData::ModAssignment { .. } |
      StatementData::Return { .. }
        => true,
      _ => false
    }
  }
}

/// A semantic element forming a single action or control flow choice
#[allow(missing_docs)]
pub struct Statement {
  pub data: StatementData,
  pub origin: SourceRegion,
}

impl Debug for Statement {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.data.fmt(f) }
}

impl PartialEq for Statement {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for Statement {
  type Target = StatementData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl From<Expression> for Statement {
  #[inline]
  fn from (e: Expression) -> Self {
    let origin = e.origin;
    Self::new(StatementData::Expression(e), origin)
  }
}

impl<T: Into<StatementData>> From<T> for Statement {
  #[inline]
  fn from (data: T) -> Self { Self::no_src(data.into()) }
}

impl Statement {
  /// Create a new Statement
  pub fn new (data: StatementData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Statement with no SourceRegion origin
  pub fn no_src (data: StatementData) -> Self {
    Self { data, origin: SourceLocation::ZERO.to_region() }
  }
}


/// A series of Statements and an optional trailing Expression
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
    Self { statements, trailing_expression, origin: SourceLocation::ZERO.to_region() }
  }

  /// Determine if a Block has a trailing Expression, making the Block itself an Expression
  pub fn is_expression (&self) -> bool {
    self.trailing_expression.is_some()
  }
}

/// An individual conditional Block and its predicate Expression
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

impl PartialEq for ConditionalBranch {
  #[inline] fn eq (&self, other: &Self) -> bool { self.condition == other.condition && self.body == other.body }
}

impl ConditionalBranch {
  /// Create a new ConditionalBranch
  pub fn new (condition: Expression, body: Block, origin: SourceRegion) -> Self {
    Self { condition, body, origin }
  }

  /// Create a new ConditionalBranch with no SourceRegion
  pub fn no_src (condition: Expression, body: Block) -> Self {
    Self { condition, body, origin: SourceLocation::ZERO.to_region() }
  }

  /// Determine if the Block of a ConditionalBranch has a trailing Expression, making the ConditionalBranch itself an Expression
  pub fn is_expression (&self) -> bool {
    self.body.is_expression()
  }
}

/// A set of 1 or more sequenced ConditionalBranches and an optional else Block
#[allow(missing_docs)]
pub struct Conditional {
  pub if_branch: ConditionalBranch,
  pub else_if_branches: Vec<ConditionalBranch>,
  pub else_block: Option<Block>,
  pub origin: SourceRegion,
}

impl Debug for Conditional {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult {
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
    Self { if_branch, else_if_branches, else_block, origin: SourceLocation::ZERO.to_region() }
  }

  /// Determine if a Conditional's if Branch Block has a trailing Expression, making the Conditional itself an Expression
  pub fn is_expression (&self) -> bool {
    self.if_branch.is_expression()
  }
}



/// An enum containing the particular variant of an Item
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum ItemData {
  Global { identifier: Identifier, explicit_type: TypeExpression, initializer: Option<Expression> },
  Function { identifier: Identifier, parameters: Vec<(Identifier, TypeExpression)>, return_type: Option<TypeExpression>, body: Option<Block> },
}

impl ItemData {
  /// Determine if a particular variant of ItemData
  /// requires a semicolon to separate it from other Items
  pub fn requires_semi (&self) -> bool {
    match self {
      ItemData::Global { .. }
        => true,
      _ => false
    }
  }
}

/// A semantic element forming a single top-level entity such as a function or global variable
#[allow(missing_docs)]
pub struct Item {
  pub data: ItemData,
  pub origin: SourceRegion,
}

impl Debug for Item {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.data.fmt(f) }
}

impl PartialEq for Item {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for Item {
  type Target = ItemData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<ItemData>> From<T> for Item {
  #[inline]
  fn from (data: T) -> Self { Self::no_src(data.into()) }
}

impl Item {
  /// Create a new Item
  pub fn new (data: ItemData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Item with no SourceRegion origin
  pub fn no_src (data: ItemData) -> Self {
    Self { data, origin: SourceLocation::ZERO.to_region() }
  }
}


/// A set of top level items and a reference to the TokenStream they originated from
#[allow(missing_docs)]
pub struct AST<'a> {
  items: Vec<Item>,
  pub stream: &'a TokenStream<'a>,
}

impl<'a> AST<'a> {
  /// Create a new AST
  pub fn new (items: Vec<Item>, stream: &'a TokenStream<'a>) -> Self {
    Self {
      items,
      stream,
    }
  }

  /// Get a slice of the Items in an AST
  pub fn items (&self) -> &[Item] {
    self.items.as_slice()
  }
}

impl<'a> Display for AST<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    writeln!(f, "AST [")?;

    for item in self.items.iter() {
      writeln!(f, "{:#?}", item)?;
    }

    writeln!(f, "]")
  }
}