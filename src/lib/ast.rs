//! Contains the AST structure and its subordinate structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
};

use crate::{
  source::{ SourceRegion, SourceLocation },
  token::{ TokenStream, Number, Identifier, Operator },
};


/// An enum containing the particular variant of an expression referencing a type
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum TypeExpressionData {
  Identifier(Identifier),
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
  Return(Option<Expression>),

  Block(Box<Block>),
  Conditional(Box<Conditional>),
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



/// A series of statements and an optional trailing expression
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct Block {
  pub statements: Vec<Statement>,
  pub trailing_expression: Option<Expression>,
}

/// An individual conditional block and its predicate expression
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct ConditionalBranch {
  pub condition: Expression,
  pub body: Block,
}

/// A set of 1 or more sequenced conditional branches and an optional else block
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct Conditional {
  pub if_branch: ConditionalBranch,
  pub else_if_branches: Vec<ConditionalBranch>,
  pub else_block: Option<Block>
}



/// An enum containing the particular variant of an item
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum ItemData {
  Global { name: Identifier, explicit_type: Option<TypeExpression>, initializer: Option<Expression> },
  Function { name: Identifier, parameters: Vec<(Identifier, TypeExpression)>, return_type: TypeExpression, body: Option<Block> },
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