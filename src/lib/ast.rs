//! Contains the AST structure and its subordinate structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
};

use crate::{
  source::SourceRegion,
  token::{ TokenStream, Number, Identifier, Operator },
};


/// An enum containing the particular variant of an expression referencing a type
#[allow(missing_docs)]
#[derive(Debug)]
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

impl TypeExpression {
  /// Create a new TypeExpression
  pub fn new (data: TypeExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }
}


/// An enum containing the particular variant of an expression
#[allow(missing_docs)]
#[derive(Debug)]
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

impl Expression {
  /// Create a new Expression
  pub fn new (data: ExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }
}



/// An enum containing the particular variant of a statement
#[allow(missing_docs)]
#[derive(Debug)]
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

impl Statement {
  /// Create a new Statement
  pub fn new (data: StatementData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }
}



/// A series of statements and an optional trailing expression
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Block {
  pub statements: Vec<Statement>,
  pub trailing_expression: Option<Expression>,
}

/// An individual conditional block and its predicate expression
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ConditionalBranch {
  pub condition: Expression,
  pub body: Block,
}

/// A set of 1 or more sequenced conditional branches and an optional else block
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Conditional {
  pub if_branch: ConditionalBranch,
  pub else_if_branches: Vec<ConditionalBranch>,
  pub else_block: Option<Block>
}



/// An enum containing the particular variant of an item
#[derive(Debug)]
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

impl Item {
  /// Create a new Item
  pub fn new (data: ItemData, origin: SourceRegion) -> Self {
    Self { data, origin }
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