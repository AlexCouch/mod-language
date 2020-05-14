//! Contains the AST structure and its subordinate structures

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  ops::{ Deref, },
};

use crate::{
  source::{ SourceRegion, ASTKey, },
  common::{ Constant, Identifier, Operator, },
};


/// A series of identifiers representing a hierarchical path
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct Path {
  pub absolute: bool,
  pub chain: Vec<Identifier>,
  pub origin: SourceRegion,
}

impl PartialEq for Path {
  #[inline] fn eq (&self, other: &Self) -> bool { self.absolute == other.absolute && self.chain == other.chain }
}

impl Path {
  /// Create a new Path 
  pub fn new (absolute: bool, chain: Vec<Identifier>, origin: SourceRegion) -> Self {
    Self {
      absolute,
      chain,
      origin
    }
  }

  /// Create a new Path with an anonymous source origin
  pub fn no_src (absolute: bool, chain: Vec<Identifier>) -> Self {
    Self::new(absolute, chain, SourceRegion::ANONYMOUS)
  }

  
  /// Create a new Path by extending an existing Path
  pub fn extend<I: Into<Identifier>> (&self, extension: I) -> Self {
    let mut chain = self.chain.clone();
    chain.push(extension.into());
    Self::new(self.absolute, chain, self.origin)
  }

  
  /// Update a Path by extending
  pub fn extend_in_place<I: Into<Identifier>> (&mut self, extension: I) {
    self.chain.push(extension.into());
  }

  /// Create a new Path by extending an existing Path
  pub fn extend_multi<I: IntoIterator<Item = Identifier>> (&self, extension: I) -> Self {
    Self::new(self.absolute, self.chain.iter().map(|i| i.to_owned()).chain(extension.into_iter()).collect(), self.origin)
  }

  /// Update a Path by extending
  pub fn extend_in_place_multi<I: IntoIterator<Item = Identifier>> (&mut self, extension: I) {
    for ident in extension.into_iter() {
      self.chain.push(ident)
    }
  }

  /// Pop an identifier off the end of a Path and update its chain hash
  /// 
  /// Panics if there is no identifier to pop, or if popping the identifier causes the path to be empty when it is non-absolute
  pub fn pop (&mut self) -> Identifier {
    let ident = self.chain.pop().expect("Internal error, found empty Path");

    assert!(self.absolute || !self.chain.is_empty(), "Internal error, non-absolute Path emptied");

    ident
  }
}


impl Deref for Path {
  type Target = Vec<Identifier>;
  #[inline] fn deref (&self) -> &Self::Target { &self.chain }
}


impl Display for Path {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    if self.absolute {
      write!(f, "::")?;
    }

    let mut iter = self.iter().peekable();
    
    while let Some(ident) = iter.next() {
      write!(f, "{}", ident)?;

      if iter.peek().is_some() {
        write!(f, "::")?;
      }
    }

    Ok(())
  }
}


/// A declaration of a field or function parameter
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub struct LocalDeclaration {
  pub identifier: Identifier,
  pub ty: TypeExpression,
  pub origin: SourceRegion,
}

impl LocalDeclaration {
  /// Create a new LocalDeclaration
  pub fn new (identifier: Identifier, ty: TypeExpression, origin: SourceRegion) -> Self {
    Self { identifier, ty, origin }
  }

  /// Create a new LocalDeclaration with no SourceRegion origin
  pub fn no_src (identifier: Identifier, ty: TypeExpression) -> Self {
    Self { identifier, ty, origin: SourceRegion::ANONYMOUS }
  }
}


/// An enum containing the particular variant of an expression referencing a type
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpressionData {
  Identifier(Identifier),
  Path(Path),
  Pointer(Box<TypeExpression>),
  Function { parameter_types: Vec<TypeExpression>, return_type: Box<Option<TypeExpression>> },
}

impl From<Identifier> for TypeExpressionData {
  #[inline] fn from (ident: Identifier) -> Self { Self::Identifier(ident) }
}

impl From<&str> for TypeExpressionData {
  #[inline]  fn from (ident: &str) -> Self { Self::Identifier(ident.into()) }
}

impl From<Path> for TypeExpressionData {
  #[inline] fn from (path: Path) -> Self { Self::Path(path) }
}


/// A syntactic element referencing or describing a type
#[allow(missing_docs)]
#[derive(Clone)]
pub struct TypeExpression {
  pub data: TypeExpressionData,
  pub origin: SourceRegion,
}

impl Debug for TypeExpression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { Debug::fmt(&self.data, f) }
}

impl Display for TypeExpression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, 0) }
}

impl HierarchicalDisplay for TypeExpression {
  #[inline] fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, level) }
}

impl PartialEq for TypeExpression {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for TypeExpression {
  type Target = TypeExpressionData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<TypeExpressionData>> From<T> for TypeExpression {
  #[inline] fn from (value: T) -> Self { Self::no_src(value.into()) }
}

impl TypeExpression {
  /// Create a new TypeExpression
  pub fn new (data: TypeExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new TypeExpression with no SourceRegion origin
  pub fn no_src (data: TypeExpressionData) -> Self {
    Self { data, origin: SourceRegion::ANONYMOUS }
  }
}


/// An enum containing the particular variant of an expression
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionData {
  Identifier(Identifier),
  Path(Path),

  Constant(Constant),

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
  #[inline] fn from (ident: Identifier) -> Self { Self::Identifier(ident) }
}

impl From<&str> for ExpressionData {
  #[inline] fn from (ident: &str) -> Self { Self::Identifier(ident.into()) }
}

impl From<Path> for ExpressionData {
  #[inline] fn from (path: Path) -> Self { Self::Path(path) }
}


impl From<Constant> for ExpressionData {
  #[inline] fn from (constant: Constant) -> Self { Self::Constant(constant) }
}

impl From<u64> for ExpressionData {
  #[inline] fn from (num: u64) -> Self { Self::Constant(Constant::Number(num.into())) }
}

impl From<f64> for ExpressionData {
  #[inline] fn from (num: f64) -> Self { Self::Constant(Constant::Number(num.into())) }
}

/// A syntactic element forming a sequence of actions or a reference
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Expression {
  pub data: ExpressionData,
  pub origin: SourceRegion,
}

impl Debug for Expression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { Debug::fmt(&self.data, f) }
}

impl Display for Expression {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, 0) }
}

impl HierarchicalDisplay for Expression {
  #[inline] fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, level) }
}

impl PartialEq for Expression {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for Expression {
  type Target = ExpressionData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<ExpressionData>> From<T> for Expression {
  #[inline] fn from (data: T) -> Self { Self::no_src(data.into()) }
}

impl Expression {
  /// Create a new Expression
  pub fn new (data: ExpressionData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Expression with no SourceRegion origin
  pub fn no_src (data: ExpressionData) -> Self {
    Self { data, origin: SourceRegion::ANONYMOUS }
  }
}



/// An enum containing the particular variant of a statement
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
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

/// A syntactic element forming a single action or control flow choice
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Statement {
  pub data: StatementData,
  pub origin: SourceRegion,
}

impl Debug for Statement {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { Debug::fmt(&self.data, f) }
}

impl Display for Statement {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, 0) }
}

impl HierarchicalDisplay for Statement {
  #[inline] fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, level) }
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
  #[inline] fn from (data: T) -> Self { Self::no_src(data.into()) }
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


/// A series of Statements and an optional trailing Expression
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


/// Data associated with a pseudonym, either an alias or an export
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub struct PseudonymData {
  pub path: Path,
  pub new_name: Option<Identifier>,
  pub origin: SourceRegion,
}

impl PseudonymData {
  /// Create a new PseudonymData
  pub fn new (path: Path, new_name: Option<Identifier>, origin: SourceRegion) -> Self {
    Self { path, new_name, origin }
  }

  /// Create a new PseudonymData with no SourceRegion
  pub fn no_src (path: Path, new_name: Option<Identifier>) -> Self {
    Self { path, new_name, origin: SourceRegion::ANONYMOUS }
  }
}

/// The data associated with an Export
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExportData {
  List(Vec<PseudonymData>),
  Inline(Box<Item>),
}


/// An enum containing the particular variant of an Item
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum ItemData {
  Import { identifier: Identifier, new_name: Option<Identifier>, ast_key: ASTKey },

  Alias { data: Vec<PseudonymData>, terminal: bool },
  Export { data: ExportData, terminal: bool },

  Struct { identifier: Identifier, fields: Vec<LocalDeclaration>, terminal: bool, },
  Type { identifier: Identifier, type_expression: TypeExpression },
  Namespace { identifier: Identifier, items: Vec<Item>, inline: bool },
  Global { identifier: Identifier, explicit_type: TypeExpression, initializer: Option<Expression> },
  Function { identifier: Identifier, parameters: Vec<LocalDeclaration>, return_type: Option<TypeExpression>, body: Option<Block> },
}

impl ItemData {
  /// Determine if a particular variant of ItemData
  /// requires a semicolon to separate it from other Items
  pub fn requires_semi (&self) -> bool {
    match self {
      | ItemData::Import { .. }
      | ItemData::Global { .. }
      | ItemData::Type   { .. }
      => true,

      ItemData::Namespace { inline, .. } => !*inline,
      
      ItemData::Function { body, .. } => body.is_none(),
      
      | ItemData::Struct { terminal, .. }
      | ItemData::Alias  { terminal, .. }
      | ItemData::Export { terminal, .. } => !*terminal,
    }
  }
}

/// A syntactic element forming a single top-level entity such as a function or global variable
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Item {
  pub data: ItemData,
  pub origin: SourceRegion,
}

impl Debug for Item {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { Debug::fmt(&self.data, f) }
}

impl Display for Item {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, 0) }
}

impl HierarchicalDisplay for Item {
  #[inline] fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult { HierarchicalDisplay::fmt_hierarchical(&self.data, f, level) }
}

impl PartialEq for Item {
  #[inline] fn eq (&self, other: &Self) -> bool { self.data == other.data }
}

impl Deref for Item {
  type Target = ItemData;
  #[inline] fn deref (&self) -> &Self::Target { &self.data }
}

impl<T: Into<ItemData>> From<T> for Item {
  #[inline] fn from (data: T) -> Self { Self::no_src(data.into()) }
}

impl Item {
  /// Create a new Item
  pub fn new (data: ItemData, origin: SourceRegion) -> Self {
    Self { data, origin }
  }

  /// Create a new Item with no SourceRegion origin
  pub fn no_src (data: ItemData) -> Self {
    Self { data, origin: SourceRegion::ANONYMOUS }
  }
}



trait HierarchicalDisplay {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult;
}


struct Padding;

impl HierarchicalDisplay for Padding {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    for _ in 0..level {
      write!(f, "  ")?;
    }

    Ok(())
  }
}


impl Display for TypeExpressionData {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for TypeExpressionData {
  fn fmt_hierarchical (&self, f: &mut Formatter, _level: usize) -> FMTResult {
    match self {
      TypeExpressionData::Identifier(ident) => Display::fmt(ident, f),
      TypeExpressionData::Path(path) => Display::fmt(path, f),
      TypeExpressionData::Pointer(sub_texpr) => write!(f, "^{}", sub_texpr),
      TypeExpressionData::Function { parameter_types, return_type } => {
        write!(f, "fn")?;

        if !parameter_types.is_empty() {
          write!(f, " (")?;

          let mut iter = parameter_types.iter().peekable();

          while let Some(param_texpr) = iter.next() {
            Display::fmt(param_texpr, f)?;

            if iter.peek().is_some() { write!(f, ", ")?; }
          }

          write!(f, ")")?;
        }

        if let box Some(return_texpr) = return_type {
          write!(f, " -> {}", return_texpr)?;
        }

        Ok(())
      }
    }
  }
}

impl Display for ExpressionData {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for ExpressionData {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    match self {
      ExpressionData::Identifier(ident) => Display::fmt(ident, f),
      ExpressionData::Path(path) => Display::fmt(path, f),
      ExpressionData::Constant(constant) => Display::fmt(constant, f),
      ExpressionData::Conditional(conditional) => conditional.fmt_hierarchical(f, level),
      ExpressionData::Block(block) => block.fmt_hierarchical(f, level),

      ExpressionData::Unary { operand, operator } => {
        write!(f, "{}(", operator.value())?;
        operand.fmt_hierarchical(f, level)?;
        write!(f, ")")
      }

      ExpressionData::Binary { left, right, operator } => {
        write!(f, "(")?;
        left.fmt_hierarchical(f, level)?;
        write!(f, ") {} (", operator.value())?;
        right.fmt_hierarchical(f, level)?;
        write!(f, ")")
      },
      
      ExpressionData::Call { callee, arguments } => {
        write!(f, "(")?;
        callee.fmt_hierarchical(f, level)?;
        write!(f, ")")?;

        let mut iter = arguments.iter().peekable();

        while let Some(arg_expr) = iter.next() {
          arg_expr.fmt_hierarchical(f, level)?;

          if iter.peek().is_some() { write!(f, ", ")?; }
        }

        write!(f, ")")
      },
    }
  }
}


impl Display for StatementData {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for StatementData {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    match self {
      StatementData::Expression(expr) => expr.fmt_hierarchical(f, level),
      StatementData::Block(block) => block.fmt_hierarchical(f, level),
      StatementData::Conditional(conditional) => conditional.fmt_hierarchical(f, level),

      StatementData::Declaration { identifier, explicit_type, initializer } => {
        write!(f, "let {}", identifier)?;

        if let Some(explicit_texpr) = explicit_type {
          write!(f, ": {}", explicit_texpr)?;
        }

        if let Some(initializer_expr) = initializer {
          write!(f, " = ")?;
          initializer_expr.fmt_hierarchical(f, level)?;
        }

        Ok(())
      },

      StatementData::Assignment { target, value } => {
        target.fmt_hierarchical(f, level)?;

        write!(f, " = ")?;

        value.fmt_hierarchical(f, level)
      },

      StatementData::ModAssignment { target, value, operator } => {
        target.fmt_hierarchical(f, level)?;

        write!(f, " {} ", operator.value())?;

        value.fmt_hierarchical(f, level)
      },

      StatementData::Return(value) => {
        write!(f, "return")?;

        if let Some(value_expr) = value {
          value_expr.fmt_hierarchical(f, level)?;
        }

        Ok(())
      },
    }
  }
}



impl Display for Block {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for Block {
  fn fmt_hierarchical (&self, f: &mut Formatter, mut level: usize) -> FMTResult {
    write!(f, "{{\n")?;
    level += 1;

    for stmt in self.statements.iter() {    
      Padding.fmt_hierarchical(f, level)?;

      stmt.fmt_hierarchical(f, level)?;

      if stmt.requires_semi() {
        write!(f, ";")?;
      }

      write!(f, "\n")?;
    }

    if let Some(trail_expr) = &self.trailing_expression {
      trail_expr.fmt_hierarchical(f, level)?;
    }

    level -= 1;
    Padding.fmt_hierarchical(f, level)?;
    write!(f, "}}")
  }
}



impl Display for ConditionalBranch {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for ConditionalBranch {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    write!(f, "if ")?;

    self.condition.fmt_hierarchical(f, level)?;

    self.body.fmt_hierarchical(f, level)
  }
}


impl Display for Conditional {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for Conditional {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    self.if_branch.fmt_hierarchical(f, level)?;

    for elif in self.else_if_branches.iter() {
      write!(f, "else ")?;
      elif.fmt_hierarchical(f, level)?;
    }

    if let Some(els) = &self.else_block {
      write!(f, "else ")?;
      els.fmt_hierarchical(f, level)?
    }

    Ok(())
  }
}



impl Display for ItemData {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl HierarchicalDisplay for ItemData {
  fn fmt_hierarchical (&self, f: &mut Formatter, mut level: usize) -> FMTResult {
    match self {
      ItemData::Import { identifier, new_name, .. } => {
        write!(f, "import {}", identifier)?;

        if let Some(new_name) = new_name {
          write!(f, " as {}", new_name)?;
        }

        Ok(())
      },

      ItemData::Alias { data, .. } => {
        write!(f, "alias ")?;
        if data.len() == 1 {
          let PseudonymData { path, new_name, .. } = data.first().unwrap();

          write!(f, "{}", path)?;

          if let Some(ident) = new_name {
            write!(f, " as {}", ident)?;
          }

          Ok(())
        } else {
          write!(f, "{{\n")?;
          level += 1;

          let mut iter = data.iter().peekable();
          
          while let Some(PseudonymData { path, new_name, .. }) = iter.next() {
            Padding.fmt_hierarchical(f, level)?;

            write!(f, "{}", path)?;

            if let Some(ident) = new_name {
              write!(f, " as {}", ident)?;
            }

            if iter.peek().is_some() {
              write!(f, ",")?;
            }

            write!(f, "\n")?;
          }

          level -= 1;
          Padding.fmt_hierarchical(f, level)?;
          write!(f, "}}")
        }
      },

      ItemData::Export { data, .. } => {
        write!(f, "export ")?;

        match data {
          ExportData::Inline(item) => item.fmt_hierarchical(f, level),
          ExportData::List(entries) => {
            if entries.len() == 1 {
              let PseudonymData { path, new_name, .. } = entries.first().unwrap();

              write!(f, "{}", path)?;

              if let Some(ident) = new_name {
                write!(f, " as {}", ident)?;
              }

              Ok(())
            } else {
              write!(f, "{{\n")?;
              level += 1;

              let mut iter = entries.iter().peekable();
          
              while let Some(PseudonymData { path, new_name, .. }) = iter.next() {
                Padding.fmt_hierarchical(f, level)?;

                write!(f, "{}", path)?;

                if let Some(ident) = new_name {
                  write!(f, " as {}", ident)?;
                }

                if iter.peek().is_some() {
                  write!(f, ",")?;
                }

                write!(f, "\n")?;
              }

              level -= 1;
              Padding.fmt_hierarchical(f, level)?;
              write!(f, "}}")
            }
          }
        }
      },

      ItemData::Namespace { identifier, items, .. } => {
        write!(f, "ns {} {{\n", identifier)?;
        level += 1;
    
        Displayer(items).fmt_hierarchical(f, level)?;

        level -= 1;
        Padding.fmt_hierarchical(f, level)?;
        write!(f, "}}")
      },

      ItemData::Type { identifier, type_expression } => {
        write!(f, "type {}: ", identifier)?;
        type_expression.fmt_hierarchical(f, level)
      },

      ItemData::Struct { identifier, fields, .. } => {
        write!(f, "struct {} {{\n", identifier)?;
        level += 1;

        let mut iter = fields.iter().peekable();
    
        while let Some(LocalDeclaration { identifier, ty, .. }) = iter.next() {
          Padding.fmt_hierarchical(f, level)?;

          write!(f, "{}: ", identifier)?;

          ty.fmt_hierarchical(f, level)?;

          if iter.peek().is_some() {
            write!(f, ",")?;
          }

          write!(f, "\n")?;
        }

        level -= 1;
        Padding.fmt_hierarchical(f, level)?;
        write!(f, "}}")
      },

      ItemData::Global { identifier, explicit_type, initializer } => {
        write!(f, "global {}: ", identifier)?;

        explicit_type.fmt_hierarchical(f, level)?;

        if let Some(initializer_expr) = initializer {
          write!(f, " = ")?;
          initializer_expr.fmt_hierarchical(f, level)?;
        }

        Ok(())
      },

      ItemData::Function { identifier, parameters, return_type, body } => {
        write!(f, "fn {}", identifier)?;

        if !parameters.is_empty() {
          write!(f, " (")?;

          let mut iter = parameters.iter().peekable();
          
          while let Some(LocalDeclaration { identifier, ty, .. }) = iter.next() {
            write!(f, "{}: ", identifier)?;

            ty.fmt_hierarchical(f, level)?;

            if iter.peek().is_some() {
              write!(f, ", ")?;
            }
          }

          write!(f, ")")?;
        }

        if let Some(return_texpr) = return_type {
          write!(f, " -> ")?;
          return_texpr.fmt_hierarchical(f, level)?;
        }

        if let Some(body_block) = body {
          write!(f, " ")?;
          body_block.fmt_hierarchical(f, level)?;
        }

        Ok(())
      }
    }
  }
}

/// A wrapper for display vecs of ast Items
pub struct Displayer<'a> (pub &'a Vec<Item>);

impl<'a> Display for Displayer<'a> {
  #[inline] fn fmt (&self, f: &mut Formatter) -> FMTResult { self.fmt_hierarchical(f, 0) }
}

impl<'a> HierarchicalDisplay for Displayer<'a> {
  fn fmt_hierarchical (&self, f: &mut Formatter, level: usize) -> FMTResult {
    for item in self.0.iter() {
      Padding.fmt_hierarchical(f, level)?;

      item.fmt_hierarchical(f, level)?;

      if (
        !matches!(&item.data,
            ItemData::Alias     { .. }
          | ItemData::Struct    { .. }
          | ItemData::Export { data: ExportData::List(_), .. }
        ) && item.requires_semi()
      ) || matches!(&item.data,
          ItemData::Alias { data: list, .. }
        | ItemData::Export { data: ExportData::List(list), .. }
        if list.len() == 1
      ) {
        write!(f, ";")?;
      }

      write!(f, "\n")?;
    }

    Ok(())
  }
}