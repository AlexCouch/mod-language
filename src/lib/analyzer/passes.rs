use crate::{
  ast::{ ItemData, TypeExpression, TypeExpressionData, },
};

use super::{
  Analyzer,
  AnalysisKey,
  Global,
  Function,
};


/// A phase of work done by an Analyzer while processing an AST
pub trait Pass {
  /// Process a single Item of an AST for a Pass in an Analyzer
  fn process (&mut self, analyzer: &mut Analyzer);
}

impl<T> Pass for T
where T: FnMut (&mut Analyzer)
{
  fn process (&mut self, analyzer: &mut Analyzer) {
    (self)(analyzer)
  }
}


/// Used by Analyzer Passes to evaluate a TypeExpression or Expression into a type reference
pub trait EvaluateType {
  /// The kind of value returned by Evaluate::eval when called on a value during an Analyzer Pass
  type Result;

  /// Process a TypeExpression or Expression during an Analyzer Pass
  fn eval_type (&self, analyzer: &mut Analyzer) -> Self::Result;
}


impl EvaluateType for TypeExpression {
  type Result = Option<AnalysisKey>;

  fn eval_type (&self, _analyzer: &mut Analyzer) -> Self::Result {
    
    None
  }
}


/// Extract the value of an Option::Some or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! some {
  ($val: expr) => { if let Some(v) = $val { v } else { return } };
}

/// Extract the value of a Result::Ok or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! ok {
  ($val: expr) => { if let Ok(v) = $val { v } else { return } };
}

/// Extract the value of a Result::Err or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! err {
  ($val: expr) => { if let Err(v) = $val { v } else { return } };
}


struct ItemPrePass;

impl Pass for ItemPrePass {
  fn process (&mut self, analyzer: &mut Analyzer) {
    for item in analyzer.ast.items() {
      match &item.data {
        ItemData::Global { identifier, explicit_type, .. } => {
          let ty = some!(explicit_type.eval_type(analyzer));

          analyzer.create_item(identifier, Global { ty }, item.origin);
        },
        ItemData::Function { identifier, parameters, return_type, .. } => {
          let ty_expr = TypeExpression::new(
            TypeExpressionData::Function {
              parameter_types: parameters.iter().map(|(_, ty)| ty.clone()).collect(),
              return_type: box return_type.as_ref().cloned()
            },
            item.origin
          );

          let ty = some!(ty_expr.eval_type(analyzer));

          analyzer.create_item(identifier, Function { ty }, item.origin);
        },
      }
    }
  }
}

impl<'a> Analyzer<'a> {
  pub(super) fn get_passes () -> Vec<Box<dyn Pass>> {
    vec! [
      box ItemPrePass
    ]
  }
}