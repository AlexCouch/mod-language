use crate::{
  some,
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