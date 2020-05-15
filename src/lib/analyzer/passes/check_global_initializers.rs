use crate::{
  source::{ SourceRegion, },
  ctx::{ Context, ContextItem, Global, ContextKey, },
  ir::{ Expression, ExpressionData, Statement, StatementData, Reference, Conditional, ConditionalBranch, Block, },
};

use super::{
  Analyzer,
  // support_structures::{ Expect, TyMeetResult, },
  // ty_helpers::{ ty_from_global_item, ty_from_unary, ty_from_binary, ty_meet, ty_will_coerce, ty_meet_n, ty_handle_coercion, ty_finalize_coercible, ty_of_constant, },
  // eval_helpers::{ eval_path, eval_local_ident, eval_texpr, },
};



#[repr(u8)]
#[derive(PartialEq)]
enum GlobalInitError {
  Circlular(SourceRegion),
  Rank { reference_location: SourceRegion, referenced_global: ContextKey },
  External { reference_location: SourceRegion, referenced_global: ContextKey },
}


/// Performs analysis at the global, block, statement & expression levels, providing typechecking and IR generation
pub fn check_global_initializers (analyzer: &mut Analyzer) {
  for &global_key in analyzer.context.globals.iter() {
    let global: &Global = analyzer.context.items.get(global_key).unwrap().ref_global().unwrap();
    
    if global.parent_module != analyzer.context.main_mod { continue }

    if let Some(initializer_ir) = &global.initializer {
      if let Some(error) = check_expression_ir(&analyzer.context, global.rank, global_key, initializer_ir) {
        match error {
          GlobalInitError::Circlular(reference_location) => {
            analyzer.error(global.origin, "Circular reference found in global initializer".to_owned())
            .append(reference_location, "This expression references the global being initialized".to_owned());
          },

          GlobalInitError::Rank { reference_location, referenced_global } => {
            let referenced_global = analyzer.context.items.get(referenced_global).unwrap().ref_global().unwrap();

            analyzer.error(global.origin, "Global initializer references uninitialized data".to_owned())
            .append(reference_location, format!(
              "This expression references a global `{}` which is initialized later",
              referenced_global.canonical_name
            ))
            .append(referenced_global.origin, "The referenced global is later initialized here".to_owned());
          },

          GlobalInitError::External { reference_location, referenced_global } => {
            let referenced_global = analyzer.context.items.get(referenced_global).unwrap().ref_global().unwrap();
            let parent_module = analyzer.context.items.get(referenced_global.parent_module).unwrap().ref_module().unwrap();
            
            analyzer.warning(global.origin, "Global initializer references external module data. Module initialization order cannot be guaranteed by the compiler".to_owned())
            .append(reference_location, format!(
              "This expression references a global `{}` which is imported from external module `{}`",
              referenced_global.canonical_name,
              parent_module.canonical_name
            ))
            .append(parent_module.origin.unwrap(), "External module originally imported here".to_owned());
          },
        }
      }
    }
  }
}


fn check_expression_ir (ctx: &Context, global_rank: usize, global_key: ContextKey, ir: &Expression) -> Option<GlobalInitError> {
  match &ir.data {
    &ExpressionData::Reference(Reference::Global(context_key)) => {
      if context_key == global_key {
        Some(GlobalInitError::Circlular(ir.origin))
      } else {
        match ctx.items.get(context_key).unwrap() {
          ContextItem::Global(referenced_global) => if referenced_global.parent_module != ctx.main_mod {
            Some(GlobalInitError::External { reference_location: ir.origin, referenced_global: context_key })
          } else if referenced_global.rank > global_rank {
            Some(GlobalInitError::Rank { reference_location: ir.origin, referenced_global: context_key })
          } else {
            None
          },


          ContextItem::Function(function) => if let Some(body) = &function.body {
            check_block_ir(ctx, global_rank, global_key, body)
          } else {
            None
          },


          // Cannot contain a global reference
          | ContextItem::Module(_)
          | ContextItem::Namespace(_)
          | ContextItem::Type(_)
          => None
        }
      }
    },


    | ExpressionData::Coerce(inner)
    | ExpressionData::Unary { operand: inner, .. }
    => check_expression_ir(ctx, global_rank, global_key, inner),


    ExpressionData::Binary { left, right, .. }
    => check_expression_ir(ctx, global_rank, global_key, left).or_else(|| check_expression_ir(ctx, global_rank, global_key, right)),


    ExpressionData::Call { callee, arguments } => {
      for arg in arguments.iter() {
        let arg_res = check_expression_ir(ctx, global_rank, global_key, arg);

        if arg_res.is_some() { return arg_res }
      }

      check_expression_ir(ctx, global_rank, global_key, callee)
    },

    
    ExpressionData::Conditional(conditional) => check_conditional_ir(ctx, global_rank, global_key, conditional),
    ExpressionData::Block(block) => check_block_ir(ctx, global_rank, global_key, block),


    // cannot contain a reference to a global
    | ExpressionData::Reference(Reference::Local { .. })
    | ExpressionData::Constant(_)
    => None
  }
}


fn check_conditional_ir (ctx: &Context, global_rank: usize, global_key: ContextKey, ir: &Conditional) -> Option<GlobalInitError> {
  let if_res = check_conditional_branch_ir(ctx, global_rank, global_key, &ir.if_branch);

  if if_res.is_some() { return if_res }

  for branch in ir.else_if_branches.iter() {
    let branch_res = check_conditional_branch_ir(ctx, global_rank, global_key, branch);

    if branch_res.is_some() { return branch_res }
  }

  if let Some(else_block) = &ir.else_block {
    check_block_ir(ctx, global_rank, global_key, else_block)
  } else {
    None
  }
}


fn check_conditional_branch_ir (ctx: &Context, global_rank: usize, global_key: ContextKey, ir: &ConditionalBranch) -> Option<GlobalInitError> {
  let condition_res = check_expression_ir(ctx, global_rank, global_key, &ir.condition);

  if condition_res.is_some() { return condition_res }

  check_block_ir(ctx, global_rank, global_key, &ir.body)
}


fn check_block_ir (ctx: &Context, global_rank: usize, global_key: ContextKey, ir: &Block) -> Option<GlobalInitError> {
  for statement in ir.statements.iter() {
    let statement_res = check_statement_ir(ctx, global_rank, global_key, statement);

    if statement_res.is_some() { return statement_res }
  }

  if let Some(trailing_expression) = &ir.trailing_expression {
    check_expression_ir(ctx, global_rank, global_key, trailing_expression)
  } else {
    None
  }
}


fn check_statement_ir (ctx: &Context, global_rank: usize, global_key: ContextKey, ir: &Statement) -> Option<GlobalInitError> {
  match &ir.data {
    | StatementData::Expression(expression)
    | StatementData::Return(Some(expression))
    | StatementData::Declaration { initializer: Some(expression), .. }
    => check_expression_ir(ctx, global_rank, global_key, expression),


    | StatementData::Assignment { target, value }
    | StatementData::ModAssignment { target, value, ..}
    => check_expression_ir(ctx, global_rank, global_key, target).or_else(|| check_expression_ir(ctx, global_rank, global_key, value)),

    
    StatementData::Conditional(conditional) => check_conditional_ir(ctx, global_rank, global_key, conditional),


    StatementData::Block(block) => check_block_ir(ctx, global_rank, global_key, block),
  

    // Cannot contain a reference to a global
    | StatementData::Return(None)
    | StatementData::Declaration { initializer: None, .. }
    => None
  }
}