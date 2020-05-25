use mod_utils::{ some, };
use mod_common::{ Operator, };

use crate::{
  ast::{ self, Item, ItemData, ExportData, },
  ctx::{ ContextItem, Type, TypeData, LocalItem,  MultiKey, TypeDisplay, },
  ir,
};

use super::{
  Analyzer,
  support_structures::{ Expect, TyMeetResult, },
  ty_helpers::{ ty_from_global_item, ty_from_unary, ty_from_binary, ty_meet, ty_will_coerce, ty_meet_n, ty_handle_coercion, ty_finalize_coercible, ty_of_constant, },
  eval_helpers::{ eval_path, eval_local_ident, eval_texpr, },
};



/// Performs analysis at the global, block, statement & expression levels, providing typechecking and IR generation
pub fn generate_bodies (analyzer: &mut Analyzer, items: &mut Vec<Item>) {
  for item in items.iter_mut() {
    match &mut item.data {
      | ItemData::Import { .. }
      | ItemData::Alias  { .. }
      | ItemData::Type   { .. }
      | ItemData::Struct { .. }
      | ItemData::Export { data: ExportData::List { .. }, .. }
      => continue,
      
      ItemData::Export { data: ExportData::Inline(item), .. } => generate_item(analyzer, item),
      
      | ItemData::Namespace { .. }
      | ItemData::Global    { .. }
      | ItemData::Function  { .. }
      => generate_item(analyzer, item)
    }
  }
}


fn generate_item (analyzer: &mut Analyzer, item: &mut Item) {
  match &mut item.data {
    ItemData::Namespace { identifier, items, .. } => {
      analyzer.push_active_namespace(analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap());
      generate_bodies(analyzer, items);
      analyzer.pop_active_namespace();
    },

    ItemData::Global { identifier, initializer, .. } => {
      let global_key = analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap();

      if let Some(initializer_expr) = initializer {
        if analyzer.get_active_module_key() != analyzer.context.main_mod {
          analyzer.error(item.origin, "External global declarations cannot have an initializer".to_owned());
          return
        }

        analyzer.create_local_context();
        
        let initializer_ir = generate_expr(analyzer, initializer_expr);
        
        analyzer.remove_local_context();

        // its possible some shadowing error has overwritten this def and if so we just return
        let global = some!(analyzer.context.items.get(global_key).unwrap().ref_global());

        let mut initializer_ir = some!(initializer_ir);
        let explicit_ty = some!(global.ty);

        if initializer_ir.ty != explicit_ty {
          if ty_will_coerce(analyzer, false, initializer_ir.ty, explicit_ty) {
            ty_handle_coercion(explicit_ty, &mut initializer_ir);
          } else {
            analyzer.error(initializer_ir.origin, format!(
              "The type of this expression (`{}`) \
               is not the same as the explicit type given for this declaration (`{}`), \
               and will not automatically coerce to it",
              TypeDisplay { ty_key: initializer_ir.ty, context: &analyzer.context },
              TypeDisplay { ty_key: explicit_ty, context: &analyzer.context }
            ));

            return
          }
        }

        unsafe { analyzer.context.items.get_unchecked_mut(global_key).mut_global_unchecked() }
          .initializer.replace(initializer_ir);
          //.expect_none("Internal error, global initializer IR replaced"); there was an error, yes, but this is ok; our codegen is dead anyways
      }
    },

    ItemData::Function { identifier, body, .. } => {
      let function_key = analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap();

      if let Some(body_block) = body {
        if analyzer.get_active_module_key() != analyzer.context.main_mod {
          analyzer.error(item.origin, "External function declarations cannot have an initializer".to_owned());
          return
        }

        // its possible some shadowing error has overwritten this def and if so we just return
        let function = some!(analyzer.context.items.get(function_key).unwrap().ref_function());

        // TODO :[ temp allocation here sucks but what can you do?
        let params = function.params.clone();
        
        let local_ctx = analyzer.create_local_context();

        for (param_name, param_type, param_origin) in params.into_iter() {
          local_ctx.create_variable(param_name, param_type, true, param_origin);
        }

        let body_ir = generate_block(analyzer, Expect::Allow, body_block);
        
        analyzer.remove_local_context();

        unsafe { analyzer.context.items.get_unchecked_mut(function_key).mut_function_unchecked() }
          .body.replace(some!(body_ir));
          // .expect_none("Internal error, function body IR replaced"); there was an error, yes, but this is ok; our codegen is dead anyways
      } else if analyzer.get_active_module().is_main {
        analyzer.error(item.origin, "Function definitions inside a source module must have a body".to_owned());
      }
    },

    // Handled in previous pass
    | ItemData::Import { .. }
    | ItemData::Struct { .. }
    | ItemData::Type   { .. }
    => { },

    // Not allowed, already produced panic in first pass
    | ItemData::Alias { .. }
    | ItemData::Export { .. }
    => unreachable!()
  }
}


fn generate_conditional (analyzer: &mut Analyzer, expect_expression: Expect, conditional: &ast::Conditional) -> Option<ir::Conditional> {
  let if_branch = generate_conditional_branch(analyzer, expect_expression, &conditional.if_branch);

  let mut else_if_branches = Some(Vec::new());

  for branch in conditional.else_if_branches.iter() {
    if let Some(branch_ir) = generate_conditional_branch(analyzer, expect_expression, branch) {
      if let Some(branches) = &mut else_if_branches {
        branches.push(branch_ir);
      }
    } else {
      else_if_branches.take();
    }
  }

  let else_block = if let Some(else_block) = &conditional.else_block {
    if let Some(block_ir) = generate_block(analyzer, expect_expression, else_block) {
      Ok(Some(block_ir))
    } else {
      Err(())
    }
  } else {
    Ok(None)
  };

  if let (Some(mut if_branch), Some(mut else_if_branches), Ok(mut else_block)) = (if_branch, else_if_branches, else_block) {
    if conditional.is_expression() {
      let mut trail_tys = Vec::with_capacity(else_if_branches.len() + 2);
      
      if let Some(if_trail) = if_branch.body.trailing_expression.as_ref() {
        trail_tys.push(if_trail.ty);
      }

      else_if_branches.iter().for_each(|branch| if let Some(trail) = branch.body.trailing_expression.as_ref() { trail_tys.push(trail.ty) });

      if let Some(trail) = else_block.as_ref().and_then(|block| block.trailing_expression.as_ref()) { trail_tys.push(trail.ty) };

      let meet_result = ty_meet_n(analyzer, false, trail_tys.as_slice());

      match meet_result {
        TyMeetResult::Ok(coerce_ty) => {
          if let Some(if_trail) = &mut if_branch.body.trailing_expression {
            ty_handle_coercion(coerce_ty, if_trail);
          }

          else_if_branches.iter_mut().for_each(|branch| if let Some(trail) = branch.body.trailing_expression.as_mut() { ty_handle_coercion(coerce_ty, trail) });

          if let Some(trail) = else_block.as_mut().and_then(|block| block.trailing_expression.as_mut()) { ty_handle_coercion(coerce_ty, trail) };
        },

        TyMeetResult::None => {
          analyzer.error(
            conditional.origin,
            "This conditional expression has multiple branches that evaluate to different types, \
             with no possible coercion that works for all branches; \
             The final type is unresolvable".to_owned()
          );

          return None
        },

        TyMeetResult::Unresolvable => {
          analyzer.error(
            conditional.origin,
            "This conditional expression has multiple branches that evaluate to different types, \
             with multiple possible coercion outcomes; \
             The final type is unresolvable".to_owned()
          );

          return None
        },
      }
    }

    Some(ir::Conditional::new(
      if_branch,
      else_if_branches,
      else_block,
      conditional.origin
    ))
  } else {
    None
  }
}

fn generate_conditional_branch (analyzer: &mut Analyzer, expect_expression: Expect, conditional_branch: &ast::ConditionalBranch) -> Option<ir::ConditionalBranch> {
  let condition_ir = generate_expr(analyzer, &conditional_branch.condition);

  let body_ir = generate_block(analyzer, expect_expression, &conditional_branch.body);

  let condition_ir = condition_ir?;

  if condition_ir.ty != analyzer.context.bool_ty {
    analyzer.error(condition_ir.origin, format!(
      "Expected an expression of type `bool`, found type `{}`",
      TypeDisplay { ty_key: condition_ir.ty, context: &analyzer.context }
    ));

    None
  } else {
    Some(ir::ConditionalBranch::new(
      condition_ir,
      body_ir?,
      conditional_branch.origin
    ))
  }
}

fn generate_block (analyzer: &mut Analyzer, expect_expression: Expect, block: &ast::Block) -> Option<ir::Block> {
  analyzer.get_local_context_mut().push_stack_frame();

  let mut statements = Some(Vec::new());

  for stmt in block.statements.iter() {
    if let Some(stmt_ir) = generate_stmt(analyzer, stmt) {
      if let Some(stmts) = &mut statements {
        stmts.push(stmt_ir);
      }
    } else {
      statements.take();
    }
  }

  let trail = match (expect_expression, &block.trailing_expression) {
    (Expect::Allow | Expect::Require, Some(expr)) => if let Some(trail) = generate_expr(analyzer, expr) { Ok(Some(trail)) } else { Err(()) },
    (Expect::Allow | Expect::Deny, None) => Ok(None),
    (Expect::Deny, Some(expr)) => {
      analyzer.error(expr.origin, "Unexpected trailing expression".to_owned());
      Err(())
    },
    (Expect::Require, None) => {
      if !matches!(block.statements.last(), Some(ast::Statement { data: ast::StatementData::Return(_), .. })) {
        analyzer.error(block.origin, "Expected trailing expression".to_owned());
        Err(())
      } else {
        Ok(None)
      }
    }
  };

  analyzer.get_local_context_mut().pop_stack_frame();

  if let (Some(statements), Ok(trailing_expression)) = (statements, trail) {
    Some(ir::Block::new(
      statements,
      trailing_expression,
      block.origin
    ))
  } else {
    None
  }
}

fn generate_stmt (analyzer: &mut Analyzer, stmt: &ast::Statement) -> Option<ir::Statement> {
  match &stmt.data {
    ast::StatementData::Declaration { identifier, explicit_type, initializer } => {
      let explicit_tk = if let Some(texpr) = explicit_type { Some(eval_texpr(analyzer, texpr)) } else { None };

      let initializer_ir = if let Some(expr) = initializer { Some(generate_expr(analyzer, expr)) } else { None };

      let (ty, initializer) = match (explicit_tk, initializer_ir) {
        (Some(Some(e_tk)), Some(Some(mut i_ir))) => {
          if i_ir.ty != e_tk {
            if ty_will_coerce(analyzer, false, i_ir.ty, e_tk) {
              ty_handle_coercion(e_tk, &mut i_ir);
              (e_tk, Some(i_ir))
            } else {
              analyzer.error(i_ir.origin, format!(
                "The type of this expression (`{}`) \
                 is not the same as the explicit type given for this declaration (`{}`), \
                 and will not automatically coerce to it",
                TypeDisplay { ty_key: i_ir.ty, context: &analyzer.context },
                TypeDisplay { ty_key: e_tk, context: &analyzer.context },
              ));

              return None
            }
          } else {
            (e_tk, Some(i_ir))
          }
        },

        (None, Some(Some(mut i_ir))) => {
          ty_finalize_coercible(analyzer, &mut i_ir);
          (i_ir.ty, Some(i_ir))
        },

        (Some(Some(e_tk)), None) => (e_tk, None),

        (None, None) => {
          analyzer.error(
            stmt.origin,
            "Local variable declarations must specify an explicit type, \
             an initializer expression, or both".to_owned()
          );
          return None
        },

        | (Some(None), _)
        | (_, Some(None))
        => return None
      };

      analyzer.get_local_context_mut().create_variable(identifier.clone(), ty, false, stmt.origin);

      Some(ir::Statement::new(
        ir::StatementData::Declaration { ty, initializer },
        stmt.origin
      ))
    },

    ast::StatementData::Assignment { target, value } => {
      let target_ir = generate_lvalue(analyzer, target);
      let value_ir = generate_expr(analyzer, value);

      Some(ir::Statement::new(
        ir::StatementData::Assignment { target: target_ir?, value: value_ir? },
        stmt.origin
      ))
    },

    &ast::StatementData::ModAssignment { ref target, ref value, operator } => {
      let target_ir = generate_lvalue(analyzer, target);
      let value_ir = generate_expr(analyzer, value);

      Some(ir::Statement::new(
        ir::StatementData::ModAssignment { target: target_ir?, value: value_ir?, operator },
        stmt.origin
      ))
    },

    ast::StatementData::Return(value) => Some(ir::Statement::new(
      ir::StatementData::Return(if let Some(expr) = value {
        Some(generate_expr(analyzer, expr)?)
      } else {
        None
      }),
      stmt.origin
    )),


    ast::StatementData::Expression(expr) => Some(ir::Statement::new(
      ir::StatementData::Expression(generate_expr(analyzer, expr)?),
      stmt.origin
    )),

    ast::StatementData::Conditional(conditional) => Some(ir::Statement::new(
      ir::StatementData::Conditional(box generate_conditional(analyzer, Expect::Deny, conditional)?),
      stmt.origin
    )),

    ast::StatementData::Block(block) => Some(ir::Statement::new(
      ir::StatementData::Block(box generate_block(analyzer, Expect::Deny, block)?),
      stmt.origin
    )),
  }
}


fn generate_lvalue (analyzer: &mut Analyzer, expr: &ast::Expression) -> Option<ir::Expression> {
  let ir = generate_expr(analyzer, expr)?;

  match &ir.data {
    ir::ExpressionData::Unary { operator, .. }
    if *operator == Operator::Dereference
    => Some(ir),

    ir::ExpressionData::Reference(ir::Reference::Local { .. })
    => Some(ir),
    
    ir::ExpressionData::Reference(ir::Reference::Global(key))
    if analyzer.context.items.get(*key).unwrap().ref_global().is_some()
    => Some(ir),

    _ => {
      analyzer.error(expr.origin, "This expression is not a valid L-value, it cannot be assigned to".to_owned());
      None
    }
  }
}


fn generate_expr (analyzer: &mut Analyzer, expr: &ast::Expression) -> Option<ir::Expression> {
  match &expr.data {
    ast::ExpressionData::Path(path) => {
      let key = eval_path(analyzer, path, expr.origin)?;

      let item = analyzer.context.items.get(key).unwrap();

      let ty = ty_from_global_item(item);

      if let Some(ty) = ty {
        Some(ir::Expression::new(ir::ExpressionData::Reference(key.into()), ty, expr.origin))
      } else {
        analyzer.error(expr.origin, "Path does not evaluate to a value".to_owned());

        None
      }
    },

    ast::ExpressionData::Identifier(ident) => {
      let multi_key = eval_local_ident(analyzer, ident, expr.origin)?;
      
      match multi_key {
        MultiKey::LocalKey(local_key) => {
          let local: &LocalItem = analyzer.get_local_context().variables.get(local_key).unwrap();

          Some(ir::Expression::new(ir::ExpressionData::Reference(local.into()), local.ty, expr.origin))
        },

        MultiKey::ContextKey(global_key) => {
          let global: &ContextItem = analyzer.context.items.get(global_key).unwrap();

          let ty = ty_from_global_item(global);

          if let Some(ty) = ty {
            Some(ir::Expression::new(ir::ExpressionData::Reference(global_key.into()), ty, expr.origin))
          } else {
            analyzer.error(expr.origin, "Path does not evaluate to a value".to_owned());

            None
          }
        },
      }
    },

    ast::ExpressionData::Constant(constant) => Some(ir::Expression::new(
      ir::ExpressionData::Constant(constant.clone()),
      ty_of_constant(analyzer, constant, expr.origin),
      expr.origin
    )),
  
    &ast::ExpressionData::Unary { box ref operand, operator } => {
      let mut operand_ir = generate_expr(analyzer, operand)?;

      ty_finalize_coercible(analyzer, &mut operand_ir);

      let result_ty = ty_from_unary(analyzer, operand_ir.ty, operator, expr.origin)?;

      Some(ir::Expression::new(
        ir::ExpressionData::Unary { operand: box operand_ir, operator },
        result_ty,
        expr.origin
      ))
    },

    &ast::ExpressionData::Binary { box ref left, box ref right, operator } => {
      let irs = (generate_expr(analyzer, left), generate_expr(analyzer, right));

      let (mut left_ir, mut right_ir) = (irs.0?, irs.1?);

      ty_finalize_coercible(analyzer, &mut left_ir);
      ty_finalize_coercible(analyzer, &mut right_ir);

      let operand_tk =
        if let Some(tk) = ty_meet(analyzer, true, left_ir.ty, right_ir.ty) { tk }
        else {
          analyzer.error(expr.origin, format!(
            "The types of the subexpressions for this binary operator \
             (left: `{}`, right: `{}`), \
             are not equal and cannot coerce to the same type",
            TypeDisplay { ty_key: left_ir.ty,  context: &analyzer.context },
            TypeDisplay { ty_key: right_ir.ty, context: &analyzer.context },
          ));

          return None
        };

      ty_handle_coercion(operand_tk, &mut left_ir);
      ty_handle_coercion(operand_tk, &mut right_ir);

      let result_tk = ty_from_binary(analyzer, operand_tk, operator, expr.origin)?;

      Some(ir::Expression::new(ir::ExpressionData::Binary { left: box left_ir, right: box right_ir, operator }, result_tk, expr.origin))
    },

    ast::ExpressionData::Call { box callee, arguments } => {
      let argument_irs: Vec<_> = arguments.iter().map(|arg| (generate_expr(analyzer, arg), arg.origin)).collect();
      
      let callee_ir = generate_expr(analyzer, callee)?;
      
      let callee_ty: &Type = analyzer.context.items.get(callee_ir.ty).unwrap().ref_type().unwrap();
      
      let callee_ty_data = callee_ty.data.as_ref().unwrap();

      let num_args = argument_irs.len();

      match callee_ty_data {
        TypeData::Function { parameter_types, return_type } => {
          let num_params = parameter_types.len();

          if num_params == num_args {
            let iter = argument_irs.into_iter().enumerate();
            let mut argument_irs = Some(Vec::with_capacity(num_args));

            for (i, (arg_ir, arg_origin)) in iter {
              if let Some(mut arg_ir) = arg_ir {
                let param_ty = unsafe { *parameter_types.get_unchecked(i) };

                if arg_ir.ty != param_ty {
                  if ty_will_coerce(analyzer, false, arg_ir.ty, param_ty) {
                    ty_handle_coercion(param_ty, &mut arg_ir);
                    
                    if let Some(argument_irs) = &mut argument_irs {
                      argument_irs.push(arg_ir);
                    }
                  } else {
                    analyzer.error(arg_origin, format!(
                      "The type of argument {} (`{}`) \
                       is not the same as the explicit type given for the parameter declaration (`{}`), \
                       and will not automatically coerce to it",
                      i,
                      TypeDisplay { ty_key: arg_ir.ty, context: &analyzer.context },
                      TypeDisplay { ty_key: param_ty, context: &analyzer.context },
                    ));

                    argument_irs = None;
                  }
                } else if let Some(argument_irs) = &mut argument_irs {
                  argument_irs.push(arg_ir);
                }
              }
            };

            if let Some(argument_irs) = argument_irs {
              return Some(ir::Expression::new(
                ir::ExpressionData::Call { callee: box callee_ir, arguments: argument_irs },
                return_type.unwrap_or(analyzer.context.void_ty),
                expr.origin
              ))
            }
          } else {
            analyzer.error(
              expr.origin,
              format!(
                "Call contains {} arguments, \
                 but the callee function defines {} parameters",
                num_args, num_params
              )
            );
          }
        },

        _ => {
          analyzer.error(
            callee.origin,
            "Subexpression does not evaluate to a function, \
             and cannot be used as the callee of a call expression".to_owned()
          );
        }
      }

      None
    },

    ast::ExpressionData::Block(box block) => {
      let block = generate_block(analyzer, Expect::Require, block)?;

      let ty = block.trailing_expression.as_ref().unwrap().ty;

      Some(ir::Expression::new(
        ir::ExpressionData::Block(box block),
        ty,
        expr.origin
      ))
    }

    ast::ExpressionData::Conditional(box conditional) => {
      let conditional = generate_conditional(analyzer, Expect::Require, conditional)?;

      let ty = if let Some(trail_expr) = conditional.if_branch.body.trailing_expression.as_ref() {
        trail_expr.ty
      } else {
        let mut ty = None;

        for else_branch in conditional.else_if_branches.iter() {
          if let Some(trail_expr) = else_branch.body.trailing_expression.as_ref() {
            ty = Some(trail_expr.ty);
            break
          }
        }

        if let Some(ty) = ty {
          ty
        } else if let Some(trail_expr) = conditional.else_block.as_ref().and_then(|block| block.trailing_expression.as_ref()) {
          trail_expr.ty
        } else {
          panic!("Internal error, found expression conditional with no trailing expressions");
        }
      };

      Some(ir::Expression::new(
        ir::ExpressionData::Conditional(box conditional),
        ty,
        expr.origin
      ))
    }
  }
}