#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
  // util::{ UnwrapUnchecked, },
  some,
  common::{ Number, },
  source::{ SourceRegion, },
  ast::{ self, Item, ItemData, ExportData, },
  ctx::{  GlobalItem, Type, TypeData, LocalItem,  MultiKey, TypeDisplay, },
  ir,
};

use super::{
  Analyzer,
  ty_helpers::{ ty_from_global_item, ty_from_unary, ty_from_binary, ty_resolve_alias, ty_meet, },
  eval_helpers::{ eval_path, eval_local_ident, },
};





/// Performs analysis at the statement & expression levels
pub fn generate_bodies (analyzer: &mut Analyzer, items: &mut Vec<Item>) {
  for item in items.iter_mut() {
    match &mut item.data {
      | ItemData::Import { .. }
      | ItemData::Export { data: ExportData::List { .. }, .. }
      => continue,
      
      ItemData::Export { data: ExportData::Inline(item), .. } => generate_item(analyzer, item),
      
      | ItemData::Module   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => generate_item(analyzer, item)
    }
  }
}


/// Peforms type evaluation and ir generation for conditionals
fn generate_conditional (analyzer: &mut Analyzer, as_expression: bool, conditional: &ast::Conditional) -> Option<ir::Conditional> {
  unimplemented!()
}

/// Peforms type evaluation and ir generation for blocks
fn generate_block (analyzer: &mut Analyzer, as_expression: bool, block: &ast::Block) -> Option<ir::Block> {
  unimplemented!()
}

/// Peforms type evaluation and ir generation for statements
fn generate_statement (analyzer: &mut Analyzer, statement: &ast::Statement) -> Option<ir::Statement> {
  unimplemented!()
}



/// Peforms type evaluation and ir generation for expressions
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
          let local: &LocalItem = analyzer.get_active_local_context().variables.get(local_key).unwrap();

          Some(ir::Expression::new(ir::ExpressionData::Reference(multi_key), local.ty, expr.origin))
        },

        MultiKey::GlobalKey(global_key) => {
          let global: &GlobalItem = analyzer.context.items.get(global_key).unwrap();

          let ty = ty_from_global_item(global);

          if let Some(ty) = ty {
            Some(ir::Expression::new(ir::ExpressionData::Reference(multi_key), ty, expr.origin))
          } else {
            analyzer.error(expr.origin, "Path does not evaluate to a value".to_owned());

            None
          }
        },
      }
    },

    &ast::ExpressionData::Number(number) => Some(ir::Expression::new(
      ir::ExpressionData::Number(number),
      match number {
        Number::Integer(_) => analyzer.context.int_ty,
        Number::FloatingPoint(_) => analyzer.context.float_ty,
      },
      expr.origin
    )),
  
    &ast::ExpressionData::Unary { box ref operand, operator } => {
      let operand_ir = generate_expr(analyzer, operand)?;

      let result_ty = ty_from_unary(analyzer, operand_ir.ty, operator, expr.origin)?;

      Some(ir::Expression::new(ir::ExpressionData::Unary { operand: box operand_ir, operator }, result_ty, expr.origin))
    },

    &ast::ExpressionData::Binary { box ref left, box ref right, operator } => {
      let irs = (generate_expr(analyzer, left), generate_expr(analyzer, right));

      let (mut left_ir, mut right_ir) = (irs.0?, irs.1?);

      let left_tk = ty_resolve_alias(analyzer, left_ir.ty)?;
      let right_tk = ty_resolve_alias(analyzer, right_ir.ty)?;

      let operand_tk =
        if let Some(tk) = ty_meet(analyzer, true, left_ir.ty, right_ir.ty) { tk }
        else {
          analyzer.error(expr.origin, format!(
            "The types of the subexpressions for this binary operator (left: `{}`, right: `{}`), \
            are not equal and cannot coerce to the same type",
            TypeDisplay { ty_key: left_tk,  context: &analyzer.context },
            TypeDisplay { ty_key: right_tk, context: &analyzer.context },
          ));

          return None
        };

      if left_tk != operand_tk {
        let origin = left_ir.origin;
        left_ir = ir::Expression::new(ir::ExpressionData::Coerce(box left_ir), operand_tk, origin)
      } else if right_tk != operand_tk {
        let origin = right_ir.origin;
        right_ir = ir::Expression::new(ir::ExpressionData::Coerce(box right_ir), operand_tk, origin)
      }

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
              if let Some(arg_ir) = arg_ir {
                let param_ty = unsafe { *parameter_types.get_unchecked(i) };

                if arg_ir.ty != param_ty {
                  analyzer.error(arg_origin, format!(
                    "Parameter {} is of type `{}`, but the argument provided is of type `{}`",
                    i,
                    TypeDisplay { ty_key: param_ty, context: &analyzer.context },
                    TypeDisplay { ty_key: arg_ir.ty, context: &analyzer.context },
                  ));

                  argument_irs = None;
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
              SourceRegion {
                source: callee.origin.source,
                start: callee.origin.end,
                end: expr.origin.end
              },
              format!(
                "Call contains {} arguments, but the callee function defines {} parameters",
                num_args, num_params
              )
            );
          }
        },

        _ => {
          analyzer.error(callee.origin, "Subexpression does not evaluate to a function and cannot be used as the callee of a call expression".to_owned());
        }
      }

      None
    },

    ast::ExpressionData::Block(box block) => {
      let block = generate_block(analyzer, true, block)?;

      let ty = block.trailing_expression.as_ref().unwrap().ty;

      Some(ir::Expression::new(ir::ExpressionData::Block(box block), ty, expr.origin))
    }

    ast::ExpressionData::Conditional(box conditional) => {
      let conditional = generate_conditional(analyzer, true, conditional)?;

      let ty = conditional.if_branch.body.trailing_expression.as_ref().unwrap().ty;

      Some(ir::Expression::new(ir::ExpressionData::Conditional(box conditional), ty, expr.origin))
    }
  }
}


fn generate_item (analyzer: &mut Analyzer, item: &mut Item) {
  match &mut item.data {
    ItemData::Module { identifier, items, .. } => {
      analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
      generate_bodies(analyzer, items);
      analyzer.pop_active_module();
    },

    ItemData::Global { identifier, initializer, .. } => {
      let global_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

      // its possible some shadowing error has overwritten this def and if so we just return
      let global = some!(analyzer.context.items.get(global_key).unwrap().ref_global());
    
      
    },

    ItemData::Function { identifier, parameters, body, .. } => {
      let function_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

      // its possible some shadowing error has overwritten this def and if so we just return
      let function = some!(analyzer.context.items.get(function_key).unwrap().ref_function());


    },

    | ItemData::Import { .. }
    | ItemData::Export { .. }
    => unreachable!()
  }
}