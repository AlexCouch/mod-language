//! The generator for bytecode

// TODO the pass which checks for circular and forward references in global initializers could be inlined here,
// but as to whether this is something that should be done im not sure. for the purposes of having an easily
// understandable and factorable analyzer, it should probably be kept within the specific passes where possible.
// however, for performance obviously inlining it here would be better.
// there is a tradeoff to be made either way but im not sure which is more favorable

// TODO there needs to be a similar pass for checking local variable initializers
  // this may be doable as part of the main type checking / ir gen pass

// TODO there needs to be an lvalue checking pass
  // this may be doable as part of the main type checking / ir gen pass

// TODO there needs to be a dereferencee and callee checking pass
  // this may be doable as part of the main type checking / ir gen pass

// TODO ensure proper checking for coercions

// TODO add a statement expression with no effect check?


#![allow(dead_code)]
#![allow(unused_imports)]


use std::{
  collections::{ HashMap, },
  iter::{ Peekable as PeekableIter, },
  slice::{ Iter as SliceIter, },
  marker::{ PhantomData, },
};

type PeekableSliceIter<'a, T> = PeekableIter<SliceIter<'a, T>>;

use crate::{
  common::{ Identifier, Version, Operator, Constant, Number, FloatingPoint, },
  ast,
  ctx::{ self, Context, ContextKey, ContextItem, },
  ir,
  bc,
};


/// An ID counter for generating Module items
pub struct Counter<T: From<bc::ID>> {
  base: bc::ID,
  _p: PhantomData<T>,
}

impl<T: From<bc::ID>> Default for Counter<T> { fn default () -> Self { Counter::new() } }

impl<T: From<bc::ID>> Counter<T> {
  /// Create a new Counter with a zero-initialized base
  pub fn new () -> Self {
    Self { base: 0usize.into(), _p: PhantomData }
  }

  /// Get a new ID from a Counter
  pub fn get_next_base (&mut self) -> bc::ID {
    let result = self.base;
    self.base.0 += 1;
    result
  }

  /// Get a new ID from a Counter
  pub fn get_next (&mut self) -> T {
    self.get_next_base().into()
  }

  /// Clear a Counter back to 0
  pub fn clear (&mut self) {
    self.base.0 = 0;
  }
}

/// Represents local state for a codegen session,
/// inside a bytecoded context such as a global initializer or function body
pub struct LocalCodegen {
  /// A counter for getting unique local variable ids for instruction generation
  pub id_counter: Counter<bc::LocalID>,
  /// A counter for tracking source-level local indices encountered
  pub index_counter: Counter<usize>,
  /// A local map from Context local variable indices to bytecode LocalIDs
  pub index_id_map: HashMap<usize, bc::LocalID>,
}

impl Default for LocalCodegen { fn default () -> Self { Self::new() } }

impl LocalCodegen {
  /// Create a new LocalCodegen
  pub fn new () -> Self {
    Self {
      id_counter: Counter::default(),
      index_counter: Counter::default(),
      index_id_map: HashMap::default(),
    }
  }

  /// Clear a LocalCodegen's state
  pub fn clear (&mut self) {
    self.id_counter.clear();
    self.index_id_map.clear();
  }
}

/// The main state container for generating bytecode from a Context
pub struct Codegen<'a> {
  /// The Context being converted to bytecode by a Codegen session
  pub context: &'a Context,
  /// The bytecode Module being produced by a Codegen session
  pub module: bc::Module,
  /// A lookup helper from context keys to module ids
  pub key_id_map: HashMap<ContextKey, bc::ID>,
  /// A counter for getting unique type ids for module generation
  pub type_id_counter: Counter<bc::TypeID>,
  /// A counter for getting unique global ids for module generation
  pub global_id_counter: Counter<bc::GlobalID>,
  /// A counter for getting unique function ids for module generation
  pub function_id_counter: Counter<bc::FunctionID>,
  /// Local state for a Codegen inside a bytecode context
  pub local: LocalCodegen,
}

impl<'a> Codegen<'a> {
  /// Create a new Codegen from a Context
  pub fn new (context: &'a Context, name: String, version: Version) -> Self {
    Self {
      context,
      module: bc::Module::empty(name, version),
      key_id_map: HashMap::default(),
      type_id_counter: Counter::default(),
      global_id_counter: Counter::default(),
      function_id_counter: Counter::default(),
      local: LocalCodegen::default(),
    }
  }


  /// Fill in the Module of a Codegen, consuming the Codegen and returning the Module
  pub fn generate (mut self) -> bc::Module {
    generate_module(&mut self);
    self.module
  }
}




fn generate_module (cg: &mut Codegen) {
  let exports = generate_namespace_export_body(
    cg,
    cg.context.items
      .get(cg.context.main_ns)
      .unwrap()
      .ref_namespace()
      .unwrap()
  );

  cg.module.exports = exports;
}


fn generate_namespace_export (cg: &mut Codegen, ns: &ctx::Namespace) -> bc::ExportData {
  bc::ExportData::Namespace(generate_namespace_export_body(cg, ns))
}


fn generate_namespace_export_body (cg: &mut Codegen, ns: &ctx::Namespace) -> Vec<bc::Export> {
  let mut exports = Vec::new();

  for (export_name, &export_key) in ns.export_bindings.entry_iter() {
    let export_item: &ContextItem = cg.context.items.get(export_key).unwrap();

    let export = match export_item {
      ContextItem::Module(_) => continue, // we do nothing here because other modules using these exports are redirected in their own codegen, there is no aliasing
      ContextItem::Type(_) => continue, // types are not exported in bytecode modules, and their defs are generated as a side effect of other items

      ContextItem::Namespace(ns) => {
        if ns.parent_module == cg.context.main_mod {
          generate_namespace_export(cg, ns)
        } else { // no aliasing
          continue
        }
      },

      ContextItem::Global(glo) => {
        if glo.parent_module == cg.context.main_mod {
          bc::ExportData::Global(generate_global(cg, glo, export_key))
        } else { // no aliasing
          continue
        }
      },

      ContextItem::Function(func) => {
        if func.parent_module == cg.context.main_mod {
          bc::ExportData::Function(generate_function(cg, func, export_key))
        } else { // no aliasing
          continue
        }
      },
    };

    exports.push(bc::Export::new(export_name.to_string(), export));
  }

  exports
}

fn generate_global (cg: &mut Codegen, global_ctx: &ctx::Global, key: ContextKey) -> bc::GlobalID {
  if let Some(existing_id) = cg.key_id_map.get(&key) {
    // if we've already registered this global we can just return
    (*existing_id).into()
  } else if global_ctx.parent_module == cg.context.main_mod {
    // first we need to register the new global we will be creating
    let id: bc::GlobalID = cg.global_id_counter.get_next();
    cg.key_id_map.insert(key, id.into()).unwrap_none();

    // then we need to evaluate the type of this global, generating a typedef if one does not already exist
    let ty = generate_type_def(cg, global_ctx.ty.unwrap());

    // now we can create the global with an empty initializer, a mutable reference to which we will pass into the instruction codegen
    let mut global_bc = bc::Global::empty(id, ty);

    // then we can evaluate the initializer code if there is any, and recursively generate dependencies
    if let Some(initializer) = global_ctx.initializer.as_ref() {
      cg.local.clear();
      generate_expression(cg, initializer, &mut global_bc.initializer);
    }

    // finally we register the new global
    cg.module.globals.push(global_bc);

    // and return the id
    id
  } else {
    unimplemented!()
  }
}

fn generate_function (cg: &mut Codegen, function_ctx: &ctx::Function, key: ContextKey) -> bc::FunctionID {
  if let Some(existing_id) = cg.key_id_map.get(&key) {
    // if we've already registered this function we can just return
    (*existing_id).into()
  } else if function_ctx.parent_module == cg.context.main_mod {
    // first we need to register the new function we will be creating
    let id: bc::FunctionID = cg.function_id_counter.get_next();
    cg.key_id_map.insert(key, id.into()).unwrap_none();

    // then we need to evaluate the type of this function, generating a typedef if one does not already exist
    let ty = generate_type_def(cg, function_ctx.ty.unwrap());

    // now we can create the function with an empty body, a mutable reference to which we will pass into the instruction codegen
    let mut function_bc = bc::Function::empty(id, ty);

    // then we can evaluate the body code if there is any, and recursively generate dependencies
    if let Some(body) = function_ctx.body.as_ref() {
      cg.local.clear();
      generate_block(cg, body, &mut function_bc.body);
    }

    // finally we register the new function
    cg.module.functions.push(function_bc);

    // and return the id
    id
  } else {
    unimplemented!()
  }
}


fn generate_type_def (cg: &mut Codegen, key: ContextKey) -> bc::TypeID {
  if let Some(existing_id) = cg.key_id_map.get(&key) {
    (*existing_id).into()
  } else {
    let id: bc::TypeID = cg.type_id_counter.get_next();
    cg.key_id_map.insert(key, id.into()).unwrap_none();

    let type_ctx: &ctx::Type = cg.context.items.get(key).unwrap().ref_type().unwrap();

    let data_bc = match type_ctx.data.as_ref().unwrap() {
      ctx::TypeData::Primitive(prim_type) => {
        bc::TypeData::Intrinsic(match prim_type {
          ctx::PrimitiveType::Void => bc::IntrinsicType::Void,
          ctx::PrimitiveType::Bool => bc::IntrinsicType::Bool,
          ctx::PrimitiveType::Integer { signed, bit_size } => {
            match (signed, bit_size) {
              (true,  8) => bc::IntrinsicType::S8,
              (true, 16) => bc::IntrinsicType::S16,
              (true, 32) => bc::IntrinsicType::S32,
              (true, 64) => bc::IntrinsicType::S64,

              (false,  8) => bc::IntrinsicType::U8,
              (false, 16) => bc::IntrinsicType::U16,
              (false, 32) => bc::IntrinsicType::U32,
              (false, 64) => bc::IntrinsicType::U64,

              _ => unimplemented!()
            }
          },
          ctx::PrimitiveType::FloatingPoint { bit_size } => {
            match bit_size {
              32 => bc::IntrinsicType::F32,
              64 => bc::IntrinsicType::F64,
              __ => unimplemented!()
            }
          }
        })
      },

      ctx::TypeData::Pointer(val_key) => bc::TypeData::Pointer(generate_type_def(cg, *val_key)),

      ctx::TypeData::Structure { field_types, .. } => bc::TypeData::Struct(field_types.iter().map(|&field_key| generate_type_def(cg, field_key)).collect()),

      ctx::TypeData::Function { parameter_types, return_type } => {
        let parameters = parameter_types.iter().map(|&param_key| generate_type_def(cg, param_key)).collect();
        let result = return_type.map(|ret_key| generate_type_def(cg, ret_key));

        bc::TypeData::Function { parameters, result }
      },

      _ => unreachable!()
    };

    cg.module.types.push(bc::Type::new(id, data_bc));

    id
  }
}


fn generate_conditional (cg: &mut Codegen, conditional_ir: &ir::Conditional, code: &mut Vec<bc::Instruction>) {
  generate_expression(cg, &conditional_ir.if_branch.condition, code);

  let mut then_instrs = Vec::new();
  generate_block(cg, &conditional_ir.if_branch.body, &mut then_instrs);

  let mut else_if_iter = conditional_ir.else_if_branches.iter().peekable();

  let mut else_instrs = Vec::new();
  generate_else_chain(cg, &mut else_if_iter, conditional_ir.else_block.as_ref(), &mut else_instrs);

  code.push(bc::Instruction::IfBlock(then_instrs, else_instrs))
}

fn generate_else_chain (cg: &mut Codegen, iter: &mut PeekableIter<SliceIter<ir::ConditionalBranch>>, else_br: Option<&ir::Block>, code: &mut Vec<bc::Instruction>) {
  if let Some(else_if_br) = iter.next() {
    generate_expression(cg, &else_if_br.condition, code);

    let mut then_instrs = Vec::new();
    generate_block(cg, &else_if_br.body, &mut then_instrs);

    let mut else_instrs = Vec::new();
    if iter.peek().is_some() {
      generate_else_chain(cg, iter, else_br, &mut else_instrs);
    } else if let Some(else_block) = else_br {
      generate_block(cg, else_block, &mut else_instrs);
    }

    code.push(bc::Instruction::IfBlock(then_instrs, else_instrs))
  } else if let Some(else_block) = else_br {
    generate_block(cg, else_block, code);
  }
}


fn generate_block (cg: &mut Codegen, block_ir: &ir::Block, code: &mut Vec<bc::Instruction>) {
  for statement in block_ir.statements.iter() {
    generate_statement(cg, statement, code);
  }

  if let Some(expression) = block_ir.trailing_expression.as_ref() {
    generate_expression(cg, expression, code);
  }
}


fn generate_statement (cg: &mut Codegen, statement_ir: &ir::Statement, code: &mut Vec<bc::Instruction>) {
  match &statement_ir.data {
    ir::StatementData::Expression(expression) => {
      generate_expression(cg, expression, code);
      code.push(bc::Instruction::Discard);
    },

    ir::StatementData::Declaration { ty, initializer } => {
      generate_type_def(cg, *ty);

      let index = cg.local.index_counter.get_next();
      let id = cg.local.id_counter.get_next();

      cg.local.index_id_map.insert(index, id).unwrap_none();
      
      if let Some(expression) = initializer.as_ref() {
        generate_expression(cg, expression, code);
      }
    },

    ir::StatementData::Assignment { target, value } => {
      generate_lvalue(cg, target, code);
      generate_expression(cg, value, code);
      code.push(bc::Instruction::Store);
    },

    ir::StatementData::ModAssignment { target, value, operator } => {
      generate_lvalue(cg, target, code);
      code.push(bc::Instruction::Duplicate);
      code.push(bc::Instruction::Load);
      generate_expression(cg, value, code);
      code.push(bc::Instruction::from_operator(*operator).unwrap());
      code.push(bc::Instruction::Store);
    },

    ir::StatementData::Return(expression) => {
      if let Some(expression) = expression {
        generate_expression(cg, expression, code);
      }
      code.push(bc::Instruction::Return);
    },

    ir::StatementData::Block(block) => generate_block(cg, block, code),

    ir::StatementData::Conditional(conditional) => generate_conditional(cg, conditional, code),
  }
}


fn generate_lvalue (cg: &mut Codegen, expression_ir: &ir::Expression, code: &mut Vec<bc::Instruction>) {
  if let ir::ExpressionData::Unary { operand, operator: Operator::Dereference } = &expression_ir.data {
    generate_expression(cg, operand, code)
  } else {
    generate_expression(cg, expression_ir, code)
  }
}

fn generate_expression (cg: &mut Codegen, expression_ir: &ir::Expression, code: &mut Vec<bc::Instruction>) {
  match &expression_ir.data {
    ir::ExpressionData::Coerce(sub_expression) => generate_cast(cg, expression_ir.ty, sub_expression, code),

    ir::ExpressionData::Reference(reference) => {
      match reference {
        ir::Reference::Local { index, .. } => {
          code.push(bc::Instruction::LocalAddress((*index as u64).into()));
          code.push(bc::Instruction::Load);
        },
        &ir::Reference::Global(ctx_key) => {
          match cg.context.items.get(ctx_key).unwrap() {
            ContextItem::Global(glo) => {
              code.push(bc::Instruction::GlobalAddress(generate_global(cg, glo, ctx_key)));
              code.push(bc::Instruction::Load);
            },
            ContextItem::Function(func) => code.push(bc::Instruction::FunctionAddress(generate_function(cg, func, ctx_key))),
            _ => unreachable!()
          }
        }
      }
    },

    ir::ExpressionData::Constant(constant) => {
      code.push(bc::Instruction::ImmediateValue(match *constant {
        Constant::NullPointer => bc::ImmediateValue::Null,
        Constant::Bool(bool) => bc::ImmediateValue::Bool(bool),
        Constant::Number(Number::Integer(int)) => bc::ImmediateValue::S32(int as _),
        Constant::Number(Number::FloatingPoint(FloatingPoint::Norm(float))) => bc::ImmediateValue::F32(float as _),
        Constant::Number(Number::FloatingPoint(FloatingPoint::Inf)) => bc::ImmediateValue::F32(f32::INFINITY),
        Constant::Number(Number::FloatingPoint(FloatingPoint::NaN)) => bc::ImmediateValue::F32(f32::NAN),
        Constant::String(ref _s) => unimplemented!("strings are not yet implemented"),
      }));
    },

    &ir::ExpressionData::Unary { ref operand, operator } => {
      if operator == Operator::AddressOf {
        if let box ir::Expression { data: ir::ExpressionData::Reference(reference), .. } = operand {
          // &local is just local_address, while if we did it naively via recursion to generate_expression,
          // we would generate a load and then a spill of the variable into a new temp variable
          match reference {
            ir::Reference::Local { index, .. } => code.push(bc::Instruction::LocalAddress((*index as u64).into())),
            &ir::Reference::Global(ctx_key) => {
              match cg.context.items.get(ctx_key).unwrap() {
                ContextItem::Global(glo) => code.push(bc::Instruction::GlobalAddress(generate_global(cg, glo, ctx_key))),
                ContextItem::Function(func) => code.push(bc::Instruction::FunctionAddress(generate_function(cg, func, ctx_key))),
                _ => unreachable!()
              }
            }
          }
        } else if let box ir::Expression { data: ir::ExpressionData::Unary { operand, operator: Operator::Dereference }, .. } = operand {
          // TODO this should be handled in IR generation, and produce a warning
          // elide `&*x` to just `x`
          generate_expression(cg, operand, code);
        } else {
          // Generate a spill of operand stack data
          let id = cg.local.id_counter.get_next();
          let ty = generate_type_def(cg, operand.ty);

          code.push(bc::Instruction::CreateLocal(ty));

          generate_expression(cg, operand, code);

          code.push(bc::Instruction::LocalAddress(id));
          code.push(bc::Instruction::Duplicate);
          code.push(bc::Instruction::Store);
        }
      } else if let (Operator::Dereference, box ir::Expression { data: ir::ExpressionData::Unary { operand, operator: Operator::AddressOf }, .. }) = (operator, operand) {
        // TODO this should be handled in IR generation, and produce a warning
        // elide `*&x` to just `x`
        generate_expression(cg, operand, code);
      } else {
        generate_expression(cg, operand, code);
        code.push(bc::Instruction::from_operator(operator).unwrap());
      }
    },

    ir::ExpressionData::Binary { left, right, operator } => {
      generate_expression(cg, left, code);
      generate_expression(cg, right, code);
      code.push(bc::Instruction::from_operator(*operator).unwrap());
    },

    ir::ExpressionData::Call { callee, arguments } => {
      for arg in arguments.iter() {
        generate_expression(cg, arg, code);
      }

      if let ir::ExpressionData::Reference(ir::Reference::Global(ctx_key)) = callee.data {
        if let Some(function) = cg.context.items.get(ctx_key).unwrap().ref_function() {
          let direct_ref = generate_function(cg, function, ctx_key);
          code.push(bc::Instruction::CallDirect(direct_ref));
          return
        }
      }

      generate_expression(cg, callee, code);
      code.push(bc::Instruction::CallIndirect);
    },

    ir::ExpressionData::Conditional(conditional) => generate_conditional(cg, conditional, code),

    ir::ExpressionData::Block(block) => generate_block(cg, block, code),
  }
}

fn generate_cast (cg: &mut Codegen, ty_key: ContextKey, expression_ir: &ir::Expression, code: &mut Vec<bc::Instruction>) {
  let type_id = generate_type_def(cg, ty_key);

  let type_ctx: &ctx::Type = cg.context.items.get(ty_key).unwrap().ref_type().unwrap();

  match &expression_ir.data {
    ir::ExpressionData::Constant(constant) => {
      code.push(bc::Instruction::ImmediateValue(match (type_ctx.data.as_ref().unwrap(), constant) {
        (ctx::TypeData::Pointer(_), Constant::NullPointer) => bc::ImmediateValue::Null,

        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: true, bit_size:  8}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::S8(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: true, bit_size: 16}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::S16(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: true, bit_size: 32}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::S32(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: true, bit_size: 64}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::S64(*int as _),

        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: false, bit_size:  8}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::U8(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: false, bit_size: 16}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::U16(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: false, bit_size: 32}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::U32(*int as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::Integer { signed: false, bit_size: 64}), Constant::Number(Number::Integer(int)))
        => bc::ImmediateValue::U64(*int as _),

        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 32 }), Constant::Number(Number::FloatingPoint(FloatingPoint::Norm(float))))
        => bc::ImmediateValue::F32(*float as _),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 64 }), Constant::Number(Number::FloatingPoint(FloatingPoint::Norm(float))))
        => bc::ImmediateValue::F64(*float as _),

        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 32 }), Constant::Number(Number::FloatingPoint(FloatingPoint::Inf)))
        => bc::ImmediateValue::F32(f32::INFINITY),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 64 }), Constant::Number(Number::FloatingPoint(FloatingPoint::Inf)))
        => bc::ImmediateValue::F64(f64::INFINITY),

        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 32 }), Constant::Number(Number::FloatingPoint(FloatingPoint::NaN)))
        => bc::ImmediateValue::F32(f32::NAN),
        (ctx::TypeData::Primitive(ctx::PrimitiveType::FloatingPoint { bit_size: 64 }), Constant::Number(Number::FloatingPoint(FloatingPoint::NaN)))
        => bc::ImmediateValue::F64(f64::NAN),
        
        (_, Constant::String(ref _s)) => unimplemented!("strings are not yet implemented"),

        _ => {
          generate_expression(cg, expression_ir, code);
          code.push(bc::Instruction::Cast(type_id));
          return
        }
      }));
    },


    | ir::ExpressionData::Reference   { .. }
    | ir::ExpressionData::Unary       { .. }
    | ir::ExpressionData::Binary      { .. }
    | ir::ExpressionData::Call        { .. }
    | ir::ExpressionData::Block       { .. }
    | ir::ExpressionData::Conditional { .. }
    => {
      generate_expression(cg, expression_ir, code);
      code.push(bc::Instruction::Cast(type_id));
    },


    ir::ExpressionData::Coerce(_) => unreachable!()
  }
}