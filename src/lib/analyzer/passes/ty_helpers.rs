//! Helpers for type evaluation, inferrence, and coercion

use std::{
  mem::{ swap, },
};


use crate::{
  common::{ Operator, Constant, Number, },
  source::{ SourceRegion, },
  ctx::{ Type, Global, Function, ContextItem, ContextKey, TypeData, PrimitiveType, CoercibleType, TypeDisplay, },
  ir,
};

use super::{
  Analyzer,
  support_structures::{ TyMeetResult, },
};


/// Get the result type of a unary expression from an operand type and an operator
pub fn ty_from_unary (analyzer: &mut Analyzer, operand_tk: ContextKey, operator: Operator, origin: SourceRegion) -> Option<ContextKey> {
  let operand_ty = analyzer.context.items.get(operand_tk).unwrap().ref_type().unwrap();

  let operand_td = operand_ty.data.as_ref()?;
  
  Some(match operand_td {
    TypeData::Pointer(tk) if operator == Operator::Dereference => *tk,
    
    | TypeData::Function { .. }
    | TypeData::Pointer  { .. }
    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Coercible(CoercibleType::FloatingPoint)
    | TypeData::Primitive(PrimitiveType::Bool)
    | TypeData::Primitive(PrimitiveType::Integer { .. })
    | TypeData::Primitive(PrimitiveType::FloatingPoint { .. })

    if operator == Operator::AddressOf
    => ty_from_anon_data(analyzer, TypeData::Pointer(operand_tk), origin),


    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Primitive(PrimitiveType::Bool)
    | TypeData::Primitive(PrimitiveType::Integer { .. })

    if matches!(operator, Operator::Not)
    => operand_tk,


    | TypeData::Coercible(CoercibleType::Integer)
    | TypeData::Coercible(CoercibleType::FloatingPoint)
    | TypeData::Primitive(PrimitiveType::Integer { .. })
    | TypeData::Primitive(PrimitiveType::FloatingPoint { .. })

    if matches!(operator, Operator::Sub | Operator::Add)
    => operand_tk,
    
    
    TypeData::Error { .. } => return None,

    _ => {
      analyzer.error(origin, format!(
        "This expression type (`{}`) is not compatible with unary operator `{}`",
        TypeDisplay { ty_key: operand_tk, context: &analyzer.context },
        operator.value()
      ));
      return None
    }
  })
}


/// Determine if a type will coerce into another type
/// 
/// Allows control of conversion from integers to pointers via `allow_int_to_ptr`
pub fn ty_will_coerce (analyzer: &Analyzer, allow_int_to_ptr: bool, from_tk: ContextKey, into_tk: ContextKey) -> bool {
  if from_tk == into_tk { return true }
  
  let from_ty = analyzer.context.items.get(from_tk).unwrap().ref_type().unwrap();
  let into_ty = analyzer.context.items.get(into_tk).unwrap().ref_type().unwrap();

  
  if let (Some(from_td), Some(into_td)) = (&from_ty.data, &into_ty.data) {
    match (from_td, into_td) {
      | (TypeData::Primitive(PrimitiveType::Integer { signed: true, bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::Integer { signed: true, bit_size: into_size }))

      | (TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::Integer { signed: false, bit_size: into_size }))

      | (TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: from_size })
      ,  TypeData::Primitive(PrimitiveType::FloatingPoint { bit_size: into_size }))

      if from_size <= into_size
      => true,


      | (TypeData::Coercible(CoercibleType::FloatingPoint)
      ,  TypeData::Primitive(PrimitiveType::FloatingPoint { .. }))

      | (TypeData::Coercible(CoercibleType::Integer)
      ,  TypeData::Primitive(PrimitiveType::Integer { .. }))

      => true,
      

      | (TypeData::Coercible(CoercibleType::Integer)
      , TypeData::Pointer(_))

      | (TypeData::Primitive(PrimitiveType::Integer { .. })
      ,  TypeData::Pointer(_))

      if allow_int_to_ptr
      => true,


      | (&TypeData::Pointer(from_p)
      ,  &TypeData::Pointer(_))

      if from_p == analyzer.context.void_ty
      => true,


      _ => false
    }
  } else {
    false
  }
}


/// Get the type coerced union of two types, if one is available
/// 
/// Allows control of conversion from integers to pointers via `allow_int_to_ptr`
pub fn ty_meet (analyzer: &mut Analyzer, allow_int_to_ptr: bool, a_tk: ContextKey, b_tk: ContextKey) -> Option<ContextKey> {
  if ty_will_coerce(analyzer, allow_int_to_ptr, a_tk, b_tk) { Some(b_tk) }
  else if ty_will_coerce(analyzer, allow_int_to_ptr, b_tk, a_tk) { Some(a_tk) }
  else { None }
}


/// Get the type coerced union of N types, if one is available
/// 
/// Allows control of conversion from integers to pointers via `allow_int_to_ptr`
/// 
/// Returns `TyMeetResult`,
/// see its documentation for more information on possible results
pub fn ty_meet_n (analyzer: &mut Analyzer, allow_int_to_ptr: bool, tks: &[ContextKey]) -> TyMeetResult {
  let mut solution = None;

  'outer:
  for &tk_a in tks.iter() {
    for &tk_b in tks.iter() {
      if !ty_will_coerce(analyzer, allow_int_to_ptr, tk_b, tk_a) { continue 'outer }
    }

    // if we reach here all types tk_b will coerce to tk_a
    if let Some(existing_solution) = solution {
      if tk_a != existing_solution {
        // multiple possible solutions, unresolvable
        return TyMeetResult::Unresolvable
      }
    } else {
      solution.replace(tk_a);
    }
  }

  if let Some(tk) = solution {
    TyMeetResult::Ok(tk)
  } else {
    TyMeetResult::None
  }
}


/// Handles type coercion, wrapping an IR node into a new IR Coerce node if necessary
pub fn ty_handle_coercion (coerce_ty: ContextKey, expr_ir: &mut ir::Expression) {
  if expr_ir.ty == coerce_ty { return }

  let mut new_ir = ir::Expression::new(
    ir::ExpressionData::Constant(0u64.into()), // placeholder data
    coerce_ty,
    expr_ir.origin
  );

  swap(expr_ir, &mut new_ir);

  expr_ir.data = ir::ExpressionData::Coerce(box new_ir);
}

/// Get a type key from a constant
pub fn ty_of_constant (analyzer: &mut Analyzer, constant: &Constant, origin: SourceRegion) -> ContextKey {
  match constant {
    Constant::NullPointer => ty_from_anon_data(analyzer, TypeData::Pointer(analyzer.context.void_ty), origin),
    Constant::Bool(_) => analyzer.context.bool_ty,
    Constant::Number(Number::Integer(_)) => analyzer.context.int_ty,
    Constant::Number(Number::FloatingPoint(_)) => analyzer.context.float_ty,
    Constant::String(_) => unimplemented!("String types are not yet implemented"),
  }
}


/// Finalizes speculative coercible literal types to their concrete type
pub fn ty_finalize_coercible (analyzer: &mut Analyzer, expr_ir: &mut ir::Expression) {
  let concrete_ty = if expr_ir.ty == analyzer.context.int_ty {
    analyzer.context.concrete_int_ty
  } else if expr_ir.ty == analyzer.context.float_ty {
    analyzer.context.concrete_float_ty
  } else {
    return
  };

  ty_handle_coercion(concrete_ty, expr_ir)
}


/// Get result type of a binary expression from its (pre-coerced if needed) operand union type and an operator
pub fn ty_from_binary (analyzer: &mut Analyzer, operand_tk: ContextKey, operator: Operator, origin: SourceRegion) -> Option<ContextKey> {
  let operand_ty = analyzer.context.items.get(operand_tk).unwrap().ref_type().unwrap();

  let operand_td = operand_ty.data.as_ref()?;

  Some(match (operand_td, operator) {
    | (TypeData::Primitive(PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::And | Operator::Or | Operator::Xor | Operator::Add | Operator::Sub | Operator::Mul | Operator::Div | Operator::Rem)   

    => operand_tk,
    
    
    | (TypeData::Primitive(PrimitiveType::Bool)
    ,  Operator::And | Operator::Or | Operator::Xor)

    | (TypeData::Primitive(PrimitiveType::Bool | PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::Equal | Operator::NotEqual)

    | (TypeData::Primitive(PrimitiveType::Integer { .. } | PrimitiveType::FloatingPoint { .. }) | TypeData::Pointer(_) | TypeData::Coercible(_)
    ,  Operator::Lesser | Operator::Greater | Operator::LesserOrEqual | Operator::GreaterOrEqual)

    => analyzer.context.bool_ty,


    _ => {
      analyzer.error(origin, format!(
        "The operand type of this binary expression (`{}`) does not support the operator `{}`",
        TypeDisplay { ty_key: operand_tk,  context: &analyzer.context },
        operator.value()
      ));

      return None
    }
  })
}


/// Extract the Type ContextKey from a ContextItem, if it is a value item
pub fn ty_from_global_item (context_item: &ContextItem) -> Option<ContextKey> {
  match context_item {
    | &ContextItem::Global(Global { ty, .. })
    | &ContextItem::Function(Function { ty, .. })
    => ty,

    | ContextItem::Module    { .. }
    | ContextItem::Namespace { .. }
    | ContextItem::Type      { .. }
    => None
  }
}


/// Get a ContextKey for an anonymous (unnamed) TypeData,
/// either by getting an existing key or registering a new one
pub fn ty_from_anon_data (analyzer: &mut Analyzer, type_data: TypeData, origin: SourceRegion) -> ContextKey {
  assert!(type_data.is_anon(), "Internal error, non-anonymous TypeData passed to eval_anon_tdata");

  if let Some(existing_key) = analyzer.context.anon_types.get(&type_data) {
    *existing_key
  } else {
    let new_key = analyzer.context.items.insert(Type::new(None, None, None, origin, Some(type_data.clone())).into());
    analyzer.context.anon_types.insert(type_data, new_key);
    new_key
  }
}