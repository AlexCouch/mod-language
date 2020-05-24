use std::{
  ops::{ Deref, DerefMut, },
};

use llvm_sys::{
  LLVMIntPredicate,
  LLVMRealPredicate,
  core::{
    LLVMConstInt,
    LLVMConstPointerNull,
    LLVMConstReal,
    LLVMGetParam,
    LLVMAppendBasicBlockInContext,
    LLVMRemoveBasicBlockFromParent,
    LLVMMoveBasicBlockAfter,
    LLVMPositionBuilderAtEnd,
    LLVMBuildAlloca,
    LLVMBuildRet,
    LLVMBuildRetVoid,
    LLVMBuildStructGEP2,
    LLVMBuildIntCast,
    LLVMBuildFPToSI,
    LLVMBuildSIToFP,
    LLVMBuildFPToUI,
    LLVMBuildUIToFP,
    LLVMBuildPtrToInt,
    LLVMBuildIntToPtr,
    LLVMBuildPointerCast,
    LLVMBuildLoad,
    LLVMBuildStore,
    LLVMBuildNeg,
    LLVMBuildFNeg,
    LLVMBuildNot,
    LLVMBuildAdd,
    LLVMBuildFAdd,
    LLVMBuildSub,
    LLVMBuildFSub,
    LLVMBuildMul,
    LLVMBuildFMul,
    LLVMBuildSDiv,
    LLVMBuildUDiv,
    LLVMBuildFDiv,
    LLVMBuildSRem,
    LLVMBuildURem,
    LLVMBuildFRem,
    LLVMBuildAnd,
    LLVMBuildOr,
    LLVMBuildXor,
    LLVMBuildLShr,
    LLVMBuildAShr,
    LLVMBuildShl,
    LLVMBuildICmp,
    LLVMBuildFCmp,
    LLVMBuildCall,
    LLVMBuildCall2,
    LLVMBuildBr,
    LLVMBuildCondBr,
  },
  analysis::{
    LLVMVerifyFunction,
    LLVMVerifierFailureAction,
  },
  prelude::{
    LLVMValueRef,
    LLVMBasicBlockRef,
  },
};

use mod_utils::{ equal, };
use mod_bytecode::{ self as bc, ImmediateValue, };

use crate::{
  c_lit,
  llvm_extras::{ LLVM_SUCCESS, LLVM_FALSE, LLVM_TRUE, },
  context::{ self as ctx, Context, IntrinsicType, },
  module_loader::{ IR, CompilationError, CompilationErrorData, CompilationResult, },
};


type Value = (LLVMValueRef, ctx::TypeLink);

type Stack = Vec<Value>;

type FuncSignature = (Vec<ctx::TypeLink>, Option<ctx::TypeLink>, bool);


struct LocalIR<'a, 'b> {
  ir: &'a mut IR<'b>,

  ref_id: bc::GenericID,
  func_sig: FuncSignature,
  func_llvm: LLVMValueRef,

  locals: Stack,
  stack: Stack,
}

impl<'a, 'b> LocalIR<'a, 'b> {
  fn new (ir: &'a mut IR<'b>, ref_id: bc::GenericID, func_link: ctx::FunctionLink) -> Result<Self, CompilationError> {
    let ctx_func = ir.context.get_function(func_link);
    let func_tl = ctx_func.ty;
    let func_ty = ir.context.get_type(func_tl);
    let func_llvm = ctx_func.llvm;
    let func_sig =
      if let ctx::TypeData::Function { parameters, result, is_var_arg } = &func_ty.data { (parameters.clone(), *result, *is_var_arg) }
      else {
        let t_id = ir.reverse_type_lookup(ctx_func.ty).into();
        return ir.error(CompilationErrorData::InvalidID(ref_id, t_id))
      };

    Ok(Self {
      ir,

      ref_id,
      func_sig,
      func_llvm,

      locals: Vec::default(),
      stack: Vec::default(),
    })
  }

  fn create_local (&mut self, ty: ctx::TypeLink) {
    let local_storage = unsafe { LLVMBuildAlloca(self.ir.context.llvm.builder, self.ir.context.get_type(ty).llvm_t(&self.ir.context), c_lit!("local")) };
    self.locals.push((local_storage, ty))
  }
  
  fn get_local (&mut self, id: bc::LocalID) -> Result<&Value, CompilationError> {
    let index = u64::from(id) as usize;
    
    if self.locals.len() > index {
      Ok(&self.locals[index])
    } else {
      let ref_id = self.ref_id;
      self.error(CompilationErrorData::InvalidLocalID(ref_id, id))
    }
  }

  fn stack_push (&mut self, v: Value) {
    self.stack.push(v)
  }

  fn stack_pop (&mut self) -> Result<Value, CompilationError> {
    if let Some(v) = self.stack.pop() {
      Ok(v)
    } else {
      let ref_id = self.ref_id;
      self.error(CompilationErrorData::StackUnderflow(ref_id))
    }
  }

  fn stack_peek (&mut self) -> Result<Value, CompilationError> {
    if let Some(v) = self.stack.last() {
      Ok(*v)
    } else {
      let ref_id = self.ref_id;
      self.error(CompilationErrorData::StackUnderflow(ref_id))
    }
  }
}

impl<'a, 'b> Deref for LocalIR<'a, 'b> {
  type Target = IR<'b>;
  fn deref (&self) -> &IR<'b> { self.ir }
}

impl<'a, 'b> DerefMut for LocalIR<'a, 'b> {
  fn deref_mut (&mut self) -> &mut IR<'b> { self.ir }
}


/// Generates LLVM instructions from bytecode instructions
pub(crate) fn generate_function_body (ir: &mut IR, ref_id: bc::GenericID, func_link: ctx::FunctionLink, instructions: &[bc::Instruction]) -> CompilationResult { unsafe {
  let mut ir = LocalIR::new(ir, ref_id, func_link)?;
  
  let (param_tys, _, is_var_arg) = &ir.func_sig;

  assert!(!is_var_arg, "Var arg functions NYI");

  let llvm_func = ir.context.get_function(func_link).llvm;

  let entry_b = LLVMAppendBasicBlockInContext(ir.context.llvm.ctx, llvm_func, c_lit!("entry"));
  LLVMPositionBuilderAtEnd(ir.context.llvm.builder, entry_b);
  
  for (i, &param_tl) in param_tys.iter().enumerate() {
    let param_ty = ir.context.get_type(param_tl);

    let param_storage = LLVMBuildAlloca(ir.context.llvm.builder, param_ty.llvm_t(&ir.context), c_lit!("param"));
    
    LLVMBuildStore(ir.context.llvm.builder, LLVMGetParam(llvm_func, i as _), param_storage);

    ir.locals.push((param_storage, param_tl))
  }

  let (termination, _) = generate_block(&mut ir, entry_b, None, instructions)?;

  if !termination {
    generate_return(&mut ir)?;
  }

  if LLVMVerifyFunction(llvm_func, LLVMVerifierFailureAction::LLVMReturnStatusAction) == LLVM_SUCCESS {
    Ok(())
  } else {
    ir.error(CompilationErrorData::ValidationFailed(ref_id))
  }
} }


fn generate_block (
  ir: &mut LocalIR,
  mut block: LLVMBasicBlockRef,
  landing_pad: Option<LLVMBasicBlockRef>,
  instructions: &[bc::Instruction]
) -> Result<(bool, LLVMBasicBlockRef), CompilationError> { unsafe {
  let ref_id = ir.ref_id;
  let builder = ir.context.llvm.builder;

  LLVMPositionBuilderAtEnd(builder, block);

  let mut instr_iter  = instructions.iter().peekable();

  while let Some(instr) = instr_iter.next() {
    use bc::Instruction::*;

    match instr {
      NoOp => { },

      &ImmediateValue(imm) => ir.stack_push(generate_immediate(ir.context, imm)),


      &CreateLocal(ty_id) => {
        let tl = ir.get_type(ref_id, ty_id)?;

        ir.create_local(tl)
      },


      &LocalAddress(l_id) => {
        let (val, tl) = *ir.get_local(l_id)?;

        let ptr_tl = ir.context.tl_pointer(tl);

        ir.stack_push((val, ptr_tl))
      },

      &GlobalAddress(g_id) => {
        let gl = ir.get_global(ref_id, g_id)?;

        let global = ir.context.get_global(gl);
        let llvm = global.llvm;
        let tl = global.ty;
        let ptr_tl = ir.context.tl_pointer(tl);

        // TODO i dont know if this works, may need some conversion or something
        ir.stack_push((llvm, ptr_tl));
      },

      &FunctionAddress(f_id) => {
        let fl = ir.get_function(ref_id, f_id)?;

        let function = ir.context.get_function(fl);
        let llvm = function.llvm;
        let tl = function.ty;

        // TODO i dont know if this works, may need some conversion or something
        ir.stack_push((llvm, tl));
      },

      &GetElement(e_id) => {
        let index = u64::from(e_id) as usize;

        let (val, tl) = ir.stack_pop()?;

        let ty = ir.context.get_type(tl);

        if let ctx::TypeData::Pointer(v_tl) = &ty.data {
          let v_ty = ir.context.get_type(*v_tl);

          if let ctx::TypeData::Structure { types, .. } = &v_ty.data {
            if index < types.len() {
              let gep = LLVMBuildStructGEP2(builder, v_ty.llvm_t(ir.context), val, index as _, c_lit!("gep"));
              
              let tl = types[index];

              // TODO Im not sure if this should be a pointer type, or if its considered a value

              let ptr_tl = ir.context.tl_pointer(tl);

              ir.stack_push((gep, ptr_tl));

              continue
            }
          }
        }

        return ir.error(CompilationErrorData::GetInvalidElement(ref_id))
      },

      &Cast(t_id) => {
        let new_tl = ir.get_type(ref_id, t_id)?;
        
        let (val, val_tl) = ir.stack_pop()?;

        let new_ty = ir.context.get_type(new_tl);
        let val_ty = ir.context.get_type(val_tl);

        let llvm_t = new_ty.llvm_t(ir.context);

        use ctx::TypeData::*;
        use IntrinsicType::*;

        let llvm = match (&val_ty.data, &new_ty.data) {
          (Pointer(_) , Pointer(_))
          => LLVMBuildPointerCast(builder, val, llvm_t, c_lit!("ptr_cast")),

          (Pointer(_), Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => LLVMBuildPtrToInt(builder, val, llvm_t, c_lit!("ptr_to_int")),

          (Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64), Pointer(_))
          => LLVMBuildIntToPtr(builder, val, llvm_t, c_lit!("int_to_ptr")),

          (Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool)
          ,Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool))
          => LLVMBuildIntCast(builder, val, llvm_t, c_lit!("int_cast")),

          (Intrinsic(S8 | S16 | S32 | S64), Intrinsic(F32 | F64))
          => LLVMBuildSIToFP(builder, val, llvm_t, c_lit!("s_to_f")),

          (Intrinsic(F32 | F64), Intrinsic(S8 | S16 | S32 | S64))
          => LLVMBuildFPToSI(builder, val, llvm_t, c_lit!("f_to_s")),

          (Intrinsic(U8 | U16 | U32 | U64), Intrinsic(F32 | F64))
          => LLVMBuildUIToFP(builder, val, llvm_t, c_lit!("u_to_f")),

          (Intrinsic(F32 | F64), Intrinsic(U8 | U16 | U32 | U64))
          => LLVMBuildFPToUI(builder, val, llvm_t, c_lit!("f_to_u")),

          _ => {
            let a_id = ir.reverse_type_lookup(val_tl);
            let b_id = ir.reverse_type_lookup(new_tl);
            return ir.error(CompilationErrorData::InvalidCast(ref_id, a_id, b_id))
          }
        };

        ir.stack_push((llvm, new_tl))
      },

      Load => {
        let (ptr, ptr_tl) = ir.stack_pop()?;

        let ptr_ty = ir.context.get_type(ptr_tl);

        if let ctx::TypeData::Pointer(v_tl) = ptr_ty.data {
          // TODO may need to type check pointer value type? 
          let llvm = LLVMBuildLoad(builder, ptr, c_lit!("load"));

          ir.stack_push((llvm, v_tl));

          continue
        }

        let t_id = ir.reverse_type_lookup(ptr_tl);
        return ir.error(CompilationErrorData::InvalidLoad(ref_id, t_id))
      },

      Store => {
        let (val, val_tl) = ir.stack_pop()?;
        let (ptr, ptr_tl) = ir.stack_pop()?;

        let ptr_ty = ir.context.get_type(ptr_tl);

        if let ctx::TypeData::Pointer(ptr_val_tl) = ptr_ty.data {
          if ptr_val_tl == val_tl {
            LLVMBuildStore(builder, val, ptr);

            continue
          }
        }

        let a_id = ir.reverse_type_lookup(ptr_tl);
        let b_id = ir.reverse_type_lookup(val_tl);
        return ir.error(CompilationErrorData::InvalidStore(ref_id, a_id, b_id))
      },

      Duplicate => {
        let val = ir.stack_peek()?;
        ir.stack_push(val);
      },

      Discard => {
        ir.stack_pop()?;
      },
      
      un_op @ (
        Neg | Not
      ) => {
        let (val, val_tl) = ir.stack_pop()?;
        
        let val_ty = ir.context.get_type(val_tl);

        use ctx::TypeData::*;
        use IntrinsicType::*;

        let llvm = match (un_op, &val_ty.data) {
          (Neg, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => LLVMBuildNeg(builder, val, c_lit!("ineg")),

          (Neg, Intrinsic(F32 | F64))
          => LLVMBuildFNeg(builder, val, c_lit!("fneg")),

          (Not, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool))
          => LLVMBuildNot(builder, val, c_lit!("not")),

          _ => {
            let t_id = ir.reverse_type_lookup(val_tl);
            return ir.error(CompilationErrorData::InvalidUnary(ref_id, un_op.get_kind(), t_id))
          }
        };

        ir.stack_push((llvm, val_tl));
      },

      bin_op @ (
        Add | Sub | Mul | Div | Rem | And | Or | Xor | LShift | RShift | EQ | NEQ | LT | GT | LEQ | GEQ
      ) => {
        let (mut b_val, b_tl) = ir.stack_pop()?;
        let (mut a_val, a_tl) = ir.stack_pop()?;

        equal!(a_tl, b_tl; {
          let a_id = ir.reverse_type_lookup(a_tl);
          let b_id = ir.reverse_type_lookup(b_tl);
          ir.error(CompilationErrorData::BinaryMismatch(ref_id, bin_op.get_kind(), a_id, b_id))
        });

        let op_tl = a_tl;
        let op_ty = ir.context.get_type(op_tl);

        let bool_tl = ir.context.tl_intrinsic(Bool);

        use ctx::TypeData::*;
        use IntrinsicType::*;

        let (conv, conv_op_tl, conv_op_ty) = if let Pointer(_) = op_ty.data {
          let tl = ir.context.tl_intrinsic(U64);
          let ty = ir.context.get_type(tl);
          a_val = LLVMBuildPtrToInt(builder, a_val, ty.llvm_t(ir.context), c_lit!("a_op_int_to_ptr"));
          b_val = LLVMBuildPtrToInt(builder, b_val, ty.llvm_t(ir.context), c_lit!("b_op_int_to_ptr"));

          (true, tl, ty)
        } else {
          (false, op_tl, op_ty)
        };


        let (mut llvm, mut res_tl) = match (bin_op, &conv_op_ty.data) {
          (Add, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => (LLVMBuildAdd(builder, a_val, b_val, c_lit!("iadd")), conv_op_tl),

          (Add, Intrinsic(F32 | F64))
          => (LLVMBuildFAdd(builder, a_val, b_val, c_lit!("fadd")), conv_op_tl),

          (Sub, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => (LLVMBuildSub(builder, a_val, b_val, c_lit!("isub")), conv_op_tl),

          (Sub, Intrinsic(F32 | F64))
          => (LLVMBuildFSub(builder, a_val, b_val, c_lit!("fsub")), conv_op_tl),

          (Mul, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => (LLVMBuildMul(builder, a_val, b_val, c_lit!("imul")), conv_op_tl),

          (Mul, Intrinsic(F32 | F64))
          => (LLVMBuildFMul(builder, a_val, b_val, c_lit!("fmul")), conv_op_tl),

          (Div, Intrinsic(U8 | U16 | U32 | U64))
          => (LLVMBuildUDiv(builder, a_val, b_val, c_lit!("udiv")), conv_op_tl),

          (Div, Intrinsic(S8 | S16 | S32 | S64))
          => (LLVMBuildSDiv(builder, a_val, b_val, c_lit!("sdiv")), conv_op_tl),

          (Div, Intrinsic(F32 | F64))
          => (LLVMBuildFDiv(builder, a_val, b_val, c_lit!("fdiv")), conv_op_tl),

          (Rem, Intrinsic(U8 | U16 | U32 | U64))
          => (LLVMBuildURem(builder, a_val, b_val, c_lit!("urem")), conv_op_tl),

          (Rem, Intrinsic(S8 | S16 | S32 | S64))
          => (LLVMBuildSRem(builder, a_val, b_val, c_lit!("srem")), conv_op_tl),

          (Rem, Intrinsic(F32 | F64))
          => (LLVMBuildFRem(builder, a_val, b_val, c_lit!("frem")), conv_op_tl),


          (And, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool))
          => (LLVMBuildAnd(builder, a_val, b_val, c_lit!("and")), conv_op_tl),

          (Or, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool))
          => (LLVMBuildOr(builder, a_val, b_val, c_lit!("or")), conv_op_tl),

          (Xor, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 | Bool))
          => (LLVMBuildXor(builder, a_val, b_val, c_lit!("xor")), conv_op_tl),

          (LShift, Intrinsic(U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64))
          => (LLVMBuildShl(builder, a_val, b_val, c_lit!("lshift")), conv_op_tl),

          (RShift, Intrinsic(U8 | U16 | U32 | U64))
          => (LLVMBuildLShr(builder, a_val, b_val, c_lit!("rshift_l")), conv_op_tl),

          (RShift, Intrinsic(S8 | S16 | S32 | S64))
          => (LLVMBuildAShr(builder, a_val, b_val, c_lit!("rshift_a")), conv_op_tl),

          (cmp @ (EQ | NEQ), Intrinsic(Bool))
          => (
            LLVMBuildICmp(
              builder,
              int_predicate_from_instruction_kind(cmp.get_kind(), false).unwrap(),
              a_val, b_val,
              c_lit!("bcmp")
            ),
            bool_tl
          ),

          (cmp @ (EQ | NEQ | LT | GT | LEQ | GEQ), Intrinsic(U8 | U16 | U32 | U64))
          => (
            LLVMBuildICmp(
              builder,
              int_predicate_from_instruction_kind(cmp.get_kind(), false).unwrap(),
              a_val, b_val,
              c_lit!("ucmp")
            ),
            bool_tl
          ),

          (cmp @ (EQ | NEQ | LT | GT | LEQ | GEQ), Intrinsic(S8 | S16 | S32 | S64))
          => (
            LLVMBuildICmp(
              builder,
              int_predicate_from_instruction_kind(cmp.get_kind(), true).unwrap(),
              a_val, b_val,
              c_lit!("scmp")
            ),
            bool_tl
          ),

          (cmp @ (EQ | NEQ | LT | GT | LEQ | GEQ), Intrinsic(F32 | F64))
          => (
            LLVMBuildFCmp(
              builder,
              real_predicate_from_instruction_kind(cmp.get_kind()).unwrap(),
              a_val, b_val,
              c_lit!("fcmp")
            ),
            bool_tl
          ),

          _ => {
            let op_id = ir.reverse_type_lookup(op_tl);
            return ir.error(CompilationErrorData::InvalidBinary(ref_id, bin_op.get_kind(), op_id))
          }
        };

        if conv && res_tl != bool_tl {
          llvm = LLVMBuildIntToPtr(builder, llvm, op_ty.llvm_t(ir.context), c_lit!("res_int_to_ptr"));
          res_tl = op_tl;
        }

        ir.stack_push((llvm, res_tl));
      },

      &CallDirect(f_id) => {
        let fl = ir.get_function(ref_id, f_id)?;

        let function = ir.context.get_function(fl);

        let func_ty = ir.context.get_type(function.ty);
        let func_llvm = function.llvm;
        
        if let &ctx::TypeData::Function { ref parameters, result, is_var_arg } = &func_ty.data {
          assert!(!is_var_arg, "var arg functions NYI");

          // TODO this sucks
          let parameter_tls: Vec<_> = parameters.iter().rev().cloned().collect();

          let mut args = Vec::new();

          for param_tl in parameter_tls {
            let (arg, arg_tl) = ir.stack_pop()?;

            equal!(param_tl, arg_tl; ir.error(CompilationErrorData::InvalidCall(ref_id)));

            args.push(arg);
          }

          let ret_val = LLVMBuildCall(builder, func_llvm, args.as_mut_ptr(), args.len() as _, c_lit!("direct_call"));

          if let Some(result_tl) = result {
            ir.stack_push((ret_val, result_tl));
          }
        } else {
          unreachable!() // this has already been checked
        }
      },

      CallIndirect => {
        let (callee, callee_tl) = ir.stack_pop()?;

        let callee_ty = ir.context.get_type(callee_tl);

        let llvm_t = callee_ty.llvm_real_t();

        if let &ctx::TypeData::Function { ref parameters, result, is_var_arg } = &callee_ty.data {
          assert!(!is_var_arg, "var arg functions NYI");

          // TODO this sucks
          let parameter_tls: Vec<_> = parameters.iter().rev().cloned().collect();

          let mut args = Vec::new();

          for param_tl in parameter_tls {
            let (arg, arg_tl) = ir.stack_pop()?;

            equal!(param_tl, arg_tl; ir.error(CompilationErrorData::InvalidCall(ref_id)));

            args.push(arg);
          }

          let ret_val = LLVMBuildCall2(builder, llvm_t, callee, args.as_mut_ptr(), args.len() as _, c_lit!("indirect_call"));

          if let Some(result_tl) = result {
            ir.stack_push((ret_val, result_tl));
          }
        } else {
          return ir.error(CompilationErrorData::InvalidCall(ref_id))
        }
      },


      IfBlock(then_instrs, else_instrs) => {
        let (cond_val, cond_tl) = ir.stack_pop()?;

        equal!(cond_tl, ir.context.tl_intrinsic(IntrinsicType::Bool); {
          let t_id = ir.reverse_type_lookup(cond_tl);
          ir.error(CompilationErrorData::InvalidBranchPredicate(ref_id, t_id))
        });

        let llvm_ctx = ir.context.llvm.ctx;

        let finally = LLVMAppendBasicBlockInContext(llvm_ctx, ir.func_llvm, c_lit!("if_else_end"));

        let begin_then = LLVMAppendBasicBlockInContext(llvm_ctx, ir.func_llvm, c_lit!("then"));
        let (then_terminated, _) = generate_block(ir, begin_then, landing_pad, then_instrs)?;
        if !then_terminated { LLVMBuildBr(builder, finally); }

        let begin_else = LLVMAppendBasicBlockInContext(llvm_ctx, ir.func_llvm, c_lit!("else"));
        let (else_terminated, end_else) = generate_block(ir, begin_else, landing_pad, else_instrs)?;
        if !else_terminated { LLVMBuildBr(builder, finally); }

        LLVMPositionBuilderAtEnd(builder, block);

        LLVMBuildCondBr(builder, cond_val, begin_then, begin_else);

        if then_terminated && else_terminated {
          LLVMRemoveBasicBlockFromParent(finally);

          if instr_iter.peek().is_some() {
            return ir.error(CompilationErrorData::UnusedInstructions(ref_id))
          }

          return Ok((true, end_else))
        } else {
          LLVMMoveBasicBlockAfter(finally, end_else);
          LLVMPositionBuilderAtEnd(builder, finally);

          block = finally;
        }
      },

      LoopBlock(loop_instrs) => {
        let llvm_ctx = ir.context.llvm.ctx;

        let finally = LLVMAppendBasicBlockInContext(llvm_ctx, ir.func_llvm, c_lit!("if_else_end"));

        let begin_loop = LLVMAppendBasicBlockInContext(llvm_ctx, ir.func_llvm, c_lit!("loop"));
        let (loop_terminated, end_loop) = generate_block(ir, begin_loop, landing_pad, loop_instrs)?;
        if !loop_terminated { LLVMBuildBr(builder, begin_loop); }

        LLVMPositionBuilderAtEnd(builder, block);
        LLVMBuildBr(builder, begin_loop);

        if loop_terminated {
          LLVMRemoveBasicBlockFromParent(finally);

          if instr_iter.peek().is_some() {
            return ir.error(CompilationErrorData::UnusedInstructions(ref_id))
          }

          return Ok((true, end_loop))
        } else {
          LLVMMoveBasicBlockAfter(finally, end_loop);
          LLVMPositionBuilderAtEnd(builder, finally);

          block = finally;
        }
      },

      Break => {
        return if let Some(landing_pad) = landing_pad {
          if instr_iter.peek().is_some() {
            return ir.error(CompilationErrorData::UnusedInstructions(ref_id))
          }

          LLVMBuildBr(builder, landing_pad);
        
          Ok((true, block))
        } else {
          ir.error(CompilationErrorData::UnexpectedLoopControl(ref_id))
        }
      },

      Continue => {
        return if landing_pad.is_some() { // we wont be using the landing pad here, but we use it to validate that we are in a loop
          if instr_iter.peek().is_some() {
            return ir.error(CompilationErrorData::UnusedInstructions(ref_id))
          }

          LLVMBuildBr(builder, block);

          Ok((true, block))
        } else {
          ir.error(CompilationErrorData::UnexpectedLoopControl(ref_id))
        }
      },

      Return => {
        if instr_iter.peek().is_some() {
          return ir.error(CompilationErrorData::UnusedInstructions(ref_id))
        }

        generate_return(ir)?;

        return Ok((true, block))
      },
    }
  }

  Ok((false, block))
} }



fn generate_return (ir: &mut LocalIR) -> CompilationResult { unsafe {
  let (_, ret_tl, _ ) = ir.func_sig;

  if let Some(ret_tl) = ret_tl {
    let (val, tl) = ir.stack_pop()?;

    ty_ck(ir, ret_tl, tl)?;

    LLVMBuildRet(ir.context.llvm.builder, val);
  } else {
    LLVMBuildRetVoid(ir.context.llvm.builder);
  }
  
  Ok(())
} }



fn ty_ck (ir: &mut LocalIR, a: ctx::TypeLink, b: ctx::TypeLink) -> CompilationResult {
  if a == b {
    Ok(())
  } else {
    let ref_id = ir.ref_id;
    let a_id = ir.reverse_type_lookup(a);
    let b_id = ir.reverse_type_lookup(b);
    ir.error(CompilationErrorData::TypeMismatch(ref_id, a_id, b_id))
  }
}

fn int_predicate_from_instruction_kind (instr: bc::InstructionKind, signed_operand: bool) -> Option<LLVMIntPredicate> {
  use bc::InstructionKind::*;
  use LLVMIntPredicate::*;

  Some(match (instr, signed_operand) {
    (EQ, _) => LLVMIntEQ,
    (NEQ, _) => LLVMIntNE,
    (LT, true) => LLVMIntSLT,
    (LT, false) => LLVMIntULT,
    (GT, true) => LLVMIntSGT,
    (GT, false) => LLVMIntUGT,
    (LEQ, true) => LLVMIntSLE,
    (LEQ, false) => LLVMIntULE,
    (GEQ, true) => LLVMIntSGE,
    (GEQ, false) => LLVMIntUGE,
    _ => return None
  })
}

fn real_predicate_from_instruction_kind (instr: bc::InstructionKind) -> Option<LLVMRealPredicate> {
  use bc::InstructionKind::*;
  use LLVMRealPredicate::*;
  
  Some(match instr {
    EQ => LLVMRealOEQ,
    NEQ => LLVMRealONE,
    LT => LLVMRealOLT,
    GT => LLVMRealOGT,
    LEQ => LLVMRealOLE,
    GEQ => LLVMRealOGE,
    _ => return None
  })
}

fn generate_immediate (ctx: &Context, imm: ImmediateValue) -> (LLVMValueRef, ctx::TypeLink) { unsafe {
  use ImmediateValue::*;

  let tl = ctx.tl_intrinsic(imm.get_intrinsic_type());
  let llvm_t = ctx.get_type(tl).llvm_t(ctx);

  (match imm {
    Null => LLVMConstPointerNull(llvm_t),

    Bool(bool) => LLVMConstInt(llvm_t, bool as _, LLVM_FALSE),

    U8(u8)   => LLVMConstInt(llvm_t, u8  as _, LLVM_FALSE),
    U16(u16) => LLVMConstInt(llvm_t, u16 as _, LLVM_FALSE),
    U32(u32) => LLVMConstInt(llvm_t, u32 as _, LLVM_FALSE),
    U64(u64) => LLVMConstInt(llvm_t, u64 as _, LLVM_FALSE),

    S8(s8)   => LLVMConstInt(llvm_t, s8  as _, LLVM_TRUE),
    S16(s16) => LLVMConstInt(llvm_t, s16 as _, LLVM_TRUE),
    S32(s32) => LLVMConstInt(llvm_t, s32 as _, LLVM_TRUE),
    S64(s64) => LLVMConstInt(llvm_t, s64 as _, LLVM_TRUE),

    F32(f32) => LLVMConstReal(llvm_t, f32 as _),
    F64(f64) => LLVMConstReal(llvm_t, f64 as _),
  }, tl)
} }