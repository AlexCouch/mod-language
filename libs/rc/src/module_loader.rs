//! The bytecode compiler and support structures

use std::{
  mem::{ transmute, },
  collections::{ HashMap, },
  fmt::{ Display, Formatter, Result as FMTResult },
  ops::{ Deref, DerefMut, },
};

use llvm_sys::{
  core::{
    LLVMRunFunctionPassManager,
  },
};

use mod_utils::{ some, none, equal, UnwrapUnchecked, };
use mod_bytecode::{ self as bc, Version, };

use crate::{
  context::{ self as ctx, Context, },
  generator::{ generate_function_body, },
};


/// Data for an error that occurred while compiling bytecode
pub enum CompilationErrorData {
  /// Some unexpected error occurred
  Unexpected,
  /// An item referenced an invalid id
  InvalidID(bc::GenericID, bc::GenericID),
  /// Multiple definitions bound to the same id
  DuplicateDefinition(bc::GenericID),
  /// Multiple exports bound to the same name
  DuplicateExport(String),
  /// The ID associated with an export path was invalid
  InvalidExportID(String, bc::GenericID),
  /// Found no existing module with the given name and version of an import
  MissingImport(String, Version),
  /// An import path inside an import module was invalid
  InvalidImportPath(String),
  /// Imported value evaluated to a different type than was expected, or an invalid type
  InvalidImportType(String),
  /// A module with the same name has already been loaded in the target Context
  ModuleAlreadyExists(String),
  /// A module with the same name and a different version has already been loaded in the target Context
  ModuleExistsWithDifferentVersion(String, Version, Version),
  /// A function had no value on its stack for an instruction that called stack.pop
  StackUnderflow(bc::GenericID),
  /// A function had no local variable with the given id
  InvalidLocalID(bc::GenericID, bc::LocalID),
  /// A value at the reference location was expected to be type A, but was type B
  TypeMismatch(bc::GenericID, bc::TypeID, bc::TypeID),
  /// LLVM Function validation failed
  ValidationFailed(bc::GenericID),
  /// An instruction either tried to get an element of a non-structural type, or tried to get an out-of-range element
  GetInvalidElement(bc::GenericID),
  /// An instruction tried to perform an invalid cast
  InvalidCast(bc::GenericID, bc::TypeID, bc::TypeID),
  /// An instruction tried to load a value that was not a value pointer
  InvalidLoad(bc::GenericID, bc::TypeID),
  /// An instruction either tried to store a value that was not the same type as a pointer's value type,
  /// or tried to store to a value that was not a pointer
  InvalidStore(bc::GenericID, bc::TypeID, bc::TypeID),
  /// An instruction tried to perform a unary operation on an invalid operand
  InvalidUnary(bc::GenericID, bc::InstructionKind, bc::TypeID),
  /// An instruction tried to perform a binary operation on operands with different types
  BinaryMismatch(bc::GenericID, bc::InstructionKind, bc::TypeID, bc::TypeID),
  /// An instruction tried to perform a binary operation on an invalid operand type
  InvalidBinary(bc::GenericID, bc::InstructionKind, bc::TypeID),
  /// An instruction tried to perform a call with an invalid argument operand, without enough operands on the stack,
  /// or an invalid function address
  InvalidCall(bc::GenericID),
  /// A branch found a predicate operand of a type other than bool
  InvalidBranchPredicate(bc::GenericID, bc::TypeID),
  /// A block contained instructions after a terminating instruction such as return or continue
  UnusedInstructions(bc::GenericID),
  /// An instruction tried to break or continue while not within a loop block
  UnexpectedLoopControl(bc::GenericID),
}

/// An error that occurred while compiling bytecode
pub struct CompilationError {
  /// The module of a compilation set in which an error occurred
  pub module_origin: String,
  /// The specific data for the error
  pub data: CompilationErrorData,
}

/// A Result with no Ok value and CompilationError as its Err
pub type CompilationResult = Result<(), CompilationError>;


struct PathDisplayer<'a>(&'a[&'a str]);

impl Display for PathDisplayer<'_> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    let mut iter = self.0.iter().peekable();

    while let Some(&s) = iter.next() {
      Display::fmt(s, f)?;

      if iter.peek().is_some() {
        write!(f, "::")?;
      }
    }

    Ok(())
  }
}

/// Contains links to values being processed by the compiler for a specific bytecode module
pub(crate) struct IRModule {
  name: String,
  version: Version,

  types: HashMap<bc::TypeID, ctx::TypeLink>,
  globals: HashMap<bc::GlobalID, ctx::GlobalLink>,
  functions: HashMap<bc::FunctionID, ctx::FunctionLink>,
}

impl IRModule {
  fn new (name: String, version: Version) -> Self {
    Self {
      name,
      version,
      
      types: HashMap::default(),
      globals: HashMap::default(),
      functions: HashMap::default(),
    }
  }

  pub(crate) fn reverse_type_lookup (&self, tl: ctx::TypeLink) -> bc::TypeID {
    for (&l_id, &l_tl) in self.types.iter() {
      if l_tl == tl { return l_id }
    }

    panic!("Failed to find type in reverse lookup");
  }

  // pub(crate) fn reverse_global_lookup (&self, gl: ctx::GlobalLink) -> bc::GlobalID {
  //   for (&l_id, &l_gl) in self.globals.iter() {
  //     if l_gl == gl { return l_id }
  //   }

  //   panic!("Failed to find global in reverse lookup");
  // }

  // pub(crate) fn reverse_function_lookup (&self, fl: ctx::FunctionLink) -> bc::FunctionID {
  //   for (&l_id, &l_fl) in self.functions.iter() {
  //     if l_fl == fl { return l_id }
  //   }

  //   panic!("Failed to find function in reverse lookup");
  // }
}

/// Contains links to values being processed by the compiler
pub(crate) struct IR<'a> {
  pub(crate) context: &'a mut Context,

  modules: Vec<IRModule>,

  active_module: Option<usize>,

  global_base: usize,
  function_base: usize,
}

impl<'a> IR<'a> {
  fn new (context: &'a mut Context) -> Self {
    let global_base = context.global_count();
    let function_base = context.function_count();

    Self {
      context,

      modules: Vec::default(),

      active_module: None,

      global_base,
      function_base,
    }
  }

  pub(crate) fn create_module (&mut self, name: String, version: Version) -> CompilationResult {
    use CompilationErrorData::*;
  
    let dup_version =
      self.get_module(&name)
        .map(|dup_ir| dup_ir.version)
        .or_else(||
          self.context.find_module(&name)
          .map(|dup_link| self.context.get_module(dup_link).version)
        );

    if let Some(dup_module_version) = dup_version {
      return self.error(if dup_module_version != version {
        ModuleExistsWithDifferentVersion(name, version, dup_module_version)
      } else {
        ModuleAlreadyExists(name)
      })
    }

    let index = self.modules.len();

    self.modules.push(IRModule::new(name, version));

    self.active_module.replace(index).unwrap_none();

    Ok(())
  }

  pub(crate) fn set_active_module<S: AsRef<str>> (&mut self, s: S) {
    self.active_module.replace(self.get_module_index(s).unwrap()).unwrap_none()
  }

  pub(crate) fn unset_active_module (&mut self) {
    self.active_module.take().unwrap();
  }

  pub(crate) fn get_module<S: AsRef<str>> (&self, s: S) -> Option<&IRModule> {
    Some(unsafe { self.modules.get_unchecked(self.get_module_index(s)?) })
  }

  pub(crate) fn get_module_index<S: AsRef<str>> (&self, s: S) -> Option<usize> {
    let s = s.as_ref();

    for (i, module) in self.modules.iter().enumerate() {
      if module.name == s {
        return Some(i)
      }
    }

    None
  }

  unsafe fn recover_error (&mut self) {
    self.context.purge_globals(self.global_base);
    self.context.purge_functions(self.function_base);
  }

  pub(crate) fn error<T> (&mut self, data: CompilationErrorData) -> Result<T, CompilationError> {
    unsafe { self.recover_error() }
    Err(CompilationError { module_origin: self.active_module_ref().name.to_owned(), data })
  }

  pub(crate) fn active_module_ref (&self) -> &IRModule {
    self.modules.get(self.active_module.unwrap()).unwrap()
  }

  pub(crate) fn active_module_mut (&mut self) -> &mut IRModule {
    self.modules.get_mut(self.active_module.unwrap()).unwrap()
  }

  pub(crate) fn get_type<G: Into<bc::GenericID>> (&mut self, ref_from: G, id: bc::TypeID) -> Result<ctx::TypeLink, CompilationError> {
    if let Some(link) = self.active_module_ref().types.get(&id) {
      Ok(*link)
    } else {
      self.error(CompilationErrorData::InvalidID(ref_from.into(), id.into()))
    }
  }

  pub(crate) fn get_global<G: Into<bc::GenericID>> (&mut self, ref_from: G, id: bc::GlobalID) -> Result<ctx::GlobalLink, CompilationError> {
    if let Some(link) = self.active_module_ref().globals.get(&id) {
      Ok(*link)
    } else {
      self.error(CompilationErrorData::InvalidID(ref_from.into(), id.into()))
    }
  }

  pub(crate) fn get_function<G: Into<bc::GenericID>> (&mut self, ref_from: G, id: bc::FunctionID) -> Result<ctx::FunctionLink, CompilationError> {
    if let Some(link) = self.active_module_ref().functions.get(&id) {
      Ok(*link)
    } else {
      self.error(CompilationErrorData::InvalidID(ref_from.into(), id.into()))
    }
  }
}

impl<'a> Deref for IR<'a> {
  type Target = IRModule;
  fn deref (&self) -> &IRModule { self.active_module_ref() }
}

impl<'a> DerefMut for IR<'a> {
  fn deref_mut (&mut self) -> &mut IRModule { self.active_module_mut() }
}




/// Load a single module into a Context
/// 
/// Note this is just a convenience wrapper for `load_modules`,
/// if you need to support cross-dependency you should use that function instead
pub fn load_module (context: &mut Context, bc_module: bc::Module) -> CompilationResult {
  load_modules(context, &[ bc_module ])
}


/// Load a series of modules into a Context simultaneously, allowing cross-module linking between them
pub fn load_modules (context: &mut Context, bc_modules: &[bc::Module]) -> CompilationResult {
  let mut ir = IR::new(context);

  for bc_module in bc_modules.iter() {
    declare_module(&mut ir, bc_module)?;
  }

  for bc_module in bc_modules.iter() {
    link_module(&mut ir, bc_module)?;
  }

  for bc_module in bc_modules.iter() {
    define_module(&mut ir, bc_module)?;
  }

  Ok(())
}



fn declare_module (ir: &mut IR, bc_module: &bc::Module) -> CompilationResult {
  ir.create_module(bc_module.name.to_owned(), bc_module.version)?;


  link_types(ir, &bc_module.types)?;


  create_global_declarations(ir, &bc_module.globals)?;

  create_function_declarations(ir, &bc_module.functions)?;
  

  let mut exports: Vec<ctx::Export> = Vec::default();

  link_exports(ir, &mut exports, &bc_module.exports)?;


  let module = ctx::Module { name: bc_module.name.to_owned(), version: bc_module.version, exports };

  ir.context.add_module(module);


  ir.unset_active_module();

  Ok(())
}


fn link_module (ir: &mut IR, bc_module: &bc::Module) -> CompilationResult {
  ir.set_active_module(&bc_module.name);

  link_imports(ir, &bc_module.imports)?;

  ir.unset_active_module();

  Ok(())
}


fn define_module (ir: &mut IR, bc_module: &bc::Module) -> CompilationResult {
  ir.set_active_module(&bc_module.name);

  create_global_initializers(ir, &bc_module.globals)?;

  create_function_bodies(ir, &bc_module.functions)?;

  ir.unset_active_module();

  Ok(())
}



fn link_types (ir: &mut IR, bc_types: &[bc::Type]) -> CompilationResult {
  use CompilationErrorData::*;

  let mut temp_tls: Vec<ctx::TypeLink> = Vec::default();

  for bc_type in bc_types.iter() {
    let bc_type: &bc::Type = bc_type;

    let link = match &bc_type.data {
      &bc::TypeData::Intrinsic(ity) => ir.context.tl_intrinsic(ity),

      &bc::TypeData::Pointer(value_id) => {
        let link = ir.get_type(bc_type.id, value_id)?;
        ir.context.tl_pointer(link)
      },

      bc::TypeData::Struct(field_ids) => {
        temp_tls.clear();

        for &field_id in field_ids.iter() {
          temp_tls.push(ir.get_type(bc_type.id, field_id)?);
        }

        ir.context.tl_structure(temp_tls.as_slice(), false)
      },

      bc::TypeData::Function { parameters, result } => {
        temp_tls.clear();

        for &param_id in parameters.iter() {
          temp_tls.push(ir.get_type(bc_type.id, param_id)?)
        }

        let result_tl = if let &Some(result_id) = result {
          Some(ir.get_type(bc_type.id, result_id)?)
        } else {
          None
        };

        ir.context.tl_function(temp_tls.as_slice(), result_tl, false)
      },
    };

    none!(ir.types.insert(bc_type.id, link); ir.error(DuplicateDefinition(bc_type.id.into())))
  }

  Ok(())
}


fn discard_lifetime<'a, T: ?Sized> (v: &'a T) -> &'static T {
  unsafe { transmute(v) }
}

fn link_imports (ir: &mut IR, bc_imports: &[bc::ImportModule]) -> CompilationResult {
  use CompilationErrorData::*;

  let mut temp_path: Vec<&'static str> = Vec::default();

  for import_module in bc_imports.iter() {
    temp_path.clear();

    temp_path.push(discard_lifetime(import_module.name.as_str()));
    
    if let Some(module_link) = ir.context.find_module(&import_module.name) {
      let module = ir.context.get_module(module_link);

      if module.version != import_module.version {
        return ir.error(MissingImport(import_module.name.to_owned(), import_module.version))
      }

      fn traverse (ir: &mut IR, bc_imports: &[bc::Import], temp_path: &mut Vec<&'static str>) -> CompilationResult {
        macro_rules! path_string { () => { PathDisplayer(temp_path).to_string() }; }
        for import in bc_imports.iter() {
          let base = temp_path.len();
      
          temp_path.push(discard_lifetime(import.name.as_str()));

          match &import.data {
            bc::ImportData::Namespace(bc_imports) => traverse(ir, bc_imports, temp_path)?,
            bc::ImportData::Global(g_id, t_id) => {
              let link = some!(
                ir.context.resolve_path(temp_path.as_slice());
                ir.error(InvalidImportPath(path_string!()))
              );
              
              if let ctx::GenericLink::Global(link) = link {
                let global = ir.context.get_global(link);
                let tl = *some!(ir.types.get(t_id); ir.error(InvalidImportType(path_string!())));

                equal!(global.ty, tl; ir.error(InvalidImportType(path_string!())));

                none!(ir.globals.insert(*g_id, link); ir.error(DuplicateDefinition((*g_id).into())))
              } else {
                return ir.error(InvalidImportType(path_string!()))
              }
            },
            bc::ImportData::Function(g_id, t_id) => {
              let link = some!(
                ir.context.resolve_path(temp_path.as_slice());
                ir.error(InvalidImportPath(path_string!()))
              );
              
              if let ctx::GenericLink::Function(link) = link {
                let function = ir.context.get_function(link);
                let tl = *some!(ir.types.get(t_id); ir.error(InvalidImportType(path_string!())));

                equal!(function.ty, tl; ir.error(InvalidImportType(path_string!())));

                none!(ir.functions.insert(*g_id, link); ir.error(DuplicateDefinition((*g_id).into())))
              } else {
                return ir.error(InvalidImportType(path_string!()))
              }
            }
          }

          temp_path.truncate(base);
        }

        Ok(())
      }

      traverse(ir, import_module.items.as_slice(), &mut temp_path)?;
    } else {
      return ir.error(MissingImport(import_module.name.to_owned(), import_module.version))
    }
  }

  Ok(())
}


fn create_global_declarations (ir: &mut IR, bc_globals: &[bc::Global]) -> CompilationResult {
  use CompilationErrorData::*;

  for bc_global in bc_globals.iter() {
    let t_link = ir.get_type(bc_global.id, bc_global.ty)?;

    let g_link = ir.context.create_global(t_link);

    none!(ir.globals.insert(bc_global.id, g_link); ir.error(DuplicateDefinition(bc_global.id.into())));
  }

  Ok(())
}


fn create_function_declarations (ir: &mut IR, bc_functions: &[bc::Function]) -> CompilationResult {
  use CompilationErrorData::*;

  for bc_function in bc_functions.iter() {
    let t_link = ir.get_type(bc_function.id, bc_function.ty)?;

    let ty = ir.context.get_type(t_link);

    if !matches!(&ty.data, ctx::TypeData::Function { .. }) {
      return ir.error(InvalidID(bc_function.id.into(), bc_function.ty.into()));
    }

    let f_link = ir.context.create_function(t_link);

    none!(ir.functions.insert(bc_function.id, f_link); ir.error(DuplicateDefinition(bc_function.id.into())));
  }

  Ok(())
}


fn link_exports (ir: &mut IR, ctx_exports: &mut Vec<ctx::Export>, bc_exports: &[bc::Export]) -> CompilationResult {
  use CompilationErrorData::*;

  let mut temp_path: Vec<&'static str> = Vec::default();

  fn traverse (ir: &mut IR, ctx_exports: &mut Vec<ctx::Export>, bc_exports: &[bc::Export], temp_path: &mut Vec<&'static str>) -> CompilationResult {
    macro_rules! path_string { () => { PathDisplayer(temp_path).to_string() }; }
    for bc_export in bc_exports.iter() {

      let existing_export_index = ctx_exports.iter().enumerate().find(|(_, ctx_export)| ctx_export.name == bc_export.name).map(|(i, _)| i);

      temp_path.push(discard_lifetime(bc_export.name.as_str()));

      let base = temp_path.len();

      match &bc_export.data {
        bc::ExportData::Namespace(bc_exports) => {
          let existing_export = unsafe { if let Some(i) = existing_export_index {
            ctx_exports.get_mut(i).unwrap_unchecked()
          } else {
            ctx_exports.push(ctx::Export { name: bc_export.name.to_owned(), data: ctx::ExportData::Namespace(Vec::default()) });
            ctx_exports.last_mut().unwrap_unchecked()
          } };

          let ctx_exports = if let ctx::ExportData::Namespace(ctx_exports) = &mut existing_export.data {
            ctx_exports
          } else {
            return ir.error(DuplicateExport(path_string!()))
          };

          traverse(ir, ctx_exports, bc_exports, temp_path)?;
        },

        bc::ExportData::Global(g_id) => {
          none!(existing_export_index; ir.error(DuplicateExport(path_string!())));

          let g_link = *some!(ir.globals.get(g_id); ir.error(InvalidExportID(path_string!(), (*g_id).into())));

          ctx_exports.push(ctx::Export { name: bc_export.name.to_owned(), data: ctx::ExportData::Global(g_link) });
        },

        bc::ExportData::Function(f_id) => {
          none!(existing_export_index; ir.error(DuplicateExport(path_string!())));

          let f_link = *some!(ir.functions.get(f_id); ir.error(InvalidExportID(path_string!(), (*f_id).into())));

          ctx_exports.push(ctx::Export { name: bc_export.name.to_owned(), data: ctx::ExportData::Function(f_link) });
        }
      }

      temp_path.truncate(base);
    }

    Ok(())
  }

  traverse(ir, ctx_exports, bc_exports, &mut temp_path)
}


fn create_global_initializers (ir: &mut IR, bc_globals: &[bc::Global]) -> CompilationResult {
  for bc_global in bc_globals.iter() {
    let g_link = *ir.globals.get(&bc_global.id).unwrap();
    
    if !bc_global.initializer.is_empty() {
      let g_tl = ir.context.get_global(g_link).ty;
      let void_fn_tl = ir.context.tl_function(&[], Some(g_tl), false);
      
      let init_fl = ir.context.create_function(void_fn_tl);
      
      ir.context.set_global_initializer(g_link, init_fl);

      generate_function_body(ir, bc_global.id.into(), init_fl, bc_global.initializer.as_slice())?;
    }
  }

  Ok(())
}


fn create_function_bodies (ir: &mut IR, bc_functions: &[bc::Function]) -> CompilationResult {
  for bc_function in bc_functions.iter() {
    let f_link = *ir.functions.get(&bc_function.id).unwrap();
    
    generate_function_body(ir, bc_function.id.into(), f_link, bc_function.body.as_slice())?;
  }

  Ok(())
}