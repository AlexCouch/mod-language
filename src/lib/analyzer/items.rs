//! AnalysisItem and supporting structures

use core::hint::unreachable_unchecked;

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
};

use crate::util::make_key_type;

use super::{
  modules::{ Module, },
  types::{ Type, },
  values::{ Global, Function, },
};


/// A simple value enum representing the expected variant of an undefined AnalysisItem
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UndefinedKind {
  Unknown,
  Module,
  Type,
  Global,
  Function,
}

impl Display for UndefinedKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Unknown  => "Unknown",
      Self::Module   => "Module",
      Self::Type     => "Type",
      Self::Global   => "Global",
      Self::Function => "Function",
    })
  }
}

/// A simple value enum representing the variant of an AnalysisItem
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnalysisItemKind {
  Undefined(UndefinedKind),
  Module,
  Type,
  Global,
  Function,
}


impl AnalysisItemKind {
  fn into_undefined (self) -> UndefinedKind {
    match self {
      Self::Undefined(undefined) => undefined,

      Self::Module   => UndefinedKind::Module,
      Self::Type     => UndefinedKind::Type,
      Self::Global   => UndefinedKind::Global,
      Self::Function => UndefinedKind::Function,
    }
  }
}


impl Display for AnalysisItemKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Undefined(kind) => return write!(f, "Undefined {}", kind),

      Self::Module   => "Module",
      Self::Type     => "Type",
      Self::Global   => "Global",
      Self::Function => "Function",
    })
  }
}

/// A wrapper enum serving as either a place holder for an unknown value or a Module, Type, Global, or Function
#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum AnalysisItem {
  Undefined(UndefinedKind),
  Module(Module),
  Type(Type),
  Global(Global),
  Function(Function),
}

impl From<UndefinedKind> for AnalysisItem { #[inline] fn from (u: UndefinedKind) -> Self { Self::Undefined(u) } }
impl From<Module> for AnalysisItem { #[inline] fn from (m: Module) -> Self { Self::Module(m) } }
impl From<Type> for AnalysisItem { #[inline] fn from (t: Type) -> Self { Self::Type(t) } }
impl From<Global> for AnalysisItem { #[inline] fn from (g: Global) -> Self { Self::Global(g) } }
impl From<Function> for AnalysisItem { #[inline] fn from (f: Function) -> Self { Self::Function(f) } }

impl AnalysisItem {
  /// Get the AnalysisItemKind of an AnalysisItem
  pub fn kind (&self) -> AnalysisItemKind {
    match self {
      Self::Undefined(kind) => AnalysisItemKind::Undefined(*kind),
      Self::Module(_)   => AnalysisItemKind::Module,
      Self::Type(_)     => AnalysisItemKind::Type,
      Self::Global(_)   => AnalysisItemKind::Global,
      Self::Function(_) => AnalysisItemKind::Function,
    }
  }


  /// Determine if an AnalysisItem is Undefined
  pub fn is_undefined (&self) -> bool { if let Self::Undefined(_) = self { true } else { false } }
  
  /// Determine if an AnalysisItem is Module
  pub fn is_module (&self) -> bool { self.kind() == AnalysisItemKind::Module }
  
  /// Determine if an AnalysisItem is Type
  pub fn is_type (&self) -> bool { self.kind() == AnalysisItemKind::Type }
  
  /// Determine if an AnalysisItem is Global
  pub fn is_global (&self) -> bool { self.kind() == AnalysisItemKind::Global }
  
  /// Determine if an AnalysisItem is Function
  pub fn is_function (&self) -> bool { self.kind() == AnalysisItemKind::Function }


  /// Determine if an AnalysisItem is any of a set of AnalysisItemKinds
  pub fn is_kind (&self, kinds: &[AnalysisItemKind]) -> bool { kinds.contains(&self.kind()) }


  /// Determine if an AnalysisItem is a given AnalysisItemKind, its UndefinedKind equivalent, or totally Unknown
  pub fn is_equivalent_kind (&self, kind: AnalysisItemKind) -> bool {
    let self_kind = self.kind();

       self_kind == kind
    || self_kind.into_undefined() == kind.into_undefined()
    || self_kind == AnalysisItemKind::Undefined(UndefinedKind::Unknown)
  }


  /// Determine if an AnalysisItem is an uninitialized value that can be finalized with the given AnalysisItemKind
  pub fn will_finalize_to (&self, kind: AnalysisItemKind) -> bool {
    if let Some(&self_kind) = self.get_undefined_ref() {
         self_kind == UndefinedKind::Unknown
      || self_kind == kind.into_undefined()
    } else {
      false
    }
  }


  /// Convert an AnalysisItem to a presumed interior value
  pub fn get_undefined (self) -> Option<UndefinedKind> { if let AnalysisItem::Undefined(undefined) = self { Some(undefined) } else { None } }

  /// Convert an AnalysisItem to a presumed interior value
  pub fn get_module (self) -> Option<Module> { if let AnalysisItem::Module(module) = self { Some(module) } else { None } }

  /// Convert an AnalysisItem to a presumed interior value
  pub fn get_type (self) -> Option<Type> { if let AnalysisItem::Type(ty) = self { Some(ty) } else { None } }

  /// Convert an AnalysisItem to a presumed interior value
  pub fn get_global (self) -> Option<Global> { if let AnalysisItem::Global(global) = self { Some(global) } else { None } }

  /// Convert an AnalysisItem to a presumed interior value
  pub fn get_function (self) -> Option<Function> { if let AnalysisItem::Function(function) = self { Some(function) } else { None } }

  
  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_undefined_ref (&self) -> Option<&UndefinedKind> { if let AnalysisItem::Undefined(undefined) = self { Some(undefined) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_module_ref (&self) -> Option<&Module> { if let AnalysisItem::Module(module) = self { Some(module) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_type_ref (&self) -> Option<&Type> { if let AnalysisItem::Type(ty) = self { Some(ty) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_global_ref (&self) -> Option<&Global> { if let AnalysisItem::Global(global) = self { Some(global) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_function_ref (&self) -> Option<&Function> { if let AnalysisItem::Function(function) = self { Some(function) } else { None } }

  
  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_undefined_ref_mut (&mut self) -> Option<&mut UndefinedKind> { if let AnalysisItem::Undefined(undefined) = self { Some(undefined) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_module_ref_mut (&mut self) -> Option<&mut Module> { if let AnalysisItem::Module(module) = self { Some(module) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_type_ref_mut (&mut self) -> Option<&mut Type> { if let AnalysisItem::Type(ty) = self { Some(ty) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_global_ref_mut (&mut self) -> Option<&mut Global> { if let AnalysisItem::Global(global) = self { Some(global) } else { None } }

  /// Get a reference to the interior value of an AnalysisItem
  pub fn get_function_ref_mut (&mut self) -> Option<&mut Function> { if let AnalysisItem::Function(function) = self { Some(function) } else { None } }


  
  /// Convert an AnalysisItem to a presumed interior value
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_undefined_unchecked (self) -> UndefinedKind { if cfg!(debug_assertions) { self.get_undefined().unwrap() } else if let AnalysisItem::Undefined(undefined) = self { undefined } else { unreachable_unchecked() } }

  /// Convert an AnalysisItem to a presumed interior value
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_module_unchecked (self) -> Module { if cfg!(debug_assertions) { self.get_module().unwrap() } else if let AnalysisItem::Module(module) = self { module } else { unreachable_unchecked() } }

  /// Convert an AnalysisItem to a presumed interior value
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_type_unchecked (self) -> Type { if cfg!(debug_assertions) { self.get_type().unwrap() } else if let AnalysisItem::Type(ty) = self { ty } else { unreachable_unchecked() } }

  /// Convert an AnalysisItem to a presumed interior value
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_global_unchecked (self) -> Global { if cfg!(debug_assertions) { self.get_global().unwrap() } else if let AnalysisItem::Global(global) = self { global } else { unreachable_unchecked() } }

  /// Convert an AnalysisItem to a presumed interior value
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_function_unchecked (self) -> Function { if cfg!(debug_assertions) { self.get_function().unwrap() } else if let AnalysisItem::Function(function) = self { function } else { unreachable_unchecked() } }


  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_undefined_ref_unchecked (&self) -> &UndefinedKind { if cfg!(debug_assertions) { self.get_undefined_ref().unwrap() } else if let AnalysisItem::Undefined(undefined) = self { undefined } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_module_ref_unchecked (&self) -> &Module { if cfg!(debug_assertions) { self.get_module_ref().unwrap() } else if let AnalysisItem::Module(module) = self { module } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_type_ref_unchecked (&self) -> &Type { if cfg!(debug_assertions) { self.get_type_ref().unwrap() } else if let AnalysisItem::Type(ty) = self { ty } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_global_ref_unchecked (&self) -> &Global { if cfg!(debug_assertions) { self.get_global_ref().unwrap() } else if let AnalysisItem::Global(global) = self { global } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_function_ref_unchecked (&self) -> &Function { if cfg!(debug_assertions) { self.get_function_ref().unwrap() } else if let AnalysisItem::Function(function) = self { function } else { unreachable_unchecked() } }


  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_undefined_ref_unchecked_mut (&mut self) -> &mut UndefinedKind { if cfg!(debug_assertions) { self.get_undefined_ref_mut().unwrap() } else if let AnalysisItem::Undefined(undefined) = self { undefined } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_module_ref_unchecked_mut (&mut self) -> &mut Module { if cfg!(debug_assertions) { self.get_module_ref_mut().unwrap() } else if let AnalysisItem::Module(module) = self { module } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_type_ref_unchecked_mut (&mut self) -> &mut Type { if cfg!(debug_assertions) { self.get_type_ref_mut().unwrap() } else if let AnalysisItem::Type(ty) = self { ty } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_global_ref_unchecked_mut (&mut self) -> &mut Global { if cfg!(debug_assertions) { self.get_global_ref_mut().unwrap() } else if let AnalysisItem::Global(global) = self { global } else { unreachable_unchecked() } }

  /// Get a reference to the interior value of an AnalysisItem
  /// # Safety
  /// This will only assert that the item is the correct variant if debug assertions are enabled,
  /// otherwise it is up to the caller to determine if this is a safe transformation
  pub unsafe fn get_function_ref_unchecked_mut (&mut self) -> &mut Function { if cfg!(debug_assertions) { self.get_function_ref_mut().unwrap() } else if let AnalysisItem::Function(function) = self { function } else { unreachable_unchecked() } }

}


make_key_type! {
  /// A Key linking a Module, Type, Global, or Function in an Analyzer
  pub struct AnalysisKey;
}