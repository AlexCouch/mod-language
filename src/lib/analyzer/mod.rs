//! The semantic Analyzer and its supporting data structures

use crate::{
  some,
  util::{ Unref, },
  collections::{ SlotMap, },
  source::{ MessageKind, SourceRegion, },
  ast::{ AST, },
};

pub mod types;
pub mod values;
pub mod namespaces;
pub mod modules;
pub mod items;
mod passes;

// pub mod modules;

use self::{
  types::{ TypeData, },
  values::{ Global, Function, },
  modules::{ Module, ModuleItem, },
  namespaces::{ Namespace, },
  items::{ UndefinedKind, AnalysisItem, AnalysisKey, },
};




/// State information for a semantic analysis session
#[allow(missing_docs)]
pub struct Analyzer<'a> {
  pub ast: &'a AST<'a>,
  pub items:   SlotMap<AnalysisKey, AnalysisItem>,
  pub namespaces: Vec<Namespace>,
  pub active_module: AnalysisKey,
}


impl<'a> Analyzer<'a> {
  /// Create a new semantic Analyzer
  pub fn new (ast: &'a AST) -> Self {
    let mut items = SlotMap::new();
    let active_module = items.insert(AnalysisItem::Module(Module::new(None, "root".to_owned())));

    Self {
      ast,
      items,
      namespaces: vec![ Namespace::new() ],
      active_module
    }
  }



  /// Create a user-directed Message in the Source of the AST of an Analyzer,
  /// with a custom line and column origin
  pub fn message_at (&self, origin: SourceRegion, kind: MessageKind, content: String) {
    self.ast.stream.source.message(
      Some(origin),
      kind,
      content
    )
  }

  /// Create a user-directed Error Message in the Source of the AST of an Analyzer,
  /// with a custom line and column origin
  pub fn error_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Error, content)
  }

  /// Create a user-directed Warning Message in the Source of the AST of an Analyzer,
  /// with a custom line and column origin
  pub fn warning_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Warning, content)
  }

  /// Create a user-directed Notice Message in the Source of the AST of an Analyzer,
  /// with a custom line and column origin
  pub fn notice_at (&self, origin: SourceRegion, content: String) {
    self.message_at(origin, MessageKind::Notice, content)
  }



  /// Register a TypeData and get a Key referencing it, or get the Key to a matching existing Type
  pub fn get_type_key (&mut self, td: TypeData) -> AnalysisKey {
    let new_ty = AnalysisItem::Type(td.into());

    for (key, ty) in self.items.pair_iter() {
      if ty == &new_ty {
        return *key
      }
    }

    self.items.insert(new_ty)
  }


  /// Get a reference to the active module in an Analyzer
  pub fn active_module (&self) -> &Module {
    unsafe { self.items.get_unchecked(self.active_module).get_module_ref_unchecked() }
  }

  /// Get a mutable reference to the active module in an Analyzer
  pub fn active_module_mut (&mut self) -> &mut Module {
    unsafe { self.items.get_unchecked_mut(self.active_module).get_module_ref_unchecked_mut() }
  }


  /// Set the import source for a ModuleItem in an Analyzer's active Module
  pub fn set_import_attribution_in_active_module<S: AsRef<str>> (&mut self, name: &S, import: Option<AnalysisKey>) {
    some!(self.active_module_mut().get_mut(name.as_ref())).import = import;
  }

  /// Try to get a ModuleItem by searching for a name in an Analyzer's active Module
  pub fn get_binding_in_active_module<S: AsRef<str>> (&self, name: &S) -> Option<ModuleItem> {
    self.active_module().get(name.as_ref()).unref()
  }


  /// Try to get an AnalysisItem by searching for a name in an Analyzer's active Module
  pub fn get_item_in_active_module<S: AsRef<str>> (&self, name: &S) -> Option<&AnalysisItem> {
    if let Some(item) = self.get_binding_in_active_module(name) { unsafe { Some(self.items.get_unchecked(item.key)) } }
    else { None }
  }

  /// Try to get an AnalysisItem by searching for a name in an Analyzer's active Module
  pub fn get_item_mut_in_active_module<S: AsRef<str>> (&mut self, name: &S) -> Option<&mut AnalysisItem> {
    if let Some(item) = self.get_binding_in_active_module(name) { let key = item.key; unsafe { Some(self.items.get_unchecked_mut(key)) } }
    else { None }
  }

  /// Try to get an AnalysisItem and its associated Key by searching for a name in an Analyzer's active Module
  pub fn get_pair_in_active_module<S: AsRef<str>> (&self, name: &S) -> Option<(ModuleItem, &AnalysisItem)> {
    if let Some(item) = self.get_binding_in_active_module(name) { unsafe { Some((item, self.items.get_unchecked(item.key))) } }
    else { None }
  }

  /// Try to get an AnalysisItem and its associated ModuleItem by searching for a name in an Analyzer's active Module
  pub fn get_pair_mut_in_active_module<S: AsRef<str>> (&mut self, name: &S) -> Option<(ModuleItem, &mut AnalysisItem)> {
    if let Some(item) = self.get_binding_in_active_module(name) { unsafe { Some((item, self.items.get_unchecked_mut(item.key))) } }
    else { None }
  }


  /// Try to get an AnalysisKey by searching for a name in an Analyzer's stack of Namespaces
  pub fn get_key_in_namespace<S: AsRef<str>> (&self, name: &S) -> Option<AnalysisKey> {
    let name = name.as_ref();

    for space in self.namespaces.iter().rev() {
      let entry = space.get(name);

      if entry.is_some() { return entry.unref() }
    }

    None
  }

  /// Try to get an AnalysisItem by searching for a name in an Analyzer's stack of Namespaces
  pub fn get_item_in_namespace<S: AsRef<str>> (&self, name: &S) -> Option<&AnalysisItem> {
    if let Some(key) = self.get_key_in_namespace(name) { unsafe { Some(self.items.get_unchecked(key)) } }
    else { None }
  }

  /// Try to get an AnalysisItem by searching for a name in an Analyzer's stack of Namespaces
  pub fn get_item_mut_in_namespace<S: AsRef<str>> (&mut self, name: &S) -> Option<&mut AnalysisItem> {
    if let Some(key) = self.get_key_in_namespace(name) { unsafe { Some(self.items.get_unchecked_mut(key)) } }
    else { None }
  }

  /// Try to get an AnalysisItem and its associated Key by searching for a name in an Analyzer's stack of Namespaces
  pub fn get_pair_in_namespace<S: AsRef<str>> (&self, name: &S) -> Option<(AnalysisKey, &AnalysisItem)> {
    if let Some(key) = self.get_key_in_namespace(name) { unsafe { Some((key, self.items.get_unchecked(key))) } }
    else { None }
  }

  /// Try to get an AnalysisItem and its associated Key by searching for a name in an Analyzer's stack of Namespaces
  pub fn get_pair_mut_in_namespace<S: AsRef<str>> (&mut self, name: &S) -> Option<(AnalysisKey, &mut AnalysisItem)> {
    if let Some(key) = self.get_key_in_namespace(name) { unsafe { Some((key, self.items.get_unchecked_mut(key))) } }
    else { None }
  }


  /// Create or finalize an item in the current Analyzer context
  pub fn create_item<S: AsRef<str>, I: Into<AnalysisItem>> (&mut self, name: &S, import: Option<AnalysisKey>, item: I, origin: SourceRegion) -> Option<AnalysisKey> {
    let item = item.into();
    let name = name.as_ref();

    if let Some((ModuleItem { import: existing_import, key }, existing_item)) = self.get_pair_mut_in_active_module(&name) {
      // Item name is already bound, need to find if the existing binding is compatible with the new one
      let existing_kind = existing_item.kind();

      if item.is_undefined() && existing_item.is_undefined() {
        // A secondary reference to some item that has been mentioned before but not defined
        let undef_kind = unsafe { item.get_undefined_ref_unchecked() };
        let existing_undef_kind = unsafe { existing_item.get_undefined_ref_unchecked_mut() };

        if existing_undef_kind == &UndefinedKind::Unknown
        || existing_undef_kind == undef_kind {
          // Our undefines are compatible, but we must check that our imports are compatible
          if import.is_none() {
            // Okay, no import defined for new undefined
            *existing_undef_kind = *undef_kind;
            Some(key)
          } else if existing_import == import
                 || existing_import.is_none() {
            // Okay, imports are identical or there is no existing
            *existing_undef_kind = *undef_kind;
            self.set_import_attribution_in_active_module(&name, import);
            Some(key)
          } else {
            // Error, same name attributed to multiple import sources
            self.error_at(
              origin,
              format!(
                "Cannot bind name `{}` to multiple imports",
                name
              )
            );

            None
          }
        } else {
          // Error, item is undefined but there are unmet expectations
          self.error_at(
            origin,
            format!(
              "Cannot bind name `{}` to a {}, it is already bound to a {}",
              name,
              item.kind(),
              existing_kind,
            )
          );

          None
        }
      } else if existing_item.will_finalize_to(item.kind()) {
        // A definition for some value that has been mentioned before but not defined until now
        if let Some(owning_module_key) = existing_import {
          // Error, this reference is known to be an import
          let owning_module = self.items.get(owning_module_key).unwrap().get_module_ref().unwrap();

          self.error_at(
            origin,
            format!(
              "Cannot define `{}` here, it is imported from Module `{}`",
              name,
              owning_module.name
            )
          );

          None
        } else {
          // Okay, we can define the item and set our import value
          *existing_item = item;
          self.set_import_attribution_in_active_module(&name, import);
          Some(key)
        }
      } else {
        // Error, the item is already defined
        self.error_at(
          origin,
          format!(
            "Cannot create {} with the name `{}`, a {} with the same name already exists in this module",
            item.kind(),
            name,
            existing_kind
          )
        );

        None
      }
    } else {
      // Okay, item is totally new
      let key = self.items.insert(item);

      let module = self.active_module_mut();

      module.insert(name.to_owned(), ModuleItem { import, key });
      
      Some(key)
    }
  }


  /// Process a full AST using a semantic Analyzer
  /// 
  /// Note this cannot be done element-wise like previous steps in the pipeline,
  /// because the Analyzer must use a series of Passes which traverse the full AST in sequence
  pub fn analyze_ast (&mut self) {
    let passes = Self::get_passes();
    
    for mut pass in passes {
      pass.process(self)
    }
  }


  /// Create a new Analyzer and run its Analysis passes in a single step
  pub fn analyze (ast: &'a AST) {
    Self::new(ast).analyze_ast()
  }
}