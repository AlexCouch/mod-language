//! The semantic Analyzer and its supporting data structures

use crate::{
  collections::{ SlotMap, },
  ast::{ AST, Item, },
};

pub mod types;
pub mod values;
pub mod namespaces;

mod analysis;
pub use analysis::Analysis;

// pub mod modules;

use self::{
  types::{ Type, TypeKey, },
  values::{ Global, GlobalKey, Function, FunctionKey, },
  // modules::{ Module, ModuleKey, },
  namespaces::{ Namespace, },
};


/// State information for a semantic analysis session
#[allow(missing_docs)]
pub struct Analyzer {
  // pub modules:   SlotMap<ModuleKey, Module>,
  pub types:     SlotMap<TypeKey, Type>,
  pub globals:   SlotMap<GlobalKey, Global>,
  pub functions: SlotMap<FunctionKey, Function>,

  pub namespaces: Vec<Namespace>,
  
  // pub active_module: Option<ModuleKey>,
}

impl Default for Analyzer {
  #[inline] fn default () -> Self { Self::new() }
}

impl Analyzer {
  /// Create a new semantic Analyzer
  pub fn new () -> Self {
    Self {
      // modules:   SlotMap::new(),
      types:     SlotMap::new(),
      globals:   SlotMap::new(),
      functions: SlotMap::new(),

      namespaces: vec![ Namespace::new() ],

      // active_module: None
    }
  }

  /// Process a single top level Item of an AST using a semantic Analyzer
  pub fn analyze_item (&mut self, item: &Item) {
    item.analyze(self)
  }

  /// Process a full AST using a semantic Analyzer
  pub fn analyze_ast (&mut self, ast: &AST) {
    for item in ast.items().iter() {
      self.analyze_item(item)
    }
  }
}