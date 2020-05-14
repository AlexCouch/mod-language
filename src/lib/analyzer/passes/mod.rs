//! Analysis passes for the semantic Analyzer

use std::{
  collections::{ HashSet, },
};

use crate::{
  source::{ ASTKey, },
  ast::{ Item, },
};

use super::{ Analyzer, };


pub mod support_structures;

pub mod eval_helpers;
pub mod ty_helpers;

mod bind_top_level;
pub use bind_top_level::*;

mod resolve_pseudonyms;
pub use resolve_pseudonyms::*;

mod type_link_top_level;
pub use type_link_top_level::*;

mod generate_bodies;
pub use generate_bodies::*;



impl Analyzer {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self, ast: &mut Vec<Item>) {
    {
      let mut pseudonyms = Vec::new();

      bind_top_level(self, ast, &mut pseudonyms);

      resolve_pseudonyms(self, &mut pseudonyms);
    }

    let mut linked_module_asts: HashSet<ASTKey> = HashSet::default();

    type_link_top_level(self, &mut linked_module_asts, ast);

    generate_bodies(self, ast);

    assert!(self.get_active_namespace_key() == self.context.main_ns, "Internal error, a pass did not pop an active namespace");
  }
}