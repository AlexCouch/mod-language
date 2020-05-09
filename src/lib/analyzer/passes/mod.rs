//! Analysis passes for the semantic Analyzer

use crate::{
  ast::{ Item, },
};

use super::{ Analyzer, };


pub mod alias;

pub mod eval_helpers;
pub mod ty_helpers;

mod bind_top_level;
pub use bind_top_level::*;

mod resolve_aliases;
pub use resolve_aliases::*;

mod type_link_top_level;
pub use type_link_top_level::*;

mod generate_bodies;
pub use generate_bodies::*;



impl Analyzer {
  /// Runs each pass of analysis session in sequence
  pub fn run_passes (&mut self, ast: &mut Vec<Item>) {
    {
      let mut aliases = Vec::new();

      bind_top_level(self, ast, &mut aliases);

      resolve_aliases(self, &mut aliases);
    }

    type_link_top_level(self, ast);

    generate_bodies(self, ast);

    assert!(self.get_active_module_key() == self.context.lib_mod, "Internal error, a pass did not pop an active module");
  }
}