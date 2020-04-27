use crate::{
  ast::{ Item, ItemData, TypeExpression, TypeExpressionData, },
  ctx::{ Module, TypeData, Global, Function, TypeKey, NamespaceKey, },
};

use super::{ Analyzer, };



/// A layer of analysis used by an Analyzer
pub trait Pass {
  /// Run a Pass on an Analyzer
  fn analyze (&mut self, analyzer: &mut Analyzer);
}

impl<F> Pass for F where F: Fn (&mut Analyzer) { #[inline] fn analyze (&mut self, analyzer: &mut Analyzer) { (self)(analyzer) } }


trait ResolveTypeError {
  type Resolved;
  fn resolve (self) -> Self::Resolved;
}




/// Creates or gets an existing TypeKey from a TypeExpression, during the prepass
/// 
/// This is a simple wrapper for the prebuild_type_impl,
/// to unwrap the type whether it was an err or ok,
/// while _impl uses Result to make escaping recursion easier
#[inline] fn prebuild_type (analyzer: &mut Analyzer, texpr: &TypeExpression) -> TypeKey {
  fn prebuild_type_impl (analyzer: &mut Analyzer, texpr: &TypeExpression) -> Result<TypeKey, TypeKey> {
    match &texpr.data {
      TypeExpressionData::Identifier(identifier) => {
        if let Some(existing_binding) = analyzer.lookup_ident(identifier, true) {
          if let NamespaceKey::Type(tk) = existing_binding {
            analyzer.add_reference(tk, texpr.origin);
            Ok(tk)
          } else {
            let kind = existing_binding.kind();
            analyzer.error(texpr.origin, format!("Identifier `{}` does not resolve to a type, it is bound to a {}", identifier, kind));
            Err(analyzer.context.err_ty)
          }
        } else {
          let key = analyzer.context.types.insert(None);
          analyzer.bind_item_ident(identifier, key, None);
          analyzer.add_reference(key, texpr.origin);
          Ok(key)
        }
      },

      TypeExpressionData::Function { parameter_types: parameter_texprs, return_type: return_texpr } => {
        let mut parameter_types = Vec::new();

        for texpr in parameter_texprs.iter() {
          parameter_types.push(prebuild_type_impl(analyzer, texpr)?);
        }

        let return_type = if let box Some(texpr) = return_texpr {
          Some(prebuild_type_impl(analyzer, texpr)?)
        } else {
          None
        };

        let fn_td = TypeData::Function { parameter_types, return_type };

        let key = analyzer.resolve_anon_type_key(fn_td);

        analyzer.add_reference(key, texpr.origin);

        Ok(key)
      }
    }
  }

  match prebuild_type_impl(analyzer, texpr) { Ok(ty) => ty, Err(ty) => ty }
}


/// This pass binds all the top-level identifiers together,
/// allowing out of order declarations
/// 
/// After scanning and binding the entire source tree,
/// it scans the items that were collected,
/// and reports any items that were referenced but never defined
fn prepass (analyzer: &mut Analyzer) {
  fn prepass_impl (analyzer: &mut Analyzer, items: &[Item]) {
    for item in items {
      match &item.data {
        ItemData::Module { identifier, items, .. } => {
          let module_key = analyzer.define_module(
            item.origin,
            identifier,
            Module::new(
              Some(analyzer.get_active_module_key()),
              Some(item.origin)
            )
          );

          analyzer.push_active_module(module_key);
          prepass_impl(analyzer, items);
          analyzer.pop_active_module();
        },

        ItemData::Global { identifier, explicit_type, initializer: _ } => {
          let ty = prebuild_type(analyzer, explicit_type);
          let global = Global { ty, origin: Some(item.origin) };

          analyzer.define_global(item.origin, identifier, global);
        },
        
        ItemData::Function { identifier, parameters, return_type, body: _ } => {
          let ty = prebuild_type(analyzer, &TypeExpression::new(TypeExpressionData::Function {
            parameter_types: parameters.iter().map(|(_, texpr)| texpr.clone()).collect(),
            return_type: box return_type.clone()
          }, item.origin));

          let function = Function { ty, origin: Some(item.origin) };

          analyzer.define_function(item.origin, identifier, function);
        },
      }
    }
  }

  prepass_impl(analyzer, analyzer.ast);

  macro_rules! check_undefines {
    ($name:ident [$plural:ident]; $($rest:tt)*) => {
      for (&key, $name) in analyzer.context.$plural.pair_iter() {
        if $name.is_none() {
          // TODO it would be nice to print the identifier here
          analyzer.error(
            analyzer.get_first_reference(key),
            concat!("Undefined ", stringify!($name)).to_owned()
          )
        }
      }

      check_undefines! { $($rest)* }
    };

    () => {};
  }

  check_undefines! {
    module [modules];
    global [globals];
    function [functions];
  }
}


impl<'a> Analyzer<'a> {
  /// Create a new Pass list for use with an Analyzer
  pub fn create_passes () -> Vec<Box<dyn Pass>> {
    vec! [
      box prepass,
    ]
  }
}