use crate::{
  ast::{ ItemData, TypeExpression, TypeExpressionData, },
  ctx::{ TypeData, Global, Function, TypeKey, NamespaceKey, },
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
fn prebuild_type_impl (analyzer: &mut Analyzer, texpr: &TypeExpression) -> Result<TypeKey, TypeKey> {
  match &texpr.data {
    TypeExpressionData::Identifier(identifier) => {
      if let Some(existing_binding) = analyzer.lookup_ident(identifier) {
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
        analyzer.bind_ident(identifier, key);
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

/// This is a simple wrapper for the prebuild_type_impl,
/// to unwrap the type whether it was an err or ok,
/// while _impl uses Result to make escaping recursion easier
#[inline] fn prebuild_type (analyzer: &mut Analyzer, texpr: &TypeExpression) -> TypeKey { match prebuild_type_impl(analyzer, texpr) { Ok(ty) => ty, Err(ty) => ty } }


/// This pass binds all the top-level identifiers together,
/// allowing out of order declarations
/// 
/// After scanning and binding the entire source tree,
/// it scans the items that were collected,
/// and reports any items that were referenced but never defined
fn prepass (analyzer: &mut Analyzer) {
  for item in analyzer.ast.items() {
    match &item.data {
      ItemData::Global { identifier, explicit_type, initializer: _ } => {
        let ty = prebuild_type(analyzer, explicit_type);
        let global = Global { ty, origin: Some(item.origin) };

        if let Some(existing_binding) = analyzer.lookup_user_ident(identifier) {
          if let Some(&existing_key) = analyzer.get_active_module().globals.get(identifier.as_ref()) {
            let existing_global = unsafe { analyzer.context.globals.get_unchecked_mut(existing_key) };
            
            if let Some(existing_global) = existing_global {
              // TODO: need non-debug fmt for source attribution
              // TODO: need source attribution to support multiple source files
              let message = format!(
                "Duplicate definition for global variable `{}`, previous definition is at {}:{:?}",
                identifier.as_ref(),
                analyzer.ast.stream.source.path,
                existing_global.origin.expect("Internal error: Found duplicate global definition but existing global had no source attribution")
              );
              
              analyzer.error(item.origin, message);
            } else {
              existing_global.replace(global);
              analyzer.add_reference(existing_key, item.origin);
              continue
            }
          // TODO: need some way of finding where the shadowed item was imported
          } else if let NamespaceKey::Global(_) = existing_binding {
            analyzer.error(item.origin, format!("Global variable definition `{}` shadows an imported global", identifier.as_ref()));
          } else {
            analyzer.error(item.origin, format!("Global variable definition `{}` shadows an item of a different kind", identifier.as_ref()));
          }
        }

        let key = analyzer.context.globals.insert(Some(global));
        analyzer.get_active_module_mut().globals.insert(identifier.into(), key);
        analyzer.bind_ident(identifier, key);
        analyzer.add_reference(key, item.origin);
      },
      
      ItemData::Function { identifier, parameters, return_type, body: _ } => {
        let ty = prebuild_type(analyzer, &TypeExpression::new(TypeExpressionData::Function {
          parameter_types: parameters.iter().map(|(_, texpr)| texpr.clone()).collect(),
          return_type: box return_type.clone()
        }, item.origin));

        let function = Function { ty, origin: Some(item.origin) };

        if let Some(existing_binding) = analyzer.lookup_user_ident(identifier) {
          if let Some(&existing_key) = analyzer.get_active_module().functions.get(identifier.as_ref()) {
            let existing_function = unsafe { analyzer.context.functions.get_unchecked_mut(existing_key) };

            if let Some(existing_function) = existing_function {
              // TODO: need non-debug fmt for source attribution
              // TODO: need source attribution to support multiple source files
              let message = format!(
                "Duplicate definition for function `{}`, previous definition is at {}:{:?}",
                identifier.as_ref(),
                analyzer.ast.stream.source.path,
                existing_function.origin.expect("Internal error: Found duplicate function definition but existing function had no source attribution")
              );
              
              analyzer.error(item.origin, message);
            } else {
              existing_function.replace(function);
              analyzer.add_reference(existing_key, item.origin);
              continue
            }
          // TODO: need some way of finding where the shadowed item was imported
          } else if let NamespaceKey::Function(_) = existing_binding {
            analyzer.error(item.origin, format!("Function definition `{}` shadows an imported function", identifier.as_ref()));
          } else {
            analyzer.error(item.origin, format!("Function definition `{}` shadows an item of a different kind", identifier.as_ref()));
          }
        }
       
        let key = analyzer.context.functions.insert(Some(function));
        analyzer.get_active_module_mut().functions.insert(identifier.into(), key);
        analyzer.bind_ident(identifier, key);
        analyzer.add_reference(key, item.origin);
      },
    }
  }

  for (&key, global) in analyzer.context.globals.pair_iter() {
    if global.is_none() {
      // TODO it would be nice to print the identifier here
      analyzer.error(
        *analyzer.context.reference_locations
          .get(&key.into())
          .expect("Internal error: Global created with no reference locations")
          .first()
          .expect("Internal error: Global reference array was empty"),
        "Undefined global variable".to_owned()
      )
    }
  }

  for (&key, function) in analyzer.context.functions.pair_iter() {
    if function.is_none() {
      // TODO it would be nice to print the identifier here
      analyzer.error(
        *analyzer.context.reference_locations
          .get(&key.into())
          .expect("Internal error: Function created with no reference locations")
          .first()
          .expect("Internal error: Function reference array was empty"),
        "Undefined function".to_owned()
      )
    }
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