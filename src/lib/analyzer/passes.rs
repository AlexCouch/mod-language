use crate::{
  breakable_block,
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
          let module_key = breakable_block! {
            let module = Module::new(Some(analyzer.get_active_module_key()), Some(item.origin));

            if let Some(existing_binding) = analyzer.lookup_ident(identifier, false) {
              if let Some(&existing_key) = analyzer.get_active_module().modules.get(identifier.as_ref()) {
                // Found an item known to exist in this module
                let existing_module = unsafe { analyzer.context.modules.get_unchecked_mut(existing_key) };

                if let Some(existing_module) = existing_module {
                  // Item was already defined
                  let message = format!(
                    "Duplicate definition for module `{}`, previous definition is at [{}]",
                    identifier.as_ref(),
                    existing_module.origin
                      .expect("Internal error: Found duplicate module definition but existing module had no source attribution")
                  );
                  
                  analyzer.error(item.origin, message);
                } else {
                  // Item was only referenced
                  // not sure if this is possible
                  existing_module.replace(module);
                  analyzer.add_reference(existing_key, item.origin);
                  analyzer.bind_item_namespace_origin(existing_key, item.origin);
                  break existing_key;
                }
              } else if let Some(import_origin) = analyzer.get_item_namespace_origin(existing_binding) {
                // Item is an import
                analyzer.error(item.origin, format!(
                  "Module definition `{}` shadows an {} item imported at [{}]",
                  identifier.as_ref(), existing_binding.kind(), import_origin
                ));
              } else {
                // Item is something that has been referenced but not attributed to any location
                if let NamespaceKey::Module(module_key) = existing_binding {
                  // Item was expected to be a module, this is fine
                  let existing_module = unsafe { analyzer.context.modules.get_unchecked_mut(module_key) };

                  // insanity check
                  assert!(existing_module.is_none(), "Internal error: Found unbound module with definition");

                  existing_module.replace(module);
                  analyzer.get_active_module_mut().modules.insert(identifier.into(), module_key);
                  analyzer.add_reference(module_key, item.origin);
                  analyzer.bind_item_namespace_origin(module_key, item.origin);
                  break module_key;
                } else {
                  // Item was expected to be some other kind
                  analyzer.error(item.origin, format!(
                    "Module definition `{}` shadows an identifier that was expected to be defined as an {}, first referenced at [{}]",
                    identifier,
                    existing_binding.kind(),
                    analyzer.context.reference_locations
                      .get(&existing_binding)
                      .expect("Internal error: Existing binding has no reference locations")
                      .first()
                      .expect("Internal error: Existing binding has reference location vec but no locations")
                  ));
                }
              }
            }

            let key = analyzer.context.modules.insert(Some(module));
            analyzer.get_active_module_mut().modules.insert(identifier.into(), key);
            analyzer.bind_item_ident(identifier, key, Some(item.origin));
            analyzer.add_reference(key, item.origin);
            break key;
          };

          analyzer.push_active_module(module_key);
          prepass_impl(analyzer, items);
          analyzer.pop_active_module();
        },

        ItemData::Global { identifier, explicit_type, initializer: _ } => {
          let ty = prebuild_type(analyzer, explicit_type);
          let global = Global { ty, origin: Some(item.origin) };

          if let Some(existing_binding) = analyzer.lookup_ident(identifier, false) {
            if let Some(&existing_key) = analyzer.get_active_module().globals.get(identifier.as_ref()) {
              let existing_global = unsafe { analyzer.context.globals.get_unchecked_mut(existing_key) };
              
              if let Some(existing_global) = existing_global {
                let message = format!(
                  "Duplicate definition for global variable `{}`, previous definition is at [{}]",
                  identifier.as_ref(),
                  existing_global.origin
                    .expect("Internal error: Found duplicate global definition but existing global had no source attribution")
                );
                
                analyzer.error(item.origin, message);
              } else {
                existing_global.replace(global);
                analyzer.add_reference(existing_key, item.origin);
                analyzer.bind_item_namespace_origin(existing_key, item.origin);
                continue
              }
            } else if let Some(import_origin) = analyzer.get_item_namespace_origin(existing_binding) {
              analyzer.error(item.origin, format!(
                "Global variable definition `{}` shadows an {} item imported at [{}]",
                identifier.as_ref(), existing_binding.kind(), import_origin
              ));
            } else {
              // Item is something that has been referenced but not attributed to any location
              if let NamespaceKey::Global(global_key) = existing_binding {
                // Item was expected to be a global, this is fine
                let existing_global = unsafe { analyzer.context.globals.get_unchecked_mut(global_key) };

                // insanity check
                assert!(existing_global.is_none(), "Internal error: Found unbound global with definition");

                existing_global.replace(global);
                analyzer.get_active_module_mut().globals.insert(identifier.into(), global_key);
                analyzer.add_reference(global_key, item.origin);
                analyzer.bind_item_namespace_origin(global_key, item.origin);
                continue
              } else {
                // Item was expected to be some other kind
                analyzer.error(item.origin, format!(
                  "Module definition `{}` shadows an identifier that was expected to be defined as an {}, first referenced at [{}]",
                  identifier,
                  existing_binding.kind(),
                  analyzer.context.reference_locations
                    .get(&existing_binding)
                    .expect("Internal error: Existing binding has no reference locations")
                    .first()
                    .expect("Internal error: Existing binding has reference location vec but no locations")
                ));
              }
            }
          }

          let key = analyzer.context.globals.insert(Some(global));
          analyzer.get_active_module_mut().globals.insert(identifier.into(), key);
          analyzer.bind_item_ident(identifier, key, Some(item.origin));
          analyzer.add_reference(key, item.origin);
        },
        
        ItemData::Function { identifier, parameters, return_type, body: _ } => {
          let ty = prebuild_type(analyzer, &TypeExpression::new(TypeExpressionData::Function {
            parameter_types: parameters.iter().map(|(_, texpr)| texpr.clone()).collect(),
            return_type: box return_type.clone()
          }, item.origin));

          let function = Function { ty, origin: Some(item.origin) };

          if let Some(existing_binding) = analyzer.lookup_ident(identifier, false) {
            if let Some(&existing_key) = analyzer.get_active_module().functions.get(identifier.as_ref()) {
              let existing_function = unsafe { analyzer.context.functions.get_unchecked_mut(existing_key) };

              if let Some(existing_function) = existing_function {
                let message = format!(
                  "Duplicate definition for function `{}`, previous definition is at [{}]",
                  identifier.as_ref(),
                  existing_function.origin
                    .expect("Internal error: Found duplicate function definition but existing function had no source attribution")
                );
                analyzer.error(item.origin, message);
              } else {
                existing_function.replace(function);
                analyzer.add_reference(existing_key, item.origin);
                analyzer.bind_item_namespace_origin(existing_key, item.origin);
                continue
              }
            } else if let Some(import_origin) = analyzer.get_item_namespace_origin(existing_binding) {
              analyzer.error(item.origin, format!(
                "Function definition `{}` shadows an {} item imported at [{}]",
                identifier.as_ref(), existing_binding.kind(), import_origin
              ));
            } else {
              // Item is something that has been referenced but not attributed to any location
              if let NamespaceKey::Function(function_key) = existing_binding {
                // Item was expected to be a function, this is fine
                let existing_function = unsafe { analyzer.context.functions.get_unchecked_mut(function_key) };

                // insanity check
                assert!(existing_function.is_none(), "Internal error: Found unbound function with definition");

                existing_function.replace(function);
                analyzer.get_active_module_mut().functions.insert(identifier.into(), function_key);
                analyzer.add_reference(function_key, item.origin);
                analyzer.bind_item_namespace_origin(function_key, item.origin);
                continue
              } else {
                // Item was expected to be some other kind
                analyzer.error(item.origin, format!(
                  "Function definition `{}` shadows an identifier that was expected to be defined as an {}, first referenced at [{}]",
                  identifier,
                  existing_binding.kind(),
                  analyzer.context.reference_locations
                    .get(&existing_binding)
                    .expect("Internal error: Existing binding has no reference locations")
                    .first()
                    .expect("Internal error: Existing binding has reference location vec but no locations")
                ));
              }
            }
          }
        
          let key = analyzer.context.functions.insert(Some(function));
          analyzer.get_active_module_mut().functions.insert(identifier.into(), key);
          analyzer.bind_item_ident(identifier, key, Some(item.origin));
          analyzer.add_reference(key, item.origin);
        },
      }
    }
  }

  prepass_impl(analyzer, analyzer.ast);

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