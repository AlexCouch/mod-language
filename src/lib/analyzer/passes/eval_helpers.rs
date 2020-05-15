//! Helpers for the evaluation of reference values such as identifiers and type expressions

use crate::{
  common::{ Identifier, },
  source::{ SourceRegion, },
  ast::{ TypeExpression, TypeExpressionData, Path, },
  ctx::{ ContextItem, ContextKey, Module, TypeData,  MultiKey, },
};

use super::{
  Analyzer,
  ty_helpers::{ ty_from_anon_data },
};



/// Get the ContextKey associated with an Identifier in the current context
pub fn eval_global_ident (analyzer: &Analyzer, identifier: &Identifier, origin: SourceRegion) -> Option<ContextKey> {
  let namespace = analyzer.get_active_namespace();

  Some(if let Some(local) = namespace.local_bindings.get_entry(identifier) {
    local
  } else if let Some(core) = analyzer.context.core_bs.get_entry(identifier) {
    core
  } else {
    analyzer.error(origin, format!("Namespace `{}` does not have access to an item named `{}`", namespace.canonical_name, identifier));
    return None
  })
}


/// Evaluate an identifier in the active local context,
/// bubbling to the global context if necessary,
/// and get a MultiKey representing the value referenced
pub fn eval_local_ident (analyzer: &mut Analyzer, identifier: &Identifier, origin: SourceRegion) -> Option<MultiKey> {
  Some(if let Some(local) = analyzer.get_local_context().get_variable(identifier) {
    local
  } else {
    let namespace = analyzer.get_active_namespace();

    if let Some(global) = namespace.local_bindings.get_entry(identifier) {
      global
    } else if let Some(core) = analyzer.context.core_bs.get_entry(identifier) {
      core
    } else {
      analyzer.error(origin, format!("Cannot find item or local variable named `{}`", identifier));
      return None
    }.into()
  })
}


/// Get the ContextKey associated with a Path in the current Context
pub fn eval_path (analyzer: &mut Analyzer, path: &Path, origin: SourceRegion) -> Option<ContextKey> {
  let mut base_name = Identifier::default();
                
  let base_key = if path.absolute {
    analyzer.context.main_ns
  } else {
    analyzer.get_active_namespace_key()
  };

  let mut resolved_key = base_key;
  
  for ident in path.iter() {
    let base = analyzer.context.items.get(resolved_key).expect("Internal error, invalid lowered key during path resolution");

    let (ns_key, namespace) = match base {
      ContextItem::Namespace(namespace) => (resolved_key, namespace),
      &ContextItem::Module(Module { namespace, .. }) => (namespace, analyzer.context.items.get(namespace).unwrap().ref_namespace().unwrap()),

      _ => {
        analyzer.error(path.origin, format!("{} is not a Namespace or Module and has no exports", base_name));
        return None
      }
    };

    base_name.set(&namespace.canonical_name);

    resolved_key = if !path.absolute && ns_key == base_key {
      if let Some(local) = namespace.local_bindings.get_entry(ident) {
        local
      } else if let Some(core) = analyzer.context.core_bs.get_entry(ident) {
        core
      } else {
        analyzer.error(origin, format!("Namespace `{}` does not have access to an item named `{}`", base_name, ident));
        return None
      }
    } else if let Some(exported_key) = namespace.export_bindings.get_entry(ident) {
      exported_key
    } else {
      analyzer.error(origin, format!("Namespace `{}` does not export an item named `{}`", base_name, ident));
      return None
    };
  }

  Some(resolved_key)
}


/// Evaluate a TypeExpression in the active context and get a ContextKey representing the type referenced by it
pub fn eval_texpr (analyzer: &mut Analyzer, texpr: &TypeExpression) -> Option<ContextKey> {
  match &texpr.data {
    TypeExpressionData::Path(path) => {
      let key = eval_path(analyzer, path, texpr.origin)?;
      
      let item = analyzer.context.items.get(key).expect("Internal error, path evaluated to invalid key");

      if item.ref_type().is_some() {
        Some(key)
      } else {
        analyzer.error(texpr.origin, format!("Path `{}` does not evaluate to a type", path));

        None
      }
    },

    TypeExpressionData::Identifier(identifier) => {
      let key = eval_global_ident(analyzer, identifier, texpr.origin)?;

      let item = analyzer.context.items.get(key).expect("Internal error, identifier evaluated to invalid key");

      if item.ref_type().is_some() {
        Some(key)
      } else {
        analyzer.error(texpr.origin, format!("Identifier `{}` does not evaluate to a type", identifier));

        None
      }
    },

    TypeExpressionData::Pointer(value_texpr) => {
      let value_key = eval_texpr(analyzer, value_texpr)?;

      Some(ty_from_anon_data(analyzer, TypeData::Pointer(value_key), texpr.origin))
    },

    TypeExpressionData::Function { parameter_types: parameter_texprs, return_type: return_texpr } => {
      let mut parameter_types = Vec::with_capacity(parameter_texprs.len());

      for texpr in parameter_texprs.iter() {
        parameter_types.push(eval_texpr(analyzer, texpr)?);
      }

      let return_type = if let box Some(return_texpr) = return_texpr { Some(eval_texpr(analyzer, return_texpr)?) } else { None };

      let fn_td = TypeData::Function { parameter_types, return_type };

      Some(ty_from_anon_data(analyzer, fn_td, texpr.origin))
    },
  }
}