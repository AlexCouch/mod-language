use std::{
  collections::{ HashSet, },
};

use crate::{
  util::{ some, },
  source::{ SOURCE_MANAGER, ASTKey, },
  ast::{ Item, ItemData, ExportData, LocalDeclaration, },
  ctx::{ TypeData, },
};

use super::{
  Analyzer,
  eval_helpers::{ eval_texpr, },
  ty_helpers::{ ty_from_anon_data, },
};



/// Binds top level globals and functions to their types
pub fn type_link_top_level (analyzer: &mut Analyzer, linked_module_asts: &mut HashSet<ASTKey>, items: &[Item]) {
  for item in items.iter() {
    match &item.data {
      | ItemData::Alias { .. }
      | ItemData::Export { data: ExportData::List(_), .. } 
      => continue,

      ItemData::Export { data: ExportData::Inline(item), .. } => type_link_item(analyzer, linked_module_asts, item),
      
      | ItemData::Import    { .. }
      | ItemData::Namespace { .. }
      | ItemData::Struct    { .. }
      | ItemData::Type      { .. }
      | ItemData::Global    { .. }
      | ItemData::Function  { .. }
      => type_link_item(analyzer, linked_module_asts, item)
    }
  }
}


fn type_link_item (analyzer: &mut Analyzer, linked_module_asts: &mut HashSet<ASTKey>, item: &Item) {
  match &item.data {
    &ItemData::Import { ref identifier, ast_key, .. } => {
      if !linked_module_asts.contains(&ast_key) {
        let &module_key = analyzer.context.modules.get(identifier).unwrap();
        let ast = SOURCE_MANAGER.get_ast(ast_key).unwrap();

        // Its important to register it before starting to analyze it incase of circular deps
        linked_module_asts.insert(ast_key);

        analyzer.push_active_module(module_key);

        type_link_top_level(analyzer, linked_module_asts, ast);

        analyzer.pop_active_module();
      }
    }

    ItemData::Namespace { identifier, items, .. } => {
      analyzer.push_active_namespace(analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap());
      type_link_top_level(analyzer, linked_module_asts, items);
      analyzer.pop_active_namespace();
    },

    ItemData::Struct { identifier, fields, .. } => {
      let struct_key = analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap();
      
      // its possible some shadowing error has overwritten this def and if so we just return
      some!(analyzer.context.items.get(struct_key).unwrap().ref_type());

      let mut field_names = Vec::new();
      let mut field_types = Some(Vec::new());
      
      for LocalDeclaration { identifier, ty, .. } in fields.iter() {
        field_names.push(identifier.clone());

        if let Some(field_type) = eval_texpr(analyzer, ty) {
          if let Some(field_types) = field_types.as_mut() {
            field_types.push(field_type)
          }
        } else {
          field_types = None;
        }
      }

      if let Some(field_types) = field_types {
        let struct_td = TypeData::Structure { field_names, field_types };

        unsafe { analyzer.context.items.get_unchecked_mut(struct_key).mut_type_unchecked() }
          .data.replace(struct_td);
      }
    },

    ItemData::Global { identifier, explicit_type, .. } => {
      let global_key = analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap();

      // its possible some shadowing error has overwritten this def and if so we just return
      some!(analyzer.context.items.get(global_key).unwrap().ref_global());

      let ty = eval_texpr(analyzer, explicit_type).unwrap_or(analyzer.context.err_ty);

      unsafe { analyzer.context.items.get_unchecked_mut(global_key).mut_global_unchecked() }
        .ty.replace(ty);
    },

    ItemData::Function { identifier, parameters, return_type, .. } => {
      let function_key = analyzer.get_active_namespace().local_bindings.get_entry(identifier).unwrap();
      
      // its possible some shadowing error has overwritten this def and if so we just return
      some!(analyzer.context.items.get(function_key).unwrap().ref_function());

      let parameter_texprs = parameters.iter().map(|LocalDeclaration { ty, .. }| ty);
      let parameter_name_origins = parameters.iter().map(|LocalDeclaration { identifier, origin, .. }| (identifier, origin));

      let mut parameter_types = Vec::with_capacity(parameters.len());

      for texpr in parameter_texprs {
        parameter_types.push(some!(eval_texpr(analyzer, texpr)));
      }

      let param_info = parameter_name_origins.zip(parameter_types.iter()).map(|((identifier, origin), ty)| (identifier.clone(), *ty, *origin)).collect();

      let return_type = if let Some(return_texpr) = return_type { Some(some!(eval_texpr(analyzer, return_texpr))) } else { None };

      let fn_td = TypeData::Function { parameter_types, return_type };

      // TODO itd be nice to attribute this type to just the signature portion of the function def, but the parser will need to adjust
      let fn_ty = ty_from_anon_data(analyzer, fn_td, item.origin);

      let func = unsafe { analyzer.context.items.get_unchecked_mut(function_key).mut_function_unchecked() };

      func.ty.replace(fn_ty);
      func.params = param_info;
      func.return_ty = return_type;
    }


    // Handled in previous pass
    | ItemData::Type   { .. }
    => { },

    // Not allowed, already produced panic in first pass
    | ItemData::Alias { .. }
    | ItemData::Export { .. }
    => unreachable!()
  }
}