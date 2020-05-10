use crate::{
  some,
  ast::{ Item, ItemData, ExportData, LocalDeclaration, },
  ctx::{ TypeData, },
};

use super::{
  Analyzer,
  eval_helpers::{ eval_texpr, },
  ty_helpers::{ ty_from_anon_data, },
};



/// Binds top level globals and functions to their types
pub fn type_link_top_level (analyzer: &mut Analyzer, items: &[Item]) {
  for item in items.iter() {
    match &item.data {
      | ItemData::Import { .. }
      | ItemData::Export { data: ExportData::List(_), .. } 
      => continue,

      ItemData::Export { data: ExportData::Inline(item), .. } => type_link_item(analyzer, item),

      | ItemData::Module   { .. }
      | ItemData::Global   { .. }
      | ItemData::Function { .. }
      => type_link_item(analyzer, item)
    }
  }
}


fn type_link_item (analyzer: &mut Analyzer, item: &Item) {
  match &item.data {
    ItemData::Module { identifier, items, .. } => {
      analyzer.push_active_module(analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap());
      type_link_top_level(analyzer, items);
      analyzer.pop_active_module();
    },

    ItemData::Global { identifier, explicit_type, .. } => {
      let global_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();

      // its possible some shadowing error has overwritten this def and if so we just return
      some!(analyzer.context.items.get(global_key).unwrap().ref_global());

      let ty = eval_texpr(analyzer, explicit_type).unwrap_or(analyzer.context.err_ty);

      unsafe { analyzer.context.items.get_unchecked_mut(global_key).mut_global_unchecked() }.ty.replace(ty);
    },

    ItemData::Function { identifier, parameters, return_type, .. } => {
      let function_key = analyzer.get_active_module().local_bindings.get_entry(identifier).unwrap();
      
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

      let fn_data = unsafe { analyzer.context.items.get_unchecked_mut(function_key).mut_function_unchecked() };

      fn_data.ty.replace(fn_ty);
      fn_data.params = param_info;
      fn_data.return_ty = return_type;
    }
    
    | ItemData::Import { .. }
    | ItemData::Export { .. }
    => unreachable!()
  }
}