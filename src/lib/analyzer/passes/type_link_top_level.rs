use crate::{
  some,
  ast::{ Item, ItemData, ExportData, TypeExpression, TypeExpressionData, },
};

use super::{
  Analyzer,
  eval_helpers::{ eval_texpr, },
};


/// Binds top level globals and functions to their types
pub fn type_link_top_level (analyzer: &mut Analyzer, items: &[Item]) {
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

        let parameter_types = parameters.iter().map(|(_, texpr)| texpr.clone()).collect();
        let return_type = box return_type.as_ref().cloned();

        let fn_texpr = TypeExpression::new(TypeExpressionData::Function { parameter_types, return_type }, item.origin);

        let ty = eval_texpr(analyzer, &fn_texpr).unwrap_or(analyzer.context.err_ty);

        unsafe { analyzer.context.items.get_unchecked_mut(function_key).mut_function_unchecked() }.ty.replace(ty);
      }
      
      | ItemData::Import { .. }
      | ItemData::Export { .. }
      => unreachable!()
    }
  }
  
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