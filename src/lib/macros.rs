//! General purpose macros TODO: move this to mod-utilities

/// Extract the value of an Option::Some or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! some {
  ($val: expr) => { if let Some(v) = $val { v } else { return } };
}

/// Extract the value of a Result::Ok or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! ok {
  ($val: expr) => { if let Ok(v) = $val { v } else { return } };
}

/// Extract the value of a Result::Err or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! err {
  ($val: expr) => { if let Err(v) = $val { v } else { return } };
}