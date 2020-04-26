//! General purpose macros TODO: move this to mod-utilities

/// Extract the value of an Option::Some or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! some {
  ($val: expr) => { if let Some(v) = $val { v } else { return } };
  ($val: expr ; $ret: expr) => { if let Some(v) = $val { v } else { return $ret } };
}

/// Extract the value of a Result::Ok or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! ok {
  ($val: expr) => { if let Ok(v) = $val { v } else { return } };
  ($val: expr ; $ret: expr) => { if let Ok(v) = $val { v } else { return $ret } };
}

/// Extract the value of a Result::Err or return from the current context
/// 
/// Works similar to the try operator, but is usable in functions that return ()
#[macro_export]
macro_rules! err {
  ($val: expr) => { if let Err(v) = $val { v } else { return } };
  ($val: expr ; $ret: expr) => { if let Err(v) = $val { v } else { return $ret } };
}

/// A block with fallthrough capability
#[macro_export]
macro_rules! breakable_block {
  ($($tt: tt)*) => { #[allow(unreachable_code)] loop {
    { $($tt)* }
    unreachable!("breakable_block not broken");
  } }
}


/// A trait that allows discarding Option values if some condition is or is not met
pub trait AllowIf: Sized {
  /// The type of value given by AllowIf calls
  type Accepted;

  /// Discard Option value if some condition is not met
  fn allow_if<F: FnOnce(&Self::Accepted) -> bool> (self, f: F) -> Option<Self::Accepted>;
  
  /// Discard Option value if some condition is not met
  fn allow_if_mut<F: FnOnce(&mut Self::Accepted) -> bool> (self, f: F) -> Option<Self::Accepted>;
  
  /// Discard Option value if some condition is met
  #[inline]
  fn allow_if_not<F: FnOnce(&Self::Accepted) -> bool> (self, f: F) -> Option<Self::Accepted> {
    self.allow_if(|x| !f(x))
  }
  
  /// Discard Option value if some condition is met
  #[inline]
  fn allow_if_not_mut<F: FnOnce(&mut Self::Accepted) -> bool> (self, f: F) -> Option<Self::Accepted> {
    self.allow_if_mut(|x| !f(x))
  }
}

impl<T> AllowIf for Option<T> {
  type Accepted = T;

  #[inline]
  fn allow_if<F: FnOnce(&Self::Accepted) -> bool> (self, f: F) -> Option<T> {
    match self {
      Some(x) => if f(&x) { Some(x) } else { None },
      None => None
    }
  }

  #[inline]
  fn allow_if_mut<F: FnOnce(&mut Self::Accepted) -> bool> (self, f: F) -> Option<T> {
    match self {
      Some(mut x) => if f(&mut x) { Some(x) } else { None },
      None => None
    }
  }
}