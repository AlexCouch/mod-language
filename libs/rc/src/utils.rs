//! Various utilities to assist in using mod_rc

/// A convenience macro for creating null-terminated string literals
#[macro_export]
macro_rules! c_lit {
  ($x:literal) => {
    concat!($x, "\0").as_ptr() as _
  };
}

/// A convenience macro for creating null-terminated CStrs from literals (uses `c_lit` internally)
#[macro_export]
macro_rules! c_str {
  ($x:literal) => {
    unsafe { std::ffi::CStr::from_ptr(c_lit!($x)) }
  }
}


/// A convenience macro for hashing a series of values with the DefaultHasher
#[macro_export]
macro_rules! hash {
  ($($exprs:expr),* $(,)?) => {
    {
      use ::std::hash::{ Hash, Hasher };
      let mut hasher = ::std::collections::hash_map::DefaultHasher::default();
      {
        $($exprs.hash(&mut hasher));*
      }
      hasher.finish()
    }
  };
}