//! Thread local temporary allocations for use during conversions

use std::{
  thread_local,
  cell::RefCell,
  ffi::CStr,
  mem::{
    transmute,
    size_of,
  },
  slice::from_raw_parts_mut as make_slice
};

use super::{ unescape_str_into, escape_str_into, into_decimal };


/// Static allocating function for getting a temporary buffer of any type,
/// guaranteed to have at least `size * size_of::<T>()` bytes available
/// 
/// # Safety
/// Values placed in the buffer are not dropped automatically
/// 
/// Buffer is not initialized and may contain random junk data
/// 
/// Resulting buffer is only valid until the next call of this function on this thread for this type
pub unsafe fn buffer<T> (size: usize) -> &'static mut [T] {
  thread_local! {
    static BUFF: RefCell<Vec<u8>> = RefCell::new(Vec::new());
  }

  BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    buff.reserve(size * size_of::<T>());

    make_slice(buff.as_mut_ptr() as *mut T, size)
  })
}


/// Static allocating function for getting a temporary version of a string with `util::unescape_str_into` applied to it
/// 
/// Resulting str is only valid until the next call of this function on this thread
pub fn unescape_str (s: &str) -> &'static str {
  thread_local! {
    static UE_BUFF: RefCell<String> = RefCell::new(String::new());
  }

  UE_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    buff.clear();

    unescape_str_into(s, &mut buff);
    
    unsafe { transmute(buff.as_str()) }
  })
}



/// Static allocating function for getting a temporary version of a string with `util::escape_str_into` applied to it
/// 
/// Resulting str is only valid until the next call of this function on this thread
pub fn escape_str (s: &str) -> &'static str {
  thread_local! {
    static E_BUFF: RefCell<String> = RefCell::new(String::new());
  }

  E_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    buff.clear();

    escape_str_into(s, &mut buff);

    unsafe { transmute(buff.as_str()) }
  })
}

/// Static allocating function for getting a temporary lowercase version of a string
/// 
/// Resulting str is only valid until the next call of this function on this thread
pub fn to_lowercase (s: &str) -> &'static str {
  thread_local! {
    static LC_BUFF: RefCell<String> = RefCell::new(String::new());
  }

  LC_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    buff.clear();

    for c in s.chars() {
      for lc in c.to_lowercase() {
        buff.push(lc);
      }
    }

    unsafe { transmute(buff.as_str()) }
  })
}


/// Static allocating function for getting a temporary uppercase version of a string
/// 
/// Resulting str is only valid until the next call of this function on this thread
pub fn to_uppercase (s: &str) -> &'static str {
  thread_local! {
    static UC_BUFF: RefCell<String> = RefCell::new(String::new());
  }

  UC_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    buff.clear();

    for c in s.chars() {
      for uc in c.to_uppercase() {
        buff.push(uc);
      }
    }

    unsafe { transmute(buff.as_str()) }
  })
}


/// Get a temporary str version of a number
/// 
/// Resulting str is only valid until the next call of this function on this thread
pub fn num_as_str (num: u64) -> &'static str {
  thread_local! {
    static N_BUFF: RefCell<[u8; 20]> = RefCell::new([0u8; 20]);
  }

  N_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    let offset = into_decimal(num, &mut *buff);

    unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(buff.as_ptr(), offset)) }
  })
}

/// Get a temporary cstr version of a number, with a null terminator
/// 
/// Resulting cstr is only valid until the next call of this function on this thread
pub fn num_as_cstr (num: u64) -> &'static CStr {
  thread_local! {
    static N_BUFF: RefCell<[u8; 21]> = RefCell::new([0u8; 21]);
  }

  N_BUFF.with(|rc| {
    let mut buff = rc.borrow_mut();

    let offset = into_decimal(num, &mut *buff);

    buff[offset] = b'\0';

    unsafe { CStr::from_ptr(buff.as_ptr() as _) }
  })
}


#[cfg(test)]
mod test {
  #[test]
  fn thread_safe () {
    use super::*;

    use std::thread;

    let main_lc = to_lowercase("I'M THE MAIN THREAD");
    
    thread::spawn(|| {
      let child_lc = to_lowercase("I'M THE CHILD THREAD");

      println!("{}", child_lc);

      assert_eq!(child_lc, "i'm the child thread");
    }).join().unwrap();

    println!("{}", main_lc);

    assert_eq!(main_lc, "i'm the main thread");
  }
}