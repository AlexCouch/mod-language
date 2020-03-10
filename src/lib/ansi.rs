//! Contains procedures for colorizing console messages

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult },
};


#[cfg(windows)]
mod windows {
  const STD_OUTPUT_HANDLE: u32 = -11 as _;
  const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;

  type Handle = *const std::ffi::c_void;

  extern "C" {
    fn GetStdHandle (id: u32) -> Handle;
    fn GetConsoleMode (h: Handle, out_mode: *mut u32) -> i32;
    fn SetConsoleMode (h: Handle, in_mode: u32) -> i32;
  }

  pub fn enable_ansi_colors () -> bool {
    unsafe {
      let handle = GetStdHandle(STD_OUTPUT_HANDLE);
      let mut original_mode = 0u32;
         GetConsoleMode(handle, &mut original_mode) != 0
      && SetConsoleMode(handle, original_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0
    }
  }
}


static mut ANSI_ENABLED: bool = false;

/// Enable ansi color codes
/// Always returns true on non-windows platforms
pub fn enable () -> bool {
  unsafe {
    ANSI_ENABLED = if cfg!(windows) {
      windows::enable_ansi_colors()
    } else {
      true
    };

    ANSI_ENABLED
  }
}


/// Check if ansi color coding is enabled
#[inline]
pub fn is_enabled () -> bool {
  unsafe { ANSI_ENABLED }
}


/// An ansi foreground color, printing this will colorize future text output
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Foreground {
  Reset,
  Black,
  Red,
  Green,
  Yellow,
  Blue,
  Magenta,
  Cyan,
  White,
  BrightBlack,
  BrightRed,
  BrightGreen,
  BrightYellow,
  BrightBlue,
  BrightMagenta,
  BrightCyan,
  BrightWhite,
}

impl Foreground {
  /// Get the ansi code of a foreground color
  pub fn code (self) -> &'static str {
    use Foreground::*;
    match self {
      Reset => "39",
      Black => "30",
      Red => "31",
      Green => "32",
      Yellow => "33",
      Blue => "34",
      Magenta => "35",
      Cyan => "36",
      White => "37",
      BrightBlack => "90",
      BrightRed => "91",
      BrightGreen => "92",
      BrightYellow => "93",
      BrightBlue => "94",
      BrightMagenta => "95",
      BrightCyan => "96",
      BrightWhite => "97",
    }
  }
}

impl Display for Foreground {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    if is_enabled() { write!(f, "\x1b[{}m", self.code()) }
    else { Ok(()) }
  }
}

impl Debug for Foreground {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Display::fmt(self, f)
  }
}

/// An ansi background color, printing this will colorize future text output
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Background {
  Reset,
  Black,
  Red,
  Green,
  Yellow,
  Blue,
  Magenta,
  Cyan,
  White,
  BrightBlack,
  BrightRed,
  BrightGreen,
  BrightYellow,
  BrightBlue,
  BrightMagenta,
  BrightCyan,
  BrightWhite,
}

impl Background {
  /// Get the ansi code of a background color
  pub fn code (self) -> &'static str {
    use Background::*;
    match self {
      Reset => "49",
      Black => "40",
      Red => "41",
      Green => "42",
      Yellow => "43",
      Blue => "44",
      Magenta => "45",
      Cyan => "46",
      White => "47",
      BrightBlack => "100",
      BrightRed => "101",
      BrightGreen => "102",
      BrightYellow => "103",
      BrightBlue => "104",
      BrightMagenta => "105",
      BrightCyan => "106",
      BrightWhite => "107",
    }
  }
}

impl Display for Background {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    if is_enabled() { write!(f, "\x1b[{}m", self.code()) }
    else { Ok(()) }
  }
}

impl Debug for Background {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Display::fmt(self, f)
  }
}