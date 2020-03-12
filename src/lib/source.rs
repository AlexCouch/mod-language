//! Contains Source, SourceTracker, and access functions, as well as SourceLocation, SourceRegion, and SourceOrigin

use std::{
  io::Result as IOResult,
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  fs::read_to_string,
  cell::RefCell,
};

use crate::{
  ansi,
  util::{ padding, count_digits },
};


/// A set of integers representing an index, line and column in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub struct SourceLocation {
  pub index: usize,
  pub line: u32,
  pub column: u32,
}

impl SourceLocation {
  /// A SourceLocation with all values initialized to zero
  pub const ZERO: Self = Self { index: 0, line: 0, column: 0 };

  /// Create a zero-width SourceRegion from a SourceLocation
  pub fn to_region (self) -> SourceRegion {
    SourceRegion {
      start: self,
      end: self,
    }
  }
}

/// A pair of SourceLocations indicating a region in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub struct SourceRegion {
  pub start: SourceLocation,
  pub end: SourceLocation,
}

impl Debug for SourceLocation {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}:{}", self.line + 1, self.column + 1)
  }
}

impl Debug for SourceRegion {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{:?} to {:?}", self.start, self.end)
  }
}


/// The kind of content contained in a user-directed message such as Error, or Warning
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum MessageKind {
  Error,
  Warning,
  Notice,
}

impl MessageKind {
  /// Get the ansi Foreground color code associated with a MessageKind
  pub fn get_ansi (self) -> ansi::Foreground {
    use MessageKind::*;

    match self {
      Error => ansi::Foreground::Red,
      Warning => ansi::Foreground::Yellow,
      Notice => ansi::Foreground::Green,
    }
  }

  /// Get a MessageKind in str form
  pub fn get_name (self) -> &'static str {
    use MessageKind::*;

    match self {
      Error => "Error",
      Warning => "Warning",
      Notice => "Notice",
    }
  }

  /// Get a string of whitespace the same length as a MessageKind in str form, minus some offset
  pub fn get_whitespace (self, offset: usize) -> &'static str {
    padding((self.get_name().len() - offset) as _)
  }
}

impl Display for MessageKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(
      f, "{}{}{}",
      self.get_ansi(),
      self.get_name(),
      ansi::Foreground::Reset
    )
  }
}

/// A user-directed message such as an Error
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
  /// Possibly contains an area of a Message's parent source at which the Message was created
  pub origin: Option<SourceRegion>,
  /// The variant or severity of a Message
  pub kind: MessageKind,
  /// Possibly contains contextual information for a Message
  pub content: String,
}

impl Message {
  /// Create a new user-directed Message for a Source
  pub fn new (origin: Option<SourceRegion>, kind: MessageKind, content: String) -> Self {
    Self {
      origin,
      kind,
      content,
    }
  }

  /// Print the source excerpt for a Message
  /// 
  /// Requires a reference to source chars
  #[allow(clippy::cognitive_complexity)] // Some functions are just complicated ok?
  pub fn excerpt (&self, chars: &[char]) {
    let origin = if let Some(origin) = self.origin { origin } else { return };

    let mut start_index = origin.start.index.min(chars.len() - 1);

    if start_index != 0 {
      if chars[start_index] == '\n' {
        start_index += 1;
      } else {
        while start_index > 0 {
          if chars[start_index - 1] != '\n' {
            start_index -= 1;
          } else {
            break
          }
        }
      }
    }

    let mut end_index = origin.end.index.min(chars.len());

    if end_index != chars.len() {
      if chars[end_index] == '\n' {
        end_index -= 1;
      } else {
        while end_index < chars.len() - 1 {
          if chars[end_index + 1] != '\n' {
            end_index += 1;
          } else {
            break
          }
        }
      }
    }

    let slice = &chars[start_index..end_index];
    
    let mut num_lines = 1usize;

    for ch in slice.iter() {
      if *ch == '\n' { num_lines += 1 }
    }

    if num_lines == 1 {
      let line_num = origin.start.line + 1;
      let line_num_digits = count_digits(line_num as _, 10);
      
      print!("{}|\n|{} {} {}|{}  ", self.kind.get_ansi(), ansi::Foreground::Cyan, line_num, self.kind.get_ansi(), ansi::Foreground::BrightBlack);

      for (i, ch) in slice.iter().enumerate() {
        if i == origin.start.column as _ { print!("{}", ansi::Foreground::Reset); }
        else if i == origin.end.column as _ { print!("{}", ansi::Foreground::BrightBlack); }
        print!("{}", ch);
      }

      print!("\n{}{}|--", padding((line_num_digits + 3) as _), self.kind.get_ansi());

      for i in 0..slice.len() as _ {
        if i < origin.start.column { print!("-") }
        else if i < origin.end.column { print!("^") }
        else { break }
      }

      println!("{}", ansi::Foreground::Reset);
    } else {
      let last_line_num = origin.end.line + 1;
      let last_line_num_digits = count_digits(last_line_num as _, 10);
      let gap_pad = padding((last_line_num_digits + 2) as _);

      print!("{}|\n|{}|", self.kind.get_ansi(), gap_pad,);
      
      for _ in 0..origin.start.column + 2 {
        print!("-");
      }
      
      let first_line_num = origin.start.line + 1;
      let first_line_num_digits = count_digits(first_line_num as _, 10);
      print!("v\n| {}{}{}{} |{}  ", padding((last_line_num_digits - first_line_num_digits) as _), ansi::Foreground::Cyan, first_line_num, self.kind.get_ansi(), ansi::Foreground::BrightBlack);

      let mut line_num = first_line_num;
      let mut column = 0;

      for ch in slice.iter() {
        if *ch != '\n' {
          if line_num - 1 == origin.start.line && column == origin.start.column as _ { print!("{}", ansi::Foreground::Reset); }
          else if line_num - 1 == origin.end.line && column == origin.end.column as _ { print!("{}", ansi::Foreground::BrightBlack); }
          print!("{}", ch);
          column += 1;
        } else {
          column = 0;
          line_num += 1;
          let line_num_digits = count_digits(line_num as _, 10);
          print!("\n{}| {}{}{}{} |  ", self.kind.get_ansi(), padding((last_line_num_digits - line_num_digits) as _), ansi::Foreground::Cyan, line_num, self.kind.get_ansi());
          if line_num > first_line_num { print!("{}", ansi::Foreground::Reset); }
        }
      }

      print!("\n {}{}|", gap_pad, self.kind.get_ansi());

      for _ in 0..origin.end.column + 1 {
        print!("-")
      }
      
      println!("^{}", ansi::Foreground::Reset);
    }
  }

  /// Print a Message to the stdout
  /// 
  /// Requires a reference to its parent Source for paths and excerpts
  pub fn print (&self, source: &Source) {
    println!("\n{}: {}", self.kind, self.content);

    print!("{}|{} {}at: {}{}", self.kind.get_ansi(), ansi::Foreground::Reset, self.kind.get_whitespace(4), ansi::Foreground::Cyan, source.path);

    if let Some(origin) = self.origin {
      print!(":{:?}{}", origin.start, ansi::Foreground::Reset);
    }
    
    println!("{}", ansi::Foreground::Reset);

    self.excerpt(source.chars());
  }
}


/// Contains source code data including its original file path and content
pub struct Source {
  /// The file path a Source originated from
  pub path: String,
  /// The content of a Source
  pub content: Vec<char>,
  /// User-directed messages created while processing a Source
  pub messages: RefCell<Vec<Message>>,
}


impl Source {
  /// Load a Source from a text file at a path on disk
  pub fn load (path: String) -> IOResult<Source> {
    let content = read_to_string(&path)?;

    Ok(Source {
      path,
      content: content.chars().collect(),
      messages: RefCell::new(Vec::new())
    })
  }

  /// Get an iterator of the chars of the content of a Source
  pub fn chars (&self) -> &[char] {
    self.content.as_slice()
  }


  /// Add a Message to the list of Messages associated with a Source
  pub fn message (&self, origin: Option<SourceRegion>, kind: MessageKind, content: String) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      kind,
      content
    ))
  }

  
  /// Add an Error Message to the list of Messages associated with a Source
  pub fn error (&self, origin: Option<SourceRegion>, content: String) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      MessageKind::Error,
      content
    ))
  }
  
  /// Add a Warning Message to the list of Messages associated with a Source
  pub fn warning (&self, origin: Option<SourceRegion>, content: String) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      MessageKind::Warning,
      content
    ))
  }
  
  /// Add a Notice Message to the list of Messages associated with a Source
  pub fn notice (&self, origin: Option<SourceRegion>, content: String) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      MessageKind::Notice,
      content
    ))
  }


  /// Print all Messages associated with a Source
  pub fn print_messages (&self) {
    for message in self.messages.borrow().iter() {
      message.print(self)
    }
  }
  

  /// Print all Error Messages associated with a Source
  pub fn print_errors (&self) {
    for message in self.messages.borrow().iter() {
      if message.kind == MessageKind::Error { message.print(self) }
    }
  }

  /// Print all Warning Messages associated with a Source
  pub fn print_warnings (&self) {
    for message in self.messages.borrow().iter() {
      if message.kind == MessageKind::Warning { message.print(self) }
    }
  }

  /// Print all Notice Messages associated with a Source
  pub fn print_notices (&self) {
    for message in self.messages.borrow().iter() {
      if message.kind == MessageKind::Notice { message.print(self) }
    }
  }

  /// Find the index offset of a line and column in a Source, if it is in range
  pub fn line_and_column_to_index (&self, line: u32, column: u32) -> Option<usize> {
    let mut index = 0;
    let mut ol = 0u32;
    let mut oc = 0u32;

    for ch in self.chars().iter() {
      if ol == line && oc == column { return Some(index) }

      if *ch == '\n' {
        ol += 1;
        oc = 0;
      } else {
        oc += 1;
      }

      index += 1;
    }

    if ol == line && oc == column { Some(index) }
    else { None }
  }
}