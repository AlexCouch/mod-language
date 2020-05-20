//! The Session structure and singleton, containing all the user-facing messages for a compilation session

use std::{
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  cell::{ UnsafeCell, },
};

use crate::{
  ansi,
  util::{ padding, count_digits, some, },
  source::{ SOURCE_MANAGER, SourceRegion, },
};


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
    self.get_ansi().wrap(self.get_name()).fmt(f)
  }
}

/// A single information entry in a Message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageItem {
  /// Contains an area of a Message's parent source at which the Message was created
  pub origin: SourceRegion,
  /// Contains contextual information for a Message
  pub content: String,
}

impl MessageItem {
  /// Display a MessageItem
  pub fn fmt (&self, f: &mut Formatter, kind: MessageKind, is_last: bool) -> FMTResult {
    let line = kind.get_ansi().wrap("|");

    writeln!(
      f,
      "{} {}\n\
       {} at: {}",
      line,
      self.content,
      line,
      self.origin,
    )?;

    // TODO control excerpts with flag
    self.excerpt(f, kind, is_last)
  }

  /// Print the source excerpt for a MessageItem
  /// 
  /// Requires a reference to source chars
  #[allow(clippy::cognitive_complexity)] // Some functions are just complicated ok?
  pub fn excerpt (&self, f: &mut Formatter, kind: MessageKind, is_last: bool) -> FMTResult {
    let source = some!(SOURCE_MANAGER.get_source(self.origin.source); Ok(()));
    let chars = source.chars();

    let pre_line = kind.get_ansi().wrap("|");
    let trailing_line = kind.get_ansi().wrap(if is_last { "|" } else { " " });

    let mut start_index = self.origin.start.index.min(chars.len() - 1);

    if start_index != 0 {
      if matches!(chars[start_index], '\r'|'\n') {
        start_index += 1;
      } else {
        while start_index > 0 {
          if !matches!(chars[start_index - 1], '\r'|'\n') {
            start_index -= 1;
          } else {
            break
          }
        }
      }
    }

    let mut end_index = self.origin.end.index.min(chars.len());

    if end_index != chars.len() {
      if matches!(chars[end_index], '\r'|'\n') {
        end_index -= 1;
      } else {
        while end_index < chars.len() - 1 {
          if !matches!(chars[end_index + 1], '\r'|'\n') {
            end_index += 1;
          } else {
            break
          }
        }
      }
    }

    let slice = &chars[start_index..(end_index + 1).min(chars.len())];
    
    let mut num_lines = 1usize;

    for ch in slice.iter() {
      if *ch == '\n' { num_lines += 1 }
    }

    if num_lines == 1 {
      let line_num = self.origin.start.line + 1;
      let line_num_digits = count_digits(line_num as _, 10);
      
      write!(f, "{}\n{} {} {}{}  ", pre_line, pre_line, ansi::Foreground::Cyan.wrap(line_num), pre_line, ansi::Foreground::BrightBlack)?;

      for (i, ch) in slice.iter().enumerate() {
        if i == self.origin.start.column as _ { write!(f, "{}", ansi::Foreground::Reset)?; }
        else if i == self.origin.end.column as _ { write!(f, "{}", ansi::Foreground::BrightBlack)?; }
        write!(f, "{}", ch)?;
      }

      write!(f, "\n{}{}{}└──", trailing_line, padding((line_num_digits + 2) as _), kind.get_ansi())?;

      for i in 0..slice.len() as _ {
        if i < self.origin.start.column { write!(f, "─")?; }
        else if i < self.origin.end.column { write!(f, "^")?; }
        else { break }
      }

      writeln!(f, "{}", ansi::Foreground::Reset)?;
    } else {
      let last_line_num = self.origin.end.line + 1;
      let last_line_num_digits = count_digits(last_line_num as _, 10);
      let gap_pad = padding((last_line_num_digits + 2) as _);

      write!(f, "{}│\n│{}┌", kind.get_ansi(), gap_pad,)?;
      
      for _ in 0..self.origin.start.column + 2 {
        write!(f, "─")?;
      }
      
      let first_line_num = self.origin.start.line + 1;
      let first_line_num_digits = count_digits(first_line_num as _, 10);
      write!(f, "v\n│ {}{}{}{} │{}  ", padding((last_line_num_digits - first_line_num_digits) as _), ansi::Foreground::Cyan, first_line_num, kind.get_ansi(), ansi::Foreground::BrightBlack)?;

      let mut line_num = first_line_num;
      let mut column = 0;

      for ch in slice.iter() {
        if *ch != '\n' {
          if line_num - 1 == self.origin.start.line && column == self.origin.start.column as _ { write!(f, "{}", ansi::Foreground::Reset)?; }
          else if line_num - 1 == self.origin.end.line && column == self.origin.end.column as _ { write!(f, "{}", ansi::Foreground::BrightBlack)?; }
          write!(f, "{}", ch)?;
          column += 1;
        } else {
          column = 0;
          line_num += 1;
          let line_num_digits = count_digits(line_num as _, 10);
          write!(f, "\n{}│ {}{}{}{} │  ", kind.get_ansi(), padding((last_line_num_digits - line_num_digits) as _), ansi::Foreground::Cyan, line_num, kind.get_ansi())?;
          if line_num > first_line_num { write!(f, "{}", ansi::Foreground::Reset)?; }
        }
      }

      write!(f, "\n{}{}{}└", trailing_line, gap_pad, kind.get_ansi())?;

      for _ in 0..self.origin.end.column + 1 {
        write!(f, "─")?;
      }
      
      writeln!(f, "^{}", ansi::Foreground::Reset)?;
    }

    Ok(())
  }
}

/// A user-directed message such as an Error
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
  /// The variant or severity of a Message
  pub kind: MessageKind,
  /// Informational entries detailing a Message
  pub items: Vec<MessageItem>,
}

impl Message {
  /// Create a new user-directed Message for a Source
  pub fn new (kind: MessageKind, origin: SourceRegion, content: String) -> Self {
    Self {
      kind,
      items: vec![ MessageItem {
        origin,
        content,
      } ]
    }
  }

  /// Add a new MessageItem to the end of a message
  pub fn append (&mut self, origin: SourceRegion, content: String) -> &mut Self {
    self.items.push(MessageItem { origin, content });
    self
  }
}

impl Display for Message {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    writeln!(f, "\n{}", self.kind)?;
    
    let mut iter = self.items.iter().peekable();

    while let Some(item) = iter.next() {
      let is_last = iter.peek().is_some();
      item.fmt(f, self.kind, is_last)?;
      if is_last {
        self.kind.get_ansi().wrap("|\n").fmt(f)?;
      }
    }

    Ok(())
  }
}



/// The type of the central repository for Messages created during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub struct Session (UnsafeCell<Option<Vec<Message>>>);

unsafe impl Send for Session { }
unsafe impl Sync for Session { }

/// The central repository for Messages created during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub static SESSION: Session = Session(UnsafeCell::new(None));


impl Session {
  #[allow(clippy::mut_from_ref)]
  unsafe fn inner (&self) -> &mut Option<Vec<Message>> {
    &mut *self.0.get()
  }

  #[allow(clippy::mut_from_ref)]
  fn vec (&self) -> &mut Vec<Message> {
    let inner = unsafe { self.inner() };
    inner.as_mut().expect("Internal error: Session not initialized")
  }

  /// Get a slice of the Messages in a SESSION
  pub fn messages (&self) -> &[Message] {
    self.vec().as_slice()
  }

  /// Initialize the Session singleton
  /// 
  /// # Safety
  /// This should only be called once at the start of a session
  pub fn init (&self) {
    let inner = unsafe { self.inner() };
    assert!(inner.is_none(), "Internal error: Session double initialized");

    inner.replace(Vec::default());
  }

  /// Add a Message to the list of Messages associated with a Session
  #[allow(clippy::mut_from_ref)]
  pub fn message (&self, origin: SourceRegion, kind: MessageKind, content: String) -> &mut Message {
    let msg = Message::new(
      kind,
      origin,
      content
    );

    #[cfg(feature = "backtrace")] {
      let bt = backtrace::Backtrace::new();

      println!("New session message caught:\n{}\nat {:?}", &msg, bt);
    }

    self.vec().push(msg);

    self.vec().last_mut().unwrap()
  }

  
  /// Add an Error Message to the list of Messages associated with a Session
  #[allow(clippy::mut_from_ref)]
  pub fn error (&self, origin: SourceRegion, content: String) -> &mut Message {
    self.message(
      origin,
      MessageKind::Error,
      content
    )
  }
  
  /// Add a Warning Message to the list of Messages associated with a Session
  #[allow(clippy::mut_from_ref)]
  pub fn warning (&self, origin: SourceRegion, content: String) -> &mut Message {
    self.message(
      origin,
      MessageKind::Warning,
      content
    )
  }
  
  /// Add a Notice Message to the list of Messages associated with a Session
  #[allow(clippy::mut_from_ref)]
  pub fn notice (&self, origin: SourceRegion, content: String) -> &mut Message {
    self.message(
      origin,
      MessageKind::Notice,
      content
    )
  }


  /// Print all Messages associated with a Session
  pub fn print_messages (&self) {
    for message in self.vec().iter() {
      print!("{}", message)
    }
  }
  

  /// Print all Error Messages associated with a Session
  pub fn print_errors (&self) {
    for message in self.vec().iter() {
      if message.kind == MessageKind::Error { print!("{}", message) }
    }
  }

  /// Print all Warning Messages associated with a Session
  pub fn print_warnings (&self) {
    for message in self.vec().iter() {
      if message.kind == MessageKind::Warning { print!("{}", message) }
    }
  }

  /// Print all Notice Messages associated with a Session
  pub fn print_notices (&self) {
    for message in self.vec().iter() {
      if message.kind == MessageKind::Notice { print!("{}", message) }
    }
  }

    /// Count how many Messages associated with a Session are Errors
    pub fn count_errors (&self) -> usize {
      let mut count = 0usize;
      for message in self.vec().iter() {
        if message.kind == MessageKind::Error { count += 1 }
      }
      count
    }
  
    /// Count how many Messages associated with a Session are Warnings
    pub fn count_warnings (&self) -> usize {
      let mut count = 0usize;
      for message in self.vec().iter() {
        if message.kind == MessageKind::Warning { count += 1 }
      }
      count
    }
  
    /// Count how many Messages associated with a Session are Notices
    pub fn count_notices (&self) -> usize {
      let mut count = 0usize;
      for message in self.vec().iter() {
        if message.kind == MessageKind::Notice { count += 1 }
      }
      count
    }
}