//! Contains Source, SourceTracker, and access functions, as well as SourceLocation, SourceRegion, and SourceOrigin

use std::{
  io::Result as IOResult,
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  fs::read_to_string,
  cell::RefCell,
  str::Chars,
};


/// A pair of integers representing a line and column in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub struct SourceLocation {
  pub line: u32,
  pub column: u32,
}

impl SourceLocation {
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

impl Display for MessageKind {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}", match self {
      Self::Error => "Error",
      Self::Warning => "Warning",
      Self::Notice => "Notice",
    })
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
  pub content: Option<String>,
}

impl Message {
  /// Create a new user-directed Message for a Source
  pub fn new (origin: Option<SourceRegion>, kind: MessageKind, content: Option<String>) -> Self {
    Self {
      origin,
      kind,
      content,
    }
  }

  /// Print a Message to the stdout
  /// 
  /// Requires a reference to its parent Source for paths
  pub fn print (&self, source: &Source) {
    print!("{} at [{}", self.kind, source.path);

    if let Some(origin) = self.origin {
      print!(":{:?} to {}:{:?}", origin.start, source.path, origin.end);
    }

    print!("]");

    if let Some(content) = self.content.as_ref() {
      print!(": {}", content);
    }

    println!();
  }
}


/// Contains source code data including its original file path and content
pub struct Source {
  /// The file path a Source originated from
  pub path: String,
  /// The content of a Source
  pub content: String,
  /// User-directed messages created while processing a Source
  pub messages: RefCell<Vec<Message>>,
}


impl Source {
  /// Load a Source from a text file at a path on disk
  pub fn load (path: String) -> IOResult<Source> {
    let content = read_to_string(&path)?;

    Ok(Source {
      path,
      content,
      messages: RefCell::new(Vec::new())
    })
  }

  /// Get an iterator of the chars of the content of a Source
  pub fn chars (&self) -> Chars {
    self.content.chars()
  }


  /// Add a Message to the list of Messages associated with a Source
  pub fn message (&self, origin: Option<SourceRegion>, kind: MessageKind, content: Option<String>) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      kind,
      content
    ))
  }

  
  /// Add an Error Message to the list of Messages associated with a Source
  pub fn error (&self, origin: Option<SourceRegion>, content: Option<String>) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      MessageKind::Error,
      content
    ))
  }
  
  /// Add a Warning Message to the list of Messages associated with a Source
  pub fn warning (&self, origin: Option<SourceRegion>, content: Option<String>) {
    self.messages.borrow_mut().push(Message::new(
      origin,
      MessageKind::Warning,
      content
    ))
  }
  
  /// Add a Notice Message to the list of Messages associated with a Source
  pub fn notice (&self, origin: Option<SourceRegion>, content: Option<String>) {
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
}