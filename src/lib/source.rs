//! Contains Source, SourceManager structure and singleton, as well as SourceLocation and SourceRegion

use std::{
  io::Result as IOResult,
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  fs::read_to_string,
  cell::{ UnsafeCell, },
  path::{ Path, PathBuf, },
};

use crate::{
  util::{ make_key_type, },
  collections::{ SlotMap, },
};


/// A set of integers representing an index, line and column in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
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
  pub fn to_region (self, source: Option<SourceKey>) -> SourceRegion {
    SourceRegion {
      source,
      start: self,
      end: self,
    }
  }
}

/// A pair of SourceLocations indicating a region in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
#[allow(missing_docs)]
pub struct SourceRegion {
  pub source: Option<SourceKey>,
  pub start: SourceLocation,
  pub end: SourceLocation,
}

impl SourceRegion {
  /// Create a new SourceRegion from the start of one and the end of another
  /// 
  /// Panics if the input SourceRegions' sources are not equal
  pub fn merge (start: Self, end: Self) -> Self {
    // TODO figure out a way to make this sensible
    // we can't just merge source regions if they are from different files
    // they probably shouldnt ever be but who knows what might happen later
    assert!(start.source == end.source, "Internal error, cannot merge SourceRegions from multiple files");

    Self {
      source: start.source,
      start: start.start,
      end: end.end,
    }
  }
}

impl Debug for SourceLocation {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}:{}", self.line + 1, self.column + 1)
  }
}

impl Display for SourceLocation {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Debug::fmt(self, f)
  }
}

impl Debug for SourceRegion {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    if let Some(source) = self.source {
      let source = SOURCE_MANAGER.get(source).expect("Internal error: Could not get Source for SourceRegion Display");

      write!(f, "{}:{}", source.path.display(), self.start)?;

      if self.end != self.start
      && !(self.end.line == self.start.line && self.end.column == self.start.column + 1) {
        write!(f, " to {}", self.end)?;
      }
    } else {
      write!(f, "AnonymousSource")?;
    }

    Ok(())
  }
}

impl Display for SourceRegion {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Debug::fmt(self, f)
  }
}

/// Contains source code data including its original file path and content
pub struct Source {
  /// The file path a Source originated from
  pub path: PathBuf,
  /// The content of a Source
  pub content: Vec<char>,
}


impl Source {
  /// Load a Source from a text file at a path on disk
  pub fn load<P: AsRef<Path>> (path: P) -> IOResult<Source> {
    let content = read_to_string(&path)?;

    Ok(Source {
      path: path.as_ref().to_path_buf(),
      content: content.chars().collect()
    })
  }

  /// Get an iterator of the chars of the content of a Source
  pub fn chars (&self) -> &[char] {
    self.content.as_slice()
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


make_key_type! {
  /// A SlotMap key used to reference a Source file
  pub struct SourceKey;
}

/// The type of the central repository for Sources processed during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub struct SourceManager (UnsafeCell<Option<SlotMap<SourceKey, Source>>>);

unsafe impl Send for SourceManager { }
unsafe impl Sync for SourceManager { }

/// The central repository for Sources processed during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub static SOURCE_MANAGER: SourceManager = SourceManager(UnsafeCell::new(None));

impl SourceManager {
  #[allow(clippy::mut_from_ref)]
  unsafe fn inner (&self) -> &mut Option<SlotMap<SourceKey, Source>> {
    &mut *self.0.get()
  }

  #[allow(clippy::mut_from_ref)]
  fn map (&self) -> &mut SlotMap<SourceKey, Source> {
    let inner = unsafe { self.inner() };
    inner.as_mut().expect("Internal error: SourceManager not initialized")
  }

  /// Initialize the SourceManager singleton
  /// 
  /// # Safety
  /// This should only be called once at the start of a session
  pub fn init (&self) {
    let inner = unsafe { self.inner() };
    assert!(inner.is_none(), "Internal error: SourceManager double initialized");

    inner.replace(SlotMap::default());
  }

  /// Load a Source from a file path and get a key to it
  pub fn load<P: AsRef<Path>> (&self, path: P) -> IOResult<SourceKey> {
    Ok(self.map().insert(Source::load(path)?))
  }

  /// Convert a SourceKey into a Source reference
  pub fn get (&self, key: SourceKey) -> Option<&Source> {
    self.map().get(key)
  }
}