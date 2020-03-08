//! Contains Source, SourceTracker, and access functions, as well as SourceLocation, SourceRegion, and SourceOrigin

use std::{
  collections::{ HashMap, hash_map::DefaultHasher },
  hash::{ Hash, Hasher },
  io::Result as IOResult,
  fmt::{ Debug, Formatter, Result as FMTResult, },
  fs::read_to_string,
};

/// Contains source code data including its original file path and content
pub struct Source {
  /// The unique ID code used to retrieve a Source from the SourceTracker singleton
  pub id: u32,
  /// A hash of the path associated with a Source, allowing for faster lookup by path
  pub path_hash: u64,
  /// The file path a Source originated from
  pub path: String,
  /// The content of a Source
  pub content: String,
}

/// Contains all Sources used by the compiler
#[derive(Default)]
pub struct SourceTracker {
  sources: HashMap<u32, Source>,
  id_counter: u32,
}

/// The single static container of all Sources used by the compiler
/// 
/// This is currently unsafe to use across threads
pub static mut SOURCES: Option<SourceTracker> = None;

/// Get a mutable reference to the static SourceTracker singleton
/// 
/// Creates the singleton if it is not yet initialized
pub fn get_source_tracker () -> &'static mut SourceTracker {
  unsafe { if let Some(tracker) = SOURCES.as_mut() { tracker } else { SOURCES = Some(SourceTracker::default()); SOURCES.as_mut().unwrap() } }
}


/// Register a Source with the SourceTracker singleton
pub fn register_source (path: String, content: String) -> u32 {
  let mut hasher = DefaultHasher::new();
  path.hash(&mut hasher);
  let path_hash = hasher.finish();

  let tracker = get_source_tracker();
  
  for (id, source) in tracker.sources.iter_mut() {
    if source.path_hash == path_hash
    && source.path == path {
      source.content = content;
      return *id;
    }
  }

  let id = tracker.id_counter;
  tracker.id_counter += 1;

  tracker.sources.insert(id, Source {
    id,
    path_hash,
    path,
    content,
  });

  id
}

/// Load a Source from a file path and register it in the SourceTracker singleton
pub fn load_source (path: String) -> IOResult<u32> {
  let content = read_to_string(&path)?;
  Ok(register_source(path, content))
}

/// Get a registered Source by ID in the SourceTracker singleton
pub fn get_source<'a> (id: u32) -> Option<&'a Source> {
  unsafe { SOURCES.as_mut()? }.sources.get(&id)
}

/// The the ID of a registered Source by looking up its path in the SourceTracker singleton
pub fn get_source_id (path: &str) -> Option<u32> {
  let mut hasher = DefaultHasher::new();
  path.hash(&mut hasher);
  let path_hash = hasher.finish();

  let tracker = unsafe { SOURCES.as_ref()? };

  for (id, source) in tracker.sources.iter() {
    if source.path_hash == path_hash
    && source.path == path {
      return Some(*id);
    }
  }

  None
}

/// Get the path of a Source by ID in the SourceTracker singleton
pub fn get_source_path<'a> (id: u32) -> Option<&'a str> {
  get_source(id).map(|source| source.path.as_ref())
}

/// Get the content of a Source by ID in the SourceTracker singleton
pub fn get_source_content<'a> (id: u32) -> Option<&'a str> {
  get_source(id).map(|source| source.content.as_ref())
}

/// Get the path and content of a Source by ID in the SourceTracker singleton
pub fn get_source_path_and_content<'a> (id: u32) -> Option<(&'a str, &'a str)> {
  get_source(id).map(|source| (source.path.as_ref(), source.content.as_ref()))
}

/// Remove a Source from the SourceTracker singleton
pub fn remove_source (id: u32) {
  unsafe { if let Some(tracker) = SOURCES.as_mut() { tracker.sources.remove(&id); } }
}



/// A pair of integers representing a line and column in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub struct SourceLocation {
  pub line: u32,
  pub column: u32,
}

/// A pair of SourceLocations indicating a region in a source file
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(missing_docs)]
pub struct SourceRegion {
  pub start: SourceLocation,
  pub end: SourceLocation,
}

/// A pair of SourceLocations and a Source ID
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceOrigin {
  /// The area of a Source an item came from
  pub region: SourceRegion,
  /// The id of the Source an item came from
  pub id: u32,
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

impl Debug for SourceOrigin {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    let path = get_source_path(self.id).unwrap_or("[Removed Source]");
    write!(
      f, "[{}:{:?} to {}:{:?}]",
      path,
      self.region.start,
      path,
      self.region.end
    )
  }
}