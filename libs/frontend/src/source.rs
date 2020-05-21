//! Contains Source, SourceManager structure and singleton, as well as SourceLocation and SourceRegion

use std::{
  io::{ Error as IOError, Result as IOResult, },
  fmt::{ Display, Debug, Formatter, Result as FMTResult, },
  fs::read_to_string,
  cell::{ UnsafeCell, },
  path::{ Path, PathBuf, },
};

use mod_ansi as ansi;
use mod_utils::{ make_key_type, Either, Unref, collections::{ SlotMap, BiMap, }, };

use crate::{
  ast,
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
  pub const fn to_region (self, source: SourceKey) -> SourceRegion {
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
  pub source: SourceKey,
  pub start: SourceLocation,
  pub end: SourceLocation,
}

impl SourceRegion {
  /// A SourceRegion that is not attributed to any file or specific location
  pub const ANONYMOUS: Self = SourceLocation::ZERO.to_region(SourceKey::NULL);

  /// Create a new SourceRegion from the start of one and the end of another
  /// 
  /// If the input SourceRegions' sources are not equal,
  /// this will create an anonymous SourceRegion instead
  pub fn merge (start: Self, end: Self) -> Self {
    // TODO figure out a way to make this more sensible
    if start.source == end.source {
      Self {
        source: start.source,
        start: start.start,
        end: end.end,
      }
    } else {
      Self::ANONYMOUS
    }
  }

  /// Create a new SourceRegion from the start of an existing one
  pub fn clip_to_start (&self) -> Self {
    Self { 
      source: self.source,
      start: self.start,
      end: self.start,
    }
  }

  /// Create a new SourceRegion from the end of an existing one
  pub fn clip_to_end (&self) -> Self {
    Self { 
      source: self.source,
      start: self.end,
      end: self.end,
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
    if let Some(source) = SOURCE_MANAGER.get_source(self.source) {
      write!(f, "{}:{}", source.path.display(), self.start)?;

      if self.end != self.start
      && !(self.end.line == self.start.line && self.end.column == self.start.column + 1) {
        write!(f, " to {}", self.end)?;
      }
    } else {
      write!(f, "UnknownSource")?;
    }

    Ok(())
  }
}

impl Display for SourceRegion {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    write!(f, "{}{:?}{}", ansi::Foreground::Cyan, self, ansi::Foreground::Reset)
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

  /// A SlotMap key used to reference a Module declaration AST
  pub struct ASTKey;
}

struct SourceManagerInterior {
  source_map: SlotMap<SourceKey, Source>,
  ast_map: SlotMap<ASTKey, Option<Vec<ast::Item>>>,
  bi_map: BiMap<SourceKey, ASTKey>,

  module_dir: PathBuf,
}

/// The type of the central repository for Sources processed during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub struct SourceManager (UnsafeCell<Option<SourceManagerInterior>>);

unsafe impl Send for SourceManager { }
unsafe impl Sync for SourceManager { }

/// The central repository for Sources processed during a compilation session
/// 
/// # Safety
/// This is not a thread safe structure
pub static SOURCE_MANAGER: SourceManager = SourceManager(UnsafeCell::new(None));

impl SourceManager {
  #[allow(clippy::mut_from_ref)]
  unsafe fn inner (&self) -> &mut Option<SourceManagerInterior> {
    &mut *self.0.get()
  }

  #[allow(clippy::mut_from_ref)]
  fn source_map (&self) -> &mut SlotMap<SourceKey, Source> {
    let inner = unsafe { self.inner() };
    &mut inner.as_mut().expect("Internal error: SourceManager not initialized").source_map
  }

  #[allow(clippy::mut_from_ref)]
  fn ast_map (&self) -> &mut SlotMap<ASTKey, Option<Vec<ast::Item>>> {
    let inner = unsafe { self.inner() };
    &mut inner.as_mut().expect("Internal error: SourceManager not initialized").ast_map
  }

  #[allow(clippy::mut_from_ref)]
  fn bi_map (&self) -> &mut BiMap<SourceKey, ASTKey> {
    let inner = unsafe { self.inner() };
    &mut inner.as_mut().expect("Internal error: SourceManager not initialized").bi_map
  }

  /// Get the directory containing module declaration files
  pub fn get_module_dir (&self) -> &Path {
    let inner = unsafe { self.inner() };
    inner.as_mut().expect("Internal error: SourceManager not initialized").module_dir.as_ref()
  }

  /// Initialize the SourceManager singleton
  /// 
  /// # Safety
  /// This should only be called once at the start of a session
  pub fn init (&self, module_dir: PathBuf) {
    let inner = unsafe { self.inner() };
    assert!(inner.is_none(), "Internal error: SourceManager double initialized");

    inner.replace(SourceManagerInterior { source_map: SlotMap::default(), ast_map: SlotMap::default(), bi_map: BiMap::new(), module_dir });
  }

  /// Load a Source from a file path and get a key to it
  pub fn load_source<P: AsRef<Path>> (&self, path: P) -> Result<SourceKey, Either<SourceKey, IOError>> {
    for (&key, src) in self.source_map().pair_iter() {
      if src.path == path.as_ref() {
        return Err(Either::A(key))
      }
    }

    match Source::load(path) {
      Ok(source) => Ok(self.source_map().insert(source)),
      Err(e) => Err(Either::B(e))
    }
  }

  /// Convert a SourceKey into a Source reference
  pub fn get_source (&self, key: SourceKey) -> Option<&Source> {
    self.source_map().get(key)
  }


  /// Reserve a cache slot for an AST
  pub fn reserve_ast_cache (&self) -> ASTKey {
    self.ast_map().insert(None)
  }

  /// Set the value of an existing ast cache slot
  /// 
  /// Panics if the designated slot doesnt exist or is already filled
  pub fn set_reserved_ast_cache (&self, key: ASTKey, ast: Vec<ast::Item>) {
    self.ast_map()
        .get_mut(key)
        .expect("Internal error, tried to fill non-existant ast cache")
        .replace(ast)
        .expect_none("Internal error, tried to fill ast reserved cache, but it was already filled");
  }

  /// Get an AST cached in the SourceManager singleton from its ASTKey
  /// 
  /// Panics if the designated slot has not been filled
  pub fn get_ast (&self, key: ASTKey) -> Option<&[ast::Item]> {
    self.ast_map().get(key).map(|opt_vec| opt_vec.as_ref().expect("Internal error, tried to get unfilled ast reserve cache").as_slice())
  }

  /// Bind an ASTKey to a SourceKey for lookup later
  /// 
  /// Panics if the given SourceKey already has an ASTKey bound to it
  pub fn bind_ast_to_source (&self, ast_key: ASTKey, src_key: SourceKey) {
    self.bi_map().insert_at_key(src_key, ast_key).unwrap_none();
  }

  /// Get an ASTKey from a SourceKey if one is bound to it
  pub fn get_ast_key_from_source (&self, src_key: SourceKey) -> Option<ASTKey> {
    self.bi_map().find_value(&src_key).unref()
  }

  /// Get an SourceKey from an ASTKey if one is bound to it
  pub fn get_source_key_from_ast (&self, ast_key: ASTKey) -> Option<SourceKey> {
    self.bi_map().find_key(&ast_key).unref()
  }
}