use std::fmt::{ Debug, Display, Formatter, Result as FMTResult };

/// Wrapper for printing slices of char
pub struct CharPrintWrapper<'a>(pub &'a[char]);

impl<'a> Display for CharPrintWrapper<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    for ch in self.0.iter() {
      write!(f, "{}", ch)?;
    }
    
    Ok(())
  }
}

impl<'a> Debug for CharPrintWrapper<'a> {
  fn fmt (&self, f: &mut Formatter) -> FMTResult {
    Display::fmt(self, f)
  }
}