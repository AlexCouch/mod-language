/// A trait allowing wrapper types to dereference their interior value into a copy while maintaining the wrapper
/// 
/// For example, Option<&T> can be dereferenced to Option<T> where the interior value is copied into the new option
pub trait DerefWrapper {
  /// The type returned by `deref_wrapper`,
  /// eg for `Option<&T>` DerefWrapperResult is `Option<T>`
  type DerefWrapperResult;
  /// Dereference the interior of a wrapper type and copy its value into a new wrapper
  fn deref_wrapper (self) -> Self::DerefWrapperResult;
}

impl<T> DerefWrapper for Option<&T>
where T: Copy
{
  type DerefWrapperResult = Option<T>;

  fn deref_wrapper (self) -> Option<T> {
    match self {
      Some(val) => Some(*val),
      None => None
    }
  }
}

impl<R, E> DerefWrapper for Result<&R, E>
where R: Copy
{
  type DerefWrapperResult = Result<R, E>;

  fn deref_wrapper (self) -> Result<R, E> {
    match self {
      Ok(val) => Ok(*val),
      Err(err) => Err(err)
    }
  }
}