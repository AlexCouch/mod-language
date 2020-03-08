//! General use utility functions and structures not related to a specific area of the compiler

mod deref_wrapper;
pub use deref_wrapper::*;

mod char_print_wrapper;
pub use char_print_wrapper::*;

mod count_digits;
pub use count_digits::*;

mod padding;
pub use padding::*;

mod either;
pub use either::*;