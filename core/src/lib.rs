#![feature(generic_nonzero, iter_advance_by)]

mod char_ext;
pub mod diag;
mod option_ext;
mod peekable_chars;

pub use crate::{char_ext::CharExt, option_ext::OptionExt, peekable_chars::PeekableChars};
