#![feature(impl_trait_in_assoc_type, iter_advance_by)]

mod char_ext;
pub mod diag;
pub mod dump;
pub mod parser_ext;
mod peekable_chars;

pub use crate::{char_ext::CharExt, peekable_chars::PeekableChars};
