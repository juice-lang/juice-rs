#![feature(impl_trait_in_assoc_type, iter_advance_by)]

mod char_ext;
pub mod diag;
pub mod dump;
pub mod parser_ext;
mod peekable_chars;

use std::io::{IsTerminal, Write};

pub use crate::{char_ext::CharExt, peekable_chars::PeekableChars};

pub trait OutputStream: Write + IsTerminal {}

impl<T: Write + IsTerminal> OutputStream for T {}
