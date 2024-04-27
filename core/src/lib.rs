#![feature(iter_advance_by, never_type, try_trait_v2)]

mod char_ext;
pub mod diag;
mod option_ext;
mod peekable_chars;

use std::ops::{ControlFlow, FromResidual, Try};

pub use crate::{char_ext::CharExt, option_ext::OptionExt, peekable_chars::PeekableChars};

pub struct Unit;

impl FromResidual<!> for Unit {
    fn from_residual(_: !) -> Self {
        unreachable!()
    }
}

impl Try for Unit {
    type Output = ();
    type Residual = !;

    fn branch(self) -> ControlFlow<!> {
        ControlFlow::Continue(())
    }

    fn from_output(_: ()) -> Self {
        Self
    }
}
