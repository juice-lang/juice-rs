#![feature(
    assert_matches,
    iter_advance_by,
    macro_metavar_expr,
    min_exhaustive_patterns,
    trait_alias
)]

mod ast;
mod diag;
mod error;
mod parser;
mod runner;
mod source_loc;
mod source_manager;

pub(crate) use crate::error::{Error, Result};
pub use crate::runner::{Action as RunnerAction, Args as RunnerArgs, Runner};
