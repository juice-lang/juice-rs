#![feature(
    assert_matches,
    iter_advance_by,
    is_none_or,
    macro_metavar_expr,
    min_exhaustive_patterns,
    once_cell_try_insert,
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
