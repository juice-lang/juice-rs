#![feature(assert_matches, iter_advance_by, macro_metavar_expr, try_trait_v2)]

mod diag;
mod error;
mod parser;
mod runner;
mod source_loc;
mod source_manager;

pub(crate) use crate::error::{Error, Result};
pub use crate::runner::{Action as RunnerAction, Args as RunnerArgs, Runner};
