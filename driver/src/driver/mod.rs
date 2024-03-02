mod frontend;
mod main;
mod repl;

use std::{future::Future, io, path::PathBuf, pin::Pin};

use derive_more::From;

pub use self::{
    frontend::Driver as FrontendDriver,
    main::{Action as MainAction, Args as MainArgs, Driver as MainDriver},
    repl::Driver as ReplDriver,
};
use crate::cli::{Cli, Command};

#[derive(Debug, From)]
pub enum Error {
    #[from]
    Io(io::Error),
    ExecutableNotFound {
        executable_name: String,
        error: which::Error,
    },
    ExecutionFailed {
        executable_path: PathBuf,
        exit_code: i32,
    },
    ProcessTerminatedBySignal {
        executable_path: PathBuf,
        signal: i32,
    },
    FileNotFound(PathBuf),
    FileNotRegular(PathBuf),
    FileHasNoName(PathBuf),
    LinkerOutputToStdout,
    ObjectToStdout,
    Unexpected,
    AlreadyHandled(i32),
}

pub type Result<T> = std::result::Result<T, Error>;

trait Driver {
    async fn run(self) -> Result<()>;
}

pub trait ErasedDriver {
    fn run(self: Box<Self>) -> Pin<Box<dyn Future<Output = Result<()>>>>;
}

impl<T: 'static + Driver> ErasedDriver for T {
    fn run(self: Box<Self>) -> Pin<Box<dyn Future<Output = Result<()>>>> {
        Box::pin(Box::into_inner(self).run())
    }
}

pub fn get_driver(args: Cli) -> Result<Box<dyn ErasedDriver>> {
    Ok(match args.command()? {
        Command::Frontend(frontend_args) => Box::new(FrontendDriver::new(frontend_args)),
        Command::Main(main_args) => Box::new(MainDriver::new(main_args)),
        Command::Repl => Box::new(ReplDriver::new()),
    })
}
