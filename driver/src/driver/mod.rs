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
    IO(io::Error),
    #[from]
    ExecutableNotFound(which::Error),
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
    LinkerOutputToStdout,
    ObjectToStdout,
    Unexpected,
    AlreadyHandled,
}

trait Driver {
    async fn run(&self) -> Result<(), Error>;
}

pub trait ErasedDriver {
    fn run(&self) -> Pin<Box<dyn '_ + Future<Output = Result<(), Error>>>>;
}

impl<T: Driver> ErasedDriver for T {
    fn run(&self) -> Pin<Box<dyn '_ + Future<Output = Result<(), Error>>>> {
        Box::pin(self.run())
    }
}

pub fn get_driver(args: Cli) -> Result<Box<dyn ErasedDriver>, Error> {
    Ok(match args.command()? {
        Command::Frontend(frontend_args) => Box::new(FrontendDriver::new(frontend_args)),
        Command::Main(main_args) => Box::new(MainDriver::new(main_args)),
        Command::Repl => Box::new(ReplDriver::new()),
    })
}
