#![feature(box_into_inner)]
#![feature(try_blocks)]

pub mod cli;
pub mod driver;

use clap::{
    error::{DefaultFormatter, Error as ClapError, ErrorKind as ClapErrorKind},
    Parser as _,
};

use crate::{
    cli::Cli,
    driver::{Error as DriverError, Result as DriverResult},
};

#[tokio::main]
async fn main() {
    let args = Cli::parse();

    let res: DriverResult<()> = try {
        let driver = driver::get_driver(args)?;
        driver.run().await?
    };

    if let Err(err) = res {
        let message = match err {
            DriverError::Io(err) => err.to_string(),
            DriverError::ExecutableNotFound { executable_name, error } => {
                format!("executable not found: {}: {}\n", executable_name, error)
            }
            DriverError::ExecutionFailed {
                executable_path,
                exit_code,
            } => format!("execution failed: {}: {}\n", executable_path.display(), exit_code),
            DriverError::ProcessTerminatedBySignal {
                executable_path,
                signal,
            } => format!(
                "process terminated by signal: {}: {}\n",
                executable_path.display(),
                signal
            ),
            DriverError::FileNotFound(path) => {
                format!("couldn't read {}: no such file or directory\n", path.display())
            }
            DriverError::FileNotRegular(path) => format!("couldn't read {}: not a regular file\n", path.display()),
            DriverError::FileHasNoName(path) => format!("couldn't extract file name from {}\n", path.display()),
            DriverError::LinkerOutputToStdout => "cannot output executable to stdout\n".to_string(),
            DriverError::ObjectToStdout => "cannot output object file to stdout\n".to_string(),
            DriverError::Unexpected => "unexpected error\n".to_string(),
            DriverError::AlreadyHandled(exit_code) => std::process::exit(exit_code),
        };

        ClapError::<DefaultFormatter>::raw(ClapErrorKind::Io, message).exit();
    }
}
