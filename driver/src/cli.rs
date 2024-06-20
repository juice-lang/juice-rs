use std::{
    borrow::Cow,
    ffi::OsStr,
    fs::File,
    io,
    path::{self, PathBuf},
};

use clap::{error::ErrorKind, Args, CommandFactory, Parser, Subcommand};
use juice_core::OutputStream;
use juice_frontend::{RunnerAction as FrontendAction, RunnerArgs as FrontendArgs};

use crate::driver::{Error as DriverError, MainAction, MainArgs, Result as DriverResult};

#[derive(Debug, Parser)]
#[command(name = "juice")]
#[command(version, about = "The juice-lang compiler", long_about = None)]
#[command(args_conflicts_with_subcommands = true)]
pub struct Cli {
    #[command(subcommand)]
    command: Option<private::Command>,

    #[command(flatten)]
    main_args: Option<private::MainArgs>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OutputFilePath {
    Stdout,
    File(PathBuf),
}

impl OutputFilePath {
    pub fn try_into_stream(self) -> DriverResult<Box<dyn OutputStream>> {
        match self {
            Self::Stdout => Ok(Box::new(io::stdout())),
            Self::File(path) => Ok(Box::new(File::create(path)?)),
        }
    }

    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        match self {
            Self::Stdout => Cow::Borrowed("-"),
            Self::File(path) => path.to_string_lossy(),
        }
    }

    pub fn as_os_str(&self) -> &OsStr {
        match self {
            Self::Stdout => OsStr::new("-"),
            Self::File(path) => path.as_os_str(),
        }
    }
}

mod private {
    use super::*;

    #[derive(Debug, Args)]
    #[clap(group = clap::ArgGroup::new("frontend-action").multiple(false))]
    pub struct FrontendArgs {
        /// The input file to compile
        #[arg(long = "input-file", value_parser = absolute_path_parser, value_name = "FILE")]
        pub input_filepath: PathBuf,
        /// The output file to write to (can be `-` for stdout)
        #[arg(long = "output-file", value_parser = absolute_output_file_path_parser, value_name = "FILE")]
        pub output_filepath: OutputFilePath,
        /// Parse the input file and dump the AST
        #[arg(long, group = "frontend-action", help_heading = "Actions")]
        pub dump_parse: bool,
        /// Parse and type-check the input file and dump the type-checked AST
        #[arg(long, group = "frontend-action", help_heading = "Actions")]
        pub dump_ast: bool,
        /// Emit an LLVM IR file
        #[arg(long, group = "frontend-action", help_heading = "Actions")]
        pub emit_ir: bool,
        /// Emit an object file
        #[arg(long, group = "frontend-action", help_heading = "Actions")]
        pub emit_object: bool,
    }

    fn absolute_path_parser(s: &str) -> Result<PathBuf, String> {
        let path = PathBuf::from(s);

        if path.is_absolute() {
            Ok(path)
        } else {
            Err("path must be absolute".into())
        }
    }

    fn absolute_output_file_path_parser(s: &str) -> Result<OutputFilePath, String> {
        if s == "-" {
            Ok(OutputFilePath::Stdout)
        } else {
            absolute_path_parser(s).map(OutputFilePath::File)
        }
    }

    #[derive(Debug, Subcommand)]
    pub enum Command {
        /// Run the frontend to compile one file
        #[command(hide = true)]
        Frontend(FrontendArgs),
        /// Run the run-eval-print loop
        Repl,
    }

    #[derive(Debug, Args)]
    #[clap(group = clap::ArgGroup::new("main-action").multiple(false))]
    pub struct MainArgs {
        /// The input file to compile
        #[arg(value_name = "FILE")]
        pub input_filename: PathBuf,
        /// The output file to write to (can be `-` for stdout)
        #[arg(short, long = "output-file", value_parser = output_file_path_parser, value_name = "FILE")]
        pub output_filename: Option<OutputFilePath>,
        /// Use verbose output
        #[arg(short, long)]
        pub verbose: bool,
        /// Parse the input file and dump the AST
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub dump_parse: bool,
        /// Parse and type-check the input file and dump the type-checked AST
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub dump_ast: bool,
        /// Dump the generated LLVM IR
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub dump_ir: bool,
        /// Emit an LLVM IR file
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub emit_ir: bool,
        /// Emit an object file
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub emit_object: bool,
        /// Emit an executable (default)
        #[arg(long, group = "main-action", help_heading = "Actions")]
        pub emit_executable: bool,
    }

    fn output_file_path_parser(s: &str) -> Result<OutputFilePath, String> {
        if s == "-" {
            Ok(OutputFilePath::Stdout)
        } else {
            Ok(OutputFilePath::File(s.into()))
        }
    }
}

impl TryFrom<private::FrontendArgs> for FrontendArgs {
    type Error = DriverError;

    fn try_from(args: private::FrontendArgs) -> DriverResult<Self> {
        let input_filepath = args.input_filepath;
        let output_stream = args.output_filepath.try_into_stream()?;

        let action = if args.dump_parse {
            FrontendAction::DumpParse
        } else if args.dump_ast {
            FrontendAction::DumpAst
        } else if args.emit_ir {
            FrontendAction::EmitIr
        } else {
            FrontendAction::EmitObject
        };

        Ok(Self {
            input_filepath,
            output_stream,
            action,
        })
    }
}

impl TryFrom<private::MainArgs> for MainArgs {
    type Error = DriverError;

    fn try_from(args: private::MainArgs) -> DriverResult<Self> {
        let input_filepath = path::absolute(&args.input_filename)?;

        let action = if args.dump_parse {
            MainAction::DumpParse
        } else if args.dump_ast {
            MainAction::DumpAst
        } else if args.dump_ir {
            MainAction::DumpIr
        } else if args.emit_ir {
            MainAction::EmitIr
        } else if args.emit_object {
            MainAction::EmitObject
        } else {
            MainAction::EmitExecutable
        };

        let output_filepath = action.output_filepath(&input_filepath, args.output_filename)?;

        Ok(Self {
            input_filepath,
            output_filepath,
            verbose: args.verbose,
            action,
        })
    }
}

#[derive(Debug)]
pub enum Command {
    Frontend(FrontendArgs),
    Repl,
    Main(MainArgs),
}

impl TryFrom<private::Command> for Command {
    type Error = DriverError;

    fn try_from(command: private::Command) -> DriverResult<Self> {
        match command {
            private::Command::Frontend(args) => args.try_into().map(Self::Frontend),
            private::Command::Repl => Ok(Self::Repl),
        }
    }
}

impl Cli {
    pub fn command(self) -> DriverResult<Command> {
        if let Some(command) = self.command {
            command.try_into()
        } else {
            let Some(main_args) = self.main_args else {
                <Self as CommandFactory>::command()
                    .error(
                        ErrorKind::MissingRequiredArgument,
                        "either a subcommand or main arguments are required",
                    )
                    .exit();
            };

            main_args.try_into().map(Command::Main)
        }
    }
}

#[cfg(test)]
mod tests {
    use clap::CommandFactory;

    use super::*;

    #[test]
    fn test_cli() {
        <Cli as CommandFactory>::command().debug_assert()
    }
}
