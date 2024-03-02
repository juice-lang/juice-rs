#[cfg(target_os = "macos")]
mod macos;
mod task;

use std::{
    io,
    path::{self, Path, PathBuf},
};

use self::task::{CompilationTask, ErasedTask, InputTask, LinkingTask};
use super::{Driver as DriverTrait, Error as DriverError, Result as DriverResult};
use crate::cli::OutputFilePath;

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub enum Action {
    DumpParse,
    DumpAst,
    DumpIr,
    EmitIr,
    EmitObject,
    #[default]
    EmitExecutable,
}

impl Action {
    pub fn output_filepath(
        &self,
        input_filepath: &Path,
        output_filename: Option<OutputFilePath>,
    ) -> Result<OutputFilePath, io::Error> {
        match output_filename {
            Some(OutputFilePath::Stdout) => Ok(OutputFilePath::Stdout),
            Some(OutputFilePath::File(filename)) => path::absolute(filename).map(OutputFilePath::File),
            None => {
                let extension = match self {
                    Self::DumpParse | Self::DumpAst | Self::DumpIr => return Ok(OutputFilePath::Stdout),
                    Self::EmitIr => "ll",
                    Self::EmitObject => "o",
                    Self::EmitExecutable => "",
                };

                let mut output_filepath = input_filepath.to_owned();
                output_filepath.set_extension(extension);
                Ok(OutputFilePath::File(output_filepath))
            }
        }
    }
}

#[derive(Debug)]
pub struct Args {
    pub input_filepath: PathBuf,
    pub output_filepath: OutputFilePath,
    pub action: Action,
}

pub struct Driver {
    args: Args,
}

impl Driver {
    pub fn new(args: Args) -> Self {
        Self { args }
    }
}

impl DriverTrait for Driver {
    async fn run(self) -> DriverResult<()> {
        let input_task = InputTask::new(self.args.input_filepath.clone());

        let action = self.args.action;

        let task: Box<dyn ErasedTask> = if matches!(self.args.output_filepath, OutputFilePath::Stdout) {
            match action {
                Action::EmitObject => return Err(DriverError::ObjectToStdout),
                Action::EmitExecutable => return Err(DriverError::LinkerOutputToStdout),
                _ => Box::new(CompilationTask::new(action, input_task, OutputFilePath::Stdout, false)?),
            }
        } else if action == Action::EmitExecutable {
            let compilation_task = CompilationTask::new_with_temporary_output(action, input_task)?;

            Box::new(LinkingTask::new(
                vec![Box::new(compilation_task)],
                self.args.output_filepath.clone(),
            )?)
        } else {
            Box::new(CompilationTask::new(
                action,
                input_task,
                self.args.output_filepath.clone(),
                false,
            )?)
        };

        task.execute().await
    }
}
