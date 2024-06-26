mod os;

use std::{
    borrow::Cow,
    env,
    ffi::{OsStr, OsString},
    fs,
    future::Future,
    io::ErrorKind as IoErrorKind,
    os::unix::process::ExitStatusExt as _,
    path::{Path, PathBuf},
    pin::Pin,
    sync::LazyLock,
    time::SystemTime,
};

use async_process::Command;
use tempfile::Builder as TempFileBuilder;

use self::os::get_linker_path_and_args;
use super::Action;
use crate::{
    cli::OutputFilePath,
    driver::{Error as DriverError, Result as DriverResult},
};

mod private {
    use super::*;

    pub trait Task: Send + Sync {
        fn get_executable_path(&self) -> &PathBuf;
        fn get_arguments(&self) -> &[OsString];
        fn get_inputs(&self) -> &[Box<dyn ErasedTask>];
        fn get_output_path(&self) -> Cow<OutputFilePath>;
        fn get_output_is_temporary(&self) -> bool;

        fn create_execution_error(&self, exit_code: i32) -> DriverError {
            DriverError::ExecutionFailed {
                executable_path: self.get_executable_path().clone(),
                exit_code,
            }
        }

        async fn execute_inputs(&self, time_point: SystemTime, verbose: bool) -> DriverResult<bool> {
            let mut executed = false;

            for input in self.get_inputs() {
                if input.execute_if_necessary(time_point, verbose).await? {
                    executed = true;
                }
            }

            Ok(executed)
        }
    }
}

pub trait Task: private::Task {
    async fn execute_if_necessary(&self, time_point: SystemTime, verbose: bool) -> DriverResult<bool> {
        match self.get_output_path().as_ref() {
            OutputFilePath::Stdout => {
                self.execute_inputs(time_point, verbose).await?;
            }
            OutputFilePath::File(path) => match fs::metadata(path) {
                Ok(metadata) if metadata.is_file() => {
                    if self.get_output_is_temporary() {
                        let inputs_executed = self.execute_inputs(time_point, verbose).await?;

                        if !inputs_executed {
                            return Ok(false);
                        }
                    } else {
                        let modification_time = metadata.modified().unwrap();

                        let inputs_executed = self.execute_inputs(modification_time, verbose).await?;

                        if !inputs_executed {
                            return Ok(modification_time > time_point);
                        }
                    }
                }
                Ok(_) => return Err(DriverError::FileNotRegular(path.to_owned())),
                Err(error) if error.kind() == IoErrorKind::NotFound => {
                    self.execute_inputs(time_point, verbose).await?;
                }
                Err(error) => return Err(error.into()),
            },
        }

        if verbose {
            print!("{}", self.get_executable_path().to_string_lossy());
            for argument in self.get_arguments() {
                print!(" {}", argument.to_string_lossy());
            }
            println!();
        }

        let status = Command::new(self.get_executable_path())
            .args(self.get_arguments())
            .status()
            .await?;

        match status.code() {
            Some(0) => Ok(true),
            Some(exit_code) => Err(self.create_execution_error(exit_code)),
            None => Err(if let Some(signal) = status.signal() {
                DriverError::ProcessTerminatedBySignal {
                    executable_path: self.get_executable_path().clone(),
                    signal,
                }
            } else if let Some(signal) = status.stopped_signal() {
                DriverError::ProcessTerminatedBySignal {
                    executable_path: self.get_executable_path().clone(),
                    signal,
                }
            } else {
                DriverError::Unexpected
            }),
        }
    }

    async fn execute(&self, verbose: bool) -> DriverResult<()> {
        let time_point = SystemTime::now();

        self.execute_if_necessary(time_point, verbose).await?;

        Ok(())
    }
}

pub trait ErasedTask: Send + Sync {
    fn get_output_path(&self) -> Cow<OutputFilePath>;

    fn execute_if_necessary(
        &self,
        time_point: SystemTime,
        verbose: bool,
    ) -> Pin<Box<dyn '_ + Future<Output = DriverResult<bool>>>>;

    fn execute(&self, verbose: bool) -> Pin<Box<dyn '_ + Future<Output = DriverResult<()>>>>;
}

impl<T: ?Sized + Task> ErasedTask for T {
    fn get_output_path(&self) -> Cow<OutputFilePath> {
        self.get_output_path()
    }

    fn execute_if_necessary(
        &self,
        time_point: SystemTime,
        verbose: bool,
    ) -> Pin<Box<dyn '_ + Future<Output = DriverResult<bool>>>> {
        Box::pin(self.execute_if_necessary(time_point, verbose))
    }

    fn execute(&self, verbose: bool) -> Pin<Box<dyn '_ + Future<Output = DriverResult<()>>>> {
        Box::pin(self.execute(verbose))
    }
}

static EMPTY_PATH: LazyLock<PathBuf> = LazyLock::new(PathBuf::new);
static EMPTY_ARGUMENTS: LazyLock<Vec<OsString>> = LazyLock::new(Vec::new);
static EMPTY_INPUTS: LazyLock<Vec<Box<dyn ErasedTask>>> = LazyLock::new(Vec::new);

pub struct InputTask(PathBuf);

impl InputTask {
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Self(path.as_ref().to_owned())
    }
}

impl private::Task for InputTask {
    fn get_executable_path(&self) -> &PathBuf {
        &EMPTY_PATH
    }

    fn get_arguments(&self) -> &[OsString] {
        &EMPTY_ARGUMENTS
    }

    fn get_inputs(&self) -> &[Box<dyn ErasedTask>] {
        &EMPTY_INPUTS
    }

    fn get_output_path(&self) -> Cow<OutputFilePath> {
        Cow::Owned(OutputFilePath::File(self.0.to_owned()))
    }

    fn get_output_is_temporary(&self) -> bool {
        false
    }
}

impl Task for InputTask {
    async fn execute_if_necessary(&self, time_point: SystemTime, _verbose: bool) -> DriverResult<bool> {
        match fs::metadata(&self.0) {
            Ok(metadata) if metadata.is_file() => {
                if metadata.modified().unwrap() > time_point {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Ok(_) => Err(DriverError::FileNotRegular(self.0.to_owned())),
            Err(error) if error.kind() == IoErrorKind::NotFound => Err(DriverError::FileNotFound(self.0.to_owned())),
            Err(error) => Err(error.into()),
        }
    }
}

pub struct CompilationTask {
    executable_path: PathBuf,
    arguments: Vec<OsString>,
    inputs: Vec<Box<dyn ErasedTask>>,
    output_path: OutputFilePath,
    output_is_temporary: bool,
}

impl CompilationTask {
    pub fn new(
        action: Action,
        input: impl 'static + Task,
        output_path: OutputFilePath,
        output_is_temporary: bool,
    ) -> DriverResult<Self> {
        let executable_path = env::current_exe()?;

        let (action_string, json) = match action {
            Action::DumpParse { json } => ("--dump-parse", json.then_some("--json")),
            Action::DumpAst { json } => ("--dump-ast", json.then_some("--json")),
            Action::DumpIr | Action::EmitIr => ("--emit-ir", None),
            Action::EmitObject | Action::EmitExecutable => ("--emit-object", None),
        };

        let arguments = vec![
            OsStr::new("frontend"),
            OsStr::new(action_string),
            OsStr::new("--input-file"),
            input.get_output_path().as_os_str(),
            OsStr::new("--output-file"),
            output_path.as_os_str(),
        ]
        .into_iter()
        .chain(json.map(OsStr::new))
        .map(OsString::from)
        .collect();

        let input = Box::new(input) as Box<dyn ErasedTask>;

        Ok(Self {
            executable_path,
            arguments,
            inputs: vec![input],
            output_path,
            output_is_temporary,
        })
    }

    pub fn new_with_temporary_output(action: Action, input: impl 'static + Task) -> DriverResult<Self> {
        let input_path = input.get_output_path();
        let input_base_name = if let OutputFilePath::File(path) = input_path.as_ref() {
            path.file_stem().ok_or(DriverError::FileHasNoName(path.clone()))?
        } else {
            return Err(DriverError::Unexpected);
        };

        let temp_file = TempFileBuilder::new()
            .prefix(input_base_name)
            .suffix(".o")
            .rand_bytes(5)
            .tempfile()?;

        Self::new(action, input, OutputFilePath::File(temp_file.path().to_owned()), true)
    }
}

impl private::Task for CompilationTask {
    fn get_executable_path(&self) -> &PathBuf {
        &self.executable_path
    }

    fn get_arguments(&self) -> &[OsString] {
        &self.arguments
    }

    fn get_inputs(&self) -> &[Box<dyn ErasedTask>] {
        &self.inputs
    }

    fn get_output_path(&self) -> Cow<OutputFilePath> {
        Cow::Borrowed(&self.output_path)
    }

    fn get_output_is_temporary(&self) -> bool {
        self.output_is_temporary
    }

    fn create_execution_error(&self, exit_code: i32) -> DriverError {
        DriverError::AlreadyHandled(exit_code)
    }
}

impl Task for CompilationTask {}

pub struct LinkingTask {
    executable_path: PathBuf,
    arguments: Vec<OsString>,
    inputs: Vec<Box<dyn ErasedTask>>,
    output_path: OutputFilePath,
}

impl LinkingTask {
    pub async fn new(inputs: Vec<Box<dyn ErasedTask>>, output_path: OutputFilePath) -> DriverResult<Self> {
        let (executable_path, arguments) = get_linker_path_and_args(&inputs, &output_path).await?;

        Ok(Self {
            executable_path,
            arguments,
            inputs,
            output_path,
        })
    }
}

impl private::Task for LinkingTask {
    fn get_executable_path(&self) -> &PathBuf {
        &self.executable_path
    }

    fn get_arguments(&self) -> &[OsString] {
        &self.arguments
    }

    fn get_inputs(&self) -> &[Box<dyn ErasedTask>] {
        &self.inputs
    }

    fn get_output_path(&self) -> Cow<OutputFilePath> {
        Cow::Borrowed(&self.output_path)
    }

    fn get_output_is_temporary(&self) -> bool {
        false
    }
}

impl Task for LinkingTask {}
