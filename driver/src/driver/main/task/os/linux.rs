use std::{ffi::OsString, path::PathBuf};

use itertools::Itertools as _;
use which::which;

use crate::{
    cli::OutputFilePath,
    driver::{main::task::ErasedTask, Error as DriverError, Result as DriverResult},
};

pub fn find_linker() -> DriverResult<PathBuf> {
    const LINKER_NAMES: [&str; 5] = ["lld", "ld.lld", "gold", "ld.gold", "ld"];

    LINKER_NAMES
        .iter()
        .map(which)
        .find_or_last(Result::is_ok)
        .unwrap()
        .map_err(|err| DriverError::ExecutableNotFound {
            executable_name: String::from("ld"),
            error: err,
        })
}

pub async fn get_linker_path_and_args(
    inputs: &Vec<Box<dyn ErasedTask>>,
    output_path: &OutputFilePath,
) -> DriverResult<(PathBuf, Vec<OsString>)> {
    let executable_path = find_linker()?;

    let mut arguments = vec![OsString::from("-o"), output_path.as_os_str().to_owned()];

    for input in inputs {
        arguments.push(input.get_output_path().as_os_str().to_owned());
    }

    Ok((executable_path, arguments))
}
