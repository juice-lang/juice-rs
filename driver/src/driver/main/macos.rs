use std::{
    path::{Path, PathBuf},
    sync::OnceLock,
};

use which::which;

use crate::driver::{Error as DriverError, Result as DriverResult};

pub fn get_sdk_path() -> DriverResult<&'static Path> {
    static SDK_PATH: OnceLock<PathBuf> = OnceLock::new();

    SDK_PATH
        .get_or_try_init(|| {
            let xcrun_path = which("xcrun").map_err(|err| DriverError::ExecutableNotFound {
                executable_name: String::from("xcrun"),
                error: err,
            })?;

            let output = std::process::Command::new(xcrun_path)
                .args(["--sdk", "macosx", "--show-sdk-path"])
                .output()?;

            Ok(PathBuf::from(
                String::from_utf8(output.stdout).map_err(|_| DriverError::Unexpected)?,
            ))
        })
        .map(AsRef::as_ref)
}
