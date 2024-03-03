use std::{
    ffi::OsStr,
    os::unix::{ffi::OsStrExt as _, process::ExitStatusExt as _},
    path::{Path, PathBuf},
};

use async_once_cell::OnceCell;
use async_process::Command;
use which::which;

use crate::driver::{Error as DriverError, Result as DriverResult};

pub async fn get_sdk_path() -> DriverResult<&'static Path> {
    static SDK_PATH: OnceCell<PathBuf> = OnceCell::new();

    SDK_PATH
        .get_or_try_init(async {
            let xcrun_path = which("xcrun").map_err(|err| DriverError::ExecutableNotFound {
                executable_name: String::from("xcrun"),
                error: err,
            })?;

            let output = Command::new(&xcrun_path)
                .args(["--sdk", "macosx", "--show-sdk-path"])
                .output()
                .await?;

            let status = output.status;

            match status.code() {
                Some(0) => Ok(PathBuf::from(OsStr::from_bytes(output.stdout.trim_ascii()))),
                Some(exit_code) => Err(DriverError::ExecutionFailed {
                    executable_path: xcrun_path,
                    exit_code,
                }),
                None => Err(if let Some(signal) = status.signal() {
                    DriverError::ProcessTerminatedBySignal {
                        executable_path: xcrun_path,
                        signal,
                    }
                } else if let Some(signal) = status.stopped_signal() {
                    DriverError::ProcessTerminatedBySignal {
                        executable_path: xcrun_path,
                        signal,
                    }
                } else {
                    DriverError::Unexpected
                }),
            }
        })
        .await
        .map(AsRef::as_ref)
}
