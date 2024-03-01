use std::{
    path::{Path, PathBuf},
    sync::OnceLock,
};

use which::which;

use crate::driver::Error;

pub fn get_sdk_path() -> Result<&'static Path, Error> {
    static SDK_PATH: OnceLock<PathBuf> = OnceLock::new();

    SDK_PATH
        .get_or_try_init(|| {
            let xcrun_path = which("xcrun")?;

            let output = std::process::Command::new(xcrun_path)
                .args(&["--sdk", "macosx", "--show-sdk-path"])
                .output()?;

            Ok(PathBuf::from(
                String::from_utf8(output.stdout).map_err(|_| Error::Unexpected)?,
            ))
        })
        .map(AsRef::as_ref)
}
