cfg_if::cfg_if! {
    if #[cfg(target_os = "linux")] {
        mod linux;

        pub use self::linux::get_linker_path_and_args;
    } else if #[cfg(target_os = "macos")] {
        mod macos;

        pub use self::macos::get_linker_path_and_args;
    } else {
        use std::path::PathBuf;
        use std::ffi::OsString;

        use crate::{
            cli::OutputFilePath,
            driver::{main::task::ErasedTask, Result as DriverResult},
        };

        pub async fn get_linker_path_and_args(
            _inputs: &Vec<Box<dyn ErasedTask>>,
            _output_path: &OutputFilePath,
        ) -> DriverResult<(PathBuf, Vec<OsString>)> {
            compile_error!("Unsupported OS")
        }
    }
}
