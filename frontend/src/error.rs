use std::io;

use derive_more::From;

#[derive(Debug, From)]
pub enum Error {
    #[from]
    Io(io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
