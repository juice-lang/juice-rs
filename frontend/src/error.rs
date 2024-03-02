use std::io;

use derive_more::From;

#[derive(Debug, From)]
pub enum Error {
    #[from]
    Io(io::Error),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
