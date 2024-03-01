use std::{
    fs::File,
    io::{self, Result, Write},
    path::PathBuf,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OutputFilePath {
    Stdout,
    File(PathBuf),
}

impl OutputFilePath {
    pub fn try_into_stream(self) -> Result<Box<dyn Write>> {
        match self {
            Self::Stdout => Ok(Box::new(io::stdout())),
            Self::File(path) => Ok(Box::new(File::create(path)?)),
        }
    }

    pub fn to_str(&self) -> Option<&str> {
        match self {
            Self::Stdout => Some("-"),
            Self::File(path) => path.to_str(),
        }
    }
}
