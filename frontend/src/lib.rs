use std::{
    fmt::{self, Debug, Formatter},
    io::Write,
    path::PathBuf,
};

#[derive(Debug, PartialEq, Eq)]
pub enum FrontendAction {
    DumpParse,
    DumpAst,
    EmitIr,
    EmitObject,
}

pub struct FrontendArgs {
    pub input_filepath: PathBuf,
    pub output_stream: Box<dyn Write>,
    pub action: FrontendAction,
}

impl Debug for FrontendArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("FrontendArgs")
            .field("input_filepath", &self.input_filepath)
            .field("output_stream", &"Box<dyn Write>")
            .field("action", &self.action)
            .finish()
    }
}
