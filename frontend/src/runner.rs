use std::{
    fmt::{self, Debug, Formatter},
    io::Write,
    path::PathBuf,
};

use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticEngine},
    parser::Lexer,
    source_loc::SourceRange,
    source_manager::SourceManager,
    Result,
};

macro_rules! check_error {
    ($diagnostics:expr) => {
        if $diagnostics.had_error() {
            return Ok(false);
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Action {
    DumpParse,
    DumpAst,
    EmitIr,
    EmitObject,
}

pub struct Args {
    pub input_filepath: PathBuf,
    pub output_stream: Box<dyn Write>,
    pub action: Action,
}

impl Debug for Args {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("FrontendArgs")
            .field("input_filepath", &self.input_filepath)
            .field("output_stream", &"Box<dyn Write>")
            .field("action", &self.action)
            .finish()
    }
}

pub struct Runner {
    args: Args,
}

impl Runner {
    pub fn new(args: Args) -> Self {
        Self { args }
    }
}

impl Runner {
    pub fn run(self) -> bool {
        match self.run_impl() {
            Ok(true) => true,
            Ok(false) => false,
            Err(err) => {
                err.diagnose();
                false
            }
        }
    }

    fn run_impl(self) -> Result<bool> {
        let source_manager = SourceManager::new(self.args.input_filepath)?;

        let diagnostics = DiagnosticEngine::new(&source_manager);

        let lexer = Lexer::new(source_manager.get_main_source());

        match lexer.collect::<Result<Vec<_>, _>>() {
            Ok(tokens) => {
                for token in tokens {
                    println!(
                        "{:?} {:?} {} {}",
                        token.kind,
                        token.source_range.get_text(),
                        !token.leading_whitespace_range.is_empty(),
                        token.has_trailing_whitespace
                    );
                }
            }
            Err(err) => {
                err.diagnose(&diagnostics)?;
            }
        }

        check_error!(diagnostics);
        Ok(true)
    }
}
