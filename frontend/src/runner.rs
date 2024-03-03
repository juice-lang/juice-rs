use std::{
    fmt::{self, Debug, Formatter},
    io::Write,
    path::PathBuf,
};

use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticEngine},
    source_loc::SourceRange,
    source_manager::SourceManager,
    Result,
};

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

        let main_source = source_manager.get_main_source();

        let borrow_loc = SourceRange::new(main_source, 66, 70);
        let def_loc = SourceRange::new(main_source, 0, 5);

        diagnostics
            .report(borrow_loc.start_loc(), Diagnostic::cannot_borrow_let_mutable("a"))
            .with_context_note(borrow_loc, DiagnosticContextNote::mutable_borrow_here())
            .with_context_note(def_loc, DiagnosticContextNote::variable_defined_here("a"))
            .diagnose()?;

        Ok(!diagnostics.had_error())
    }
}
