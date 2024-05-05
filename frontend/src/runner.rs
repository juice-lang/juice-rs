use std::{
    fmt::{self, Debug, Formatter},
    io::Write,
    path::PathBuf,
};

use chumsky::{
    input::{Input as _, Stream},
    Parser as _,
};

use crate::{
    diag::DiagnosticEngine,
    parser::{expr_parser, Lexer},
    source_manager::{DefaultSourceManager, SourceManager},
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

    fn run_impl(mut self) -> Result<bool> {
        let source_manager = DefaultSourceManager::new(self.args.input_filepath)?;

        let diagnostics = DiagnosticEngine::new(&source_manager);

        let source = source_manager.get_main_source();

        let mut lexer = Lexer::new(source);

        let parser_input = Stream::from_iter((&mut lexer).map(|t| (t.kind, t.source_range)))
            .boxed()
            .spanned(source.get_eof_range());

        let (ast, errors) = expr_parser().parse(parser_input).into_output_errors();

        lexer.diagnose_errors(&diagnostics)?;

        println!("{:?}", errors);

        if let Some(ast) = ast {
            if self.args.action == Action::DumpParse {
                writeln!(self.args.output_stream, "{}", ast)?;
                return Ok(true);
            }
        }

        check_error!(diagnostics);
        Ok(true)
    }
}
