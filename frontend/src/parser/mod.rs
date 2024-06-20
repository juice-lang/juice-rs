mod decl;
mod error;
mod expr;
pub mod lexer;
mod stmt;

use std::sync::OnceLock;

use chumsky::{
    extra::Err as ExtraErr,
    input::{BoxedStream, Input as _, SpannedInput, Stream},
    recursive::{Indirect, Recursive},
    Parser as ChumskyParser,
};
use derive_where::derive_where;
use juice_core::parser_ext::ParserExt as _;
use stmt::StmtListParser;

pub use self::lexer::Lexer;
use self::{
    error::{Error, ParserError},
    lexer::{Tok, TokenKind},
};
use crate::{
    ast::{decl::Decl, expr::Expr, stmt::StmtList},
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine},
    source_loc::SourceRange,
    source_manager::{Source, SourceManager},
};

macro_rules! just {
    ($kind:pat_param $(if $guard:expr)?) => {
        chumsky::primitive::any().filter(|k| matches!(k, $kind $(if $guard)?))
    }
}

#[allow(clippy::useless_attribute)] // false positive: https://github.com/rust-lang/rust-clippy/issues/12808
#[allow(clippy::needless_pub_self)]
pub(self) use just;

type TokenStream<'src, 'lex, M> = BoxedStream<'lex, (TokenKind<'src, M>, SourceRange<'src, M>)>;

type ParserInput<'src, 'lex, M> = SpannedInput<TokenKind<'src, M>, SourceRange<'src, M>, TokenStream<'src, 'lex, M>>;

trait ParserTrait<'src, 'lex, M: 'src + SourceManager, O> =
    ChumskyParser<'lex, ParserInput<'src, 'lex, M>, O, ExtraErr<ParserError<'src, M>>> + Clone + Send + Sync
    where 'src: 'lex;

type RecursiveParser<'src, 'lex, M, O> =
    Recursive<Indirect<'lex, 'lex, ParserInput<'src, 'lex, M>, O, ExtraErr<ParserError<'src, M>>>>;

#[derive_where(Clone, Default)]
struct OnceRecursiveParser<'src, 'lex, M: 'src + SourceManager, O>(OnceLock<RecursiveParser<'src, 'lex, M, O>>)
where
    'src: 'lex;

impl<'src, 'lex, M: 'src + SourceManager, O> OnceRecursiveParser<'src, 'lex, M, O>
where
    'src: 'lex,
{
    /// Get the parser if it has been initialized, otherwise initialize it with the given function.
    ///
    /// Returns `Ok(parser)` if the parser has already been initialized, otherwise returns `Err(parser)`.
    fn get_or_init(
        &self,
        f: impl FnOnce() -> RecursiveParser<'src, 'lex, M, O>,
    ) -> Result<&RecursiveParser<'src, 'lex, M, O>, &RecursiveParser<'src, 'lex, M, O>> {
        if let Some(parser) = self.0.get() {
            Ok(parser)
        } else {
            match self.0.try_insert(f()) {
                Ok(parser) => Err(parser),
                Err((parser, _)) => Ok(parser),
            }
        }
    }
}

#[derive_where(Default)]
struct ParserCache<'src, 'lex, M: 'src + SourceManager>
where
    'src: 'lex,
{
    decl_parser: OnceRecursiveParser<'src, 'lex, M, Decl<'src, M>>,
    stmt_list_parser: OnceRecursiveParser<'src, 'lex, M, StmtList<'src, M>>,
    expr_parser: OnceRecursiveParser<'src, 'lex, M, Expr<'src, M>>,
}

pub struct Parser<'src, M: 'src + SourceManager> {
    source: Source<'src, M>,
    lexer: Lexer<'src, M>,
}

impl<'src, M: 'src + SourceManager> Parser<'src, M> {
    pub fn new(source: Source<'src, M>) -> Self {
        let parser = Self {
            source,
            lexer: Lexer::new(source),
        };

        parser
    }

    pub fn parse_stmt_list<C: DiagnosticConsumer<'src, M>>(
        &mut self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<Option<StmtList<'src, M>>, C::Error> {
        let parser_input = Stream::from_iter((&mut self.lexer).map(|t| (t.kind, t.source_range)))
            .boxed()
            .spanned(self.source.get_eof_range());

        let (stmt, errors) = {
            StmtListParser::parser(&ParserCache::default())
                .padded_by(<NewlinesParser>::parser())
                .map_err_with_span(|_, mut range| {
                    let start_loc = range.start_loc();

                    if range.is_empty() {
                        range = start_loc.get_character_range().unwrap();
                    }

                    Error::new(
                        start_loc,
                        Diagnostic::expected_statement("at top level"),
                        vec![(range, DiagnosticContextNote::expected_statement_location())],
                        None,
                    )
                    .into()
                })
                .parse(parser_input)
                .into_output_errors()
        };

        self.lexer.diagnose_errors(diagnostics)?;

        for error in errors {
            error.diagnose(diagnostics)?;
        }

        Ok(stmt)
    }
}

struct NewlinesParser<const MIN: usize = 0, const MAX: usize = { !0 }>;

impl<const MIN: usize, const MAX: usize> NewlinesParser<MIN, MAX> {
    fn parser<'src, 'lex, M: 'src + SourceManager>() -> impl ParserTrait<'src, 'lex, M, ()>
    where
        'src: 'lex,
    {
        chumsky::primitive::just(Tok![Newline])
            .repeated()
            .at_least(MIN)
            .at_most(MAX)
            .ignored()
    }
}
