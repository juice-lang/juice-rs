use std::{convert::Infallible, marker::PhantomData};

use chumsky::{
    primitive::{any, choice, just, one_of},
    recursive::Recursive,
    IterParser as _,
};
use juice_core::parser_ext::ParserExt as _;

use super::{
    decl::DeclParser, expr::ExprParser, lexer::Tok, Error, NewlinesParser, ParserCache, ParserTrait, RecursiveParser,
};
use crate::{
    ast::stmt::{Stmt, StmtList},
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    source_loc::SourceRange,
    source_manager::SourceManager,
};

// Poor-man's substitute for a generic module
pub struct StmtListParser<'src, 'lex, M: 'src + SourceManager>(Infallible, PhantomData<&'lex &'src M>)
where
    'src: 'lex;

impl<'src, 'lex, M: 'src + SourceManager> StmtListParser<'src, 'lex, M> {
    pub fn parser(cache: &ParserCache<'src, 'lex, M>) -> RecursiveParser<'src, 'lex, M, StmtList<'src, M>> {
        match cache.stmt_list_parser.get_or_init(Recursive::declare) {
            Ok(parser) => parser.clone(),
            Err(parser) => {
                let mut parser = parser.clone();
                parser.define(Self::parser_impl(cache));

                parser
            }
        }
    }

    pub fn parser_impl(cache: &ParserCache<'src, 'lex, M>) -> impl ParserTrait<'src, 'lex, M, StmtList<'src, M>> {
        Self::stmt_parser(cache)
            .separated_by(
                one_of([Tok![;], Tok![Newline]])
                    .repeated()
                    .at_least(1)
                    .map(|_| ())
                    .ok_or_rewind(any().to_span())
                    .validate(|res, _e, emitter| {
                        if let Err(Some(span)) = res {
                            let span: SourceRange<'src, M> = span;
                            let error = Error::new(
                                span.start_loc(),
                                Diagnostic::unseparated_statements(span.get_str()),
                                vec![(span, DiagnosticContextNote::expected_statement_separator_location())],
                                Some(DiagnosticNote::unseparated_statements()),
                            );

                            emitter.emit(error.into());
                        }
                    }),
            )
            .at_least(1)
            .collect()
            .then(<NewlinesParser>::parser().ignore_then(just(Tok![;]).or_not().map(|x| x.is_some())))
            .padded_by(one_of([Tok![;], Tok![Newline]]).repeated())
            .map(|(stmts, trailing_semicolon)| StmtList::new(stmts, trailing_semicolon))
    }

    fn stmt_parser(cache: &ParserCache<'src, 'lex, M>) -> impl ParserTrait<'src, 'lex, M, Stmt<'src, M>> {
        choice((
            DeclParser::parser(cache).map(Into::into),
            ExprParser::parser(cache).map(Into::into),
        ))
    }
}
