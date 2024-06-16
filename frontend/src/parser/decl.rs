use std::{convert::Infallible, marker::PhantomData};

use chumsky::{
    primitive::{any, choice, group, just},
    recursive::Recursive,
    Parser,
};
use juice_core::parser_ext::ParserExt as _;

use super::{
    error::Error, expr::ExprParser, just, lexer::Tok, NewlinesParser, ParserCache, ParserTrait, RecursiveParser,
};
use crate::{
    ast::decl::{Decl, DeclKind, VarDecl},
    diag::{Diagnostic, DiagnosticContextNote},
    source_manager::SourceManager,
};

// Poor-man's substitute for a generic module
pub struct DeclParser<'src, 'lex, M: 'src + SourceManager>(Infallible, PhantomData<&'lex &'src M>)
where
    'src: 'lex;

impl<'src, 'lex, M: 'src + SourceManager> DeclParser<'src, 'lex, M> {
    pub fn parser(cache: &ParserCache<'src, 'lex, M>) -> RecursiveParser<'src, 'lex, M, Decl<'src, M>> {
        match cache.decl_parser.get_or_init(Recursive::declare) {
            Ok(parser) => parser.clone(),
            Err(parser) => {
                let mut parser = parser.clone();
                parser.define(Self::parser_impl(cache));

                parser
            }
        }
    }

    fn parser_impl(cache: &ParserCache<'src, 'lex, M>) -> impl ParserTrait<'src, 'lex, M, Decl<'src, M>> {
        Self::var_decl_parser(cache)
    }

    fn var_decl_parser(cache: &ParserCache<'src, 'lex, M>) -> impl ParserTrait<'src, 'lex, M, Decl<'src, M>> {
        group((
            choice((just(Tok![let]).to(false), just(Tok![var]).to(true))).with_span(),
            just!(Tok![Ident(_)])
                .padded_by(<NewlinesParser>::parser())
                .to_span()
                .or_not(),
            just(Tok![=])
                .to_span()
                .then(
                    <NewlinesParser>::parser()
                        .ignore_then(ExprParser::parser(cache))
                        .ok_or_rewind(<NewlinesParser>::parser().ignore_then(any()).to_span()),
                )
                .or_not(),
        ))
        .validate(|((is_mutable, keyword_range), name_range, initializer), e, emitter| {
            let Some(name_range) = name_range else {
                let span = initializer
                    .as_ref()
                    .map(|(equals_span, _)| *equals_span)
                    .or_else(|| e.span().end_loc().get_character_range())
                    .unwrap_or_else(|| e.span().source.get_last_range());

                let error = Error::new(
                    span.start_loc(),
                    Diagnostic::expected_var_decl_name(),
                    vec![
                        (span, DiagnosticContextNote::expected_identifier_location()),
                        (e.span(), DiagnosticContextNote::containing_var_decl_location()),
                    ],
                    None,
                );

                emitter.emit(error.into());

                return DeclKind::Error.into_decl(e.span());
            };

            let initializer = initializer.map(|(equals_span, expr)| {
                expr.map(Some).unwrap_or_else(|span| {
                    let span = span
                        .or_else(|| e.span().end_loc().get_character_range())
                        .unwrap_or_else(|| e.span().source.get_last_range());

                    let error = Error::new(
                        span.start_loc(),
                        Diagnostic::expected_expression("in variable initialization"),
                        vec![
                            (span, DiagnosticContextNote::expected_expression_location()),
                            (equals_span, DiagnosticContextNote::var_decl_equals_location()),
                            (e.span(), DiagnosticContextNote::containing_var_decl_location()),
                        ],
                        None,
                    );

                    emitter.emit(error.into());

                    None
                })
            });

            let is_invalid = initializer.as_ref().is_some_and(Option::is_none);

            VarDecl::new(is_mutable, keyword_range, name_range, initializer.flatten(), is_invalid).into_decl(e.span())
        })
    }
}
