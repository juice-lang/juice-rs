use std::{convert::Infallible, marker::PhantomData};

use chumsky::{primitive::choice, recursive::Recursive};

use super::{decl::DeclParser, expr::ExprParser, ParserCache, ParserTrait, RecursiveParser};
use crate::{ast::stmt::Stmt, source_manager::SourceManager};

// Poor-man's substitute for a generic module
pub struct StmtParser<'src, 'lex, M: 'src + SourceManager>(Infallible, PhantomData<&'lex &'src M>)
where
    'src: 'lex;

impl<'src, 'lex, M: 'src + SourceManager> StmtParser<'src, 'lex, M> {
    pub fn parser(cache: &ParserCache<'src, 'lex, M>) -> RecursiveParser<'src, 'lex, M, Stmt<'src, M>> {
        match cache.stmt_parser.get_or_init(Recursive::declare) {
            Ok(parser) => parser.clone(),
            Err(parser) => {
                let mut parser = parser.clone();
                parser.define(Self::parser_impl(cache));

                parser
            }
        }
    }

    fn parser_impl(cache: &ParserCache<'src, 'lex, M>) -> impl ParserTrait<'src, 'lex, M, Stmt<'src, M>> {
        choice((
            DeclParser::parser(cache).map(Into::into),
            ExprParser::parser(cache).map(Into::into),
        ))
    }
}
