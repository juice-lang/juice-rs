mod error;
pub mod lexer;

use chumsky::{
    extra::Err as ExtraErr,
    input::{BoxedStream, Input as _, SpannedInput, Stream},
    primitive::{any, choice, group, just},
    recovery::{nested_delimiters, via_parser},
    recursive::recursive,
    select, Parser as ChumskyParser,
};
use juice_core::parser_ext::{IterParserExt as _, ParserExt as _};

pub use self::lexer::Lexer;
use self::{
    error::{Error, ParserError},
    lexer::{literal::InterpolationPart, Tok, TokenKind},
};
use crate::{
    ast::expr::{BorrowExpr, Expr, ExprKind, IntLiteralExpr, InterpolationExprPart, LiteralExpr, UnaryOperatorExpr},
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine, DiagnosticNote},
    source_loc::SourceRange,
    source_manager::{Source, SourceManager},
};

macro_rules! just {
    ($kind:pat_param $(if $guard:expr)?) => {
        chumsky::primitive::any().filter(|k| matches!(k, $kind $(if $guard)?))
    }
}

type TokenStream<'src, 'lex, M> = BoxedStream<'lex, (TokenKind<'src, M>, SourceRange<'src, M>)>;

type ParserInput<'src, 'lex, M> = SpannedInput<TokenKind<'src, M>, SourceRange<'src, M>, TokenStream<'src, 'lex, M>>;

#[rustfmt::skip]
trait ParserTrait<'src, 'lex, M: 'src + SourceManager, O> =
    ChumskyParser<
        'lex,
        ParserInput<'src, 'lex, M>,
        O,
        ExtraErr<ParserError<'src, M>>
    > + Clone
where 'src: 'lex;

pub struct Parser<'src, M: 'src + SourceManager> {
    source: Source<'src, M>,
    lexer: Lexer<'src, M>,
}

impl<'src, M: 'src + SourceManager> Parser<'src, M> {
    pub fn new(source: Source<'src, M>) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),
        }
    }

    pub fn parse_expr<C: DiagnosticConsumer<'src, M>>(
        &mut self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<Option<Expr<'src, M>>, C::Error> {
        let parser_input = Stream::from_iter((&mut self.lexer).map(|t| (t.kind, t.source_range)))
            .boxed()
            .spanned(self.source.get_eof_range());

        let (expr, errors) = Self::expr_parser()
            .padded_by(<NewlinesParser>::parser())
            .map_err_with_span(|_, mut range| {
                let start_loc = range.start_loc();

                if range.is_empty() {
                    range = start_loc.get_character_range().unwrap();
                }

                Error::new(
                    start_loc,
                    Diagnostic::expected_expression("at top level"),
                    vec![(range, DiagnosticContextNote::expected_expression_location())],
                    None,
                )
                .into()
            })
            .parse(parser_input)
            .into_output_errors();

        self.lexer.diagnose_errors(diagnostics)?;

        for error in errors {
            error.diagnose(diagnostics)?;
        }

        Ok(expr)
    }

    fn expr_parser<'lex>() -> impl ParserTrait<'src, 'lex, M, Expr<'src, M>>
    where
        'src: 'lex,
    {
        recursive(|expr| Self::binary_expr_parser(expr))
    }

    fn binary_expr_parser<'lex, P: ParserTrait<'src, 'lex, M, Expr<'src, M>>>(
        expr: P,
    ) -> impl ParserTrait<'src, 'lex, M, Expr<'src, M>>
    where
        'src: 'lex,
    {
        let unary_expr = Self::unary_expr_parser(expr);

        let first_expr = just!(Tok![BinOp(_)])
            .to_span()
            .or_not()
            .then(unary_expr.clone())
            .validate(|(op_range, expr), e, emitter| {
                if let Some(op_range) = op_range {
                    let error = Error::new(
                        op_range.start_loc(),
                        Diagnostic::unexpected_binary_operator(),
                        vec![
                            (op_range, DiagnosticContextNote::operator_location()),
                            (
                                expr.source_range,
                                DiagnosticContextNote::maybe_prefix_operand_location(),
                            ),
                        ],
                        Some(DiagnosticNote::expected_prefix_operator()),
                    );

                    emitter.emit(error.into());

                    UnaryOperatorExpr::new_invalid(expr, op_range, true).into_expr(e.span())
                } else {
                    expr
                }
            });

        first_expr.foldl_with_span(
            group((
                <NewlinesParser>::parser().ignore_then(just!(Tok![BinOp(_)]).to_span()),
                <NewlinesParser>::parser()
                    .ignore_then(unary_expr)
                    .map(Ok)
                    .or(<NewlinesParser>::parser()
                        .ignore_then(any())
                        .to_span()
                        .or_not()
                        .rewind()
                        .map(Err)),
            ))
            .validate(|(op_range, res), e, emitter| match res {
                Ok(expr) => (op_range, expr),
                Err(span) => {
                    let span = span
                        .or_else(|| e.span().end_loc().get_character_range())
                        .unwrap_or_else(|| e.span().source.get_last_range());

                    let error = Error::new(
                        span.start_loc(),
                        Diagnostic::expected_expression("after binary operator"),
                        vec![
                            (span, DiagnosticContextNote::expected_expression_location()),
                            (op_range, DiagnosticContextNote::operator_location()),
                        ],
                        None,
                    );

                    emitter.emit(error.into());

                    (op_range, Expr::new(ExprKind::Error, span))
                }
            })
            .repeated(),
            Expr::with_binary_operator,
        )
    }

    fn unary_expr_parser<'lex>(
        expr: impl ParserTrait<'src, 'lex, M, Expr<'src, M>>,
    ) -> impl ParserTrait<'src, 'lex, M, Expr<'src, M>>
    where
        'src: 'lex,
    {
        let postfix_expr = Self::primary_expr_parser(expr)
            .then(just!(Tok![PostfixOp(_)]).to_span().or_not())
            .map_with_span(|(expr, op_range), range| {
                if let Some(op_range) = op_range {
                    UnaryOperatorExpr::new(expr, op_range, false).into_expr(range)
                } else {
                    expr.kind.into_expr(range)
                }
            });

        let prefix_operator = choice((
            just(Tok![&]).to((|expr, range, _| BorrowExpr::new(expr, false).into_expr(range)) as fn(_, _, _) -> _),
            just(Tok![&w]).to((|expr, range, _| BorrowExpr::new(expr, true).into_expr(range)) as fn(_, _, _) -> _),
            just!(Tok![PrefixOp(_)]).to((|expr, range, op_range| {
                UnaryOperatorExpr::new(expr, op_range, true).into_expr(range)
            }) as fn(_, _, _) -> _),
        ))
        .with_span()
        .memoized();

        let inner_prefix_expr = prefix_operator
            .clone()
            .then(
                postfix_expr
                    .clone()
                    .map(Ok)
                    .or(any().to_span().or_not().rewind().map(Err)),
            )
            .validate(|((op_f, op_range), res), e, emitter| match res {
                Ok(expr) => op_f(expr, e.span(), op_range),
                Err(span) => {
                    let span = span
                        .or_else(|| e.span().end_loc().get_character_range())
                        .unwrap_or_else(|| e.span().source.get_last_range());

                    let error = Error::new(
                        span.start_loc(),
                        Diagnostic::expected_expression("after prefix operator"),
                        vec![
                            (span, DiagnosticContextNote::expected_expression_location()),
                            (op_range, DiagnosticContextNote::operator_location()),
                        ],
                        None,
                    );

                    emitter.emit(error.into());

                    op_f(Expr::new(ExprKind::Error, span), e.span(), op_range)
                }
            });

        let prefix_expr = prefix_operator
            .clone()
            .then_ignore(prefix_operator.rewind()) // Ensure that one prefix operator is left for inner_prefix_expr
            .repeated()
            .foldr_with_span(inner_prefix_expr, |(op_f, op_range), expr, range| {
                op_f(expr, range, op_range)
            });

        prefix_expr.or(postfix_expr)
    }

    fn primary_expr_parser<'lex>(
        expr: impl ParserTrait<'src, 'lex, M, Expr<'src, M>>,
    ) -> impl ParserTrait<'src, 'lex, M, Expr<'src, M>>
    where
        'src: 'lex,
    {
        Self::literal_parser(expr.clone())
            .or(just!(Tok![Ident(_)]).to_span().map(ExprKind::Identifier))
            .or(expr
                .padded_by(<NewlinesParser>::parser())
                .delimited_by(just(Tok![LeftParen]), just(Tok![RightParen]))
                .map(Box::new)
                .map(ExprKind::Grouping))
            .map_with_span(Expr::new)
            .recover_with(via_parser(nested_delimiters(
                Tok![LeftParen],
                Tok![RightParen],
                [
                    (Tok![LeftBracket], Tok![RightBracket]),
                    (Tok![LeftBrace], Tok![RightBrace]),
                ],
                |span| Expr::new(ExprKind::Error, span),
            )))
    }

    fn literal_parser<'lex>(
        expr: impl ParserTrait<'src, 'lex, M, Expr<'src, M>>,
    ) -> impl ParserTrait<'src, 'lex, M, ExprKind<'src, M>>
    where
        'src: 'lex,
    {
        let interpolation = select! {
            Tok![Interpolation(parts)] => parts,
        }
        .validate(move |parts, e, emitter| {
            let mut expr_parts = Vec::new();

            let span: SourceRange<M> = e.span();

            for part in parts.iter().cloned() {
                match part {
                    InterpolationPart::String(s) => {
                        expr_parts.push(InterpolationExprPart::String(s.clone()));
                    }
                    InterpolationPart::Interpolation(tokens, range) => {
                        let eoi_range = range.end_loc().get_empty_range();

                        let input = tokens
                            .iter()
                            .map(|t| (t.kind.clone(), t.source_range))
                            .collect::<Vec<_>>();

                        let parser_input = Stream::from_iter(input).boxed().spanned(eoi_range);

                        let (expr, errors) = expr
                            .clone()
                            .map_err_with_span(|_, mut range| {
                                let start_loc = range.start_loc();

                                if range.is_empty() {
                                    range = start_loc.get_character_range().unwrap();
                                }

                                Error::new(
                                    start_loc,
                                    Diagnostic::expected_expression("in string interpolation"),
                                    vec![(range, DiagnosticContextNote::expected_expression_location())],
                                    None,
                                )
                                .into()
                            })
                            .parse(parser_input)
                            .into_output_errors();

                        let interpolation_range = range.source.get_range(range.start - 2, range.end + 1);

                        for error in errors {
                            let error = error
                                .with_context_note(interpolation_range, DiagnosticContextNote::interpolation_location())
                                .with_context_note(span, DiagnosticContextNote::containing_literal_location());

                            emitter.emit(error);
                        }

                        let expr = expr.unwrap_or_else(|| Expr::new(ExprKind::Error, range));

                        expr_parts.push(InterpolationExprPart::Interpolation(expr));
                    }
                }
            }

            LiteralExpr::StringInterpolation(expr_parts.into())
        });

        select! {
            Tok![false] => LiteralExpr::Bool(false),
            Tok![true] => LiteralExpr::Bool(true),
            Tok![Int(v)] => LiteralExpr::Int(IntLiteralExpr::Int(v)),
            Tok![BigInt(v)] => LiteralExpr::Int(IntLiteralExpr::BigInt(v)),
            Tok![Float(v)] => LiteralExpr::Float(v),
            Tok![Char(c)] => LiteralExpr::Char(c),
            Tok![String(s)] => LiteralExpr::String(s),
            Tok![InvalidInt] => LiteralExpr::InvalidInt,
            Tok![InvalidFloat] => LiteralExpr::InvalidFloat,
            Tok![InvalidChar] => LiteralExpr::InvalidChar,
            Tok![InvalidString] => LiteralExpr::InvalidString,
        }
        .or(interpolation)
        .map(ExprKind::Literal)
    }
}

struct NewlinesParser<const MIN: usize = 0, const MAX: usize = { !0 }>;

impl<const MIN: usize, const MAX: usize> NewlinesParser<MIN, MAX> {
    fn parser<'src, 'lex, M: 'src + SourceManager>() -> impl ParserTrait<'src, 'lex, M, ()>
    where
        'src: 'lex,
    {
        just(Tok![Newline]).repeated().at_least(MIN).at_most(MAX).ignored()
    }
}
