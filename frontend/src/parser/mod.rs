pub mod lexer;

use chumsky::{
    error::Error as ChumskyError,
    extra::Err as ExtraErr,
    input::{BoxedStream, Input as _, SpannedInput, Stream},
    primitive::{choice, just},
    recovery::{nested_delimiters, via_parser},
    recursive::recursive,
    select,
    util::MaybeRef,
    Parser as ChumskyParser,
};
use derive_where::derive_where;
use juice_core::parser_ext::{IterParserExt as _, ParserExt as _};

pub use self::lexer::Lexer;
use self::lexer::{literal::InterpolationPart, Tok, TokenKind};
use crate::{
    ast::expr::{BorrowExpr, Expr, ExprKind, IntLiteralExpr, InterpolationExprPart, LiteralExpr, UnaryOperatorExpr},
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine, DiagnosticNote},
    source_loc::{SourceLoc, SourceRange},
    source_manager::{Source, SourceManager},
};

macro_rules! just {
    ($kind:pat_param $(if $guard:expr)?) => {
        chumsky::primitive::any().filter(|k| matches!(k, $kind $(if $guard)?))
    }
}

type LexerStream<'src, 'lex, M> = BoxedStream<'lex, (TokenKind<'src, M>, SourceRange<'src, M>)>;

type ParserInput<'src, 'lex, M> = SpannedInput<TokenKind<'src, M>, SourceRange<'src, M>, LexerStream<'src, 'lex, M>>;

#[derive_where(Debug, Clone)]
pub struct Error<'src, M: 'src + SourceManager> {
    source_loc: SourceLoc<'src, M>,
    diagnostic: Diagnostic<'src>,
    context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
    note: Option<DiagnosticNote<'src>>,
}

impl<'src, M: 'src + SourceManager> Error<'src, M> {
    pub fn new(
        source_loc: SourceLoc<'src, M>,
        diagnostic: Diagnostic<'src>,
        context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
        note: Option<DiagnosticNote<'src>>,
    ) -> Self {
        Self {
            source_loc,
            diagnostic,
            context_notes,
            note,
        }
    }

    pub fn diagnose<C: DiagnosticConsumer<'src, M>>(
        self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<(), C::Error> {
        let mut report = diagnostics.report(self.source_loc, self.diagnostic);

        for (source_range, context_note) in self.context_notes {
            report = report.with_context_note(source_range, context_note);
        }

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report.diagnose()
    }
}

#[derive_where(Debug, Clone)]
pub enum ParserError<'src, M: 'src + SourceManager> {
    ExpectedFound {
        source_range: SourceRange<'src, M>,
        expected: Vec<Option<TokenKind<'src, M>>>,
        found: Option<TokenKind<'src, M>>,
    },
    Other(Error<'src, M>),
}

impl<'src, M: 'src + SourceManager> ParserError<'src, M> {
    pub fn diagnose<C: DiagnosticConsumer<'src, M>>(
        self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<(), C::Error> {
        match self {
            Self::ExpectedFound {
                source_range,
                expected,
                found,
            } => {
                let error = Error::new(
                    source_range.start_loc(),
                    Diagnostic::unexpected_parser_error(),
                    vec![(source_range, DiagnosticContextNote::unexpected_parser_error_location())],
                    Some(DiagnosticNote::unexpected_parser_error(expected, found)),
                );

                error.diagnose(diagnostics)
            }
            Self::Other(err) => err.diagnose(diagnostics),
        }
    }
}

impl<'src, M: 'src + SourceManager> From<Error<'src, M>> for ParserError<'src, M> {
    fn from(err: Error<'src, M>) -> Self {
        Self::Other(err)
    }
}

impl<'src, 'lex, M: 'src + SourceManager> ChumskyError<'lex, ParserInput<'src, 'lex, M>> for ParserError<'src, M>
where
    'src: 'lex,
{
    fn expected_found<Iter: IntoIterator<Item = Option<MaybeRef<'lex, TokenKind<'src, M>>>>>(
        expected: Iter,
        found: Option<MaybeRef<'lex, TokenKind<'src, M>>>,
        span: SourceRange<'src, M>,
    ) -> Self {
        Self::ExpectedFound {
            source_range: span,
            expected: expected.into_iter().map(|e| e.as_deref().cloned()).collect(),
            found: found.as_deref().cloned(),
        }
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (
            Self::ExpectedFound { expected, .. },
            Self::ExpectedFound {
                expected: other_expected,
                ..
            },
        ) = (&mut self, &mut other)
        {
            expected.append(other_expected);
        }

        self
    }
}

trait ParserTrait<'src, 'lex, M: 'src + SourceManager, O> =
    ChumskyParser<'lex, ParserInput<'src, 'lex, M>, O, ExtraErr<ParserError<'src, M>>> + Clone where 'src: 'lex;

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

        let (expr, errors) = Self::expr_parser().parse(parser_input).into_output_errors();

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
        recursive(|expr| {
            let cloned_expr = expr.clone();

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
                        InterpolationPart::Interpolation(tokens) => {
                            let start_loc = tokens
                                .first()
                                .map(|t| t.source_range.start_loc())
                                .unwrap_or(span.start_loc());

                            let eoi_range = if let Some(t) = tokens.last() {
                                let end = t.source_range.end;
                                t.source_range.source.get_range(end, end)
                            } else {
                                let end = span.end;
                                span.source.get_range(end, end)
                            };

                            let inner_span = start_loc.source.get_range(start_loc.offset, eoi_range.end);

                            let input = tokens
                                .iter()
                                .cloned()
                                .map(|t| (t.kind, t.source_range))
                                .collect::<Vec<_>>();

                            let parser_input = Stream::from_iter(input).boxed().spanned(eoi_range);

                            let (expr, errors) = cloned_expr.parse(parser_input).into_output_errors();

                            for error in errors {
                                emitter.emit(error);
                            }

                            let expr = expr.unwrap_or_else(|| Expr::new(ExprKind::Error, inner_span));

                            expr_parts.push(InterpolationExprPart::Interpolation(expr));
                        }
                    }
                }

                LiteralExpr::StringInterpolation(expr_parts.into())
            });

            let literal = select! {
                Tok![false] => LiteralExpr::Bool(false),
                Tok![true] => LiteralExpr::Bool(true),
                Tok![Int(v)] => LiteralExpr::Int(IntLiteralExpr::Int(v)),
                Tok![BigInt(v)] => LiteralExpr::Int(IntLiteralExpr::BigInt(v)),
                Tok![Float(v)] => LiteralExpr::Float(v),
                Tok![Char(c)] => LiteralExpr::Char(c),
                Tok![String(s)] => LiteralExpr::String(s),
            }
            .or(interpolation)
            .map(ExprKind::Literal);

            let primary_expr = literal
                .or(just!(Tok![Ident(_)]).to_span().map(ExprKind::Identifier))
                .or(expr
                    .delimited_by(
                        just(Tok![LeftParen]).then(Self::newlines_parser()),
                        Self::newlines_parser().then(just(Tok![RightParen])),
                    )
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
                .boxed();

            let postfix_expr = primary_expr
                .then(just!(Tok![PostfixOp(_)]).to_span().or_not())
                .map(|(expr, op_range)| {
                    if let Some(op_range) = op_range {
                        ExprKind::UnaryOperator(UnaryOperatorExpr::new(expr, op_range, false))
                    } else {
                        expr.kind
                    }
                })
                .map_with_span(Expr::new);

            let prefix_operator = choice((
                just(Tok![&]).to((|e, _| ExprKind::Borrow(BorrowExpr::new(e, false))) as fn(_, _) -> _),
                just(Tok![&w]).to((|e, _| ExprKind::Borrow(BorrowExpr::new(e, true))) as fn(_, _) -> _),
                just!(Tok![PrefixOp(_)])
                    .to((|e, span| ExprKind::UnaryOperator(UnaryOperatorExpr::new(e, span, true))) as fn(_, _) -> _),
            ));

            let prefix_expr = prefix_operator
                .with_span()
                .repeated()
                .foldr_with_span(postfix_expr, |(op_f, op_span), e, span| {
                    Expr::new(op_f(e, op_span), span)
                });

            let binary_expr = prefix_expr
                .clone()
                .foldl_with_span(
                    just!(Tok![BinOp(_)])
                        .to_span()
                        .padded_by(Self::newlines_parser())
                        .then(prefix_expr)
                        .repeated(),
                    Expr::with_binary_operator,
                )
                .boxed(); // needed if we want compilation to finish in a reasonable amount of time

            binary_expr
        })
        .then_ignore(just(Tok![Newline]).repeated())
    }

    fn newlines_parser<'lex>() -> impl ParserTrait<'src, 'lex, M, ()>
    where
        'src: 'lex,
    {
        just(Tok![Newline]).repeated().ignored()
    }
}
