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
    source_loc::SourceRange,
    source_manager::SourceManager,
};

type LexerStream<'src, 'lex, M> = BoxedStream<'lex, (TokenKind<'src, M>, SourceRange<'src, M>)>;

type ParserInput<'src, 'lex, M> = SpannedInput<TokenKind<'src, M>, SourceRange<'src, M>, LexerStream<'src, 'lex, M>>;

#[derive_where(Debug, Clone)]
pub struct Error<'src, M: 'src + SourceManager> {
    source_range: SourceRange<'src, M>,
    expected: Vec<Option<TokenKind<'src, M>>>,
    found: Option<TokenKind<'src, M>>,
}

impl<'src, 'lex, M: 'src + SourceManager> ChumskyError<'lex, ParserInput<'src, 'lex, M>> for Error<'src, M>
where
    'src: 'lex,
{
    fn expected_found<Iter: IntoIterator<Item = Option<MaybeRef<'lex, TokenKind<'src, M>>>>>(
        expected: Iter,
        found: Option<MaybeRef<'lex, TokenKind<'src, M>>>,
        span: SourceRange<'src, M>,
    ) -> Self {
        Self {
            source_range: span,
            expected: expected.into_iter().map(|e| e.as_deref().cloned()).collect(),
            found: found.as_deref().cloned(),
        }
    }

    fn merge(mut self, mut other: Self) -> Self {
        self.expected.append(&mut other.expected);
        self
    }
}

pub trait Parser<'src, 'lex, M: 'src + SourceManager, O>:
    ChumskyParser<'lex, ParserInput<'src, 'lex, M>, O, ExtraErr<Error<'src, M>>>
where
    'src: 'lex,
{
}

impl<'src, 'lex, M: 'src + SourceManager, O, P> Parser<'src, 'lex, M, O> for P
where
    P: ChumskyParser<'lex, ParserInput<'src, 'lex, M>, O, ExtraErr<Error<'src, M>>>,
    'src: 'lex,
{
}

fn ignore_newlines<'src, 'lex, M: 'src + SourceManager>() -> impl Parser<'src, 'lex, M, ()> + Clone
where
    'src: 'lex,
{
    just(Tok![Newline]).repeated().ignored()
}

pub fn expr_parser<'src, 'lex, M: 'src + SourceManager>() -> impl Parser<'src, 'lex, M, Expr<'src, M>> + Clone
where
    'src: 'lex,
{
    recursive(|expr| {
        let cloned_expr = expr.clone();

        let literal = select! {
            Tok![false] => LiteralExpr::Bool(false),
            Tok![true] => LiteralExpr::Bool(true),
            Tok![Int(v)] => LiteralExpr::Int(IntLiteralExpr::Int(v)),
            Tok![BigInt(v)] => LiteralExpr::Int(IntLiteralExpr::BigInt(v)),
            Tok![Float(v)] => LiteralExpr::Float(v),
            Tok![Char(c)] => LiteralExpr::Char(c),
            Tok![String(s)] => LiteralExpr::String(s),
        }
        .or(select! {
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
        }))
        .map(ExprKind::Literal);

        let primary_expr = literal
            .or(just(Tok![Ident]).to_span().map(ExprKind::Identifier))
            .or(expr
                .delimited_by(
                    just(Tok![LeftParen]).then(ignore_newlines()),
                    ignore_newlines().then(just(Tok![RightParen])),
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
            )));

        let postfix_expr = primary_expr
            .then(just(Tok![PostfixOp]).to_span().or_not())
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
            just(Tok![PrefixOp])
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
                just(Tok![BinOp])
                    .to_span()
                    .padded_by(ignore_newlines())
                    .then(prefix_expr)
                    .repeated(),
                Expr::with_binary_operator,
            )
            .boxed(); // needed if we want compilation to finish in a reasonable amount of time

        binary_expr
    })
    .then_ignore(just(Tok![Newline]).repeated())
}
