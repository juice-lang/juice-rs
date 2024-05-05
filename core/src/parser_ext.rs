use chumsky::{
    combinator::{FoldlWith, FoldrWith, MapWith},
    extra::ParserExtra,
    input::{Input, MapExtra},
    IterParser, Parser,
};

pub trait ParserExt<'a, I: Input<'a>, O, E: ParserExtra<'a, I>>: Parser<'a, I, O, E> + Sized {
    fn foldl_with_span<B: IterParser<'a, I, OB, E>, OB>(
        self,
        other: B,
        f: impl Fn(O, OB, I::Span) -> O + Clone,
    ) -> FoldlWith<impl Fn(O, OB, &mut MapExtra<'a, '_, I, E>) -> O + Clone, Self, B, OB, E> {
        self.foldl_with(other, move |o, ob, extra| f(o, ob, extra.span()))
    }

    fn map_with_span<U>(
        self,
        f: impl Fn(O, I::Span) -> U + Clone,
    ) -> MapWith<Self, O, impl Fn(O, &mut MapExtra<'a, '_, I, E>) -> U + Clone> {
        self.map_with(move |o, extra| f(o, extra.span()))
    }

    fn with_span(self) -> MapWith<Self, O, impl Fn(O, &mut MapExtra<'a, '_, I, E>) -> (O, I::Span) + Clone> {
        self.map_with(move |o, extra| (o, extra.span()))
    }
}

impl<'a, I: Input<'a>, O, E: ParserExtra<'a, I>, P: Parser<'a, I, O, E>> ParserExt<'a, I, O, E> for P {}

pub trait IterParserExt<'a, I: Input<'a>, O, E: ParserExtra<'a, I>>: IterParser<'a, I, O, E> + Sized {
    fn foldr_with_span<B: Parser<'a, I, OA, E>, OA>(
        self,
        other: B,
        f: impl Fn(O, OA, I::Span) -> OA + Clone,
    ) -> FoldrWith<impl Fn(O, OA, &mut MapExtra<'a, '_, I, E>) -> OA + Clone, Self, B, O, E> {
        self.foldr_with(other, move |o, oa, extra| f(o, oa, extra.span()))
    }
}

impl<'a, I: Input<'a>, O, E: ParserExtra<'a, I>, P: IterParser<'a, I, O, E>> IterParserExt<'a, I, O, E> for P {}
