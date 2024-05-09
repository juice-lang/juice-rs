use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    sync::Arc,
};

use derive_where::derive_where;

use super::dump::Dump;
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive_where(Debug, Clone)]
pub struct BinaryOperatorSequenceExpr<'src, M: 'src + SourceManager> {
    pub first: Box<Expr<'src, M>>,
    pub rest: Vec<(SourceRange<'src, M>, Expr<'src, M>)>,
}

impl<'src, M: 'src + SourceManager> BinaryOperatorSequenceExpr<'src, M> {
    pub fn new(first: Box<Expr<'src, M>>) -> Self {
        Self {
            first,
            rest: Vec::new(),
        }
    }

    pub fn push(&mut self, op_range: SourceRange<'src, M>, expr: Expr<'src, M>) {
        self.rest.push((op_range, expr));
    }
}

impl<'src, M: SourceManager> BinaryOperatorSequenceExpr<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        let mut list = Vec::with_capacity(self.rest.len() * 2 + 1);

        list.push(Dump::new("ExprPart").with_field("expr", self.first.get_dump()));

        list.extend(self.rest.iter().flat_map(|(op_range, expr)| {
            [
                Dump::new("OperatorPart").with_field("operator", *op_range),
                Dump::new("ExprPart").with_field("expr", expr.get_dump()),
            ]
        }));

        Dump::new("BinaryOperatorSequenceExpr").with_field("parts", list)
    }
}

#[derive_where(Debug, Clone)]
pub struct BinaryOperatorExpr<'src, M: 'src + SourceManager> {
    pub lhs: Box<Expr<'src, M>>,
    pub op_range: SourceRange<'src, M>,
    pub rhs: Box<Expr<'src, M>>,
}

impl<'src, M: 'src + SourceManager> BinaryOperatorExpr<'src, M> {
    pub fn new(lhs: Expr<'src, M>, op_range: SourceRange<'src, M>, rhs: Expr<'src, M>) -> Self {
        Self {
            lhs: Box::new(lhs),
            op_range,
            rhs: Box::new(rhs),
        }
    }
}

impl<'src, M: SourceManager> BinaryOperatorExpr<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        Dump::new("BinaryOperatorExpr")
            .with_field("operator", self.op_range)
            .with_field("lhs", self.lhs.get_dump())
            .with_field("rhs", self.rhs.get_dump())
    }
}

#[derive_where(Debug, Clone)]
pub struct UnaryOperatorExpr<'src, M: 'src + SourceManager> {
    pub operand: Box<Expr<'src, M>>,
    pub op_range: SourceRange<'src, M>,
    pub is_prefix: bool,
}

impl<'src, M: 'src + SourceManager> UnaryOperatorExpr<'src, M> {
    pub fn new(operand: Expr<'src, M>, op_range: SourceRange<'src, M>, is_prefix: bool) -> Self {
        Self {
            operand: Box::new(operand),
            op_range,
            is_prefix,
        }
    }
}

impl<'src, M: SourceManager> UnaryOperatorExpr<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        let name = if self.is_prefix {
            "PrefixOperatorExpr"
        } else {
            "PostfixOperatorExpr"
        };

        Dump::new(name)
            .with_field("operator", self.op_range)
            .with_field("operand", self.operand.get_dump())
    }
}

#[derive_where(Debug, Clone)]
pub struct BorrowExpr<'src, M: 'src + SourceManager> {
    pub expr: Box<Expr<'src, M>>,
    pub is_mutable: bool,
}

impl<'src, M: 'src + SourceManager> BorrowExpr<'src, M> {
    pub fn new(expr: Expr<'src, M>, is_mutable: bool) -> Self {
        Self {
            expr: Box::new(expr),
            is_mutable,
        }
    }
}

impl<'src, M: SourceManager> BorrowExpr<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        Dump::new("BorrowExpr")
            .with_field("is_mutable", self.is_mutable)
            .with_field("expr", self.expr.get_dump())
    }
}

#[derive(Debug, Clone)]
pub enum IntLiteralExpr {
    Int(u64),
    BigInt(Arc<[u64]>),
}

#[derive_where(Debug, Clone)]
pub enum InterpolationExprPart<'src, M: 'src + SourceManager> {
    String(Arc<str>),
    Interpolation(Expr<'src, M>),
}

#[derive_where(Debug, Clone)]
pub enum LiteralExpr<'src, M: 'src + SourceManager> {
    Bool(bool),
    Int(IntLiteralExpr),
    Float(f64),
    Char(char),
    String(Arc<str>),
    StringInterpolation(Arc<[InterpolationExprPart<'src, M>]>),
}

impl<'src, M: SourceManager> LiteralExpr<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        match self {
            Self::Bool(v) => Dump::new("BoolExpr").with_field("value", *v),
            Self::Int(IntLiteralExpr::Int(v)) => Dump::new("IntExpr").with_field("value", *v),
            Self::Int(IntLiteralExpr::BigInt(v)) => Dump::new("IntExpr").with_field("value", v.clone()),
            Self::Float(v) => Dump::new("FloatExpr").with_field("value", *v),
            Self::Char(c) => Dump::new("CharExpr").with_field("value", *c),
            Self::String(s) => Dump::new("StringExpr").with_field("value", s.clone()),
            Self::StringInterpolation(parts) => {
                let mut list = Vec::new();
                for part in parts.as_ref() {
                    let dump = match part {
                        InterpolationExprPart::String(s) => Dump::new("StringPart").with_field("value", s.clone()),
                        InterpolationExprPart::Interpolation(expr) => {
                            Dump::new("InterpolationPart").with_field("expr", expr.get_dump())
                        }
                    };

                    list.push(dump);
                }

                Dump::new("StringInterpolationExpr").with_field("parts", list)
            }
        }
    }
}

#[derive_where(Debug, Clone)]
pub enum ExprKind<'src, M: 'src + SourceManager> {
    BinaryOperatorSequence(BinaryOperatorSequenceExpr<'src, M>),
    BinaryOperator(BinaryOperatorExpr<'src, M>),
    UnaryOperator(UnaryOperatorExpr<'src, M>),
    Borrow(BorrowExpr<'src, M>),
    Literal(LiteralExpr<'src, M>),
    Identifier(SourceRange<'src, M>),
    Grouping(Box<Expr<'src, M>>),
    Error,
}

impl<'src, M: SourceManager> ExprKind<'src, M> {
    fn get_dump(&self) -> Dump<'src> {
        match self {
            ExprKind::BinaryOperatorSequence(expr) => expr.get_dump(),
            ExprKind::BinaryOperator(expr) => expr.get_dump(),
            ExprKind::UnaryOperator(expr) => expr.get_dump(),
            ExprKind::Borrow(expr) => expr.get_dump(),
            ExprKind::Literal(expr) => expr.get_dump(),
            ExprKind::Identifier(range) => Dump::new("IdentifierExpr").with_field("ident", range.get_str()),
            ExprKind::Grouping(expr) => expr.get_dump(),
            ExprKind::Error => Dump::new_error("ErrorExpr"),
        }
    }
}

#[derive_where(Debug, Clone)]
pub struct Expr<'src, M: 'src + SourceManager> {
    pub kind: ExprKind<'src, M>,
    #[allow(dead_code)]
    pub source_range: SourceRange<'src, M>,
}

impl<'src, M: 'src + SourceManager> Expr<'src, M> {
    pub fn new(kind: ExprKind<'src, M>, source_range: SourceRange<'src, M>) -> Self {
        Self { kind, source_range }
    }

    pub fn with_binary_operator(
        self,
        (op_range, rhs): (SourceRange<'src, M>, Expr<'src, M>),
        source_range: SourceRange<'src, M>,
    ) -> Self {
        let kind = match self.kind {
            ExprKind::BinaryOperatorSequence(mut seq) => {
                seq.push(op_range, rhs);
                ExprKind::BinaryOperatorSequence(seq)
            }
            ExprKind::BinaryOperator(lhs) => {
                let mut seq = BinaryOperatorSequenceExpr::new(lhs.lhs);
                seq.push(lhs.op_range, *lhs.rhs);
                seq.push(op_range, rhs);
                ExprKind::BinaryOperatorSequence(seq)
            }
            _ => ExprKind::BinaryOperator(BinaryOperatorExpr::new(self, op_range, rhs)),
        };

        Self::new(kind, source_range)
    }

    fn get_dump(&self) -> Dump<'src> {
        self.kind.get_dump()
    }
}

impl<M: SourceManager> Display for Expr<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.get_dump())
    }
}
