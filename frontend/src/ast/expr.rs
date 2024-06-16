use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    sync::Arc,
};

use derive_where::derive_where;
use juice_core::dump::{Dump, ToDump};

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

    pub fn into_expr(self, range: SourceRange<'src, M>) -> Expr<'src, M> {
        Expr::new(ExprKind::BinaryOperatorSequence(self), range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for BinaryOperatorSequenceExpr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        let mut list = Vec::with_capacity(self.rest.len() * 2 + 1);

        list.push(Dump::new("ExprPart").with_field("expr", self.first.to_dump()));

        list.extend(self.rest.iter().flat_map(|(op_range, expr)| {
            [
                Dump::new("OperatorPart").with_field("operator", *op_range),
                Dump::new("ExprPart").with_field("expr", expr.to_dump()),
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

    pub fn into_expr(self, range: SourceRange<'src, M>) -> Expr<'src, M> {
        Expr::new(ExprKind::BinaryOperator(self), range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for BinaryOperatorExpr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        Dump::new("BinaryOperatorExpr")
            .with_field("operator", self.op_range)
            .with_field("lhs", self.lhs.to_dump())
            .with_field("rhs", self.rhs.to_dump())
    }
}

#[derive_where(Debug, Clone)]
pub struct UnaryOperatorExpr<'src, M: 'src + SourceManager> {
    pub operand: Box<Expr<'src, M>>,
    pub op_range: SourceRange<'src, M>,
    pub is_prefix: bool,
    pub is_invalid: bool,
}

impl<'src, M: 'src + SourceManager> UnaryOperatorExpr<'src, M> {
    pub fn new(operand: Expr<'src, M>, op_range: SourceRange<'src, M>, is_prefix: bool) -> Self {
        Self {
            operand: Box::new(operand),
            op_range,
            is_prefix,
            is_invalid: false,
        }
    }

    pub fn new_invalid(operand: Expr<'src, M>, op_range: SourceRange<'src, M>, is_prefix: bool) -> Self {
        Self {
            operand: Box::new(operand),
            op_range,
            is_prefix,
            is_invalid: true,
        }
    }

    pub fn into_expr(self, range: SourceRange<'src, M>) -> Expr<'src, M> {
        Expr::new(ExprKind::UnaryOperator(self), range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for UnaryOperatorExpr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        let name = match (self.is_prefix, self.is_invalid) {
            (true, false) => "PrefixOperatorExpr",
            (false, false) => "PostfixOperatorExpr",
            (true, true) => "InvalidPrefixOperatorExpr",
            (false, true) => "InvalidPostfixOperatorExpr",
        };

        let dump = if self.is_invalid {
            Dump::new_error(name)
        } else {
            Dump::new(name)
        };

        dump.with_field("operator", self.op_range)
            .with_field("operand", self.operand.to_dump())
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

    pub fn into_expr(self, range: SourceRange<'src, M>) -> Expr<'src, M> {
        Expr::new(ExprKind::Borrow(self), range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for BorrowExpr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        Dump::new("BorrowExpr")
            .with_field("is_mutable", self.is_mutable)
            .with_field("expr", self.expr.to_dump())
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
    InvalidInt,
    InvalidFloat,
    InvalidChar,
    InvalidString,
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for LiteralExpr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
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
                            Dump::new("InterpolationPart").with_field("expr", expr.to_dump())
                        }
                    };

                    list.push(dump);
                }

                Dump::new("StringInterpolationExpr").with_field("parts", list)
            }
            Self::InvalidInt => Dump::new_error("InvalidIntExpr"),
            Self::InvalidFloat => Dump::new_error("InvalidFloatExpr"),
            Self::InvalidChar => Dump::new_error("InvalidCharExpr"),
            Self::InvalidString => Dump::new_error("InvalidStringExpr"),
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

impl<'src, M: 'src + SourceManager> ExprKind<'src, M> {
    pub fn into_expr(self, range: SourceRange<'src, M>) -> Expr<'src, M> {
        Expr::new(self, range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for ExprKind<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        match self {
            Self::BinaryOperatorSequence(expr) => expr.to_dump(),
            Self::BinaryOperator(expr) => expr.to_dump(),
            Self::UnaryOperator(expr) => expr.to_dump(),
            Self::Borrow(expr) => expr.to_dump(),
            Self::Literal(expr) => expr.to_dump(),
            Self::Identifier(range) => Dump::new("IdentifierExpr").with_field("ident", *range),
            Self::Grouping(expr) => expr.to_dump(),
            Self::Error => Dump::new_error("ErrorExpr"),
        }
    }
}

#[derive_where(Debug, Clone)]
pub struct Expr<'src, M: 'src + SourceManager> {
    pub kind: ExprKind<'src, M>,
    pub source_range: SourceRange<'src, M>,
}

impl<'src, M: 'src + SourceManager> Expr<'src, M> {
    pub fn new(kind: ExprKind<'src, M>, source_range: SourceRange<'src, M>) -> Self {
        Self { kind, source_range }
    }

    pub fn with_binary_operator(
        self,
        (op_range, rhs): (SourceRange<'src, M>, Self),
        source_range: SourceRange<'src, M>,
    ) -> Self {
        match self.kind {
            ExprKind::BinaryOperatorSequence(mut seq) => {
                seq.push(op_range, rhs);
                seq.into_expr(source_range)
            }
            ExprKind::BinaryOperator(lhs) => {
                let mut seq = BinaryOperatorSequenceExpr::new(lhs.lhs);
                seq.push(lhs.op_range, *lhs.rhs);
                seq.push(op_range, rhs);
                seq.into_expr(source_range)
            }
            _ => BinaryOperatorExpr::new(self, op_range, rhs).into_expr(source_range),
        }
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for Expr<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        self.kind.to_dump()
    }
}

impl<M: SourceManager> Display for Expr<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.to_dump())
    }
}
