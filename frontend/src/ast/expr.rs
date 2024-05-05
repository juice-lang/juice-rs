use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    sync::Arc,
};

use ariadne::{Color, Fmt as _};
use derive_where::derive_where;
use juice_core::diag::{ColorExt as _, ColorGenerator};
use thousands::{digits::ASCII_HEXADECIMAL, Separable as _, SeparatorPolicy};

use crate::{source_loc::SourceRange, source_manager::SourceManager};

const UNDERSCORE_HEX_SEPARATOR: SeparatorPolicy = SeparatorPolicy {
    separator: "_",
    groups: &[4],
    digits: ASCII_HEXADECIMAL,
};

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

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let color = colors.next();
        let indent_str = " ".repeat(indentation * 4);

        let part_color = colors.next();

        write!(
            f,
            "{0}\n{1}  {2} [\n{1}    {3}\n{1}      {4} ",
            "BinaryOperatorSequenceExpr(".fg(color),
            indent_str,
            "parts:".fg(color),
            "ExprPart(".fg(part_color),
            "expr:".fg(part_color),
        )?;

        self.first.kind.display(f, indentation + 2, colors.clone())?;

        writeln!(f, "{}    {},", indent_str, ")".fg(part_color))?;

        for (op_range, expr) in &self.rest {
            writeln!(
                f,
                "{}    {} {:?}{},",
                indent_str,
                "OperatorPart(operator:".fg(part_color),
                op_range.get_str(),
                ")".fg(part_color)
            )?;

            write!(
                f,
                "{0}    {1}\n{0}      {2} ",
                indent_str,
                "ExprPart(".fg(part_color),
                "expr:".fg(part_color)
            )?;

            expr.kind.display(f, indentation + 2, colors.clone())?;

            writeln!(f, "{}    {},", indent_str, ")".fg(part_color))?;
        }

        writeln!(f, "{0}  ]\n{0}{1}", indent_str, ")".fg(color))
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

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let color = colors.next();
        let indent_str = " ".repeat(indentation * 4);

        write!(
            f,
            "{0}\n{1}  {2} {3:?}\n{1}  {4} ",
            "BinaryOperatorExpr(".fg(color),
            indent_str,
            "operator:".fg(color),
            self.op_range.get_str(),
            "lhs:".fg(color),
        )?;

        self.lhs.kind.display(f, indentation + 1, colors.clone())?;

        write!(f, "{}  {} ", indent_str, "rhs:".fg(color))?;

        self.rhs.kind.display(f, indentation + 1, colors)?;

        writeln!(f, "{}{}", indent_str, ")".fg(color))
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

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let color = colors.next();
        let indent_str = " ".repeat(indentation * 4);

        let name = if self.is_prefix { "Prefix" } else { "Postfix" };

        write!(
            f,
            "{0}{1}\n{2}  {3} {4:?}\n{2}  {5} ",
            name.fg(color),
            "OperatorExpr(".fg(color),
            indent_str,
            "operator:".fg(color),
            self.op_range.get_str(),
            "operand:".fg(color),
        )?;

        self.operand.kind.display(f, indentation + 1, colors)?;

        writeln!(f, "{}{}", indent_str, ")".fg(color))
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

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let color = colors.next();
        let indent_str = " ".repeat(indentation * 4);

        write!(
            f,
            "{0}\n{1}  {2} {3}\n{1}  {4} ",
            "BorrowExpr(".fg(color),
            indent_str,
            "is_mutable:".fg(color),
            self.is_mutable,
            "expr:".fg(color),
        )?;

        self.expr.kind.display(f, indentation + 1, colors)?;

        writeln!(f, "{}{}", indent_str, ")".fg(color))
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

impl<M: SourceManager> LiteralExpr<'_, M> {
    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let color = colors.next();
        let indent_str = " ".repeat(indentation * 4);

        match self {
            Self::Bool(value) => writeln!(f, "{} {}{}", "BoolExpr(value:".fg(color), value, ")".fg(color)),
            Self::Int(IntLiteralExpr::Int(value)) => {
                writeln!(f, "{} {}{}", "IntExpr(value:".fg(color), value, ")".fg(color))
            }
            Self::Int(IntLiteralExpr::BigInt(value)) => {
                let (last, rest) = value.split_last().unwrap();

                write!(
                    f,
                    "{}\n{}  {} 0x{}_",
                    "IntExpr(".fg(color),
                    indent_str,
                    "value:".fg(color),
                    last.separate_by_policy(UNDERSCORE_HEX_SEPARATOR)
                )?;

                for part in rest.iter().rev() {
                    write!(
                        f,
                        "{}",
                        format!("{:016x}", part).separate_by_policy(UNDERSCORE_HEX_SEPARATOR)
                    )?;
                }

                writeln!(f, "{}{}", indent_str, ")".fg(color))
            }
            Self::Float(value) => writeln!(f, "{} {}{}", "FloatExpr(value:".fg(color), value, ")".fg(color)),
            Self::Char(value) => writeln!(f, "{} '{}'{}", "CharExpr(value:".fg(color), value, ")".fg(color)),
            Self::String(value) => writeln!(f, "{} {:?}{}", "StringExpr(value:".fg(color), value, ")".fg(color)),
            Self::StringInterpolation(parts) => {
                writeln!(
                    f,
                    "{}\n{}  {} [",
                    "StringInterpolationExpr(".fg(color),
                    indent_str,
                    "parts:".fg(color)
                )?;

                let part_color = colors.next();

                for part in parts.as_ref() {
                    match part {
                        InterpolationExprPart::String(s) => {
                            writeln!(
                                f,
                                "{}    {} {:?}{},",
                                indent_str,
                                "StringPart(value:".fg(part_color),
                                s,
                                ")".fg(part_color)
                            )?;
                        }
                        InterpolationExprPart::Interpolation(expr) => {
                            write!(
                                f,
                                "{0}    {1}\n{0}      {2} ",
                                indent_str,
                                "InterpolationPart(".fg(part_color),
                                "expr:".fg(part_color)
                            )?;
                            expr.kind.display(f, indentation + 2, colors.clone())?;
                            writeln!(f, "{}    {},", indent_str, ")".fg(part_color))?;
                        }
                    }
                }

                writeln!(f, "{0}  ]\n{0}{1}", indent_str, ")".fg(color))
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

impl<M: SourceManager> ExprKind<'_, M> {
    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        match self {
            Self::BinaryOperatorSequence(expr) => expr.display(f, indentation, colors),
            Self::BinaryOperator(expr) => expr.display(f, indentation, colors),
            Self::UnaryOperator(expr) => expr.display(f, indentation, colors),
            Self::Borrow(expr) => expr.display(f, indentation, colors),
            Self::Literal(expr) => expr.display(f, indentation, colors),
            Self::Identifier(range) => {
                let color = colors.next();
                writeln!(
                    f,
                    "{} {:?}{}",
                    "IdentifierExpr(ident:".fg(color),
                    range.get_str(),
                    ")".fg(color)
                )
            }
            Self::Grouping(expr) => expr.kind.display(f, indentation, colors),
            Self::Error => writeln!(f, "{}", "ErrorExpr()".fg(Color::error_color())),
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
}

impl<M: SourceManager> Display for Expr<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.kind.display(f, 0, ColorGenerator::default())
    }
}
