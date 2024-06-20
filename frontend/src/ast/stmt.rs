use derive_where::derive_where;
use juice_core::dump::{Dump, ToDump};

use super::{decl::Decl, expr::Expr};
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive_where(Debug, Clone)]
pub enum StmtKind<'src, M: 'src + SourceManager> {
    Decl(Decl<'src, M>),
    Expr(Expr<'src, M>),
}

impl<'src, M: 'src + SourceManager> StmtKind<'src, M> {
    pub fn into_stmt(self, range: SourceRange<'src, M>) -> Stmt<'src, M> {
        Stmt::new(self, range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for StmtKind<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        match self {
            Self::Decl(decl) => decl.to_dump(),
            Self::Expr(expr) => Dump::new("ExprStmt").with_field("expr", expr.to_dump()),
        }
    }
}

#[derive_where(Debug, Clone)]
pub struct Stmt<'src, M: 'src + SourceManager> {
    pub kind: StmtKind<'src, M>,
    pub source_range: SourceRange<'src, M>,
}

impl<'src, M: 'src + SourceManager> Stmt<'src, M> {
    pub fn new(kind: StmtKind<'src, M>, source_range: SourceRange<'src, M>) -> Self {
        Self { kind, source_range }
    }
}

impl<'src, M: 'src + SourceManager> From<Decl<'src, M>> for Stmt<'src, M> {
    fn from(decl: Decl<'src, M>) -> Self {
        let source_range = decl.source_range;

        Self {
            kind: StmtKind::Decl(decl),
            source_range,
        }
    }
}

impl<'src, M: 'src + SourceManager> From<Expr<'src, M>> for Stmt<'src, M> {
    fn from(expr: Expr<'src, M>) -> Self {
        let source_range = expr.source_range;

        Self {
            kind: StmtKind::Expr(expr),
            source_range,
        }
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for Stmt<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        self.kind.to_dump()
    }
}

#[derive_where(Debug, Clone)]
pub struct StmtList<'src, M: 'src + SourceManager> {
    pub stmts: Vec<Stmt<'src, M>>,
    pub has_trailing_semicolon: bool,
}

impl<'src, M: 'src + SourceManager> StmtList<'src, M> {
    pub fn new(stmts: Vec<Stmt<'src, M>>, has_trailing_semicolon: bool) -> Self {
        Self {
            stmts,
            has_trailing_semicolon,
        }
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for StmtList<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        Dump::new("StmtList")
            .with_field("stmts", self.stmts.iter().map(ToDump::to_dump).collect::<Vec<_>>())
            .with_field("has_trailing_semicolon", self.has_trailing_semicolon)
    }
}
