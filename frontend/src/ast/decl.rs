use std::fmt::{Display, Formatter, Result as FmtResult};

use derive_where::derive_where;
use juice_core::dump::{Dump, ToDump};

use super::expr::Expr;
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive_where(Debug, Clone)]
pub struct VarDecl<'src, M: 'src + SourceManager> {
    pub is_mutable: bool,
    pub keyword_range: SourceRange<'src, M>,
    pub name_range: SourceRange<'src, M>,
    pub initializer: Option<Expr<'src, M>>,
    pub is_invalid: bool,
}

impl<'src, M: 'src + SourceManager> VarDecl<'src, M> {
    pub fn new(
        is_mutable: bool,
        keyword_range: SourceRange<'src, M>,
        name_range: SourceRange<'src, M>,
        initializer: Option<Expr<'src, M>>,
        is_invalid: bool,
    ) -> Self {
        Self {
            is_mutable,
            keyword_range,
            name_range,
            initializer,
            is_invalid,
        }
    }

    pub fn into_decl(self, range: SourceRange<'src, M>) -> Decl<'src, M> {
        Decl::new(DeclKind::Var(self), range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for VarDecl<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        let dump = if self.is_invalid {
            Dump::new_error("InvalidVarDecl")
        } else {
            Dump::new("VarDecl")
        };

        let mut dump = dump
            .with_field("is_mutable", self.is_mutable)
            .with_field("keyword", self.keyword_range)
            .with_field("name", self.name_range);

        if let Some(initializer) = &self.initializer {
            dump = dump.with_field("initializer", initializer.to_dump());
        }

        dump
    }
}

#[derive_where(Debug, Clone)]
pub enum DeclKind<'src, M: 'src + SourceManager> {
    Var(VarDecl<'src, M>),
    Error,
}

impl<'src, M: 'src + SourceManager> DeclKind<'src, M> {
    pub fn into_decl(self, range: SourceRange<'src, M>) -> Decl<'src, M> {
        Decl::new(self, range)
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for DeclKind<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        match self {
            Self::Var(decl) => decl.to_dump(),
            Self::Error => Dump::new_error("ErrorDecl"),
        }
    }
}

#[derive_where(Debug, Clone)]
pub struct Decl<'src, M: 'src + SourceManager> {
    pub kind: DeclKind<'src, M>,
    pub source_range: SourceRange<'src, M>,
}

impl<'src, M: 'src + SourceManager> Decl<'src, M> {
    pub fn new(kind: DeclKind<'src, M>, source_range: SourceRange<'src, M>) -> Self {
        Self { kind, source_range }
    }
}

impl<'src, M: 'src + SourceManager> ToDump<'src> for Decl<'src, M> {
    fn to_dump(&self) -> Dump<'src> {
        self.kind.to_dump()
    }
}

impl<M: SourceManager> Display for Decl<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.to_dump())
    }
}
