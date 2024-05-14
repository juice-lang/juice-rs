use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    sync::Arc,
};

use ariadne::{Color, Fmt as _};
use derive_more::From;
use itertools::Itertools as _;
use thousands::{digits::ASCII_HEXADECIMAL, Separable as _, SeparatorPolicy};

use crate::diag::{ColorExt as _, ColorGenerator};

const UNDERSCORE_HEX_SEPARATOR: SeparatorPolicy = SeparatorPolicy {
    separator: "_",
    groups: &[4],
    digits: ASCII_HEXADECIMAL,
};

#[derive(Debug, Clone, From)]
pub enum DumpField<'src> {
    Bool(bool),
    Int(u64),
    BigInt(Arc<[u64]>),
    Float(f64),
    Char(char),
    String(&'src str),
    ArcStr(Arc<str>),
    List(Vec<Dump<'src>>),
    Dump(Dump<'src>),
}

impl DumpField<'_> {
    fn is_multiline(&self) -> bool {
        matches!(self, Self::List(_) | Self::Dump(_))
    }

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, colors: ColorGenerator) -> FmtResult {
        let indent_str = "  ".repeat(indentation);

        match self {
            Self::Bool(v) => write!(f, "{v}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::BigInt(v) => {
                let (first, rest) = v.split_last().unwrap();

                let first = format!("{first:x}").separate_by_policy(UNDERSCORE_HEX_SEPARATOR);

                let rest = rest.iter().rev().format_with("_", |part, f| {
                    f(&format!("{part:016x}").separate_by_policy(UNDERSCORE_HEX_SEPARATOR))
                });

                write!(f, "0x{first}_{rest}")?;

                Ok(())
            }
            Self::Float(v) => write!(f, "{v}"),
            Self::Char(c) => write!(f, "{c:?}"),
            Self::String(s) => write!(f, "{s:?}"),
            Self::ArcStr(s) => write!(f, "{s:?}"),
            Self::List(items) => {
                writeln!(f, "[")?;

                for item in items {
                    write!(f, "{indent_str}  ")?;

                    item.display(f, indentation + 1, colors.clone())?;

                    writeln!(f, ",")?;
                }

                write!(f, "{indent_str}]")
            }
            Self::Dump(dump) => dump.display(f, indentation, colors),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dump<'src> {
    pub name: &'static str,
    pub is_error: bool,
    pub fields: Vec<(&'static str, DumpField<'src>)>,
}

impl<'src> Dump<'src> {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            is_error: false,
            fields: Vec::new(),
        }
    }

    pub fn new_error(name: &'static str) -> Self {
        Self {
            name,
            is_error: true,
            fields: Vec::new(),
        }
    }

    pub fn with_field(mut self, name: &'static str, field: impl Into<DumpField<'src>>) -> Self {
        self.fields.push((name, field.into()));
        self
    }

    fn display(&self, f: &mut Formatter<'_>, indentation: usize, mut colors: ColorGenerator) -> FmtResult {
        let is_multiline = self.fields.len() > 2 || self.fields.iter().any(|(_, field)| field.is_multiline());

        let indent_str = "  ".repeat(indentation);

        let color = if self.is_error {
            Color::error_color()
        } else {
            colors.next()
        };

        write!(f, "{}", format!("{}(", self.name).fg(color))?;

        for (i, (name, field)) in self.fields.iter().enumerate() {
            if is_multiline {
                write!(f, "\n{}  ", indent_str)?;
            } else if i > 0 {
                write!(f, " ")?;
            }

            write!(f, "{} ", format!("{}:", name).fg(color))?;

            field.display(f, indentation + 1, colors.clone())?;

            if is_multiline || i + 1 < self.fields.len() {
                write!(f, "{}", ",".fg(color))?;
            }
        }

        if is_multiline {
            write!(f, "\n{}", indent_str)?;
        }

        write!(f, "{}", ")".fg(color))?;

        Ok(())
    }
}

impl Display for Dump<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.display(f, 0, ColorGenerator::default())?;
        writeln!(f)
    }
}

pub trait ToDump<'src> {
    fn to_dump(&self) -> Dump<'src>;
}
