use std::{io, sync::Arc};

use ariadne::{Color, Fmt as _};
use derive_more::From;
use itertools::Itertools as _;
use thousands::{digits::ASCII_HEXADECIMAL, Separable as _, SeparatorPolicy};

use crate::{
    diag::{ColorExt as _, ColorGenerator},
    OutputStream,
};

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

    fn write_impl(&self, stream: &mut dyn OutputStream, indentation: usize, colors: ColorGenerator) -> io::Result<()> {
        let indent_str = "  ".repeat(indentation);

        match self {
            Self::Bool(v) => write!(stream, "{v}"),
            Self::Int(v) => write!(stream, "{v}"),
            Self::BigInt(v) => {
                let (first, rest) = v.split_last().unwrap();

                let first = format!("{first:x}").separate_by_policy(UNDERSCORE_HEX_SEPARATOR);

                let rest = rest.iter().rev().format_with("_", |part, f| {
                    f(&format!("{part:016x}").separate_by_policy(UNDERSCORE_HEX_SEPARATOR))
                });

                write!(stream, "0x{first}_{rest}")?;

                Ok(())
            }
            Self::Float(v) => write!(stream, "{v}"),
            Self::Char(c) => write!(stream, "{c:?}"),
            Self::String(s) => write!(stream, "{s:?}"),
            Self::ArcStr(s) => write!(stream, "{s:?}"),
            Self::List(items) => {
                writeln!(stream, "[")?;

                for item in items {
                    write!(stream, "{indent_str}  ")?;

                    item.write_impl(stream, indentation + 1, colors.clone())?;

                    writeln!(stream, ",")?;
                }

                write!(stream, "{indent_str}]")
            }
            Self::Dump(dump) => dump.write_impl(stream, indentation, colors),
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

    fn write_impl(
        &self,
        stream: &mut dyn OutputStream,
        indentation: usize,
        mut colors: ColorGenerator,
    ) -> io::Result<()> {
        let is_multiline = self.fields.len() > 2 || self.fields.iter().any(|(_, field)| field.is_multiline());

        let indent_str = "  ".repeat(indentation);

        let color = if !stream.is_terminal() {
            None
        } else if self.is_error {
            Some(Color::error_color())
        } else {
            Some(colors.next())
        };

        write!(stream, "{}", format!("{}(", self.name).fg(color))?;

        for (i, (name, field)) in self.fields.iter().enumerate() {
            if is_multiline {
                write!(stream, "\n{}  ", indent_str)?;
            } else if i > 0 {
                write!(stream, " ")?;
            }

            write!(stream, "{} ", format!("{}:", name).fg(color))?;

            field.write_impl(stream, indentation + 1, colors.clone())?;

            if is_multiline || i + 1 < self.fields.len() {
                write!(stream, "{}", ",".fg(color))?;
            }
        }

        if is_multiline {
            write!(stream, "\n{}", indent_str)?;
        }

        write!(stream, "{}", ")".fg(color))?;

        Ok(())
    }

    pub fn write(&self, stream: &mut dyn OutputStream) -> io::Result<()> {
        self.write_impl(stream, 0, ColorGenerator::default())?;
        writeln!(stream)
    }
}

pub trait ToDump<'src> {
    fn to_dump(&self) -> Dump<'src>;
}
