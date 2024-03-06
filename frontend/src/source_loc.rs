use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result as FmtResult},
};

use crate::source_manager::Source;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceLoc<'a> {
    pub source: Source<'a>,
    pub offset: usize,
}

impl<'a> SourceLoc<'a> {
    pub fn new(source: Source<'a>, offset: usize) -> Self {
        Self { source, offset }
    }

    pub fn get_line_and_column(&self) -> Option<(usize, usize)> {
        self.source
            .get_ariadne_source()
            .get_offset_line(self.offset)
            .map(|(_, line, column)| (line, column))
    }
}

impl<'a> PartialOrd for SourceLoc<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.source == other.source {
            self.offset.partial_cmp(&other.offset)
        } else {
            None
        }
    }
}

impl<'a> Display for SourceLoc<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some((line, column)) = self.get_line_and_column() {
            write!(f, "{}:{}:{}", self.source, line, column)
        } else {
            write!(f, "({} at offset {})", self.source, self.offset)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceRange<'a> {
    pub source: Source<'a>,
    pub start: usize,
    pub end: usize,
}

impl<'a> SourceRange<'a> {
    pub fn new(source: Source<'a>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    pub fn start_loc(&self) -> SourceLoc<'a> {
        SourceLoc::new(self.source, self.start)
    }

    pub fn end_loc(&self) -> SourceLoc<'a> {
        SourceLoc::new(self.source, self.end)
    }

    pub fn get_text(&self) -> &'a str {
        &self.source.get_contents()[self.start..self.end]
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl<'a> ariadne::Span for SourceRange<'a> {
    type SourceId = Source<'a>;

    fn source(&self) -> &Source<'a> {
        &self.source
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl<'a> Display for SourceRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match (
            self.start_loc().get_line_and_column(),
            self.end_loc().get_line_and_column(),
        ) {
            (Some((start_line, start_column)), Some((end_line, end_column))) => {
                if start_line == end_line {
                    write!(f, "{}:{}:{}-{}", self.source, start_line, start_column, end_column)
                } else {
                    write!(
                        f,
                        "{}:{}:{}-{}:{}",
                        self.source, start_line, start_column, end_line, end_column
                    )
                }
            }
            _ => write!(f, "({} at offset {}-{})", self.source, self.start, self.end),
        }
    }
}
