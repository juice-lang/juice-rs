use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result as FmtResult},
    ops::{Add, Sub},
};

use derive_where::derive_where;

use crate::source_manager::{AriadneSourceManager, Source, SourceManager};

#[derive_where(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc<'a, M: SourceManager> {
    pub source: Source<'a, M>,
    pub offset: usize,
}

impl<'a, M: SourceManager> SourceLoc<'a, M> {
    pub fn new(source: Source<'a, M>, offset: usize) -> Self {
        Self { source, offset }
    }

    pub fn to_range(self, len: usize) -> SourceRange<'a, M> {
        SourceRange::new(self.source, self.offset, self.offset + len)
    }
}

impl<M: AriadneSourceManager> SourceLoc<'_, M> {
    pub fn get_line_and_column(&self) -> Option<(usize, usize)> {
        self.source.get_line_and_column(self.offset)
    }
}

impl<M: SourceManager> PartialOrd for SourceLoc<'_, M> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.source == other.source {
            self.offset.partial_cmp(&other.offset)
        } else {
            None
        }
    }
}

impl<M: SourceManager> Add<usize> for SourceLoc<'_, M> {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self::new(self.source, self.offset + rhs)
    }
}

impl<M: SourceManager> Sub<usize> for SourceLoc<'_, M> {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self::new(self.source, self.offset - rhs)
    }
}

impl<M: AriadneSourceManager> Display for SourceLoc<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some((line, column)) = self.get_line_and_column() {
            write!(f, "{}:{}:{}", self.source, line, column)
        } else {
            write!(f, "({} at offset {})", self.source, self.offset)
        }
    }
}

#[derive_where(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceRange<'a, M: SourceManager> {
    pub source: Source<'a, M>,
    pub start: usize,
    pub end: usize,
}

impl<'a, M: SourceManager> SourceRange<'a, M> {
    pub fn new(source: Source<'a, M>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    pub fn start_loc(&self) -> SourceLoc<'a, M> {
        SourceLoc::new(self.source, self.start)
    }

    pub fn end_loc(&self) -> SourceLoc<'a, M> {
        SourceLoc::new(self.source, self.end)
    }

    pub fn get_str(&self) -> &'a str {
        &self.source.get_contents()[self.start..self.end]
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl<'a, M: SourceManager> ariadne::Span for SourceRange<'a, M> {
    type SourceId = Source<'a, M>;

    fn source(&self) -> &Source<'a, M> {
        &self.source
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl<M: AriadneSourceManager> Display for SourceRange<'_, M> {
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
