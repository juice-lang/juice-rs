use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    ops::{Add, Range, Sub},
};

use derive_where::derive_where;

use crate::source_manager::{AriadneSourceManager, Source, SourceManager};

#[derive_where(Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc<'src, M: 'src + SourceManager> {
    pub source: Source<'src, M>,
    pub offset: usize,
}

impl<'src, M: 'src + SourceManager> SourceLoc<'src, M> {
    pub fn new(source: Source<'src, M>, offset: usize) -> Self {
        Self { source, offset }
    }

    pub fn get_range_with_len(self, len: usize) -> SourceRange<'src, M> {
        SourceRange::new(self.source, self.offset, self.offset + len)
    }

    pub fn get_range_to(self, end: Self) -> SourceRange<'src, M> {
        assert_eq!(self.source, end.source);

        SourceRange::new(self.source, self.offset, end.offset)
    }

    pub fn get_empty_range(self) -> SourceRange<'src, M> {
        SourceRange::new(self.source, self.offset, self.offset)
    }

    pub fn get_character_range(self) -> Option<SourceRange<'src, M>> {
        self.source.get_contents()[self.offset..].chars().next().map(|c| {
            let end = self.offset + c.len_utf8();
            SourceRange::new(self.source, self.offset, end)
        })
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

impl<M: SourceManager> Debug for SourceLoc<'_, M> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("SourceLoc")
            .field("source", &format!("{}", self.source))
            .field("offset", &self.offset)
            .finish()
    }
}

impl<M: AriadneSourceManager> Display for SourceLoc<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some((line, column)) = self.get_line_and_column() {
            write!(f, "{}:{}:{}", self.source, line + 1, column + 1)
        } else {
            write!(f, "({} at offset {})", self.source, self.offset)
        }
    }
}

#[derive_where(PartialEq, Eq, Clone, Copy)]
pub struct SourceRange<'src, M: 'src + SourceManager> {
    pub source: Source<'src, M>,
    pub start: usize,
    pub end: usize,
}

impl<'src, M: 'src + SourceManager> SourceRange<'src, M> {
    pub fn new(source: Source<'src, M>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    pub fn start_loc(&self) -> SourceLoc<'src, M> {
        SourceLoc::new(self.source, self.start)
    }

    pub fn end_loc(&self) -> SourceLoc<'src, M> {
        SourceLoc::new(self.source, self.end)
    }

    pub fn get_str(&self) -> &'src str {
        &self.source.get_contents()[self.start..self.end]
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl<'src, M: 'src + SourceManager> ariadne::Span for SourceRange<'src, M> {
    type SourceId = Source<'src, M>;

    fn source(&self) -> &Source<'src, M> {
        &self.source
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl<'src, M: 'src + SourceManager> chumsky::span::Span for SourceRange<'src, M> {
    type Context = Source<'src, M>;
    type Offset = usize;

    fn new(context: Source<'src, M>, range: Range<usize>) -> Self {
        Self::new(context, range.start, range.end)
    }

    fn context(&self) -> Source<'src, M> {
        self.source
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl<M: SourceManager> Debug for SourceRange<'_, M> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("SourceRange")
            .field("source", &format!("{}", self.source))
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
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
                    write!(
                        f,
                        "{}:{}:{}-{}",
                        self.source,
                        start_line + 1,
                        start_column + 1,
                        end_column + 1
                    )
                } else {
                    write!(
                        f,
                        "{}:{}:{}-{}:{}",
                        self.source,
                        start_line + 1,
                        start_column + 1,
                        end_line + 1,
                        end_column + 1
                    )
                }
            }
            _ => write!(f, "({} at offset {}-{})", self.source, self.start, self.end),
        }
    }
}
