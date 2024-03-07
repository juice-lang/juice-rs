use std::{num::NonZero, str::Chars};

#[derive(Debug, Clone)]
pub struct PeekableChars<'a>(Chars<'a>);

impl<'a> PeekableChars<'a> {
    pub fn peek(&self) -> Option<char> {
        self.0.clone().next()
    }

    pub fn peek2(&self) -> Option<char> {
        let mut chars = self.0.clone();
        chars.next();
        chars.next()
    }

    pub fn peek_first_after(&self, func: impl Fn(char) -> bool) -> Option<char> {
        let mut chars = self.0.clone();
        let peek = loop {
            let next = chars.next()?;

            if !func(next) {
                break next;
            }
        };

        Some(peek)
    }

    pub fn peek_first_after_eq(&self, expected: char) -> Option<char> {
        self.peek_first_after(|next| next == expected)
    }

    pub fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<char> {
        let next = self.peek();
        if next.is_some_and(func) {
            self.0.next()
        } else {
            None
        }
    }

    pub fn next_if_eq(&mut self, expected: char) -> Option<char> {
        self.next_if(|next| next == expected)
    }

    pub fn as_str(&self) -> &'a str {
        self.0.as_str()
    }
}

impl<'a> Iterator for PeekableChars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn advance_by(&mut self, n: usize) -> Result<(), NonZero<usize>> {
        self.0.advance_by(n)
    }
}

impl<'a> From<&'a str> for PeekableChars<'a> {
    fn from(s: &'a str) -> Self {
        Self(s.chars())
    }
}
