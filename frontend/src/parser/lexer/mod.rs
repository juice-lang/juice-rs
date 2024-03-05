mod token;

use std::str::Chars;

pub use self::token::Token;
use crate::source_manager::Source;

pub struct Lexer<'a> {
    source: Source<'a>,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        let chars = source.get_contents().chars();
        Self { source, chars }
    }

    fn next_token(&mut self) -> Option<Token> {
        todo!()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
