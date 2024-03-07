use juice_core::CharExt;

use super::Lexer;
use crate::Result;

pub trait Fsm {
    type State;

    fn next_state(state: Self::State, c: char, peek: Option<char>) -> Result<Self::State, Self::State>;
    fn no_input_state(state: Self::State) -> Self::State;

    fn run(lexer: &mut Lexer, start_state: Self::State) -> Self::State {
        let mut state = start_state;

        loop {
            let Some(c) = lexer.peek() else {
                return Self::no_input_state(state);
            };

            let res = Self::next_state(state, c, lexer.peek2());

            match res {
                Ok(next_state) => {
                    state = next_state;
                    lexer.advance();
                }
                Err(final_state) => return final_state,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberFsmState {
    ZeroStart,
    BinaryStart,
    BinaryInteger,
    OctalStart,
    OctalInteger,
    HexStart,
    HexInteger,
    Integer,
    FloatStart,
    Float,
    ExponentStart,
    ExponentSign,
    Exponent,
    InvalidDigit,
    InvalidBinaryDigit,
    MissingBinaryDigit,
    InvalidOctalDigit,
    MissingOctalDigit,
    InvalidHexDigit,
    MissingHexDigit,
    InvalidFloatDigit,
    InvalidExponentDigit,
    MissingExponentDigit,
}

pub struct NumberFsm;

impl Fsm for NumberFsm {
    type State = NumberFsmState;

    fn next_state(state: Self::State, c: char, peek: Option<char>) -> Result<Self::State, Self::State> {
        use NumberFsmState::*;

        let state = match (state, c) {
            (ZeroStart, 'b') => BinaryStart,
            (ZeroStart, 'o') => OctalStart,
            (ZeroStart, 'x') => HexStart,
            (ZeroStart, '.') => {
                if peek.is_some_and(CharExt::is_decimal_digit) {
                    FloatStart
                } else {
                    return Err(Integer);
                }
            }
            (ZeroStart, 'e' | 'E') => ExponentStart,
            (ZeroStart, '_') => Integer,
            (ZeroStart, _) => {
                if CharExt::is_decimal_digit(c) {
                    Integer
                } else if CharExt::is_number_end(c) {
                    return Err(Integer);
                } else {
                    return Err(InvalidDigit);
                }
            }
            (BinaryStart, _) => {
                if CharExt::is_binary_digit(c) {
                    BinaryInteger
                } else {
                    return Err(InvalidBinaryDigit);
                }
            }
            (BinaryInteger, '_') => BinaryInteger,
            (BinaryInteger, _) => {
                if CharExt::is_binary_digit(c) {
                    BinaryInteger
                } else if CharExt::is_number_end(c) {
                    return Err(BinaryInteger);
                } else {
                    return Err(InvalidBinaryDigit);
                }
            }
            (OctalStart, _) => {
                if CharExt::is_octal_digit(c) {
                    OctalInteger
                } else {
                    return Err(InvalidOctalDigit);
                }
            }
            (OctalInteger, '_') => OctalInteger,
            (OctalInteger, _) => {
                if CharExt::is_octal_digit(c) {
                    OctalInteger
                } else if CharExt::is_number_end(c) {
                    return Err(OctalInteger);
                } else {
                    return Err(InvalidOctalDigit);
                }
            }
            (HexStart, _) => {
                if CharExt::is_hex_digit(c) {
                    HexInteger
                } else {
                    return Err(InvalidHexDigit);
                }
            }
            (HexInteger, '_') => HexInteger,
            (HexInteger, _) => {
                if CharExt::is_hex_digit(c) {
                    HexInteger
                } else if CharExt::is_number_end(c) {
                    return Err(HexInteger);
                } else {
                    return Err(InvalidHexDigit);
                }
            }
            (Integer, '.') => {
                if peek.is_some_and(CharExt::is_decimal_digit) {
                    FloatStart
                } else {
                    return Err(Integer);
                }
            }
            (Integer, 'e' | 'E') => ExponentStart,
            (Integer, '_') => Integer,
            (Integer, _) => {
                if CharExt::is_decimal_digit(c) {
                    Integer
                } else if CharExt::is_number_end(c) {
                    return Err(Integer);
                } else {
                    return Err(InvalidDigit);
                }
            }
            (FloatStart, _) => {
                if CharExt::is_decimal_digit(c) {
                    Float
                } else {
                    unreachable!("Only got here if the next character is a decimal digit")
                }
            }
            (Float, 'e' | 'E') => ExponentStart,
            (Float, '_') => Float,
            (Float, _) => {
                if CharExt::is_decimal_digit(c) {
                    Float
                } else if CharExt::is_number_end(c) {
                    return Err(Float);
                } else {
                    return Err(InvalidFloatDigit);
                }
            }
            (ExponentStart, '+') | (ExponentStart, '-') => ExponentSign,
            (ExponentStart, _) => {
                if CharExt::is_decimal_digit(c) {
                    Exponent
                } else if CharExt::is_number_end(c) {
                    return Err(MissingExponentDigit);
                } else {
                    return Err(InvalidExponentDigit);
                }
            }
            (ExponentSign, _) => {
                if CharExt::is_decimal_digit(c) {
                    Exponent
                } else if CharExt::is_number_end(c) {
                    return Err(MissingExponentDigit);
                } else {
                    return Err(InvalidExponentDigit);
                }
            }
            (Exponent, '_') => Exponent,
            (Exponent, _) => {
                if CharExt::is_decimal_digit(c) {
                    Exponent
                } else if CharExt::is_number_end(c) {
                    return Err(Exponent);
                } else {
                    return Err(InvalidExponentDigit);
                }
            }
            _ => unreachable!("Invalid state"),
        };

        Ok(state)
    }

    fn no_input_state(state: Self::State) -> Self::State {
        use NumberFsmState::*;

        match state {
            ZeroStart => Integer,
            BinaryInteger | OctalInteger | HexInteger | Integer | Float | Exponent => state,
            BinaryStart => MissingBinaryDigit,
            OctalStart => MissingOctalDigit,
            HexStart => MissingHexDigit,
            ExponentStart | ExponentSign => MissingExponentDigit,
            _ => unreachable!("Invalid state"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringFsmStateKind {
    StringStart(usize),
    String,
    EscapeStart,
    UnicodeEscapeStart,
    UnicodeEscape(usize),
    InterpolationStart,
    Interpolation,
    StringEnd(usize),
    InvalidEscape,
    MissingUnicodeEscapeBrace,
    InvalidUnicodeEscapeDigit,
    MissingUnicodeEscape,
    OverlongUnicodeEscape,
    MissingTerminator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringFsmState {
    pub kind: StringFsmStateKind,
    pub multiline: bool,
}

impl StringFsmState {
    pub fn new(kind: StringFsmStateKind, multiline: bool) -> Self {
        Self { kind, multiline }
    }
}

pub struct StringFsm;

impl Fsm for StringFsm {
    type State = StringFsmState;

    fn next_state(state: Self::State, c: char, peek: Option<char>) -> Result<Self::State, Self::State> {
        use StringFsmStateKind::*;

        let StringFsmState { kind, multiline } = state;

        let make_error = |kind| Err(StringFsmState::new(kind, multiline));

        let kind = match (kind, c) {
            (StringStart(1), '"') => {
                if peek == Some('"') {
                    return Ok(StringFsmState::new(StringStart(2), true));
                } else {
                    StringEnd(1)
                }
            }
            (StringStart(1), '\\') => EscapeStart,
            (StringStart(1), '$') if peek == Some('{') => InterpolationStart,
            (StringStart(1), '\n' | '\r') => return make_error(MissingTerminator),
            (StringStart(1), _) => String,
            (StringStart(2), '"') => String,
            (String, '"') => StringEnd(1),
            (String, '\\') => EscapeStart,
            (String, '$') if peek == Some('{') => InterpolationStart,
            (String, '\n' | '\r') if !multiline => return make_error(MissingTerminator),
            (String, _) => String,
            (EscapeStart, 'u') => UnicodeEscapeStart,
            (EscapeStart, '\r') if multiline && peek == Some('\n') => EscapeStart,
            (EscapeStart, '\n' | '\r') if multiline => String,
            (EscapeStart, _) => {
                if c.is_string_escape() {
                    String
                } else {
                    return make_error(InvalidEscape);
                }
            }
            (UnicodeEscapeStart, '{') => UnicodeEscape(0),
            (UnicodeEscapeStart, _) => return make_error(MissingUnicodeEscapeBrace),
            (UnicodeEscape(count), '}') => {
                if count == 0 {
                    return make_error(MissingUnicodeEscape);
                } else {
                    String
                }
            }
            (UnicodeEscape(count), _) => {
                if c.is_hex_digit() {
                    if count < 8 {
                        UnicodeEscape(count + 1)
                    } else {
                        return make_error(OverlongUnicodeEscape);
                    }
                } else {
                    return make_error(InvalidUnicodeEscapeDigit);
                }
            }
            (InterpolationStart, '{') => Interpolation,
            (Interpolation, _) => return make_error(Interpolation),
            (StringEnd(1), _) if !multiline => return make_error(StringEnd(1)),
            (StringEnd(1), '"') if peek == Some('"') => StringEnd(2),
            (StringEnd(1), '\\') => EscapeStart,
            (StringEnd(1), '$') if peek == Some('{') => InterpolationStart,
            (StringEnd(1), _) => String,
            (StringEnd(2), '"') => StringEnd(3),
            (StringEnd(3), _) => return make_error(StringEnd(3)),
            _ => unreachable!("Invalid state"),
        };

        Ok(StringFsmState::new(kind, multiline))
    }

    fn no_input_state(state: Self::State) -> Self::State {
        use StringFsmStateKind::*;

        if let StringEnd(count) = state.kind {
            if count == 1 && !state.multiline || count == 3 && state.multiline {
                return state;
            }
        }

        StringFsmState::new(MissingTerminator, state.multiline)
    }
}
