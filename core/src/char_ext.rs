#[allow(clippy::wrong_self_convention)]
pub trait CharExt: Sized {
    fn is_whitespace_or_newline(self) -> bool;
    fn is_insignificant_whitespace(self) -> bool;
    fn is_operator(self) -> bool;
    fn is_dot_operator(self) -> bool;
    fn is_identifier_start(self) -> bool;
    fn is_identifier_char(self) -> bool;
    fn is_binary_digit(self) -> bool;
    fn is_octal_digit(self) -> bool;
    fn is_decimal_digit(self) -> bool;
    fn is_hex_digit(self) -> bool;
    fn is_number_end(self) -> bool;
}

macro_rules! impl_char_ext {
    ($self:ident; $($name:ident => $val:expr);* $(;)?) => {
        impl CharExt for char {
            $(
                #[inline]
                fn $name($self) -> bool {
                    $val
                }
            )*
        }

        impl CharExt for &char {
            $(
                #[inline]
                fn $name($self) -> bool {
                    CharExt::$name(*$self)
                }
            )*
        }
    };
}

impl_char_ext! {
    self;
    is_whitespace_or_newline => matches!(self, ' ' | '\n' | '\r' | '\t' | '\x0C' | '\x0B' | '\0');
    is_insignificant_whitespace => matches!(self, ' ' | '\r' | '\t' | '\x0C' | '\x0B' | '\0');
    is_operator => matches!(
        self,
        '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '&' | '|' | '^' | '!' | '~'
    );
    is_dot_operator => self.is_operator() || self == '.';
    is_identifier_start => self.is_ascii_alphabetic() || matches!(self, '_' | '$');
    is_identifier_char => self.is_ascii_alphanumeric() || self == '_';
    is_binary_digit => matches!(self, '0' | '1');
    is_octal_digit => matches!(self, '0'..='7');
    is_decimal_digit => self.is_ascii_digit();
    is_hex_digit => self.is_ascii_hexdigit();
    is_number_end => !self.is_identifier_char();
}
