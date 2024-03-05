use juice_macros::Keyword;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Keyword)]
enum Keyword {
    If,
    Else,
    While,
    For,
    In,
    Let,
    Var,
    Func,
    Init,
    #[kw("self")]
    SelfLower,
    #[kw("Self")]
    SelfUpper,
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_success() {
        assert_eq!("if".parse(), Ok(Keyword::If));
        assert_eq!("else".parse(), Ok(Keyword::Else));
        assert_eq!("while".parse(), Ok(Keyword::While));
        assert_eq!("for".parse(), Ok(Keyword::For));
        assert_eq!("in".parse(), Ok(Keyword::In));
        assert_eq!("let".parse(), Ok(Keyword::Let));
        assert_eq!("var".parse(), Ok(Keyword::Var));
        assert_eq!("func".parse(), Ok(Keyword::Func));
        assert_eq!("init".parse(), Ok(Keyword::Init));
        assert_eq!("self".parse(), Ok(Keyword::SelfLower));
        assert_eq!("Self".parse(), Ok(Keyword::SelfUpper));
    }

    #[test]
    fn test_parse_failure() {
        assert_eq!("".parse::<Keyword>(), Err(()));
        assert_eq!("i".parse::<Keyword>(), Err(()));
        assert_eq!("els".parse::<Keyword>(), Err(()));
        assert_eq!("whil".parse::<Keyword>(), Err(()));
        assert_eq!("ford".parse::<Keyword>(), Err(()));
        assert_eq!("iin".parse::<Keyword>(), Err(()));
        assert_eq!("Let".parse::<Keyword>(), Err(()));
        assert_eq!("function".parse::<Keyword>(), Err(()));
    }

    #[test]
    fn test_to_string() {
        assert_eq!(Keyword::If.to_string(), "if");
        assert_eq!(Keyword::Else.to_string(), "else");
        assert_eq!(Keyword::While.to_string(), "while");
        assert_eq!(Keyword::For.to_string(), "for");
        assert_eq!(Keyword::In.to_string(), "in");
        assert_eq!(Keyword::Let.to_string(), "let");
        assert_eq!(Keyword::Var.to_string(), "var");
        assert_eq!(Keyword::Func.to_string(), "func");
        assert_eq!(Keyword::Init.to_string(), "init");
        assert_eq!(Keyword::SelfLower.to_string(), "self");
        assert_eq!(Keyword::SelfUpper.to_string(), "Self");
    }
}
