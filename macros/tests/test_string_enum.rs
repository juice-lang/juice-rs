use juice_macros::string_enum;

string_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum KeywordKind {
        If,
        Else,
        While,
        For,
        In,
        Let,
        Var,
        Func,
        Init,
        SelfLower = "self",
        SelfUpper = "Self",
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_success() {
        assert_eq!("if".parse(), Ok(KeywordKind::If));
        assert_eq!("else".parse(), Ok(KeywordKind::Else));
        assert_eq!("while".parse(), Ok(KeywordKind::While));
        assert_eq!("for".parse(), Ok(KeywordKind::For));
        assert_eq!("in".parse(), Ok(KeywordKind::In));
        assert_eq!("let".parse(), Ok(KeywordKind::Let));
        assert_eq!("var".parse(), Ok(KeywordKind::Var));
        assert_eq!("func".parse(), Ok(KeywordKind::Func));
        assert_eq!("init".parse(), Ok(KeywordKind::Init));
        assert_eq!("self".parse(), Ok(KeywordKind::SelfLower));
        assert_eq!("Self".parse(), Ok(KeywordKind::SelfUpper));
    }

    #[test]
    fn test_parse_failure() {
        assert_eq!("".parse::<KeywordKind>(), Err(()));
        assert_eq!("i".parse::<KeywordKind>(), Err(()));
        assert_eq!("els".parse::<KeywordKind>(), Err(()));
        assert_eq!("whil".parse::<KeywordKind>(), Err(()));
        assert_eq!("ford".parse::<KeywordKind>(), Err(()));
        assert_eq!("iin".parse::<KeywordKind>(), Err(()));
        assert_eq!("Let".parse::<KeywordKind>(), Err(()));
        assert_eq!("function".parse::<KeywordKind>(), Err(()));
    }

    #[test]
    fn test_to_string() {
        assert_eq!(KeywordKind::If.to_string(), "if");
        assert_eq!(KeywordKind::Else.to_string(), "else");
        assert_eq!(KeywordKind::While.to_string(), "while");
        assert_eq!(KeywordKind::For.to_string(), "for");
        assert_eq!(KeywordKind::In.to_string(), "in");
        assert_eq!(KeywordKind::Let.to_string(), "let");
        assert_eq!(KeywordKind::Var.to_string(), "var");
        assert_eq!(KeywordKind::Func.to_string(), "func");
        assert_eq!(KeywordKind::Init.to_string(), "init");
        assert_eq!(KeywordKind::SelfLower.to_string(), "self");
        assert_eq!(KeywordKind::SelfUpper.to_string(), "Self");
    }
}
