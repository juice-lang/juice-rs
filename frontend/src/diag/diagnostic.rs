use juice_core::diag::Colored;
use juice_macros::diagnostic;

diagnostic!(
    #[derive(Debug, Clone)]
    pub enum Diagnostic<'a> {
        [error] InvalidCharacter(c: into Colored<char>) => "Invalid character `{}` in source file",
        [error] UnterminatedComment => "Unterminated block comment",
    }

    #[derive(Debug, Clone)]
    pub enum StaticDiagnostic<'a> {
        [error] IoError(message: into Colored<String>) => "Error while doing IO: {}",
    }
);
