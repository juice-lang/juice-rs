use juice_core::diag::Colored;
use juice_macros::diagnostic;

diagnostic!(
    pub enum Diagnostic<'a> {
        [error] CannotBorrowLetMutable(name: into Colored<&'a str>) =>
            "cannot borrow `{}` as mutable, as it is declared as a `let` binding",
    }

    pub enum StaticDiagnostic {
        [error] IoError(message: into Colored<String>) => "while doing IO: {}",
    }
);
