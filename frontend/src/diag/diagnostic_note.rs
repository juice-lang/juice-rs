use juice_core::diag::Colored;
use juice_macros::diagnostic_note;

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticContextNote<'a> {
        MutableBorrowHere => "mutable borrow occurs here",
        VariableDefinedHere(name: into Colored<&'a str>) => "`{}` is defined here",
    }
);

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticNote {}
);
