use proc_macro::TokenStream;
use quote::ToTokens as _;
use syn::parse_macro_input;

use crate::diagnostic_variants::{DiagnosticNote, Diagnostics};

mod diagnostic_variants;

#[proc_macro]
pub fn diagnostic(input: TokenStream) -> TokenStream {
    let diagnostics: Diagnostics = parse_macro_input!(input);
    diagnostics.into_token_stream().into()
}

#[proc_macro]
pub fn diagnostic_note(input: TokenStream) -> TokenStream {
    let diagnostic_note: DiagnosticNote = parse_macro_input!(input);
    diagnostic_note.into_token_stream().into()
}
