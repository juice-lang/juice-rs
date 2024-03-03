use std::fmt::{Display, Formatter, Result as FmtResult};

use convert_case::{Case, Casing as _};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    bracketed, custom_keyword, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Ident, LitStr, Result, Token, Type,
};

macro_rules! error {
    ($($name: ident => $message: literal,)*) => {
        enum Error {
            $(
                $name,
            )*
        }

        impl Display for Error {
            fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                let message = match self {
                    $(
                        Self::$name => $message,
                    )*
                };

                write!(f, "{}", message)
            }
        }
    }
}

error! {
    ExpectedDiagnosticKind => "Expected either 'error' or 'warning'",
    FieldNamedColor => "Diagnostic fields cannot be named 'color'",
    NoFieldsWithFormat => "Format string contains a placeholder but no fields were provided",
}

mod keyword {
    use super::*;

    custom_keyword!(color);
    custom_keyword!(error);
    custom_keyword!(into);
    custom_keyword!(warning);
}

pub enum DiagnosticKind {
    Error,
    Warning,
    StaticError,
    StaticWarning,
}

impl Parse for DiagnosticKind {
    fn parse(input: ParseStream) -> Result<Self> {
        let is_static = if input.peek(Token![static]) {
            input.parse::<Token![static]>()?;

            true
        } else {
            false
        };

        if input.peek(keyword::error) {
            input.parse::<keyword::error>()?;

            Ok(if is_static { Self::StaticError } else { Self::Error })
        } else if input.peek(keyword::warning) {
            input.parse::<keyword::warning>()?;

            Ok(if is_static { Self::StaticWarning } else { Self::Warning })
        } else {
            Err(input.error(Error::ExpectedDiagnosticKind))
        }
    }
}

pub struct DiagnosticField {
    pub name: Ident,
    pub ty: Type,
    pub is_into: bool,
}

impl DiagnosticField {
    pub fn function_argument(&self) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;

        let ty = if self.is_into {
            quote! { impl Into<#ty> }
        } else {
            quote! { #ty }
        };

        quote! {
            #name: #ty
        }
    }
}

impl Parse for DiagnosticField {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(keyword::color) {
            return Err(input.error(Error::FieldNamedColor));
        }

        let name = input.parse::<Ident>()?;

        input.parse::<Token![:]>()?;

        let is_into = if input.peek(keyword::into) {
            input.parse::<keyword::into>()?;

            true
        } else {
            false
        };

        let ty = input.parse::<Type>()?;

        Ok(Self { name, ty, is_into })
    }
}

pub struct DiagnosticMessageVariant {
    pub name: Ident,
    pub fields: Vec<DiagnosticField>,
    pub format: LitStr,
}

impl DiagnosticMessageVariant {
    pub fn enum_variant(&self) -> TokenStream {
        let name = &self.name;
        let fields = if self.fields.is_empty() {
            None
        } else {
            let fields = self.fields.iter().map(|field| {
                let name = &field.name;
                let ty = &field.ty;

                quote! { #name: #ty, }
            });

            Some(quote! {
                {
                    #(#fields)*
                }
            })
        };

        quote! {
            #name #fields
        }
    }

    pub fn match_pattern(&self, variant_base: TokenStream, ignore_fields: bool) -> TokenStream {
        let name = &self.name;
        let pattern_base = quote! {
            #variant_base::#name
        };

        let pattern_fields = if self.fields.is_empty() {
            None
        } else {
            let inner = if ignore_fields {
                quote! { .. }
            } else {
                let names = self.fields.iter().map(|field| &field.name);

                quote! { #(#names),* }
            };

            Some(quote! { { #inner } })
        };

        quote! {
            #pattern_base #pattern_fields
        }
    }
}

impl Parse for DiagnosticMessageVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;

        let fields = if input.peek(Paren) {
            let fields_input;
            parenthesized!(fields_input in input);

            Punctuated::<DiagnosticField, Token![,]>::parse_terminated(&fields_input)?
                .into_iter()
                .collect()
        } else {
            Vec::new()
        };

        input.parse::<Token![=>]>()?;

        let format = input.parse::<LitStr>()?;

        if fields.is_empty() && format.value().contains("{}") {
            return Err(syn::Error::new(format.span(), Error::NoFieldsWithFormat));
        }

        Ok(Self { name, fields, format })
    }
}

pub struct DiagnosticVariant {
    pub kind: DiagnosticKind,
    pub inner: DiagnosticMessageVariant,
}

impl Parse for DiagnosticVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let kind_input;
        bracketed!(kind_input in input);

        let kind = kind_input.parse::<DiagnosticKind>()?;

        let inner = input.parse::<DiagnosticMessageVariant>()?;

        Ok(Self { kind, inner })
    }
}

pub struct Diagnostic {
    pub variants: Vec<DiagnosticVariant>,
}

impl Diagnostic {
    fn diagnostic(variants: Vec<(usize, &DiagnosticVariant)>) -> TokenStream {
        Diagnostic::enum_definition(format_ident!("Diagnostic"), &variants)
    }

    fn static_diagnostic(variants: Vec<(usize, &DiagnosticVariant)>) -> TokenStream {
        Diagnostic::enum_definition(format_ident!("StaticDiagnostic"), &variants)
    }

    fn enum_definition(name: Ident, variants: &[(usize, &DiagnosticVariant)]) -> TokenStream {
        let enum_variants = variants.iter().map(|(_, variant)| variant.inner.enum_variant());

        let constructor_functions = variants.iter().map(|(_, variant)| {
            let name = &variant.inner.name;
            let fn_name = format_ident!("{}", name.to_string().to_case(Case::Snake));

            let arguments = variant.inner.fields.iter().map(|field| field.function_argument());

            let into_assignments = variant.inner.fields.iter().filter(|field| field.is_into).map(|field| {
                let name = &field.name;

                quote! {
                    let #name = #name.into();
                }
            });

            let field_names = variant.inner.fields.iter().map(|field| &field.name);

            quote! {
                pub fn #fn_name(#(#arguments),*) -> Self {
                    #(
                        #into_assignments
                    )*

                    Self::#name {
                        #(#field_names),*
                    }
                }
            }
        });

        let match_value = if variants.is_empty() {
            quote! { *self }
        } else {
            quote! { self }
        };

        let match_patterns = variants
            .iter()
            .map(|(i, variant)| {
                let pattern = variant.inner.match_pattern(quote! { Self }, true);

                (*i, *variant, pattern)
            })
            .collect::<Vec<_>>();

        let diagnostic_kind = quote! {
            ::juice_core::diag::DiagnosticKind
        };

        let diagnostic_code = quote! {
            ::juice_core::diag::DiagnosticCode
        };

        let error_code_match_arms = match_patterns.iter().map(|(i, variant, pattern)| {
            let kind = match variant.kind {
                DiagnosticKind::Error | DiagnosticKind::StaticError => quote! { #diagnostic_kind::Error },
                DiagnosticKind::Warning | DiagnosticKind::StaticWarning => quote! { #diagnostic_kind::Warning },
            };

            let i = *i as u32;

            quote! {
                #pattern => #diagnostic_code::new(#kind, #i)
            }
        });

        let diagnostic_kind_match_arms = match_patterns.iter().map(|(_, variant, pattern)| {
            let kind = match variant.kind {
                DiagnosticKind::Error | DiagnosticKind::StaticError => quote! { #diagnostic_kind::Error },
                DiagnosticKind::Warning | DiagnosticKind::StaticWarning => quote! { #diagnostic_kind::Warning },
            };

            quote! {
                #pattern => #kind
            }
        });

        let message_match_arms = variants.iter().map(|(_, variant)| {
            let pattern = variant.inner.match_pattern(quote! { Self }, false);

            let format = &variant.inner.format;

            let formatted_message = if variant.inner.fields.is_empty() {
                quote! { #format.to_string() }
            } else {
                let colored_fields = variant.inner.fields.iter().map(|field| {
                    let name = &field.name;

                    quote! {
                        ::juice_core::diag::DiagnosticArg::with_color(#name, color)
                    }
                });

                quote! {
                    format!(#format, #(#colored_fields),*)
                }
            };

            quote! {
                #pattern => #formatted_message
            }
        });

        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #(
                    #enum_variants,
                )*
            }

            impl #name {
                #(
                    #constructor_functions
                )*

                pub fn get_code(&self) -> #diagnostic_code {
                    match #match_value {
                        #(
                            #error_code_match_arms,
                        )*
                    }
                }

                pub fn get_kind(&self) -> #diagnostic_kind {
                    match #match_value {
                        #(
                            #diagnostic_kind_match_arms,
                        )*
                    }
                }

                pub fn into_formatted_message(self, color: impl Into<Option<::ariadne::Color>>) -> String {
                    let color: Option<::ariadne::Color> = color.into();

                    match self {
                        #(
                            #message_match_arms,
                        )*
                    }
                }
            }
        }
    }
}

impl Parse for Diagnostic {
    fn parse(input: ParseStream) -> Result<Self> {
        let variants = Punctuated::<DiagnosticVariant, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();

        Ok(Self { variants })
    }
}

impl ToTokens for Diagnostic {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (variants, static_variants) = self.variants.iter().enumerate().partition::<Vec<_>, _>(|(_, variant)| {
            matches!(variant.kind, DiagnosticKind::Error | DiagnosticKind::Warning)
        });

        let diagnostic = Diagnostic::diagnostic(variants);
        let static_diagnostic = Diagnostic::static_diagnostic(static_variants);

        tokens.extend([diagnostic, static_diagnostic]);
    }
}

pub struct DiagnosticNote {
    pub enum_name: Ident,
    pub variants: Vec<DiagnosticMessageVariant>,
}

impl DiagnosticNote {
    fn enum_definition(&self) -> TokenStream {
        let name = &self.enum_name;

        let enum_variants = self.variants.iter().map(|variant| variant.enum_variant());

        let constructor_functions = self.variants.iter().map(|variant| {
            let name = &variant.name;
            let fn_name = format_ident!("{}", name.to_string().to_case(Case::Snake));

            let arguments = variant.fields.iter().map(|field| field.function_argument());

            let into_assignments = variant.fields.iter().filter(|field| field.is_into).map(|field| {
                let name = &field.name;

                quote! {
                    let #name = #name.into();
                }
            });

            let field_names = variant.fields.iter().map(|field| &field.name);

            quote! {
                pub fn #fn_name(#(#arguments),*) -> Self {
                    #(
                        #into_assignments
                    )*

                    Self::#name {
                        #(#field_names),*
                    }
                }
            }
        });

        let message_match_arms = self.variants.iter().map(|variant| {
            let pattern = variant.match_pattern(quote! { Self }, false);

            let format = &variant.format;

            let formatted_message = if variant.fields.is_empty() {
                quote! { #format.to_string() }
            } else {
                let colored_fields = variant.fields.iter().map(|field| {
                    let name = &field.name;

                    quote! {
                        ::juice_core::diag::DiagnosticArg::with_color(#name, color)
                    }
                });

                quote! {
                    format!(#format, #(#colored_fields),*)
                }
            };

            quote! {
                #pattern => #formatted_message
            }
        });

        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #(
                    #enum_variants,
                )*
            }

            impl #name {
                #(
                    #constructor_functions
                )*

                pub fn into_formatted_message(self, color: impl Into<Option<::ariadne::Color>>) -> String {
                    let color: Option<::ariadne::Color> = color.into();

                    match self {
                        #(
                            #message_match_arms,
                        )*
                    }
                }
            }
        }
    }
}

impl Parse for DiagnosticNote {
    fn parse(input: ParseStream) -> Result<Self> {
        let enum_name = input.parse::<Ident>()?;

        input.parse::<Token![;]>()?;

        let variants = Punctuated::<DiagnosticMessageVariant, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();

        Ok(Self { enum_name, variants })
    }
}

impl ToTokens for DiagnosticNote {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let definition = self.enum_definition();

        tokens.extend(definition);
    }
}
