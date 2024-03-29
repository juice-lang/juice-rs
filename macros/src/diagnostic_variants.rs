use convert_case::{Case, Casing as _};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    braced, bracketed, custom_keyword, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Expr, Ident, LitStr, Result, Token, Type,
};

use crate::{enum_def::EnumDefinition, Error};

mod keyword {
    use super::*;

    custom_keyword!(color);
    custom_keyword!(error);
    custom_keyword!(into);
    custom_keyword!(warning);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

impl Parse for DiagnosticKind {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(keyword::error) {
            input.parse::<keyword::error>()?;

            Ok(Self::Error)
        } else if input.peek(keyword::warning) {
            input.parse::<keyword::warning>()?;

            Ok(Self::Warning)
        } else {
            Err(input.error(Error::ExpectedDiagnosticKind))
        }
    }
}

impl ToTokens for DiagnosticKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let diagnostic_kind = quote! {
            ::juice_core::diag::DiagnosticKind
        };

        let kind = match self {
            Self::Error => quote! { #diagnostic_kind::Error },
            Self::Warning => quote! { #diagnostic_kind::Warning },
        };

        tokens.extend(kind);
    }
}

pub struct DiagnosticField {
    pub name: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub is_into: bool,
}

impl DiagnosticField {
    pub fn function_argument(&self) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;

        let ty = if self.is_into {
            quote! { impl ::core::convert::Into<#ty> }
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

        let default = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self {
            name,
            ty,
            default,
            is_into,
        })
    }
}

pub struct DiagnosticMessageVariant {
    pub name: Ident,
    pub fields: Vec<(usize, DiagnosticField)>,
    pub default_fields: Vec<(usize, DiagnosticField)>,
    pub format: LitStr,
}

impl DiagnosticMessageVariant {
    pub fn enum_variant(&self) -> TokenStream {
        let name = &self.name;
        let fields = if self.fields.is_empty() {
            None
        } else {
            let fields = self.fields.iter().map(|(_, field)| {
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
                let names = self.fields.iter().map(|(_, field)| &field.name);

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

        let (fields, default_fields) = fields
            .into_iter()
            .enumerate()
            .partition(|(_, field)| field.default.is_none());

        Ok(Self {
            name,
            fields,
            default_fields,
            format,
        })
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
    pub definition: EnumDefinition,
    pub variants: Vec<DiagnosticVariant>,
}

impl Diagnostic {
    pub fn enum_definition(&self, start_index: usize) -> TokenStream {
        let name = &self.definition.name;
        let definition = &self.definition;

        let generic_parameters = self.definition.generic_parameters();

        let enum_variants = self.variants.iter().map(|variant| variant.inner.enum_variant());

        let constructor_functions = self.variants.iter().map(|variant| {
            let name = &variant.inner.name;
            let fn_name = Ident::new(&name.to_string().to_case(Case::Snake), name.span());

            let arguments = variant.inner.fields.iter().map(|(_, field)| field.function_argument());

            let into_assignments = variant
                .inner
                .fields
                .iter()
                .filter(|(_, field)| field.is_into)
                .map(|(_, field)| {
                    let name = &field.name;

                    quote! {
                        let #name = #name.into();
                    }
                });

            let fields = if variant.inner.fields.is_empty() {
                None
            } else {
                let field_names = variant.inner.fields.iter().map(|(_, field)| &field.name);

                Some(quote! {
                    {
                        #(#field_names),*
                    }
                })
            };

            quote! {
                pub fn #fn_name(#(#arguments),*) -> Self {
                    #(
                        #into_assignments
                    )*

                    Self::#name #fields
                }
            }
        });

        let match_value = if self.variants.is_empty() && self.definition.lifetime.is_none() {
            quote! { *self }
        } else {
            quote! { self }
        };

        let match_patterns = self
            .variants
            .iter()
            .map(|variant| {
                let pattern = variant.inner.match_pattern(quote! { Self }, true);

                (variant, pattern)
            })
            .collect::<Vec<_>>();

        let diagnostic_code = quote! {
            ::juice_core::diag::DiagnosticCode
        };

        let error_code_match_arms = match_patterns.iter().enumerate().map(|(i, (variant, pattern))| {
            let kind = variant.kind;

            let i = (i + start_index) as u32;

            quote! {
                #pattern => #diagnostic_code::new(#kind, #i)
            }
        });

        let diagnostic_kind_match_arms = match_patterns.iter().map(|(variant, pattern)| {
            let kind = variant.kind;

            quote! {
                #pattern => #kind
            }
        });

        let message_match_arms = self.variants.iter().map(|variant| {
            let pattern = variant.inner.match_pattern(quote! { Self }, false);

            let format = &variant.inner.format;

            let formatted_message = if variant.inner.fields.is_empty() && variant.inner.default_fields.is_empty() {
                quote! { #format.to_string() }
            } else {
                let fields = variant.inner.fields.iter();
                let default_fields = variant.inner.default_fields.iter();
                let colored_fields = fields.merge_by(default_fields, |a, b| a.0 < b.0).map(|(_, field)| {
                    let name = &field.name;

                    let value = if let Some(default) = field.default.as_ref() {
                        if field.is_into {
                            let ty = &field.ty;
                            quote! {
                                <#ty>::from(#default)
                            }
                        } else {
                            quote! { #default }
                        }
                    } else {
                        quote! { #name }
                    };

                    quote! {
                        ::juice_core::diag::DiagnosticArg::with_color(#value, color)
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

        let unreachable_variant = self.definition.lifetime.as_ref().map(|(_, lifetime, _)| {
            quote! {
                _Unreachable(::core::convert::Infallible, ::core::marker::PhantomData<&#lifetime ()>),
            }
        });

        let unreachable_match_arm = self.definition.lifetime.as_ref().and(Some(quote! {
            Self::_Unreachable(_, _) => unreachable!(),
        }));

        quote! {
            #definition {
                #(
                    #enum_variants,
                )*
                #unreachable_variant
            }

            #[automatically_derived]
            impl #generic_parameters #name #generic_parameters {
                #(
                    #constructor_functions
                )*

                pub fn get_code(&self) -> #diagnostic_code {
                    match #match_value {
                        #(
                            #error_code_match_arms,
                        )*
                        #unreachable_match_arm
                    }
                }

                pub fn get_kind(&self) -> ::juice_core::diag::DiagnosticKind {
                    match #match_value {
                        #(
                            #diagnostic_kind_match_arms,
                        )*
                        #unreachable_match_arm
                    }
                }

                pub fn into_formatted_message(
                    self,
                    color: impl ::core::convert::Into<::core::option::Option<::ariadne::Color>>
                ) -> String {
                    let color: Option<::ariadne::Color> = color.into();

                    match self {
                        #(
                            #message_match_arms,
                        )*
                        #unreachable_match_arm
                    }
                }
            }
        }
    }
}

impl Parse for Diagnostic {
    fn parse(input: ParseStream) -> Result<Self> {
        let definition = input.parse::<EnumDefinition>()?;

        let braced_input;
        braced!(braced_input in input);

        let variants = Punctuated::<DiagnosticVariant, Token![,]>::parse_terminated(&braced_input)?
            .into_iter()
            .collect();

        Ok(Self { definition, variants })
    }
}

pub struct Diagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

impl Parse for Diagnostics {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut diagnostics = Vec::new();

        while input.peek(Token![enum]) || input.peek(Token![pub]) || input.peek(Token![#]) {
            diagnostics.push(input.parse()?);
        }

        Ok(Self { diagnostics })
    }
}

impl ToTokens for Diagnostics {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.diagnostics.iter().fold(0, |index, diagnostic| {
            let definition = diagnostic.enum_definition(index);

            tokens.extend(definition);

            index + diagnostic.variants.len()
        });
    }
}

pub struct DiagnosticNote {
    pub definition: EnumDefinition,
    pub variants: Vec<DiagnosticMessageVariant>,
}

impl DiagnosticNote {
    fn enum_definition(&self) -> TokenStream {
        let name = &self.definition.name;
        let definition = &self.definition;

        let generic_parameters = self.definition.generic_parameters();

        let enum_variants = self.variants.iter().map(|variant| variant.enum_variant());

        let constructor_functions = self.variants.iter().map(|variant| {
            let name = &variant.name;
            let fn_name = Ident::new(&name.to_string().to_case(Case::Snake), name.span());

            let arguments = variant.fields.iter().map(|(_, field)| field.function_argument());

            let into_assignments = variant
                .fields
                .iter()
                .filter(|(_, field)| field.is_into)
                .map(|(_, field)| {
                    let name = &field.name;

                    quote! {
                        let #name = #name.into();
                    }
                });

            let fields = if variant.fields.is_empty() {
                None
            } else {
                let field_names = variant.fields.iter().map(|(_, field)| &field.name);

                Some(quote! {
                    {
                        #(#field_names),*
                    }
                })
            };

            quote! {
                pub fn #fn_name(#(#arguments),*) -> Self {
                    #(
                        #into_assignments
                    )*

                    Self::#name #fields
                }
            }
        });

        let message_match_arms = self.variants.iter().map(|variant| {
            let pattern = variant.match_pattern(quote! { Self }, false);

            let format = &variant.format;

            let formatted_message = if variant.fields.is_empty() && variant.default_fields.is_empty() {
                quote! { #format.to_string() }
            } else {
                let fields = variant.fields.iter();
                let default_fields = variant.default_fields.iter();
                let colored_fields = fields.merge_by(default_fields, |a, b| a.0 < b.0).map(|(_, field)| {
                    let name = &field.name;

                    let value = if let Some(default) = field.default.as_ref() {
                        if field.is_into {
                            let ty = &field.ty;
                            quote! {
                                <#ty>::from(#default)
                            }
                        } else {
                            quote! { #default }
                        }
                    } else {
                        quote! { #name }
                    };

                    quote! {
                        ::juice_core::diag::DiagnosticArg::with_color(#value, color)
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

        let unreachable_variant = self.definition.lifetime.as_ref().map(|(_, lifetime, _)| {
            quote! {
                _Unreachable(::core::convert::Infallible, ::core::marker::PhantomData<&#lifetime ()>),
            }
        });

        let unreachable_match_arm = self.definition.lifetime.as_ref().and(Some(quote! {
            Self::_Unreachable(_, _) => unreachable!(),
        }));

        quote! {
            #definition {
                #(
                    #enum_variants,
                )*
                #unreachable_variant
            }

            #[automatically_derived]
            impl #generic_parameters #name #generic_parameters {
                #(
                    #constructor_functions
                )*

                pub fn into_formatted_message(
                    self,
                    color: impl ::core::convert::Into<::core::option::Option<::ariadne::Color>>
                ) -> String {
                    let color: Option<::ariadne::Color> = color.into();

                    match self {
                        #(
                            #message_match_arms,
                        )*
                        #unreachable_match_arm
                    }
                }
            }
        }
    }
}

impl Parse for DiagnosticNote {
    fn parse(input: ParseStream) -> Result<Self> {
        let definition = input.parse::<EnumDefinition>()?;

        let braced_input;
        braced!(braced_input in input);

        let variants = Punctuated::<DiagnosticMessageVariant, Token![,]>::parse_terminated(&braced_input)?
            .into_iter()
            .collect();

        Ok(Self { definition, variants })
    }
}

impl ToTokens for DiagnosticNote {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let definition = self.enum_definition();

        tokens.extend(definition);
    }
}
