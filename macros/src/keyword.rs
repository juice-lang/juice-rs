use std::iter::Peekable;

use convert_case::{Case, Casing as _};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Data, DataStruct, DataUnion, DeriveInput, Fields, Ident, LitStr, Result,
};

use crate::Error;

pub struct KeywordEnum {
    name: Ident,
    variant_strings: Vec<(String, Ident, Span)>,
}

impl KeywordEnum {
    fn impl_display(&self) -> TokenStream {
        let name = &self.name;

        let match_arms = self.variant_strings.iter().map(|(string, variant_name, span)| {
            let string = quote_spanned! { *span => #string };

            quote! {
                Self::#variant_name => #string
            }
        });

        quote! {
            #[automatically_derived]
            impl ::core::fmt::Display for #name {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    write!(f, "{}", match self {
                        #(#match_arms),*
                    })
                }
            }
        }
    }

    fn impl_from_str(&self) -> TokenStream {
        let name = &self.name;

        let variant_strings = self
            .variant_strings
            .iter()
            .map(|(string, variant_name, _)| (string.as_bytes(), variant_name))
            .collect::<Vec<_>>();

        let mut variant_strings = variant_strings.into_iter();
        let variant_strings: &mut dyn ExactSizeIterator<Item = _> = &mut variant_strings;

        let match_variants = KeywordEnum::match_variants(variant_strings.peekable(), 0);

        quote! {
            #[automatically_derived]
            impl ::core::str::FromStr for #name {
                type Err = ();

                #[inline]
                fn from_str(value: &str) -> ::core::result::Result<Self, Self::Err> {
                    let bytes = value.as_bytes();
                    let len = value.len();

                    #match_variants
                }
            }
        }
    }

    fn match_variants<'a>(
        mut variant_strings: Peekable<&mut dyn ExactSizeIterator<Item = (&'a [u8], &'a Ident)>>,
        depth: usize,
    ) -> TokenStream {
        if let Some(first) = variant_strings.next_if(|(string, _)| string.is_empty()) {
            let variant_name = first.1;

            let match_variants = KeywordEnum::match_variants(variant_strings, depth);

            quote! {
                if len == #depth {
                    Ok(Self::#variant_name)
                } else {
                    #match_variants
                }
            }
        } else if variant_strings.len() == 1 {
            let (string, variant_name) = variant_strings.next().unwrap();

            quote! {
                if bytes[#depth..] == [#(#string),*] {
                    Ok(Self::#variant_name)
                } else {
                    Err(())
                }
            }
        } else if variant_strings.len() > 1 {
            let variant_strings = variant_strings
                .map(|(string, variant_name)| {
                    let (first, rest) = string.split_first().expect("empty strings should not arrive here");

                    (*first, rest, variant_name)
                })
                .collect::<Vec<_>>();

            let match_arms = variant_strings.chunk_by(|(a, _, _), (b, _, _)| a == b).map(|chunk| {
                let (byte, _, _) = chunk[0];

                let mut variant_strings = chunk.iter().map(|(_, rest, variant_name)| (*rest, *variant_name));
                let variant_strings: &mut dyn ExactSizeIterator<Item = _> = &mut variant_strings;

                let match_variants = KeywordEnum::match_variants(variant_strings.peekable(), depth + 1);

                quote! {
                    #byte => #match_variants
                }
            });

            quote! {
                if len == #depth {
                    Err(())
                } else {
                    match bytes[#depth] {
                        #(#match_arms),*
                        _ => Err(())
                    }
                }
            }
        } else {
            return quote! { Err(()) };
        }
    }
}

impl Parse for KeywordEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let input: DeriveInput = input.parse()?;

        let data = match input.data {
            Data::Enum(data) => data,
            Data::Struct(DataStruct { struct_token, .. }) => {
                return Err(syn::Error::new_spanned(struct_token, Error::NotAnEnum))
            }
            Data::Union(DataUnion { union_token, .. }) => {
                return Err(syn::Error::new_spanned(union_token, Error::NotAnEnum))
            }
        };

        let mut errors = Vec::new();

        let mut variant_strings = data
            .variants
            .into_iter()
            .filter_map(|variant| {
                let variant_span = variant.span();
                let variant_name = variant.ident;

                let string = variant.attrs.iter().find_map(|attr| {
                    if attr.path().is_ident("kw") {
                        match attr.parse_args::<LitStr>() {
                            Ok(string) => Some(string),
                            Err(err) => {
                                errors.push(err);
                                None
                            }
                        }
                    } else {
                        None
                    }
                });

                let (string, span) = if let Some(string) = string {
                    (string.value(), string.span())
                } else {
                    (
                        variant_name.to_string().trim_start_matches("r#").to_case(Case::Snake),
                        variant_name.span(),
                    )
                };

                match variant.fields {
                    Fields::Unit => Some((string, variant_name, span)),
                    _ => {
                        errors.push(syn::Error::new(variant_span, Error::NotAUnitVariant));
                        None
                    }
                }
            })
            .collect::<Vec<_>>();

        variant_strings.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));

        let mut last_was_duplicate = false;
        for [(a, _, span_a), (b, _, span_b)] in variant_strings.array_windows() {
            if a == b {
                if !last_was_duplicate {
                    errors.push(syn::Error::new(*span_a, Error::DuplicateVariantString));
                }

                errors.push(syn::Error::new(*span_b, Error::DuplicateVariantString));

                last_was_duplicate = true;
            } else {
                last_was_duplicate = false;
            }
        }

        if let Some(mut error) = errors.pop() {
            error.extend(errors);
            Err(error)
        } else {
            Ok(Self {
                name: input.ident,
                variant_strings,
            })
        }
    }
}

impl ToTokens for KeywordEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.impl_from_str());
        tokens.extend(self.impl_display());
    }
}
