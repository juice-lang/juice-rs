use std::iter::Peekable;

use convert_case::{Case, Casing as _};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, LitStr, Result, Token,
};

use crate::{enum_def::EnumDefinition, Error};

pub struct StringEnumVariant {
    string: String,
    variant_name: Ident,
    span: Span,
}

impl Parse for StringEnumVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let variant_name = input.parse::<Ident>()?;

        let (string, span) = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            let string = input.parse::<LitStr>()?;
            (string.value(), string.span())
        } else {
            (
                variant_name.to_string().trim_start_matches("r#").to_case(Case::Snake),
                variant_name.span(),
            )
        };

        Ok(Self {
            string,
            variant_name,
            span,
        })
    }
}

pub struct StringEnum {
    definition: EnumDefinition,
    variant_strings: Vec<(String, Ident, Span)>,
}

impl StringEnum {
    fn enum_definition(&self) -> TokenStream {
        let definition = &self.definition;

        let variants = self.variant_strings.iter().map(|(_, variant_name, _)| variant_name);

        quote! {
            #definition {
                #(#variants),*
            }
        }
    }

    fn impl_display(&self) -> TokenStream {
        let name = &self.definition.name;

        if self.variant_strings.is_empty() {
            quote! {
                #[automatically_derived]
                impl ::core::fmt::Display for #name {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        match *self {}
                    }
                }
            }
        } else {
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
    }

    fn impl_from_str(&self) -> TokenStream {
        let name = &self.definition.name;

        let variant_strings = self
            .variant_strings
            .iter()
            .map(|(string, variant_name, _)| (string.as_bytes(), variant_name))
            .collect::<Vec<_>>();

        let mut variant_strings = variant_strings.into_iter();
        let variant_strings: &mut dyn ExactSizeIterator<Item = _> = &mut variant_strings;

        let match_variants = StringEnum::match_variants(variant_strings.peekable(), 0);

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

            let match_variants = StringEnum::match_variants(variant_strings, depth);

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

                let match_variants = StringEnum::match_variants(variant_strings.peekable(), depth + 1);

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

impl Parse for StringEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let definition = input.parse::<EnumDefinition>()?;

        if definition.lifetime.is_some() {
            return Err(syn::Error::new_spanned(
                definition.lifetime.unwrap().1,
                Error::EnumCannotHaveLifetime,
            ));
        }

        let braced_input;
        braced!(braced_input in input);

        let mut variant_strings = Punctuated::<StringEnumVariant, Token![,]>::parse_terminated(&braced_input)?
            .into_iter()
            .map(|variant| (variant.string, variant.variant_name, variant.span))
            .collect::<Vec<_>>();

        variant_strings.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));

        let mut errors = Vec::new();

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
                definition,
                variant_strings,
            })
        }
    }
}

impl ToTokens for StringEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend([self.enum_definition(), self.impl_from_str(), self.impl_display()]);
    }
}
