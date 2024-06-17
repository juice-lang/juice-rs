use convert_case::{Case, Casing as _};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Data, DataEnum, DataStruct, DataUnion, DeriveInput, Expr, Field, Fields, GenericParam, Generics, Ident, LitInt,
    LitStr, Result, Type, Variant,
};

use crate::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticKind {
    Error,
    Warning,
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

struct DiagnosticField {
    name: Ident,
    ty: Type,
    default: Option<Expr>,
    is_into: bool,
}

impl DiagnosticField {
    fn from_field(index: usize, field: Field) -> Result<Self> {
        let name = field.ident.unwrap_or_else(|| format_ident!("field{}", index));

        if name == "color" {
            return Err(syn::Error::new_spanned(name, Error::FieldNamedColor));
        }

        let ty = field.ty;

        let mut default = None;
        let mut is_into = None;
        for attr in &field.attrs {
            if attr.path().is_ident("diag") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("default") {
                        if default.replace(meta.value()?.parse()?).is_some() {
                            return Err(meta.error(Error::DuplicateAttr("default")));
                        }
                    } else if meta.path.is_ident("into") {
                        if is_into.replace(true).is_some() {
                            return Err(meta.error(Error::DuplicateAttr("into")));
                        }
                    } else {
                        let path = meta.path.to_token_stream().to_string();
                        return Err(meta.error(Error::UnknownAttribute(path)));
                    }

                    Ok(())
                })?;
            }
        }

        let is_into = is_into.unwrap_or_default();

        Ok(Self {
            name,
            ty,
            default,
            is_into,
        })
    }

    fn function_argument(&self) -> TokenStream {
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

trait VariantTrait: Sized {
    fn from_variant(variant: Variant) -> Result<Self>;
}

struct DiagnosticMessageVariant {
    format: Option<LitStr>,
    name: Ident,
    fields_named: Option<bool>,
    fields: Vec<DiagnosticField>,
}

impl DiagnosticMessageVariant {
    fn is_reachable(&self) -> bool {
        self.format.is_some()
    }

    fn from_variant_impl(format: Option<LitStr>, name: Ident, fields: Fields) -> Result<Self> {
        let fields_named = match &fields {
            Fields::Named(_) => Some(true),
            Fields::Unnamed(_) => Some(false),
            Fields::Unit => None,
        };

        let fields = fields
            .into_iter()
            .enumerate()
            .map(|(i, field)| DiagnosticField::from_field(i, field))
            .collect::<Result<Vec<_>>>()?;

        if let Some(format) = &format {
            if fields.is_empty() && format.value().contains("{}") {
                return Err(syn::Error::new(format.span(), Error::NoFieldsWithFormat));
            }
        }

        Ok(Self {
            format,
            name,
            fields_named,
            fields,
        })
    }

    fn match_pattern(&self, variant_base: TokenStream, ignore_fields: bool) -> TokenStream {
        let name = &self.name;
        let pattern_base = quote! {
            #variant_base::#name
        };

        let pattern_fields = if let Some(fields_named) = self.fields_named {
            let inner = if ignore_fields {
                quote! { .. }
            } else {
                let names = self.fields.iter().map(|field| &field.name);

                quote! { #(#names),* }
            };

            Some(if fields_named {
                quote! { { #inner } }
            } else {
                quote! { (#inner) }
            })
        } else {
            None
        };

        quote! {
            #pattern_base #pattern_fields
        }
    }
}

impl VariantTrait for DiagnosticMessageVariant {
    fn from_variant(variant: Variant) -> Result<Self> {
        let span = variant.span();

        let Variant {
            attrs, ident, fields, ..
        } = variant;

        let mut format = None;
        let mut unreachable = false;
        for attr in &attrs {
            if attr.path().is_ident("diag") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("note") {
                        if format.replace(meta.value()?.parse()?).is_some() {
                            return Err(meta.error(Error::DuplicateAttr("note")));
                        }
                    } else if meta.path.is_ident("unreachable") {
                        if unreachable {
                            return Err(meta.error(Error::DuplicateAttr("unreachable")));
                        }

                        unreachable = true;
                    } else {
                        let path = meta.path.to_token_stream().to_string();
                        return Err(meta.error(Error::UnknownAttribute(path)));
                    }

                    Ok(())
                })?;
            }
        }

        if unreachable {
            if format.is_some() {
                return Err(syn::Error::new_spanned(format, Error::UnreachableWithFormat));
            }
        } else if format.is_none() {
            return Err(syn::Error::new(span, Error::ExpectedNoteAttr));
        }

        Self::from_variant_impl(format, ident, fields)
    }
}

struct DiagnosticVariant {
    kind: DiagnosticKind,
    inner: DiagnosticMessageVariant,
}

impl DiagnosticVariant {
    fn is_reachable(&self) -> bool {
        self.inner.is_reachable()
    }
}

impl VariantTrait for DiagnosticVariant {
    fn from_variant(variant: Variant) -> Result<Self> {
        let span = variant.span();

        let Variant {
            attrs, ident, fields, ..
        } = variant;

        let mut kind = DiagnosticKind::Error;
        let mut format = None;
        let mut unreachable = false;
        for attr in &attrs {
            if attr.path().is_ident("diag") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("error") {
                        if format.replace(meta.value()?.parse()?).is_some() {
                            return Err(meta.error(Error::DuplicateDiagnosticKindAttr));
                        }
                    } else if meta.path.is_ident("warning") {
                        kind = DiagnosticKind::Warning;
                        if format.replace(meta.value()?.parse()?).is_some() {
                            return Err(meta.error(Error::DuplicateDiagnosticKindAttr));
                        }
                    } else if meta.path.is_ident("unreachable") {
                        if unreachable {
                            return Err(meta.error(Error::DuplicateAttr("unreachable")));
                        }

                        unreachable = true;
                    } else {
                        let path = meta.path.to_token_stream().to_string();
                        return Err(meta.error(Error::UnknownAttribute(path)));
                    }

                    Ok(())
                })?;
            }
        }

        if unreachable {
            if format.is_some() {
                return Err(syn::Error::new_spanned(format, Error::UnreachableWithFormat));
            }
        } else if format.is_none() {
            return Err(syn::Error::new(span, Error::ExpectedDiagnosticKindAttr));
        }

        Ok(Self {
            kind,
            inner: DiagnosticMessageVariant::from_variant_impl(format, ident, fields)?,
        })
    }
}

struct EnumData<V> {
    code_offset: Option<(usize, Span)>,
    ident: Ident,
    generics: Generics,
    variants: Vec<V>,
}

impl<V> EnumData<V> {
    fn generics_stream(&self) -> TokenStream {
        let lt_token = &self.generics.lt_token;
        let gt_token = &self.generics.gt_token;

        let params = &self.generics.params;

        quote! {
            #lt_token #params #gt_token
        }
    }

    fn stripped_generics_stream(&self) -> TokenStream {
        let lt_token = &self.generics.lt_token;
        let gt_token = &self.generics.gt_token;

        let params = self.generics.params.iter().map(|param| match param {
            GenericParam::Type(param) => {
                let ident = &param.ident;
                quote! { #ident }
            }
            GenericParam::Lifetime(param) => {
                let lifetime = &param.lifetime;
                quote! { #lifetime }
            }
            GenericParam::Const(param) => {
                let ident = &param.ident;

                quote! { #ident }
            }
        });

        quote! {
            #lt_token #(#params),* #gt_token
        }
    }
}

impl<V: VariantTrait> Parse for EnumData<V> {
    fn parse(input: ParseStream) -> Result<Self> {
        let DeriveInput {
            attrs,
            ident,
            generics,
            data,
            ..
        } = input.parse::<DeriveInput>()?;

        let variants = match data {
            Data::Enum(DataEnum { variants, .. }) => variants,
            Data::Struct(DataStruct { struct_token, .. }) => {
                return Err(syn::Error::new_spanned(struct_token, Error::NotAnEnum));
            }
            Data::Union(DataUnion { union_token, .. }) => {
                return Err(syn::Error::new_spanned(union_token, Error::NotAnEnum));
            }
        };

        let variants = variants.into_iter().map(V::from_variant).collect::<Result<_>>()?;

        let mut code_offset = None;
        for attr in &attrs {
            if attr.path().is_ident("diag") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("offset") {
                        if code_offset
                            .replace((meta.value()?.parse::<LitInt>()?.base10_parse()?, attr.span()))
                            .is_some()
                        {
                            return Err(meta.error(Error::DuplicateAttr("offset")));
                        }
                    } else {
                        let path = meta.path.to_token_stream().to_string();
                        return Err(meta.error(Error::UnknownAttribute(path)));
                    }

                    Ok(())
                })?;
            }
        }

        Ok(Self {
            code_offset,
            ident,
            generics,
            variants,
        })
    }
}

pub struct Diagnostic(EnumData<DiagnosticVariant>);

impl Diagnostic {
    fn impl_block(&self) -> TokenStream {
        let code_offset = self.0.code_offset.map(|(offset, _)| offset).unwrap_or_default();
        let name = &self.0.ident;

        let generic_parameters = self.0.generics_stream();
        let stripped_generic_parameters = self.0.stripped_generics_stream();
        let where_clause = &self.0.generics.where_clause;

        let constructor_functions = self
            .0
            .variants
            .iter()
            .filter(|variant| variant.is_reachable())
            .map(|variant| {
                let variant = &variant.inner;
                let name = &variant.name;
                let fn_name = Ident::new(&name.to_string().to_case(Case::Snake), name.span());

                let (fields, default_fields) = variant
                    .fields
                    .iter()
                    .partition::<Vec<_>, _>(|field| field.default.is_none());

                let arguments = fields.iter().map(|field| field.function_argument());

                let default_assignments = default_fields.iter().map(|field| {
                    let name = &field.name;
                    let default = field.default.as_ref().unwrap();

                    quote! {
                        let #name = #default;
                    }
                });

                let into_assignments = variant.fields.iter().filter(|field| field.is_into).map(|field| {
                    let name = &field.name;

                    quote! {
                        let #name = #name.into();
                    }
                });

                let fields = if let Some(fields_named) = variant.fields_named {
                    let field_names = variant.fields.iter().map(|field| &field.name);

                    Some(if fields_named {
                        quote! {
                            {
                                #(#field_names),*
                            }
                        }
                    } else {
                        quote! { (#(#field_names),*) }
                    })
                } else {
                    None
                };

                quote! {
                    pub fn #fn_name(#(#arguments),*) -> Self {
                        #(
                            #default_assignments
                        )*

                        #(
                            #into_assignments
                        )*

                        Self::#name #fields
                    }
                }
            });

        let match_value = if self.0.variants.is_empty() {
            quote! { *self }
        } else {
            quote! { self }
        };

        let match_patterns = self
            .0
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

        let unreachable = quote! {
            ::core::unreachable!()
        };

        let error_code_match_arms = match_patterns.iter().enumerate().map(|(i, (variant, pattern))| {
            let kind = variant.kind;

            let i = (i + code_offset) as u32;

            if variant.is_reachable() {
                quote! {
                    #pattern => #diagnostic_code::new(#kind, #i)
                }
            } else {
                quote! {
                    #pattern => #unreachable
                }
            }
        });

        let diagnostic_kind_match_arms = match_patterns.iter().map(|(variant, pattern)| {
            let kind = variant.kind;

            if variant.is_reachable() {
                quote! {
                    #pattern => #kind
                }
            } else {
                quote! {
                    #pattern => #unreachable
                }
            }
        });

        let message_match_arms = self.0.variants.iter().map(|variant| {
            let variant = &variant.inner;
            let pattern = variant.match_pattern(quote! { Self }, !variant.is_reachable());

            if let Some(format) = &variant.format {
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
                        ::std::format!(#format, #(#colored_fields),*)
                    }
                };

                quote! {
                    #pattern => #formatted_message
                }
            } else {
                quote! {
                    #pattern => #unreachable
                }
            }
        });

        quote! {
            impl #generic_parameters #name #stripped_generic_parameters #where_clause {
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

                pub fn get_kind(&self) -> ::juice_core::diag::DiagnosticKind {
                    match #match_value {
                        #(
                            #diagnostic_kind_match_arms,
                        )*
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
                    }
                }
            }
        }
    }
}

impl Parse for Diagnostic {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Self)
    }
}

impl ToTokens for Diagnostic {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.impl_block());
    }
}

pub struct DiagnosticNote(EnumData<DiagnosticMessageVariant>);

impl DiagnosticNote {
    fn impl_block(&self) -> TokenStream {
        let name = &self.0.ident;

        let generic_parameters = self.0.generics_stream();
        let stripped_generic_parameters = self.0.stripped_generics_stream();
        let where_clause = &self.0.generics.where_clause;

        let constructor_functions = self
            .0
            .variants
            .iter()
            .filter(|variant| variant.is_reachable())
            .map(|variant| {
                let name = &variant.name;
                let fn_name = Ident::new(&name.to_string().to_case(Case::Snake), name.span());

                let (fields, default_fields) = variant
                    .fields
                    .iter()
                    .partition::<Vec<_>, _>(|field| field.default.is_none());

                let arguments = fields.iter().map(|field| field.function_argument());

                let default_assignments = default_fields.iter().map(|field| {
                    let name = &field.name;
                    let default = field.default.as_ref().unwrap();

                    quote! {
                        let #name = #default;
                    }
                });

                let into_assignments = variant.fields.iter().filter(|field| field.is_into).map(|field| {
                    let name = &field.name;

                    quote! {
                        let #name = #name.into();
                    }
                });

                let fields = if let Some(fields_named) = variant.fields_named {
                    let field_names = variant.fields.iter().map(|field| &field.name);

                    Some(if fields_named {
                        quote! {
                            {
                                #(#field_names),*
                            }
                        }
                    } else {
                        quote! { (#(#field_names),*) }
                    })
                } else {
                    None
                };

                quote! {
                    pub fn #fn_name(#(#arguments),*) -> Self {
                        #(
                            #default_assignments
                        )*

                        #(
                            #into_assignments
                        )*

                        Self::#name #fields
                    }
                }
            });

        let message_match_arms = self.0.variants.iter().map(|variant| {
            let pattern = variant.match_pattern(quote! { Self }, !variant.is_reachable());

            if let Some(format) = &variant.format {
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
                        ::std::format!(#format, #(#colored_fields),*)
                    }
                };

                quote! {
                    #pattern => #formatted_message
                }
            } else {
                quote! {
                    #pattern => ::core::unreachable!()
                }
            }
        });

        quote! {
            impl #generic_parameters #name #stripped_generic_parameters #where_clause {
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
                    }
                }
            }
        }
    }
}

impl Parse for DiagnosticNote {
    fn parse(input: ParseStream) -> Result<Self> {
        let enum_data = input.parse::<EnumData<_>>()?;

        if let Some((_, span)) = enum_data.code_offset {
            return Err(syn::Error::new(span, Error::DiagnosticNoteOffsetAttr));
        }

        Ok(Self(enum_data))
    }
}

impl ToTokens for DiagnosticNote {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.impl_block());
    }
}
