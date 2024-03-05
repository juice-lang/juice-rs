use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Attribute, Ident, Lifetime, Result, Token, Visibility,
};

pub struct EnumDefinition {
    pub attributes: Vec<Attribute>,
    pub visiblity: Option<Visibility>,
    pub enum_token: Token![enum],
    pub name: Ident,
    pub lifetime: Option<(Token![<], Lifetime, Token![>])>,
}

impl EnumDefinition {
    pub fn generic_parameters(&self) -> TokenStream {
        if let Some((open, lifetime, close)) = self.lifetime.as_ref() {
            quote! { #open #lifetime #close }
        } else {
            TokenStream::new()
        }
    }
}

impl Parse for EnumDefinition {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = Attribute::parse_outer(input)?;

        let visiblity = if input.peek(Token![pub]) {
            Some(input.parse()?)
        } else {
            None
        };

        let enum_token = input.parse::<Token![enum]>()?;

        let name = input.parse::<Ident>()?;

        let lifetime = if input.peek(Token![<]) {
            let open = input.parse::<Token![<]>()?;

            let lifetime = input.parse::<Lifetime>()?;

            let close = input.parse::<Token![>]>()?;

            Some((open, lifetime, close))
        } else {
            None
        };

        Ok(Self {
            attributes,
            visiblity,
            name,
            lifetime,
            enum_token,
        })
    }
}

impl ToTokens for EnumDefinition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attributes = &self.attributes;
        let visiblity = self.visiblity.as_ref();
        let enum_token = &self.enum_token;
        let name = &self.name;
        let generic_parameters = self.generic_parameters();

        tokens.extend(quote! {
            #(#attributes)*
            #visiblity #enum_token #name #generic_parameters
        });
    }
}
