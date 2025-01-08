use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

mod value;
pub use value::AttrValue;

/// A collection of named parameters that can be used to generate Rust code that will instantiate a
/// [`mapstic_core::Params`].
#[derive(Debug, Clone)]
pub struct AttrParams(HashMap<String, AttrValue>);

impl Deref for AttrParams {
    type Target = HashMap<String, AttrValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AttrParams {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<HashMap<String, AttrValue>> for AttrParams {
    fn from(value: HashMap<String, AttrValue>) -> Self {
        Self(value)
    }
}

impl IntoIterator for AttrParams {
    type Item = <HashMap<String, AttrValue> as IntoIterator>::Item;
    type IntoIter = <HashMap<String, AttrValue> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl ToTokens for AttrParams {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.0.is_empty() {
            tokens.extend(quote! { ::mapstic::Params::default() });
        } else {
            let params = self.0.iter().map(|(name, value)| {
                quote! {
                    (#name, #value)
                }
            });

            tokens.extend(quote! {
                [
                    #( #params ),*
                ]
                .into_iter()
                .collect::<::mapstic::Params>()
            });
        }
    }
}
