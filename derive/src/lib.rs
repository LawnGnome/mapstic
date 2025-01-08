//! Provides the [`ToMapping`] derive macro.

use proc_macro::TokenStream;
use proc_macro_error::{abort_call_site, proc_macro_error};

#[proc_macro_error]
#[proc_macro_derive(ToMapping, attributes(mapstic))]
pub fn to_mapping_derive(input: TokenStream) -> TokenStream {
    match mapstic_derive_impl::to_mapping(input.into()) {
        Ok(stream) => stream.into(),
        Err(e) => abort_call_site!("{}", e),
    }
}
