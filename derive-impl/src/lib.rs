use std::collections::HashMap;

use darling::{
    ast::{Data, Fields},
    FromDeriveInput, FromField,
};
use helpers::{FieldIdent, FieldsExt, StructType};
use indexmap::IndexMap;
use itertools::Itertools;
use params::{AttrParams, AttrValue};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{ext::IdentExt, DeriveInput, Ident, Type};

mod error;
mod helpers;
mod params;

pub use error::Error;

/// Attributes defined on a top-level type that derives [`mapstic_core::ToMapping`].
#[derive(FromDeriveInput, Debug)]
#[darling(attributes(mapstic))]
struct StructAttrs {
    /// The optional `mapping_type` attribute, which prevents type inference from taking place.
    #[darling(default)]
    mapping_type: Option<String>,

    /// The optional `params` attribute, used to define any field parameters that should be
    /// included within the explicit mapping.
    #[darling(default)]
    params: HashMap<String, AttrValue>,

    /// Fields defined within the type.
    ///
    /// Note that the enum variant of Data is defined as () — we simply won't be handling enums
    /// unless `mapping_type` was defined.
    data: Data<(), FieldAttrs>,
}

/// Attributes defined on a field within a struct deriving [`mapstic_core::ToMapping`].
#[derive(FromField, Debug)]
#[darling(attributes(mapstic))]
struct FieldAttrs {
    /// The field name.
    ident: Option<Ident>,

    /// The field type.
    ty: Type,

    /// The optional `mapping_type` attribute, which prevents type inference.
    #[darling(default)]
    mapping_type: Option<String>,

    /// The optional `skip` attribute, which will result in the field being completely ignored.
    #[darling(default)]
    skip: bool,

    /// The optional `params` attribute, used to define any field parameters that should be
    /// included within the explicit mapping.
    #[darling(default)]
    params: HashMap<String, AttrValue>,
}

impl FieldIdent for FieldAttrs {
    fn ident(&self) -> Option<&Ident> {
        self.ident.as_ref()
    }
}

/// Implementation of the [`mapstic_core::ToMapping`] derive macro.
pub fn to_mapping(input: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = match syn::parse2(input) {
        Ok(input) => input,
        Err(e) => return Ok(e.to_compile_error()),
    };

    // Have darling parse the type definition.
    let StructAttrs {
        mapping_type,
        params,
        data,
    } = match StructAttrs::from_derive_input(&input) {
        Ok(attrs) => attrs,
        Err(e) => return Ok(e.write_errors()),
    };

    // Get the bits of type metadata we need to build impl blocks later.
    let DeriveInput {
        generics, ident, ..
    } = input;
    let (impl_generics, ty_generics, where_generics) = generics.split_for_impl();

    // Wrap the parameters to make them easier to use.
    let params = AttrParams::from(params);

    if let Some(ty) = mapping_type {
        // If mapping_type was defined on the type, there's basically nothing for us to do here: we
        // can simply generate a ToMapping impl based on the mapping type and whatever parameters
        // were provided.
        //
        // Note that doing this before we check if we have a struct or enum means that this will
        // also work for an enum that we otherwise wouldn't be able to handle.
        Ok(quote! {
            impl #impl_generics ::mapstic::ToMapping for #ident #ty_generics #where_generics {
                fn to_mapping() -> ::mapstic::Mapping {
                    ::mapstic::Mapping::scalar(#ty, #params)
                }

                fn to_mapping_with_params(params: ::mapstic::Params) -> ::mapstic::Mapping {
                    // Extend the parameters from the struct, if any, with the parameters passed in
                    // from any field definitions that use this struct.
                    let mut local = #params;
                    local.extend(params);

                    ::mapstic::Mapping::scalar(#ty, local)
                }
            }
        })
    } else {
        // OK, so no mapping_type. Let's see what we have in this type.
        let repr = match data {
            Data::Struct(fields) => TypeRepr::try_from_fields(fields, params.iter())?,
            // We can't automatically derive enums, so let's just bail and give the user a
            // hopefully helpful message.
            Data::Enum(_) => return Err(Error::Enum),
        };

        match repr {
            // This is a tuple struct with one field: we should treat this as if it has been
            // flattened into its container.
            TypeRepr::Flat(mut field) => {
                field.params.extend(params);

                // This is a little hacky: while the generated code using #field would include any
                // parameters defined on the tuple struct or the tuple struct's field, it wouldn't
                // include any parameters passed in from a container unless they're added to the
                // parameters at runtime in to_mapping_with_params, but the ToTokens impl for Field
                // doesn't take that into account. Instead, we'll use FieldWithInjectedParams to
                // extend the parameters from a variable in the environment.
                let inj = FieldWithInjectedParams {
                    field: &field,
                    ident: Ident::new("params", Span::call_site()),
                };

                Ok(quote! {
                    impl #impl_generics ::mapstic::ToMapping for #ident #ty_generics #where_generics {
                        fn to_mapping() -> ::mapstic::Mapping {
                            #field
                        }

                        fn to_mapping_with_params(mut params: ::mapstic::Params) -> ::mapstic::Mapping {
                            #inj
                        }
                    }
                })
            }
            // A traditional named struct with one or more fields.
            TypeRepr::Named(fields) => {
                let param_extend = match params.is_empty() {
                    false => Some(quote! { params.extend(#params); }),
                    true => None,
                };

                Ok(quote! {
                    impl #impl_generics ::mapstic::ToMapping for #ident #ty_generics #where_generics {
                        fn to_mapping() -> ::mapstic::Mapping {
                            ::mapstic::Mapping::object(#fields.into_iter(), #params)
                        }

                        fn to_mapping_with_params(mut params: ::mapstic::Params) -> ::mapstic::Mapping {
                            #param_extend
                            ::mapstic::Mapping::object(#fields.into_iter(), params)
                        }
                    }
                })
            }
        }
    }
}

/// A collection of named fields.
struct NamedFields(IndexMap<String, Field>);

impl ToTokens for NamedFields {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // This is represented as a slice of (name, value) pairs.

        let fields = self.0.iter().map(|(name, field)| {
            quote! { (#name, #field) }
        });

        tokens.extend(quote! {
            [
                #( #fields ),*
            ]
        });
    }
}

/// A field, which includes both a type and an optional set of parameters.
#[derive(Debug)]
struct Field {
    ty: FieldType,
    params: AttrParams,
}

impl From<FieldAttrs> for Field {
    fn from(opts: FieldAttrs) -> Self {
        Self {
            ty: match opts.mapping_type {
                Some(ty) => FieldType::Explicit(ty),
                None => FieldType::Inferred(opts.ty.into_token_stream()),
            },
            params: opts.params.into(),
        }
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // This is represented as an expression that returns a mapstic::Mapping.

        let Field { ty, params } = self;

        tokens.extend(match ty {
            FieldType::Explicit(ty) => quote! {
                ::mapstic::Mapping::scalar(#ty, #params)
            },
            FieldType::Inferred(ty) => {
                quote! { <#ty as ::mapstic::ToMapping>::to_mapping_with_params(#params.into()) }
            }
        });
    }
}

/// An extension to [`Field`] which allows for parameters to be injected from the environment when
/// building the tokens that return a [`mapstic_core::Mapping`].
///
/// It is up to the caller to ensure that [`ToTokens`] is used in a context where the [`Ident`]
/// variable is available.
struct FieldWithInjectedParams<'a> {
    field: &'a Field,
    ident: Ident,
}

impl ToTokens for FieldWithInjectedParams<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Like the Field impl of ToTokens, this is represented as an expression that returns a
        // mapstic::Mapping, but with the added wrinkle that the parameters will be merged with a
        // variable that is required to be defined outside of the context within which this
        // function is called.

        let Self {
            field: Field { ty, params },
            ident,
        } = self;

        let params = quote! {
            let mut local = #params;
            local.extend(#ident);
        };

        tokens.extend(match ty {
            FieldType::Explicit(ty) => quote! {{
                #params
                ::mapstic::Mapping::scalar(#ty, local)
            }},
            FieldType::Inferred(ty) => quote! {{
                #params
                <#ty as ::mapstic::ToMapping>::to_mapping_with_params(local)
            }},
        });
    }
}

/// The type of a [`Field`]: either an explicit string type that Elasticsearch understands, or a
/// Rust type (represented as a [`TokenStream`]) that impls [`mapstic_core::ToMapping`].
#[derive(Debug)]
enum FieldType {
    Explicit(String),
    Inferred(TokenStream),
}

/// The representation of a type deriving [`mapstic_core::ToMapping`]: either as a flat type, or a
/// set of named fields.
enum TypeRepr {
    Flat(Field),
    Named(NamedFields),
}

impl TypeRepr {
    /// Given a set of fields parsed from a `struct`, figure out the right representation of the
    /// fields.
    fn try_from_fields<'a, I, K>(
        fields: Fields<FieldAttrs>,
        container_params: I,
    ) -> Result<Self, Error>
    where
        I: Iterator<Item = (K, &'a AttrValue)>,
        K: ToString,
    {
        match fields.into_struct_type() {
            StructType::Unit => Err(Error::UnitStruct),
            StructType::TupleMultiple(_) => Err(Error::TupleStruct),
            StructType::TupleSingle(opts) if opts.skip => Err(Error::AllSkipped),
            StructType::TupleSingle(opts) => {
                // Special case: if there are parameters on the container attribute, we need to
                // merge these into the field parameters, since this is being flattened.
                let mut field = Field::from(opts);
                field
                    .params
                    .extend(container_params.map(|(k, v)| (k.to_string(), v.clone())));

                Ok(Self::Flat(field))
            }
            StructType::Named(fields) => {
                let types: IndexMap<_, _> = fields
                    .into_iter()
                    .filter(|opts| !opts.skip)
                    .map(|opts| match opts.ident {
                        Some(ref ident) => Ok((ident.unraw().to_string(), Field::from(opts))),
                        None => Err(Error::MixedStruct),
                    })
                    .try_collect()?;

                if types.is_empty() {
                    Err(Error::AllSkipped)
                } else {
                    Ok(Self::Named(NamedFields(types)))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use claims::assert_matches;

    use super::*;

    #[test]
    fn all_skipped_named() {
        let source = r#"
            #[derive(ToMapping)]
            struct Foo {
                #[mapstic(skip)]
                a: String,

                #[mapstic(skip)]
                b: String,
            }
        "#;
        let stream = TokenStream::from_str(source).expect("parse input");

        assert_matches!(to_mapping(stream), Err(Error::AllSkipped));
    }

    #[test]
    fn all_skipped_tuple() {
        let source = r#"
            #[derive(ToMapping)]
            struct Foo(#[mapstic(skip)] String);
        "#;
        let stream = TokenStream::from_str(source).expect("parse input");

        assert_matches!(to_mapping(stream), Err(Error::AllSkipped));
    }

    #[test]
    fn enum_fail() {
        let source = r#"
            #[derive(ToMapping)]
            enum E { A, B }
        "#;
        let stream = TokenStream::from_str(source).expect("parse input");

        assert_matches!(to_mapping(stream), Err(Error::Enum));
    }

    #[test]
    fn tuple() {
        let source = r#"
            #[derive(ToMapping)]
            struct Foo(String, String);
        "#;
        let stream = TokenStream::from_str(source).expect("parse input");

        assert_matches!(to_mapping(stream), Err(Error::TupleStruct));
    }

    #[test]
    fn unit() {
        let source = r#"
            #[derive(ToMapping)]
            struct Foo;
        "#;
        let stream = TokenStream::from_str(source).expect("parse input");

        assert_matches!(to_mapping(stream), Err(Error::UnitStruct));
    }
}
