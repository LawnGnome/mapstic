use std::num::IntErrorKind;

use darling::{ast::NestedMeta, FromMeta};
use indexmap::IndexMap;
use mapstic_core::ParamValue;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{ext::IdentExt, Expr, Ident, Lit, Meta, UnOp};

/// Wrapper for [`ParamValue`] to allow us to impl [`FromMeta`] and [`ToTokens`].
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct AttrValue(ParamValue);

/// Given a list of nested [`Meta`] items, convert them into a map of string/value pairs.
fn from_list<'a>(
    items: impl Iterator<Item = &'a NestedMeta>,
) -> darling::Result<IndexMap<String, ParamValue>> {
    let mut nested = IndexMap::new();

    for item in items {
        if let NestedMeta::Meta(ref meta) = item {
            let (ident, value) = from_meta(meta)?;
            nested.insert(ident.to_string(), value);
        } else {
            return Err(darling::Error::custom(
                "unexpected literal when a key-value pair was expected",
            )
            .with_span(item));
        }
    }

    Ok(nested)
}

/// Parse a single [`Meta`] item into a name and value pair.
fn from_meta(meta: &Meta) -> darling::Result<(Ident, ParamValue)> {
    match meta {
        Meta::NameValue(nv) => {
            if let Some(ident) = nv.path.get_ident() {
                Ok((ident, AttrValue::from_expr(&nv.value)?.0))
            } else {
                // In practice, I'm not sure how you actually trigger this.
                Err(darling::Error::unsupported_shape("missing name/value ident").with_span(meta))
            }
        }
        Meta::List(list) => {
            if let Some(ident) = list.path.get_ident() {
                let value = from_list(NestedMeta::parse_meta_list(list.tokens.clone())?.iter())?;
                Ok((ident, ParamValue::OwnedNested(value)))
            } else {
                // In practice, I'm not sure how you actually trigger this.
                Err(darling::Error::unsupported_shape("missing list ident").with_span(meta))
            }
        }
        Meta::Path(path) => {
            // We'll treat a bare path as implicitly being a boolean true.
            if let Some(ident) = path.get_ident() {
                Ok((ident, ParamValue::Bool(true)))
            } else {
                // Unless, of course, it somehow doesn't have an ident.
                Err(darling::Error::unsupported_shape("missing path ident"))
            }
        }
    }
    // We have to handle raw identifiers, since they won't be identifiers in the generated code.
    .map(|(ident, value)| (ident.unraw(), value))
    // We have to attach the span so that rustc can highlight the right part of the attribute.
    .map_err(|e| e.with_span(meta))
}

impl FromMeta for AttrValue {
    fn from_list(items: &[darling::ast::NestedMeta]) -> darling::Result<Self> {
        Ok(Self(ParamValue::OwnedNested(from_list(items.iter())?)))
    }

    fn from_value(lit: &Lit) -> darling::Result<Self> {
        Ok(Self(
            match lit {
                Lit::Str(lit_str) => Ok(ParamValue::OwnedString(lit_str.value())),
                Lit::Char(lit_char) => Ok(ParamValue::OwnedString(lit_char.value().into())),
                Lit::Int(lit_int) => match lit_int.base10_digits().parse() {
                    Ok(uint) => Ok(ParamValue::Uint(uint)),
                    Err(e) if e.kind() == &IntErrorKind::InvalidDigit => lit_int
                        .base10_parse()
                        .map(ParamValue::Int)
                        .map_err(darling::Error::custom),
                    Err(e) => Err(darling::Error::custom(e)),
                },
                Lit::Float(lit_float) => lit_float
                    .base10_parse()
                    .map(ParamValue::Float)
                    .map_err(darling::Error::custom),
                Lit::Bool(lit_bool) => Ok(ParamValue::Bool(lit_bool.value())),
                _ => Err(unexpected_lit_type(lit)),
            }
            .map_err(|e| e.with_span(lit))?,
        ))
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        // We'll override this to support negative numbers properly.
        match expr {
            Expr::Lit(lit) => Self::from_value(&lit.lit),
            Expr::Group(group) => Self::from_expr(&group.expr),
            Expr::Unary(unary) => match unary.op {
                UnOp::Neg(_) => {
                    // We only support integer or float literals as the expression.
                    match unary.expr.as_ref() {
                        Expr::Lit(lit) => match &lit.lit {
                            Lit::Int(lit_int) => Ok(Self(
                                lit_int
                                    .base10_parse()
                                    .map(|n: i64| ParamValue::Int(-n))
                                    .map_err(|e| darling::Error::custom(e).with_span(unary))?,
                            )),
                            Lit::Float(lit_float) => Ok(Self(
                                lit_float
                                    .base10_parse()
                                    .map(|n: f64| ParamValue::Float(-n))
                                    .map_err(|e| darling::Error::custom(e).with_span(unary))?,
                            )),
                            _ => Err(unexpected_lit_type(&lit.lit)),
                        },
                        _ => Err(darling::Error::unexpected_expr_type(&unary.expr)),
                    }
                }
                _ => Err(darling::Error::unexpected_expr_type(expr)),
            },
            _ => Err(darling::Error::unexpected_expr_type(expr)),
        }
        .map_err(|e| e.with_span(expr))
    }

    fn from_word() -> darling::Result<Self> {
        // Bare words are treated as true values.
        Ok(Self(ParamValue::Bool(true)))
    }
}

fn unexpected_lit_type(lit: &Lit) -> darling::Error {
    darling::Error::custom(format!(
        "{} `{}`: {}",
        "unexpected type",
        lit_as_string(lit),
        "expected a string, char, int, float, or bool literal"
    ))
}

fn lit_as_string(lit: &Lit) -> &'static str {
    match lit {
        Lit::Str(_) => "string",
        Lit::ByteStr(_) => "byte string",
        Lit::CStr(_) => "C string",
        Lit::Byte(_) => "byte",
        Lit::Char(_) => "char",
        Lit::Int(_) => "int",
        Lit::Float(_) => "float",
        Lit::Bool(_) => "bool",
        Lit::Verbatim(_) => "verbatim",
        _ => "unknown",
    }
}

impl ToTokens for AttrValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(param_value_to_token_stream(&self.0))
    }
}

fn param_value_to_token_stream(value: &ParamValue) -> proc_macro2::TokenStream {
    use ParamValue::*;

    match &value {
        Bool(b) => quote! { ::mapstic::ParamValue::Bool(#b) },
        Int(i) => quote! { ::mapstic::ParamValue::Int(#i) },
        Uint(u) => quote! { ::mapstic::ParamValue::Uint(#u) },
        Float(f) => quote! { ::mapstic::ParamValue::Float(#f) },
        String(s) => quote! { ::mapstic::ParamValue::String(#s) },
        OwnedString(s) => quote! { ::mapstic::ParamValue::String(#s) },
        Nested(index_map) => nested_to_token_stream(index_map.iter()),
        OwnedNested(index_map) => nested_to_token_stream(index_map.iter()),
    }
}

fn nested_to_token_stream<'a, I, S>(iter: I) -> proc_macro2::TokenStream
where
    I: Iterator<Item = (S, &'a ParamValue)>,
    S: AsRef<str>,
{
    let tokens = iter.map(|(name, value)| {
        let name = name.as_ref();
        let value = param_value_to_token_stream(value);

        quote! { (#name, #value) }
    });

    quote! { ::mapstic::ParamValue::Nested([#( #tokens ),*].into_iter().collect()) }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{BTreeMap, HashMap},
        str::FromStr,
    };

    use darling::FromDeriveInput;

    use super::*;

    #[derive(FromDeriveInput, Debug)]
    #[darling(attributes(test))]
    struct TestAttrs {
        #[darling(default)]
        params: HashMap<String, AttrValue>,
    }

    trait ToValue {
        fn to_value(self) -> AttrValue;
    }

    macro_rules! impl_to_value {
        ($ty:ty, $variant:path) => {
            impl ToValue for $ty {
                fn to_value(self) -> AttrValue {
                    AttrValue($variant(self))
                }
            }
        };
    }

    impl_to_value!(bool, ParamValue::Bool);
    impl_to_value!(i64, ParamValue::Int);
    impl_to_value!(u64, ParamValue::Uint);
    impl_to_value!(f64, ParamValue::Float);
    impl_to_value!(&'static str, ParamValue::String);

    impl ToValue for ParamValue {
        fn to_value(self) -> AttrValue {
            AttrValue(self)
        }
    }

    impl ToValue for AttrValue {
        fn to_value(self) -> AttrValue {
            self
        }
    }

    macro_rules! assert_attrs {
        ($input:literal, $expected:expr,) => {{
            let stream = TokenStream::from_str($input).expect("parse input");
            let input = syn::parse2(stream).expect("parse token stream");
            let attrs = TestAttrs::from_derive_input(&input).expect("parse attributes");

            let params: BTreeMap<_, _> = attrs.params.into_iter().collect();
            let expected: BTreeMap<_, _> = $expected
                .into_iter()
                .map(|(k, v)| (k.to_string(), v.to_value()))
                .collect();

            assert_eq!(
                serde_json::to_string(&params).expect("params"),
                serde_json::to_string(&expected).expect("expected"),
            );
        }};
        ($input:literal, $expected:expr) => {
            assert_attrs!($input, $expected,)
        };
    }

    #[test]
    fn attrs() {
        // We have integration tests for the more complex examples, but this tests that we get the
        // exact types that we expect for all the interesting cases.

        assert_attrs!(
            r#"
                #[test(params(foo))]
                struct Foo {}
            "#,
            [("foo", true)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = true))]
                struct Foo {}
            "#,
            [("foo", true)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = false))]
                struct Foo {}
            "#,
            [("foo", false)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = 42))]
                struct Foo {}
            "#,
            [("foo", 42i64)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = -42))]
                struct Foo {}
            "#,
            [("foo", -42i64)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = 42.))]
                struct Foo {}
            "#,
            [("foo", 42.0f64)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = -42.))]
                struct Foo {}
            "#,
            [("foo", -42.0f64)],
        );

        assert_attrs!(
            r#"
                #[test(params(foo = "bar"))]
                struct Foo {}
            "#,
            [("foo", "bar")],
        );

        assert_attrs!(
            r#"
                #[test(params(foo, bar(quux)))]
                struct Foo {}
            "#,
            [
                ("foo", true.to_value()),
                (
                    "bar",
                    ParamValue::OwnedNested(
                        [("quux".to_string(), ParamValue::Bool(true))]
                            .into_iter()
                            .collect()
                    )
                    .to_value(),
                )
            ]
        );
    }

    macro_rules! assert_parse_error {
        ($input:literal) => {{
            let stream = TokenStream::from_str($input).expect("parse input");
            let input = syn::parse2(stream).expect("parse token stream");
            let error = TestAttrs::from_derive_input(&input).expect_err("parse attributes");

            insta::assert_debug_snapshot!(error);
        }};
    }

    #[test]
    fn int_out_of_range() {
        assert_parse_error!(
            r#"
                #[test(params(large_int = 300000000000000000000))]
                struct Foo {}
            "#
        );
    }

    #[test]
    fn negative_int_out_of_range() {
        assert_parse_error!(
            r#"
                #[test(params(large_int = -300000000000000000000))]
                struct Foo {}
            "#
        );
    }
}
