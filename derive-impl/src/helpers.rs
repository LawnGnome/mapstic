//! Helpful types and traits when figuring out what type of `struct` is being handled by darling.

use darling::ast::Fields;
use syn::Ident;

/// The type of struct being handled, based on the fields within it (represented as `T`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructType<T> {
    Unit,
    TupleSingle(T),
    TupleMultiple(Vec<T>),
    Named(Vec<T>),
}

pub trait FieldsExt<T> {
    fn into_struct_type(self) -> StructType<T>;
}

impl<T> FieldsExt<T> for Fields<T>
where
    T: FieldIdent,
{
    fn into_struct_type(mut self) -> StructType<T> {
        if self.fields.is_empty() {
            StructType::Unit
        } else if self
            .fields
            .first()
            .expect("fields with len >= 1 must have a first element")
            .ident()
            .is_none()
        {
            if self.fields.len() == 1 {
                StructType::TupleSingle(
                    self.fields
                        .pop()
                        .expect("fields with len 1 must have an element"),
                )
            } else {
                StructType::TupleMultiple(self.fields)
            }
        } else {
            StructType::Named(self.fields)
        }
    }
}

/// Identifies a field.
pub trait FieldIdent {
    fn ident(&self) -> Option<&Ident>;
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use claims::assert_none;
    use darling::{ast::Data, FromDeriveInput, FromField};
    use proc_macro2::TokenStream;

    use super::*;

    #[derive(FromDeriveInput)]
    struct StructOpts {
        data: Data<(), FieldOpts>,
    }

    impl StructOpts {
        fn derive(input: &'static str) -> Self {
            let stream = TokenStream::from_str(input).expect("parse input");
            let input = syn::parse2(stream).expect("parse token stream");
            Self::from_derive_input(&input).expect("derive opts")
        }

        fn struct_type(self) -> StructType<FieldOpts> {
            self.data
                .take_struct()
                .expect("take struct")
                .into_struct_type()
        }
    }

    #[derive(FromField, Debug, Eq, PartialEq)]
    struct FieldOpts {
        ident: Option<Ident>,
    }

    impl FieldIdent for FieldOpts {
        fn ident(&self) -> Option<&Ident> {
            self.ident.as_ref()
        }
    }

    macro_rules! assert_struct_type_unit {
        ($input:literal) => {
            if !matches!(StructOpts::derive($input).struct_type(), StructType::Unit) {
                panic!("unexpected variant for {}; expected unit", $input);
            }
        };
    }

    macro_rules! assert_struct_type_non_unit {
        ($input:literal, $variant:ident) => {{
            match StructOpts::derive($input).struct_type() {
                StructType::$variant(inner) => inner,
                st => {
                    panic!(
                        "unexpected variant for {}: got {st:?}; expected {}",
                        $input,
                        stringify!($pat)
                    );
                }
            }
        }};
    }

    macro_rules! assert_struct_type_tuple_single {
        ($input:literal) => {
            assert_struct_type_non_unit!($input, TupleSingle)
        };
    }

    macro_rules! assert_struct_type_tuple_multiple {
        ($input:literal) => {
            assert_struct_type_non_unit!($input, TupleMultiple)
        };
    }

    macro_rules! assert_struct_type_named {
        ($input:literal) => {
            assert_struct_type_non_unit!($input, Named)
        };
    }

    #[test]
    fn test_struct_type() {
        assert_struct_type_unit!(r#"struct Foo;"#);

        assert_struct_type_unit!(r#"struct Foo();"#);

        let field = assert_struct_type_tuple_single!(r#"struct Foo(String);"#);
        assert_none!(field.ident());

        let fields = assert_struct_type_tuple_multiple!(r#"struct Foo(String, String);"#);
        assert_none!(fields.into_iter().find(|field| field.ident().is_some()));

        let fields = assert_struct_type_named!(r#"struct Foo { a: String }"#);
        assert_eq!(
            fields
                .into_iter()
                .map(|field| field.ident().expect("field ident").to_string())
                .collect::<Vec<_>>(),
            vec!["a"]
        );

        let fields = assert_struct_type_named!(r#"struct Foo { a: String, b: String }"#);
        assert_eq!(
            fields
                .into_iter()
                .map(|field| field.ident().expect("field ident").to_string())
                .collect::<Vec<_>>(),
            vec!["a", "b"]
        );
    }
}
