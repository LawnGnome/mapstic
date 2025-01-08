//! Core types and functionality used by the code generated by the `mapstic-derive` crate.

use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    cmp::Reverse,
    collections::{BTreeSet, HashSet, LinkedList, VecDeque},
    ffi::{CStr, CString},
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU8,
    },
    sync::{
        atomic::{
            AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicU16, AtomicU32, AtomicU64, AtomicU8,
        },
        Mutex, RwLock,
    },
};

use indexmap::IndexMap;
use serde::{Serialize, Serializer};

mod param;
pub use {param::Params, param::Value as ParamValue};

/// A type that can be described as an explicit Elasticsearch mapping.
///
/// This trait must be derived, rather than being implemented directly onto a type.
pub trait ToMapping {
    /// Returns the Elasticsearch mapping for the type.
    fn to_mapping() -> Mapping {
        Self::to_mapping_with_params(Params::default())
    }

    /// Returns the Elasticsearch mapping for the type, mixing in the given options.
    ///
    /// This is for internal use only.
    #[doc(hidden)]
    fn to_mapping_with_params(options: Params) -> Mapping;
}

/// An Elasticsearch field mapping.
///
/// Generally speaking, this will be serialised and sent to Elasticsearch via [the update mapping
/// API][update-mapping]. The default `serde_json` representation of this type is a valid request
/// body for that API.
///
/// To use this with the `elasticsearch` crate:
///
/// ```ignore
/// use elasticsearch::{http::transport::Transport, indices::IndicesPutMappingParts, Elasticsearch};
/// use mapstic::ToMapping;
///
/// #[derive(ToMapping)]
/// struct MyIndex {
///     id: i64,
/// }
///
/// #[tokio::main]
/// async fn main() -> anyhow::Result<()> {
///     let client = Elasticsearch::new(Transport::single_node("http://localhost:9200")?);
///     let response = client
///         .indices()
///         .put_mapping(IndicesPutMappingParts::Index(&["my-index"]))
///         .body(MyIndex::to_mapping())
///         .send()
///         .await?;
///
///     dbg!(response);
///     Ok(())
/// }
/// ```
///
/// [update-mapping]: https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-put-mapping.html
#[derive(Debug, Clone)]
pub enum Mapping {
    #[doc(hidden)]
    Scalar(Property),
    #[doc(hidden)]
    Object(Object),
}

// These functions are considered internal and are not subject to any usual semver guarantees.
#[doc(hidden)]
impl Mapping {
    pub fn scalar(type_name: &'static str, options: impl Into<Params>) -> Self {
        Self::Scalar(Property {
            ty: type_name,
            options: options.into(),
        })
    }

    pub fn object(
        i: impl Iterator<Item = (&'static str, Mapping)>,
        options: impl Into<Params>,
    ) -> Self {
        Self::Object(Object {
            properties: i.collect(),
            options: options.into(),
        })
    }
}

impl Serialize for Mapping {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Mapping::Scalar(type_name) => type_name.serialize(serializer),
            Mapping::Object(map) => map.serialize(serializer),
        }
    }
}

/// A scalar field within an Elasticsearch mapping.
#[derive(Serialize, Debug, Clone)]
pub struct Property {
    #[serde(rename = "type")]
    ty: &'static str,

    #[serde(flatten)]
    options: Params,
}

/// A complex object within an Elasticsearch mapping.
#[derive(Serialize, Debug, Clone)]
pub struct Object {
    properties: IndexMap<&'static str, Mapping>,

    #[serde(flatten)]
    options: Params,
}

macro_rules! impl_scalar_mapping {
    ($name:literal, $($ty:ty),+) => {
        $(
            impl $crate::ToMapping for $ty {
                fn to_mapping_with_params(options: $crate::Params) -> $crate::Mapping {
                    $crate::Mapping::scalar($name, options)
                }
            }
        )+
    };
}

// Define common binary string mappings by default. Note that we can't impl ToMapping for Vec<u8>
// as it'll conflict with the generic Vec<T> impl below.
impl_scalar_mapping!("binary", &[u8], &CStr, CString);

impl_scalar_mapping!("boolean", bool);

// Numbers. We won't do u128 or i128 because there isn't an obvious type to map them to.
impl_scalar_mapping!("long", i64, NonZeroI64, AtomicI64);
impl_scalar_mapping!("unsigned_long", u64, NonZeroU64, AtomicU64);
impl_scalar_mapping!("integer", i32, NonZeroI32, AtomicI32);
impl_scalar_mapping!("unsigned_long", u32, NonZeroU32, AtomicU32);
impl_scalar_mapping!("short", i16, NonZeroI16, AtomicI16);
impl_scalar_mapping!("unsigned_long", u16, NonZeroU16, AtomicU16);
impl_scalar_mapping!("byte", i8, NonZeroI8, AtomicI8);
impl_scalar_mapping!("unsigned_long", u8, NonZeroU8, AtomicU8);
impl_scalar_mapping!("double", f64);
impl_scalar_mapping!("float", f32);

#[cfg(feature = "nightly")]
impl_scalar_mapping!("half_float", f16);

// Date types. We won't use date_nanos by default, but that should be able to be opted into.
impl_scalar_mapping!("date", std::time::SystemTime);

// IP addresses.
impl_scalar_mapping!("ip", IpAddr, Ipv4Addr, Ipv6Addr);

// Strings.
//
// We're going to match the dynamic mapping behaviour of Elasticsearch by default, so this is not a
// simple scalar mapping, since there are default options.
macro_rules! impl_string_mapping {
    ($($ty:ty),+) => {
        $(
            impl $crate::ToMapping for $ty {
                fn to_mapping_with_params(options: $crate::Params) -> $crate::Mapping {
                    let mut local = $crate::default_string_options();
                    local.extend(options);

                    $crate::Mapping::scalar("text", local)
                }
            }
        )+
    }
}

/// Default options to be applied to string types.
///
/// These match the defaults [when a string is dynamically mapped by Elasticsearch][dynamic].
///
/// [dynamic]: https://www.elastic.co/guide/en/elasticsearch/reference/current/dynamic-field-mapping.html
pub fn default_string_options() -> Params {
    [(
        "fields",
        ParamValue::Nested(
            [(
                "keyword",
                ParamValue::Nested(
                    [
                        ("ignore_above", ParamValue::Uint(256)),
                        ("type", ParamValue::String("keyword")),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )]
            .into_iter()
            .collect(),
        ),
    )]
    .into_iter()
    .collect()
}

impl_string_mapping!(String, &str, Cow<'_, str>);

// chrono compatibility.
#[cfg(feature = "chrono")]
impl_scalar_mapping!("date", chrono::NaiveDateTime, chrono::NaiveDate);

#[cfg(feature = "chrono")]
macro_rules! impl_chrono_mapping {
    ($name:literal, $generic:ident, $($ty:ty),+) => {
        $(
            #[allow(deprecated)]
            #[cfg(feature = "chrono")]
            impl<$generic: chrono::TimeZone> $crate::ToMapping for $ty {
                fn to_mapping_with_params(options: $crate::Params) -> $crate::Mapping {
                    $crate::Mapping::scalar($name, options)
                }
            }
        )+
    }
}

#[cfg(feature = "chrono")]
impl_chrono_mapping!("date", T, chrono::DateTime<T>, chrono::Date<T>);

macro_rules! impl_container_mapping {
    (<$($generics:tt),+> $inner:ident for $ty:ty where $($where:ident: $trait:ident),*) => {
        impl<$($generics),+> ToMapping for $ty
        where
            $inner: ToMapping,
            $($where: $trait),*
        {
            fn to_mapping_with_params(options: Params) -> Mapping {
                $inner::to_mapping_with_params(options)
            }
        }
    };
    (<$($generics:tt),+> $inner:ident for $ty:ty) => {
        impl_container_mapping!(<$($generics),+> $inner for $ty where);
    };
    ($generic:ident for $ty:ty) => {
        impl_container_mapping!(<$generic> $generic for $ty);
    };
}

// Since ElasticSearch explicit mappings are always nullable, unwrap Option<> to make it easier.
impl_container_mapping!(T for Option<T>);

// Similarly, handle basic containers the same way Serde does. The collections in this section all
// serialise into arrays, which Elasticsearch handles transparently.
impl_container_mapping!(T for BTreeSet<T>);
impl_container_mapping!(T for Box<T>);
impl_container_mapping!(T for Cell<T>);
impl_container_mapping!(<'a, T> T for Cow<'a, T> where T: Clone);
impl_container_mapping!(T for HashSet<T>);
impl_container_mapping!(T for LinkedList<T>);
impl_container_mapping!(T for Mutex<T>);
impl_container_mapping!(T for RefCell<T>);
impl_container_mapping!(T for Reverse<T>);
impl_container_mapping!(T for RwLock<T>);
impl_container_mapping!(T for Vec<T>);
impl_container_mapping!(T for VecDeque<T>);

#[cfg(feature = "rc")]
impl_container_mapping!(T for std::sync::Arc<T>);
#[cfg(feature = "rc")]
impl_container_mapping!(T for std::rc::Rc<T>);
#[cfg(feature = "rc")]
impl_container_mapping!(T for std::rc::Weak<T>);
#[cfg(feature = "rc")]
impl_container_mapping!(T for std::sync::Weak<T>);
