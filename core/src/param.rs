use indexmap::IndexMap;
use serde::Serialize;

/// Collection of field mapping parameters.
#[derive(Debug, Clone, Default, Serialize)]
pub struct Params {
    #[serde(flatten)]
    options: IndexMap<&'static str, Value>,
}

impl Params {
    /// Extends the options from another option collection.
    pub fn extend(&mut self, other: Self) {
        self.options.extend(other.options)
    }

    /// Inserts an option into the collection.
    pub fn insert(&mut self, name: &'static str, value: Value) -> Option<Value> {
        self.options.insert(name, value)
    }

    /// Returns an iterator over the collection.
    pub fn iter(&self) -> impl Iterator<Item = (&'static str, &Value)> {
        self.options.iter().map(|(name, value)| (*name, value))
    }
}

impl IntoIterator for Params {
    type Item = (&'static str, Value);
    type IntoIter = <IndexMap<&'static str, Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.options.into_iter()
    }
}

impl FromIterator<(&'static str, Value)> for Params {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (&'static str, Value)>,
    {
        Self {
            options: iter.into_iter().collect(),
        }
    }
}

/// A value for a specific field mapping option.
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),

    // It's convenient to reuse this type both in the proc macro and the user facing API, but
    // there's an implementation difference: the proc macro needs to be able to deal with owned
    // strings, since we're building this from `Ident` instances, but the user API gets generated
    // with string literals so we don't need to bother instantiating `String`s at runtime. We'll
    // define slightly different variants to handle each case, hidden behind a feature.
    //
    // First up, the public API.
    String(&'static str),
    Nested(IndexMap<&'static str, Value>),

    // Now the proc macro specific versions, only defined when the `proc-macro` feature is enabled.
    #[cfg(feature = "proc-macro")]
    OwnedString(String),
    #[cfg(feature = "proc-macro")]
    OwnedNested(IndexMap<String, Value>),
}
