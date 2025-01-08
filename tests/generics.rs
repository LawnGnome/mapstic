//! Test case where there are only explicit types.

use helpers::assert_snapshot;
use inputs::generics::{ExtraBound, ExtraTypes, TypeBound, WhereBound};
use mapstic::ToMapping;

mod helpers;
mod inputs;

#[test]
fn extra_bound() {
    assert_snapshot!(ExtraBound::<bool>::to_mapping());
}

#[test]
fn extra_types() {
    assert_snapshot!(ExtraTypes::<Vec<u8>, String>::to_mapping());
}

#[test]
fn type_bound() {
    assert_snapshot!(TypeBound::<i32>::to_mapping());
}

#[test]
fn where_bound() {
    assert_snapshot!(WhereBound::<u32>::to_mapping());
}
