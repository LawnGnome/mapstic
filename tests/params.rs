//! Test case that exercises option parsing pathways.

use helpers::assert_snapshot;
use inputs::params::{Override, Params};
use mapstic::ToMapping;

mod helpers;
mod inputs;

#[test]
fn over_ride() {
    assert_snapshot!(Override::to_mapping());
}

#[test]
fn params() {
    assert_snapshot!(Params::to_mapping());
}
