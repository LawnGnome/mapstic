//! Test as many auto-derived field types as possible.

use helpers::assert_snapshot;
use inputs::tuple::Tuple;
use mapstic::ToMapping;

mod helpers;
mod inputs;

#[test]
fn auto() {
    assert_snapshot!(Tuple::to_mapping());
}
