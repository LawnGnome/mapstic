//! Test as many auto-derived field types as possible.

use helpers::assert_snapshot;
use inputs::auto::Auto;
use mapstic::ToMapping;

mod helpers;
mod inputs;

#[test]
fn auto() {
    assert_snapshot!(Auto::to_mapping());
}
