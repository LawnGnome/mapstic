//! Test case where there are only explicit types.

use helpers::assert_snapshot;
use inputs::explicit::Explicit;
use mapstic::ToMapping;

mod helpers;
mod inputs;

#[test]
fn explicit() {
    assert_snapshot!(Explicit::to_mapping());
}
