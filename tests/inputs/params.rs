#![allow(dead_code)]

use mapstic::ToMapping;

#[derive(ToMapping)]
// We'll test each possible literal type that is supported.
#[mapstic(
    mapping_type = "text",
    params(
        str = "str",
        char = 'c',
        int = -42,
        uint = 9_223_372_036_854_775_808u64,
        float = 42.1,
        truthy,
        actually_true = true,
        falsey = false,
        nested(str = "str", also_true, sub(subsub(wub = "wub wub wub"))),
    )
)]
// Tuple structs are normally unsupported, but with the explicit mapping_type this should work.
pub struct Params(String, Vec<u8>);

#[derive(ToMapping)]
#[mapstic(params(on_container = true))]
pub struct Override {
    #[mapstic(params(on_field = true))]
    field: Field,
}

#[derive(ToMapping)]
#[mapstic(params(on_sub_container = true, on_container = false))]
pub struct Field {
    value: i32,
}
