#![allow(dead_code)]

use mapstic::ToMapping;

#[derive(ToMapping)]
pub struct Tuple {
    #[mapstic(params(on_container = true))]
    tuple: TupleStruct,
}

#[derive(ToMapping)]
#[mapstic(params(on_struct = true))]
struct TupleStruct(#[mapstic(mapping_type = "keyword")] String);
