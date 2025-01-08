#![allow(dead_code)]

use mapstic::ToMapping;

#[derive(ToMapping)]
struct Foo {
    #[mapstic(params(on_container = true))]
    tuple: Tuple,
}

#[derive(ToMapping)]
#[mapstic(params(on_struct = true))]
struct Tuple(#[mapstic(mapping_type = "keyword")] String);

fn main() -> anyhow::Result<()> {
    serde_json::to_writer_pretty(std::io::stdout(), &Foo::to_mapping())?;
    Ok(())
}
