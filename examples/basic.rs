#![allow(dead_code)]

use std::net::IpAddr;

use chrono::{DateTime, Utc};
use mapstic::ToMapping;

#[derive(ToMapping)]
struct Foo {
    id: i64,
    ip: IpAddr,

    #[mapstic(
        mapping_type = "keyword",
        params(
            x = 12,
            store(x = 12, foo = "bar"),
            fields(raw = "raw", search = "search"),
        )
    )]
    keyword: String,

    nested: Nested,
    #[mapstic(params(on_container = true))]
    opaque: Opaque,

    #[mapstic(skip)]
    skipped: (),

    #[mapstic(params(on_container = false))]
    tuple: Tuple,
}

#[derive(ToMapping)]
struct Nested {
    from: DateTime<Utc>,
    to: Option<DateTime<Utc>>,
}

#[derive(ToMapping)]
#[mapstic(mapping_type = "text", params(fields(raw = "raw")))]
struct Opaque {
    not_exposed: String,
}

#[derive(ToMapping)]
struct Tuple(String);

fn main() -> anyhow::Result<()> {
    serde_json::to_writer_pretty(std::io::stdout(), &Foo::to_mapping())?;
    Ok(())
}
