# Mapstic

Mapstic provides a derive macro that allows for [explicit Elasticsearch
mappings][explicit] to be generated based on Rust types.

[Full documentation is available on docs.rs][docs], but here's a simple example
of how this looks in practice:

```rust
use mapstic::ToMapping;

#[derive(ToMapping)]
struct MyIndex {
    id: u64,
    score: f64,
    time: std::time::SystemTime,
    tags: Vec<Tag>,
}

#[derive(ToMapping)]
struct Tag(String);
```

The `ToMapping` trait then provides a `to_mapping()` method on the type that
can be used to get a `Mapping` that implements `serde::Serialize` and can be
provided directly to Elasticsearch:

```rust
use elasticsearch::{Elasticsearch, indices::IndicesPutMappingParts};

let client = Elasticsearch::default();
let mapping = MyIndex::to_mapping();
let response = client
    .indices()
    .put_mapping(IndicesPutMappingParts::Index(&["my-index"]))
    .body(mapping)
    .send()
    .await;
```

Development of this project was supported by the [Rust
Foundation][rust-foundation].

## Maintenance status and future plans

Right now, I don't really have too many plans. This was developed for a
specific project, and implements all the functionality needed for that project.

This could easily be used as the base for a full blown Elasticsearch mapping
migration tool, but since I personally don't need that, I won't be writing it.

I'm happy to review PRs for things like:

- Implementing support for other serialisers than Serde.
- Adding feature gated support to derive mappings for other common ecosystem
  crates, such as `time` or `jiff`.
- Handling weird corner cases in generating JSON. (For example, there's no way
  to express an array right now in a `#[mapstic(params(...))]` attribute,
  mostly because it seemed hard to do with Darling and it wasn't necessary for
  any common field parameters.)

Since this was also my first "real" derive macro project, and first time using
Darling at all, it's inevitable that I've done some of it badly. Feedback
welcome!

---

## [Code of Conduct][code-of-conduct]

The [Rust Foundation][rust-foundation] has adopted a Code of Conduct that we
expect project participants to adhere to. Please read [the full
text][code-of-conduct] so that you can understand what actions will and will not
be tolerated.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Licenses

Rust is primarily distributed under the terms of both the MIT license and the
Apache License (Version 2.0), with documentation portions covered by the
Creative Commons Attribution 4.0 International license..

See [LICENSE-APACHE](LICENSE-APACHE), [LICENSE-MIT](LICENSE-MIT),
[LICENSE-documentation](LICENSE-documentation), and
[COPYRIGHT](COPYRIGHT) for details.

You can also read more under the Foundation's [intellectual property
policy][ip-policy].

## Other Policies

You can read about other Rust Foundation policies in the footer of the Foundation
[website][foundation-website].

[code-of-conduct]: https://foundation.rust-lang.org/policies/code-of-conduct/
[docs]: https://docs.rs/mapstic/latest/mapstic/
[explicit]: https://www.elastic.co/guide/en/elasticsearch/reference/current/explicit-mapping.html
[foundation-website]: https://foundation.rust-lang.org
[ip-policy]: https://foundation.rust-lang.org/policies/intellectual-property-policy/
[media-guide and trademark]: https://foundation.rust-lang.org/policies/logo-policy-and-media-guide/
[rust-foundation]: https://foundation.rust-lang.org/
