#![allow(dead_code)]

use mapstic::ToMapping;

#[derive(ToMapping)]
pub struct Explicit {
    #[mapstic(mapping_type = "keyword")]
    keyword: String,

    // Without a field attribute, the container attribute should be used.
    text: Text,

    // With a mapping_type, we don't need to derive ToMapping on MatchOnlyText.
    #[mapstic(mapping_type = "match_only_text")]
    match_only_text: MatchOnlyText,

    // Also with a mapping type, we _only_ get the options defined in the field attribute.
    #[mapstic(mapping_type = "text", params(analyzer = "simple"))]
    completion_as_text: Completion,

    // Test the merger of options. (The field attribute should win here, so the analyzer should be
    // "simple", but preserve_separators should still be false.)
    #[mapstic(params(analyzer = "simple"))]
    completion: Completion,

    #[mapstic(skip)]
    skipped: String,
}

#[derive(ToMapping)]
#[mapstic(mapping_type = "text")]
struct Text(String);

struct MatchOnlyText(String);

#[derive(ToMapping)]
#[mapstic(
    mapping_type = "completion",
    params(analyzer = "search_analyzer", preserve_separators = false)
)]
struct Completion(String);
