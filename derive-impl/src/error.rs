use thiserror::Error;

/// Errors that may be returned when deriving [`mapstic_core::ToMapping`].
#[derive(Debug, Error)]
pub enum Error {
    #[error(
        "{}; {}, {}",
        "all fields in the struct were skipped",
        "consider using #[mapstic(skip)] to skip the field",
        "or #[mapstic(mapping_type = ...)] to specify the desired type"
    )]
    AllSkipped,

    #[error(
        "{}; {}",
        "cannot generate mapping for enums",
        "consider using #[mapstic(mapping_type = ...)] to specify an explicit mapping type"
    )]
    Enum,

    // Left implied: how did this even get parsed in the first place?
    #[error("a tuple field was found in a named struct")]
    MixedStruct,

    #[error(
        "{}; {}, {}",
        "a type cannot be derived from tuple structs with multiple fields",
        "consider using #[mapstic(mapping_type = ...)] to specify the desired type",
        "or #[mapstic(skip)] to ignore the field"
    )]
    TupleStruct,

    #[error(
        "{}; {}, {}",
        "a type cannot be derived from unit structs",
        "consider using #[mapstic(mapping_type = ...)] to specify the desired type",
        "or #[mapstic(skip)] to ignore the field"
    )]
    UnitStruct,
}
