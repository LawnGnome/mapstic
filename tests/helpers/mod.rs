macro_rules! assert_snapshot {
    ($($arg:tt)*) => {
        insta::with_settings!(
            { sort_maps => true},
            { insta::assert_yaml_snapshot!($($arg)*); }
        );
    };
}

pub(crate) use assert_snapshot;
