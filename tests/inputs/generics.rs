#![allow(dead_code)]

use std::fmt::{Debug, Display};

use mapstic::ToMapping;

#[derive(ToMapping)]
pub struct ExtraBound<T>
where
    T: ToMapping + Clone + Debug,
{
    inner: T,
}

#[derive(ToMapping)]
pub struct ExtraTypes<T, U>
where
    T: ToMapping,
    U: Display,
{
    #[mapstic(skip)]
    display: U,

    inner: T,
}

#[derive(ToMapping)]
pub struct TypeBound<T: ToMapping> {
    inner: T,
}

#[derive(ToMapping)]
pub struct WhereBound<T>
where
    T: ToMapping,
{
    inner: T,
}
