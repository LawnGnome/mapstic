#![allow(dead_code)]

use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    cmp::Reverse,
    collections::{BTreeSet, HashSet, LinkedList, VecDeque},
    ffi::{CStr, CString},
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    num::{
        NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroU16, NonZeroU32, NonZeroU64,
        NonZeroU8,
    },
    sync::{
        atomic::{
            AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicU16, AtomicU32, AtomicU64, AtomicU8,
        },
        Mutex, RwLock,
    },
    time::SystemTime,
};

use mapstic::ToMapping;

#[derive(ToMapping)]
pub struct Auto {
    binary: &'static [u8],
    cstr: &'static CStr,
    cstring: CString,

    boolean: bool,

    normal_i64: i64,
    non_zero_i64: NonZeroI64,
    atomic_i64: AtomicI64,

    normal_u64: u64,
    non_zero_u64: NonZeroU64,
    atomic_u64: AtomicU64,

    normal_i32: i32,
    non_zero_i32: NonZeroI32,
    atomic_i32: AtomicI32,

    normal_u32: u32,
    non_zero_u32: NonZeroU32,
    atomic_u32: AtomicU32,

    normal_i16: i16,
    non_zero_i16: NonZeroI16,
    atomic_i16: AtomicI16,

    normal_u16: u16,
    non_zero_u16: NonZeroU16,
    atomic_u16: AtomicU16,

    normal_i8: i8,
    non_zero_i8: NonZeroI8,
    atomic_i8: AtomicI8,

    normal_u8: u8,
    non_zero_u8: NonZeroU8,
    atomic_u8: AtomicU8,

    double: f64,
    float: f32,

    date: SystemTime,

    ip: IpAddr,
    ipv4: Ipv4Addr,
    ipv6: Ipv6Addr,

    // For containers, we won't run through every possible combination, but the basic ones are
    // useful.
    option: Option<i32>,
    btree_set: BTreeSet<i32>,
    box_: Box<i32>,
    cell: Cell<i32>,
    cow: Cow<'static, i32>,
    hash_set: HashSet<i32>,
    list: LinkedList<i32>,
    mutex: Mutex<i32>,
    ref_cell: RefCell<i32>,
    reverse: Reverse<Vec<IpAddr>>,
    rw_lock: RwLock<i32>,
    vec: Vec<i32>,
    vec_deque: VecDeque<i32>,

    string: String,
    str: &'static str,
    cow_str: Cow<'static, str>,

    nested: Nested,
}

#[derive(ToMapping)]
struct Nested {
    value: i32,
}
