#![allow(clippy::non_canonical_partial_ord_impl)]
#![allow(clippy::derive_ord_xor_partial_ord)]
#![allow(dead_code)]

pub mod hello {
    #[derive(Debug)]
    struct Foo;
}

use Ord as StdTrait;
use ignored_derive::Ord as Trait;

check!(empty);
check!(r#enum);
check!(enum_empty);
check!(named_struct);
check!(tuple_struct);

macro_rules! check {
    ($path:tt) => {
        mod $path;

        impl PartialEq for $path::A {
            fn eq(&self, _other: &Self) -> bool {
                unimplemented!()
            }
        }
        impl PartialEq for $path::B {
            fn eq(&self, _other: &Self) -> bool {
                unimplemented!()
            }
        }
        impl Eq for $path::A {}
        impl Eq for $path::B {}
        impl PartialOrd for $path::A {
            fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
                unimplemented!()
            }
        }
        impl PartialOrd for $path::B {
            fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
                unimplemented!()
            }
        }
    };
}
use check;
