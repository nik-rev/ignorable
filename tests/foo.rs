use ignored_derive::{Debug, Hash, Ord, PartialEq, PartialOrd};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum A {
    Foo,
    Bar(u32, String, u32),
    Baz {
        field1: u32,
        field2: String,
        field3: u32,
    },
}
