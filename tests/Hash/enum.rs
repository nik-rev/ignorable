#[derive(crate::Trait)]
pub enum A {
    Foo,
    Bar(u32, String, u32),
    Baz {
        field1: u32,
        field2: String,
        field3: u32,
    },
}

#[derive(crate::StdTrait)]
pub enum B {
    Foo,
    Bar(u32, String, u32),
    Baz {
        field1: u32,
        field2: String,
        field3: u32,
    },
}
