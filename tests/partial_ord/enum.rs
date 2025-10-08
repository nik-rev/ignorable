#[derive(ignored_derive::PartialOrd)]
enum A {
    Foo,
    Bar(u32, String, u32),
    Baz {
        field1: u32,
        field2: String,
        field3: u32,
    },
}

#[derive(PartialOrd)]
enum B {
    Foo,
    Bar(u32, String, u32),
    Baz {
        field1: u32,
        field2: String,
        field3: u32,
    },
}

impl PartialEq for A {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}
impl PartialEq for B {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}
