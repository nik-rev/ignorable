#[derive(ignored_derive::PartialOrd)]
struct A {
    field1: u32,
    field2: String,
    field3: u32,
}

#[derive(std::cmp::PartialOrd)]
struct B {
    field1: u32,
    field2: String,
    field3: u32,
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
