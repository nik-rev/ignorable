#[derive(ignored_derive::PartialOrd)]
struct A {}

#[derive(PartialOrd)]
struct B {}

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
