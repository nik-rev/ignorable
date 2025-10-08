#[derive(ignored_derive::PartialOrd)]
enum A {}

#[derive(PartialOrd)]
enum B {}

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
