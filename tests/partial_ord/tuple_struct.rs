#[derive(ignored_derive::PartialOrd)]
struct A(u32, #[ignored(PartialOrd)] String, u32, u32, u32);

#[derive(std::cmp::PartialOrd)]
struct B(u32, String, u32, u32, u32);

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
