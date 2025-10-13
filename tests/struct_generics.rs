use ignorable::{Debug, Hash, Ord, PartialEq, PartialOrd};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct GenericStruct<T, I> {
    data: T,
    #[ignored(PartialEq, PartialOrd, Ord, Debug, Hash)]
    #[allow(unused)]
    internal: I,
}

struct Noop;

#[test]
fn test() {
    let _s = GenericStruct {
        data: 42u32,
        internal: Noop,
    };
}
