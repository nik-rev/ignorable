// tests/ui/03-struct-generics.rs

use ignored_derive::{Debug, Hash, Ord, PartialEq, PartialOrd};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct GenericStruct<T, I> {
    data: T,
    #[ignored(PartialEq, PartialOrd, Ord, Debug, Hash)]
    #[allow(unused)]
    internal: I,
}

struct Noop;

fn main() {
    // This must compile successfully, proving that `I` (which is `Noop`)
    // does not require any trait bounds, as it is ignored.
    let _s = GenericStruct {
        data: 42u32,
        internal: Noop,
    };
}
