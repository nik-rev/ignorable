use ignorable::{Debug, Hash, Ord, PartialEq, PartialOrd};
use std::cmp::Ordering;
use std::hash::{DefaultHasher, Hash as StdHash, Hasher};

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
enum MyEnum {
    Unit,
    Tuple(u32, #[ignored(PartialEq, Hash)] u32),
    Struct {
        id: i32,
        #[ignored(PartialOrd, Debug, Ord)]
        data: String,
    },
    #[allow(dead_code)]
    Other,
}

#[test]
fn main() {
    let t1 = MyEnum::Tuple(1, 100);
    let t2 = MyEnum::Tuple(1, 200);

    // PartialEq ignores field 1
    assert_eq!(t1, t2);
    let mut h1 = DefaultHasher::new();
    let mut h2 = DefaultHasher::new();
    t1.hash(&mut h1);
    t2.hash(&mut h2);
    // Hash ignores field 1
    assert_eq!(h1.finish(), h2.finish());

    // Equality/Hash check across variants
    assert_ne!(MyEnum::Unit, t1);

    let s1 = MyEnum::Struct {
        id: 5,
        data: "a".to_string(),
    };
    let s2 = MyEnum::Struct {
        id: 5,
        data: "b".to_string(),
    };

    // field `data` is ignored
    assert_eq!(s1.partial_cmp(&s2), Some(Ordering::Equal));
    assert_eq!(s1.cmp(&s2), Ordering::Equal);

    let u = MyEnum::Unit;
    let t = MyEnum::Tuple(1, 1);
    assert_eq!(u.cmp(&t), Ordering::Less);

    let d1 = MyEnum::Struct {
        id: 1,
        data: "hidden".to_string(),
    };
    let debug_d1 = format!("{d1:?}");
    assert!(debug_d1.contains("id: 1"));
    // field `data` ignored
    assert!(!debug_d1.contains("data"));
}
