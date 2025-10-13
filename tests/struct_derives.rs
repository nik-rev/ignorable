use ignored_derive::{Debug, Hash, Ord, PartialEq, PartialOrd};
use std::cmp::Ordering;
use std::hash::{DefaultHasher, Hash as StdHash, Hasher};

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
struct NamedStruct {
    a: u32,
    #[ignored(PartialEq, PartialOrd, Ord, Hash)]
    #[allow(dead_code)]
    b: String,
    #[ignored(PartialEq, PartialOrd, Ord, Debug, Hash)]
    #[allow(dead_code)]
    c: u8,
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
struct TupleStruct(
    u32,
    #[allow(dead_code)]
    #[ignored(PartialEq, PartialOrd, Ord, Hash)]
    String,
    #[allow(dead_code)]
    #[ignored(Debug, PartialEq, PartialOrd, Ord, Hash)]
    u8,
);

#[test]
fn struct_tuple() {
    let t1 = TupleStruct(10, "hello".to_string(), 5);
    let t2 = TupleStruct(10, "world".to_string(), 99);

    // Compares fields 0, ignoring fields 1 and 2
    assert_eq!(t1, t2);

    let mut h1 = DefaultHasher::new();
    let mut h2 = DefaultHasher::new();
    t1.hash(&mut h1);
    t2.hash(&mut h2);

    assert_eq!(h1.finish(), h2.finish());

    // Debug: print '0' and '1' but ignore '2'
    let debug_t1 = format!("{t1:?}");
    assert!(debug_t1.contains("10"));
    assert!(debug_t1.contains("\"hello\""));
    assert!(!debug_t1.contains("5"));
}

#[test]
fn struct_named() {
    let s1 = NamedStruct {
        a: 10,
        b: "hello".to_string(),
        c: 5,
    };
    let s2 = NamedStruct {
        a: 10,
        b: "world".to_string(),
        c: 99,
    };

    // Compares fields 0, ignoring fields 1 and 2
    assert_eq!(s1, s2);

    let mut h1 = DefaultHasher::new();
    let mut h2 = DefaultHasher::new();
    s1.hash(&mut h1);
    s2.hash(&mut h2);

    assert_eq!(h1.finish(), h2.finish());

    // Debug: do not print 'c'
    let debug_s1 = format!("{s1:?}");
    assert!(debug_s1.contains("a: 10"));
    assert!(debug_s1.contains("b: \"hello\""));
    assert!(!debug_s1.contains("c: "));

    // PartialOrd/Ord: only compare 'a'
    assert_eq!(s1.cmp(&s2), Ordering::Equal);
}
