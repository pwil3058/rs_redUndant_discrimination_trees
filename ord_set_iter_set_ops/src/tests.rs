// Copyright 2023 Peter Williams <pwil3058@gmail.com> <pwil3058@bigpond.net.au>
use super::ord_list_set::*;
use super::*;

#[test]
fn oso_iter_ord_list_set() {
    let set1 = OrdListSet::<&str>::from(["a", "b", "c", "d"]);
    let mut oso_iter = set1.oso_iter();
    assert_eq!(oso_iter.next(), Some(&"a"));
    assert_eq!(oso_iter.next(), Some(&"b"));
    assert_eq!(oso_iter.next(), Some(&"c"));
    assert_eq!(oso_iter.next(), Some(&"d"));
    assert_eq!(oso_iter.next(), None);
}

#[test]
fn advance_until_ord_list_set() {
    let set1 = OrdListSet::<&str>::from(["a", "b", "c", "d", "e", "f"]);
    let mut oso_iter = set1.oso_iter();
    oso_iter.advance_until(&"c");
}

#[test]
fn oso_iter_b_tree_set() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let mut oso_iter = set1.oso_iter();
    assert_eq!(oso_iter.next(), Some(&"a"));
    assert_eq!(oso_iter.next(), Some(&"b"));
    assert_eq!(oso_iter.next(), Some(&"c"));
    assert_eq!(oso_iter.next(), Some(&"d"));
    assert_eq!(oso_iter.next(), None);
}

#[test]
fn is_disjoint_btree_set() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = BTreeSet::<&str>::from(["a", "c", "e", "g"]);
    let set3 = BTreeSet::<&str>::from(["b", "d", "e", "f"]);
    let set4 = BTreeSet::<&str>::from(["b", "d", "f", "h"]);
    let empty_set = BTreeSet::<&str>::new();
    debug_assert!(!set1.oso_iter().is_disjoint(&set1.oso_iter()));
    debug_assert!(!set2.oso_iter().is_disjoint(&set3.oso_iter()));
    debug_assert!(set2.oso_iter().is_disjoint(&set4.oso_iter()));
    debug_assert!(set4.oso_iter().is_disjoint(&set2.oso_iter()));
    debug_assert!(set4.oso_iter().is_disjoint(&empty_set.oso_iter()));
    debug_assert!(empty_set.oso_iter().is_disjoint(&set2.oso_iter()));
    debug_assert_eq!(
        set2.oso_iter().is_disjoint(&set3.oso_iter()),
        set2.oso_iter()
            .intersection(&set3.oso_iter())
            .next()
            .is_none()
    );
    debug_assert_eq!(
        set2.oso_iter().is_disjoint(&set4.oso_iter()),
        set2.oso_iter()
            .intersection(&set4.oso_iter())
            .next()
            .is_none()
    );
}

#[test]
fn is_disjoint_ord_list_set() {
    let set1 = OrdListSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = OrdListSet::<&str>::from(["a", "c", "e", "g"]);
    let set3 = OrdListSet::<&str>::from(["b", "d", "e", "f"]);
    let set4 = OrdListSet::<&str>::from(["b", "d", "f", "h"]);
    let empty_set = OrdListSet::<&str>::empty_set();
    debug_assert!(!set1.oso_iter().is_disjoint(&set1.oso_iter()));
    debug_assert!(!set2.oso_iter().is_disjoint(&set3.oso_iter()));
    debug_assert!(set2.oso_iter().is_disjoint(&set4.oso_iter()));
    debug_assert!(set4.oso_iter().is_disjoint(&set2.oso_iter()));
    debug_assert!(set4.oso_iter().is_disjoint(&empty_set.oso_iter()));
    debug_assert!(empty_set.oso_iter().is_disjoint(&set2.oso_iter()));
    debug_assert_eq!(
        set2.oso_iter().is_disjoint(&set3.oso_iter()),
        set2.oso_iter()
            .intersection(&set3.oso_iter())
            .next()
            .is_none()
    );
    debug_assert_eq!(
        set2.oso_iter().is_disjoint(&set4.oso_iter()),
        set2.oso_iter()
            .intersection(&set4.oso_iter())
            .next()
            .is_none()
    );
}

#[test]
fn is_superset() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = BTreeSet::<&str>::from(["b", "c", "d"]);
    let set3 = BTreeSet::<&str>::from(["a", "b", "c", "d", "e"]);
    let empty_set = BTreeSet::<&str>::new();
    debug_assert!(empty_set.oso_iter().is_superset(&empty_set.oso_iter()));
    debug_assert!(set1.oso_iter().is_superset(&empty_set.oso_iter()));
    debug_assert!(set1.oso_iter().is_superset(&set1.oso_iter()));
    debug_assert!(set1.oso_iter().is_superset(&set2.oso_iter()));
    debug_assert!(!set1.oso_iter().is_superset(&set3.oso_iter()));
}

#[test]
fn is_proper_superset() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = BTreeSet::<&str>::from(["b", "c", "d"]);
    let set3 = BTreeSet::<&str>::from(["a", "b", "c", "d", "e"]);
    let empty_set = BTreeSet::<&str>::new();
    debug_assert!(!empty_set
        .oso_iter()
        .is_proper_superset(&empty_set.oso_iter()));
    debug_assert!(set1.oso_iter().is_proper_superset(&empty_set.oso_iter()));
    debug_assert!(!set1.oso_iter().is_proper_superset(&set1.oso_iter()));
    debug_assert!(set1.oso_iter().is_proper_superset(&set2.oso_iter()));
    debug_assert!(!set1.oso_iter().is_proper_superset(&set3.oso_iter()));
}

#[test]
fn is_subset() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = BTreeSet::<&str>::from(["b", "c", "d"]);
    let set3 = BTreeSet::<&str>::from(["a", "b", "c", "d", "e"]);
    let empty_set = BTreeSet::<&str>::new();
    debug_assert!(empty_set.oso_iter().is_subset(&empty_set.oso_iter()));
    debug_assert!(!set1.oso_iter().is_subset(&empty_set.oso_iter()));
    debug_assert!(set1.oso_iter().is_subset(&set1.oso_iter()));
    debug_assert!(!set1.oso_iter().is_subset(&set2.oso_iter()));
    debug_assert!(set1.oso_iter().is_subset(&set3.oso_iter()));
}

#[test]
fn is_proper_subset() {
    let set1 = BTreeSet::<&str>::from(["a", "b", "c", "d"]);
    let set2 = BTreeSet::<&str>::from(["b", "c", "d"]);
    let set3 = BTreeSet::<&str>::from(["a", "b", "c", "d", "e"]);
    let empty_set = BTreeSet::<&str>::new();
    debug_assert!(!empty_set.oso_iter().is_proper_subset(&empty_set.oso_iter()));
    debug_assert!(!set1.oso_iter().is_proper_subset(&empty_set.oso_iter()));
    debug_assert!(!set1.oso_iter().is_proper_subset(&set1.oso_iter()));
    debug_assert!(!set1.oso_iter().is_proper_subset(&set2.oso_iter()));
    debug_assert!(set1.oso_iter().is_proper_subset(&set3.oso_iter()));
}

#[test]
fn oso_iter_b_tree_map() {
    let set1 = BTreeMap::<&str, i32>::from([("a", 1), ("b", 2), ("c", 3), ("d", 4)]);
    let mut oso_iter = set1.oso_keys();
    assert_eq!(oso_iter.next(), Some(&"a"));
    assert_eq!(oso_iter.next(), Some(&"b"));
    assert_eq!(oso_iter.next(), Some(&"c"));
    assert_eq!(oso_iter.next(), Some(&"d"));
    assert_eq!(oso_iter.next(), None);
}
