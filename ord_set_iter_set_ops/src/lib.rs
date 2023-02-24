use std::{
    cmp::Ordering,
    collections::{
        btree_map::{self, BTreeMap},
        btree_set::{self, BTreeSet},
    },
    convert::Infallible,
    iter::Peekable,
    marker::PhantomData,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

/// Ordered Iterator over set operations on the contents of an ordered set.
pub trait PeepAdvanceIter<'a, T: 'a + Ord>: Iterator<Item = &'a T> {
    /// Peep at the next item in the iterator without advancing the iterator.
    fn peep(&mut self) -> Option<&'a T>;

    /// Advance this iterator to the next item at or after the given item.
    /// Default implementation is O(n) but custom built implementations could be as good as O(log(n)).
    // TODO: try to make advance_until() return &mut Self
    fn advance_until(&mut self, t: &T) {
        while let Some(item) = self.peep() {
            if t > item {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Advance this iterator to the next item at or after the given item.
    /// Default implementation is O(n) but custom built implementations could be as good as O(log(n)).
    // TODO: try to make advance_until() return &mut Self
    fn advance_after(&mut self, t: &T) {
        while let Some(item) = self.peep() {
            if t >= item {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Get the next element equal to or greater than the target
    fn next_at_or_after(&mut self, target: &T) -> Option<&'a T> {
        self.skip_while(|x| x < &target).next()
    }
}

pub struct PeepAdvanceAdapter<I: Iterator> {
    iter: I,
    next_result: Option<I::Item>,
}

impl<I: Iterator> From<I> for PeepAdvanceAdapter<I> {
    fn from(mut iter: I) -> Self {
        let next_result = iter.next();
        Self { iter, next_result }
    }
}

impl<I: Iterator> Iterator for PeepAdvanceAdapter<I>
where
    I::Item: Clone,
{
    type Item = <I as Iterator>::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(result) = self.next_result.clone() {
            self.next_result = self.iter.next();
            Some(result)
        } else {
            None
        }
    }
}

impl<'a, T, I> PeepAdvanceIter<'a, T> for PeepAdvanceAdapter<I>
where
    T: Ord + 'a + Clone,
    I: Iterator<Item = &'a T> + Clone,
{
    #[inline]
    fn peep(&mut self) -> Option<&'a T> {
        self.next_result.clone()
    }
}

impl<'a, T, I> PeepAdvanceIter<'a, T> for Peekable<I>
where
    T: 'a + Ord,
    I: Iterator<Item = &'a T>,
{
    fn peep(&mut self) -> Option<&'a T> {
        self.peek().copied()
    }
}

pub trait SetOperations<'a, T: 'a + Ord>: Sized {
    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn difference(self, iter: Self) -> OrdSetOpsIter<'a, T>;

    /// Iterate over the set intersection of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn intersection(self, iter: Self) -> OrdSetOpsIter<'a, T>;

    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn symmetric_difference(self, iter: Self) -> OrdSetOpsIter<'a, T>;

    /// Iterate over the set union of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn union(self, iter: Self) -> OrdSetOpsIter<'a, T>;
}

pub trait SetRelationShips<'a, T: 'a + Ord>: Sized {
    /// Is the output of the given Iterator disjoint from the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    fn is_disjoint(&self, other: &'a Self) -> bool;

    /// Is the output of the given Iterator a proper subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    fn is_proper_subset(&self, other: &'a Self) -> bool;

    /// Is the output of the given Iterator a proper superset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    fn is_proper_superset(&self, other: &'a Self) -> bool;

    /// Is the output of the given Iterator a subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    fn is_subset(&self, other: &'a Self) -> bool;

    /// Is the output of the given Iterator a superset of the output of
    /// this iterator?
    fn is_superset(&self, other: &'a Self) -> bool;
}

pub trait IterSetOperations<'a, T: 'a + Ord>:
    SetOperations<'a, T> + PeepAdvanceIter<'a, T> + Sized
{
}

pub trait IterSetRelationships<'a, T: 'a + Ord>: PeepAdvanceIter<'a, T> + Sized {
    #[allow(clippy::wrong_self_convention)]
    fn is_disjoint(mut self, mut other: Self) -> bool {
        loop {
            if let Some(my_item) = self.peep() {
                if let Some(other_item) = other.peep() {
                    match my_item.cmp(other_item) {
                        Ordering::Less => {
                            self.advance_until(other_item);
                        }
                        Ordering::Greater => {
                            self.advance_until(my_item);
                        }
                        Ordering::Equal => {
                            return false;
                        }
                    }
                } else {
                    return true;
                }
            } else {
                return true;
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_proper_subset(mut self, mut other: Self) -> bool {
        let mut result = false;
        while let Some(my_item) = self.peep() {
            if let Some(other_item) = other.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        return false;
                    }
                    Ordering::Greater => {
                        result = true;
                        other.advance_until(my_item);
                    }
                    Ordering::Equal => {
                        other.next();
                        self.next();
                    }
                }
            } else {
                return false;
            }
        }
        result
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_proper_superset(mut self, mut other: Self) -> bool {
        let mut result = false;
        while let Some(my_item) = self.peep() {
            if let Some(other_item) = other.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        result = true;
                        self.advance_until(other_item);
                    }
                    Ordering::Greater => {
                        return false;
                    }
                    Ordering::Equal => {
                        other.next();
                        self.next();
                    }
                }
            } else {
                return false;
            }
        }
        result
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_subset(mut self, mut other: Self) -> bool {
        while let Some(my_item) = self.peep() {
            if let Some(other_item) = other.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        return false;
                    }
                    Ordering::Greater => {
                        other.advance_until(my_item);
                    }
                    Ordering::Equal => {
                        other.next();
                        self.next();
                    }
                }
            } else {
                return false;
            }
        }
        true
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_superset(mut self, mut other: Self) -> bool {
        while let Some(my_item) = self.peep() {
            if let Some(other_item) = other.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        self.advance_until(other_item);
                    }
                    Ordering::Greater => {
                        return false;
                    }
                    Ordering::Equal => {
                        other.next();
                        self.next();
                    }
                }
            } else {
                return false;
            }
        }
        true
    }
}

pub enum OrdSetOpsIter<'a, T>
where
    T: Ord,
{
    Difference(
        Box<Self>,
        Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>,
    ),
    Intersection(
        Box<Self>,
        Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>,
    ),
    SymmetricDifference(
        Box<Self>,
        Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>,
    ),
    Union(
        Box<Self>,
        Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>,
    ),
    Plain(Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>),
    _Phantom(Infallible, PhantomData<&'a T>),
}

impl<'a, T> PeepAdvanceIter<'a, T> for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    fn peep(&mut self) -> Option<&'a T> {
        use OrdSetOpsIter::*;
        match self {
            Difference(ref mut self_iter, ref mut other_iter) => {
                while let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return Some(l_item);
                            }
                            Ordering::Greater => {
                                other_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                self_iter.next();
                                other_iter.next();
                            }
                        }
                    } else {
                        return Some(l_item);
                    }
                }
                None
            }
            Intersection(ref mut self_iter, ref mut other_iter) => {
                if let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                self_iter.advance_until(r_item);
                                self_iter.peep()
                            }
                            Ordering::Greater => {
                                other_iter.advance_until(l_item);
                                other_iter.peep()
                            }
                            Ordering::Equal => Some(l_item),
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            SymmetricDifference(ref mut self_iter, ref mut other_iter) => {
                while let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return Some(l_item);
                            }
                            Ordering::Greater => {
                                return Some(r_item);
                            }
                            Ordering::Equal => {
                                self_iter.next();
                                other_iter.next();
                            }
                        }
                    } else {
                        return Some(l_item);
                    }
                }
                other_iter.peep()
            }
            Union(ref mut self_iter, ref mut other_iter) => {
                if let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less | Ordering::Equal => Some(l_item),
                            Ordering::Greater => Some(r_item),
                        }
                    } else {
                        Some(l_item)
                    }
                } else {
                    other_iter.peep()
                }
            }
            Plain(ref mut iter) => iter.peep(),
            _ => None,
        }
    }

    fn advance_until(&mut self, target: &T) {
        match self {
            Self::Difference(ref mut self_iter, ref mut other_iter) => {
                self_iter.advance_until(target);
                other_iter.advance_until(target);
            }
            Self::Intersection(ref mut self_iter, ref mut other_iter) => {
                self_iter.advance_until(target);
                other_iter.advance_until(target);
            }
            Self::SymmetricDifference(ref mut self_iter, ref mut other_iter) => {
                self_iter.advance_until(target);
                other_iter.advance_until(target);
            }
            Self::Union(ref mut self_iter, ref mut other_iter) => {
                self_iter.advance_until(target);
                other_iter.advance_until(target);
            }
            Self::Plain(ref mut iter) => {
                iter.advance_until(target);
            }
            _ => (),
        }
    }
}

impl<'a, T> SetOperations<'a, T> for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    fn difference(self, other: Self) -> OrdSetOpsIter<'a, T> {
        self.sub(other)
    }

    fn intersection(self, other: Self) -> OrdSetOpsIter<'a, T> {
        self.bitand(other)
    }

    fn symmetric_difference(self, other: Self) -> OrdSetOpsIter<'a, T> {
        self.bitxor(other)
    }

    fn union(self, other: Self) -> OrdSetOpsIter<'a, T> {
        self.bitor(other)
    }
}

impl<'a, T> Iterator for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Difference(ref mut self_iter, ref mut other_iter) => {
                while let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return self_iter.next();
                            }
                            Ordering::Greater => {
                                other_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                self_iter.next();
                                other_iter.next();
                            }
                        }
                    } else {
                        return self_iter.next();
                    }
                }
                None
            }
            Self::Intersection(ref mut self_iter, ref mut other_iter) => {
                while let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                self_iter.advance_until(r_item);
                            }
                            Ordering::Greater => {
                                other_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                other_iter.next();
                                return self_iter.next();
                            }
                        }
                    } else {
                        return None;
                    }
                }
                None
            }
            Self::SymmetricDifference(ref mut self_iter, ref mut other_iter) => {
                while let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return self_iter.next();
                            }
                            Ordering::Greater => {
                                return other_iter.next();
                            }
                            Ordering::Equal => {
                                self_iter.next();
                                other_iter.next();
                            }
                        }
                    } else {
                        return self_iter.next();
                    }
                }
                other_iter.next()
            }
            Self::Union(ref mut self_iter, ref mut other_iter) => {
                if let Some(l_item) = self_iter.peep() {
                    if let Some(r_item) = other_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => self_iter.next(),
                            Ordering::Greater => other_iter.next(),
                            Ordering::Equal => {
                                other_iter.next();
                                self_iter.next()
                            }
                        }
                    } else {
                        self_iter.next()
                    }
                } else {
                    other_iter.next()
                }
            }
            Self::Plain(ref mut iter) => iter.next(),
            _ => None,
        }
    }
}

impl<'a, T> BitAnd for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    type Output = Self;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        Self::Intersection(Box::new(self), Box::new(other))
    }
}

impl<'a, T> BitOr for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    type Output = Self;

    #[inline]
    fn bitor(self, other: Self) -> Self::Output {
        Self::Union(Box::new(self), Box::new(other))
    }
}

impl<'a, T> BitXor for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    type Output = Self;

    #[inline]
    fn bitxor(self, other: Self) -> Self::Output {
        Self::SymmetricDifference(Box::new(self), Box::new(other))
    }
}

impl<'a, T> Sub for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    type Output = Self;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        Self::Difference(Box::new(self), Box::new(other))
    }
}

pub trait SetOsoIterAdaption<'a, T: 'a + Ord, I>
where
    T: 'a + Ord + Clone,
    I: Iterator<Item = &'a T> + Clone,
{
    fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T>;

    fn oso_difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().difference(other.oso_iter())
    }

    fn oso_intersection(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().intersection(other.oso_iter())
    }

    fn oso_symmetric_difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().symmetric_difference(other.oso_iter())
    }

    fn oso_union(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().union(other.oso_iter())
    }
}

impl<'a, T: 'a + Ord + Clone> SetOsoIterAdaption<'a, T, btree_set::Iter<'a, T>> for BTreeSet<T> {
    fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T> {
        OrdSetOpsIter::Plain(Box::new(PeepAdvanceAdapter::from(self.iter())))
    }
}

pub trait MapOsoIterAdaption<'a, K, I>
where
    K: 'a + Ord + Clone,
    I: Iterator<Item = &'a K> + Clone,
{
    fn oso_keys(&'a self) -> OrdSetOpsIter<'a, K>;
}

impl<'a, K, V> MapOsoIterAdaption<'a, K, btree_map::Keys<'a, K, V>> for BTreeMap<K, V>
where
    K: 'a + Ord + Clone,
{
    fn oso_keys(&'a self) -> OrdSetOpsIter<'a, K> {
        OrdSetOpsIter::Plain(Box::new(PeepAdvanceAdapter::from(self.keys())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    //use crate::OrdSetOpsIterator;

    struct Set<'a, T: 'a + Ord> {
        vec: Vec<T>,
        phantom: PhantomData<&'a T>,
    }

    impl<'a, T: Ord + Clone> From<Vec<T>> for Set<'a, T> {
        fn from(mut elements: Vec<T>) -> Self {
            elements.sort();
            elements.dedup();
            Self {
                vec: elements,
                phantom: PhantomData,
            }
        }
    }

    #[derive(Clone)]
    struct SetIter<'a, T: Ord + Clone> {
        elements: &'a [T],
        index: usize,
    }

    impl<'a, T: Ord + Clone> Iterator for SetIter<'a, T> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(element) = self.elements.get(self.index) {
                self.index += 1;
                Some(element)
            } else {
                None
            }
        }
    }

    impl<'a, T: 'a + Ord + Clone> PeepAdvanceIter<'a, T> for SetIter<'a, T> {
        fn advance_until(&mut self, t: &T) {
            self.index += match self.elements[self.index..].binary_search(t) {
                Ok(index) => index,
                Err(index) => index,
            };
        }

        fn peep(&mut self) -> Option<&'a T> {
            self.elements.get(self.index)
        }
    }

    impl<'a, T: 'a + Ord + Clone> Set<'a, T> {
        pub fn iter(&'a self) -> SetIter<'a, T> {
            SetIter {
                elements: &self.vec,
                index: 0,
            }
        }
    }

    impl<'a, T: 'a + Ord + Clone> SetOsoIterAdaption<'a, T, btree_set::Iter<'a, T>> for Set<'a, T> {
        fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T> {
            OrdSetOpsIter::Plain(Box::new(self.iter()))
        }
    }

    #[test]
    fn oso_iter() {
        let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
        let mut oso_iter = set1.oso_iter();
        assert_eq!(oso_iter.next(), Some(&"a"));
        assert_eq!(oso_iter.next(), Some(&"b"));
        assert_eq!(oso_iter.next(), Some(&"c"));
        assert_eq!(oso_iter.next(), Some(&"d"));
        assert_eq!(oso_iter.next(), None);
    }

    #[test]
    fn advance_until() {
        let set1 = Set::<&str>::from(vec!["a", "b", "c", "d", "e", "f"]);
        let mut oso_iter = set1.oso_iter();
        oso_iter.advance_until(&"c");
        assert_eq!(oso_iter.next(), Some(&"c"));
        assert_eq!(oso_iter.next_at_or_after(&"e"), Some(&"e"));
    }

    #[test]
    fn next_or_after() {
        let set1 = Set::<&str>::from(vec!["a", "b", "c", "d", "e", "f"]);
        let mut oso_iter = set1.oso_iter();
        assert_eq!(oso_iter.next_at_or_after(&"c"), Some(&"c"));
        assert_eq!(oso_iter.next_at_or_after(&"e"), Some(&"e"));
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
    fn oso_iter_b_tree_map() {
        let set1 = BTreeMap::<&str, i32>::from([("a", 1), ("b", 2), ("c", 3), ("d", 4)]);
        let mut oso_iter = set1.oso_keys();
        assert_eq!(oso_iter.next(), Some(&"a"));
        assert_eq!(oso_iter.next(), Some(&"b"));
        assert_eq!(oso_iter.next(), Some(&"c"));
        assert_eq!(oso_iter.next(), Some(&"d"));
        assert_eq!(oso_iter.next(), None);
    }
}
