// Copyright 2020 Peter Williams <pwil3058@gmail.com> <pwil3058@bigpond.net.au>
//! Sets implemented as a sorted list.

use std::collections::BTreeSet;
use std::{
    iter::FromIterator,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

use ord_set_iter_set_ops::{self, OrdSetOpsIter, PeepAdvanceIter};

/// A set of items of type T ordered according to Ord (with no duplicates)
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrdListSet<T: Ord> {
    pub members: Vec<T>,
}

impl<T: Ord> Default for OrdListSet<T> {
    fn default() -> Self {
        Self {
            members: Vec::new(),
        }
    }
}

impl<T: Ord> OrdListSet<T> {
    pub fn empty_set() -> Self {
        Self::default()
    }
}

impl<T: Ord> OrdListSet<T> {
    /// Return number of members in this set.
    pub fn len(&self) -> usize {
        self.members.len()
    }

    /// Return `true` if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }

    /// Return an iterator over the members in the `OrdListSet` in ascending order.
    pub fn iter<'a>(&'a self) -> OrdSetOpsIter<'a, T> {
        OrdSetOpsIter::Plain(Box::new(OrdListSetIter {
            elements: &self.members,
            index: 0,
        }))
    }
}

// set functions that don't modify the set
impl<'a, T: 'a + Ord> OrdListSet<T> {
    ///Returns true if the set contains an element equal to the value.
    pub fn contains(&self, item: &T) -> bool {
        self.members.binary_search(item).is_ok()
    }

    /// Returns a reference to an element or subslice depending on the type of index.
    ///
    ///If given a position, returns a reference to the element at that position or None if out of bounds.
    ///If given a range, returns the subslice corresponding to that range, or None if out of bounds.
    pub fn get<I>(&self, index: I) -> Option<&<I as std::slice::SliceIndex<[T]>>::Output>
    where
        I: std::slice::SliceIndex<[T]>,
    {
        self.members.get(index)
    }

    /// Returns a reference to the first element in the set, if any. This element is always the minimum of all elements in the set.
    pub fn first(&self) -> Option<&T>
    where
        T: Ord,
    {
        self.members.first()
    }

    /// Returns a reference to the last element in the set, if any. This element is always the maximum of all elements in the set.
    pub fn last(&self) -> Option<&T>
    where
        T: Ord,
    {
        self.members.last()
    }

    /// Visits the values representing the difference, i.e., all the values in `self` but not in
    /// `other`,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let difference: Vec<&str> = a.difference(&b).cloned().collect();
    /// assert_eq!(difference, ["a", "f",]);
    /// ```
    pub fn difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.iter().difference(other.iter())
    }

    /// Visits the values representing the intersectio, i.e., all the values in both `self` and
    /// `other`,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let intersection: Vec<&str> = a.intersection(&b).cloned().collect();
    /// assert_eq!(intersection, ["d", "h",]);
    /// ```
    pub fn intersection(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.iter().intersection(other.iter())
    }

    /// Visits the values representing the symmetric difference, i.e., all the values in `self` or
    /// `other` but not in both,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let symmetric_difference: Vec<&str> = a.symmetric_difference(&b).cloned().collect();
    /// assert_eq!(symmetric_difference, ["a", "b", "c", "f", "i"]);
    /// ```
    pub fn symmetric_difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.iter().symmetric_difference(other.iter())
    }

    /// Visits the values representing the union, i.e., all the values in `self` or `other`,
    /// without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a: OrdListSet<&str> = ["a", "d", "f", "h"].into();
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].into();
    ///
    /// let union: Vec<&str> = a.union(&b).cloned().collect();
    /// assert_eq!(union, ["a", "b", "c", "d", "f", "h", "i",]);
    /// ```
    pub fn union(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.iter().union(other.iter())
    }

    /// Is the output of the given Iterator disjoint from the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_disjoint(&self, other: &'a Self) -> bool {
        self.iter().is_disjoint(other.iter())
    }

    /// Is the output of the given Iterator a proper subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_proper_subset(&self, other: &'a Self) -> bool {
        self.iter().is_proper_subset(other.iter())
    }

    /// Is the output of the given Iterator a proper superset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_proper_superset(&self, other: &'a Self) -> bool {
        self.iter().is_proper_superset(other.iter())
    }

    /// Is the output of the given Iterator a subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_subset(&self, other: &'a Self) -> bool {
        self.iter().is_subset(other.iter())
    }

    /// Is the output of the given Iterator a superset of the output of
    /// this iterator?
    pub fn is_superset(&self, other: &'a Self) -> bool {
        self.iter().is_superset(other.iter())
    }
}

impl<T: Ord, const N: usize> From<[T; N]> for OrdListSet<T> {
    fn from(members: [T; N]) -> Self {
        let mut members = Vec::from(members);
        members.sort_unstable();
        members.dedup();
        Self { members }
    }
}

impl<T: Ord> From<BTreeSet<T>> for OrdListSet<T> {
    fn from(mut set: BTreeSet<T>) -> Self {
        let mut members: Vec<T> = Vec::with_capacity(set.len());
        while let Some(member) = set.pop_first() {
            members.push(member);
        }
        Self { members }
    }
}

impl<'a, T: Ord + Clone> From<OrdSetOpsIter<'a, T>> for OrdListSet<T> {
    fn from(oso_iter: OrdSetOpsIter<T>) -> Self {
        let members: Vec<T> = oso_iter.cloned().collect();
        Self { members }
    }
}

fn is_sorted<T: Ord>(list: &[T]) -> bool {
    if !list.is_empty() {
        let mut last = &list[0];
        for element in list[1..].iter() {
            if element <= last {
                return false;
            } else {
                last = element;
            }
        }
    }
    true
}

impl<T: Ord> FromIterator<T> for OrdListSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let members: Vec<T> = iter.into_iter().collect();
        debug_assert!(is_sorted(&members));
        Self { members }
    }
}

impl<T: Ord + Clone> Sub<&OrdListSet<T>> for &OrdListSet<T> {
    type Output = OrdListSet<T>;

    /// Returns the difference of `self` and `rhs` as a new `OrdListSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from([1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from([2, 3, 4]);
    ///
    /// assert_eq!(&a - &b, OrdListSet::<u32>::from([1, 5]));
    /// ```
    fn sub(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.difference(rhs).into()
    }
}

impl<T: Ord + Clone> BitAnd<&OrdListSet<T>> for &OrdListSet<T> {
    type Output = OrdListSet<T>;

    /// Returns the intersection of `self` and `rhs` as a new `OrdListSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from([1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from([2, 3, 4]);
    ///
    /// assert_eq!(&a & &b, OrdListSet::<u32>::from([2, 3,]));
    /// ```
    fn bitand(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.intersection(rhs).into()
    }
}

impl<T: Ord + Clone> BitXor<&OrdListSet<T>> for &OrdListSet<T> {
    type Output = OrdListSet<T>;

    /// Returns the symmetric difference of `self` and `rhs` as a new `OrdListSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from([1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from([2, 3, 4]);
    ///
    /// assert_eq!(&a ^ &b, OrdListSet::<u32>::from([1, 4, 5]));
    /// ```
    fn bitxor(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.symmetric_difference(rhs).into()
    }
}

impl<T: Ord + Clone> BitOr<&OrdListSet<T>> for &OrdListSet<T> {
    type Output = OrdListSet<T>;

    /// Returns the union of `self` and `rhs` as a new `OrdListSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from([1, 2, 3]);
    /// let b = OrdListSet::<u32>::from([2, 3, 4]);
    ///
    /// assert_eq!(&a | &b, OrdListSet::<u32>::from([1, 2, 3, 4]));
    /// ```
    fn bitor(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.union(rhs).into()
    }
}

/// An Iterator over the elements in an ordered list in ascending order.  Implements the
/// `PeepAdvanceIter` trait enable it to be used in set expressions (or chained functions)
/// obviating the need for the creation of temporary sets to hold intermediate results.
///
/// # Examples
/// ```
/// use ord_list_set::OrdListSet;
/// use ord_set_iter_set_ops::PeepAdvanceIter;
///
/// let a = OrdListSet::<u32>::from([1, 2, 3, 7, 8, 9]);
/// let b = OrdListSet::<u32>::from([2, 3, 4]);
/// let c = OrdListSet::<u32>::from([3, 5, 6]);
/// let d = OrdListSet::<u32>::from([2, 7, 9]);
///
/// let slow_way = &(&(&a - &b) | &c) ^ &d;
/// let fast_way: OrdListSet<u32> = (((a.iter() - b.iter()) | c.iter()) ^ d.iter()).cloned().collect();
/// let chain_way: OrdListSet<u32> = a.difference(&b)
///                                     .union(c.iter())
///                                     .symmetric_difference(d.iter())
///                                     .cloned().collect();
/// assert_eq!(fast_way, slow_way);
/// assert_eq!(fast_way, chain_way);
/// ```
#[derive(Clone, Default)]
pub struct OrdListSetIter<'a, T: Ord> {
    elements: &'a [T],
    index: usize,
}

impl<'a, T: Ord> Iterator for OrdListSetIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        self.elements.get(self.index - 1)
    }
}

impl<'a, T: Ord> ExactSizeIterator for OrdListSetIter<'a, T> {
    fn len(&self) -> usize {
        self.elements.len() - self.index
    }
}

impl<'a, T: Ord> OrdListSetIter<'a, T> {
    pub fn is_empty(&self) -> bool {
        self.index > self.elements.len()
    }
}

impl<'a, T: 'a + Ord> PeepAdvanceIter<'a, T> for OrdListSetIter<'a, T> {
    /// Peep at the next item in the iterator without advancing the iterator.
    fn peep(&mut self) -> Option<&'a T> {
        self.elements.get(self.index)
    }

    /// Advance this iterator to the next item at or after the given item.
    /// Implementation is O(log(n)).
    fn advance_until(&mut self, t: &T) {
        // Make sure we don't go backwards
        if let Some(item) = self.peep() {
            if item < t {
                self.index += match self.elements[self.index..].binary_search(t) {
                    Ok(index) => index,
                    Err(index) => index,
                };
            }
        }
    }

    /// Advance this iterator to the next item at or after the given item.
    /// Default implementation is O(n) but custom built implementations could be as good as O(log(n)).
    fn advance_after(&mut self, t: &T) {
        // Make sure we don't go backwards
        if let Some(item) = self.peep() {
            if item <= t {
                self.index += match self.elements[self.index..].binary_search(t) {
                    Ok(index) => index + 1,
                    Err(index) => index,
                };
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: create tests for set relationships
    #[test]
    fn union() {
        let set1 = OrdListSet::<&str>::from(["a", "b", "c"]);
        let set2 = OrdListSet::<&str>::from(["d", "e", "b", "c"]);
        assert_eq!(
            OrdListSet::<&str>::from(["a", "b", "c", "d", "e"]),
            &set1 | &set2
        );
    }

    #[test]
    fn intersection() {
        let set1 = OrdListSet::<&str>::from(["a", "b", "c"]);
        let set2 = OrdListSet::<&str>::from(["d", "e", "b", "c"]);
        assert_eq!(OrdListSet::<&str>::from(["b", "c"]), &set1 & &set2);
    }

    #[test]
    fn difference() {
        let set1 = OrdListSet::<&str>::from(["a", "b", "c"]);
        let set2 = OrdListSet::<&str>::from(["d", "e", "b", "c"]);
        assert_eq!(OrdListSet::<&str>::from(["a"]), &set1 - &set2);
        assert_eq!(OrdListSet::<&str>::from(["d", "e"]), &set2 - &set1);
    }

    #[test]
    fn symmetric_difference() {
        let set1 = OrdListSet::<&str>::from(["a", "b", "c"]);
        let set2 = OrdListSet::<&str>::from(["d", "e", "b", "c"]);
        assert_eq!(OrdListSet::<&str>::from(["a", "d", "e"]), &set1 ^ &set2);
        assert_eq!(OrdListSet::<&str>::from(["a", "d", "e"]), &set2 ^ &set1);
    }
}
