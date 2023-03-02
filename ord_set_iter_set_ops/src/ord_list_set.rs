// Copyright 2020 Peter Williams <pwil3058@gmail.com> <pwil3058@bigpond.net.au>
//! Sets implemented as a sorted list.

use std::{
    collections::BTreeSet,
    fmt::Debug,
    iter::FromIterator,
    ops::{BitAnd, BitOr, BitXor, RangeBounds, Sub},
};

use super::{OrdSetOpsIter, PeepAdvanceIter, SetOsoIter};

use crate::error::Error;

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
    pub fn iter<'a>(&'a self) -> OrdListSetIter<'a, T> {
        OrdListSetIter {
            elements: &self.members,
            index: 0,
        }
    }
}

impl<'a, T: 'a + Ord + Clone> SetOsoIter<'a, T> for OrdListSet<T> {
    /// Return an iterator over the members in the `OrdListSet` in ascending order.
    fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T> {
        OrdSetOpsIter::Plain(Box::new(OrdListSetIter {
            elements: &self.members,
            index: 0,
        }))
    }
}

// set functions that don't modify the set
impl<'a, T: 'a + Ord + Clone> OrdListSet<T> {
    ///Returns true if the set contains an element equal to the value.
    pub fn contains(&self, item: &T) -> bool {
        self.members.binary_search(item).is_ok()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.members.get(index)
    }

    pub fn items(&self, range: impl RangeBounds<usize>) -> &[T] {
        use std::ops::Bound;
        let members = match range.start_bound() {
            Bound::Included(start) => match range.end_bound() {
                Bound::Included(end) => self.members.get(*start..=*end),
                Bound::Excluded(end) => self.members.get(*start..*end),
                Bound::Unbounded => self.members.get(*start..),
            },
            Bound::Excluded(start) => match range.end_bound() {
                Bound::Included(end) => self.members.get(*start + 1..=*end),
                Bound::Excluded(end) => self.members.get(*start + 1..*end),
                Bound::Unbounded => self.members.get(*start..),
            },
            Bound::Unbounded => match range.end_bound() {
                Bound::Included(end) => self.members.get(..=*end),
                Bound::Excluded(end) => self.members.get(..*end),
                Bound::Unbounded => self.members.get(..),
            },
        };
        if let Some(members) = members {
            members
        } else {
            &[]
        }
    }

    fn index_for(&self, item: &T) -> Result<usize, Error> {
        match self.members.binary_search(item) {
            Ok(index) => Ok(index),
            Err(_) => Err(Error::NotInSet),
        }
    }

    pub fn item_items(&self, range: impl RangeBounds<T>) -> Result<&[T], Error> {
        use std::ops::Bound;
        let members = match range.start_bound() {
            Bound::Included(start) => {
                let start = self.index_for(start)?;
                match range.end_bound() {
                    Bound::Included(end) => {
                        let end = self.index_for(end)?;
                        self.members.get(start..=end)
                    }
                    Bound::Excluded(end) => {
                        let end = self.index_for(end)?;
                        self.members.get(start..end)
                    }
                    Bound::Unbounded => self.members.get(start..),
                }
            }
            Bound::Excluded(start) => {
                let start = self.index_for(start)?;
                match range.end_bound() {
                    Bound::Included(end) => {
                        let end = self.index_for(end)?;
                        self.members.get(start + 1..=end)
                    }
                    Bound::Excluded(end) => {
                        let end = self.index_for(end)?;
                        self.members.get(start + 1..end)
                    }
                    Bound::Unbounded => self.members.get(start..),
                }
            }
            Bound::Unbounded => match range.end_bound() {
                Bound::Included(end) => {
                    let end = self.index_for(end)?;
                    self.members.get(..=end)
                }
                Bound::Excluded(end) => {
                    let end = self.index_for(end)?;
                    self.members.get(..end)
                }
                Bound::Unbounded => self.members.get(..),
            },
        };
        Ok(members.unwrap())
    }

    pub fn get_subset(&self, range: impl RangeBounds<usize>) -> OrdListSet<T> {
        Self::from(self.items(range))
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
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let difference: Vec<&str> = a.difference(&b).cloned().collect();
    /// assert_eq!(difference, ["a", "f",]);
    /// ```
    pub fn difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().difference(other.oso_iter())
    }

    /// Visits the values representing the intersectio, i.e., all the values in both `self` and
    /// `other`,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let intersection: Vec<&str> = a.intersection(&b).cloned().collect();
    /// assert_eq!(intersection, ["d", "h",]);
    /// ```
    pub fn intersection(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().intersection(other.oso_iter())
    }

    /// Visits the values representing the symmetric difference, i.e., all the values in `self` or
    /// `other` but not in both,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::from(["a", "d", "f", "h"]);
    /// let b = OrdListSet::<&str>::from(["b", "c", "d", "i", "h"]);
    ///
    /// let symmetric_difference: Vec<&str> = a.symmetric_difference(&b).cloned().collect();
    /// assert_eq!(symmetric_difference, ["a", "b", "c", "f", "i"]);
    /// ```
    pub fn symmetric_difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().symmetric_difference(other.oso_iter())
    }

    /// Visits the values representing the union, i.e., all the values in `self` or `other`,
    /// without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
    ///
    /// let a: OrdListSet<&str> = ["a", "d", "f", "h"].into();
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].into();
    ///
    /// let union: Vec<&str> = a.union(&b).cloned().collect();
    /// assert_eq!(union, ["a", "b", "c", "d", "f", "h", "i",]);
    /// ```
    pub fn union(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().union(other.oso_iter())
    }

    /// Is the output of the given Iterator disjoint from the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_disjoint(&self, other: &'a Self) -> bool {
        self.oso_iter().is_disjoint(other.oso_iter())
    }

    /// Is the output of the given Iterator a proper subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_proper_subset(&self, other: &'a Self) -> bool {
        self.oso_iter().is_proper_subset(other.oso_iter())
    }

    /// Is the output of the given Iterator a proper superset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_proper_superset(&self, other: &'a Self) -> bool {
        self.oso_iter().is_proper_superset(other.oso_iter())
    }

    /// Is the output of the given Iterator a subset of the output of
    /// this iterator?
    #[allow(clippy::wrong_self_convention)]
    pub fn is_subset(&self, other: &'a Self) -> bool {
        self.oso_iter().is_subset(other.oso_iter())
    }

    /// Is the output of the given Iterator a superset of the output of
    /// this iterator?
    pub fn is_superset(&self, other: &'a Self) -> bool {
        self.oso_iter().is_superset(other.oso_iter())
    }
}

fn is_sorted_and_no_dups<T: Ord>(list: &[T]) -> bool {
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

impl<T: Ord, const N: usize> From<[T; N]> for OrdListSet<T> {
    fn from(members: [T; N]) -> Self {
        let mut members = Vec::from(members);
        members.sort_unstable();
        debug_assert!(is_sorted_and_no_dups(&members));
        Self { members }
    }
}

impl<T: Ord + Clone> From<&[T]> for OrdListSet<T> {
    fn from(members: &[T]) -> Self {
        let mut members = Vec::from(members);
        members.sort_unstable();
        debug_assert!(is_sorted_and_no_dups(&members));
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

impl<T: Ord> FromIterator<T> for OrdListSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut members: Vec<T> = iter.into_iter().collect();
        members.sort_unstable();
        debug_assert!(is_sorted_and_no_dups(&members));
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
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
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
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
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
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
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
    /// use ord_set_iter_set_ops::ord_list_set::OrdListSet;
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
/// `PeepAdvanceIter`.
#[derive(Clone, Default)]
pub struct OrdListSetIter<'a, T: Ord> {
    elements: &'a [T],
    index: usize,
}

impl<'a, T: Ord> Iterator for OrdListSetIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        self.index += 1;
        self.elements.get(index)
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

    #[test]
    fn get_subset() {
        let set = OrdListSet::<&str>::from(["a", "b", "d", "e", "g", "h", "i"]);
        assert_eq!(set.get_subset(..), set);
        assert_eq!(
            set.get_subset(1..),
            OrdListSet::<&str>::from(["b", "d", "e", "g", "h", "i"])
        );
        assert_eq!(
            set.get_subset(2..5),
            OrdListSet::<&str>::from(["d", "e", "g"])
        );
        assert_eq!(
            set.get_subset(2..=5),
            OrdListSet::<&str>::from(["d", "e", "g", "h"])
        );
        assert_eq!(
            set.get_subset(..5),
            OrdListSet::<&str>::from(["a", "b", "d", "e", "g"])
        );
        assert_eq!(
            set.get_subset(..=5),
            OrdListSet::<&str>::from(["a", "b", "d", "e", "g", "h"])
        );
    }

    #[test]
    fn item_items() {
        let set = OrdListSet::<&str>::from(["a", "b", "d", "e", "g", "h", "i"]);
        assert_eq!(
            set.item_items(..).unwrap(),
            ["a", "b", "d", "e", "g", "h", "i"]
        );
        assert_eq!(
            set.item_items("b"..).unwrap(),
            ["b", "d", "e", "g", "h", "i"]
        );
        assert_eq!(set.item_items("d".."h").unwrap(), ["d", "e", "g"]);
        assert_eq!(set.item_items("d"..="h").unwrap(), ["d", "e", "g", "h"]);
        assert_eq!(set.item_items(.."h").unwrap(), ["a", "b", "d", "e", "g"]);
        assert_eq!(
            set.item_items(..="h").unwrap(),
            ["a", "b", "d", "e", "g", "h"]
        );
        assert!(set.item_items(..="j").is_err());
    }
}
