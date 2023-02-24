// Copyright 2020 Peter Williams <pwil3058@gmail.com> <pwil3058@bigpond.net.au>
//! Sets implemented as a sorted list.

use std::{
    iter::FromIterator,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

use ord_set_iter_set_ops::*;

/// A set of items of type T ordered according to Ord (with no duplicates)
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct OrdListSet<T: Ord> {
    members: Vec<T>,
}

impl<T: Ord + Clone> OrdListSet<T> {
    pub fn new(list: &[T]) -> Self {
        let mut members: Vec<T> = list.to_vec();
        members.sort_unstable();
        members.dedup();
        Self { members }
    }
}

impl<T: Ord + Clone> OrdListSet<T> {
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

impl<'a, T: 'a + Ord + Clone> SetOperations<'a, T> for OrdListSet<T> {
    /// Visits the values representing the difference, i.e., all the values in `self` but not in
    /// `other`,without duplicates, in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<&str>::new(&["a", "d", "f", "h"]);
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].iter().cloned().collect();
    ///
    /// let difference: Vec<&str> = a.difference(&b).cloned().collect();
    /// assert_eq!(difference, ["a", "f",]);
    /// ```
    fn difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
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
    /// let a = OrdListSet::<&str>::new(&["a", "d", "f", "h"]);
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].iter().cloned().collect();
    ///
    /// let intersection: Vec<&str> = a.intersection(&b).cloned().collect();
    /// assert_eq!(intersection, ["d", "h",]);
    /// ```
    fn intersection(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
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
    /// let a = OrdListSet::<&str>::new(&["a", "d", "f", "h"]);
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].iter().cloned().collect();
    ///
    /// let symmetric_difference: Vec<&str> = a.symmetric_difference(&b).cloned().collect();
    /// assert_eq!(symmetric_difference, ["a", "b", "c", "f", "i"]);
    /// ```
    fn symmetric_difference(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
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
    /// let a: OrdListSet<&str> = ["a", "d", "f", "h"].iter().cloned().collect();
    /// let b: OrdListSet<&str> = ["b", "c", "d", "i", "h"].iter().cloned().collect();
    ///
    /// let union: Vec<&str> = a.union(&b).cloned().collect();
    /// assert_eq!(union, ["a", "b", "c", "d", "f", "h", "i",]);
    /// ```
    fn union(&'a self, other: &'a Self) -> OrdSetOpsIter<'a, T> {
        self.iter().union(other.iter())
    }
}

impl<T: Ord> From<Vec<T>> for OrdListSet<T> {
    fn from(mut members: Vec<T>) -> Self {
        members.sort_unstable();
        members.dedup();
        Self { members }
    }
}

impl<T: Ord> FromIterator<T> for OrdListSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut members: Vec<T> = iter.into_iter().collect();
        members.sort_unstable();
        members.dedup();
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
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// assert_eq!(&a - &b, OrdListSet::<u32>::from(vec![1, 5]));
    /// ```
    fn sub(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.difference(rhs).cloned().collect()
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
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// assert_eq!(&a & &b, OrdListSet::<u32>::from(vec![2, 3,]));
    /// ```
    fn bitand(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.intersection(rhs).cloned().collect()
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
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// assert_eq!(&a ^ &b, OrdListSet::<u32>::from(vec![1, 4, 5]));
    /// ```
    fn bitxor(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.symmetric_difference(rhs).cloned().collect()
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
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// assert_eq!(&a | &b, OrdListSet::<u32>::from(vec![1, 2, 3, 4]));
    /// ```
    fn bitor(self, rhs: &OrdListSet<T>) -> OrdListSet<T> {
        self.union(rhs).cloned().collect()
    }
}

/// An Iterator over the elements in an ordered list in ascending order.  Implements the
/// `PeepAdvanceIter` trait enable it to be used in set expressions (or chained functions)
/// obviating the need for the creation of temporary sets to hold intermediate results.
///
/// # Examples
/// ```
/// use ord_list_set::OrdListSet;
/// use ord_set_ops_iter::PeepAdvanceIter;
///
/// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 7, 8, 9]);
/// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
/// let c = OrdListSet::<u32>::from(vec![3, 5, 6]);
/// let d = OrdListSet::<u32>::from(vec![2, 7, 9]);
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
#[derive(Clone)]
pub struct OrdListSetIter<'a, T: Ord + Clone> {
    elements: &'a [T],
    index: usize,
}

impl<'a, T: Ord + Clone> Iterator for OrdListSetIter<'a, T> {
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

impl<'a, T: 'a + Ord + Clone> PeepAdvanceIter<'a, T> for OrdListSetIter<'a, T> {
    /// Peep at the next item in the iterator without advancing the iterator.
    fn peep(&mut self) -> Option<&'a T> {
        self.elements.get(self.index)
    }

    /// Advance this iterator to the next item at or after the given item.
    /// Implementation is O(log(n)).
    fn advance_until(&mut self, t: &T) {
        self.index += match self.elements[self.index..].binary_search(t) {
            Ok(index) => index,
            Err(index) => index,
        };
    }
}

impl<'a, T: 'a + Ord + Clone> IterSetOperations<'a, T> for OrdListSetIter<'a, T> {
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

impl<'a, T, O> Sub<O> for OrdListSetIter<'a, T>
where
    T: Ord + 'a + Clone,
    O: PeepAdvanceIter<'a, T> + 'a,
{
    type Output = OrdSetOpsIter<'a, T>;

    /// Returns the difference of `self` and `other` as a new ` OrdSetOpsIter<'a, T, Self, O>`
    /// iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// let result: OrdListSet<u32>  = (a.iter() - b.iter()).cloned().collect();
    /// assert_eq!(result, OrdListSet::<u32>::from(vec![1, 5]));
    /// ```
    #[inline]
    fn sub(self, other: O) -> Self::Output {
        OrdSetOpsIter::Difference(
            Box::new(OrdSetOpsIter::Plain(Box::new(self))),
            Box::new(other),
        )
    }
}

impl<'a, T, O> BitAnd<O> for OrdListSetIter<'a, T>
where
    T: Ord + 'a + Clone,
    O: PeepAdvanceIter<'a, T> + 'a,
{
    type Output = OrdSetOpsIter<'a, T>;

    /// Returns the intersection of `self` and `other` as a new ` OrdSetOpsIter<'a, T, Self, O>`
    /// iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// let result: OrdListSet<u32>  = (a.iter() & b.iter()).cloned().collect();
    /// assert_eq!(result, OrdListSet::<u32>::from(vec![2, 3]));
    /// ```
    #[inline]
    fn bitand(self, other: O) -> Self::Output {
        OrdSetOpsIter::Intersection(
            Box::new(OrdSetOpsIter::Plain(Box::new(self))),
            Box::new(other),
        )
    }
}

impl<'a, T, O> BitXor<O> for OrdListSetIter<'a, T>
where
    T: Ord + 'a + Clone,
    O: PeepAdvanceIter<'a, T> + 'a,
{
    type Output = OrdSetOpsIter<'a, T>;

    /// Returns the symmetric difference of `self` and `other` as a new ` OrdSetOpsIter<'a, T, Self, O>`
    /// iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// let result: OrdListSet<u32>  = (a.iter() ^ b.iter()).cloned().collect();
    /// assert_eq!(result, OrdListSet::<u32>::from(vec![1, 4, 5]));
    /// ```
    #[inline]
    fn bitxor(self, other: O) -> Self::Output {
        OrdSetOpsIter::SymmetricDifference(
            Box::new(OrdSetOpsIter::Plain(Box::new(self))),
            Box::new(other),
        )
    }
}

impl<'a, T, O> BitOr<O> for OrdListSetIter<'a, T>
where
    T: Ord + 'a + Clone,
    O: PeepAdvanceIter<'a, T> + 'a,
{
    type Output = OrdSetOpsIter<'a, T>;

    /// Returns the union of `self` and `other` as a new ` OrdSetOpsIter<'a, T, Self, O>`
    /// iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use ord_list_set::OrdListSet;
    ///
    /// let a = OrdListSet::<u32>::from(vec![1, 2, 3, 5]);
    /// let b = OrdListSet::<u32>::from(vec![2, 3, 4]);
    ///
    /// let result: OrdListSet<u32>  = (a.iter() | b.iter()).cloned().collect();
    /// assert_eq!(result, OrdListSet::<u32>::from(vec![1, 2, 3, 4, 5]));
    /// ```
    #[inline]
    fn bitor(self, other: O) -> Self::Output {
        OrdSetOpsIter::Union(
            Box::new(OrdSetOpsIter::Plain(Box::new(self))),
            Box::new(other),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn union() {
        let set1: OrdListSet<&str> = ["a", "b", "c"].iter().cloned().collect();
        let set2: OrdListSet<&str> = ["d", "e", "b", "c"].iter().cloned().collect();
        assert_eq!(
            vec!["a", "b", "c", "d", "e"],
            set1.union(&set2).cloned().collect::<Vec<&str>>()
        );
    }
}
