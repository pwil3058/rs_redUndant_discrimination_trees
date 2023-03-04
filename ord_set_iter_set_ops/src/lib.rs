use crate::ord_list_set::OrdListSetIter;
use dyn_clonable::*;

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

pub mod error;
pub mod ord_list_set;

/// Ordered Iterator over set operations on the contents of an ordered set.
#[clonable]
pub trait PeepAdvanceIter<'a, T: 'a + Ord>: Iterator<Item = &'a T> + Clone {
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
}

impl<'a, T, I> PeepAdvanceIter<'a, T> for Peekable<I>
where
    T: 'a + Ord,
    I: Iterator<Item = &'a T> + Clone,
{
    fn peep(&mut self) -> Option<&'a T> {
        self.peek().copied()
    }
}

pub enum OrdSetOpsIter<'a, T>
where
    T: Ord,
{
    Difference(Box<Self>, Box<Self>),
    Intersection(Box<Self>, Box<Self>),
    SymmetricDifference(Box<Self>, Box<Self>),
    Union(Box<Self>, Box<Self>),
    OrdListSet(OrdListSetIter<'a, T>),
    BTreeSet(Peekable<btree_set::Iter<'a, T>>),
    Plain(Box<dyn PeepAdvanceIter<'a, T, Item = &'a T> + 'a>),
    _Phantom(Infallible, PhantomData<&'a T>),
}

impl<'a, T: Ord> Clone for OrdSetOpsIter<'a, T> {
    #[allow(unreachable_code)]
    fn clone(&self) -> Self {
        use OrdSetOpsIter::*;
        match self {
            Difference(left_iter, right_iter) => Difference(left_iter.clone(), right_iter.clone()),
            Intersection(left_iter, right_iter) => {
                Intersection(left_iter.clone(), right_iter.clone())
            }
            SymmetricDifference(left_iter, right_iter) => {
                SymmetricDifference(left_iter.clone(), right_iter.clone())
            }
            Union(left_iter, right_iter) => Union(left_iter.clone(), right_iter.clone()),
            OrdListSet(iter) => OrdListSet(iter.clone()),
            BTreeSet(iter) => BTreeSet(iter.clone()),
            Plain(iter) => Plain(iter.clone()),
            _Phantom(a, _b) => _Phantom(*a, *_b),
        }
    }
}

impl<'a, T> PeepAdvanceIter<'a, T> for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord + Clone,
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
            OrdListSet(ref mut iter) => iter.peep(),
            BTreeSet(ref mut iter) => iter.peep(),
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
            Self::OrdListSet(ref mut iter) => {
                iter.advance_until(target);
            }
            Self::BTreeSet(ref mut iter) => {
                iter.advance_until(target);
            }
            Self::Plain(ref mut iter) => {
                iter.advance_until(target);
            }
            _ => (),
        }
    }
}

impl<'a, T> OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    pub fn difference(&self, other: &Self) -> OrdSetOpsIter<'a, T> {
        Self::Difference(Box::new(self.clone()), Box::new(other.clone()))
    }

    pub fn intersection(&self, other: &Self) -> OrdSetOpsIter<'a, T> {
        Self::Intersection(Box::new(self.clone()), Box::new(other.clone()))
    }

    pub fn symmetric_difference(&self, other: &Self) -> OrdSetOpsIter<'a, T> {
        Self::SymmetricDifference(Box::new(self.clone()), Box::new(other.clone()))
    }

    pub fn union(&self, other: &Self) -> OrdSetOpsIter<'a, T> {
        Self::Union(Box::new(self.clone()), Box::new(other.clone()))
    }
}

// TODO: move these back into PeekAdvanceIterator
impl<'a, T: 'a + Ord + Clone> OrdSetOpsIter<'a, T> {
    pub fn is_disjoint(&self, other: &Self) -> bool {
        let mut self_iter = self.clone();
        let mut other_iter = other.clone();
        loop {
            if let Some(my_item) = self_iter.peep() {
                if let Some(other_item) = other_iter.peep() {
                    match my_item.cmp(other_item) {
                        Ordering::Less => {
                            self_iter.advance_until(other_item);
                        }
                        Ordering::Greater => {
                            other_iter.advance_until(my_item);
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

    pub fn is_proper_subset(&self, other: &Self) -> bool {
        let mut self_iter = self.clone();
        let mut other_iter = other.clone();
        let mut result = false;
        while let Some(my_item) = self_iter.peep() {
            if let Some(other_item) = other_iter.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        return false;
                    }
                    Ordering::Greater => {
                        result = true;
                        other_iter.advance_until(my_item);
                    }
                    Ordering::Equal => {
                        other_iter.next();
                        self_iter.next();
                    }
                }
            } else {
                return false;
            }
        }
        result || other_iter.peep().is_some()
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        let mut self_iter = self.clone();
        let mut other_iter = other.clone();
        while let Some(my_item) = self_iter.peep() {
            if let Some(other_item) = other_iter.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        return false;
                    }
                    Ordering::Greater => {
                        other_iter.advance_until(my_item);
                    }
                    Ordering::Equal => {
                        other_iter.next();
                        self_iter.next();
                    }
                }
            } else {
                return false;
            }
        }
        true
    }

    pub fn is_proper_superset(&self, other: &Self) -> bool {
        let mut self_iter = self.clone();
        let mut other_iter = other.clone();
        let mut result = false;
        while let Some(my_item) = self_iter.peep() {
            if let Some(other_item) = other_iter.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        result = true;
                        self_iter.advance_until(other_item);
                    }
                    Ordering::Greater => {
                        return false;
                    }
                    Ordering::Equal => {
                        other_iter.next();
                        self_iter.next();
                    }
                }
            } else {
                return true;
            }
        }
        result && other_iter.peep().is_none()
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        let mut self_iter = self.clone();
        let mut other_iter = other.clone();
        while let Some(my_item) = self_iter.peep() {
            if let Some(other_item) = other_iter.peep() {
                match my_item.cmp(other_item) {
                    Ordering::Less => {
                        self_iter.advance_until(other_item);
                    }
                    Ordering::Greater => {
                        return false;
                    }
                    Ordering::Equal => {
                        other_iter.next();
                        self_iter.next();
                    }
                }
            } else {
                return true;
            }
        }
        other_iter.peep().is_none()
    }
}

impl<'a, T> Iterator for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord + Clone,
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
            Self::OrdListSet(ref mut iter) => iter.next(),
            Self::BTreeSet(ref mut iter) => iter.next(),
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

pub trait SetOsoIter<'a, T: 'a + Ord>
where
    T: 'a + Ord + Clone,
{
    fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T>;

    fn oso_difference(&'a self, other: &'a impl SetOsoIter<'a, T>) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().difference(&other.oso_iter())
    }

    fn oso_intersection(&'a self, other: &'a impl SetOsoIter<'a, T>) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().intersection(&other.oso_iter())
    }

    fn oso_symmetric_difference(
        &'a self,
        other: &'a impl SetOsoIter<'a, T>,
    ) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().symmetric_difference(&other.oso_iter())
    }

    fn oso_union(&'a self, other: &'a impl SetOsoIter<'a, T>) -> OrdSetOpsIter<'a, T> {
        self.oso_iter().union(&other.oso_iter())
    }

    fn is_oso_disjoint(&'a self, other: &'a impl SetOsoIter<'a, T>) -> bool {
        self.oso_iter().is_disjoint(&other.oso_iter())
    }

    fn is_oso_subset(&'a self, other: &'a impl SetOsoIter<'a, T>) -> bool {
        self.oso_iter().is_subset(&other.oso_iter())
    }

    fn is_oso_superset(&'a self, other: &'a impl SetOsoIter<'a, T>) -> bool {
        self.oso_iter().is_superset(&other.oso_iter())
    }

    fn is_oso_proper_subset(&'a self, other: &'a impl SetOsoIter<'a, T>) -> bool {
        self.oso_iter().is_proper_subset(&other.oso_iter())
    }

    fn is_oso_proper_superset(&'a self, other: &'a impl SetOsoIter<'a, T>) -> bool {
        self.oso_iter().is_proper_superset(&other.oso_iter())
    }
}

impl<'a, T: 'a + Ord + Clone> SetOsoIter<'a, T> for BTreeSet<T> {
    fn oso_iter(&'a self) -> OrdSetOpsIter<'a, T> {
        OrdSetOpsIter::BTreeSet(self.iter().peekable())
    }
}

pub trait MapOsoIter<'a, K, I>
where
    K: 'a + Ord + Clone,
    I: Iterator<Item = &'a K> + Clone,
{
    fn oso_keys(&'a self) -> OrdSetOpsIter<'a, K>;
}

impl<'a, K, V> MapOsoIter<'a, K, btree_map::Keys<'a, K, V>> for BTreeMap<K, V>
where
    K: 'a + Ord + Clone,
{
    fn oso_keys(&'a self) -> OrdSetOpsIter<'a, K> {
        OrdSetOpsIter::Plain(Box::new(self.keys().peekable()))
    }
}

#[cfg(test)]
mod tests;
