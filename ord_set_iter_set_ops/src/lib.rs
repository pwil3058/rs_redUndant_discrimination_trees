use std::{
    cmp::Ordering,
    convert::Infallible,
    marker::PhantomData,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

/// Ordered Iterator over set operations on the contents of an ordered set.
pub trait PeepAdvanceIter<'a, T: 'a + Ord>: Iterator<Item = &'a T> {
    /// Peep at the next item in the iterator without advancing the iterator.
    fn peep(&mut self) -> Option<&'a T>;

    /// Advance this iterator to the next item at or after the given item.
    /// Default implementation is O(n) but custom built implementations could be as good as O(log(n)).
    fn advance_until(&mut self, t: &T) {
        while let Some(item) = self.peep() {
            if t > item {
                self.next();
            } else {
                break;
            }
        }
    }
}
//
// pub struct Wrapper<'a, T>
// where
//     T: 'a + Ord,
//     I: PeepAdvanceIter<'a, T>,
// {
//     iter: I,
//     phantom: PhantomData<&'a T>,
// }
//
// impl<'a, T> From<I> for Wrapper<'a, T>
// where
//     T: 'a + Ord,
//     I: PeepAdvanceIter<'a, T>,
// {
//     fn from(iter: I) -> Self {
//         Wrapper {
//             iter,
//             phantom: PhantomData,
//         }
//     }
// }
//
// impl<'a, T> Iterator for Wrapper<'a, T>
// where
//     T: 'a + Ord,
//     I: PeepAdvanceIter<'a, T>,
// {
//     type Item = &'a T;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter.next()
//     }
// }
//
// impl<'a, T> PeepAdvanceIter<'a, T> for Wrapper<'a, T>
// where
//     T: 'a + Ord,
//     I: PeepAdvanceIter<'a, T>,
// {
//     fn peep(&mut self) -> Option<&'a T> {
//         self.iter.peep()
//     }
// }

pub trait SetOperations<'a, T: 'a + Ord>: PeepAdvanceIter<'a, T> + Sized {
    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn difference(self, iter: Self) -> Self;

    /// Iterate over the set intersection of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn intersection(self, iter: Self) -> Self;

    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn symmetric_difference(self, iter: Self) -> Self;

    /// Iterate over the set union of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn union(self, iter: Self) -> Self;

    /// Is the output of the given Iterator disjoint from the output of
    /// this iterator?
    fn is_disjoint<I: PeepAdvanceIter<'a, T> + Clone>(mut self, mut other: Self) -> bool {
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

    /// Is the output of the given Iterator a proper subset of the output of
    /// this iterator?
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

    /// Is the output of the given Iterator a proper superset of the output of
    /// this iterator?
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

    /// Is the output of the given Iterator a subset of the output of
    /// this iterator?
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

    /// Is the output of the given Iterator a superset of the output of
    /// this iterator?
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
    T: 'a + Ord,
{
    Difference(Box<Self>, Box<Self>),
    Intersection(Box<Self>, Box<Self>),
    SymmetricDifference(Box<Self>, Box<Self>),
    Union(Box<Self>, Box<Self>),
    Plain(Box<dyn PeepAdvanceIter<'a, T>>),
    _Phantom(Infallible, PhantomData<&'a T>),
}

impl<'a, T> OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    pub fn new(iter: Box<dyn PeepAdvanceIter<'a, T>>) -> Self {
        Self::Plain(iter.into())
    }
}

impl<'a, T> PeepAdvanceIter<'a, T> for OrdSetOpsIter<'a, T>
where
    T: 'a + Ord,
{
    fn peep(&mut self) -> Option<&'a T> {
        use OrdSetOpsIter::*;
        match self {
            Difference(ref mut l_iter, ref mut r_iter) => {
                while let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return Some(l_item);
                            }
                            Ordering::Greater => {
                                r_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                l_iter.next();
                                r_iter.next();
                            }
                        }
                    } else {
                        return Some(l_item);
                    }
                }
                None
            }
            Intersection(ref mut l_iter, ref mut r_iter) => {
                if let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                l_iter.advance_until(r_item);
                                l_iter.peep()
                            }
                            Ordering::Greater => {
                                r_iter.advance_until(l_item);
                                r_iter.peep()
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
            SymmetricDifference(ref mut l_iter, ref mut r_iter) => {
                while let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return Some(l_item);
                            }
                            Ordering::Greater => {
                                return Some(r_item);
                            }
                            Ordering::Equal => {
                                l_iter.next();
                                r_iter.next();
                            }
                        }
                    } else {
                        return Some(l_item);
                    }
                }
                r_iter.peep()
            }
            Union(ref mut l_iter, ref mut r_iter) => {
                if let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less | Ordering::Equal => Some(l_item),
                            Ordering::Greater => Some(r_item),
                        }
                    } else {
                        Some(l_item)
                    }
                } else {
                    r_iter.peep()
                }
            }
            Plain(ref mut iter) => iter.peep(),
            _ => None,
        }
    }

    fn advance_until(&mut self, target: &T) {
        match self {
            Self::Difference(ref mut l_iter, ref mut r_iter) => {
                l_iter.advance_until(target);
                r_iter.advance_until(target);
            }
            Self::Intersection(ref mut l_iter, ref mut r_iter) => {
                l_iter.advance_until(target);
                r_iter.advance_until(target);
            }
            Self::SymmetricDifference(ref mut l_iter, ref mut r_iter) => {
                l_iter.advance_until(target);
                r_iter.advance_until(target);
            }
            Self::Union(ref mut l_iter, ref mut r_iter) => {
                l_iter.advance_until(target);
                r_iter.advance_until(target);
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
    fn difference(self, other: Self) -> Self {
        self.sub(other)
    }

    fn intersection(self, other: Self) -> Self {
        self.bitand(other)
    }

    fn symmetric_difference(self, other: Self) -> Self {
        self.bitxor(other)
    }

    fn union(self, other: Self) -> Self {
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
            Self::Difference(ref mut l_iter, ref mut r_iter) => {
                while let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return l_iter.next();
                            }
                            Ordering::Greater => {
                                r_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                l_iter.next();
                                r_iter.next();
                            }
                        }
                    } else {
                        return l_iter.next();
                    }
                }
                None
            }
            Self::Intersection(ref mut l_iter, ref mut r_iter) => {
                while let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                l_iter.advance_until(r_item);
                            }
                            Ordering::Greater => {
                                r_iter.advance_until(l_item);
                            }
                            Ordering::Equal => {
                                r_iter.next();
                                return l_iter.next();
                            }
                        }
                    } else {
                        return None;
                    }
                }
                None
            }
            Self::SymmetricDifference(ref mut l_iter, ref mut r_iter) => {
                while let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => {
                                return l_iter.next();
                            }
                            Ordering::Greater => {
                                return r_iter.next();
                            }
                            Ordering::Equal => {
                                l_iter.next();
                                r_iter.next();
                            }
                        }
                    } else {
                        return l_iter.next();
                    }
                }
                r_iter.next()
            }
            Self::Union(ref mut l_iter, ref mut r_iter) => {
                if let Some(l_item) = l_iter.peep() {
                    if let Some(r_item) = r_iter.peep() {
                        match l_item.cmp(r_item) {
                            Ordering::Less => l_iter.next(),
                            Ordering::Greater => r_iter.next(),
                            Ordering::Equal => {
                                r_iter.next();
                                l_iter.next()
                            }
                        }
                    } else {
                        l_iter.next()
                    }
                } else {
                    r_iter.next()
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

#[cfg(test)]
mod tests {
    use super::*;
    //use crate::OrdSetOpsIterator;

    struct Set<T: Ord>(Vec<T>);

    impl<T: Ord + Clone> From<Vec<T>> for Set<T> {
        fn from(mut elements: Vec<T>) -> Self {
            elements.sort();
            elements.dedup();
            Self(elements)
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

    impl<'a, T: Ord + Clone> Set<T> {
        pub fn iter(&self) -> OrdSetOpsIter<T> {
            let set_iter = SetIter::<T> {
                elements: &self.0,
                index: 0,
            };
            // let wrapper = Wrapper {
            //     iter: set_iter,
            //     phantom: PhantomData,
            // };
            OrdSetOpsIter::Plain(Box::new(Box::new(set_iter)))
        }

        // impl<'a, T: 'a + Ord + Clone> SetOperations<'a, T> for SetIter<'a, T> {
        //     fn difference(self, iter: Self) -> Self {}
        // }

        // pub fn is_superset(&self, other: &Self) -> bool {
        //     self.iter().is_superset(other.iter())
        // }
        //
        // pub fn is_subset(&self, other: &Self) -> bool {
        //     self.iter().is_subset(other.iter())
        // }
    }

    #[test]
    fn new_plain() {
        let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
        let mut oso_iter = OrdSetOpsIter::new(Box::new(set1.iter()));
        assert_eq!(oso_iter.next(), Some(&"a"));
        assert_eq!(oso_iter.next(), Some(&"b"));
        assert_eq!(oso_iter.next(), Some(&"c"));
        assert_eq!(oso_iter.next(), Some(&"d"));
        assert_eq!(oso_iter.next(), None);
    }

    // #[test]
    // fn can_make_oso_iter() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let iter = Box::new(set1.iter());
    //     let oso_iter1 = OrdSetOpsIter::Plain(iter);
    //     let x = oso_iter1.next();
    // }

    // #[test]
    // fn set_relations() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let set2 = Set::<&str>::from(vec!["b", "c", "d"]);
    //     assert!(set1.is_superset(&set2));
    //     assert!(!set1.is_subset(&set2));
    // }
    //
    // #[test]
    // fn set_difference() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let set2 = Set::<&str>::from(vec!["b", "c", "d", "e"]);
    //     assert_eq!(
    //         vec!["a"],
    //         (set1.iter().difference(set2.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    //     assert_eq!(
    //         vec!["e"],
    //         (set2.iter().difference(set1.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    // }
    //
    // #[test]
    // fn set_intersection() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let set2 = Set::<&str>::from(vec!["b", "c", "d", "e"]);
    //     assert_eq!(
    //         vec!["b", "c", "d"],
    //         (set1.iter().intersection(set2.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    //     assert_eq!(
    //         vec!["b", "c", "d"],
    //         (set2.iter().intersection(set1.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    //     let set1 = Set::<u32>::from(vec![1, 2, 3, 5]);
    //     let set2 = Set::<u32>::from(vec![2, 3, 4]);
    //     assert_eq!(
    //         vec![2, 3],
    //         (set2.iter().intersection(set1.iter()))
    //             .cloned()
    //             .collect::<Vec<u32>>()
    //     );
    // }
    //
    // #[test]
    // fn set_symmetric_difference() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let set2 = Set::<&str>::from(vec!["b", "c", "d", "e"]);
    //     assert_eq!(
    //         vec!["a", "e"],
    //         (set1.iter().symmetric_difference(set2.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    //     assert_eq!(
    //         vec!["a", "e"],
    //         (set2.iter().symmetric_difference(set1.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    // }
    //
    // #[test]
    // fn set_union() {
    //     let set1 = Set::<&str>::from(vec!["a", "b", "c", "d"]);
    //     let set2 = Set::<&str>::from(vec!["b", "c", "d", "e"]);
    //     assert_eq!(
    //         vec!["a", "b", "c", "d", "e"],
    //         (set1.iter().union(set2.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    //     assert_eq!(
    //         vec!["a", "b", "c", "d", "e"],
    //         (set2.iter().union(set1.iter()))
    //             .cloned()
    //             .collect::<Vec<&str>>()
    //     );
    // }
}
