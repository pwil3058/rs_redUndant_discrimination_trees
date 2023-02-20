use std::{cmp::Ordering, convert::Infallible, marker::PhantomData, ops::BitAnd};

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

pub trait SetOperations<'a, T: 'a + Ord>: PeepAdvanceIter<'a, T> + Sized {
    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn difference<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self;

    /// Iterate over the set intersection of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn intersection<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self;

    /// Iterate over the set difference of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn symmetric_difference<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self;

    /// Iterate over the set union of this Iterator and the given Iterator
    /// in the order defined by their elements `Ord` trait implementation.
    fn union<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self;

    /// Is the output of the given Iterator disjoint from the output of
    /// this iterator?
    fn is_disjoint<I: PeepAdvanceIter<'a, T> + Clone>(mut self, mut other: I) -> bool {
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
    fn is_proper_subset<I: PeepAdvanceIter<'a, T>>(mut self, mut other: I) -> bool {
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
    fn is_proper_superset<I: PeepAdvanceIter<'a, T>>(mut self, mut other: I) -> bool {
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
    fn is_subset<I: PeepAdvanceIter<'a, T>>(mut self, mut other: I) -> bool {
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
    fn is_superset<I: PeepAdvanceIter<'a, T>>(mut self, mut other: I) -> bool {
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

pub enum OrdSetOpsIter<'a, T: Ord> {
    Difference(Box<OrdSetOpsIter<'a, T>>, Box<OrdSetOpsIter<'a, T>>),
    Intersection(Box<OrdSetOpsIter<'a, T>>, Box<OrdSetOpsIter<'a, T>>),
    SymmetricDifference(Box<OrdSetOpsIter<'a, T>>, Box<OrdSetOpsIter<'a, T>>),
    Union(Box<OrdSetOpsIter<'a, T>>, Box<OrdSetOpsIter<'a, T>>),
    Plain(Box<dyn PeepAdvanceIter<'a, T>>),
    _Phantom(Infallible, PhantomData<&'a T>),
}

impl<'a, T: Ord> PeepAdvanceIter<'a, T> for OrdSetOpsIter<'a, T> {
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

    // fn difference<I: PeepAdvanceIter<'a, T>>(self, iter: Box<I>) -> Box<Self> {
    //     Box::new(Self::Difference(Box::new(self), iter))
    // }
    //
    // fn intersection<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self {
    //     Self::Intersection(self, iter)
    // }
    //
    // fn symmetric_difference<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self {
    //     Self::SymmetricDifference(self, iter)
    // }
    //
    // fn union<I: PeepAdvanceIter<'a, T>>(self, iter: I) -> Self {
    //     Self::Union(self, iter)
    // }
}

impl<'a, T: Ord> Iterator for OrdSetOpsIter<'a, T> {
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
            Self::Intersection(ref mut l_iter, ref mut r_iter) => None,
            Self::SymmetricDifference(ref mut l_iter, ref mut r_iter) => None,
            Self::Union(ref mut l_iter, ref mut r_iter) => None,
            Self::Plain(ref mut iter) => iter.next(),
            _ => None,
        }
    }
}

impl<'a, T> BitAnd for OrdSetOpsIter<'a, T>
where
    T: Ord + 'a + Clone,
{
    type Output = OrdSetOpsIter<'a, T>;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        Self::Intersection(Box::new(self), Box::new(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(1, 1);
    }
}
