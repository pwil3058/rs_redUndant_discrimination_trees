use ord_set_iter_set_ops::{self, MapOsoIterAdaption, SetOsoIterAdaption};
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

// TODO: undo using this trait after development is finished
pub trait ItemTraits: Ord + Clone + Debug {}

impl ItemTraits for &str {}

type ChildMap<E> = BTreeMap<Rc<E>, Rc<TreeNode<E>>>;
type ChildAndKeys<E> = (Rc<TreeNode<E>>, BTreeSet<Rc<E>>);

#[allow(clippy::derive_ord_xor_partial_ord)]
#[derive(Ord, Eq)]
struct ChildAndKeysStruct<E: ItemTraits>(ChildAndKeys<E>);

impl<E: ItemTraits> PartialEq for ChildAndKeysStruct<E> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<E: ItemTraits> PartialOrd for ChildAndKeysStruct<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<E: ItemTraits> Debug for ChildAndKeysStruct<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let elements = &self.0 .0.elements;
        let keys = &self.0 .1;
        writeln!(f, "{keys:?} -> TreeNode: {elements:?}")
    }
}

trait ChildMapOps<'a, E: ItemTraits + 'a> {
    fn get_keys(&self) -> BTreeSet<Rc<E>>;
    fn get_child(&self, key: &Rc<E>) -> Option<Rc<TreeNode<E>>>;
    fn get_child_keys(&self, child: &Rc<TreeNode<E>>) -> BTreeSet<Rc<E>>;
    fn get_child_and_keys(&self, key: &Rc<E>) -> Option<ChildAndKeys<E>>;
    fn get_children_and_keys(&self) -> BTreeSet<ChildAndKeysStruct<E>>;
    fn insert_child<I: Iterator<Item = &'a Rc<E>>>(&mut self, iter: I, child: &Rc<TreeNode<E>>);
}

impl<'a, E: ItemTraits + 'a> ChildMapOps<'a, E> for ChildMap<E> {
    fn get_keys(&self) -> BTreeSet<Rc<E>> {
        BTreeSet::from_iter(self.keys().map(Rc::clone))
    }

    fn get_child(&self, key: &Rc<E>) -> Option<Rc<TreeNode<E>>> {
        let child = self.get(key)?;
        Some(Rc::clone(child))
    }

    fn get_child_keys(&self, child: &Rc<TreeNode<E>>) -> BTreeSet<Rc<E>> {
        BTreeSet::from_iter(
            child
                .elements
                .iter()
                .filter(|e| {
                    if let Some(c) = self.get(*e) {
                        c == child
                    } else {
                        false
                    }
                })
                .map(Rc::clone),
        )
    }

    fn get_child_and_keys(&self, key: &Rc<E>) -> Option<ChildAndKeys<E>> {
        let child = self.get(key)?;
        let child_keys = self.get_child_keys(child);
        Some((Rc::clone(child), child_keys))
    }

    fn get_children_and_keys(&self) -> BTreeSet<ChildAndKeysStruct<E>> {
        let mut set = BTreeSet::new();

        let mut keys = self.get_keys();
        while let Some(key) = keys.first() {
            let child_and_keys = self.get_child_and_keys(key).unwrap();
            keys = &keys - &child_and_keys.1;
            set.insert(ChildAndKeysStruct(child_and_keys));
        }

        set
    }

    fn insert_child<I: Iterator<Item = &'a Rc<E>>>(&mut self, iter: I, child: &Rc<TreeNode<E>>) {
        for key in iter {
            self.insert(Rc::clone(key), Rc::clone(child));
        }
    }
}

struct TreeNode<E: ItemTraits> {
    elements: BTreeSet<Rc<E>>,
    r_children: RefCell<ChildMap<E>>,
    v_children: RefCell<ChildMap<E>>,
    insert_count: Cell<usize>,
    subset_count: Cell<usize>,
}

impl<E: ItemTraits> Debug for TreeNode<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let elements = &self.elements;
        let r_children = self.r_children.borrow().get_children_and_keys();
        let v_children = self.v_children.borrow().get_children_and_keys();
        writeln!(
            f,
            "TreeNode: {elements:?}[{}, {}] Real Children: {r_children:?} VirtualChildren: {v_children:?}",
            self.insert_count.get(),
            self.subset_count.get(),
        )
    }
}

impl<E: ItemTraits> Default for TreeNode<E> {
    fn default() -> Self {
        Self {
            elements: BTreeSet::new(),
            r_children: RefCell::new(BTreeMap::new()),
            v_children: RefCell::new(BTreeMap::new()),
            insert_count: Cell::new(0),
            subset_count: Cell::new(0),
        }
    }
}

impl<E: ItemTraits> PartialEq for TreeNode<E> {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl<E: ItemTraits> Eq for TreeNode<E> {}

impl<E: ItemTraits> PartialOrd for TreeNode<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.elements.partial_cmp(&other.elements)
    }
}

impl<E: ItemTraits> Ord for TreeNode<E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a, E: 'a + ItemTraits> TreeNode<E> {
    fn tabula_rasa() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn new_insert(elements: BTreeSet<Rc<E>>) -> Rc<Self> {
        Rc::new(Self {
            insert_count: Cell::new(1),
            elements,
            ..Self::default()
        })
    }

    pub fn new_subset(elements: BTreeSet<Rc<E>>, v_children: RefCell<ChildMap<E>>) -> Rc<Self> {
        Rc::new(Self {
            elements,
            v_children,
            subset_count: Cell::new(1),
            ..Self::default()
        })
    }

    fn delete_v_children<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I) {
        let mut v_children = self.v_children.borrow_mut();
        for key in iter {
            v_children.remove(key);
        }
    }

    fn incr_insert_count(&self) {
        self.insert_count.set(self.insert_count.get() + 1)
    }

    fn incr_subset_count(&self) {
        self.subset_count.set(self.subset_count.get() + 1)
    }

    fn is_real_path_compatible_with(&self, set: &BTreeSet<Rc<E>>) -> bool {
        if !self.elements.is_subset(set) {
            println!("{self:?} elements are not a subset of {set:?}");
            false
        } else {
            let mut mut_set = set.clone();
            while let Some(key) = mut_set.pop_first() {
                if let Some((_, r_child_keys)) = self.get_r_child_and_keys(&key) {
                    if !r_child_keys.is_subset(set) {
                        println!("{self:?}: Real child keys {r_child_keys:?} are not a subset of {set:?}");
                        return false;
                    }
                    mut_set = &mut_set - &r_child_keys;
                }
            }
            true
        }
    }

    fn is_recursive_real_path_compatible_with(&self, excerpt: &BTreeSet<Rc<E>>) -> bool {
        if self.is_real_path_compatible_with(excerpt) {
            let mut keys = excerpt - &self.elements;
            while let Some(key) = keys.pop_first() {
                if let Some((r_child, r_child_keys)) = self.get_r_child_and_keys(&key) {
                    if !r_child.is_recursive_real_path_compatible_with(excerpt) {
                        return false;
                    }
                    keys = &keys - &r_child_keys;
                }
            }
            true
        } else {
            println!("{self:?}) is real path incompatible with {excerpt:?}");
            false
        }
    }

    fn is_compatible_with(&self, set: &BTreeSet<Rc<E>>) -> bool {
        if !self.elements.is_subset(set) {
            println!("{self:?} elements are not a subset of {set:?}");
            false
        } else {
            let mut mut_set = set.clone();
            while let Some(key) = mut_set.pop_first() {
                if let Some((_, r_child_keys)) = self.get_r_child_and_keys(&key) {
                    if !r_child_keys.is_subset(set) {
                        println!("{self:?}: Real child keys {r_child_keys:?} are not a subset of {set:?}");
                        return false;
                    }
                    mut_set = &mut_set - &r_child_keys;
                } else if let Some((_, v_child_keys)) = self.get_v_child_and_keys(&key) {
                    if !v_child_keys.is_subset(set) {
                        println!("{self:?}: Virtual child keys {v_child_keys:?} are not a subset of {set:?}");
                        return false;
                    }
                    mut_set = &mut_set - &v_child_keys;
                }
            }
            true
        }
    }

    fn is_recursive_compatible_with(&self, excerpt: &BTreeSet<Rc<E>>) -> bool {
        if &self.elements == excerpt {
            true
        } else if self.is_compatible_with(excerpt) {
            let mut keys = excerpt - &self.elements;
            while let Some(key) = keys.pop_first() {
                if let Some((r_child, r_child_keys)) = self.get_r_child_and_keys(&key) {
                    if !r_child.is_recursive_compatible_with(excerpt) {
                        println!("REAL CHILD");
                        return false;
                    }
                    keys = &keys - &r_child_keys;
                } else if let Some((v_child, v_child_keys)) = self.get_v_child_and_keys(&key) {
                    if !v_child.is_recursive_compatible_with(excerpt) {
                        println!("VIRTUAL CHILD");
                        return false;
                    }
                    keys = &keys - &v_child_keys;
                }
            }
            true
        } else {
            println!("{self:?}) is incompatible with {excerpt:?}");
            false
        }
    }
}

// Inline some child functions
impl<'a, E: 'a + ItemTraits> TreeNode<E> {
    #[inline]
    fn get_r_keys(&self) -> BTreeSet<Rc<E>> {
        self.r_children.borrow().get_keys()
    }
    #[inline]
    fn get_v_keys(&self) -> BTreeSet<Rc<E>> {
        self.v_children.borrow().get_keys()
    }

    // TODO: make this redundant by using oso technology
    #[inline]
    fn get_merged_keys(&self) -> BTreeSet<Rc<E>> {
        &self.get_r_keys() | &self.get_v_keys()
    }

    #[inline]
    fn get_r_child(&self, key: &Rc<E>) -> Option<Rc<TreeNode<E>>> {
        self.r_children.borrow().get_child(key)
    }

    #[inline]
    fn get_v_child(&self, key: &Rc<E>) -> Option<Rc<TreeNode<E>>> {
        self.v_children.borrow().get_child(key)
    }

    #[inline]
    fn get_r_child_and_keys(&self, key: &Rc<E>) -> Option<ChildAndKeys<E>> {
        self.r_children.borrow().get_child_and_keys(key)
    }

    #[inline]
    fn get_v_child_and_keys(&self, key: &Rc<E>) -> Option<ChildAndKeys<E>> {
        self.v_children.borrow().get_child_and_keys(key)
    }

    #[inline]
    fn insert_r_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, r_child: &Rc<Self>) {
        self.r_children.borrow_mut().insert_child(iter, r_child);
    }

    #[inline]
    fn insert_v_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, v_child: &Rc<Self>) {
        self.v_children.borrow_mut().insert_child(iter, v_child);
    }
}

impl<E: ItemTraits> TreeNode<E> {
    /// Find the Rc containing the target E if it exists
    fn find_key(&self, target: &E) -> Option<Rc<E>> {
        if let Some((key, _)) = self.r_children.borrow().get_key_value(target) {
            Some(Rc::clone(key))
        } else if let Some((key, _)) = self.v_children.borrow().get_key_value(target) {
            Some(Rc::clone(key))
        } else {
            None
        }
    }

    // Algorithm: 6.2
    fn interpose_for_real_compatibility(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
        let elements = &r_child.elements & excerpt;
        let v_children = r_child.merged_children();
        let new_node = Self::new_subset(elements, v_children);
        new_node.insert_r_child(r_child.elements.difference(&new_node.elements), &r_child);
        self.insert_r_child(r_child_keys.iter(), &new_node);
        debug_assert!(new_node.is_real_path_compatible_with(excerpt));
        debug_assert!(self.is_real_path_compatible_with(excerpt));
    }

    // Algorithm 6.3
    fn split(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
        let elements = &r_child.elements & excerpt;
        let v_children = r_child.merged_children();
        let new_node = Self::new_subset(elements, v_children);
        new_node.insert_v_child(r_child.elements.difference(&new_node.elements), &r_child);
        self.insert_r_child(excerpt.intersection(&r_child_keys), &new_node);
        debug_assert!(new_node.is_real_path_compatible_with(excerpt));
        debug_assert!(self.is_real_path_compatible_with(excerpt));
    }

    // Algorithm 6.4
    #[allow(clippy::mutable_key_type)]
    fn reorganise_descendants_for_real_path_compatibility(
        &self,
        excerpt: &BTreeSet<Rc<E>>,
        base_node: &Rc<Self>,
        changes: &mut BTreeSet<(Rc<Self>, Rc<Self>)>,
    ) {
        debug_assert!(self.elements.is_subset(excerpt));
        let mut keys = BTreeSet::<Rc<E>>::from_iter(
            (excerpt.oso_iter() & self.r_children.borrow().oso_keys()).cloned(),
        );
        while let Some(key) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
            let r_child_before = Rc::clone(&r_child);
            if !excerpt.is_superset(&r_child_keys) {
                self.split(key, excerpt);
                let r_child_after = self.get_r_child(key).unwrap();
                r_child_after.fix_v_links_for_changes(changes);
                base_node
                    .fix_v_links_for_pair(&(Rc::clone(&r_child_before), Rc::clone(&r_child_after)));
                changes.insert((Rc::clone(&r_child_before), Rc::clone(&r_child_after)));
                debug_assert!(self.is_real_path_compatible_with(excerpt));
            } else if !excerpt.is_superset(&(&r_child.elements - &self.elements)) {
                self.interpose_for_real_compatibility(key, excerpt);
                let r_child_after = self.get_r_child(key).unwrap();
                r_child_after.fix_v_links_for_changes(changes);
                base_node
                    .fix_v_links_for_pair(&(Rc::clone(&r_child_before), Rc::clone(&r_child_after)));
                changes.insert((Rc::clone(&r_child_before), Rc::clone(&r_child_after)));
                debug_assert!(self.is_recursive_real_path_compatible_with(excerpt));
            } else {
                r_child.reorganise_descendants_for_real_path_compatibility(
                    excerpt, base_node, changes,
                );
                debug_assert!(self.is_recursive_real_path_compatible_with(excerpt));
            }
            keys = &keys - &r_child_keys;
        }
        debug_assert!(self.is_recursive_real_path_compatible_with(excerpt));
    }

    // Algorithm 6.6
    fn interpose_for_virtual_compatibility(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        let (v_child, v_child_keys) = self.get_v_child_and_keys(key).unwrap();
        let elements = &v_child.elements - excerpt;
        let v_children = v_child.merged_children();
        let new_node = Self::new_subset(elements, v_children);
        new_node.insert_v_child(v_child.elements.difference(&new_node.elements), &v_child);
        self.insert_r_child(excerpt.intersection(&v_child_keys), &new_node);
        self.delete_v_children(excerpt.intersection(&v_child_keys));
    }

    // Algorithm 6.7
    #[allow(clippy::mutable_key_type)]
    fn reorganise_descendants_for_full_compatibility(
        &self,
        excerpt: &BTreeSet<Rc<E>>,
        base_node: &Rc<Self>,
        changes: &mut BTreeSet<(Rc<Self>, Rc<Self>)>,
    ) {
        let mut keys = BTreeSet::<Rc<E>>::from_iter(
            (excerpt.oso_iter() & self.v_children.borrow().oso_keys()).cloned(),
        );
        while let Some(key) = keys.first() {
            let (v_child, v_child_keys) = self.get_v_child_and_keys(key).unwrap();
            if excerpt.is_superset(&v_child_keys) {
                keys = &keys - &v_child_keys;
            } else {
                self.interpose_for_virtual_compatibility(key, excerpt);
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                r_child.fix_v_links_for_changes(changes);
                base_node.fix_v_links_for_pair(&(Rc::clone(&v_child), Rc::clone(&r_child)));
                changes.insert((Rc::clone(&v_child), Rc::clone(&r_child)));
                keys = &keys - &r_child_keys;
                debug_assert!(self.is_compatible_with(excerpt));
            }
        }
        let mut keys = BTreeSet::from_iter(
            (excerpt.oso_iter() & self.r_children.borrow().oso_keys()).cloned(),
        );
        while let Some(key) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
            r_child.reorganise_descendants_for_full_compatibility(excerpt, base_node, changes);
            keys = &keys - &r_child_keys;
        }
        debug_assert!(
            self.is_compatible_with(excerpt),
            "{self:?} is not compatible with {excerpt:?}"
        );
    }

    #[allow(clippy::mutable_key_type)]
    fn reorganize_paths_for_compatibility(&self, excerpt: &BTreeSet<Rc<E>>, base_node: &Rc<Self>) {
        let mut changes = BTreeSet::<(Rc<Self>, Rc<Self>)>::new();
        base_node.reorganise_descendants_for_real_path_compatibility(
            excerpt,
            base_node,
            &mut changes,
        );
        debug_assert!(self.is_recursive_real_path_compatible_with(excerpt));
        base_node.reorganise_descendants_for_full_compatibility(excerpt, base_node, &mut changes);
    }

    // Algorithm 6.9
    #[allow(clippy::mutable_key_type)]
    fn fix_v_links_for_changes(&self, changes: &BTreeSet<(Rc<Self>, Rc<Self>)>) {
        for (node1, node2) in changes.iter() {
            if node2.elements.is_superset(&self.elements) {
                for key in node2.elements.iter() {
                    if let Some(v_child) = self.get_v_child(key) {
                        if v_child == *node1 {
                            let overwritten = self
                                .v_children
                                .borrow_mut()
                                .insert(Rc::clone(key), Rc::clone(node2));
                            debug_assert!(overwritten.is_some());
                        }
                    }
                }
            }
        }
    }

    // Algorithm 6.10
    fn fix_v_links_for_pair(&self, nodes: &(Rc<Self>, Rc<Self>)) {
        if nodes.1.elements.is_superset(&self.elements) {
            let keys = &nodes.1.elements - &self.elements;
            for key in keys.iter() {
                if let Some(v_child) = self.get_v_child(key) {
                    if v_child == nodes.0 {
                        self.v_children
                            .borrow_mut()
                            .insert(Rc::clone(key), Rc::clone(&nodes.1));
                    }
                }
            }
            let mut keys = &keys & &self.get_r_keys();
            while let Some(key) = keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                r_child.fix_v_links_for_pair(nodes);
                keys = &keys - &r_child_keys;
            }
        }
    }

    fn merged_children(&self) -> RefCell<ChildMap<E>> {
        let mut map = BTreeMap::new();
        for (key, v) in self.r_children.borrow().iter() {
            assert!(map.insert(Rc::clone(key), Rc::clone(v)).is_none());
        }
        for (key, v) in self.v_children.borrow().iter() {
            assert!(map.insert(Rc::clone(key), Rc::clone(v)).is_none());
        }
        RefCell::new(map)
    }
}

trait Engine<E: ItemTraits>: Sized {
    fn absorb(&self, insertion: &BTreeSet<Rc<E>>, new_insert_is: &mut Option<Rc<TreeNode<E>>>);
}

impl<E: ItemTraits> Engine<E> for Rc<TreeNode<E>> {
    // Algorithm: 6.11
    fn absorb(&self, insertion: &BTreeSet<Rc<E>>, new_insert_is: &mut Option<Rc<TreeNode<E>>>) {
        debug_assert!(self.is_recursive_compatible_with(insertion));
        let keys = insertion - &self.elements;
        if keys.is_empty() {
            debug_assert_eq!(insertion, &self.elements);
            *new_insert_is = Some(Rc::clone(self));
            self.incr_insert_count();
        } else {
            // Get these keys before any new child so that we don't double up
            let mut absorb_keys = &keys & &self.get_r_keys();
            let unused_keys = &keys - &self.get_merged_keys();
            if !unused_keys.is_empty() {
                if let Some(new_node) = new_insert_is {
                    debug_assert_eq!(&new_node.elements, insertion);
                    self.insert_v_child(unused_keys.iter(), new_node);
                } else {
                    let new_node = TreeNode::<E>::new_insert(insertion.clone());
                    self.insert_r_child(unused_keys.iter(), &new_node);
                    *new_insert_is = Some(new_node);
                }
            }
            while let Some(key) = absorb_keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                r_child.absorb(insertion, new_insert_is);
                absorb_keys = &absorb_keys - &r_child_keys;
            }
            self.incr_subset_count();
        }
    }
}

#[derive(Default, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct Answer<E: ItemTraits> {
    pub set: BTreeSet<Rc<E>>,
    pub insert_count: usize,
    pub subset_count: usize,
}

#[derive(PartialOrd, PartialEq, Ord, Eq, Debug)]
pub enum Class {
    Insertion,
    Subset,
    Both,
}

impl<E: ItemTraits> Answer<E> {
    pub fn class(&self) -> Class {
        if self.insert_count > 0 {
            if self.subset_count > 0 {
                Class::Both
            } else {
                Class::Insertion
            }
        } else if self.subset_count > 0 {
            Class::Subset
        } else {
            panic!("there's a bug somewhere")
        }
    }
}

#[derive(Default)]
pub struct RedundantDiscriminationTree<E: ItemTraits> {
    root: Rc<TreeNode<E>>,
}

impl<E: ItemTraits> RedundantDiscriminationTree<E> {
    pub fn new() -> Self {
        Self {
            root: TreeNode::<E>::tabula_rasa(),
        }
    }

    /// Convert a BTreeSet<E> to a BTreeSet<Rc<E>> using existing Rc<E>
    /// instances where available.
    fn convert(&self, mut raw_excerpt: BTreeSet<E>) -> BTreeSet<Rc<E>> {
        let mut insertion = BTreeSet::<Rc<E>>::new();
        while let Some(element) = raw_excerpt.pop_first() {
            if let Some(key) = self.root.find_key(&element) {
                insertion.insert(Rc::clone(&key));
            } else {
                insertion.insert(Rc::new(element));
            }
        }
        insertion
    }

    pub fn insert(&mut self, raw_excerpt: BTreeSet<E>) -> BTreeSet<Rc<E>> {
        let insertion = self.convert(raw_excerpt);
        self.root
            .reorganize_paths_for_compatibility(&insertion, &self.root);
        debug_assert!(self.root.is_recursive_compatible_with(&insertion));
        let mut new_insert_is: Option<Rc<TreeNode<E>>> = None;
        self.root.absorb(&insertion, &mut new_insert_is);
        debug_assert!(self.root.verify_tree());
        debug_assert!(self.root.elements.is_empty() && self.root.v_children.borrow().is_empty());
        insertion
    }

    pub fn complete_match(&self, query: BTreeSet<E>) -> Option<Answer<E>> {
        let query = self.convert(query);
        let mut matched_node = Rc::clone(&self.root);
        // NB: even though root.elements is always empty we do this to get the right type of Iterator
        while let Some(key) =
            (query.oso_iter() - Rc::clone(&matched_node).elements.oso_iter()).next()
        {
            if let Some(r_child) = matched_node.get_r_child(key) {
                matched_node = r_child;
            } else if let Some(v_child) = matched_node.get_v_child(key) {
                matched_node = v_child;
            } else {
                return None;
            }
        }
        let insert_count = matched_node.insert_count.get();
        let subset_count = matched_node.subset_count.get();
        Some(Answer {
            set: query,
            insert_count,
            subset_count,
        })
    }

    pub fn partial_matches(&self, _query: &BTreeSet<E>) -> BTreeSet<Answer<E>> {
        BTreeSet::new()
    }

    pub fn verify_tree(&self) -> bool {
        self.root.verify_tree()
    }
}

// Debug Helpers
impl<E: ItemTraits> TreeNode<E> {
    fn verify_tree_node(&self) -> bool {
        let mut result = true;
        let r_keys = self.get_r_keys();
        let v_keys = self.get_v_keys();
        if !r_keys.is_disjoint(&self.elements) {
            println!(
                "FAIL: TreeNode: {:?} Real Keys {r_keys:?} overlap  elements",
                self.elements,
            );
            result = false;
        };
        if !v_keys.is_disjoint(&self.elements) {
            println!(
                "FAIL: TreeNode: {:?} Virtual Keys {r_keys:?} overlap  elements",
                self.elements,
            );
            result = false;
        };
        if !r_keys.is_disjoint(&v_keys) {
            let overlap = &r_keys & &v_keys;
            println!("FAIL:  {self:?} real and virtual child keys overlap: {overlap:?}");
            result = false;
        };
        result
    }

    fn verify_tree(&self) -> bool {
        if !self.verify_tree_node() {
            println!("({self:?}).verify_tree_node() FAILED");
            false
        } else {
            let mut keys = self.get_r_keys();
            while let Some(key) = keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                keys = &keys - &r_child_keys;
                if !r_child.verify_tree() {
                    println!("REAL: ({r_child:?}).verify_tree() FAILED");
                    return false;
                }
            }
            let mut keys = self.get_v_keys();
            while let Some(key) = keys.first() {
                let (v_child, v_child_keys) = self.get_v_child_and_keys(key).unwrap();
                keys = &keys - &v_child_keys;
                if !v_child.verify_tree() {
                    println!("VIRTUAL: ({v_child:?}).verify_tree() FAILED");
                    return false;
                }
            }
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut rdt = RedundantDiscriminationTree::<&str>::new();
        assert!(rdt
            .complete_match(BTreeSet::from(["a", "b", "c", "d"]))
            .is_none());
        let _abcd = rdt.insert(BTreeSet::from(["a", "b", "c", "d"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );

        let _abc = rdt.insert(BTreeSet::from(["a", "b", "c"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"]))
                .unwrap()
                .class(),
            Class::Both
        );

        let _abd = rdt.insert(BTreeSet::from(["a", "b", "d"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );

        let _ad = rdt.insert(BTreeSet::from(["a", "d"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
    }

    #[test]
    fn it_works_in_reverse_order() {
        let mut rdt = RedundantDiscriminationTree::<&str>::new();
        assert!(rdt.complete_match(BTreeSet::from(["a", "d"])).is_none());
        let _ad = rdt.insert(BTreeSet::from(["a", "d"]));
        assert!(rdt.verify_tree(), "a,d");
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );

        let _abd = rdt.insert(BTreeSet::from(["a", "b", "d"]));
        assert!(rdt.verify_tree(), "a,b, d");
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );

        let _abc = rdt.insert(BTreeSet::from(["a", "b", "c"]));
        assert!(rdt.verify_tree(), "a,b, c");
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"]))
                .unwrap()
                .class(),
            Class::Insertion
        );

        let _abcd = rdt.insert(BTreeSet::from(["a", "b", "c", "d"]));
        assert!(rdt.verify_tree(), "a,b, c, d");
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"]))
                .unwrap()
                .class(),
            Class::Both
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"]))
                .unwrap()
                .class(),
            Class::Insertion
        );
    }
}
