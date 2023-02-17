use ord_set_ops_iter::adapter::{OrdSetOpsMapAdaption, OrdSetOpsSetAdaption};
use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

// TODO: undo using this trait after development is finished
pub trait ItemTraits: Ord + Clone + Debug {}

impl ItemTraits for &str {}

type ChildMap<E> = BTreeMap<Rc<E>, Rc<TreeNode<E>>>;

trait ChildMapOps<E: ItemTraits> {
    fn get_keys(&self) -> BTreeSet<Rc<E>>;
    fn get_child_keys(&self, child: &Rc<TreeNode<E>>) -> BTreeSet<Rc<E>>;
}

impl<E: ItemTraits> ChildMapOps<E> for ChildMap<E> {
    fn get_keys(&self) -> BTreeSet<Rc<E>> {
        BTreeSet::from_iter(self.keys().map(Rc::clone))
    }

    // Although expensive this will be useful for verifying alternative method results
    fn get_child_keys(&self, child: &Rc<TreeNode<E>>) -> BTreeSet<Rc<E>> {
        BTreeSet::from_iter(
            self.iter()
                .filter(|(_, v)| v == &child)
                .map(|(k, _)| Rc::clone(k)),
        )
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
        let r_children = self.get_r_children();
        let v_children = self.get_v_children();
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

    pub fn new_excerpt(elements: BTreeSet<Rc<E>>) -> Rc<Self> {
        Rc::new(Self {
            insert_count: Cell::new(1),
            elements,
            ..Self::default()
        })
    }

    pub fn new_epitome(elements: BTreeSet<Rc<E>>, v_children: RefCell<ChildMap<E>>) -> Rc<Self> {
        Rc::new(Self {
            elements,
            v_children,
            subset_count: Cell::new(1),
            ..Self::default()
        })
    }

    fn insert_r_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, r_child: &Rc<Self>) {
        let mut r_children = self.r_children.borrow_mut();
        for key in iter {
            r_children.insert(Rc::clone(key), Rc::clone(r_child));
        }
    }

    fn get_r_children(&self) -> BTreeSet<BTreeSet<Rc<E>>> {
        let mut set = BTreeSet::new();
        let r_children = self.r_children.borrow();

        let mut r_keys = r_children.get_keys();
        while let Some(r_key) = r_keys.first() {
            let r_child = r_children.get(r_key).unwrap();
            let r_child_keys = &r_child.elements - &self.elements;
            set.insert(r_child.elements.clone());
            r_keys = &r_keys - &r_child_keys;
        }
        set
    }

    fn insert_v_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, v_child: &Rc<Self>) {
        let mut v_children = self.v_children.borrow_mut();
        for key in iter {
            v_children.insert(Rc::clone(key), Rc::clone(v_child));
        }
    }

    fn get_v_children(&self) -> BTreeSet<BTreeSet<Rc<E>>> {
        let mut set = BTreeSet::new();
        let v_children = self.v_children.borrow();

        let mut v_keys = v_children.get_keys();
        while let Some(v_key) = v_keys.first() {
            let v_child = v_children.get(v_key).unwrap();
            let v_child_keys = &v_child.elements - &self.elements;
            set.insert(v_child.elements.clone());
            v_keys = &v_keys - &v_child_keys;
        }
        set
    }

    fn delete_v_children<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I) {
        let mut v_children = self.v_children.borrow_mut();
        for key in iter {
            v_children.remove(key);
        }
    }

    fn incr_excerpt_count(&self) {
        self.insert_count.set(self.insert_count.get() + 1)
    }

    fn incr_epitome_count(&self) {
        self.subset_count.set(self.subset_count.get() + 1)
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
        let elements =
            BTreeSet::<Rc<E>>::from_iter(r_child.elements.intersection(excerpt).cloned());
        let v_children = r_child.merged_children();
        let new_node = Self::new_epitome(elements, v_children);
        r_child.insert_r_child(r_child.elements.difference(&new_node.elements), &r_child);
        self.insert_r_child(r_child_keys.iter(), &new_node);
    }

    // Algorithm 6.3
    fn split(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
        let elements =
            BTreeSet::<Rc<E>>::from_iter(r_child.elements.intersection(excerpt).cloned());
        let v_children = r_child.merged_children();
        let new_node = Self::new_epitome(elements, v_children);
        new_node.insert_v_child(r_child.elements.difference(&new_node.elements), &r_child);
        self.insert_r_child(excerpt.intersection(&r_child_keys), &new_node);
    }

    // Algorithm 6.4
    #[allow(clippy::mutable_key_type)]
    fn reorganize_paths_part1(
        &self,
        excerpt: &BTreeSet<Rc<E>>,
        base_node: &Rc<Self>,
        changes: &mut BTreeSet<(Rc<Self>, Rc<Self>)>,
    ) {
        let mut keys = BTreeSet::<Rc<E>>::from_iter(
            (excerpt.oso_iter() & self.r_children.borrow().oso_keys()).cloned(),
        );
        while let Some(key) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
            let p = &r_child;
            if !excerpt.is_superset(&r_child_keys) {
                self.split(key, excerpt);
                let r_child = self.get_r_child(key).unwrap();
                r_child.fix_v_links_for_changes(changes);
                base_node.fix_v_links_for_pair(&(Rc::clone(p), Rc::clone(&r_child)));
                changes.insert((Rc::clone(p), r_child));
            } else if !excerpt.is_superset(&(&r_child.elements - &self.elements)) {
                self.interpose_for_real_compatibility(key, excerpt);
                let r_child = self.get_r_child(key).unwrap();
                r_child.fix_v_links_for_changes(changes);
                base_node.fix_v_links_for_pair(&(Rc::clone(p), Rc::clone(&r_child)));
                changes.insert((Rc::clone(p), r_child));
            } else {
                r_child.reorganize_paths_part1(excerpt, base_node, changes);
            }
            keys = &keys - &r_child_keys;
        }
    }

    // Algorithm 6.6
    fn interpose_for_virtual_compatibility(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        let (v_child, v_child_keys) = self.get_v_child_and_keys(key).unwrap();
        let elements =
            BTreeSet::<Rc<E>>::from_iter(v_child.elements.intersection(excerpt).cloned());
        let v_children = v_child.merged_children();
        let m = Self::new_epitome(elements, v_children);
        m.insert_v_child(v_child.elements.difference(&m.elements), &v_child);
        self.insert_r_child(excerpt.intersection(&v_child_keys), &m);
        self.delete_v_children(excerpt.intersection(&v_child_keys));
    }

    // Algorithm 6.7
    fn reorganize_paths_part2(
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
            }
        }
        let mut keys: BTreeSet<Rc<E>> = (excerpt.oso_iter() & self.r_children.borrow().oso_keys())
            .cloned()
            .collect();
        while let Some(key) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
            r_child.reorganize_paths_part2(excerpt, base_node, changes);
            keys = &keys - &r_child_keys;
        }
    }

    #[allow(clippy::mutable_key_type)]
    fn reorganize_paths_for_compatibility(&self, excerpt: &BTreeSet<Rc<E>>, base_node: &Rc<Self>) {
        let mut changes = BTreeSet::<(Rc<Self>, Rc<Self>)>::new();
        base_node.reorganize_paths_part1(excerpt, base_node, &mut changes);
        base_node.reorganize_paths_part2(excerpt, base_node, &mut changes);
    }

    // Algorithm 6.9
    #[allow(clippy::mutable_key_type)]
    fn fix_v_links_for_changes(&self, changes: &BTreeSet<(Rc<Self>, Rc<Self>)>) {
        for (node1, node2) in changes.iter() {
            if node2.elements.is_superset(&self.elements) {
                for key in node2.elements.iter() {
                    if let Some(v_child) = self.get_v_child(key) {
                        if v_child == *node1 {
                            self.v_children
                                .borrow_mut()
                                .insert(Rc::clone(key), Rc::clone(node2));
                        }
                    }
                }
            }
        }
    }

    // Algorithm 6.10
    fn fix_v_links_for_pair(&self, nodes: &(Rc<Self>, Rc<Self>)) {
        if nodes.1.elements.is_superset(&self.elements) {
            let keys: BTreeSet<Rc<E>> = nodes
                .1
                .elements
                .difference(&self.elements)
                .cloned()
                .collect();
            for key in keys.iter() {
                if let Some(v_child) = self.get_v_child(key) {
                    if v_child == nodes.0 {
                        self.v_children
                            .borrow_mut()
                            .insert(Rc::clone(key), Rc::clone(&nodes.1));
                    }
                }
            }
            let mut keys = &keys & &self.r_children.borrow().get_keys();
            while let Some(key) = keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                r_child.fix_v_links_for_pair(nodes);
                keys = &keys - &r_child_keys;
            }
        }
    }

    fn get_r_child(&self, key: &E) -> Option<Rc<Self>> {
        let r_children = self.r_children.borrow();
        r_children.get(key).map(Rc::clone)
    }

    fn get_r_child_and_keys(&self, key: &Rc<E>) -> Option<(Rc<Self>, BTreeSet<Rc<E>>)> {
        let r_children = self.r_children.borrow();
        let r_child = r_children.get(key)?;
        let r_child_keys = &r_child.elements - &self.elements;
        Some((Rc::clone(r_child), r_child_keys))
    }

    fn get_v_child(&self, key: &E) -> Option<Rc<Self>> {
        let v_children = self.v_children.borrow();
        v_children.get(key).map(Rc::clone)
    }

    fn get_v_child_and_keys(&self, key: &Rc<E>) -> Option<(Rc<Self>, BTreeSet<Rc<E>>)> {
        let v_children = self.v_children.borrow();
        let v_child = v_children.get(key)?;
        let v_child_keys = &v_child.elements - &self.elements;
        Some((Rc::clone(v_child), v_child_keys))
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

    fn is_recursive_compatible_with(&self, excerpt: &BTreeSet<Rc<E>>) -> bool {
        if self.elements.is_subset(excerpt) {
            for key in excerpt.iter() {
                if let Some(node) = self.get_r_child(key) {
                    if !node.is_recursive_compatible_with(excerpt) {
                        return false;
                    }
                } else if let Some(node) = self.get_v_child(key) {
                    if !node.is_recursive_compatible_with(excerpt) {
                        return false;
                    }
                }
            }
        } else {
            return false;
        }
        true
    }
}

trait Engine<E: ItemTraits>: Sized {
    fn absorb(&self, excerpt: &BTreeSet<Rc<E>>, new_excerpt: &mut Option<Rc<TreeNode<E>>>);
}

impl<E: ItemTraits> Engine<E> for Rc<TreeNode<E>> {
    // Algorithm: 6.11
    fn absorb(&self, excerpt: &BTreeSet<Rc<E>>, new_excerpt: &mut Option<Rc<TreeNode<E>>>) {
        println!("Absorb({excerpt:?}): {self:?}");
        let keys = excerpt - &self.elements;
        if keys.is_empty() {
            *new_excerpt = Some(Rc::clone(self));
            self.incr_excerpt_count();
        } else {
            let mut a_keys = &keys & &self.r_children.borrow().get_keys();
            while let Some(j) = a_keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(j).unwrap();
                r_child.absorb(excerpt, new_excerpt);
                a_keys = &a_keys - &r_child_keys;
            }
            let mut a_keys = &keys - &self.r_children.borrow().get_keys();
            a_keys = &a_keys - &self.v_children.borrow().get_keys();
            if !a_keys.is_empty() {
                if let Some(p) = new_excerpt {
                    self.insert_v_child(a_keys.iter(), p);
                } else {
                    let p = TreeNode::<E>::new_excerpt(excerpt.clone());
                    self.insert_r_child(a_keys.iter(), &p);
                    *new_excerpt = Some(p);
                }
            }
            self.incr_epitome_count();
        }
    }
}

#[derive(Default, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct SimpleAnswer {
    pub insert_count: usize,
    pub subset_count: usize,
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
        let mut excerpt = BTreeSet::<Rc<E>>::new();
        while let Some(element) = raw_excerpt.pop_first() {
            if let Some(key) = self.root.find_key(&element) {
                excerpt.insert(Rc::clone(&key));
            } else {
                excerpt.insert(Rc::new(element));
            }
        }
        excerpt
    }

    pub fn insert(&mut self, raw_excerpt: BTreeSet<E>) {
        let excerpt = self.convert(raw_excerpt);
        self.root
            .reorganize_paths_for_compatibility(&excerpt, &self.root);
        debug_assert!(self.root.is_recursive_compatible_with(&excerpt));
        let mut new_excerpt: Option<Rc<TreeNode<E>>> = None;
        self.root.absorb(&excerpt, &mut new_excerpt);
        debug_assert!(self.root.verify_tree());
    }

    pub fn complete_match(&self, query: BTreeSet<E>) -> Option<SimpleAnswer> {
        let query = self.convert(query);
        let mut matched_node = Rc::clone(&self.root);
        // NB: even though root.elements is always empty we do this to get the right type of Iterator
        let mut keys = query.oso_iter() - self.root.elements.oso_iter();
        while let Some(key) = keys.next() {
            println!("Match: {matched_node:?}");
            if let Some(node) = matched_node.get_r_child(key) {
                matched_node = node;
                keys = query.oso_iter() - matched_node.elements.oso_iter();
            } else if let Some(node) = matched_node.get_v_child(key) {
                matched_node = node;
                keys = query.oso_iter() - matched_node.elements.oso_iter();
            } else {
                return None;
            }
        }
        let insert_count = matched_node.insert_count.get();
        let subset_count = matched_node.subset_count.get();
        Some(SimpleAnswer {
            insert_count,
            subset_count,
        })
    }
}

// Debug Helpers
fn format_set<E: ItemTraits + Clone>(set: &BTreeSet<Rc<E>>) -> String {
    let v: Vec<&Rc<E>> = set.iter().collect();
    format!("{v:?}")
}

impl<E: ItemTraits> TreeNode<E> {
    fn verify_tree_node(&self) -> bool {
        let mut result = true;
        let r_keys = self.r_children.borrow().get_keys();
        let v_keys = self.v_children.borrow().get_keys();
        if !r_keys.is_disjoint(&self.elements) {
            println!(
                "FAIL: TreeNode: {:?} real keys overlap C {} <> {}",
                self.elements,
                format_set(&r_keys),
                format_set(&self.elements)
            );
            result = false;
        };
        if !v_keys.is_disjoint(&self.elements) {
            println!(
                "FAIL: TreeNode: {:?} virt keys overlap C {} <> {}",
                self.elements,
                format_set(&v_keys),
                format_set(&self.elements)
            );
            result = false;
        };
        if !r_keys.is_disjoint(&v_keys) {
            println!("FAIL: child keys overlap {self:?}");
            result = false;
        };
        result
    }

    fn verify_tree(&self) -> bool {
        let mut result = self.verify_tree_node();
        let mut keys = self.r_children.borrow().get_keys();
        while let Some(key) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
            keys = &keys - &r_child_keys;
            result = result && r_child.verify_tree();
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut rdt = RedundantDiscriminationTree::<&str>::new();
        let excerpt = BTreeSet::from(["a", "b", "c", "d"]);
        assert!(rdt
            .complete_match(BTreeSet::from(["a", "b", "c", "d"]))
            .is_none());
        rdt.insert(BTreeSet::from(["a", "b", "c", "d"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"])),
            Some(SimpleAnswer {
                insert_count: 1,
                subset_count: 0
            })
        );

        rdt.insert(BTreeSet::from(["a", "b", "c"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"])),
            Some(SimpleAnswer {
                insert_count: 1,
                subset_count: 0
            })
        );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c"])),
            Some(SimpleAnswer {
                insert_count: 1,
                subset_count: 1
            })
        );

        rdt.insert(BTreeSet::from(["a", "b", "d"]));
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "c", "d"])),
            Some(SimpleAnswer {
                insert_count: 1,
                subset_count: 0
            })
        );
        // assert_eq!(
        //     rdt.complete_match(BTreeSet::from(["a", "b", "c"])),
        //     Some(SimpleAnswer {
        //         insert_count: 1,
        //         subset_count: 1
        //     })
        // );
        assert_eq!(
            rdt.complete_match(BTreeSet::from(["a", "b", "d"])),
            Some(SimpleAnswer {
                insert_count: 1,
                subset_count: 1
            })
        );
    }
}
