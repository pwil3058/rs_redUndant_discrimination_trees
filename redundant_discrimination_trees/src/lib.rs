use ord_set_ops_iter::adapter::{OrdSetOpsMapAdaption, OrdSetOpsSetAdaption};
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::rc::Rc;

struct TreeNode<E: Ord> {
    elements: BTreeSet<Rc<E>>,
    r_children: RefCell<BTreeMap<Rc<E>, Rc<Self>>>,
    v_children: RefCell<BTreeMap<Rc<E>, Rc<Self>>>,
    excerpt_count: Cell<usize>,
    epitome_count: Cell<usize>,
}

impl<E: Ord + Clone> Default for TreeNode<E> {
    fn default() -> Self {
        Self {
            elements: BTreeSet::new(),
            r_children: RefCell::new(BTreeMap::new()),
            v_children: RefCell::new(BTreeMap::new()),
            excerpt_count: Cell::new(0),
            epitome_count: Cell::new(0),
        }
    }
}

impl<E: Ord + Clone> PartialEq for TreeNode<E> {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl<E: Ord + Clone> Eq for TreeNode<E> {}

impl<E: Ord + Clone + Clone> PartialOrd for TreeNode<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.elements.partial_cmp(&other.elements)
    }
}

impl<E: Ord + Clone> Ord for TreeNode<E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a, E: 'a + Ord + Clone> TreeNode<E> {
    fn tabula_rasa() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn new_excerpt(elements: BTreeSet<Rc<E>>) -> Rc<Self> {
        Rc::new(Self {
            excerpt_count: Cell::new(1),
            elements,
            ..Self::default()
        })
    }

    pub fn new_epitome(
        elements: BTreeSet<Rc<E>>,
        v_children: RefCell<BTreeMap<Rc<E>, Rc<Self>>>,
    ) -> Rc<Self> {
        Rc::new(Self {
            elements,
            v_children,
            epitome_count: Cell::new(1),
            ..Self::default()
        })
    }

    fn insert_r_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, r_child: &Rc<Self>) {
        let mut r_children = self.r_children.borrow_mut();
        for key in iter {
            r_children.insert(Rc::clone(key), Rc::clone(r_child));
        }
    }

    fn insert_v_child<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I, v_child: &Rc<Self>) {
        let mut v_children = self.v_children.borrow_mut();
        for key in iter {
            v_children.insert(Rc::clone(key), Rc::clone(v_child));
        }
    }

    fn delete_v_children<I: Iterator<Item = &'a Rc<E>>>(&self, iter: I) {
        let mut v_children = self.v_children.borrow_mut();
        for key in iter {
            v_children.remove(key);
        }
    }

    fn incr_excerpt_count(&self) {
        self.excerpt_count.set(self.excerpt_count.get() + 1)
    }

    fn incr_epitome_count(&self) {
        self.epitome_count.set(self.epitome_count.get() + 1)
    }
}

impl<E: Ord + Clone> TreeNode<E> {
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

    /// Convert a BTreeSet<E> to a BTreeSet<Rc<E>> using existing Rc<E>
    /// instances where available.
    fn convert(&self, mut raw_excerpt: BTreeSet<E>) -> BTreeSet<Rc<E>> {
        let mut excerpt = BTreeSet::<Rc<E>>::new();
        while let Some(element) = raw_excerpt.pop_first() {
            if let Some(key) = self.find_key(&element) {
                excerpt.insert(key.clone());
            } else {
                excerpt.insert(Rc::new(element));
            }
        }
        excerpt
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
        r_child.insert_v_child(r_child.elements.difference(&new_node.elements), &r_child);
        self.insert_r_child(excerpt.intersection(&r_child_keys), &new_node);
    }

    // Algorithm 6.4
    fn reorganize_paths_part1(
        &self,
        excerpt: &BTreeSet<Rc<E>>,
        base_node: &Rc<Self>,
        changes: &mut BTreeSet<(Rc<Self>, Rc<Self>)>,
    ) {
        let mut keys: BTreeSet<Rc<E>> = (excerpt.oso_iter() & self.v_children.borrow().oso_keys())
            .cloned()
            .collect();
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
        let mut keys: BTreeSet<Rc<E>> = (excerpt.oso_iter() & self.v_children.borrow().oso_keys())
            .cloned()
            .collect();
        while let Some(key) = keys.first() {
            let (v_child, v_child_keys) = self.get_v_child_and_keys(key).unwrap();
            if excerpt.is_superset(&v_child_keys) {
                keys = &keys - &v_child_keys;
            } else {
                self.interpose_for_virtual_compatibility(key, excerpt);
                let (r_child, r_child_keys) = self.get_r_child_and_keys(key).unwrap();
                r_child.fix_v_links_for_changes(changes);
                base_node.fix_v_links_for_pair(&(v_child.clone(), r_child.clone()));
                changes.insert((v_child.clone(), r_child.clone()));
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

    fn reorganize_paths_for_compatibility(&self, excerpt: &BTreeSet<Rc<E>>, base_node: &Rc<Self>) {
        let mut changes = BTreeSet::<(Rc<Self>, Rc<Self>)>::new();
        base_node.reorganize_paths_part1(excerpt, base_node, &mut changes);
        base_node.reorganize_paths_part2(excerpt, base_node, &mut changes);
    }

    // Algorithm 6.9
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
            let mut keys = &keys & &self.r_children.borrow().keys().cloned().collect();
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
        if let Some(r_child) = r_children.get(key) {
            let mut keys = BTreeSet::new();
            let v_children = self.v_children.borrow();
            for key in r_child
                .elements
                .difference(&self.elements)
                .filter(|k| !v_children.contains_key(*k))
            {
                if let Some(rdt_i) = r_children.get(key) {
                    if rdt_i == r_child {
                        keys.insert(key.clone());
                    }
                }
            }
            Some((Rc::clone(r_child), keys))
        } else {
            None
        }
    }

    fn get_v_child(&self, key: &E) -> Option<Rc<Self>> {
        let v_children = self.v_children.borrow();
        v_children.get(key).map(Rc::clone)
    }

    fn get_v_child_and_keys(&self, key: &Rc<E>) -> Option<(Rc<Self>, BTreeSet<Rc<E>>)> {
        let v_children = self.v_children.borrow();
        if let Some(v_child) = v_children.get(key) {
            let mut keys = BTreeSet::new();
            let r_children = self.r_children.borrow();
            for i in v_child
                .elements
                .difference(&self.elements)
                .filter(|k| !r_children.contains_key(*k))
            {
                if let Some(rdt_i) = v_children.get(i) {
                    if rdt_i == v_child {
                        keys.insert(i.clone());
                    }
                }
            }
            Some((Rc::clone(v_child), keys))
        } else {
            None
        }
    }

    fn merged_children(&self) -> RefCell<BTreeMap<Rc<E>, Rc<Self>>> {
        let mut map = BTreeMap::new();
        // If these panic it means that there's been an implementation error somewhere
        for (key, v) in self.r_children.borrow().iter() {
            map.insert(Rc::clone(key), Rc::clone(v)).unwrap();
        }
        for (key, v) in self.v_children.borrow().iter() {
            map.insert(Rc::clone(key), Rc::clone(v)).unwrap();
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

trait Engine<E: Ord + Clone>: Sized {
    fn absorb(&self, excerpt: &BTreeSet<Rc<E>>, new_excerpt: &mut Option<Rc<TreeNode<E>>>);
    fn complete_match(&self, query: BTreeSet<E>) -> Option<Self>;
}

impl<E: Ord + Clone> Engine<E> for Rc<TreeNode<E>> {
    // Algorithm: 6.11
    fn absorb(&self, excerpt: &BTreeSet<Rc<E>>, new_excerpt: &mut Option<Rc<TreeNode<E>>>) {
        let keys = excerpt - &self.elements;
        if keys.is_empty() {
            *new_excerpt = Some(Rc::clone(self));
            self.incr_excerpt_count();
        } else {
            let mut a_keys = &keys & &self.r_children.borrow().keys().cloned().collect();
            while let Some(j) = a_keys.first() {
                let (r_child, r_child_keys) = self.get_r_child_and_keys(j).unwrap();
                r_child.absorb(excerpt, new_excerpt);
                a_keys = &a_keys - &r_child_keys;
            }
            let mut a_keys = &keys - &self.r_children.borrow().keys().cloned().collect();
            a_keys = &a_keys - &self.v_children.borrow().keys().cloned().collect();
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

    // Algorithm: 6.13
    fn complete_match(&self, query: BTreeSet<E>) -> Option<Self> {
        let mut p = Rc::clone(self);
        let query = self.convert(query);
        let mut keys = query.oso_iter() - self.elements.oso_iter();
        while let Some(key) = keys.next() {
            if let Some(node) = p.get_r_child(key) {
                p = node;
                keys = query.oso_iter() - p.elements.oso_iter();
            } else if let Some(node) = p.get_v_child(key) {
                p = node;
                keys = query.oso_iter() - p.elements.oso_iter();
            } else {
                return None;
            }
        }
        Some(p)
    }
}

#[derive(Default)]
pub struct SimpleAnswer {
    pub excerpt_count: usize,
    pub epitome_count: usize,
}

#[derive(Default)]
pub struct RedundantDiscriminationTree<E: Ord + Clone> {
    root: Rc<TreeNode<E>>,
}

impl<E: Ord + Clone + Debug> RedundantDiscriminationTree<E> {
    pub fn new() -> Self {
        Self {
            root: TreeNode::<E>::tabula_rasa(),
        }
    }

    pub fn insert(&mut self, raw_excerpt: BTreeSet<E>) {
        let excerpt = self.root.convert(raw_excerpt);
        self.root
            .reorganize_paths_for_compatibility(&excerpt, &self.root);
        debug_assert!(self.root.is_recursive_compatible_with(&excerpt));
        let mut new_excerpt: Option<Rc<TreeNode<E>>> = None;
        self.root.absorb(&excerpt, &mut new_excerpt);
        debug_assert!(self.root.verify_tree());
    }

    fn complete_match_node(&self, query: BTreeSet<E>) -> Option<Rc<TreeNode<E>>> {
        // Implement RedundantDiscriminationTree's complete_match()
        self.root.complete_match(query)
    }

    pub fn complete_match(&self, query: BTreeSet<E>) -> Option<SimpleAnswer> {
        let matched_node = self.complete_match_node(query)?;
        let excerpt_count = matched_node.excerpt_count.get();
        let epitome_count = matched_node.epitome_count.get();
        Some(SimpleAnswer {
            excerpt_count,
            epitome_count,
        })
    }
}

// Debug Helpers
fn format_set<E: Ord + Debug + Clone>(set: &BTreeSet<Rc<E>>) -> String {
    let v: Vec<&Rc<E>> = set.iter().collect();
    format!("{v:?}")
}

impl<E: Ord + Debug + Clone> TreeNode<E> {
    fn format_mop_short(&self) -> String {
        let elements: Vec<&Rc<E>> = self.elements.iter().collect();
        let r_children = self.r_children.borrow();
        let r_children_keys: Vec<&Rc<E>> = r_children.keys().collect();
        let v_children = self.v_children.borrow();
        let v_children_keys: Vec<&Rc<E>> = v_children.keys().collect();
        format!("C: {elements:?} I_r: {r_children_keys:?} I_v: {v_children_keys:?}")
    }

    fn format_mop(&self) -> String {
        if self.r_children.borrow().len() == 0 && self.v_children.borrow().len() == 0 {
            return format!("C: {} {{}}", format_set(&self.elements));
        }
        let mut fstr = format!("C: {} {{\n", format_set(&self.elements));
        let mut keys: BTreeSet<Rc<E>> = self.r_children.borrow().keys().map(Rc::clone).collect();
        while let Some(j) = keys.first() {
            let (r_child, r_child_keys) = self.get_r_child_and_keys(j).unwrap();
            let tstr = format!(
                "\tR: {} -> {}\n",
                format_set(&r_child_keys),
                r_child.format_mop_short()
            );
            fstr.push_str(&tstr);
            keys = &keys - &r_child_keys;
        }
        let mut keys: BTreeSet<Rc<E>> = self.v_children.borrow().keys().map(Rc::clone).collect();
        while let Some(j) = keys.first() {
            let (v_child, v_child_keys) = self.get_v_child_and_keys(j).unwrap();
            let tstr = format!(
                "\tV: {} -> {}\n",
                format_set(&v_child_keys),
                v_child.format_mop_short()
            );
            fstr.push_str(&tstr);
            keys = &keys - &v_child_keys;
        }
        fstr.push('}');
        fstr
    }

    fn verify_tree_node(&self) -> bool {
        let mut result = true;
        let r_keys = BTreeSet::<Rc<E>>::from_iter(self.r_children.borrow().keys().map(Rc::clone));
        let v_keys = BTreeSet::<Rc<E>>::from_iter(self.v_children.borrow().keys().map(Rc::clone));
        if !r_keys.is_disjoint(&self.elements) {
            println!(
                "real indices overlap C {} <> {}",
                format_set(&r_keys),
                format_set(&self.elements)
            );
            result = false;
        };
        if !v_keys.is_disjoint(&self.elements) {
            println!(
                "virt indices overlap C {} <> {}",
                format_set(&v_keys),
                format_set(&self.elements)
            );
            result = false;
        };
        if !r_keys.is_disjoint(&v_keys) {
            println!("child indices overlap {}", self.format_mop());
            result = false;
        };
        result
    }

    fn verify_tree(&self) -> bool {
        let mut result = self.verify_tree_node();
        let mut keys = BTreeSet::<Rc<E>>::from_iter(self.r_children.borrow().keys().map(Rc::clone));
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
        let excerpt: BTreeSet<&str> = ["a", "b", "c", "d"].into();
        assert!(rdt.complete_match(excerpt.clone()).is_none());
        rdt.insert(excerpt.clone());
        assert!(rdt.complete_match(excerpt.clone()).is_some());
    }
}
