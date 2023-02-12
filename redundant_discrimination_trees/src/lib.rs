use ord_set_ops_iter::adapter::{OrdSetOpsMapAdaption, OrdSetOpsSetAdaption};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

pub struct TreeNode<E: Ord> {
    pub elements: BTreeSet<Rc<E>>,
    r_children: BTreeMap<Rc<E>, Rc<Self>>,
    v_children: BTreeMap<Rc<E>, Rc<Self>>,
    excerpt_count: usize,
    epitome_count: usize,
}

impl<E: Ord> Default for TreeNode<E> {
    fn default() -> Self {
        Self {
            elements: BTreeSet::new(),
            r_children: BTreeMap::new(),
            v_children: BTreeMap::new(),
            excerpt_count: 0,
            epitome_count: 0,
        }
    }
}

impl<E: Ord> PartialEq for TreeNode<E> {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl<E: Ord> Eq for TreeNode<E> {}

impl<E: Ord> PartialOrd for TreeNode<E> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.elements.partial_cmp(&other.elements)
    }
}

impl<E: Ord> Ord for TreeNode<E> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a, E: 'a + Ord> TreeNode<E> {
    fn tabula_rasa() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn new_trace(elements: BTreeSet<Rc<E>>) -> Rc<Self> {
        Rc::new(Self {
            excerpt_count: 1,
            elements,
            ..Self::default()
        })
    }

    pub fn excerpts(&self) -> BTreeSet<Rc<TreeNode<E>>> {
        // TODO: implement this properly
        BTreeSet::<Rc<TreeNode<E>>>::new()
    }

    pub fn is_excerpt(&self) -> bool {
        self.excerpt_count > 0
    }

    pub fn is_epitome(&self) -> bool {
        self.epitome_count > 0
    }
}

impl<E: Ord> TreeNode<E> {
    fn find_key(&self, target: &E) -> Option<&Rc<E>> {
        if let Some((key, _)) = self.r_children.get_key_value(target) {
            Some(key)
        } else if let Some((key, _)) = self.v_children.get_key_value(target) {
            Some(key)
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

    // Algorithm 6.4
    fn reorganize_paths_part1(
        &self,
        excerpt: &BTreeSet<Rc<E>>,
        base_node: &Rc<Self>,
        changes: &mut BTreeSet<(Rc<Self>, Rc<Self>)>,
    ) {
    }

    // Algorithm 6.6
    fn interpose_for_virtual_compatibility(&self, key: &Rc<E>, excerpt: &BTreeSet<Rc<E>>) {
        // TODO: implement interpose_for_virtual_compatibility()
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
                base_node.fix_v_links_for_pair((&(v_child.clone(), r_child.clone())));
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
        base_node.reorganize_paths_part1(&excerpt, base_node, &mut changes);
        base_node.reorganize_paths_part2(&excerpt, base_node, &mut changes);
    }

    // Algorithm 6.9
    fn fix_v_links_for_changes(&self, changes: &BTreeSet<(Rc<Self>, Rc<Self>)>) {
        // TODO: Implement fix_v_links_for_changes()
    }

    // Algorithm 6.9
    fn fix_v_links_for_pair(&self, nodes: &(Rc<Self>, Rc<Self>)) {
        // TODO: Implement fix_v_links_for_pair()
    }

    fn get_r_child_and_keys(&self, key: &Rc<E>) -> Option<(Rc<Self>, BTreeSet<Rc<E>>)> {
        // TODO: implement get_r_child_and_keys()
        None
    }

    fn get_v_child_and_keys(&self, key: &Rc<E>) -> Option<(Rc<Self>, BTreeSet<Rc<E>>)> {
        // TODO: implement get_v_child_and_keys()
        None
    }
}

#[derive(Default)]
pub struct RedundantDiscriminationTree<E: Ord> {
    root: Rc<TreeNode<E>>,
}

impl<E: Ord> RedundantDiscriminationTree<E> {
    pub fn new() -> Self {
        Self {
            root: TreeNode::<E>::tabula_rasa(),
        }
    }

    pub fn insert(&mut self, raw_excerpt: BTreeSet<E>) {
        let excerpt = self.root.convert(raw_excerpt);
        self.root
            .reorganize_paths_for_compatibility(&excerpt, &self.root);
    }

    pub fn excerpts(&self) -> BTreeSet<Rc<TreeNode<E>>> {
        self.root.excerpts()
    }
}
