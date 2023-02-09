use std::sync::Arc;

pub trait MemoryOrganizationPacket<E: Ord>: Default {
    fn tabula_rasa() -> Arc<Self>;
    fn new_trace(elements: &[E]) -> Arc<Self>;
    fn new_epitome(elements: &[E], children: &[(E, Arc<Self>)]) -> Arc<Self>;

    fn children(&self) -> Vec<Arc<Self>>;
    fn elements(&self) -> Vec<E>;
    fn num_children(&self) -> usize;
    fn len(&self) -> usize;
    fn is_trace(&self) -> bool;

    fn is_epitome(&self) -> bool {
        self.num_children() > 0
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
