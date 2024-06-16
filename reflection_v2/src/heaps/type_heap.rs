use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Weak};

use owning_ref::OwningRef;
use parking_lot::{Mutex, MutexGuard};
use petgraph::prelude::{Graph, NodeIndex};

use private::*;

use crate::metadata::types::Type;
use crate::metadata::UniqueIdentifier;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TypeRelationship {
	Base,
	Impl,
	Field,
}

impl Display for TypeRelationship {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			TypeRelationship::Base => write!(f, "Points to"),
			TypeRelationship::Impl => write!(f, "Implements"),
			TypeRelationship::Field => write!(f, "Contains"),
		}
	}
}

#[derive(Clone)]
pub struct TypeRef {
	idx: NodeIndex,
	heap: Weak<TypeHeap>,
	ref_idx: Option<NodeIndex>,
}

impl TypeRef {
	#[tracing::instrument(skip_all)]
	pub fn get(&self) -> Arc<Type> {
		let heap = self.heap();
		let heap = heap.inner.lock();
		let entry = &heap.graph[self.idx];
		match entry {
			Entry::Ref(_) => unreachable!(),
			Entry::Def(ty) => ty.clone(),
		}
	}

	#[inline]
	pub fn node_idx(&self) -> NodeIndex {
		self.ref_idx.unwrap_or(self.idx)
	}

	#[inline(always)]
	pub(crate) fn heap(&self) -> Arc<TypeHeap> {
		self.heap.upgrade().expect("Parent TypeHeap has been dropped")
	}
}

impl Display for TypeRef {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.get().fmt(f)
	}
}
impl Eq for TypeRef {}
impl PartialEq for TypeRef {
	#[inline]
	fn eq(&self, other: &Self) -> bool {
		self.idx == other.idx && self.heap.as_ptr() == other.heap.as_ptr()
	}
}
impl Hash for TypeRef {
	#[inline]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.idx.hash(state);
		self.heap.as_ptr().hash(state);
	}
}

pub struct TypeHeap {
	inner: Mutex<Inner>,
	self_ref: Weak<TypeHeap>,
}

impl TypeHeap {
	#[inline]
	pub fn new() -> Arc<Self> {
		Arc::new_cyclic(|self_ref| Self {
			inner: Default::default(),
			self_ref: self_ref.clone(),
		})
	}

	#[inline]
	pub fn new_struct(&self, id: UniqueIdentifier) -> TypeRef {
		self.intern(Type::new_struct(id))
	}

	#[inline]
	pub fn as_graph(&self) -> OwningRef<MutexGuard<Inner>, Graph<Entry, TypeRelationship>> {
		OwningRef::new(self.inner.lock()).map(|inner| &inner.graph)
	}

	#[inline]
	pub(crate) fn intern<T: Intern>(&self, ty: T) -> TypeRef {
		ty.intern(self)
	}

	#[inline]
	pub(crate) fn intern_all<'a>(&self, iter: impl IntoIterator<Item = &'a mut TypeRef>) {
		for ty in iter {
			*ty = self.intern(ty.clone());
		}
	}

	#[inline]
	#[tracing::instrument(skip_all)]
	pub(crate) fn add_edges<'l>(
		&self,
		ty: &TypeRef,
		iter: impl IntoIterator<Item = &'l TypeRef>,
		rel: TypeRelationship,
	) {
		let mut inner = self.inner.lock();
		assert!(inner.map.contains_key(ty));
		let ty = ty.node_idx();
		for node in iter {
			assert!(inner.map.contains_key(node));
			let node = node.node_idx();
			if inner.graph.edges_connecting(ty, node).all(|e| *e.weight() != rel) {
				inner.graph.add_edge(ty, node, rel);
			}
		}
	}
}

mod private {
	use std::fmt::{Display, Formatter};
	use std::sync::Arc;

	use indexmap::IndexMap;
	use petgraph::Graph;
	use petgraph::graph::NodeIndex;

	use crate::heaps::type_heap::{TypeRef, TypeRelationship};
	use crate::heaps::TypeHeap;
	use crate::metadata::types::{Type, TypeKind};

	#[derive(Default)]
	pub struct Inner {
		pub(super) map: IndexMap<TypeRef, NodeIndex>,
		pub(super) graph: Graph<Entry, TypeRelationship>,
	}

	pub enum Entry {
		Ref(TypeRef),
		Def(Arc<Type>),
	}

	impl Display for Entry {
		fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
			match self {
				Entry::Def(ty) => write!(f, "{}", ty),
				Entry::Ref(ty) => write!(f, "{} [ref]", ty.get()),
			}
		}
	}

	pub trait Intern {
		fn intern(self, heap: &TypeHeap) -> TypeRef;
	}

	impl Intern for TypeRef {
		#[tracing::instrument(skip_all)]
		fn intern(self, heap: &TypeHeap) -> TypeRef {
			if std::ptr::eq(self.heap.as_ptr(), heap) {
				return self;
			}

			let idx = {
				let mut inner = heap.inner.lock();
				let Inner { map, graph } = &mut *inner;

				*map.entry(self.clone())
					.or_insert_with(|| graph.add_node(Entry::Ref(self.clone())))
			};

			let ty = self.get();
			match ty.kind() {
				TypeKind::Pointer | TypeKind::Reference => {
					let mut base = ty.base().unwrap();
					base = heap.intern(base);
					let mut inner = heap.inner.lock();
					inner.graph.add_edge(idx, base.node_idx(), TypeRelationship::Base);
				},
				_ => {},
			}

			TypeRef {
				idx: self.idx,
				ref_idx: Some(idx),
				heap: self.heap.clone(),
			}
		}
	}

	impl Intern for Type {
		#[tracing::instrument(skip_all)]
		fn intern(self, heap: &TypeHeap) -> TypeRef {
			let mut inner = heap.inner.lock();
			let Inner { map, graph, .. } = &mut *inner;
			let idx = graph.add_node(Entry::Def(self.into()));
			let ty_ref = TypeRef {
				idx,
				ref_idx: None,
				heap: heap.self_ref.clone(),
			};
			map.insert(ty_ref.clone(), idx);
			ty_ref
		}
	}
}
