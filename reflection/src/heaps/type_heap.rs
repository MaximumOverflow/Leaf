use crate::heaps::general_purpose_heap::ArenaAllocator;
use std::collections::{HashMap, HashSet};
use crate::{Struct, Type};
use std::cell::{RefCell, UnsafeCell};

pub struct TypeHeap<'l> {
	bump: &'l ArenaAllocator,
	set: RefCell<HashSet<usize>>,
	drops: RefCell<Vec<DropHelper>>,
	pointers: RefCell<HashMap<(usize, bool), &'l Type<'l>>>,
	structs: RefCell<HashMap<usize, (&'l Type<'l>, &'l Struct<'l>)>>,
}

#[rustfmt::skip]
impl<'l> TypeHeap<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			structs: Default::default(),
			pointers: Default::default(),
			drops: Default::default(),
			set: Default::default(),
			bump,
		}
	}
}

#[rustfmt::skip]
impl<'l> TypeHeap<'l> {
	pub fn r#struct(&self, ty: Struct<'l>) -> &'l Type<'l> {
		let ty = self.bump.alloc(ty);
		self.struct_ref(ty)
	}

	pub fn struct_cell(&self, ty: Struct<'l>) -> &'l UnsafeCell<Struct<'l>> {
		let ty = self.bump.alloc(UnsafeCell::new(ty));
		self.struct_ref(unsafe { &*ty.get() });
		ty
	}

	pub fn struct_ref(&self, ty: &'l Struct<'l>) -> &'l Type<'l> {
		let mut structs = self.structs.borrow_mut();
		let mut set = self.set.borrow_mut();

		let key = ty as *const _ as usize;
		if let Some((ty, _)) = structs.get(&key) {
			return *ty;
		}

		for field in ty.fields() {
			assert!(set.contains(&as_key(field.ty())), "One or more field types don't belong to this TypeHeap");
		}

		let ty_ref = self.alloc(Type::Struct(ty));
		set.insert(as_key(ty_ref));
		structs.insert(key, (ty_ref, ty));
		ty_ref
	}

	pub fn pointer(&self, ty: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut pointers = self.pointers.borrow_mut();
		let mut set = self.set.borrow_mut();

		let key = (as_key(ty), mutable);
		if let Some(ty) = pointers.get(&key) {
			return *ty;
		}

		if !set.contains(&key.0) {
			match ty {
				| Type::Void
				| Type::Char
				| Type::Bool
				| Type::Int8
				| Type::Int16
				| Type::Int32
				| Type::Int64
				| Type::UInt8
				| Type::UInt16
				| Type::UInt32
				| Type::UInt64
				| Type::Float16
				| Type::Float32
				| Type::Float64 => {}
				_ => panic!("Type `{ty}` does not belong to this TypeHeap"),
			}
		}

		let ty = self.alloc(Type::Pointer { ty, mutable });
		set.insert(as_key(ty));
		pointers.insert(key, ty);
		ty
	}

	fn alloc<T: 'l + Send>(&self, value: T) -> &'l T {
		let value = self.bump.alloc(value);
		if let Some(helper) = DropHelper::new(value as _) {
			self.drops.borrow_mut().push(helper);
		}
		value
	}
}

struct DropHelper {
	ptr: *mut u8,
	drop_fn: fn(*mut u8),
}

impl DropHelper {
	fn new<T>(ptr: *mut T) -> Option<Self> {
		unsafe {
			match std::mem::needs_drop::<T>() {
				false => None,
				true => Some(Self {
					ptr: ptr as *mut u8,
					drop_fn: |ptr| std::ptr::drop_in_place(ptr as *mut T),
				}),
			}
		}
	}
}

impl Drop for DropHelper {
	fn drop(&mut self) {
		(self.drop_fn)(self.ptr);
	}
}

fn as_key(ty: &Type) -> usize {
	ty as *const _ as usize
}
