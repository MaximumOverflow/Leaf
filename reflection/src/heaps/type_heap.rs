use std::cell::UnsafeCell;
use std::collections::{HashMap, HashSet};

use parking_lot::Mutex;

use crate::{Struct, Type};
use crate::heaps::general_purpose_heap::ArenaAllocator;

pub struct TypeHeap<'l> {
	bump: &'l ArenaAllocator,
	set: Mutex<HashSet<usize>>,
	arrays: Mutex<HashMap<(usize, usize), &'l Type<'l>>>,
	pointers: Mutex<HashMap<(usize, bool), &'l Type<'l>>>,
	references: Mutex<HashMap<(usize, bool), &'l Type<'l>>>,
	structs: Mutex<HashMap<usize, (&'l Type<'l>, &'l Struct<'l>)>>,
	function_types: Mutex<HashMap<(&'l Type<'l>, &'l [&'l Type<'l>]), &'l Type<'l>>>,
}

#[rustfmt::skip]
impl<'l> TypeHeap<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			bump,
			set: Default::default(),
			arrays: Default::default(),
			structs: Default::default(),
			pointers: Default::default(),
			references: Default::default(),
			function_types: Default::default(),
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
		let mut structs = self.structs.lock();
		let mut set = self.set.lock();

		let key = ty as *const _ as usize;
		if let Some((ty, _)) = structs.get(&key) {
			return ty;
		}

		for field in ty.fields() {
			Self::assert_type_in_heap(field.ty(), &set);
		}

		let ty_ref = self.bump.alloc(Type::Struct(ty));
		set.insert(as_key(ty_ref));
		structs.insert(key, (ty_ref, ty));
		ty_ref
	}

	pub fn pointer(&self, ty: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut pointers = self.pointers.lock();
		let mut set = self.set.lock();

		let key = (as_key(ty), mutable);
		if let Some(ty) = pointers.get(&key) {
			return ty;
		}

		Self::assert_type_in_heap(ty, &set);

		let ty = self.bump.alloc(Type::Pointer { ty, mutable });
		set.insert(as_key(ty));
		pointers.insert(key, ty);
		ty
	}

	pub fn reference(&self, ty: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut references = self.references.lock();
		let mut set = self.set.lock();

		let key = (as_key(ty), mutable);
		if let Some(ty) = references.get(&key) {
			return ty;
		}

		Self::assert_type_in_heap(ty, &set);

		let ty = self.bump.alloc(Type::Reference { ty, mutable });
		set.insert(as_key(ty));
		references.insert(key, ty);
		ty
	}

	pub fn array(&self, elem_ty: &'l Type<'l>, count: usize) -> &'l Type<'l> {
		let key = (as_key(elem_ty), count);
		let mut arrays = self.arrays.lock();
		if let Some(ty) = arrays.get(&key) {
			return ty;
		}

		let mut set = self.set.lock();
		Self::assert_type_in_heap(elem_ty, &set);
		let ty = self.bump.alloc(Type::Array { count, ty: elem_ty });
		set.insert(as_key(ty));
		arrays.insert(key, ty);
		ty
	}

	pub fn function_ptr(
		&self,
		ret_ty: &'l Type<'l>,
		param_tys: &[&'l Type<'l>],
	) -> &'l Type<'l> {
		let mut functions = self.function_types.lock();
		if let Some(ty) = functions.get(&(ret_ty, param_tys)) {
			return ty;
		}

		let mut set = self.set.lock();
		Self::assert_type_in_heap(ret_ty, &set);
		for param in param_tys {
			Self::assert_type_in_heap(param, &set);
		}

		let param_tys = self.bump.alloc_slice_copy(param_tys);
		let ty = self.bump.alloc(Type::FunctionPointer { ret_ty, param_tys });
		set.insert(as_key(ty));
		functions.insert((ret_ty, param_tys), ty);
		ty
	}

	fn assert_type_in_heap(ty: &Type, set: &HashSet<usize>) {
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
			_ => {
				assert!(set.contains(&as_key(ty)), "Type `{ty}` does not belong to this TypeHeap")
			}
		}
	}
}

#[inline(always)]
fn as_key(ty: &Type) -> usize {
	ty as *const _ as usize
}
