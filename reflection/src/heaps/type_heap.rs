use std::collections::{HashMap, HashSet};
use crate::{Pointer, Struct, Type};
use std::cell::RefCell;
use std::sync::Arc;
use bumpalo::Bump;

pub struct TypeHeap<'l> {
	bump: &'l Bump,
	set: RefCell<HashSet<usize>>,
	primitives: [&'l Type<'l>; 14],
	pointers: RefCell<HashMap<(usize, bool), &'l Type<'l>>>,
	structs: RefCell<HashMap<usize, (&'l Type<'l>, Arc<Struct<'l>>)>>,
	drops: RefCell<Vec<DropHelper>>,
}

#[rustfmt::skip]
impl<'l> TypeHeap<'l> {
    pub fn new(bump: &'l Bump) -> Self {
        let primitives = [
            &*bump.alloc(Type::Void),
            &*bump.alloc(Type::Char),
            &*bump.alloc(Type::Bool),
            &*bump.alloc(Type::Int8),
            &*bump.alloc(Type::Int16),
            &*bump.alloc(Type::Int32),
            &*bump.alloc(Type::Int64),
            &*bump.alloc(Type::UInt8),
            &*bump.alloc(Type::UInt16),
            &*bump.alloc(Type::UInt32),
            &*bump.alloc(Type::UInt64),
            &*bump.alloc(Type::Float16),
            &*bump.alloc(Type::Float32),
            &*bump.alloc(Type::Float64),
        ];
        Self {
            structs: Default::default(),
            pointers: Default::default(),
            drops: Default::default(),

            set: primitives
                .iter()
                .cloned()
                .map(as_key)
                .collect::<HashSet<_>>()
                .into(),

            primitives: unsafe { std::mem::transmute(primitives) },
            bump,
        }
    }

    pub fn void(&self) -> &'l Type<'l> { self.primitives[0] }
    pub fn char(&self) -> &'l Type<'l> { self.primitives[1] }
    pub fn bool(&self) -> &'l Type<'l> { self.primitives[2] }
    pub fn int8(&self) -> &'l Type<'l> { self.primitives[3] }
    pub fn int16(&self) -> &'l Type<'l> { self.primitives[4] }
    pub fn int32(&self) -> &'l Type<'l> { self.primitives[5] }
    pub fn int64(&self) -> &'l Type<'l> { self.primitives[6] }
    pub fn uint8(&self) -> &'l Type<'l> { self.primitives[7] }
    pub fn uint16(&self) -> &'l Type<'l> { self.primitives[8] }
    pub fn uint32(&self) -> &'l Type<'l> { self.primitives[9] }
    pub fn uint64(&self) -> &'l Type<'l> { self.primitives[10] }
    pub fn float16(&self) -> &'l Type<'l> { self.primitives[11] }
    pub fn float32(&self) -> &'l Type<'l> { self.primitives[12] }
    pub fn float64(&self) -> &'l Type<'l> { self.primitives[13] }
}

#[rustfmt::skip]
impl<'l> TypeHeap<'l> {
    pub fn struct_ref(&self, ty: &Arc<Struct<'l>>) -> &'l Type<'l> {
        let mut structs = self.structs.borrow_mut();
        let mut set = self.set.borrow_mut();

        let key = Arc::as_ptr(ty) as usize;
        if let Some((ty, _)) = structs.get(&key) {
            return *ty;
        }

        for field in ty.fields() {
            assert!(set.contains(&as_key(field.ty())), "One or more field types don't belong to this TypeHeap");
        }

        let ty_ref = self.alloc(Type::Struct(ty.clone()));
        set.insert(as_key(ty_ref));
        structs.insert(key, (ty_ref, ty.clone()));
        ty_ref
    }

    pub fn pointer(&self, ty: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
        let mut pointers = self.pointers.borrow_mut();
        let mut set = self.set.borrow_mut();

        let key = (as_key(ty), mutable);
        if let Some(ty) = pointers.get(&key) {
            return *ty;
        }

        assert!(set.contains(&key.0), "Type does not belong to this TypeHeap");
        let ty = self.alloc(Type::Pointer(Pointer::new(ty, mutable)));
        set.insert(as_key(ty));
        pointers.insert(key, ty);
        ty
    }

    fn alloc<T: 'l>(&self, value: T) -> &'l T {
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
