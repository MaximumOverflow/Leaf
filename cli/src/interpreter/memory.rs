use leaf_compilation::reflection::structured::types::{LeafType, TypeVariant};
use leaf_compilation::reflection::structured::Type;
use nohash_hasher::BuildNoHashHasher;
use std::mem::{align_of, size_of};
use std::fmt::{Debug, Formatter};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::alloc::Layout;
use std::fmt::Write;
use std::sync::Arc;
use std::rc::Rc;
use half::f16;

#[derive(Default)]
pub struct TypeLayoutCache {
	layouts: RefCell<HashMap<Arc<Type>, Layout, BuildNoHashHasher<usize>>>,
	field_layouts: RefCell<HashMap<(Arc<Type>, usize), (usize, Layout, Arc<Type>)>>,
}

impl TypeLayoutCache {
	pub fn get_layout(&self, ty: &Arc<Type>) -> Layout {
		match ty.variant() {
			TypeVariant::Void => Layout::new::<()>(),
			TypeVariant::Char => Layout::new::<char>(),
			TypeVariant::Bool => Layout::new::<bool>(),
			TypeVariant::Int8 => Layout::new::<i8>(),
			TypeVariant::Int16 => Layout::new::<i16>(),
			TypeVariant::Int32 => Layout::new::<i32>(),
			TypeVariant::Int64 => Layout::new::<i64>(),
			TypeVariant::UInt8 => Layout::new::<u8>(),
			TypeVariant::UInt16 => Layout::new::<u16>(),
			TypeVariant::UInt32 => Layout::new::<u32>(),
			TypeVariant::UInt64 => Layout::new::<u64>(),
			TypeVariant::Float16 => Layout::new::<f16>(),
			TypeVariant::Float32 => Layout::new::<f32>(),
			TypeVariant::Float64 => Layout::new::<f64>(),
			TypeVariant::Pointer(_, _) => Layout::new::<*const u8>(),
			TypeVariant::Reference(_, _) => Layout::new::<*const u8>(),
			TypeVariant::Struct(data) => {
				if let Some(layout) = self.layouts.borrow().get(ty).cloned() {
					return layout;
				}

				let mut layout = Layout::new::<()>();
				for field in data.fields() {
					let ty = field.ty();
					let ty = self.get_layout(&ty);
					layout = layout.extend(ty).unwrap().0;
				}
				let mut layouts = self.layouts.borrow_mut();
				layouts.insert(ty.clone(), layout);
				layout
			},
		}
	}

	pub fn get_field_offset_and_layout(
		&self, ty: &Arc<Type>, field: usize,
	) -> Option<(usize, Layout, Arc<Type>)> {
		let TypeVariant::Struct(data) = ty.variant() else {
			return None;
		};

		let fields = data.fields();
		if fields.len() == 0 {
			return Some((0, Layout::new::<()>(), <()>::leaf_type().clone()));
		}

		let key = (ty.clone(), field);
		if let Some((offset, layout, ty)) = self.field_layouts.borrow().get(&key).cloned() {
			return Some((offset, layout, ty));
		}

		let mut offset = 0;
		let mut layout = Layout::new::<()>();
		let mut fld_layout = Layout::new::<()>();
		let mut fld_ty = <()>::leaf_type().clone();

		for field in 0..=field {
			let Some(field) = fields.get(field) else {
				return None;
			};

			fld_ty = field.ty();
			fld_layout = self.get_layout(&fld_ty);
			(layout, offset) = layout.extend(fld_layout).unwrap();
		}

		let mut offsets = self.field_layouts.borrow_mut();

		let value = (offset, fld_layout, fld_ty);
		offsets.insert(key, value.clone());
		Some(value)
	}
}

pub struct Stack<'l> {
	sp: Cell<*mut u8>,
	ep: *mut u8,
	memory: Vec<u8>,
	layout_cache: Rc<TypeLayoutCache>,
	elements: RefCell<Vec<StackElement<'l>>>,
}

#[derive(Clone)]
pub struct StackElement<'l>(pub &'l Arc<Type>, pub Layout, pub *mut u8, *mut u8);

impl StackElement<'_> {
	pub fn as_ref<T: LeafType>(&self) -> &T {
		assert_eq!(self.0, T::leaf_type());
		debug_assert_eq!(self.1, Layout::new::<T>());
		unsafe { &*(self.2 as *const T) }
	}
}

impl<'l> Stack<'l> {
	pub fn with_capacity(layout_cache: Rc<TypeLayoutCache>, capacity: usize) -> Self {
		let mut memory = vec![0; capacity];
		let [start, .., end] = memory.as_mut_slice() else {
			unreachable!();
		};

		Self {
			sp: Cell::new(start as *mut u8),
			ep: end as *mut u8,
			memory,
			layout_cache,
			elements: Default::default(),
		}
	}

	pub fn with_elements<T>(&self, func: impl FnOnce(&[StackElement<'l>]) -> T) -> T {
		let elements = self.elements.borrow();
		func(elements.as_slice())
	}

	pub fn push(&mut self, ty: &'l Arc<Type>, bytes: &[u8]) -> (usize, *mut u8) {
		unsafe {
			let layout = self.layout_cache.get_layout(ty);
			let sp = self.sp.get();
			let align = sp.align_offset(layout.align());
			let new_sp = sp.add(layout.size() + align);
			assert!(new_sp <= self.ep, "Stack overflow");
			assert_eq!(layout.size(), bytes.len());

			let elem_ptr = sp.add(align);
			std::ptr::copy_nonoverlapping(bytes.as_ptr(), elem_ptr, bytes.len());

			let mut elements = self.elements.borrow_mut();
			let id = elements.len();

			elements.push(StackElement(ty, layout, elem_ptr, sp));
			self.sp.set(new_sp);
			(id, elem_ptr)
		}
	}

	pub fn push_uninit(&mut self, ty: &'l Arc<Type>) -> (usize, *mut u8) {
		unsafe {
			let sp = self.sp.get();
			let layout = self.layout_cache.get_layout(ty);
			let align = sp.align_offset(layout.align());
			let new_sp = sp.add(layout.size() + align);
			assert!(new_sp <= self.ep, "Stack overflow");

			let elem_ptr = sp.add(align);
			let mut elements = self.elements.borrow_mut();
			let id = elements.len();

			elements.push(StackElement(ty, layout, elem_ptr, sp));
			self.sp.set(new_sp);
			(id, elem_ptr)
		}
	}

	pub fn push_value<T: Copy + LeafType>(&mut self, value: T) -> (usize, *mut u8) {
		debug_assert!({
			let ty = T::leaf_type();
			let layout = self.layout_cache.get_layout(&ty);
			layout.size() == size_of::<T>() && layout.align() == align_of::<T>()
		});

		unsafe {
			let sp = self.sp.get();
			let align = sp.align_offset(align_of::<T>());
			let new_sp = sp.add(size_of::<T>() + align);
			assert!(new_sp <= self.ep, "Stack overflow");

			let elem_ptr = sp.add(align);
			std::ptr::write(elem_ptr as *mut T, value);

			let mut elements = self.elements.borrow_mut();
			let id = elements.len();

			elements.push(StackElement(
				T::leaf_type(),
				Layout::new::<T>(),
				elem_ptr,
				sp,
			));
			self.sp.set(new_sp);
			(id, elem_ptr)
		}
	}

	pub fn push_from_element(&mut self, element: usize) -> (usize, *mut u8) {
		let mut elements = self.elements.borrow_mut();
		let StackElement(ty, layout, ptr, _) = elements[element].clone();
		let id = elements.len();

		unsafe {
			let sp = self.sp.get();
			let align = sp.align_offset(layout.align());
			let new_sp = sp.add(layout.size() + align);
			assert!(new_sp <= self.ep, "Stack overflow");

			let elem_ptr = sp.add(align);
			std::ptr::copy_nonoverlapping(ptr, elem_ptr, layout.size());
			elements.push(StackElement(ty, layout, elem_ptr, sp));
			self.sp.set(new_sp);
			(id, elem_ptr)
		}
	}

	pub fn pop(&self) -> (&'l Arc<Type>, Layout, &[u8]) {
		let mut elements = self.elements.borrow_mut();
		let StackElement(ty, layout, ptr, sp) = elements.pop().unwrap();

		self.sp.set(sp);
		let slice = unsafe { std::slice::from_raw_parts(ptr, layout.size()) };
		(ty, layout, slice)
	}

	pub fn pop_ref<T: LeafType>(&self) -> &T {
		let mut elements = self.elements.borrow_mut();
		let StackElement(ty, layout, ptr, sp) = elements.pop().unwrap();
		assert_eq!(ty, T::leaf_type());
		debug_assert_eq!(layout, Layout::new::<T>());
		self.sp.set(sp);
		unsafe { &*(ptr as *const T) }
	}

	pub fn pop_into_element(&self, element: usize) {
		let mut elements = self.elements.borrow_mut();
		let StackElement(ty, layout, ptr, sp) = elements.pop().unwrap();
		let StackElement(elem_ty, elem_layout, elem_ptr, _) = &elements[element];

		assert_eq!(ty, *elem_ty);
		debug_assert_eq!(layout, *elem_layout);

		self.sp.set(sp);
		unsafe { std::ptr::copy_nonoverlapping(ptr, *elem_ptr, layout.size()) }
	}

	pub unsafe fn pop_into(&self, dst: *mut u8) {
		let mut elements = self.elements.borrow_mut();
		let StackElement(_, layout, src, sp) = elements.pop().unwrap();

		self.sp.set(sp);
		std::ptr::copy_nonoverlapping(src, dst, layout.size())
	}

	pub fn as_ptr(&self) -> *const u8 {
		self.memory.as_ptr()
	}
}

impl Debug for Stack<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut dbg = f.debug_struct(&format! {
			"Stack [{} / {}]", self.sp.get() as usize - self.memory.as_ptr() as usize, self.memory.len()
		});

		unsafe fn write_value(
			stack: &Stack, string: &mut String, ptr: *const u8, ty: &Arc<Type>,
		) -> std::fmt::Result {
			match ty.variant() {
				TypeVariant::Int8 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const i8))
				},
				TypeVariant::Int16 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const i16))
				},
				TypeVariant::Int32 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const i32))
				},
				TypeVariant::Int64 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const i64))
				},
				TypeVariant::UInt8 => write!(string, "{}", std::ptr::read_unaligned(ptr)),
				TypeVariant::UInt16 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const u16))
				},
				TypeVariant::UInt32 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const u32))
				},
				TypeVariant::UInt64 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const u32))
				},
				TypeVariant::Float16 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const f16))
				},
				TypeVariant::Float32 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const f32))
				},
				TypeVariant::Float64 => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const f64))
				},
				TypeVariant::Bool => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const bool))
				},
				TypeVariant::Char => {
					write!(string, "{}", std::ptr::read_unaligned(ptr as *const char))
				},

				| TypeVariant::Pointer(_, _) | TypeVariant::Reference(_, _) => write!(
					string,
					"{:#?}",
					std::ptr::read_unaligned(ptr as *const *const u8)
				),

				TypeVariant::Struct(data) => {
					write!(string, "{{ ")?;
					let mut comma = "";
					for (i, field) in data.fields().iter().enumerate() {
						let (offset, _, ty) =
							stack.layout_cache.get_field_offset_and_layout(ty, i).unwrap();

						write!(string, "{}{}: ", comma, field.name())?;
						write_value(stack, string, ptr.add(offset), &ty)?;
						comma = ", ";
					}
					write!(string, " }}")
				},

				_ => Ok(()),
			}
		}

		let mut name = String::new();
		let mut value = String::new();
		let mut type_name = String::new();

		let elements = self.elements.borrow();
		for StackElement(ty, _, ptr, _) in elements.iter() {
			name.clear();
			type_name.clear();
			write!(type_name, "{}", ty).unwrap();
			write!(name, "{:#?}", ptr).unwrap();

			value.clear();
			unsafe { write_value(self, &mut value, *ptr, ty)? };

			match value.is_empty() {
				true => dbg.field(&name, &format_args!("{}", type_name)),
				false => dbg.field(&name, &format_args!("{} {}", type_name, value)),
			};
		}

		name.clear();
		write!(name, "{:#?}", self.ep).unwrap();
		dbg.field(&name, &"--- END OF STACK ---");

		dbg.finish()
	}
}
