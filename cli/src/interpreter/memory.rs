use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::Type;
use std::ops::{Index, IndexMut, Range};
use std::fmt::{Debug, Formatter};
use std::collections::HashMap;
use bytemuck::bytes_of;
use std::alloc::Layout;
use std::cell::RefCell;
use anyhow::{anyhow};
use std::fmt::Write;
use std::sync::Arc;
use std::rc::Rc;
use half::f16;

#[derive(Default)]
pub struct TypeLayoutCache {
	layouts: RefCell<HashMap<Arc<Type>, Layout>>,
	field_layouts: RefCell<HashMap<(Arc<Type>, usize), (usize, Layout, Arc<Type>)>>,
}

impl TypeLayoutCache {
	pub fn get_layout(&self, ty: &Arc<Type>) -> Layout {
		match ty.as_ref().as_ref() {
			TypeVariant::Void => Layout::new::<()>(),
			TypeVariant::Char => Layout::new::<char>(),
			TypeVariant::Bool => Layout::new::<bool>(),
			TypeVariant::Pointer(_, _) => Layout::new::<*const u8>(),
			TypeVariant::Reference(_, _) => Layout::new::<*const u8>(),
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
			}
		}
	}

	pub fn get_field_offset_and_layout(&self, ty: &Arc<Type>, field: usize) -> Option<(usize, Layout, Arc<Type>)> {
		let TypeVariant::Struct(data) = ty.as_ref().as_ref() else {
			return None;
		};

		let fields = data.fields();
		if fields.len() == 0 {
			return Some((0, Layout::new::<()>(), Type::void().clone()));
		}

		let key = (ty.clone(), field);
		if let Some((offset, layout, ty)) = self.field_layouts.borrow().get(&key).cloned() {
			return Some((offset, layout, ty));
		}

		let mut offset = 0;
		let mut layout = Layout::new::<()>();
		let mut fld_layout = Layout::new::<()>();
		let mut fld_ty = Type::void().clone();

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

pub struct Stack {
	sp: usize,
	memory: Vec<u8>,
	layout_cache: Rc<TypeLayoutCache>,
	elements: Vec<(Arc<Type>, usize, usize)>
}

impl Stack {
	pub fn with_capacity(layout_cache: Rc<TypeLayoutCache>, capacity: usize) -> Self {
		Self {
			sp: 0,
			layout_cache,
			elements: vec![],
			memory: vec![0; capacity],
		}
	}

	pub fn peek(&self) -> Option<(&Arc<Type>, usize)> {
		let (ty, _, size) = self.elements.last()?;
		Some((ty, *size))
	}

	pub fn push(&mut self, ty: &Arc<Type>, bytes: Option<&[u8]>) -> anyhow::Result<Range<usize>> {
		let layout = self.layout_cache.get_layout(ty);

		if let Some(bytes) = bytes {
			if bytes.len() != layout.size() {
				return err_invalid_len(layout.size(), bytes.len());
			}
		}

		let mut sp = self.sp;
		let mut len = layout.size();
		let align = unsafe { self.memory.as_ptr().add(sp).align_offset(layout.align()) };
		sp += align;
		len += align;

		let range = sp..sp + layout.size();
		sp += layout.size();

		if sp > self.memory.len() {
			return err_stack_overflow();
		}

		self.sp = sp;
		self.elements.push((ty.clone(), len, layout.size()));

		if let Some(bytes) = bytes {
			self[range.clone()].copy_from_slice(bytes);
		}

		Ok(range)
	}

	pub fn push_inner(&mut self, ty: &Arc<Type>, src: Range<usize>) -> anyhow::Result<Range<usize>> {
		if src.start > src.end || src.end > self.sp {
			return err_out_of_bounds();
		}

		let layout = self.layout_cache.get_layout(ty);

		if src.len() != layout.size() {
			return err_invalid_len(layout.size(), src.len());
		}

		let mut sp = self.sp;
		let mut len = layout.size();
		let align = unsafe { self.memory.as_ptr().add(sp).align_offset(layout.align()) };
		sp += align;
		len += align;

		let range = sp..sp + layout.size();
		sp += layout.size();

		if sp > self.memory.len() {
			return err_stack_overflow();
		}

		self.memory.copy_within(src, self.sp);

		self.sp = sp;
		self.elements.push((ty.clone(), len, layout.size()));

		Ok(range)
	}

	pub fn pop(&mut self, bytes: &mut [u8]) -> anyhow::Result<Arc<Type>> {
		let Some((elem_ty, len, size)) = self.elements.last() else {
			return err_stack_empty();
		};

		if bytes.len() != *size {
			return err_invalid_len(*size, bytes.len());
		}

		let elem_ty = elem_ty.clone();
		self.sp -= *len;
		bytes.copy_from_slice(&self[self.sp..self.sp+size]);
		let _ = self.elements.pop();
		Ok(elem_ty)
	}

	pub fn pop_inner(&mut self, ty: &Arc<Type>, dst: Range<usize>) -> anyhow::Result<()> {
		if dst.start > dst.end || dst.end > self.sp {
			return err_out_of_bounds();
		}

		let Some((elem_ty, len, size)) = self.elements.last() else {
			return err_stack_empty();
		};

		if dst.len() != *size {
			return err_invalid_len(*size, dst.len());
		}

		if elem_ty != ty {
			return err_invalid_ty(ty, elem_ty);
		}

		self.sp -= *len;
		self.memory.copy_within(self.sp..self.sp + *len, dst.start);
		let _ = self.elements.pop();

		Ok(())
	}

	pub fn pop_ignore_value(&mut self) -> anyhow::Result<()> {
		let Some((_, len, _)) = self.elements.last() else {
			return err_stack_empty();
		};

		self.sp -= *len;
		let _ = self.elements.pop();
		Ok(())
	}

	pub fn as_bytes(&self) -> &[u8] {
		&self.memory[..self.sp]
	}
}

impl Index<Range<usize>> for Stack {
	type Output = [u8];
	fn index(&self, index: Range<usize>) -> &Self::Output {
		&self.memory[index]
	}
}

impl IndexMut<Range<usize>> for Stack {
	fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
		&mut self.memory[index]
	}
}

impl Debug for Stack {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let mut dbg = f.debug_struct("Stack");

		let mut sp = 0;
		let mut name = String::new();
		let mut value = String::new();
		let mut type_name = String::new();
		for (ty, len, size) in &self.elements {
			name.clear();
			type_name.clear();
			write!(type_name, "{}", ty).unwrap();
			write!(name, "{:#?}", unsafe { self.memory.as_ptr().add(sp) }).unwrap();

			value.clear();
			match ty.as_ref().as_ref() {
				TypeVariant::Int8 => write!(value, "{}", bytemuck::pod_read_unaligned::<i8>(&self[sp..sp+size]))?,
				TypeVariant::Int16 => write!(value, "{}", bytemuck::pod_read_unaligned::<i16>(&self[sp..sp+size]))?,
				TypeVariant::Int32 => write!(value, "{}", bytemuck::pod_read_unaligned::<i32>(&self[sp..sp+size]))?,
				TypeVariant::Int64 => write!(value, "{}", bytemuck::pod_read_unaligned::<i64>(&self[sp..sp+size]))?,
				TypeVariant::UInt8 => write!(value, "{}", bytemuck::pod_read_unaligned::<u8>(&self[sp..sp+size]))?,
				TypeVariant::UInt16 => write!(value, "{}", bytemuck::pod_read_unaligned::<u16>(&self[sp..sp+size]))?,
				TypeVariant::UInt32 => write!(value, "{}", bytemuck::pod_read_unaligned::<u32>(&self[sp..sp+size]))?,
				TypeVariant::UInt64 => write!(value, "{}", bytemuck::pod_read_unaligned::<u64>(&self[sp..sp+size]))?,
				TypeVariant::Float32 => write!(value, "{}", bytemuck::pod_read_unaligned::<f32>(&self[sp..sp+size]))?,
				TypeVariant::Float64 => write!(value, "{}", bytemuck::pod_read_unaligned::<f64>(&self[sp..sp+size]))?,
				_ => {},
			};

			match value.is_empty() {
				true => dbg.field(&name, &format_args!("{}", type_name)),
				false => dbg.field(&name, &format_args!("{} {}", type_name, value)),
			};
			sp += *len;
		}

		name.clear();
		write!(name, "{:#?}", unsafe { self.memory.as_ptr().add(sp) }).unwrap();
		dbg.field(&name, &"--- END OF STACK ---");

		dbg.finish()
	}
}

pub trait PushValue<T> {
	fn push_value(&mut self, value: T) -> anyhow::Result<Range<usize>>;

	fn push_value_discard(&mut self, value: T) -> anyhow::Result<()> {
		self.push_value(value)?;
		Ok(())
	}
}

macro_rules! impl_push {
    ($($ty: ident),+) => {
		$(
			impl PushValue<$ty> for Stack {
				fn push_value(&mut self, value: $ty) -> anyhow::Result<Range<usize>> {
					self.push(Type::$ty(), Some(bytes_of(&value)))
				}
			}
		)+
	};
}

impl_push!(i8, i16, i32, i64, u8, u16, u32, u64, f32, f64);

impl PushValue<bool> for Stack {
	fn push_value(&mut self, value: bool) -> anyhow::Result<Range<usize>> {
		let value = value as u8;
		self.push(Type::bool(), Some(bytes_of(&value)))
	}
}

#[cold]
#[inline(never)]
fn err_stack_overflow<T>() -> anyhow::Result<T> {
	Err(anyhow!("Stack overflow"))
}

#[cold]
#[inline(never)]
fn err_stack_empty<T>() -> anyhow::Result<T> {
	Err(anyhow!("Stack is empty"))
}

#[cold]
#[inline(never)]
fn err_out_of_bounds<T>() -> anyhow::Result<T> {
	Err(anyhow!("Out of bounds stack access"))
}

#[cold]
#[inline(never)]
fn err_invalid_len<T>(expected: usize, found: usize) -> anyhow::Result<T> {
	Err(anyhow!("Invalid buffer length. Expected {}, found {}", expected, found))
}

#[cold]
#[inline(never)]
fn err_invalid_ty<T>(expected: &Arc<Type>, found: &Arc<Type>) -> anyhow::Result<T> {
	Err(anyhow!("Invalid type. Expected {}, found {}", expected, found))
}
