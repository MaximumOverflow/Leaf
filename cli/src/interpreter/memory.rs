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
use leaf_compilation::reflection::Type;

#[derive(Default)]
pub struct TypeLayoutCache<'l> {
	layouts: RefCell<HashMap<&'l Type<'l>, Layout, BuildNoHashHasher<usize>>>,
	field_layouts: RefCell<HashMap<(&'l Type<'l>, usize), (usize, Layout, &'l Type<'l>)>>,
}

impl<'l> TypeLayoutCache<'l> {
	pub fn get_layout(&self, ty: &'l Type<'l>) -> Layout {
		match ty {
			Type::Void => Layout::new::<()>(),
			Type::Char => Layout::new::<char>(),
			Type::Bool => Layout::new::<bool>(),
			Type::Int8 => Layout::new::<i8>(),
			Type::Int16 => Layout::new::<i16>(),
			Type::Int32 => Layout::new::<i32>(),
			Type::Int64 => Layout::new::<i64>(),
			Type::UInt8 => Layout::new::<u8>(),
			Type::UInt16 => Layout::new::<u16>(),
			Type::UInt32 => Layout::new::<u32>(),
			Type::UInt64 => Layout::new::<u64>(),
			Type::Float16 => Layout::new::<f16>(),
			Type::Float32 => Layout::new::<f32>(),
			Type::Float64 => Layout::new::<f64>(),
			Type::Pointer(_) => Layout::new::<*const u8>(),
			Type::Array(data) => {
				let layout = self.get_layout(data.ty());
				Layout::from_size_align(layout.size() * data.count(), layout.align()).unwrap()
			}
			Type::Struct(data) => {
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
				layouts.insert(ty, layout);
				layout
			}
		}
	}

	pub fn get_field_offset_and_layout(
		&self, ty: &'l Type<'l>, field: usize,
	) -> Option<(usize, Layout, &'l Type<'l>)> {
		let Type::Struct(data) = ty else {
			return None;
		};

		let fields = data.fields();

		if fields.len() == 0 {
			return None;
		}

		let key = (ty, field);
		if let Some((offset, layout, ty)) = self.field_layouts.borrow().get(&key).cloned() {
			return Some((offset, layout, ty));
		}

		let mut offset = 0;
		let mut layout = Layout::new::<()>();
		let mut fld_layout = Layout::new::<()>();
		let mut fld_ty = &Type::Void;

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
