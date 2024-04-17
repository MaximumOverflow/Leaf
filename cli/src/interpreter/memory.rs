use std::alloc::Layout;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::repeat;
use std::sync::Arc;

use half::f16;
use nohash_hasher::BuildNoHashHasher;

use leaf_compilation::reflection::{Function, Type};

#[derive(Default)]
pub struct LayoutCache<'l> {
	type_layouts: RefCell<HashMap<usize, Layout, BuildNoHashHasher<usize>>>,
	field_layouts: RefCell<HashMap<(usize, usize), (usize, Layout, &'l Type<'l>)>>,
	function_layouts:
		RefCell<HashMap<usize, (Layout, Arc<[[usize; 2]]>), BuildNoHashHasher<usize>>>,
}

#[allow(unused)]
impl<'l> LayoutCache<'l> {
	pub fn get_function_stack_layout(&self, func: &'l Function<'l>) -> (Layout, Arc<[[usize; 2]]>) {
		let key = func as *const _ as usize;
		let mut function_layouts = self.function_layouts.borrow_mut();

		if let Some(data) = function_layouts.get(&key) {
			return data.clone();
		};

		let body = func.body().unwrap();

		let mut layout = Layout::new::<()>();
		let mut offsets = Arc::from_iter(repeat([0; 2]).take(body.values().len()));
		let offsets_slice = Arc::get_mut(&mut offsets).unwrap();

		for (i, value) in body.values().iter().enumerate() {
			let ty_layout = self.get_type_layout(value.ty);
			if value.const_data.is_some() {
				offsets_slice[i] = [0, ty_layout.size()];
			} else {
				let (new_layout, offset) = layout.extend(ty_layout).unwrap();
				layout = new_layout;
				offsets_slice[i] = [offset, ty_layout.size()];
			}
		}

		let result = (layout, offsets);
		function_layouts.insert(key, result.clone());
		result
	}

	pub fn get_type_layout(&self, ty: &'l Type<'l>) -> Layout {
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
				let layout = self.get_type_layout(data.ty());
				Layout::from_size_align(layout.size() * data.count(), layout.align()).unwrap()
			},
			Type::Struct(data) => {
				let key = ty as *const _ as usize;
				if let Some(layout) = self.type_layouts.borrow().get(&key).cloned() {
					return layout;
				}

				let mut layout = Layout::new::<()>();
				for field in data.fields() {
					let ty = field.ty();
					let ty = self.get_type_layout(&ty);
					layout = layout.extend(ty).unwrap().0;
				}
				let mut layouts = self.type_layouts.borrow_mut();
				layouts.insert(key, layout);
				layout
			},
		}
	}

	pub fn get_field_offset_and_layout(
		&self,
		ty: &'l Type<'l>,
		field: usize,
	) -> Option<(usize, Layout, &'l Type<'l>)> {
		let Type::Struct(data) = ty else {
			return None;
		};

		let fields = data.fields();

		if fields.len() == 0 {
			return None;
		}

		let key = (ty as *const _ as usize, field);
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
			fld_layout = self.get_type_layout(&fld_ty);
			(layout, offset) = layout.extend(fld_layout).unwrap();
		}

		let mut offsets = self.field_layouts.borrow_mut();

		let value = (offset, fld_layout, fld_ty);
		offsets.insert(key, value.clone());
		Some(value)
	}
}
