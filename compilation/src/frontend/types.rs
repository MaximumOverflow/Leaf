use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;
use fxhash::FxHashMap;
use tracing::trace;

use leaf_parsing::ast::{Expression, Ident, Integer, Literal, Node, Type as TypeNode};
use leaf_reflection::heaps::{ArenaAllocator, BlobHeapScope};
use leaf_reflection::serialization::{MetadataWrite, WriteRequirements};
use leaf_reflection::Type;
use crate::frontend::reports::{FrontEndError, ReportData, TYPE_NOT_FOUND};

pub struct TypeCache<'l> {
	bump: &'l ArenaAllocator,
	array_types: RefCell<HashMap<(&'l Type<'l>, usize), &'l Type<'l>>>,
	pointer_types: RefCell<HashMap<(&'l Type<'l>, bool), &'l Type<'l>>>,
}

impl<'l> TypeCache<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			bump,
			array_types: RefCell::default(),
			pointer_types: RefCell::default(),
		}
	}

	pub fn make_pointer(&self, base: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut pointers = self.pointer_types.borrow_mut();
		pointers
			.entry((base, mutable))
			.or_insert_with(|| self.bump.alloc(Type::Pointer { ty: base, mutable }))
	}
}

pub trait TypeResolver<'l> {
	fn type_cache(&self) -> &TypeCache<'l>;
	fn blob_heap(&self) -> &Arc<BlobHeapScope<'l>>;
	fn types(&self) -> &FxHashMap<&'l str, &'l Type<'l>>;

	#[tracing::instrument(skip_all)]
	fn resolve_type(
		&self,
		ast: &TypeNode,
		reports: &mut ReportData,
	) -> Result<&'l Type<'l>, FrontEndError> {
		let ty: &Type = match ast {
			TypeNode::Id(Ident { value: id, .. }) => {
				trace!("Resolving type {id:?}");
				match *id {
					"void" => &Type::Void,
					"char" => &Type::Char,
					"bool" => &Type::Bool,
					"i8" => &Type::Int8,
					"i16" => &Type::Int16,
					"i32" => &Type::Int32,
					"i64" => &Type::Int64,
					"u8" => &Type::UInt8,
					"u16" => &Type::UInt16,
					"u32" => &Type::UInt32,
					"u64" => &Type::UInt64,
					"f16" => &Type::Float16,
					"f32" => &Type::Float32,
					"f64" => &Type::Float64,
					_ => match self.types().get(id) {
						Some(ty) => *ty,
						None => {
							reports.add_error_label(
								ast.range(),
								format!("Type `{id}` is not available in the current scope"),
							);
							return Err(TYPE_NOT_FOUND);
						},
					},
				}
			},
			TypeNode::Pointer(base, mutable) => {
				trace!("Resolving pointer type");
				let base = self.resolve_type(base, reports)?;
				self.type_cache().make_pointer(base, *mutable)
			},
			TypeNode::Array { base, length } => match length.as_ref().map(|b| &**b) {
				Some(Expression::Literal(Literal::Integer { value: length, .. })) => {
					trace!("Resolving array type");
					let length: usize = match length {
						Integer::Any(i) => (*i).try_into().unwrap(),
						Integer::Int8(i) => (*i).try_into().unwrap(),
						Integer::Int16(i) => (*i).try_into().unwrap(),
						Integer::Int32(i) => (*i).try_into().unwrap(),
						Integer::Int64(i) => (*i).try_into().unwrap(),
						Integer::UInt8(i) => (*i).try_into().unwrap(),
						Integer::UInt16(i) => (*i).try_into().unwrap(),
						Integer::UInt32(i) => (*i).try_into().unwrap(),
						Integer::UInt64(i) => (*i).try_into().unwrap(),
					};

					let base = self.resolve_type(base, reports)?;
					let cache = self.type_cache();
					let mut arrays = cache.array_types.borrow_mut();

					let ty = arrays.entry((base, length)).or_insert_with(|| {
						cache.bump.alloc(Type::Array {
							ty: base,
							count: length,
						})
					});

					ty
				},
				_ => unimplemented!(),
			},
			_ => unimplemented!(),
		};

		let mut buf = vec![];
		Type::write(
			ty,
			&mut buf,
			&WriteRequirements {
				blobs: self.blob_heap().clone(),
			},
		)
		.unwrap();
		self.blob_heap().intern(buf);
		Ok(ty)
	}
}

// #[allow(unused)]
// pub fn invalid_type_err(expected: &Type, got: Option<&Type>) -> anyhow::Error {
// 	use anyhow::anyhow;
// 	match got {
// 		None => anyhow!("Expected type '{}', got '?'", expected),
// 		Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
// 	}
// }
