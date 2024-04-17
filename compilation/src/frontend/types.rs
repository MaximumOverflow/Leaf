use std::cell::RefCell;
use std::collections::HashMap;
use anyhow::anyhow;
use tracing::trace;

use leaf_parsing::ast::{Expression, Integer, Literal, Type as TypeNode};
use leaf_reflection::heaps::Bump;
use leaf_reflection::{Array, Pointer, Type};

pub struct TypeCache<'l> {
	bump: &'l Bump,
	array_types: RefCell<HashMap<(&'l Type<'l>, usize), &'l Type<'l>>>,
	pointer_types: RefCell<HashMap<(&'l Type<'l>, bool), &'l Type<'l>>>,
}

impl<'l> TypeCache<'l> {
	pub fn new(bump: &'l Bump) -> Self {
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
			.or_insert_with(|| self.bump.alloc(Type::Pointer(Pointer { ty: base, mutable })))
	}
}

pub trait TypeResolver<'l> {
	fn type_cache(&self) -> &TypeCache<'l>;
	fn types(&self) -> &HashMap<&'l str, &'l Type<'l>>;
	fn resolve_type(&self, ast: &TypeNode) -> anyhow::Result<&'l Type<'l>> {
		match ast {
			TypeNode::Id(id) => {
				trace!("Resolving type {id:?}");
				match *id {
					"void" => Ok(&Type::Void),
					"char" => Ok(&Type::Char),
					"bool" => Ok(&Type::Bool),
					"i8" => Ok(&Type::Int8),
					"i16" => Ok(&Type::Int16),
					"i32" => Ok(&Type::Int32),
					"i64" => Ok(&Type::Int64),
					"u8" => Ok(&Type::UInt8),
					"u16" => Ok(&Type::UInt16),
					"u32" => Ok(&Type::UInt32),
					"u64" => Ok(&Type::UInt64),
					"f16" => Ok(&Type::Float16),
					"f32" => Ok(&Type::Float32),
					"f64" => Ok(&Type::Float64),
					_ => match self.types().get(id) {
						Some(ty) => Ok(ty),
						None => Err(anyhow!("Type `{id}` is not available in the current scope")),
					},
				}
			},
			TypeNode::Pointer(base, mutable) => {
				trace!("Resolving pointer type");
				let base = self.resolve_type(base)?;
				Ok(self.type_cache().make_pointer(base, *mutable))
			},
			TypeNode::Array { base, length } => match length.as_ref().map(|b| &**b) {
				Some(Expression::Literal(Literal::Integer(length))) => {
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

					let base = self.resolve_type(base)?;
					let cache = self.type_cache();
					let mut arrays = cache.array_types.borrow_mut();

					let ty = arrays.entry((base, length)).or_insert_with(|| {
						cache.bump.alloc(Type::Array(Array {
							ty: base,
							count: length,
						}))
					});

					Ok(ty)
				},
				_ => unimplemented!(),
			},
			_ => unimplemented!(),
		}
	}
}

#[allow(unused)]
pub fn invalid_type_err(expected: &Type, got: Option<&Type>) -> anyhow::Error {
	match got {
		None => anyhow!("Expected type '{}', got '?'", expected),
		Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
	}
}
