use std::cell::RefCell;
use std::collections::HashMap;
use anyhow::anyhow;
use tracing::trace;

use leaf_parsing::ast::Type as TypeNode;
use leaf_reflection::heaps::Bump;
use leaf_reflection::{Pointer, Type};

pub struct TypeCache<'l> {
	bump: &'l Bump,
	pointer_types: RefCell<HashMap<(&'l Type<'l>, bool), &'l Type<'l>>>,
}

impl<'l> TypeCache<'l> {
	pub fn new(bump: &'l Bump) -> Self {
		Self {
			bump,
			pointer_types: RefCell::default(),
		}
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
					_ => todo!(),
				}
			},
			TypeNode::Pointer(base, mutable) => {
				trace!("Resolving pointer type");
				let base = self.resolve_type(base)?;
				let cache = self.type_cache();
				let mut pointers = cache.pointer_types.borrow_mut();

				let ty = pointers
					.entry((base, *mutable))
					.or_insert_with(|| cache.bump.alloc(Type::Pointer(Pointer {
						ty: base,
						mutable: *mutable,
					})));

				Ok(ty)
			},
			_ => unimplemented!(),
		}
	}
}

pub fn invalid_type_err(expected: &Type, got: Option<&Type>) -> anyhow::Error {
	match got {
		None => anyhow!("Expected type '{}', got '?'", expected),
		Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
	}
}
