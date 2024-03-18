use leaf_reflection::structured::types::LeafType;
use leaf_parsing::ast::Type as TypeNode;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use std::sync::Arc;
use anyhow::anyhow;
use half::f16;

pub trait TypeResolver {
	fn types(&self) -> &HashMap<Arc<str>, Arc<Type>>;
	fn resolve_type(&self, ast: &TypeNode) -> anyhow::Result<Arc<Type>> {
		match ast {
			TypeNode::Id(id) => match *id {
				"void" => Ok(<()>::leaf_type().clone()),
				"i8" => Ok(i8::leaf_type().clone()),
				"i16" => Ok(i16::leaf_type().clone()),
				"i32" => Ok(i32::leaf_type().clone()),
				"i64" => Ok(i64::leaf_type().clone()),
				"u8" => Ok(u8::leaf_type().clone()),
				"u16" => Ok(u16::leaf_type().clone()),
				"u32" => Ok(u32::leaf_type().clone()),
				"u64" => Ok(u64::leaf_type().clone()),
				"f16" => Ok(f16::leaf_type().clone()),
				"f32" => Ok(f32::leaf_type().clone()),
				"f64" => Ok(f64::leaf_type().clone()),
				_ => match self.types().get(*id) {
					Some(ty) => Ok(ty.clone()),
					None => Err(anyhow::Error::msg(format!(
						"Type {:?} is not available in the current scope",
						id
					))),
				},
			},
			TypeNode::Pointer(ty, mutable) => {
				let ty = self.resolve_type(ty)?;
				Ok(ty.make_ptr(*mutable).clone())
			},
			_ => unimplemented!(),
		}
	}
}

pub fn invalid_type_err(expected: &Arc<Type>, got: Option<&Arc<Type>>) -> anyhow::Error {
	match got {
		None => anyhow!("Expected type '{}', got '?'", expected),
		Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
	}
}
