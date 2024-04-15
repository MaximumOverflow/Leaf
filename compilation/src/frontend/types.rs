use std::collections::HashMap;
use std::sync::Arc;

use anyhow::anyhow;

use leaf_parsing::ast::Type as TypeNode;
use leaf_reflection::Type;

pub trait TypeResolver<'l> {
	fn types(&self) -> &HashMap<&'l str, &'l Type<'l>>;
	fn resolve_type(&self, ast: &TypeNode) -> anyhow::Result<Arc<Type>> {
		todo!()
	}
}

pub fn invalid_type_err(expected: &Type, got: Option<&Type>) -> anyhow::Error {
	match got {
		None => anyhow!("Expected type '{}', got '?'", expected),
		Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
	}
}
