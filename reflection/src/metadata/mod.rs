mod types;
mod assembly;
mod functions;
mod ssa;

use std::fmt::{Display, Formatter};
pub use ssa::*;
pub use types::*;
pub use assembly::*;
pub use functions::*;

use leaf_derive::Metadata;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Metadata)]
#[metadata(lifetimes(val = "l"))]
pub struct UniqueIdentifier<'l> {
	name: &'l str,
	namespace: &'l str,
}

impl<'l> UniqueIdentifier<'l> {
	pub fn new(name: &'l str, namespace: &'l str) -> Self {
		Self { name, namespace }
	}

	pub fn name(&self) -> &'l str {
		self.name
	}

	pub fn namespace(&self) -> &'l str {
		self.namespace
	}
}

impl Display for UniqueIdentifier<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}/{}", self.namespace, self.name)
	}
}
