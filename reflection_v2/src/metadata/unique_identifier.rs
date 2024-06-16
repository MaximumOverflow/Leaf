use std::fmt::{Display, Formatter};
use crate::heaps::ConstRef;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UniqueIdentifier {
	name: ConstRef<str>,
	namespace: ConstRef<str>,
}

impl UniqueIdentifier {
	pub fn new(namespace: ConstRef<str>, name: ConstRef<str>) -> Self {
		Self { name, namespace }
	}

	pub fn name(&self) -> &str {
		self.name.get()
	}

	pub fn namespace(&self) -> &str {
		self.namespace.get()
	}
}

impl Display for UniqueIdentifier {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}::{}",
			self.namespace().replace('/', "::"),
			self.name()
		)
	}
}
