use leaf_reflection::{Field, Function, Type};
use crate::frontend::types::TypeResolver;
use fxhash::FxHashMap;

pub enum Symbol<'l> {
	Type(&'l Type<'l>),
	Field(&'l Field<'l>),
	Function(&'l Function<'l>),
	Namespace(&'l Namespace<'l>),
}

impl<'l> From<&'l Type<'l>> for Symbol<'l> {
	fn from(value: &'l Type<'l>) -> Self {
		Self::Type(value)
	}
}

impl<'l> From<&'l Field<'l>> for Symbol<'l> {
	fn from(value: &'l Field<'l>) -> Self {
		Self::Field(value)
	}
}

impl<'l> From<&'l Function<'l>> for Symbol<'l> {
	fn from(value: &'l Function<'l>) -> Self {
		Self::Function(value)
	}
}

impl<'l> From<&'l Namespace<'l>> for Symbol<'l> {
	fn from(value: &'l Namespace<'l>) -> Self {
		Self::Namespace(value)
	}
}

#[derive(Debug)]
pub struct Namespace<'l> {
	name: &'l str,
	types: FxHashMap<&'l str, &'l Type<'l>>,
	children: FxHashMap<&'l str, Namespace<'l>>,
	functions: FxHashMap<&'l str, &'l Function<'l>>,
}

impl<'l> Namespace<'l> {
	#[inline]
	pub fn new(name: &'l str) -> Self {
		Self {
			name,
			types: Default::default(),
			children: Default::default(),
			functions: Default::default(),
		}
	}

	#[inline]
	pub fn types(&self) -> &FxHashMap<&'l str, &'l Type<'l>> {
		&self.types
	}

	#[tracing::instrument(skip_all)]
	pub fn get_or_add_child(&mut self, name: &'l str) -> &mut Namespace<'l> {
		match name.split_once("::") {
			None => self.children.entry(name).or_insert_with(|| Namespace::new(name)),
			Some((name, rem)) => {
				let parent = self.children.entry(name).or_insert_with(|| Namespace::new(name));
				parent.get_or_add_child(rem)
			},
		}
	}

	#[tracing::instrument(skip_all)]
	pub fn get_child_mut(&mut self, name: &str) -> Option<&mut Namespace<'l>> {
		match name.split_once("::") {
			None => self.children.get_mut(name),
			Some((name, rem)) => {
				let parent = self.children.get_mut(name)?;
				parent.get_child_mut(rem)
			},
		}
	}

	#[inline]
	pub fn add_type(&mut self, ty: &'l Type<'l>) {
		match ty {
			Type::Struct(data) => {
				self.types.insert(data.name(), ty);
			},
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn add_fn(&mut self, func: &'l Function<'l>) {
		self.functions.insert(func.name(), func);
	}
}

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub enum SymbolResolverHint {
	#[default]
	None,
	Type,
	Field,
	Function,
	Namespace,
}

pub trait SymbolResolver<'l> {
	fn get_symbol(&'l self, name: &str, hint: SymbolResolverHint) -> Option<Symbol<'l>>;
}

impl<'l> SymbolResolver<'l> for Namespace<'l> {
	fn get_symbol(&'l self, name: &str, hint: SymbolResolverHint) -> Option<Symbol<'l>> {
		match hint {
			SymbolResolverHint::Field => None,
			SymbolResolverHint::Type => self.types.get(name).cloned().map(Into::into),
			SymbolResolverHint::Function => self.functions.get(name).cloned().map(Into::into),
			SymbolResolverHint::Namespace => self.children.get(name).map(Into::into),
			SymbolResolverHint::None => {
				if let Some(symbol) = self.types.get(name) {
					return Some((*symbol).into());
				}
				if let Some(symbol) = self.functions.get(name) {
					return Some((*symbol).into());
				}
				None
			},
		}
	}
}
