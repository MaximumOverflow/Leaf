use leaf_reflection::{Function, Type, UniqueIdentifier};
use std::fmt::Write;

pub trait MangleCpp {
	type Args<'l>;

	#[inline]
	fn mangled(&self, args: Self::Args<'_>) -> String {
		let mut str = String::new();
		self.mangle(&mut str, args).unwrap();
		str
	}

	fn mangle(&self, symbol: &mut impl Write, args: Self::Args<'_>) -> std::fmt::Result;
}

impl MangleCpp for Function<'_> {
	type Args<'l> = ();
	fn mangle(&self, symbol: &mut impl Write, _: Self::Args<'_>) -> std::fmt::Result {
		write!(symbol, "_ZN")?;
		self.id().mangle(symbol, UniqueIdentifier::new("", ""))?;

		for param in self.params() {
			param.ty().mangle(symbol, self.id())?;
		}

		if self.params().is_empty() {
			write!(symbol, "v")?;
		}

		Ok(())
	}
}

impl MangleCpp for Type<'_> {
	type Args<'l> = UniqueIdentifier<'l>;
	fn mangle(&self, symbol: &mut impl Write, args: Self::Args<'_>) -> std::fmt::Result {
		match self {
			Type::Int8 => write!(symbol, "a"),
			Type::Int16 => write!(symbol, "s"),
			Type::Int32 => write!(symbol, "i"),
			Type::Int64 => write!(symbol, "l"),
			Type::UInt8 => write!(symbol, "h"),
			Type::UInt16 => write!(symbol, "t"),
			Type::UInt32 => write!(symbol, "j"),
			Type::UInt64 => write!(symbol, "m"),
			Type::Struct(ty) => ty.id().mangle(symbol, args),
			_ => unimplemented!(),
		}
	}
}

impl MangleCpp for UniqueIdentifier<'_> {
	type Args<'l> = UniqueIdentifier<'l>;
	fn mangle(&self, symbol: &mut impl Write, args: Self::Args<'_>) -> std::fmt::Result {
		let common_ancestors = args
			.namespace()
			.split('/')
			.zip(self.namespace().split('/'))
			.take_while(|(a, b)| a == b)
			.count();

		if common_ancestors != 0 {
			match common_ancestors - 1 {
				0 => write!(symbol, "NS_")?,
				i => write!(symbol, "NS{}_", i - 1)?,
			}
		}

		for ns in self.namespace().split('/').skip(common_ancestors) {
			write!(symbol, "{}{}", ns.len(), ns)?;
		}
		write!(symbol, "{}{}E", self.name().len(), self.name())?;

		Ok(())
	}
}
