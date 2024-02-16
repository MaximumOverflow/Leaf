use leaf_reflection::builders::{MetadataBuilder, TypeSignatureBuilder, TypeSignatureBytes};
use leaf_reflection::{ElementRef, TypeDef};
use std::collections::HashMap;
use leaf_parsing::ast::Type;
use anyhow::Error;
use crate::BUG_ERR;

pub trait TypeResolver {
	fn metadata_builder(&self) -> &MetadataBuilder;
	fn types(&self) -> &HashMap<ElementRef<str>, ElementRef<TypeDef>>;

	fn create_type_signature(&self, ty: &Type) -> anyhow::Result<TypeSignatureBytes> {
		create_type_signature(ty, self.metadata_builder(), self.types())
	}
}

#[inline(never)]
fn create_type_signature(
	ty: &Type, md_builder: &MetadataBuilder, types: &HashMap<ElementRef<str>, ElementRef<TypeDef>>,
) -> anyhow::Result<TypeSignatureBytes> {
	let mut builder = TypeSignatureBuilder::default();

	fn recurse(
		ty: &Type, md_builder: &MetadataBuilder,
		types: &HashMap<ElementRef<str>, ElementRef<TypeDef>>, builder: &mut TypeSignatureBuilder,
	) -> anyhow::Result<()> {
		match ty {
			Type::Id(name) => {
				match *name {
					"void" => builder.push_void(),
					"char" => builder.push_char(),
					"bool" => builder.push_bool(),
					"f16" => builder.push_decimal(16),
					"f32" => builder.push_decimal(32),
					"f64" => builder.push_decimal(64),
					"i8" => builder.push_integer(8, true),
					"i16" => builder.push_integer(16, true),
					"i32" => builder.push_integer(32, true),
					"i64" => builder.push_integer(64, true),
					"u8" => builder.push_integer(8, false),
					"u16" => builder.push_integer(16, false),
					"u32" => builder.push_integer(32, false),
					"u64" => builder.push_integer(64, false),
					_ => {
						let name = match md_builder.get_str_ref(name) {
							Some(name) => name.into(),
							None => {
								return Err(Error::msg(format! {
									"Unresolved symbol {:?}.", name
								}))
							},
						};
						if let Some(ty) = types.get(&name).cloned() {
							builder.push_type(ty.into());
							return Ok(());
						}

						unimplemented!()
					},
				}
				Ok(())
			},
			Type::Ref {
				kind: "_",
				is_mut,
				base,
			} => {
				builder.push_ptr(*is_mut);
				recurse(base, md_builder, types, builder)
			},
			Type::Ref { .. } => unimplemented!(),
			Type::Array { .. } => unimplemented!(),
			Type::Template(_) => unimplemented!(),
		}
	}

	recurse(ty, md_builder, types, &mut builder)?;
	Ok(builder.build().expect(BUG_ERR))
}
