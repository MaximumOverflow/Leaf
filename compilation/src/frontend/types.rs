use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;
use ariadne::{Color, Label};
use fxhash::FxHashMap;
use tracing::trace;

use leaf_parsing::ast::{Expression, Ident, Integer, Literal, Node, Type as TypeNode};
use leaf_reflection::heaps::{ArenaAllocator, BlobHeapScope};
use leaf_reflection::serialization::{MetadataWrite, WriteRequirements};
use leaf_reflection::{SSABuilder, Type, ValueRef};
use crate::frontend::reports::*;

pub struct TypeCache<'l> {
	bump: &'l ArenaAllocator,
	array_types: RefCell<HashMap<(&'l Type<'l>, usize), &'l Type<'l>>>,
	pointer_types: RefCell<HashMap<(&'l Type<'l>, bool), &'l Type<'l>>>,
	reference_types: RefCell<HashMap<(&'l Type<'l>, bool), &'l Type<'l>>>,
}

impl<'l> TypeCache<'l> {
	pub fn new(bump: &'l ArenaAllocator) -> Self {
		Self {
			bump,
			array_types: RefCell::default(),
			pointer_types: RefCell::default(),
			reference_types: RefCell::default(),
		}
	}

	pub fn make_pointer(&self, base: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut pointers = self.pointer_types.borrow_mut();
		pointers
			.entry((base, mutable))
			.or_insert_with(|| self.bump.alloc(Type::Pointer { ty: base, mutable }))
	}

	pub fn make_reference(&self, base: &'l Type<'l>, mutable: bool) -> &'l Type<'l> {
		let mut pointers = self.pointer_types.borrow_mut();
		pointers
			.entry((base, mutable))
			.or_insert_with(|| self.bump.alloc(Type::Reference { ty: base, mutable }))
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
		reports: &ReportData,
	) -> Result<&'l Type<'l>, (FrontEndError, FrontEndReportBuilder)> {
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
							let report = reports.new_error(ast.range().start).with_label(
								Label::new((reports.file(), ast.range()))
									.with_color(Color::Red)
									.with_message(format!(
										"Type `{id}` is not available in the current scope"
									)),
							);
							return Err((TYPE_NOT_FOUND, report));
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

pub fn try_apply_implicit_casts<'a, 'l>(
	val: ValueRef<'a, 'l>,
	expected: &'l Type<'l>,
	builder: &mut SSABuilder<'a, 'l>,
) -> ValueRef<'a, 'l> {
	match val.ty() {
		Type::Reference { ty, .. } if val.is_variable() && *ty == expected => {
			builder.load(val).unwrap()
		},
		_ => val,
	}
}

#[inline(always)]
pub fn assert_type_eq<'l>(
	ty: &'l Type<'l>,
	expected: &'l Type<'l>,
	range: Range<usize>,
	reports: &ReportData,
) -> Result<(), (FrontEndError, FrontEndReportBuilder)> {
	match ty == expected {
		true => Ok(()),
		false => {
			let report = reports.new_error(range.start).with_label(
				Label::new((reports.file(), range))
					.with_color(Color::Red)
					.with_message(format! {
						"Expected type `{}`, found `{}`",
						expected, ty,
					}),
			);
			return Err((INVALID_TYPE, report));
		},
	}
}

#[inline(always)]
pub fn assert_value_type_eq<'l>(
	val: ValueRef<'_, 'l>,
	expected: &'l Type<'l>,
	range: Range<usize>,
	reports: &ReportData<'_, 'l, '_>,
) -> Result<(), (FrontEndError, FrontEndReportBuilder)> {
	match val.ty() == expected {
		true => Ok(()),
		false => {
			let mut report = reports.new_error(range.start);

			let ty = match reports.variable_info.get(&val) {
				None => val.ty(),
				Some((id, ty, expr)) => {
					let Type::Reference { ty: base, .. } = val.ty() else {
						unreachable!();
					};

					match ty {
						Some(ty) => {
							report.add_label(
								Label::new((reports.file(), ty.range().clone())).with_message(
									format!("Type `{}` explicitly specified here", base),
								),
							);
						},
						None => {
							report.add_label(
								Label::new((reports.file(), expr.range().clone())).with_message(
									format!("This expression evaluates to `{}`", base),
								),
							);
						},
					}
					base
				},
			};

			report.add_label(
				Label::new((reports.file(), range))
					.with_color(Color::Red)
					.with_message(format! {
						"Expected type `{}`, found `{}`",
						expected, ty,
					}),
			);

			return Err((INVALID_TYPE, report));
		},
	}
}
