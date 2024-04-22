use ariadne::{Color, Label};

use leaf_parsing::ast::{BinaryOperator, Expression, FunctionCall, Ident, Integer, Literal, Node};
use leaf_reflection::{Comparison, SSABuilder, Type, ValueRef};

use crate::frontend::block::Block;
use crate::frontend::reports::*;

#[tracing::instrument(skip_all)]
pub fn compile_expression<'a, 'b, 'l>(
	expr: &Expression,
	expected: Option<&'l Type<'l>>,
	block: &'a Block<'a, 'b, 'l>,
	builder: &mut SSABuilder<'b, 'l>,
) -> Result<ValueRef<'b, 'l>, (FrontEndError, FrontEndReportBuilder)> {
	match expr {
		Expression::Literal(Literal::Integer { value, range }) => match value {
			Integer::Int8(v) => Ok(builder.constant(*v)),
			Integer::Int16(v) => Ok(builder.constant(*v)),
			Integer::Int32(v) => Ok(builder.constant(*v)),
			Integer::Int64(v) => Ok(builder.constant(*v)),
			Integer::UInt8(v) => Ok(builder.constant(*v)),
			Integer::UInt16(v) => Ok(builder.constant(*v)),
			Integer::UInt32(v) => Ok(builder.constant(*v)),
			Integer::UInt64(v) => Ok(builder.constant(*v)),
			Integer::Any(v) => {
				macro_rules! try_cast {
					($ty: ident, $expected: expr) => {{
						match (*v).try_into() {
							Ok(v) => Ok(builder.constant::<$ty>(v)),
							Err(_) => Err((
								INVALID_INTEGER,
								block.report_data.new_error(range.start).with_label(
									Label::new((block.report_data.file(), range.clone()))
										.with_color(Color::Red)
										.with_message(format!(
											"{v} cannot be converted to `{}`",
											$expected
										)),
								),
							)),
						}
					}};
				}

				match expected {
					Some(Type::Int8) => try_cast!(i8, Type::Int8),
					Some(Type::Int16) => try_cast!(i16, Type::Int16),
					Some(Type::Int32) => try_cast!(i32, Type::Int32),
					Some(Type::Int64) => try_cast!(i64, Type::Int64),
					Some(Type::UInt8) => try_cast!(u8, Type::UInt8),
					Some(Type::UInt16) => try_cast!(u16, Type::UInt16),
					Some(Type::UInt32) => try_cast!(u32, Type::UInt32),
					Some(Type::UInt64) => try_cast!(u64, Type::UInt64),
					None if (i32::MIN as i128..i32::MAX as i128).contains(v) => {
						Ok(builder.constant(*v as i32))
					},
					None if (i64::MIN as i128..i64::MAX as i128).contains(v) => {
						Ok(builder.constant(*v as i64))
					},
					_ => Err((
						NOT_IMPLEMENTED,
						unsupported_report(block.report_data.file(), expr, None::<&str>),
					)),
				}
			},
		},

		Expression::Literal(Literal::Bool { value, .. }) => Ok(builder.constant(*value)),
		Expression::Literal(Literal::String { value, .. }) => Ok(builder.constant(unescape(value))),

		Expression::Literal(Literal::Id(Ident { value, range })) => {
			if let Some(val) = block.values.get(value) {
				return match val.ty() {
					Type::Reference { ty, .. } if val.is_variable() && Some(*ty) == expected => {
						Ok(builder.load(*val).unwrap())
					},
					_ => Ok(*val),
				};
			}

			let err_label = format!("Identifier `{value}` does not exist in the current scope");
			return Err((
				VARIABLE_NOT_FOUND,
				block.report_data.new_error(range.start).with_label(
					Label::new((block.report_data.file(), range.clone()))
						.with_color(Color::Red)
						.with_message(err_label),
				),
			));
		},

		Expression::Binary {
			operator,
			lhs,
			rhs,
			range,
		} => {
			let lhs_v = compile_expression(lhs, None, block, builder)?;
			let rhs_v = compile_expression(lhs, Some(lhs_v.ty()), block, builder)?;
			unimplemented!("Use intrinsics until interfaces are implemented")
		},

		Expression::FunctionCall(call) => match &**call {
			FunctionCall {
				func:
					Expression::Literal(Literal::Id(Ident {
						value:
							name @ ("__bin_op_lt" | "__bin_op_eq" | "__bin_op_add" | "__bin_op_sub"
							| "__bin_op_mul" | "__bin_op_div" | "__bin_op_rem"),
						..
					})),
				params,
				range,
			} => {
				let [lhs, rhs] = params.as_slice() else {
					let mut report = block.report_data.new_error(range.start).with_label(
						Label::new((block.report_data.file(), range.clone()))
							.with_color(Color::Red)
							.with_message(format!("Expected 2 parameters, got {}", params.len())),
					);

					if params.len() > 2 {
						let mut additional = params[2].range();
						additional.end = additional.start;
						additional.start = additional.start - 1;
						report.add_label(
							Label::new((block.report_data.file(), additional.clone()))
								.with_message("Additional parameters start here"),
						)
					} else {
						let mut additional = params[0].range();
						additional.start = additional.end;
						additional.end = additional.end + 1;
						report.add_label(
							Label::new((block.report_data.file(), additional.clone()))
								.with_message("Additional parameters should start here"),
						)
					}

					return Err((INVALID_PARAMETER_COUNT, report));
				};
				let mut lhs_v = compile_expression(lhs, None, block, builder)?;
				let mut rhs_v = compile_expression(rhs, Some(lhs_v.ty()), block, builder)?;

				if matches!(lhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
					lhs_v = builder.load(lhs_v).unwrap();
				}
				if matches!(rhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
					rhs_v = builder.load(rhs_v).unwrap();
				}

				let (lhs_v, rhs_v) = match (lhs_v.ty(), rhs_v.ty()) {
					(lhs_t, rhs_t) if lhs_t.is_integer() && rhs_t.is_integer() => (lhs_v, rhs_v),
					_ => {
						return Err((
							INVALID_PARAMETER_TYPE,
							block.report_data.new_error(range.start).with_label(
								Label::new((block.report_data.file(), range.clone()))
									.with_color(Color::Red)
									.with_message(format! {
										"Invalid intrinsic invocation {}({}, {})",
										name, lhs_v.ty(), rhs_v.ty()
									}),
							),
						))
					},
				};

				Ok(match *name {
					"__bin_op_lt" => builder.lt(lhs_v, rhs_v).unwrap(),
					"__bin_op_eq" => builder.eq(lhs_v, rhs_v).unwrap(),
					"__bin_op_add" => builder.add(lhs_v, rhs_v).unwrap(),
					"__bin_op_sub" => builder.sub(lhs_v, rhs_v).unwrap(),
					"__bin_op_mul" => builder.mul(lhs_v, rhs_v).unwrap(),
					"__bin_op_div" => builder.div(lhs_v, rhs_v).unwrap(),
					"__bin_op_rem" => builder.rem(lhs_v, rhs_v).unwrap(),
					_ => unreachable!(),
				})
			},

			FunctionCall {
				range,
				func,
				params,
			} => {
				// let func_v = compile_expression(func, None, block, builder)?;

				Err((
					NOT_IMPLEMENTED,
					unsupported_report(block.report_data.file(), expr, None::<&str>),
				))
			},
		},
		_ => Err((
			NOT_IMPLEMENTED,
			unsupported_report(block.report_data.file(), expr, None::<&str>),
		)),
	}
}

#[allow(unused)]
fn op_to_cmp(op: BinaryOperator) -> Comparison {
	match op {
		BinaryOperator::Eq => Comparison::Eq,
		BinaryOperator::Ne => Comparison::Ne,
		BinaryOperator::Gt => Comparison::Gt,
		BinaryOperator::Lt => Comparison::Lt,
		BinaryOperator::Ge => Comparison::Ge,
		BinaryOperator::Le => Comparison::Le,
		_ => unreachable!(),
	}
}

fn unescape(str: &str) -> String {
	let mut string = String::new();
	let bytes = str.as_bytes();
	let mut i = 0;
	while let Some(ch) = bytes.get(i) {
		match *ch {
			b'\\' => {
				i += 1;
				match bytes[i] {
					b'n' => string.push('\n'),
					b'0' => string.push('\0'),
					_ => unimplemented!(),
				}
			},
			_ => string.push(*ch as char),
		}
		i += 1;
	}
	string
}
