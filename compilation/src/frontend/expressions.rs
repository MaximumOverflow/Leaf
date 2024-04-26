use ariadne::{Color, Label};

use leaf_parsing::ast::{BinaryOperator, Expression, FunctionCall, Ident, Integer, Literal, Node};
use leaf_reflection::{Comparison, SSABuilder, Type, ValueRef};

use crate::frontend::block::Block;
use crate::frontend::intrinsics::dispatch_intrinsic;
use crate::frontend::reports::*;
use crate::frontend::types::{assert_value_type_eq, try_apply_implicit_casts};

#[tracing::instrument(skip_all)]
pub fn compile_expression<'blk, 'func, 'ctx>(
	expr: &Expression,
	expected: Option<&'ctx Type<'ctx>>,
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
) -> Result<ValueRef<'func, 'ctx>, (FrontEndError, FrontEndReportBuilder)> {
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

		Expression::Literal(Literal::Id(Ident { value: id, range })) => {
			if let Some(val) = block.values.get(id) {
				return match val.ty() {
					Type::Reference { ty, .. } if val.is_variable() && Some(*ty) == expected => {
						Ok(builder.load(*val).unwrap())
					},
					_ => Ok(*val),
				};
			}

			if let Some(func) = block.functions.get(id) {
				return Ok(builder.function(func));
			}

			let err_label = format!("Identifier `{id}` does not exist in the current scope");
			return Err((
				VARIABLE_NOT_FOUND,
				block.report_data.new_error(range.start).with_label(
					Label::new((block.report_data.file(), range.clone()))
						.with_color(Color::Red)
						.with_message(err_label),
				),
			));
		},

		#[allow(unused)]
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

		Expression::FunctionCall(call) => {
			let FunctionCall {
				range,
				func,
				params,
			} = &**call;

			if let Expression::Literal(Literal::Id(id)) = func {
				if let Some(result) =
					dispatch_intrinsic(id.value, range.clone(), params, block, builder)
				{
					return result;
				}
			}

			let func_v = compile_expression(func, None, block, builder)?;
			let Type::FunctionPointer { param_tys, .. } = func_v.ty() else {
				let range = func.range();
				return Err((
					NOT_A_FUNCTION,
					block.report_data.new_error(range.start).with_label(
						Label::new((block.report_data.file(), range))
							.with_color(Color::Red)
							.with_message("Expression does not evaluate to a function"),
					),
				));
			};

			if params.len() != param_tys.len() {
				return Err(invalid_parameter_count(
					param_tys.len(),
					params,
					range.clone(),
					&block.report_data,
				));
			}

			let mut params_v = Vec::with_capacity(params.len());
			for (expr, expected) in params.iter().zip(*param_tys) {
				let mut val = compile_expression(expr, Some(expected), block, builder)?;
				val = try_apply_implicit_casts(val, expected, builder);
				assert_value_type_eq(val, expected, expr.range(), &block.report_data)?;
				params_v.push(val);
			}

			Ok(builder.call(func_v, &params_v).unwrap())
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
