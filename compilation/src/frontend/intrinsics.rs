use std::ops::Range;

use ariadne::{Color, Label};

use leaf_parsing::ast::{Expression, Literal, Node};
use leaf_reflection::{SSABuilder, Type, ValueRef};

use crate::frontend::block::Block;
use crate::frontend::expressions::compile_expression;
use crate::frontend::reports::{
	FrontEndError, FrontEndReportBuilder, invalid_parameter_count, INVALID_PARAMETER_TYPE,
};

type IntrinsicResult<'func, 'ctx> =
	Result<ValueRef<'func, 'ctx>, (FrontEndError, FrontEndReportBuilder)>;

pub(crate) fn dispatch_intrinsic<'blk, 'func, 'ctx>(
	name: &str,
	call_range: Range<usize>,
	parameters: &[Expression],
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
) -> Option<IntrinsicResult<'func, 'ctx>> {
	macro_rules! dispatch_unary {
		($(($id: literal, $op: ident)),*) => {
			match name {
				$(
					$id => return Some(apply_unary_expr(
						name,
						call_range,
						parameters,
						block,
						builder,
						&SSABuilder::$op,
					)),
				)*
				_ => {},
			}
		};
	}

	macro_rules! dispatch_binary {
		($(($id: literal, $op: ident)),*) => {
			match name {
				$(
					$id => return Some(apply_binary_expr(
						name,
						call_range,
						parameters,
						block,
						builder,
						&SSABuilder::$op,
					)),
				)*
				_ => {},
			}
		};
	}

	dispatch_unary![("__unary_op_not", not)];

	dispatch_binary![
		("__binary_op_eq", eq),
		("__binary_op_ne", ne),
		("__binary_op_lt", lt),
		("__binary_op_gt", gt),
		("__binary_op_le", le),
		("__binary_op_ge", ge),
		("__binary_op_add", add),
		("__binary_op_sub", sub),
		("__binary_op_mul", mul),
		("__binary_op_div", div),
		("__binary_op_rem", rem)
	];

	match name {
		"__binary_op_idx" => Some(idx_op(call_range, parameters, block, builder)),
		"__explicit_cast" => Some(explicit_cast(call_range, parameters, block, builder)),
		_ => None,
	}
}

fn idx_op<'blk, 'func, 'ctx>(
	call_range: Range<usize>,
	parameters: &[Expression],
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
) -> IntrinsicResult<'func, 'ctx> {
	let [lhs, rhs, mutable] = parameters else {
		return Err(invalid_parameter_count(
			3,
			parameters,
			call_range,
			&block.report_data,
		));
	};

	let mutable = match mutable {
		Expression::Literal(Literal::Bool { value, .. }) => *value,
		_ => unimplemented!(),
	};

	let lhs_v = compile_expression(lhs, None, block, builder)?;
	let mut rhs_v = compile_expression(rhs, Some(&Type::Int64), block, builder)?;

	let Type::Reference {
		ty: Type::Array { .. },
		..
	} = lhs_v.ty()
	else {
		unimplemented!();
	};

	if matches!(rhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
		rhs_v = builder.load(rhs_v).unwrap();
	}

	if !rhs_v.ty().is_integer() {
		unimplemented!();
	}

	Ok(builder.gep(lhs_v, rhs_v, mutable).unwrap())
}

fn explicit_cast<'blk, 'func, 'ctx>(
	call_range: Range<usize>,
	parameters: &[Expression],
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
) -> IntrinsicResult<'func, 'ctx> {
	let [lhs, ty_expr] = parameters else {
		return Err(invalid_parameter_count(
			2,
			parameters,
			call_range,
			&block.report_data,
		));
	};

	let Expression::Literal(Literal::Id(id)) = ty_expr else {
		let ty_range = ty_expr.range();
		return Err((
			INVALID_PARAMETER_TYPE,
			block
				.report_data
				.new_error(call_range.start)
				.with_label(
					Label::new((block.report_data.file(), call_range.clone()))
						.with_color(Color::Red)
						.with_message("Invalid intrinsic invocation"),
				)
				.with_label(
					Label::new((block.report_data.file(), ty_range))
						.with_color(Color::Cyan)
						.with_message("Expression is not a valid type name"),
				),
		));
	};

	'supported: {
		match id.value {
			"i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => {
				let mut lhs_v = compile_expression(lhs, None, block, builder)?;
				if matches!(lhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
					lhs_v = builder.load(lhs_v).unwrap();
				}

				return match (lhs_v.ty(), id.value) {
					(Type::Int32 | Type::UInt32, "u8") => {
						Ok(builder.trunc(&Type::UInt8, lhs_v).unwrap())
					},
					_ => break 'supported,
				};
			},
			_ => break 'supported,
		}
	}

	let ty_range = ty_expr.range();
	return Err((
		INVALID_PARAMETER_TYPE,
		block
			.report_data
			.new_error(call_range.start)
			.with_label(
				Label::new((block.report_data.file(), ty_range))
					.with_color(Color::Cyan)
					.with_message(format!("Unsupported type `{}`", id.value)),
			)
			.with_label(
				Label::new((block.report_data.file(), call_range.clone()))
					.with_color(Color::Red)
					.with_message("Invalid intrinsic invocation"),
			),
	));
}

fn apply_unary_expr<'blk, 'func, 'ctx>(
	name: &str,
	call_range: Range<usize>,
	parameters: &[Expression],
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
	function: &dyn Fn(
		&mut SSABuilder<'func, 'ctx>,
		ValueRef<'func, 'ctx>,
	) -> Result<ValueRef<'func, 'ctx>, String>,
) -> IntrinsicResult<'func, 'ctx> {
	let [val] = parameters else {
		return Err(invalid_parameter_count(
			1,
			parameters,
			call_range,
			&block.report_data,
		));
	};
	let mut val_v = compile_expression(val, None, block, builder)?;

	if matches!(val_v.ty(), Type::Reference { ty, ..} if ty.is_integer() || **ty == Type::Bool) {
		val_v = builder.load(val_v).unwrap();
	}

	if val_v.ty().is_integer() || *val_v.ty() == Type::Bool {
		return Ok(function(builder, val_v).unwrap());
	}

	Err((
		INVALID_PARAMETER_TYPE,
		block.report_data.new_error(call_range.start).with_label(
			Label::new((block.report_data.file(), call_range.clone()))
				.with_color(Color::Red)
				.with_message(format! {
					"Invalid intrinsic invocation {}({})",
					name, val_v.ty()
				}),
		),
	))
}

fn apply_binary_expr<'blk, 'func, 'ctx>(
	name: &str,
	call_range: Range<usize>,
	parameters: &[Expression],
	block: &'blk Block<'blk, 'func, 'ctx>,
	builder: &mut SSABuilder<'func, 'ctx>,
	function: &dyn Fn(
		&mut SSABuilder<'func, 'ctx>,
		ValueRef<'func, 'ctx>,
		ValueRef<'func, 'ctx>,
	) -> Result<ValueRef<'func, 'ctx>, String>,
) -> IntrinsicResult<'func, 'ctx> {
	let [lhs, rhs] = parameters else {
		return Err(invalid_parameter_count(
			2,
			parameters,
			call_range,
			&block.report_data,
		));
	};
	let mut lhs_v = compile_expression(lhs, None, block, builder)?;
	let mut rhs_v = compile_expression(rhs, Some(lhs_v.ty()), block, builder)?;

	if matches!(lhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
		lhs_v = builder.load(lhs_v).unwrap();
	}
	if matches!(rhs_v.ty(), Type::Reference { ty, ..} if ty.is_integer()) {
		rhs_v = builder.load(rhs_v).unwrap();
	}

	match (lhs_v.ty().is_integer(), rhs_v.ty().is_integer()) {
		(true, true) => Ok(function(builder, lhs_v, rhs_v).unwrap()),
		_ => Err((
			INVALID_PARAMETER_TYPE,
			block.report_data.new_error(call_range.start).with_label(
				Label::new((block.report_data.file(), call_range.clone()))
					.with_color(Color::Red)
					.with_message(format! {
						"Invalid intrinsic invocation {}({}, {})",
						name, lhs_v.ty(), rhs_v.ty()
					}),
			),
		)),
	}
}
