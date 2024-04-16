use anyhow::anyhow;

use leaf_parsing::ast::{BinaryOperator, Expression, Integer, Literal};
use leaf_reflection::{Comparison, Const, Function, Opcode, SSAContextBuilder, Type, ValueIdx};

use crate::frontend::block::Block;

pub enum ExpressionResult<'l> {
	Value(ValueIdx),
	Function(&'l Function<'l>),
}

impl<'l> ExpressionResult<'l> {
	pub fn unwrap_value(&self) -> ValueIdx {
		match self {
			Self::Value(v) => *v,
			_ => panic!("Expression result was not a value"),
		}
	}

	pub fn unwrap_function(&self) -> &'l Function<'l> {
		match self {
			Self::Function(v) => *v,
			_ => panic!("Expression result was not a function"),
		}
	}
}

pub fn compile_expression<'a, 'l>(
	expr: &Expression,
	expected: Option<&'l Type<'l>>,
	block: &'a Block<'a, 'l>,
	body: &mut SSAContextBuilder<'l>,
) -> anyhow::Result<ExpressionResult<'l>> {
	match expr {
		Expression::Literal(Literal::String(str)) => {
			let mut str = str.to_string();
			str.push('\0');
			let str = block.heaps.string_heap().intern_str(&str);
			Ok(ExpressionResult::Value(body.push_constant(Const::Str(str))))
		},
		Expression::Literal(Literal::Integer(Integer::Int32(v))) => {
			Ok(ExpressionResult::Value(body.push_constant(Const::I32(*v))))
		},
		#[rustfmt::skip]
		Expression::Literal(Literal::Integer(Integer::Any(v))) => {
			macro_rules! impl_int {
				($([$ty: ty, $int: ident, $const: ident]),+) => {
					match expected {
						$(
							Some(Type::$int) => {
								let v = (*v).try_into().map_err(|_| anyhow! {
									"Integer {v} cannot fit into range {:?} of type {}",
									<$ty>::MIN..<$ty>::MAX,
									Type::$int,
								})?;
								Ok(ExpressionResult::Value(body.push_constant(Const::$const(v))))
							},
						)*
						_ => unimplemented!("{:#?}", expr),
					}
				};
			}

			impl_int! {
				[i8, Int8, I8],
				[i16, Int16, I16],
				[i32, Int32, I32],
				[i64, Int64, I64],
				[u8, UInt8, I8],
				[u16, UInt16, U16],
				[u32, UInt32, U32],
				[u64, UInt64, U64]
			}
		},
		Expression::Literal(Literal::Id(ident)) => {
			if let Some(value) = block.values.get(ident) {
				return Ok(ExpressionResult::Value(value.0));
			}
			if let Some(func) = block.functions.get(ident) {
				return Ok(ExpressionResult::Function(func));
			}
			Err(anyhow!("Identifier {:?} is not present in the current scope", ident))
		},
		Expression::Binary(lhs, op, rhs) => {
			let lhs = compile_expression(lhs, expected, block, body)?.unwrap_value();
			let rhs = compile_expression(rhs, expected, block, body)?.unwrap_value();
			let lhs_ty = body.value_type(lhs).unwrap();
			let rhs_ty = body.value_type(rhs).unwrap();

			match op {
				BinaryOperator::Add => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.push_local(&Type::Int32);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				BinaryOperator::Lt => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.push_local(&Type::Bool);
						body.push_opcode(Opcode::SCmp(lhs, rhs, local, Comparison::Lt));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				_ => unimplemented!("{:#?}", op),
			}
		},
		Expression::FunctionCall(call) => {
			let func = compile_expression(&call.func, None, block, body)?.unwrap_function();
			let mut params = vec![];
			assert_eq!(call.params.len(), func.params().len());
			for (expr, param) in call.params.iter().zip(func.params()) {
				let value = compile_expression(expr, Some(param.ty()), block, body)?;
				let value = value.unwrap_value();
				assert_eq!(Some(param.ty()), body.value_type(value));
				params.push(value);
			}

			let result = body.push_local(func.ret_ty());
			body.push_opcode(Opcode::Call(func, params, result));
			Ok(ExpressionResult::Value(result))
		},
		_ => unimplemented!("{:#?}", expr),
	}
}
