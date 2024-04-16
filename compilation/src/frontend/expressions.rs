use anyhow::{anyhow, Context};
use leaf_reflection::{Comparison, Const, Opcode, SSAContextBuilder, Type, ValueIdx};
use leaf_parsing::ast::{BinaryOperator, Expression, Integer, Literal};
use crate::frontend::block::Block;

pub fn compile_expression(
	expr: &Expression, expected: Option<&Type>, block: &Block, body: &mut SSAContextBuilder,
) -> anyhow::Result<ValueIdx> {
	match expr {
		Expression::Literal(Literal::Integer(Integer::Int32(v))) => {
			Ok(body.push_constant(Const::I32(*v)))
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
								Ok(body.push_constant(Const::$const(v)))
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
			block.values.get(ident).map(|i| i.0).with_context(|| {
				format!("Identifier {:?} is not present in the current scope", ident)
			})
		},
		Expression::Binary(lhs, op, rhs) => {
			let lhs = compile_expression(lhs, expected, block, body)?;
			let rhs = compile_expression(rhs, expected, block, body)?;
			let lhs_ty = body.value_type(lhs).unwrap();
			let rhs_ty = body.value_type(rhs).unwrap();

			match op {
				BinaryOperator::Add => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.push_local(&Type::Int32);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(local)
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				BinaryOperator::Lt => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.push_local(&Type::Bool);
						body.push_opcode(Opcode::SCmp(lhs, rhs, local, Comparison::Lt));
						Ok(local)
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				_ => unimplemented!("{:#?}", op),
			}
		},
		_ => unimplemented!("{:#?}", expr),
	}
}
