use leaf_parsing::ast::{BinaryOperator, Expression, Literal};
use leaf_reflection::builders::TypeSignatureBytes;
use crate::frontend::block::Block;
use crate::frontend::types::Type;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use anyhow::Error;

impl Block {
	pub fn derive_type_signature(_expr: &Expression) -> anyhow::Result<TypeSignatureBytes> {
		unimplemented!()
	}

	pub fn generate_expression(
		expr: &Expression, opcodes: &mut Vec<Opcode>, locals: &HashMap<&str, (usize, Vec<u8>)>,
	) -> anyhow::Result<&'static Type> {
		match expr {
			Expression::Literal(literal) => match literal {
				Literal::Id(id) => match locals.get(id) {
					Some((id, _)) => {
						opcodes.push(Opcode::PushLocal(*id));
						unimplemented!()
					},
					None => Err(Error::msg(format!("Unresolved symbol {:?}.", id))),
				},

				Literal::Integer(i) => {
					let i = *i;
					match (64 - i.leading_zeros()) <= 32 {
						false => {
							opcodes.push(Opcode::PushInt64(i));
							Ok(Type::UINT64)
						},
						true => {
							opcodes.push(Opcode::PushInt32(i as i32));
							Ok(Type::INT32)
						},
					}
				},

				Literal::Decimal(dec) => {
					opcodes.push(Opcode::PushDecimal64(*dec));
					Ok(Type::DEC64)
				},

				_ => unimplemented!("Unimplemented literal: {:#?}", literal),
			},

			Expression::Binary(lhs, op, rhs) => {
				let rhs = Self::generate_expression(rhs, opcodes, locals)?;
				let lhs = Self::generate_expression(lhs, opcodes, locals)?;

				// TODO Implement operators so this is not needed
				let res_type = match lhs != rhs {
					false => lhs,
					true => match (lhs, rhs) {
						(Type::Integer(lhs_size), Type::Integer(rhs_size)) => {
							match lhs_size < rhs_size {
								true => {
									opcodes.push(Opcode::SignExtend(*rhs_size));
									rhs
								},
								false => {
									opcodes.push(Opcode::Swap);
									opcodes.push(Opcode::SignExtend(*lhs_size));
									opcodes.push(Opcode::Swap);
									lhs
								}
							}
						}

						(Type::Integer(_), Type::Decimal(rhs_size)) => {
							opcodes.push(Opcode::ConvDecimal(*rhs_size));
							rhs
						}

						_ => unimplemented!(),
					}
				};

				match *op {
					BinaryOperator::Add => opcodes.push(Opcode::Add),
					BinaryOperator::Sub => opcodes.push(Opcode::Sub),
					BinaryOperator::Mul => opcodes.push(Opcode::Mul),
					BinaryOperator::Div => opcodes.push(Opcode::Div),
					_ => unimplemented!("Unimplemented binary operation: {:#?}", op),
				}

				Ok(res_type)
			}

			_ => unimplemented!("Unimplemented expression: {:#?}", expr),
		}
	}
}
