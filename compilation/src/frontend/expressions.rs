use leaf_reflection::builders::TypeSignatureBytes;
use leaf_parsing::ast::{BinaryOperator, Expression, Literal};
use crate::frontend::block::Block;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use anyhow::Error;

impl Block {
	pub fn derive_type_signature(_expr: &Expression) -> anyhow::Result<TypeSignatureBytes> {
		unimplemented!()
	}

	pub fn generate_expression(
		expr: &Expression, opcodes: &mut Vec<Opcode>, locals: &HashMap<&str, (usize, Vec<u8>)>,
	) -> anyhow::Result<()> {
		match expr {
			Expression::Literal(literal) => match literal {
				Literal::Id(id) => match locals.get(id) {
					Some((id, _)) => opcodes.push(Opcode::PushLocal(*id)),
					None => return Err(Error::msg(format!("Unresolved symbol {:?}.", id))),
				},

				Literal::Integer(i) => {
					let i = *i;
					match i >= 0 {
						true => match i > u64::MAX as i128 {
							false => match i <= i64::MAX as i128 {
								true => opcodes.push(Opcode::PushInt(i as i64)),
								false => opcodes.push(Opcode::PushUInt(i as u64)),
							},
							true => return Err(Error::msg("Integer too big to fit in a u64.")),
						},
						false => match i < i64::MIN as i128 {
							false => opcodes.push(Opcode::PushInt(i as i64)),
							true => return Err(Error::msg("Integer too small to fit in a i64.")),
						},
					}
				},

				Literal::Decimal(dec) => {
					opcodes.push(Opcode::PushDecimal(*dec));
				},

				_ => unimplemented!("Unimplemented literal: {:#?}", literal),
			},

			Expression::Binary(lhs, op, rhs) => {
				Self::generate_expression(rhs, opcodes, locals)?;
				Self::generate_expression(lhs, opcodes, locals)?;
				match *op {
					BinaryOperator::Add => opcodes.push(Opcode::Add),
					BinaryOperator::Sub => opcodes.push(Opcode::Sub),
					BinaryOperator::Mul => opcodes.push(Opcode::Mul),
					BinaryOperator::Div => opcodes.push(Opcode::Div),
					_ => unimplemented!("Unimplemented binary operation: {:#?}", op),
				}
			}

			_ => unimplemented!("Unimplemented expression: {:#?}", expr),
		}

		Ok(())
	}
}
