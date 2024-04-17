use std::collections::BTreeMap;
use anyhow::anyhow;

use leaf_parsing::ast::{BinaryOperator, Expression, Integer, Literal, UnaryOperator};
use leaf_reflection::{Comparison, Function, Opcode, SSAContextBuilder, Type, ValueIdx};

use crate::frontend::block::Block;
use crate::frontend::types::TypeResolver;

pub enum ExpressionResult<'l> {
	Void,
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
		Expression::Literal(Literal::Boolean(v)) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::String(str)) => {
			Ok(ExpressionResult::Value(body.use_const(unescape(str))))
		},
		Expression::Literal(Literal::Integer(Integer::Int8(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::Int16(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::Int32(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::Int64(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::UInt8(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::UInt16(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::UInt32(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::Integer(Integer::UInt64(v))) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		#[rustfmt::skip]
		Expression::Literal(Literal::Integer(Integer::Any(v))) => {
			macro_rules! impl_int {
				($v: expr, $([$ty: ty; $int: ident]),+) => {
					match expected {
						$(
							Some(Type::$int) => {
								let v: $ty = (*$v).try_into().map_err(|_| anyhow! {
									"Integer {v} cannot fit into range {:?} of type {}",
									<$ty>::MIN..<$ty>::MAX,
									Type::$int,
								})?;
								Ok(ExpressionResult::Value(body.use_const(v)))
							},
						)*
						_ => unimplemented!("{:#?}", expr),
					}
				};
			}

			impl_int! {
				v,
				[i8; Int8],
				[i16; Int16],
				[i32; Int32],
				[i64; Int64],
				[u8; UInt8],
				[u16; UInt16],
				[u32; UInt32],
				[u64; UInt64]
			}
		},
		Expression::Literal(Literal::Id(ident)) => {
			if let Some(value) = block.values.get(ident) {
				return Ok(ExpressionResult::Value(value.0));
			}
			if let Some(func) = block.functions.get(ident) {
				return Ok(ExpressionResult::Function(func));
			}
			Err(anyhow!(
				"Identifier {:?} is not present in the current scope",
				ident
			))
		},
		Expression::NewStruct(new) => {
			let ty = block.resolve_type(&new.ty)?;
			let Type::Struct(r#struct) = ty else {
				return Err(anyhow!("Type `{ty}` is not a struct"));
			};

			let mut exprs = BTreeMap::new();
			for (name, (i, expr)) in &new.values {
				let Some(pos) = r#struct.fields().iter().position(|f| f.name() == *name) else {
					return Err(anyhow!("Field `{name}` not found in type `{ty}`"));
				};
				exprs.insert(*i, (pos, expr, r#struct.fields()[pos].ty()));
			}

			let mut values = vec![ValueIdx(0); exprs.len()];
			for (i, expr, ty) in exprs.values().cloned() {
				values[i] = compile_expression(expr, Some(ty), block, body)?.unwrap_value();
				assert_eq!(Some(ty), body.value_type(values[i]));
			}

			let local = body.alloca(ty);
			body.push_opcode(Opcode::Aggregate(values, local));
			Ok(ExpressionResult::Value(local))
		},
		Expression::Unary(op, val) => {
			let val = compile_expression(val, expected, block, body)?.unwrap_value();
			let val_ty = body.value_type(val).unwrap();
			match op {
				UnaryOperator::Neg => match val_ty {
					Type::Bool => {
						let local = body.alloca(&Type::Bool);
						body.push_opcode(Opcode::LNot(val, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:#?}", op),
				},
				UnaryOperator::Addr => {
					let ty = block.type_cache.make_pointer(val_ty, false);
					let local = body.alloca(&ty);
					body.push_opcode(Opcode::StoreA(val, local));
					Ok(ExpressionResult::Value(local))
				},
				_ => unimplemented!("{:#?}", op),
			}
		},
		Expression::Binary(lhs, op, rhs) => {
			let lhs = compile_expression(lhs, expected, block, body)?.unwrap_value();
			let rhs = compile_expression(rhs, expected, block, body)?.unwrap_value();
			let lhs_ty = body.value_type(lhs).unwrap();
			let rhs_ty = body.value_type(rhs).unwrap();

			match op {
				BinaryOperator::Add => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Int32);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					(Type::Pointer(_), Type::Int64) => {
						let local = body.alloca(lhs_ty);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					(Type::Pointer(_), Type::UInt64) => {
						let local = body.alloca(lhs_ty);
						body.push_opcode(Opcode::UAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				BinaryOperator::Mod => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Int32);
						body.push_opcode(Opcode::SMod(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", op, lhs_ty, rhs_ty),
				},
				| BinaryOperator::Eq
				| BinaryOperator::Ne
				| BinaryOperator::Lt
				| BinaryOperator::Gt
				| BinaryOperator::Le
				| BinaryOperator::Ge => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Bool);
						body.push_opcode(Opcode::SCmp(lhs, rhs, local, op_to_cmp(*op)));
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

			match func.ret_ty() {
				Type::Void => {
					body.push_opcode(Opcode::Call(func, params, None));
					Ok(ExpressionResult::Void)
				},
				_ => {
					let result = body.alloca(func.ret_ty());
					body.push_opcode(Opcode::Call(func, params, Some(result)));
					Ok(ExpressionResult::Value(result))
				},
			}
		},
		_ => unimplemented!("{:#?}", expr),
	}
}

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
					_ => unimplemented!(),
				}
			},
			_ => string.push(*ch as char),
		}
		i += 1;
	}
	string
}
