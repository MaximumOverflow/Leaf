use std::collections::BTreeMap;

use leaf_parsing::ast::{
	BinaryOperator, Expression, Ident, Integer, Literal, NewStruct, Node, UnaryOperator,
};
use leaf_reflection::{Comparison, Function, Opcode, SSAContextBuilder, Type, ValueIdx};

use crate::frontend::block::Block;
use crate::frontend::reports::*;
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

#[tracing::instrument(skip_all)]
pub fn compile_expression<'a, 'l>(
	expr: &Expression,
	expected: Option<&'l Type<'l>>,
	block: &'a Block<'a, 'l>,
	body: &mut SSAContextBuilder<'l>,
	reports: &mut ReportData,
) -> Result<ExpressionResult<'l>, FrontEndError> {
	match expr {
		Expression::Literal(Literal::Bool { value: v, .. }) => {
			Ok(ExpressionResult::Value(body.use_const(*v)))
		},
		Expression::Literal(Literal::String { value: str, .. }) => {
			Ok(ExpressionResult::Value(body.use_const(unescape(str))))
		},
		Expression::Literal(Literal::Integer {
			value: Integer::Int8(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::Int16(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::Int32(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::Int64(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::UInt8(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::UInt16(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::UInt32(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::UInt64(v),
			..
		}) => Ok(ExpressionResult::Value(body.use_const(*v))),
		Expression::Literal(Literal::Integer {
			value: Integer::Any(v),
			range,
		}) => {
			macro_rules! impl_int {
				($v: expr, $([$ty: ty; $int: ident]),+) => {
					match expected {
						$(
							Some(Type::$int) => {
								match (*$v).try_into() {
									Ok(v) => {
										Ok(ExpressionResult::Value(body.use_const::<$ty>(v)))
									}
									Err(_) => {
										reports.add_error_label(range.clone(), format! {
											"Integer {v} cannot fit into range {:?} of type {}",
											<$ty>::MIN..<$ty>::MAX,
											Type::$int,
										});
										Err(INVALID_INTEGER)
									}
								}
							}
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
		Expression::Literal(Literal::Id(Ident {
			value: ident,
			range,
		})) => {
			if let Some(value) = block.values.get(ident) {
				return Ok(ExpressionResult::Value(value.0));
			}
			if let Some(func) = block.functions.get(ident) {
				return Ok(ExpressionResult::Function(func));
			}
			reports.add_error_label(
				range.clone(),
				format! {
					"Identifier `{ident}` is not present in the current scope"
				},
			);
			Err(VARIABLE_NOT_FOUND)
		},
		Expression::NewStruct(NewStruct {
			ty: ty_ast,
			values,
			range,
		}) => {
			let ty = block.resolve_type(ty_ast, reports)?;
			let Type::Struct(r#struct) = ty else {
				reports.add_error_label(
					ty_ast.range(),
					format! {
						"Type `{ty}` is not a struct"
					},
				);
				return Err(NOT_A_STRUCT);
			};

			let mut exprs = BTreeMap::new();
			for (ident, expr) in values {
				let Some(pos) = r#struct.fields().iter().position(|f| f.name() == ident.value)
				else {
					reports.add_error_label(
						ident.range.clone(),
						format! {
							"Field `{ident}` not found in type `{ty}`"
						},
					);
					return Err(FIELD_NOT_FOUND);
				};
				exprs.insert(pos, (pos, ident, expr, r#struct.fields()[pos].ty()));
			}

			if exprs.len() != r#struct.fields().len() {
				let mut msg = "Struct expression missing fields: ".to_string();
				let mut separator = "";
				for field in r#struct.fields() {
					if exprs.values().find(|v| v.1.value == field.name()).is_none() {
						msg.push_str(separator);
						msg.push_str(field.name());
						separator = ", ";
					}
				}
				reports.add_error_label(range.clone(), msg);
				return Err(MISSING_FIELD);
			}

			let mut values = vec![ValueIdx(0); exprs.len()];
			for (i, _, expr, ty) in exprs.values().cloned() {
				values[i] =
					compile_expression(expr, Some(ty), block, body, reports)?.unwrap_value();
				let val_ty = body.value_type(values[i]).unwrap();
				if ty != val_ty {
					reports.add_error_label(
						expr.range(),
						format! {
							"Expected type `{ty}`, found `{val_ty}`"
						},
					);
					return Err(INVALID_FIELD_TYPE);
				}
			}

			let local = body.alloca(ty);
			body.push_opcode(Opcode::Aggregate(values, local));
			Ok(ExpressionResult::Value(local))
		},
		Expression::Unary { operator, expr, .. } => {
			let val = compile_expression(expr, expected, block, body, reports)?.unwrap_value();
			let val_ty = body.value_type(val).unwrap();
			match operator {
				UnaryOperator::Neg => match val_ty {
					Type::Bool => {
						let local = body.alloca(&Type::Bool);
						body.push_opcode(Opcode::LNot(val, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:#?}", operator),
				},
				UnaryOperator::Addr => {
					let ty = block.type_cache.make_pointer(val_ty, false);
					let local = body.alloca(&ty);
					body.push_opcode(Opcode::StoreA(val, local));
					Ok(ExpressionResult::Value(local))
				},
				_ => unimplemented!("{:#?}", operator),
			}
		},
		Expression::Binary {
			lhs, operator, rhs, ..
		} => {
			let lhs = compile_expression(lhs, expected, block, body, reports)?.unwrap_value();
			let rhs = compile_expression(rhs, expected, block, body, reports)?.unwrap_value();
			let lhs_ty = body.value_type(lhs).unwrap();
			let rhs_ty = body.value_type(rhs).unwrap();

			match operator {
				BinaryOperator::Add => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Int32);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					(Type::Pointer { .. }, Type::Int64) => {
						let local = body.alloca(lhs_ty);
						body.push_opcode(Opcode::SAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					(Type::Pointer { .. }, Type::UInt64) => {
						let local = body.alloca(lhs_ty);
						body.push_opcode(Opcode::UAdd(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", operator, lhs_ty, rhs_ty),
				},
				BinaryOperator::Mod => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Int32);
						body.push_opcode(Opcode::SMod(lhs, rhs, local));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", operator, lhs_ty, rhs_ty),
				},
				| BinaryOperator::Eq
				| BinaryOperator::Ne
				| BinaryOperator::Lt
				| BinaryOperator::Gt
				| BinaryOperator::Le
				| BinaryOperator::Ge => match (lhs_ty, rhs_ty) {
					(Type::Int32, Type::Int32) => {
						let local = body.alloca(&Type::Bool);
						body.push_opcode(Opcode::SCmp(lhs, rhs, local, op_to_cmp(*operator)));
						Ok(ExpressionResult::Value(local))
					},
					_ => unimplemented!("{:?} {} {}", operator, lhs_ty, rhs_ty),
				},
				_ => unimplemented!("{:#?}", operator),
			}
		},
		Expression::FunctionCall(call) => {
			let func =
				compile_expression(&call.func, None, block, body, reports)?.unwrap_function();
			let mut params = vec![];
			assert_eq!(call.params.len(), func.params().len());
			for (expr, param) in call.params.iter().zip(func.params()) {
				let value = compile_expression(expr, Some(param.ty()), block, body, reports)?;
				let value = value.unwrap_value();
				let val_ty = body.value_type(value).unwrap();
				if param.ty() != val_ty {
					reports.add_error_label(
						expr.range(),
						format! {
							"Expected type `{}`, found `{}`",
							param.ty(), val_ty,
						},
					);
					return Err(INVALID_PARAMETER_TYPE);
				}
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
