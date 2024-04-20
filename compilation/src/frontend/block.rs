use std::sync::Arc;
use anyhow::anyhow;
use fxhash::FxHashMap;
use tracing::instrument;

use leaf_parsing::ast::{Block as BlockAst, Expression, Literal, Statement};
use leaf_reflection::{Function, Opcode, SSAContextBuilder, Type, ValueIdx};
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes};

use crate::frontend::expressions::compile_expression;
use crate::frontend::types::{TypeCache, TypeResolver};

pub struct Block<'a, 'l> {
	pub heaps: HeapScopes<'l>,
	pub type_cache: &'a TypeCache<'l>,
	pub func: &'l Function<'l>,
	pub types: &'a FxHashMap<&'l str, &'l Type<'l>>,
	pub values: FxHashMap<&'a str, (ValueIdx, bool)>,
	pub functions: &'a FxHashMap<&'l str, &'l Function<'l>>,
}

impl<'a, 'l> Block<'a, 'l> {
	#[instrument(skip_all)]
	pub fn compile(
		&mut self,
		ast: &'a BlockAst<'a>,
		body: &mut SSAContextBuilder<'l>,
	) -> anyhow::Result<()> {
		for statement in &ast.statements {
			self.compile_statement(statement, body)?;
		}
		Ok(())
	}

	#[instrument(skip_all)]
	fn compile_statement(
		&mut self,
		statement: &'a Statement<'a>,
		body: &mut SSAContextBuilder<'l>,
	) -> anyhow::Result<()> {
		match statement {
			Statement::Return(expr) => match expr {
				None if self.func.ret_ty() == &Type::Void => {
					body.push_opcode(Opcode::Ret(None));
					Ok(())
				}
				Some(expr) => {
					let value = compile_expression(expr, Some(self.func.ret_ty()), self, body)?;
					body.push_opcode(Opcode::Ret(Some(value.unwrap_value())));
					Ok(())
				}
				None => Err(anyhow!(
					"Expected type {}, found {}",
					self.func.ret_ty(),
					Type::Void
				)),
			},

			Statement::VarDecl(decl) => {
				let expected = match &decl.ty {
					None => None,
					Some(ty) => Some(self.resolve_type(ty)?),
				};

				match decl.value {
					Expression::Literal(Literal::Uninit) => {
						let ty = expected.unwrap();
						let local = body.alloca(ty);
						self.values.insert(decl.name, (local, decl.mutable));
					}
					_ => {
						let value = compile_expression(&decl.value, expected, self, body)?;
						let ty = body.value_type(value.unwrap_value()).unwrap();
						assert_eq!(Some(expected.unwrap_or(ty)), Some(ty));

						let local = body.alloca(ty);
						body.push_opcode(Opcode::Store(value.unwrap_value(), local));
						self.values.insert(decl.name, (local, decl.mutable));
					}
				}

				Ok(())
			}

			Statement::Assignment(lhs, rhs) => {
				let lhs = compile_expression(lhs, None, self, body)?.unwrap_value();
				let lhs_t = body.value_type(lhs).unwrap();
				let rhs = compile_expression(rhs, Some(lhs_t), self, body)?.unwrap_value();
				assert_eq!(Some(lhs_t), body.value_type(rhs));
				body.push_opcode(Opcode::Store(rhs, lhs));
				Ok(())
			}

			Statement::While(stmt) => {
				let check = body.create_block();
				let exec = body.create_block();
				let exit = body.create_block();

				body.push_jp(check).unwrap();
				body.set_current_block(check).unwrap();
				let condition = compile_expression(&stmt.condition, Some(&Type::Bool), self, body)?
					.unwrap_value();
				body.push_br(condition, exec, exit).unwrap();

				body.set_current_block(exec).unwrap();
				let mut block = Block {
					heaps: self.heaps.clone(),
					func: self.func,
					types: self.types,
					values: self.values.clone(),
					type_cache: self.type_cache,
					functions: self.functions,
				};
				block.compile(&stmt.block, body)?;
				body.push_jp(check).unwrap();

				body.set_current_block(exit).unwrap();
				Ok(())
			}

			Statement::If(stmt) => {
				let cond = compile_expression(&stmt.condition, Some(&Type::Bool), self, body)?
					.unwrap_value();
				match &stmt.r#else {
					None => {
						let true_case = body.create_block();
						let false_case = body.create_block();
						body.push_br(cond, true_case, false_case).unwrap();
						body.set_current_block(true_case).unwrap();

						body.set_current_block(true_case).unwrap();
						let mut block = Block {
							heaps: self.heaps.clone(),
							func: self.func,
							types: self.types,
							values: self.values.clone(),
							type_cache: self.type_cache,
							functions: self.functions,
						};
						block.compile(&stmt.block, body)?;
						body.push_jp(false_case).unwrap();
						body.set_current_block(false_case).unwrap();
						Ok(())
					}
					_ => unimplemented!(),
				}
			}

			Statement::Expression(expr) => {
				compile_expression(expr, None, self, body)?;
				Ok(())
			}

			_ => unimplemented!("{:#?}", statement),
		}
	}
}

impl<'l> TypeResolver<'l> for Block<'_, 'l> {
	fn type_cache(&self) -> &TypeCache<'l> {
		self.type_cache
	}

	fn blob_heap(&self) -> &Arc<BlobHeapScope<'l>> {
		self.heaps.blob_heap()
	}

	fn types(&self) -> &FxHashMap<&'l str, &'l Type<'l>> {
		self.types
	}
}
