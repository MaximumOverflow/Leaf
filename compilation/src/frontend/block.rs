use std::sync::Arc;

use ariadne::Label;
use fxhash::FxHashMap;
use tracing::instrument;

use leaf_parsing::ast::{
	Block as BlockAst, Else, Expression, If, Literal, Node, Statement, VarDecl, While,
};
use leaf_reflection::{Function, SSABuilder, Type, ValueRef};
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes, TypeHeap};

use crate::frontend::expressions::compile_expression;
use crate::frontend::reports::*;
use crate::frontend::types::{assert_type_eq, assert_value_type_eq, TypeResolver};

pub struct Block<'blk, 'func, 'ctx> {
	pub heaps: HeapScopes<'ctx>,
	pub func: &'ctx Function<'ctx>,
	pub values: FxHashMap<&'blk str, ValueRef<'func, 'ctx>>,
	pub types: &'blk FxHashMap<&'ctx str, &'ctx Type<'ctx>>,
	pub functions: &'blk FxHashMap<&'ctx str, &'ctx Function<'ctx>>,
	pub report_data: ReportData<'func, 'ctx, 'blk>,
}

impl<'a: 'b, 'b, 'l> Block<'a, 'b, 'l> {
	#[instrument(skip_all)]
	pub fn compile(
		&mut self,
		ast: &'a BlockAst<'a>,
		builder: &mut SSABuilder<'b, 'l>,
	) -> Result<bool, (FrontEndError, FrontEndReportBuilder)> {
		let mut has_termination = false;
		for statement in &ast.statements {
			has_termination = self.compile_statement(statement, builder)?;
		}
		Ok(has_termination)
	}

	#[instrument(skip_all)]
	fn compile_statement(
		&mut self,
		statement: &'a Statement<'a>,
		builder: &mut SSABuilder<'b, 'l>,
	) -> Result<bool, (FrontEndError, FrontEndReportBuilder)> {
		match statement {
			Statement::VarDecl(VarDecl {
				name,
				mutable,
				ty,
				value: expr,
				..
			}) => {
				let expected_ty = match ty {
					None => None,
					Some(ty) => Some(self.resolve_type(ty, &self.report_data)?),
				};

				let local = match expr {
					Expression::Literal(Literal::Uninit { .. }) => {
						builder.alloca(expected_ty.unwrap(), *mutable)
					},
					_ => {
						let value = compile_expression(expr, expected_ty, self, builder)?;
						let expected_ty = expected_ty.unwrap_or(value.ty());
						assert_value_type_eq(value, expected_ty, expr.range(), &self.report_data)?;
						let local = builder.alloca(expected_ty, *mutable);
						builder.store(value, local).unwrap();
						local
					},
				};

				self.values.insert(name.value, local);
				self.report_data.variable_info.insert(local, (name, ty.as_ref(), expr));
				Ok(false)
			},

			Statement::Assignment { lhs, rhs } => {
				let dst = compile_expression(lhs, None, self, builder)?;
				let val = compile_expression(rhs, Some(dst.ty()), self, builder)?;
				match dst.ty() {
					Type::Reference { ty, mutable: false } => Err((
						VALUE_NOT_ASSIGNABLE,
						self.report_data.new_error(lhs.range().start).with_label(
							Label::new((self.report_data.file(), lhs.range()))
								.with_message(format!("Cannot assign `{}` to `{}`", val.ty(), ty)),
						),
					)),
					Type::Reference { ty, mutable: true } if *ty == val.ty() => {
						builder.store(val, dst).unwrap();
						Ok(false)
					},
					_ => Err((
						INVALID_TYPE,
						self.report_data.new_error(rhs.range().start).with_label(
							Label::new((self.report_data.file(), rhs.range())).with_message(
								format!("Cannot assign `{}` to `{}`", val.ty(), dst.ty()),
							),
						),
					)),
				}
			},

			Statement::If(statement) => self.compile_if(statement, builder),

			Statement::While(While {
				condition: cond,
				block: do_block_ast,
				..
			}) => {
				let check_block = builder.create_block();
				builder.jp(check_block).unwrap();

				builder.set_current_block(check_block).unwrap();
				let val = compile_expression(cond, Some(&Type::Bool), self, builder)?;
				assert_value_type_eq(val, &Type::Bool, cond.range(), &self.report_data)?;

				let do_block = builder.create_block();
				let continue_block = builder.create_block();
				builder.br(val, do_block, continue_block).unwrap();

				builder.set_current_block(do_block).unwrap();
				let mut block = self.create_child();
				if !block.compile(do_block_ast, builder)? {
					builder.jp(check_block).unwrap();
				}
				builder.set_current_block(continue_block).unwrap();
				Ok(false)
			},

			Statement::Expression(expr) => {
				compile_expression(expr, None, self, builder)?;
				Ok(false)
			},

			Statement::Return {
				expr: Option::None,
				range,
			} => {
				if let Err((_, report)) = assert_type_eq(
					&Type::Void,
					self.func.ret_ty(),
					range.clone(),
					&self.report_data,
				) {
					return Err((INVALID_RETURN_TYPE, report));
				}
				builder.ret(None).unwrap();
				Ok(true)
			},

			Statement::Return {
				expr: Some(expr), ..
			} => {
				let value = compile_expression(expr, Some(self.func.ret_ty()), self, builder)?;

				if let Err((_, report)) =
					assert_value_type_eq(value, self.func.ret_ty(), expr.range(), &self.report_data)
				{
					return Err((INVALID_RETURN_TYPE, report));
				}
				builder.ret(Some(value)).unwrap();
				Ok(true)
			},

			#[allow(unreachable_patterns)]
			_ => Err((
				NOT_IMPLEMENTED,
				unsupported_report(self.report_data.file(), statement, None::<&str>),
			)),
		}
	}

	pub fn create_child(&self) -> Block<'a, 'b, 'l> {
		Block {
			heaps: self.heaps.clone(),
			func: self.func,
			values: self.values.clone(),
			types: self.types,
			functions: self.functions,
			report_data: self.report_data.clone(),
		}
	}

	fn compile_if(
		&mut self,
		statement: &'a If<'a>,
		builder: &mut SSABuilder<'b, 'l>,
	) -> Result<bool, (FrontEndError, FrontEndReportBuilder)> {
		let If {
			condition: cond,
			block: do_block_ast,
			r#else,
			..
		} = statement;

		match &r#else {
			None => {
				let val = compile_expression(cond, Some(&Type::Bool), self, builder)?;
				assert_value_type_eq(val, &Type::Bool, cond.range(), &self.report_data)?;

				let do_block = builder.create_block();
				let continue_block = builder.create_block();
				builder.br(val, do_block, continue_block).unwrap();

				builder.set_current_block(do_block).unwrap();
				let mut block = self.create_child();
				if !block.compile(do_block_ast, builder)? {
					builder.jp(continue_block).unwrap();
				};
				builder.set_current_block(continue_block).unwrap();
				Ok(false)
			},
			Some(r#else) => {
				let val = compile_expression(cond, Some(&Type::Bool), self, builder)?;
				assert_value_type_eq(val, &Type::Bool, cond.range(), &self.report_data)?;

				let mut then_block = builder.create_block();
				let mut else_block = builder.create_block();
				builder.br(val, then_block, else_block).unwrap();

				builder.set_current_block(then_block).unwrap();
				let mut block = self.create_child();
				let then_term = block.compile(do_block_ast, builder)?;
				then_block = builder.current_block();

				builder.set_current_block(else_block).unwrap();
				block = self.create_child();
				let else_term = match r#else {
					Else::If(statement) => block.compile_if(statement, builder)?,
					Else::Block(else_block_ast) => block.compile(else_block_ast, builder)?,
				};
				else_block = builder.current_block();

				if !matches!((then_term, else_term), (true, true)) {
					let continue_block = builder.create_block();
					if !then_term {
						builder.set_current_block(then_block).unwrap();
						builder.jp(continue_block).unwrap();
					}
					if !else_term {
						builder.set_current_block(else_block).unwrap();
						builder.jp(continue_block).unwrap();
					}

					builder.set_current_block(continue_block).unwrap();
				}

				Ok(then_term & else_term)
			},
		}
	}
}

impl<'l> TypeResolver<'l> for Block<'_, '_, 'l> {
	fn type_heap(&self) -> &TypeHeap<'l> {
		self.heaps.type_heap()
	}

	fn blob_heap(&self) -> &Arc<BlobHeapScope<'l>> {
		self.heaps.blob_heap()
	}

	fn types(&self) -> &FxHashMap<&'l str, &'l Type<'l>> {
		self.types
	}
}
