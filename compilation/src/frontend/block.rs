use std::sync::Arc;

use ariadne::Label;
use fxhash::FxHashMap;
use tracing::instrument;

use leaf_parsing::ast::{Block as BlockAst, If, Node, Statement, VarDecl, While};
use leaf_reflection::{Function, SSABuilder, Type, ValueRef};
use leaf_reflection::heaps::{BlobHeapScope, HeapScopes};

use crate::frontend::expressions::compile_expression;
use crate::frontend::reports::*;
use crate::frontend::types::{assert_type_eq, assert_value_type_eq, TypeCache, TypeResolver};

pub struct Block<'a, 'b, 'l> {
	pub heaps: HeapScopes<'l>,
	pub type_cache: &'a TypeCache<'l>,
	pub func: &'l Function<'l>,
	pub values: FxHashMap<&'a str, ValueRef<'b, 'l>>,
	pub types: &'a FxHashMap<&'l str, &'l Type<'l>>,
	pub functions: &'a FxHashMap<&'l str, &'l Function<'l>>,
	pub report_data: ReportData<'b, 'l, 'a>,
}

impl<'a: 'b, 'b, 'l> Block<'a, 'b, 'l> {
	#[instrument(skip_all)]
	pub fn compile(
		&mut self,
		ast: &'a BlockAst<'a>,
		builder: &mut SSABuilder<'b, 'l>,
	) -> Result<(), (FrontEndError, FrontEndReportBuilder)> {
		for statement in &ast.statements {
			self.compile_statement(statement, builder)?;
		}
		Ok(())
	}

	#[instrument(skip_all)]
	fn compile_statement(
		&mut self,
		statement: &'a Statement<'a>,
		builder: &mut SSABuilder<'b, 'l>,
	) -> Result<(), (FrontEndError, FrontEndReportBuilder)> {
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
				let value = compile_expression(expr, expected_ty, self, builder)?;
				let local = match value.ty() {
					Type::Uninit => builder.alloca(expected_ty.unwrap(), *mutable),
					_ => {
						let expected_ty = expected_ty.unwrap_or(value.ty());
						assert_value_type_eq(value, expected_ty, expr.range(), &self.report_data)?;
						let local = builder.alloca(expected_ty, *mutable);
						builder.store(value, local).unwrap();
						local
					},
				};
				self.values.insert(name.value, local);
				self.report_data.variable_info.insert(local, (name, ty.as_ref(), expr));
				Ok(())
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
						Ok(())
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

			Statement::If(If {
				condition: cond,
				block: do_block_ast,
				r#else: Option::None,
				range,
			}) => {
				let val = compile_expression(cond, Some(&Type::Bool), self, builder)?;
				assert_value_type_eq(val, &Type::Bool, cond.range(), &self.report_data)?;

				let do_block = builder.create_block();
				let continue_block = builder.create_block();
				builder.br(val, do_block, continue_block).unwrap();

				builder.set_current_block(do_block).unwrap();
				let mut block = self.create_child();
				block.compile(do_block_ast, builder)?;
				builder.jp(continue_block).unwrap();
				builder.set_current_block(continue_block).unwrap();
				Ok(())
			},

			Statement::While(While {
				condition: cond,
				block: do_block_ast,
				range,
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
				block.compile(do_block_ast, builder)?;
				builder.jp(check_block).unwrap();
				builder.set_current_block(continue_block).unwrap();
				Ok(())
			},

			Statement::Expression(expr) => {
				compile_expression(expr, None, self, builder)?;
				Ok(())
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
				Ok(())
			},

			Statement::Return {
				expr: Some(expr), ..
			} => {
				let value = compile_expression(expr, Some(self.func.ret_ty()), self, builder)?;

				if let Err((_, mut report)) =
					assert_value_type_eq(value, self.func.ret_ty(), expr.range(), &self.report_data)
				{
					return Err((INVALID_RETURN_TYPE, report));
				}
				builder.ret(Some(value)).unwrap();
				Ok(())
			},

			_ => Err((
				NOT_IMPLEMENTED,
				unsupported_report(self.report_data.file(), statement, None::<&str>),
			)),
		}
	}

	pub fn create_child(&self) -> Block<'a, 'b, 'l> {
		Block {
			heaps: self.heaps.clone(),
			type_cache: self.type_cache,
			func: self.func,
			values: self.values.clone(),
			types: self.types,
			functions: self.functions,
			report_data: self.report_data.clone(),
		}
	}
}

impl<'l> TypeResolver<'l> for Block<'_, '_, 'l> {
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
