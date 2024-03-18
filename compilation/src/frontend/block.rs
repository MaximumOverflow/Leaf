use leaf_parsing::ast::{Block as BlockAST, Else, Expression, If, Literal, Statement, While};
use crate::frontend::expressions::{compile_expression, Mutability};
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use crate::frontend::types::{invalid_type_err, TypeResolver};
use leaf_reflection::structured::types::LeafType;
use crate::frontend::expressions::Value;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use std::sync::Arc;
use anyhow::anyhow;

pub struct Block<'l> {
	parent: &'l mut dyn BlockRequirements,
	values: HashMap<Arc<str>, Value>,
}

pub trait BlockRequirements: TypeResolver {
	fn expected_type(&self) -> &Arc<Type>;
	fn values(&self) -> &HashMap<Arc<str>, Value>;
	fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value>;
}

impl<'l> Block<'l> {
	pub fn new(parent: &'l mut dyn BlockRequirements) -> Self {
		Self {
			values: parent.values().clone(),
			parent,
		}
	}
	pub fn compile(
		&mut self, block: &BlockAST, builder: &mut FunctionBodyBuilder,
	) -> anyhow::Result<()> {
		for statement in &block.statements {
			match statement {
				Statement::Return(None) => {
					let expected = self.parent.expected_type();
					match expected == <()>::leaf_type() {
						true => {
							builder.push_opcode(Opcode::Ret);
						},
						false => {
							return Err(invalid_type_err(expected, Some(<()>::leaf_type())));
						},
					}
				},

				Statement::Return(Some(expr)) => {
					let expected = self.parent.expected_type();
					let expr = compile_expression(self, expr, builder)?;
					expr.load(builder)?;

					if expr.ty() != Some(expected) {
						return Err(invalid_type_err(expected, expr.ty()));
					}
					builder.push_opcode(Opcode::Ret);
				},

				Statement::VarDecl(decl) => {
					let ty = self.resolve_type(decl.ty.as_ref().unwrap())?;
					let local = builder.declare_local(&ty);
					let id = local.id();

					let initialized = match decl.value {
						Expression::Literal(Literal::Uninit) => false,
						_ => {
							let value = compile_expression(self, &decl.value, builder)?;
							if Some(&ty) != value.ty() {
								return Err(invalid_type_err(&ty, value.ty()));
							}

							value.load(builder)?;
							builder.store_local(id).unwrap();
							true
						},
					};

					let mutability = match decl.mutable {
						true => Mutability::Mutable,
						false => Mutability::Immutable,
					};

					self.values.insert(
						Arc::from(decl.name),
						Value::Local(ty, id, mutability, initialized),
					);
				},

				Statement::Assignment(to, value) => {
					let target = compile_expression(self, to, builder)?;
					let value = compile_expression(self, value, builder)?;
					value.load(builder)?;

					match &target {
						Value::Local(ty, id, mutability, ..) => {
							match mutability {
								Mutability::Mutable => match Some(ty) == value.ty() {
									true => builder.store_local(*id).unwrap(),
									false => return Err(invalid_type_err(&ty, value.ty())),
								},
								Mutability::Immutable => {
									let name = self
										.values
										.iter()
										.find_map(|(name, value)| match value {
											Value::Local(_, l_id, ..) if l_id == id => Some(name),
											_ => None,
										})
										.unwrap();
									return Err(anyhow!("Local '{}' is not mutable", name));
								},
							};
						},
						_ => unimplemented!("{:?} = {:?}", target, value),
					}
				},

				Statement::If(stmt) => {
					self.compile_if(stmt, builder)?;
				},

				Statement::While(While { condition, block }) => {
					let check_block = builder.add_block();
					builder.use_block(check_block).unwrap();

					let condition = compile_expression(self, condition, builder)?;
					condition.load(builder)?;

					let then_block = builder.add_block();
					let cont_block = builder.add_block();
					builder.cond_jump_n(cont_block).unwrap();

					let mut block_data = Block {
						values: self.values.clone(),
						parent: self,
					};
					builder.use_block(then_block).unwrap();
					block_data.compile(block, builder)?;

					builder.jump(check_block).unwrap();
					builder.use_block(cont_block).unwrap();
				},

				_ => unimplemented!("{:?}", statement),
			}
		}
		Ok(())
	}

	fn compile_if(&mut self, stmt: &If, builder: &mut FunctionBodyBuilder) -> anyhow::Result<()> {
		let If {
			condition,
			block,
			r#else,
		} = stmt;
		let condition = compile_expression(self, condition, builder)?;
		condition.load(builder)?;

		let then_block = builder.add_block();
		let else_block = builder.add_block();

		match r#else {
			None => {
				builder.cond_jump_n(else_block).unwrap();

				let mut block_data = Block {
					values: self.values.clone(),
					parent: self,
				};
				builder.use_block(then_block).unwrap();
				block_data.compile(block, builder)?;

				builder.use_block(else_block).unwrap();
			},

			Some(r#else) => match &**r#else {
				Else::Block(r#else) => {
					builder.cond_jump_n(else_block).unwrap();

					let mut block_data = Block {
						values: self.values.clone(),
						parent: self,
					};
					builder.use_block(then_block).unwrap();
					block_data.compile(block, builder)?;

					block_data = Block {
						values: self.values.clone(),
						parent: self,
					};
					builder.use_block(else_block).unwrap();
					block_data.compile(r#else, builder)?;
				},

				Else::If(r#if) => {
					let cont_block = builder.add_block();
					builder.cond_jump(then_block).unwrap();

					let mut block_data = Block {
						values: self.values.clone(),
						parent: self,
					};
					builder.use_block(then_block).unwrap();
					block_data.compile(block, builder)?;
					builder.jump(cont_block).unwrap();

					block_data = Block {
						values: self.values.clone(),
						parent: self,
					};
					builder.use_block(else_block).unwrap();
					block_data.compile_if(r#if, builder)?;
					builder.jump(cont_block).unwrap();

					builder.use_block(cont_block).unwrap();
				},
			},
		}

		Ok(())
	}
}

impl TypeResolver for Block<'_> {
	fn types(&self) -> &HashMap<Arc<str>, Arc<Type>> {
		self.parent.types()
	}
}

impl BlockRequirements for Block<'_> {
	fn expected_type(&self) -> &Arc<Type> {
		self.parent.expected_type()
	}

	fn values(&self) -> &HashMap<Arc<str>, Value> {
		&self.values
	}

	fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value> {
		&mut self.values
	}
}
