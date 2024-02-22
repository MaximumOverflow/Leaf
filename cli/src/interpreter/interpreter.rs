use crate::interpreter::memory::{Stack, TypeLayoutCache, PushValue};
use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::{Function, Type};
use crate::interpreter::instruction_cache::InstructionCache;
use leaf_compilation::reflection::Opcode;
use crate::interpreter::value::Value;
use anyhow::{anyhow, Context};
use core::alloc::Layout;
use std::ops::Range;
use std::sync::Arc;
use std::rc::Rc;

pub struct Interpreter {
	stack: Stack,
	layout_cache: Rc<TypeLayoutCache>,
	instruction_cache: InstructionCache,
}

impl Interpreter {
	pub fn new() -> Self {
		let layout_cache = Rc::new(TypeLayoutCache::default());
		let stack = Stack::with_capacity(layout_cache.clone(), 1000000);
		let instruction_cache = InstructionCache::default();
		Self { stack, layout_cache, instruction_cache }
	}

	#[inline(never)]
	pub fn interpret(&mut self, function: &Arc<Function>, mut params: Vec<Value>) -> anyhow::Result<Value> {
		let good_params = params
			.iter()
			.zip(function.parameters())
			.filter(|(v, p)| v.ty() == p.ty())
			.count();

		if good_params != function.parameters().len() {
			return Err(anyhow!("Invalid function parameters"));
		}

		let Some(body) = function.body() else {
			return Err(anyhow!("Function has no body"));
		};

		let instructions = self.instruction_cache.get_instructions(function)?;

		let mut locals = Vec::with_capacity(body.locals().len());
		for local in body.locals() {
			let ty = local.ty().clone();
			let layout = self.layout_cache.get_layout(&ty);
			let range = self.stack.push(&ty, layout, &[])?;
			locals.push((ty, layout, range));
		}

		let mut i = 0;
		while i < instructions.len() {
			let opcode = instructions[i];
			i += 1;

			macro_rules! impl_unary_op {
				($op: tt) => {
					{
						let mut lhs = [0u8; 8];
						let (lhs, lhs_ty) = {
							let size = self.stack.peek().context("Stack is empty")?.1;
							let lhs = &mut lhs[..size];
							let ty = self.stack.pop(lhs)?;
							(lhs, ty)
						};

						match (lhs_ty.as_ref().as_ref()) {
							TypeVariant::Bool => {
								let lhs: bool = bytemuck::pod_read_unaligned::<u8>(&lhs) != 0;
								let res = ($op lhs) as u8;
								self.stack.push(&lhs_ty, Layout::new::<bool>(), bytemuck::bytes_of(&res))?;
							}
							TypeVariant::Int32 => {
								let lhs: i32 = bytemuck::pod_read_unaligned(&lhs);
								let res = $op lhs;
								self.stack.push(&lhs_ty, Layout::new::<i32>(), bytemuck::bytes_of(&res))?;
							}
							_ => unimplemented!(),
						}
					}
				};
			}

			macro_rules! impl_binary_op {
				($op: tt) => {
					{
						let mut rhs = [0u8; 8];
						let (rhs, rhs_ty) = {
							let size = self.stack.peek().context("Stack is empty")?.1;
							let rhs = &mut rhs[..size];
							let ty = self.stack.pop(rhs)?;
							(rhs, ty)
						};

						let mut lhs = [0u8; 8];
						let (lhs, lhs_ty) = {
							let size = self.stack.peek().context("Stack is empty")?.1;
							let lhs = &mut lhs[..size];
							let ty = self.stack.pop(lhs)?;
							(lhs, ty)
						};

						match (lhs_ty.as_ref().as_ref(), rhs_ty.as_ref().as_ref()) {
							(TypeVariant::Int32, TypeVariant::Int32) => {
								let lhs: i32 = bytemuck::pod_read_unaligned(&lhs);
								let rhs: i32 = bytemuck::pod_read_unaligned(&rhs);
								let res = lhs $op rhs;
								self.stack.push(&lhs_ty, Layout::new::<u32>(), bytemuck::bytes_of(&res))?;
							}
							_ => unimplemented!(),
						}
					}
				};
			}

			macro_rules! impl_binary_op_bool {
				($op: tt) => {
					{
						let mut rhs = [0u8; 8];
						let (rhs, rhs_ty) = {
							let size = self.stack.peek().context("Stack is empty")?.1;
							let rhs = &mut rhs[..size];
							let ty = self.stack.pop(rhs)?;
							(rhs, ty)
						};

						let mut lhs = [0u8; 8];
						let (lhs, lhs_ty) = {
							let size = self.stack.peek().context("Stack is empty")?.1;
							let lhs = &mut lhs[..size];
							let ty = self.stack.pop(lhs)?;
							(lhs, ty)
						};

						match (lhs_ty.as_ref().as_ref(), rhs_ty.as_ref().as_ref()) {
							(TypeVariant::Int32, TypeVariant::Int32) => {
								let lhs: i32 = bytemuck::pod_read_unaligned(&lhs);
								let rhs: i32 = bytemuck::pod_read_unaligned(&rhs);
								let res = lhs $op rhs;
								self.stack.push(Type::bool(), Layout::new::<bool>(), bytemuck::bytes_of(&res))?;
							}
							_ => unimplemented!(),
						}
					}
				};
			}

			match opcode {
				Opcode::PushInt8(value) => self.stack.push_value_discard(value)?,
				Opcode::PushInt16(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushInt32(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushInt64(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushUInt8(value) => self.stack.push_value_discard(value)?,
				Opcode::PushUInt16(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushUInt32(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushUInt64(value) => self.stack.push_value_discard(value.0)?,
				Opcode::PushFloat16(value) => self.stack.push_value_discard(value)?,
				Opcode::PushFloat32(value) => self.stack.push_value_discard(value)?,
				Opcode::PushFloat64(value) => self.stack.push_value_discard(value)?,
				Opcode::PushBool(value) => self.stack.push_value_discard(value)?,

				Opcode::PushLocal0 => self.push_local(locals.get(0))?,
				Opcode::PushLocal1 => self.push_local(locals.get(1))?,
				Opcode::PushLocal2 => self.push_local(locals.get(2))?,
				Opcode::PushLocal3 => self.push_local(locals.get(3))?,
				Opcode::PushLocal4 => self.push_local(locals.get(4))?,
				Opcode::PushLocal5 => self.push_local(locals.get(5))?,
				Opcode::PushLocal6 => self.push_local(locals.get(6))?,
				Opcode::PushLocal(i) => self.push_local(locals.get(i.0))?,

				Opcode::PushLocalA0 => self.push_local_address(locals.get(0))?,
				Opcode::PushLocalA1 => self.push_local_address(locals.get(1))?,
				Opcode::PushLocalA2 => self.push_local_address(locals.get(2))?,
				Opcode::PushLocalA3 => self.push_local_address(locals.get(3))?,
				Opcode::PushLocalA4 => self.push_local_address(locals.get(4))?,
				Opcode::PushLocalA5 => self.push_local_address(locals.get(5))?,
				Opcode::PushLocalA6 => self.push_local_address(locals.get(6))?,
				Opcode::PushLocalA(i) => self.push_local_address(locals.get(i.0))?,

				Opcode::StoreLocal0 => self.store_local(locals.get(0))?,
				Opcode::StoreLocal1 => self.store_local(locals.get(1))?,
				Opcode::StoreLocal2 => self.store_local(locals.get(2))?,
				Opcode::StoreLocal3 => self.store_local(locals.get(3))?,
				Opcode::StoreLocal4 => self.store_local(locals.get(4))?,
				Opcode::StoreLocal5 => self.store_local(locals.get(5))?,
				Opcode::StoreLocal6 => self.store_local(locals.get(6))?,
				Opcode::StoreLocal(i) => self.store_local(locals.get(i.0))?,

				Opcode::StoreField0 => self.store_field(0)?,
				Opcode::StoreField1 => self.store_field(1)?,
				Opcode::StoreField2 => self.store_field(2)?,
				Opcode::StoreField3 => self.store_field(3)?,
				Opcode::StoreField4 => self.store_field(4)?,
				Opcode::StoreField5 => self.store_field(5)?,
				Opcode::StoreField6 => self.store_field(6)?,
				Opcode::StoreField(i) => self.store_field(i.0)?,

				Opcode::Add => impl_binary_op!(+),
				Opcode::Sub => impl_binary_op!(-),
				Opcode::Mul => impl_binary_op!(*),
				Opcode::Div => impl_binary_op!(/),
				Opcode::Mod => impl_binary_op!(%),
				Opcode::Lt => impl_binary_op_bool!(<),
				Opcode::Neg => impl_unary_op!(!),

				Opcode::Jump(target) => {
					i = target as usize;
				}

				Opcode::ConditionalJump(target) => {
					let mut value = 0u8;
					let _ = self.stack.pop(bytemuck::bytes_of_mut(&mut value))?;
					if value != 0 {
						i = target as usize;
					}
				}

				Opcode::Branch(t0, t1) => {
					let mut value = 0u8;
					let _ = self.stack.pop(bytemuck::bytes_of_mut(&mut value))?;
					i = match value != 0 {
						true => t0 as usize,
						false => t1 as usize,
					}
				}

				Opcode::Ret => {
					let value = match function.return_ty().as_ref().as_ref() {
						TypeVariant::Void => Value::void(),
						_ => unsafe {
							let ret_ty = function.return_ty();
							let layout = self.layout_cache.get_layout(ret_ty);
							let mut value = Value::new_uninit(ret_ty, layout)?;
							let ty = value.init(|buffer| self.stack.pop(buffer))?;
							if ty != *ret_ty {
								return Err(anyhow!("Invalid return value. Expected type {}, found {}", ret_ty, ty));
							}
							value
						}
					};

					for _ in 0..locals.len() {
						self.stack.pop_ignore_value()?;
					}

					return Ok(value);
				}

				_ => return Err(anyhow!("Unimplemented opcode {:?}", opcode)),
			}
		}

		Err(anyhow!("Function exited unexpectedly"))
	}

	pub fn stack(&self) -> &Stack {
		&self.stack
	}

	fn push_local(&mut self, local: Option<&(Arc<Type>, Layout, Range<usize>)>) -> anyhow::Result<()> {
		let Some((ty, layout, range)) = local else {
			return Err(anyhow!("Invalid local"));
		};

		self.stack.push_inner(ty, *layout, range.clone())?;
		Ok(())
	}

	fn push_local_address(&mut self, local: Option<&(Arc<Type>, Layout, Range<usize>)>) -> anyhow::Result<()> {
		let Some((ty, _, range)) = local else {
			return Err(anyhow!("Invalid local"));
		};

		unsafe {
			let ptr = self.stack.as_bytes().as_ptr().add(range.start) as usize;
			self.stack.push(ty.make_ptr(true), Layout::new::<*const u8>(), bytemuck::bytes_of(&ptr))?;
		}

		Ok(())
	}

	pub fn store_local(&mut self, local: Option<&(Arc<Type>, Layout, Range<usize>)>) -> anyhow::Result<()> {
		let Some((ty, _, range)) = local else {
			return Err(anyhow!("Invalid local"));
		};
		self.stack.pop_inner(ty, range.clone())
	}

	fn store_field(&mut self, field: usize) -> anyhow::Result<()> {
		let mut ptr = 0usize;

		let ty = self.stack.pop(bytemuck::bytes_of_mut(&mut ptr))?;
		let ty = match ty.as_ref().as_ref() {
			TypeVariant::Pointer(ty, _) => ty.upgrade().unwrap(),
			_ => return Err(anyhow!("Invalid field or type")),
		};

		let Some((offset, layout, fld_ty)) = self.layout_cache.get_field_offset_and_layout(&ty, field) else {
			return Err(anyhow!("Invalid field or type"));
		};

		self.stack.pop_inner(&fld_ty, offset..offset + layout.size())
	}
}
