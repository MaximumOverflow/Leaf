use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::{Function, Type};
use leaf_compilation::reflection::structured::types::LeafType;
use crate::interpreter::instruction_cache::InstructionCache;
use crate::interpreter::memory::{Stack, TypeLayoutCache};
use leaf_compilation::reflection::Opcode;
use anyhow::anyhow;
use std::sync::Arc;
use std::rc::Rc;

pub struct Interpreter<'l> {
	stack: Stack<'l>,
	layout_cache: Rc<TypeLayoutCache>,
	instruction_cache: InstructionCache,
}

impl<'l> Interpreter<'l> {
	pub fn new() -> Self {
		let layout_cache = Rc::new(TypeLayoutCache::default());
		let stack = Stack::with_capacity(layout_cache.clone(), 1000000);
		let instruction_cache = InstructionCache::default();
		Self { stack, layout_cache, instruction_cache }
	}

	#[inline(never)]
	pub fn call(&mut self, function: &'l Arc<Function>) -> anyhow::Result<()> {
		// let good_params = params
		// 	.iter()
		// 	.zip(function.parameters())
		// 	.filter(|(v, p)| v.ty() == p.ty())
		// 	.count();

		// if good_params != function.parameters().len() {
		// 	return Err(anyhow!("Invalid function parameters"));
		// }

		let Some(body) = function.body() else {
			return Err(anyhow!("Function has no body"));
		};

		let instructions = self.instruction_cache.get_instructions(function)?;

		let mut locals = Vec::with_capacity(body.locals().len());
		for local in body.locals() {
			let ty = local.ty();
			let elem = self.stack.push_uninit(&ty).0;
			locals.push((ty, elem));
		}

		let mut i = 0;
		while i < instructions.len() {
			let opcode = instructions[i];
			i += 1;

			macro_rules! impl_unary_op {
				($op: tt) => {
					{
						let (lhs_ty) = self.stack.with_elements(|elems| {
							let [.., lhs] = &elems else { panic!("Missing stack elements") };
							lhs.0
						});

						match (lhs_ty.variant()) {
							TypeVariant::Bool => {
								let lhs: bool = *self.stack.pop_ref();
								let res = ($op lhs);
								self.stack.push_value(res);
							}
							TypeVariant::Int32 => {
								let lhs: i32 = *self.stack.pop_ref();
								let res = $op lhs;
								self.stack.push_value(res);
							}
							_ => unimplemented!(),
						}
					}
				};
			}

			macro_rules! impl_binary_op {
				($op: tt) => {
					{
						let (rhs_ty, lhs_ty) = self.stack.with_elements(|elems| {
							let [.., lhs, rhs] = &elems else { panic!("Missing stack elements") };
							(rhs.0, lhs.0)
						});

						match (lhs_ty.variant(), rhs_ty.variant()) {
							(TypeVariant::Int32, TypeVariant::Int32) => {
								let rhs: &i32 = self.stack.pop_ref();
								let lhs: &i32 = self.stack.pop_ref();
								let res = *lhs $op *rhs;
								self.stack.push_value(res);
							}
							_ => unimplemented!(),
						}
					}
				};
			}

			match opcode {
				Opcode::PushInt8(value) => self.push_value(value),
				Opcode::PushInt16(value) => self.push_value(value.0),
				Opcode::PushInt32(value) => self.push_value(value.0),
				Opcode::PushInt64(value) => self.push_value(value.0),
				Opcode::PushUInt8(value) => self.push_value(value),
				Opcode::PushUInt16(value) => self.push_value(value.0),
				Opcode::PushUInt32(value) => self.push_value(value.0),
				Opcode::PushUInt64(value) => self.push_value(value.0),
				Opcode::PushFloat16(value) => self.push_value(value),
				Opcode::PushFloat32(value) => self.push_value(value),
				Opcode::PushFloat64(value) => self.push_value(value),
				Opcode::PushBool(value) => self.push_value(value),

				Opcode::PushLocal0 => self.push_local(&locals[0]),
				Opcode::PushLocal1 => self.push_local(&locals[1]),
				Opcode::PushLocal2 => self.push_local(&locals[2]),
				Opcode::PushLocal3 => self.push_local(&locals[3]),
				Opcode::PushLocal4 => self.push_local(&locals[4]),
				Opcode::PushLocal5 => self.push_local(&locals[5]),
				Opcode::PushLocal6 => self.push_local(&locals[6]),
				Opcode::PushLocal(i) => self.push_local(&locals[i.0]),

				Opcode::PushLocalA0 => self.push_local_address(&locals[0]),
				Opcode::PushLocalA1 => self.push_local_address(&locals[1]),
				Opcode::PushLocalA2 => self.push_local_address(&locals[2]),
				Opcode::PushLocalA3 => self.push_local_address(&locals[3]),
				Opcode::PushLocalA4 => self.push_local_address(&locals[4]),
				Opcode::PushLocalA5 => self.push_local_address(&locals[5]),
				Opcode::PushLocalA6 => self.push_local_address(&locals[6]),
				Opcode::PushLocalA(i) => self.push_local_address(&locals[i.0]),

				Opcode::StoreLocal0 => self.store_local(&locals[0]),
				Opcode::StoreLocal1 => self.store_local(&locals[1]),
				Opcode::StoreLocal2 => self.store_local(&locals[2]),
				Opcode::StoreLocal3 => self.store_local(&locals[3]),
				Opcode::StoreLocal4 => self.store_local(&locals[4]),
				Opcode::StoreLocal5 => self.store_local(&locals[5]),
				Opcode::StoreLocal6 => self.store_local(&locals[6]),
				Opcode::StoreLocal(i) => self.store_local(&locals[i.0]),

				Opcode::Add => impl_binary_op!(+),
				Opcode::Sub => impl_binary_op!(-),
				Opcode::Mul => impl_binary_op!(*),
				Opcode::Div => impl_binary_op!(/),
				Opcode::Mod => impl_binary_op!(%),
				Opcode::Lt => impl_binary_op!(<),
				Opcode::Gt => impl_binary_op!(>),
				Opcode::Le => impl_binary_op!(<=),
				Opcode::Ge => impl_binary_op!(>=),
				Opcode::Eq => impl_binary_op!(==),
				Opcode::Neq => impl_binary_op!(!=),

				Opcode::Neg => impl_unary_op!(!),

				Opcode::Jump(target) => {
					i = target as usize;
				}

				Opcode::CondJump(target) => {
					if *self.stack.pop_ref::<bool>() {
						i = target as usize;
					}
				}

				Opcode::CondJumpN(target) => {
					if !*self.stack.pop_ref::<bool>() {
						i = target as usize;
					}
				}

				Opcode::Ret => {
					let (ty, bytes) = match function.return_ty().variant() {
						TypeVariant::Void => (<()>::leaf_type(), [].as_slice()),
						_ => {
							let (ty, _, bytes) = self.stack.pop();
							(ty, bytes)
						}
					};

					for _ in 0..locals.len() {
						self.stack.pop();
					}

					match bytes.len() < 128 {
						true => {
							let mut buffer = [0u8; 128];
							let buffer = &mut buffer[..bytes.len()];
							buffer.copy_from_slice(bytes);
							self.stack.push(ty, buffer);
						},
						false => {
							let buffer = bytes.to_vec();
							self.stack.push(ty, &buffer);
						}
					}

					return Ok(());
				}

				_ => return Err(anyhow!("Unimplemented opcode {:?}", opcode)),
			}
		}

		Err(anyhow!("Function exited unexpectedly"))
	}

	pub fn stack(&self) -> &Stack<'l> {
		&self.stack
	}

	fn push_value<T: Copy + LeafType>(&mut self, value: T) {
		self.stack.push_value(value);
	}

	fn push_local(&mut self, local: &(&'l Arc<Type>, usize)) {
		self.stack.push_from_element(local.1);
	}

	fn push_local_address(&mut self, local: &(&'l Arc<Type>, usize)) {
		let ptr = self.stack.with_elements(|elems| elems[local.1].2) as usize;
		self.stack.push(local.0.make_ptr(true), bytemuck::bytes_of(&ptr));
	}

	fn store_local(&mut self, local: &(&'l Arc<Type>, usize)) {
		self.stack.pop_into_element(local.1)
	}
}
