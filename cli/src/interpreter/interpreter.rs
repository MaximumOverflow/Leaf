use std::alloc::Layout;
use std::collections::HashMap;
use std::rc::Rc;

use anyhow::anyhow;
use bytemuck::{bytes_of, from_bytes, Pod};
use paste::paste;

use leaf_compilation::reflection::{Comparison, Const, Function, FunctionBody, Opcode, Type, ValueIdx};

use crate::interpreter::instruction_cache::InstructionCache;
use crate::interpreter::memory::LayoutCache;
use crate::interpreter::stubs::ExternFunctionStub;

pub struct Interpreter<'l> {
	stack: Box<[u8]>,
	layout_cache: Rc<LayoutCache<'l>>,
	instruction_cache: InstructionCache<'l>,
	extern_functions: HashMap<&'l str, Box<dyn Fn(&[u8]) -> Vec<u8>>>,
}

impl<'l> Interpreter<'l> {
	pub fn new() -> Self {
		Self {
			stack: unsafe {
				let layout = Layout::array::<u8>(1000000000).unwrap();
				let ptr = std::alloc::alloc_zeroed(layout);
				if ptr.is_null() {
					panic!("Could not allocate stack buffer");
				}
				let slice = std::slice::from_raw_parts_mut(ptr, layout.size());
				Box::from_raw(slice)
			},
			instruction_cache: InstructionCache::default(),
			layout_cache: Rc::new(LayoutCache::default()),
			extern_functions: Default::default(),
		}
	}

	pub unsafe fn register_extern_fn<T: Pod, R: Pod>(&mut self, path: &'l str, func: impl ExternFunctionStub<T, R>) {
		let stub = move |bytes: &[u8]| -> Vec<u8> {
			let result = func.dyn_call(bytes);
			Vec::from(bytes_of(&result))
		};
		self.extern_functions.insert(path, Box::new(stub));
	}

	#[inline(never)]
	pub fn call_as_main(
		&mut self, function: &'l Function<'l>,
	) -> anyhow::Result<(Vec<u8>, &'l Type<'l>)> {
		let mut stack = std::mem::take(&mut self.stack);
		let result = self.call(function, &mut *stack);
		let bytes = result?.to_vec();
		self.stack = stack;

		Ok((bytes, function.ret_ty()))
	}

	#[inline(never)]
	fn call<'s>(
		&mut self, function: &'l Function<'l>, stack: &'s mut [u8],
	) -> anyhow::Result<&'s [u8]>
	where
		'l: 's,
	{
		let body = function.body().unwrap();
		let (layout, offsets) = self.layout_cache.get_function_stack_layout(function);

		let (stack_frame, stack) = {
			let align_offset = stack.as_ptr().align_offset(layout.align());
			let split_idx = align_offset + layout.size();
			let (stack_frame, stack) = stack.split_at_mut(split_idx);
			(&mut stack_frame[align_offset..], stack)
		};

		println!("{:?} {:?}", layout, offsets);
		let mut pc = 0;
		while let Some(opcode) = body.opcodes().get(pc) {
			macro_rules! impl_bin_op {
				($do: ident, $lhs: expr, $rhs: expr, $dst: expr, $($ty: ident),+) => {
					let lhs = value_bytes(stack_frame, &offsets, body, *$lhs);
					let rhs = value_bytes(stack_frame, &offsets, body, *$rhs);
					debug_assert_eq!(lhs.len(), rhs.len());

					$(paste! { const [<SIZE_OF_ $ty:upper>]: usize = std::mem::size_of::<$ty>(); })*

					match lhs.len() {
						$(
							paste!{ [<SIZE_OF_ $ty:upper>] } => {
								let result = from_bytes::<$ty>(lhs).$do(*from_bytes::<$ty>(rhs));
								let dst = value_bytes_mut(stack_frame, &offsets, *$dst);
								dst[..std::mem::size_of_val(&result)].copy_from_slice(bytes_of(&result));
							},
						)*
						_ => unimplemented!(),
					}
				};
			}

			macro_rules! impl_cmp {
				($do: ident, $lhs: expr, $rhs: expr, $dst: expr, $($ty: ident),+) => {
					let lhs = value_bytes(stack_frame, &offsets, body, *$lhs);
					let rhs = value_bytes(stack_frame, &offsets, body, *$rhs);
					debug_assert_eq!(lhs.len(), rhs.len());

					$(paste! { const [<SIZE_OF_ $ty:upper>]: usize = std::mem::size_of::<$ty>(); })*

					match lhs.len() {
						$(
							paste!{ [<SIZE_OF_ $ty:upper>] } => {
								let result = from_bytes::<$ty>(lhs).$do(from_bytes::<$ty>(rhs));
								let dst = value_bytes_mut(stack_frame, &offsets, *$dst);
								dst[..std::mem::size_of_val(&result)].copy_from_slice(bytes_of(&result));
							},
						)*
						_ => unimplemented!(),
					}
				};
			}

			match opcode {
				Opcode::Nop => {},
				Opcode::Jp(target) => {
					pc = *target;
					continue;
				},
				Opcode::Br(cond, true_case, false_case) => {
					debug_assert_eq!(body.value_type(*cond), Some(&Type::Bool));
					pc = match value_bytes(stack_frame, &offsets, body, *cond)[0] {
						0 => *false_case,
						_ => *true_case,
					};
					continue;
				},
				Opcode::SAdd(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_add, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Lt) => {
					impl_cmp!(lt, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::Store(src, dst) => unsafe {
					let dst = value_bytes_mut(stack_frame, &offsets, *dst);
					let (dst_ptr, dst_size) = (dst.as_mut_ptr(), dst.len());

					let src = value_bytes(stack_frame, &offsets, body, *src);
					let (src_ptr, src_size) = (src.as_ptr(), src.len());

					assert_eq!(dst_size, src_size);
					std::ptr::copy(src_ptr, dst_ptr, dst_size);
				},
				Opcode::Ret(value) => match value {
					Some(value) => {
						assert_eq!(body.value_type(*value), Some(function.ret_ty()));
						return Ok(value_bytes(stack_frame, &offsets, body, *value));
					},
					None => {
						unimplemented!();
					},
				},
				Opcode::Call(func, params, result) => {
					let mut param_bytes = vec![];
					for param in params {
						let bytes = value_bytes(stack_frame, &offsets, body, *param);
						param_bytes.extend_from_slice(bytes);
					}

					if func.body().is_none() {
						let Some(stub) = self.extern_functions.get(func.id()) else {
							panic!("Unregistered external function {:?}", func.id());
						};
						let result_bytes = stub(&param_bytes);
						value_bytes_mut(stack_frame, &offsets, *result).copy_from_slice(&result_bytes);
					}
					else {
						unimplemented!()
					}
				},
				_ => unimplemented!("{:?}", opcode),
			}

			pc += 1;
		}

		Err(anyhow!("Function exited unexpectedly"))
	}
}

fn value_bytes<'s, 'l: 's>(
	stack_frame: &'s [u8], offsets: &[usize], body: &'l FunctionBody<'l>, value: ValueIdx,
) -> &'s [u8] {
	match value {
		ValueIdx::Local(i) => {
			let start = offsets[i];
			let end = offsets.get(i + 1).cloned().unwrap_or(stack_frame.len());

			&stack_frame[start..end]
		},
		ValueIdx::Const(i) => match &body.constants()[i] {
			Const::U8(v) => bytes_of(v),
			Const::U16(v) => bytes_of(v),
			Const::U32(v) => bytes_of(v),
			Const::U64(v) => bytes_of(v),
			Const::I8(v) => bytes_of(v),
			Const::I16(v) => bytes_of(v),
			Const::I32(v) => bytes_of(v),
			Const::I64(v) => bytes_of(v),
			Const::F32(v) => bytes_of(v),
			Const::F64(v) => bytes_of(v),
			Const::Str(v) => unsafe {
				let data = &*(v as *const _ as *const [usize; 2]);
				match data[1] == v.len() {
					true => bytes_of(&data[0]),
					false => bytes_of(&data[1]),
				}
			},
			_ => unimplemented!(),
		},
	}
}

fn value_bytes_mut<'s, 'l: 's>(
	stack_frame: &'s mut [u8], offsets: &[usize], value: ValueIdx,
) -> &'s mut [u8] {
	match value {
		ValueIdx::Local(i) => {
			let start = offsets[i];
			let end = offsets.get(i + 1).cloned().unwrap_or(stack_frame.len());

			&mut stack_frame[start..end]
		},
		ValueIdx::Const(i) => {
			panic!("Cannot borrow constants mutably");
		},
	}
}
