use std::collections::HashMap;
use std::alloc::Layout;
use std::mem::{size_of, transmute};
use std::rc::Rc;

use anyhow::anyhow;
use bytemuck::bytes_of;
use paste::paste;
use tracing::{trace, trace_span};

use leaf_compilation::reflection::{
	Comparison, Function, FunctionBody, Opcode, Type, UniqueIdentifier, Value, ValueIdx,
};

use crate::interpreter::memory::LayoutCache;
use crate::interpreter::stubs::ExternFunctionStub;

pub struct Interpreter<'l> {
	stack: Box<[u8]>,
	layout_cache: Rc<LayoutCache<'l>>,
	extern_functions: HashMap<UniqueIdentifier<'l>, Box<dyn Fn(&[u8]) -> Vec<u8>>>,
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
			layout_cache: Rc::new(LayoutCache::default()),
			extern_functions: Default::default(),
		}
	}

	pub unsafe fn register_extern_fn<T: Copy, R: Copy>(
		&mut self,
		path: &'l str,
		func: impl ExternFunctionStub<T, R>,
	) {
		let stub = move |bytes: &[u8]| -> Vec<u8> {
			let result = func.dyn_call(bytes);
			let ptr = &result as *const R as *const u8;
			Vec::from(std::slice::from_raw_parts(ptr, size_of::<R>()))
		};

		let (namespace, name) = path.rsplit_once('/').unwrap_or(("", path));

		let id = UniqueIdentifier::new(name, namespace);
		self.extern_functions.insert(id, Box::new(stub));
	}

	#[inline(never)]
	pub fn call_as_main(
		&mut self,
		function: &'l Function<'l>,
	) -> anyhow::Result<(Vec<u8>, &'l Type<'l>)> {
		let mut stack = std::mem::take(&mut self.stack);
		let result = self.call(function, &mut *stack);
		let bytes = result?.to_vec();
		self.stack = stack;

		Ok((bytes, function.ret_ty()))
	}

	#[inline(never)]
	fn call<'s>(
		&mut self,
		function: &'l Function<'l>,
		stack: &'s mut [u8],
	) -> anyhow::Result<&'s [u8]>
	where
		'l: 's,
	{
		let scope = trace_span!(
			"call",
			namespace = function.namespace(),
			name = function.name()
		);
		let _scope = scope.enter();

		let body = function.body().unwrap();
		let (layout, offsets) = self.layout_cache.get_function_stack_layout(function);

		let (stack_frame, stack) = {
			let align_offset = stack.as_ptr().align_offset(layout.align());
			let (_, stack) = stack.split_at_mut(align_offset);
			stack.split_at_mut(layout.size())
		};

		trace!("{}: {:?} {:X?}", function.id(), layout, offsets);
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
								let result = bytemuck::pod_read_unaligned::<$ty>(lhs).$do(bytemuck::pod_read_unaligned::<$ty>(rhs));
								let dst = value_bytes_mut(stack_frame, &offsets, *$dst);
								dst[..std::mem::size_of_val(&result)].copy_from_slice(bytes_of(&result));
							}
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
								let result = bytemuck::pod_read_unaligned::<$ty>(lhs).$do(&bytemuck::pod_read_unaligned::<$ty>(rhs));
								let dst = value_bytes_mut(stack_frame, &offsets, *$dst);
								dst[..std::mem::size_of_val(&result)].copy_from_slice(bytes_of(&result));
							}
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
				Opcode::SSub(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_sub, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SMul(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_mul, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SDiv(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_div, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SMod(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_rem, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Eq) => {
					impl_cmp!(eq, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Ne) => {
					impl_cmp!(ne, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Lt) => {
					impl_cmp!(lt, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Gt) => {
					impl_cmp!(gt, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Le) => {
					impl_cmp!(le, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::SCmp(lhs, rhs, dst, Comparison::Ge) => {
					impl_cmp!(ge, lhs, rhs, dst, i8, i16, i32, i64);
				},
				Opcode::UAdd(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_add, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::USub(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_sub, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UMul(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_mul, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UDiv(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_div, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UMod(lhs, rhs, dst) => {
					impl_bin_op!(wrapping_rem, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Eq) => {
					impl_cmp!(eq, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Ne) => {
					impl_cmp!(ne, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Lt) => {
					impl_cmp!(lt, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Gt) => {
					impl_cmp!(gt, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Le) => {
					impl_cmp!(le, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::UCmp(lhs, rhs, dst, Comparison::Ge) => {
					impl_cmp!(ge, lhs, rhs, dst, u8, u16, u32, u64);
				},
				Opcode::LNot(val, dst) => {
					let val = value_bytes(stack_frame, &offsets, body, *val);
					let result = val[0] == 0;
					let dst = value_bytes_mut(stack_frame, &offsets, *dst);
					dst[0] = result as u8;
				},

				Opcode::Store(src, dst) => unsafe {
					let dst = value_bytes_mut(stack_frame, &offsets, *dst);
					let (dst_ptr, dst_size) = (dst.as_mut_ptr(), dst.len());

					let src = value_bytes(stack_frame, &offsets, body, *src);
					let (src_ptr, src_size) = (src.as_ptr(), src.len());

					assert_eq!(dst_size, src_size);
					std::ptr::copy(src_ptr, dst_ptr, dst_size);
				},
				Opcode::StoreA(val, dst) => {
					let ptr = value_bytes(stack_frame, &offsets, body, *val).as_ptr() as usize;
					let dst = value_bytes_mut(stack_frame, &offsets, *dst);
					dst.copy_from_slice(bytes_of(&ptr));
				},
				Opcode::Aggregate(values, dst) => {
					let (bytes, _) = push_values(
						stack_frame,
						stack,
						&offsets,
						body,
						&self.layout_cache,
						values,
					);

					let dst = value_bytes_mut(stack_frame, &offsets, *dst);
					dst.copy_from_slice(bytes);
				},

				Opcode::Ret(value) => match value {
					Some(value) => {
						assert_eq!(body.value_type(*value), Some(function.ret_ty()));
						return Ok(value_bytes(stack_frame, &offsets, body, *value));
					},
					None => {
						assert_eq!(function.ret_ty(), &Type::Void);
						return Ok(&[]);
					},
				},
				Opcode::Call(func, params, result) => {
					let (call_frame, _) = push_values(
						stack_frame,
						stack,
						&offsets,
						body,
						&self.layout_cache,
						params,
					);

					if func.body().is_none() {
						let Some(stub) = self.extern_functions.get(&func.id()) else {
							panic!("Unregistered external function {:?}", func.id());
						};

						let result_bytes = trace_span!(
							"call_native",
							namespace = func.namespace(),
							name = func.name()
						)
						.in_scope(|| {
							trace!("{}", function.id());
							stub(call_frame)
						});
						if let Some(result) = result {
							value_bytes_mut(stack_frame, &offsets, *result)
								.copy_from_slice(&result_bytes);
						}
					} else {
						let result_bytes = self.call(func, stack)?;
						if let Some(result) = result {
							value_bytes_mut(stack_frame, &offsets, *result)
								.copy_from_slice(&result_bytes);
						}
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
	stack_frame: &'s [u8],
	offsets: &[[usize; 2]],
	body: &'l FunctionBody<'l>,
	value: ValueIdx,
) -> &'s [u8] {
	match &body.values()[value.0] {
		Value {
			const_data: Some(data),
			ty: Type::Pointer {
				mutable: false,
				ty: &Type::UInt8,
			},
		} => unsafe {
			let ptr_data: &[usize; 2] = transmute(data);
			bytes_of(&ptr_data[(ptr_data[0] == data.len()) as usize])
		},
		Value {
			const_data: Some(data),
			..
		} => data,
		Value {
			const_data: None, ..
		} => {
			let [start, size] = offsets[value.0];
			let end = start + size;
			&stack_frame[start..end]
		},
	}
}

fn value_bytes_mut<'s, 'l: 's>(
	stack_frame: &'s mut [u8],
	offsets: &[[usize; 2]],
	value: ValueIdx,
) -> &'s mut [u8] {
	let [start, size] = offsets[value.0];
	let end = start + size;
	&mut stack_frame[start..end]
}

fn push_values<'s, 'l: 's>(
	stack_frame: &[u8],
	stack: &'s mut [u8],
	offsets: &[[usize; 2]],
	body: &'l FunctionBody<'l>,
	layout_cache: &LayoutCache<'l>,
	values: &[ValueIdx],
) -> (&'s [u8], &'s mut [u8]) {
	unsafe {
		let mut offset = 0;
		for value in values {
			let ty = body.value_type(*value).unwrap();
			let ty_layout = layout_cache.get_type_layout(ty);
			let ptr = stack.as_ptr().add(offset);
			let align_offset = ptr.align_offset(ty_layout.align());
			let bytes = value_bytes(stack_frame, &offsets, body, *value);
			let start = offset + align_offset;
			offset = start + ty_layout.size();
			stack[start..offset].copy_from_slice(bytes);
		}

		let align = match values.get(0) {
			None => 0,
			Some(param) => {
				let ty = body.value_type(*param).unwrap();
				let ty_layout = layout_cache.get_type_layout(ty);
				let ptr = stack.as_ptr();
				ptr.align_offset(ty_layout.align())
			},
		};

		let (stack_frame, stack) = stack.split_at_mut(offset);
		let stack_frame = &stack_frame[align..];
		(stack_frame, stack)
	}
}
