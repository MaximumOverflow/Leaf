use std::collections::HashMap;
use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use fxhash::FxHashMap;

use inkwell::{AddressSpace, IntPredicate};
use inkwell::module::Module;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum};
use tracing::debug;

use leaf_reflection::{
	Assembly, Comparison, ConstantRef, Function, OpCode, Type, ValueIndex, ValueIndexKind,
};
use crate::backends::CompilationBackend;
use crate::utilities::name_mangling::MangleCpp;

pub type LLVMContext = inkwell::context::Context;
pub type OptimizationLevel = inkwell::OptimizationLevel;

#[allow(non_camel_case_types)]
pub struct LLVM_Backend<'l, 'a> {
	context: &'l LLVMContext,
	modules: RefCell<HashMap<&'a str, Rc<Module<'l>>>>,
	types: RefCell<HashMap<&'a Type<'a>, AnyTypeEnum<'l>>>,
}

impl<'l> LLVM_Backend<'l, '_> {
	pub fn new(llvm_context: &'l LLVMContext, _: OptimizationLevel) -> Self {
		Self {
			context: llvm_context,
			types: Default::default(),
			modules: Default::default(),
		}
	}
}

impl<'l, 'a: 'l> CompilationBackend<'l, 'a> for LLVM_Backend<'l, 'a> {
	type Output = Rc<Module<'l>>;
	fn compile(&self, assembly: &'a Assembly<'a>) -> Result<Self::Output, Box<dyn Error>> {
		let module: Rc<Module<'l>> = self.context.create_module(assembly.name()).into();

		for func in assembly.functions() {
			let func_ty = self.get_fn_type(func);
			match func.body().is_none() || func.name() == "main" {
				true => module.add_function(func.name(), func_ty, None),
				false => module.add_function(&func.mangled(()), func_ty, None),
			};
		}

		for func in assembly.functions() {
			self.compile_function(&module, func);
		}

		let mut modules = self.modules.borrow_mut();
		modules.insert(assembly.name(), module.clone());
		Ok(module)
	}
}

impl<'l, 'a> LLVM_Backend<'l, 'a> {
	fn compile_function(&self, module: &Module<'l>, func: &Function<'a>) {
		debug!("Compiling function `{}`", func.id());
		let llvm_func = match func.body().is_none() || func.name() == "main" {
			true => module.get_function(func.name()).unwrap(),
			false => module.get_function(&func.mangled(())).unwrap(),
		};
		let Some(body) = func.body() else { return };

		let mut blocks = FxHashMap::default();
		blocks.insert(
			ValueIndex::local(0),
			self.context.append_basic_block(llvm_func, "IR_0000"),
		);
		for (i, opcode) in body.opcodes().iter().enumerate() {
			match opcode {
				OpCode::Jp { target } => {
					blocks.entry(*target).or_insert_with(|| {
						let name = format!("IR_{:04}", target.index());
						self.context.append_basic_block(llvm_func, &name)
					});
					if i < body.opcodes().len() - 1 {
						blocks.entry(ValueIndex::local(i + 1)).or_insert_with(|| {
							let name = format!("IR_{:04}", i + 1);
							self.context.append_basic_block(llvm_func, &name)
						});
					}
				},
				OpCode::Br {
					true_case,
					false_case,
					..
				} => {
					blocks.entry(*true_case).or_insert_with(|| {
						let name = format!("IR_{:04}", true_case.index());
						self.context.append_basic_block(llvm_func, &name)
					});
					blocks.entry(*false_case).or_insert_with(|| {
						let name = format!("IR_{:04}", false_case.index());
						self.context.append_basic_block(llvm_func, &name)
					});
					if i < body.opcodes().len() - 1 {
						blocks.entry(ValueIndex::local(i + 1)).or_insert_with(|| {
							let name = format!("IR_{:04}", i + 1);
							self.context.append_basic_block(llvm_func, &name)
						});
					}
				},
				OpCode::Ret { .. } if i < body.opcodes().len() - 1 => {
					blocks.entry(ValueIndex::local(i + 1)).or_insert_with(|| {
						let name = format!("IR_{:04}", i + 1);
						self.context.append_basic_block(llvm_func, &name)
					});
				},
				_ => {},
			}
		}

		let mut values = FxHashMap::default();
		let mut value_types = FxHashMap::default();
		for (i, param) in func.params().iter().enumerate() {
			let p = llvm_func.get_nth_param(i as u32).unwrap();
			values.insert(ValueIndex::parameter(i), p);
			value_types.insert(ValueIndex::parameter(i), *param.ty());
		}
		for (i, const_ref) in body.referenced_constants().iter().enumerate() {
			values.insert(
				ValueIndex::constant(i),
				self.get_constant(const_ref, module),
			);
			value_types.insert(ValueIndex::constant(i), *const_ref.ty());
		}

		let mut builder = self.context.create_builder();
		for (i, opcode) in body.opcodes().iter().enumerate() {
			if let Some(block) = blocks.get(&ValueIndex::local(i)) {
				if let Some(current) = builder.get_insert_block() {
					if current.get_terminator().is_none() {
						builder.build_unconditional_branch(*block).unwrap();
					}
				}
				builder.position_at_end(*block);
			};

			match opcode {
				OpCode::Alloca { ty } => {
					let llvm_ty = self.get_basic_type(ty);
					let val = builder.build_alloca(llvm_ty, "").unwrap();
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), Type::Pointer { mutable: true, ty });
				},

				| OpCode::Add { lhs, rhs }
				| OpCode::Sub { lhs, rhs }
				| OpCode::Mul { lhs, rhs } => {
					let ty = value_types[lhs];
					let lhs = values[lhs];
					let rhs = values[rhs];
					let val = match ty {
						Type::Int32 | Type::UInt32 => {
							let lhs = lhs.into_int_value();
							let rhs = rhs.into_int_value();
							match opcode {
								OpCode::Add { .. } => builder.build_int_add(lhs, rhs, "").unwrap(),
								OpCode::Sub { .. } => builder.build_int_sub(lhs, rhs, "").unwrap(),
								OpCode::Mul { .. } => builder.build_int_mul(lhs, rhs, "").unwrap(),
								_ => unreachable!(),
							}
						},
						_ => unimplemented!("{:?}", ty),
					};
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), ty);
				},
				OpCode::Div { lhs, rhs } | OpCode::Rem { lhs, rhs } => {
					let ty = value_types[lhs];
					let lhs = values[lhs];
					let rhs = values[rhs];
					let val = match ty {
						Type::Int32 => {
							let lhs = lhs.into_int_value();
							let rhs = rhs.into_int_value();
							match opcode {
								OpCode::Div { .. } => {
									builder.build_int_signed_div(lhs, rhs, "").unwrap()
								},
								OpCode::Rem { .. } => {
									builder.build_int_signed_rem(lhs, rhs, "").unwrap()
								},
								_ => unreachable!(),
							}
						},
						Type::UInt32 => {
							let lhs = lhs.into_int_value();
							let rhs = rhs.into_int_value();
							match opcode {
								OpCode::Div { .. } => {
									builder.build_int_unsigned_div(lhs, rhs, "").unwrap()
								},
								OpCode::Rem { .. } => {
									builder.build_int_unsigned_rem(lhs, rhs, "").unwrap()
								},
								_ => unreachable!(),
							}
						},
						_ => unimplemented!("{:?}", ty),
					};
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), ty);
				},
				OpCode::Cmp { lhs, rhs, cmp } => {
					let ty = value_types[lhs];
					let lhs = values[lhs];
					let rhs = values[rhs];
					let val = match ty {
						Type::Int32 => builder
							.build_int_compare(
								match cmp {
									Comparison::Eq => IntPredicate::EQ,
									Comparison::Ne => IntPredicate::NE,
									Comparison::Lt => IntPredicate::SLT,
									Comparison::Gt => IntPredicate::SGT,
									Comparison::Le => IntPredicate::SLE,
									Comparison::Ge => IntPredicate::SGE,
								},
								lhs.into_int_value(),
								rhs.into_int_value(),
								"",
							)
							.unwrap(),

						Type::UInt32 => builder
							.build_int_compare(
								match cmp {
									Comparison::Eq => IntPredicate::EQ,
									Comparison::Ne => IntPredicate::NE,
									Comparison::Lt => IntPredicate::ULT,
									Comparison::Gt => IntPredicate::UGT,
									Comparison::Le => IntPredicate::ULE,
									Comparison::Ge => IntPredicate::UGE,
								},
								lhs.into_int_value(),
								rhs.into_int_value(),
								"",
							)
							.unwrap(),
						_ => unimplemented!("{:?}", ty),
					};

					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), Type::Bool);
				},
				OpCode::Trunc { ty, value } => {
					let val = values[value].into_int_value();
					let int_ty = self.get_basic_type(ty).into_int_type();
					let val = builder.build_int_truncate(val, int_ty, "").unwrap();
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), **ty);
				},

				OpCode::Load { value } => {
					let ty = value_types[value];
					let (llvm_ty, ty) = match ty {
						Type::Pointer { ty, .. } => (self.get_basic_type(ty), ty),
						_ => unimplemented!("{:?}", ty),
					};
					let ptr = values[value].into_pointer_value();
					let val = builder.build_load(llvm_ty, ptr, "").unwrap();
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), *ty);
				},
				OpCode::Store { val, dst } => {
					builder.build_store(values[dst].into_pointer_value(), values[val]).unwrap();
				},
				OpCode::GEP { val, idx } => {
					let ty = value_types[val];
					match ty {
						| Type::Pointer {
							ty: Type::Array { ty: base, .. },
							mutable,
						}
						| Type::Reference {
							ty: Type::Array { ty: base, .. },
							mutable,
						} => unsafe {
							let idx = values[idx].into_int_value();
							let ptr = values[val].into_pointer_value();
							let base_llvm = self.get_basic_type(base);
							let val = builder.build_gep(base_llvm, ptr, &[idx], "").unwrap();
							values.insert(ValueIndex::local(i), val.as_basic_value_enum());
							value_types
								.insert(ValueIndex::local(i), Type::Pointer { mutable, ty: base });
						},
						_ => unimplemented!("{:?}", ty),
					};
				},

				OpCode::Call { func, params } => match func.kind() {
					ValueIndexKind::Function => {
						let func = body.referenced_functions()[func.index()];
						let llvm_func = match func.body().is_none() || func.name() == "main" {
							true => module.get_function(func.name()).unwrap(),
							false => module.get_function(&func.mangled(())).unwrap(),
						};
						let args: Vec<_> = params.iter().map(|p| values[p].into()).collect();
						let val = builder.build_call(llvm_func, &args, "").unwrap();
						if llvm_func.get_type().get_return_type().is_some() {
							let val = match val.as_any_value_enum() {
								AnyValueEnum::ArrayValue(v) => v.as_basic_value_enum(),
								AnyValueEnum::IntValue(v) => v.as_basic_value_enum(),
								AnyValueEnum::FloatValue(v) => v.as_basic_value_enum(),
								AnyValueEnum::PointerValue(v) => v.as_basic_value_enum(),
								AnyValueEnum::StructValue(v) => v.as_basic_value_enum(),
								AnyValueEnum::VectorValue(v) => v.as_basic_value_enum(),
								_ => unreachable!(),
							};
							values.insert(ValueIndex::local(i), val);
							value_types.insert(ValueIndex::local(i), *func.ret_ty());
						}
					},
					_ => unimplemented!("{:?}", func),
				},

				OpCode::Ret { value: None } => {
					builder.build_return(None).unwrap();
				},
				OpCode::Ret { value: Some(value) } => {
					builder.build_return(Some(&values[value])).unwrap();
				},
				OpCode::Jp { target } => {
					builder.build_unconditional_branch(blocks[target]).unwrap();
				},
				OpCode::Br {
					condition,
					true_case,
					false_case,
				} => {
					let true_case = blocks[true_case];
					let false_case = blocks[false_case];
					let condition = values[condition].into_int_value();
					builder.build_conditional_branch(condition, true_case, false_case).unwrap();
				},
				_ => {
					llvm_func.print_to_stderr();
					unimplemented!("{:?}", opcode)
				},
			}
		}
	}

	fn get_type(&self, ty: &'a Type<'a>) -> AnyTypeEnum<'l> {
		match ty {
			Type::Void => self.context.void_type().as_any_type_enum(),
			Type::Int32 => self.context.i32_type().into(),
			Type::UInt8 => self.context.i8_type().into(),
			Type::UInt32 => self.context.i32_type().into(),
			Type::Struct(data) => {
				let mut types = self.types.borrow_mut();
				if let Some(ty) = types.get(ty) {
					return *ty;
				};

				let llvm_ty = self.context.opaque_struct_type(&ty.id().to_string());
				types.insert(ty, llvm_ty.into());
				drop(types);

				let fields: Vec<_> =
					data.fields().iter().map(|f| self.get_basic_type(f.ty())).collect();
				llvm_ty.set_body(&fields, false);
				llvm_ty.into()
			},
			Type::Pointer { ty: Type::Void, .. } => {
				let base = self.context.i8_type();
				base.ptr_type(AddressSpace::default()).as_any_type_enum()
			},
			Type::Pointer { ty, .. } => {
				let base = self.get_basic_type(ty);
				base.ptr_type(AddressSpace::default()).as_any_type_enum()
			},
			Type::Array { ty, count } => {
				let base = self.get_basic_type(ty);
				base.array_type(*count as u32).as_any_type_enum()
			},
			_ => unimplemented!("{}", ty),
		}
	}

	fn get_basic_type(&self, ty: &'a Type<'a>) -> BasicTypeEnum<'l> {
		as_basic_value_enum(self.get_type(ty))
	}

	fn get_fn_type(&self, func: &Function<'a>) -> FunctionType<'l> {
		let param_tys: Vec<_> = func
			.params()
			.iter()
			.map(|p| self.get_basic_type(p.ty()).try_into().unwrap())
			.collect();

		match func.ret_ty() {
			Type::Void => self.context.void_type().fn_type(&param_tys, false),
			_ => self.get_basic_type(func.ret_ty()).fn_type(&param_tys, false),
		}
	}

	fn get_constant(&self, const_ref: &ConstantRef, module: &Module<'l>) -> BasicValueEnum<'l> {
		match const_ref.ty() {
			Type::Int32 => self
				.context
				.i32_type()
				.const_int(
					i32::from_le_bytes(const_ref.data().try_into().unwrap()) as u64,
					true,
				)
				.as_basic_value_enum(),

			Type::UInt8 => self
				.context
				.i8_type()
				.const_int(const_ref.data()[0] as u64, false)
				.as_basic_value_enum(),

			Type::UInt32 => self
				.context
				.i32_type()
				.const_int(
					u32::from_le_bytes(const_ref.data().try_into().unwrap()) as u64,
					false,
				)
				.as_basic_value_enum(),

			Type::Pointer {
				mutable: false,
				ty: Type::UInt8,
			} => {
				let name = format!("blob_{:?}", const_ref.data().as_ptr());
				if let Some(blob) = module.get_global(&name) {
					return blob.as_basic_value_enum();
				};

				let ty = self.context.i8_type();
				let values: Vec<_> =
					const_ref.data().iter().map(|b| ty.const_int(*b as u64, false)).collect();

				let val = ty.const_array(&values);
				let blob = module.add_global(val.get_type(), None, &name);
				blob.set_initializer(&val);
				blob.as_pointer_value().as_basic_value_enum()
			},
			_ => unimplemented!("{}", const_ref.ty()),
		}
	}
}

fn as_basic_value_enum(ty: AnyTypeEnum) -> BasicTypeEnum {
	match ty {
		AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
		AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
		AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
		AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
		AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
		AnyTypeEnum::VectorType(ty) => ty.as_basic_type_enum(),
		_ => unreachable!(),
	}
}
