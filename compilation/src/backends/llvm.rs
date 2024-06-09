use std::collections::HashMap;
use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use fxhash::FxHashMap;

use inkwell::{IntPredicate};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum};
use tracing::debug;

use leaf_reflection::{Assembly, Comparison, ConstantRef, Function, OpCode, Type, ValueIndex};
use crate::backends::CompilationBackend;
use crate::utilities::name_mangling::MangleCpp;

pub type LLVMContext = inkwell::context::Context;
pub type OptimizationLevel = inkwell::OptimizationLevel;

#[allow(non_camel_case_types)]
pub struct LLVM_Backend<'l, 'a> {
	context: &'l LLVMContext,
	modules: RefCell<HashMap<&'a str, Rc<Module<'l>>>>,
	types: RefCell<HashMap<&'a Type<'a>, BasicTypeEnum<'l>>>,
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
		let func_ty = self.get_fn_type(func);
		let llvm_func = match func.name() {
			"main" => module.add_function("main", func_ty, None),
			_ => module.add_function(&func.mangled(()), func_ty, None),
		};
		let Some(body) = func.body() else { return };

		let mut blocks = FxHashMap::default();
		blocks.insert(
			ValueIndex::local(0),
			self.context.append_basic_block(llvm_func, "IR_0000"),
		);
		for opcode in body.opcodes() {
			match opcode {
				OpCode::Jp { target } => {
					blocks.entry(*target).or_insert_with(|| {
						let name = format!("IR_{:04X}", target.index());
						self.context.append_basic_block(llvm_func, &name)
					});
				},
				OpCode::Br {
					true_case,
					false_case,
					..
				} => {
					blocks.entry(*true_case).or_insert_with(|| {
						let name = format!("IR_{:04X}", true_case.index());
						self.context.append_basic_block(llvm_func, &name)
					});
					blocks.entry(*false_case).or_insert_with(|| {
						let name = format!("IR_{:04X}", false_case.index());
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
			values.insert(ValueIndex::constant(i), self.get_constant(const_ref));
			value_types.insert(ValueIndex::constant(i), *const_ref.ty());
		}

		let mut builder = self.context.create_builder();
		for (i, opcode) in body.opcodes().iter().enumerate() {
			if let Some(block) = blocks.get(&ValueIndex::local(i)) {
				builder.position_at_end(*block);
			};

			match opcode {
				OpCode::Alloca { ty } => {
					let llvm_ty = self.get_type(ty);
					let val = builder.build_alloca(llvm_ty, "").unwrap();
					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), Type::Pointer { mutable: true, ty });
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
				OpCode::Load { value } => {
					let ty = value_types[value];
					let (llvm_ty, ty) = match ty {
						Type::Pointer { ty, .. } => (self.get_type(ty), ty),
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
				OpCode::Add { lhs, rhs } => {
					let ty = value_types[lhs];
					let lhs = values[lhs];
					let rhs = values[rhs];
					let val = match ty {
						Type::Int32 => {
							let lhs = lhs.into_int_value();
							let rhs = rhs.into_int_value();
							builder.build_int_add(lhs, rhs, "").unwrap()
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
						_ => unimplemented!("{:?}", ty),
					};

					values.insert(ValueIndex::local(i), val.as_basic_value_enum());
					value_types.insert(ValueIndex::local(i), Type::Bool);
				},
				OpCode::Ret { value: None } => {
					builder.build_return(None).unwrap();
				},
				OpCode::Ret { value: Some(value) } => {
					builder.build_return(Some(&values[value])).unwrap();
				},
				_ => {
					llvm_func.print_to_stderr();
					unimplemented!("{:?}", opcode)
				},
			}
		}
	}

	fn get_type(&self, ty: &'a Type<'a>) -> BasicTypeEnum<'l> {
		match ty {
			Type::Int32 => self.context.i32_type().into(),
			Type::Struct(data) => {
				let mut types = self.types.borrow_mut();
				if let Some(ty) = types.get(ty) {
					return *ty;
				};

				let llvm_ty = self.context.opaque_struct_type(&ty.id().to_string());
				types.insert(ty, llvm_ty.into());
				drop(types);

				let fields: Vec<_> = data.fields().iter().map(|f| self.get_type(f.ty())).collect();
				llvm_ty.set_body(&fields, false);
				llvm_ty.into()
			},
			_ => unimplemented!("{}", ty),
		}
	}

	fn get_fn_type(&self, func: &Function<'a>) -> FunctionType<'l> {
		let ret_ty = self.get_type(func.ret_ty());
		let param_tys: Vec<_> = func
			.params()
			.iter()
			.map(|p| self.get_type(p.ty()).try_into().unwrap())
			.collect();
		ret_ty.fn_type(&param_tys, false)
	}

	fn get_constant(&self, const_ref: &ConstantRef) -> BasicValueEnum {
		match const_ref.ty() {
			Type::Int32 => self
				.context
				.i32_type()
				.const_int(
					i32::from_le_bytes(const_ref.data().try_into().unwrap()) as u64,
					true,
				)
				.as_basic_value_enum(),
			_ => unimplemented!("{}", const_ref.ty()),
		}
	}
}
