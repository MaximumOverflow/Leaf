use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::mem::discriminant;
use std::cmp::Ordering;
use std::fmt::Debug;

use bumpalo::Bump;
use fxhash::FxHashMap;
use nohash_hasher::BuildNoHashHasher;

use crate::{Comparison, ConstantRef, Function, Parameter, SSAData, Type, ValueIndex};
use crate::heaps::HeapScopes;

#[derive(Debug, Copy, Clone, Hash)]
pub struct ValueRef<'a, 'l> {
	ty: &'l Type<'l>,
	op: &'a OpCode<'a, 'l>,
}

impl<'l> ValueRef<'_, 'l> {
	#[inline(always)]
	pub fn ty(&self) -> &'l Type<'l> {
		self.ty
	}

	#[inline(always)]
	pub fn is_variable(&self) -> bool {
		matches!(self.op, OpCode::Alloca { .. })
	}

	pub unsafe fn set_ty(&mut self, ty: &'l Type<'l>) {
		self.ty = ty;
	}
}

impl<'a, 'l> Eq for ValueRef<'a, 'l> {}
impl<'a, 'l> PartialEq for ValueRef<'a, 'l> {
	fn eq(&self, other: &Self) -> bool {
		std::ptr::eq(self.op, other.op)
	}
}

pub struct SSABuilder<'a, 'l> {
	bump: Bump,
	heaps: HeapScopes<'l>,
	insert_block: BlockIndex,
	blocks: Vec<Block<'a, 'l>>,
	params: &'a [Parameter<'l>],
	param_vals: &'a [ValueRef<'a, 'l>],
	functions: FxHashMap<usize, ValueRef<'a, 'l>>,
	constants: FxHashMap<(&'l Type<'l>, usize), ValueRef<'a, 'l>>,
}

macro_rules! unsupported_operation {
    ($($arg:tt)*) => {
		std::result::Result::Err(format!("Unsupported operation: {}", format_args!($($arg)*)))
	};
}

impl<'a, 'l> SSABuilder<'a, 'l> {
	pub fn new(heaps: HeapScopes<'l>, params: &'a [Parameter<'l>]) -> Self {
		let bump = Bump::new();
		Self {
			heaps,
			params,
			insert_block: BlockIndex(0),
			blocks: vec![Block {
				opcodes: vec![],
				in_transitions: BlockTransitions::Terminal,
				out_transitions: BlockTransitions::None,
				used_values: Default::default(),
			}],
			functions: Default::default(),
			constants: Default::default(),
			param_vals: unsafe {
				std::mem::transmute(bump.alloc_slice_fill_iter(params.iter().map(|p| ValueRef {
					ty: p.ty(),
					op: bump.alloc(OpCode::Param { ty: p.ty() }),
				})))
			},
			bump,
		}
	}

	pub fn current_block(&self) -> BlockIndex {
		self.insert_block
	}

	pub fn set_current_block(&mut self, block: BlockIndex) -> Result<BlockIndex, BlockIndex> {
		match block.0 >= self.blocks.len() {
			true => Err(block),
			false => Ok(std::mem::replace(&mut self.insert_block, block)),
		}
	}

	pub fn create_block(&mut self) -> BlockIndex {
		let idx = BlockIndex(self.blocks.len());
		self.blocks.push(Block::default());
		idx
	}

	#[allow(private_bounds)]
	pub fn constant<T: UseConst>(&mut self, value: T) -> ValueRef<'a, 'l> {
		value.use_const(self)
	}

	pub fn function(&mut self, func: &'l Function<'l>) -> ValueRef<'a, 'l> {
		let key = func as *const _ as usize;
		if let Some(func) = self.functions.get(&key) {
			return *func;
		}
		let param_tys: Vec<_> = func.params().iter().map(|i| i.ty()).collect();
		let op = unsafe { std::mem::transmute(self.bump.alloc(OpCode::Func { func })) };
		*self.functions.entry(key).or_insert_with(|| ValueRef {
			ty: self.heaps.type_heap().function_ptr(func.ret_ty(), &param_tys),
			op,
		})
	}

	pub fn alloca(&mut self, ty: &'l Type<'l>, mutable: bool) -> ValueRef<'a, 'l> {
		let ref_ty = self.heaps.type_heap().reference(ty, mutable);
		ValueRef {
			ty: ref_ty,
			op: self.push_opcode(OpCode::Alloca { ty }),
		}
	}

	pub fn param(&mut self, idx: usize) -> Option<ValueRef<'a, 'l>> {
		self.param_vals.get(idx).cloned()
	}

	pub fn size_of(&mut self, ty: &'l Type<'l>) -> ValueRef<'a, 'l> {
		ValueRef {
			ty: &Type::UInt64,
			op: self.push_opcode(OpCode::SizeOf { ty }),
		}
	}

	pub fn load(&mut self, value: ValueRef<'a, 'l>) -> Result<ValueRef<'a, 'l>, String> {
		match value.ty {
			Type::Pointer { ty, .. } | Type::Reference { ty, .. } => {
				self.use_value(value);
				Ok(ValueRef {
					ty,
					op: self.push_opcode(OpCode::Load { val: value }),
				})
			},
			_ => unsupported_operation!("Load {}", value.ty),
		}
	}

	pub fn gep(
		&mut self,
		array: ValueRef<'a, 'l>,
		index: ValueRef<'a, 'l>,
		mutable: bool,
	) -> Result<ValueRef<'a, 'l>, String> {
		if !index.ty.is_integer() {
			return Err(format!("Expected integer index, found `{}`", index.ty));
		}
		let Type::Reference {
			ty: Type::Array { ty, .. },
			mutable: is_mut,
		} = array.ty
		else {
			return Err(format!("Type `{}` cannot be indexed", array.ty));
		};

		if mutable && !is_mut {
			return Err(format!("Type `{}` cannot be indexed mutably", array.ty));
		}

		Ok(ValueRef {
			ty: self.heaps.type_heap().reference(ty, mutable),
			op: self.push_opcode(OpCode::GEP {
				val: array,
				idx: index,
			}),
		})
	}

	pub fn store(
		&mut self,
		val: ValueRef<'a, 'l>,
		dst: ValueRef<'a, 'l>,
	) -> Result<ValueRef<'a, 'l>, String> {
		match dst.ty {
			Type::Pointer { ty, .. } | Type::Reference { ty, .. } => {
				if val.ty != *ty {
					return Err(format!("Expected type `{}`, found `{}`", ty, val.ty));
				}
				self.use_value(dst);
				self.use_value(val);
				Ok(ValueRef {
					ty: &Type::Void,
					op: self.push_opcode(OpCode::Store { dst, val }),
				})
			},
			_ => unsupported_operation!("Load {}", dst.ty),
		}
	}

	pub fn call(
		&mut self,
		func: ValueRef<'a, 'l>,
		params: &[ValueRef<'a, 'l>],
	) -> Result<ValueRef<'a, 'l>, String> {
		let Type::FunctionPointer { ret_ty, param_tys } = func.ty else {
			return Err(format!("Type `{}` is not a function pointer", func.ty));
		};

		if params.len() != param_tys.len() {
			return Err(format!(
				"Expected {} parameters, found {}",
				param_tys.len(),
				params.len()
			));
		}

		for (i, (val, expected)) in params.iter().zip(*param_tys).enumerate() {
			if val.ty != *expected {
				return Err(format! {
					"Invalid parameter at position {i}. Expected type `{}`, found {}",
					expected, val.ty,
				});
			}
		}

		let params = unsafe { std::mem::transmute(self.bump.alloc_slice_copy(params)) };
		let op = self.push_opcode(match func.op {
			OpCode::Func { func } => OpCode::Call { func, params },
			_ => OpCode::CallInd { func, params },
		});

		Ok(ValueRef { op, ty: ret_ty })
	}

	pub fn cmp(
		&mut self,
		lhs: ValueRef<'a, 'l>,
		rhs: ValueRef<'a, 'l>,
		cmp: Comparison,
	) -> Result<ValueRef<'a, 'l>, String> {
		match cmp {
			Comparison::Lt => self.lt(lhs, rhs),
			_ => unimplemented!(),
		}
	}

	pub fn ret(&mut self, value: Option<ValueRef<'a, 'l>>) -> Result<ValueRef<'a, 'l>, String> {
		//TODO Add validation
		if let Some(value) = value {
			self.use_value(value);
		}
		self.current_block_ref().out_transitions = BlockTransitions::Terminal;
		Ok(ValueRef {
			ty: &Type::Void,
			op: self.push_opcode(OpCode::Ret { val: value }),
		})
	}

	pub fn jp(&mut self, target: BlockIndex) -> Result<ValueRef<'a, 'l>, String> {
		if target.0 >= self.blocks.len() {
			return Err(format!("Invalid block index {}", target.0));
		}
		self.current_block_ref().out_transitions =
			BlockTransitions::OtherBlocks(HashSet::from_iter([target]));
		self.add_in_block(self.current_block(), target);
		Ok(ValueRef {
			ty: &Type::Void,
			op: self.push_opcode(OpCode::Jp { target }),
		})
	}

	pub fn br(
		&mut self,
		condition: ValueRef<'a, 'l>,
		true_case: BlockIndex,
		false_case: BlockIndex,
	) -> Result<ValueRef<'a, 'l>, String> {
		if true_case.0 >= self.blocks.len() {
			return Err(format!("Invalid block index {} (true)", true_case.0));
		}
		if false_case.0 >= self.blocks.len() {
			return Err(format!("Invalid block index {} (false)", false_case.0));
		}
		if *condition.ty != Type::Bool {
			return Err(format!(
				"Expected type `{}`, found `{}`",
				Type::Bool,
				condition.ty
			));
		}
		self.current_block_ref().out_transitions =
			BlockTransitions::OtherBlocks(HashSet::from_iter([true_case, false_case]));
		self.add_in_block(self.current_block(), true_case);
		self.add_in_block(self.current_block(), false_case);
		Ok(ValueRef {
			ty: &Type::Void,
			op: self.push_opcode(OpCode::Br {
				condition,
				true_case,
				false_case,
			}),
		})
	}

	pub fn not(&mut self, value: ValueRef<'a, 'l>) -> Result<ValueRef<'a, 'l>, String> {
		if value.ty != &Type::Bool {
			return Err(format!("Expected type `bool`, found `{}`", value.ty));
		}
		self.use_value(value);
		Ok(ValueRef {
			ty: value.ty,
			op: self.push_opcode(OpCode::Not { val: value }),
		})
	}

	pub fn trunc(
		&mut self,
		ty: &'l Type<'l>,
		value: ValueRef<'a, 'l>,
	) -> Result<ValueRef<'a, 'l>, String> {
		'supported_operations: {
			return match (value.ty.integer_size(), ty.integer_size()) {
				(Some(size), Some(ty_size)) if size > ty_size => {
					self.use_value(value);
					Ok(ValueRef {
						ty,
						op: self.push_opcode(OpCode::Trunc { ty, value }),
					})
				},
				_ => break 'supported_operations,
			};
		}
		unsupported_operation!("Trunc {} -> {}", value.ty, ty)
	}

	pub fn equalize_integer_types(
		&mut self,
		lhs: ValueRef<'a, 'l>,
		rhs: ValueRef<'a, 'l>,
	) -> Result<(ValueRef<'a, 'l>, ValueRef<'a, 'l>), String> {
		let Some(lhs_size) = lhs.ty.integer_size() else {
			return Err(format!("Type `{}` (lhs) is not an integer", lhs.ty));
		};
		let Some(rhs_size) = rhs.ty.integer_size() else {
			return Err(format!("Type `{}` (rhs) is not an integer", rhs.ty));
		};
		Ok(match lhs_size.cmp(&rhs_size) {
			Ordering::Equal => (lhs, rhs),
			Ordering::Less => {
				self.use_value(lhs);
				(
					match lhs.ty.is_signed_integer().unwrap() {
						true => self.sext(rhs.ty, lhs)?,
						false => self.zext(rhs.ty, lhs)?,
					},
					rhs,
				)
			},
			Ordering::Greater => {
				self.use_value(rhs);
				(
					lhs,
					match rhs.ty.is_signed_integer().unwrap() {
						true => self.sext(lhs.ty, rhs)?,
						false => self.zext(lhs.ty, rhs)?,
					},
				)
			},
		})
	}

	fn use_value(&mut self, value: impl Into<&'a OpCode<'a, 'l>>) {
		self.current_block_ref().used_values.insert(value.into());
	}

	fn push_opcode(&mut self, opcode: OpCode<'a, 'l>) -> &'a OpCode<'a, 'l> {
		let opcode = unsafe { std::mem::transmute(self.bump.alloc(opcode)) };
		self.current_block_ref().opcodes.push(opcode);
		opcode
	}

	fn current_block_ref(&mut self) -> &mut Block<'a, 'l> {
		&mut self.blocks[self.insert_block.0]
	}

	fn add_in_block(&mut self, block: BlockIndex, into: BlockIndex) {
		match &mut self.blocks[into.0].in_transitions {
			BlockTransitions::OtherBlocks(blocks) => {
				blocks.insert(block);
			},
			transitions => {
				*transitions = BlockTransitions::OtherBlocks(HashSet::from_iter([block]))
			},
		}
	}
}

trait UseConst {
	fn use_const<'a, 'l>(self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l>;
}

macro_rules! impl_int_ext {
	($(($name: ident, $op: ident, $cond: literal)),*) => {
		impl<'a, 'l> SSABuilder<'a, 'l> {$(
			pub fn $name(&mut self, ty: &'l Type<'l>, value: ValueRef<'a, 'l>) -> Result<ValueRef<'a, 'l>, String> {
				'supported_operations: {
					match (ty.is_signed_integer(), value.ty.is_signed_integer()) {
						(Some($cond), Some(_)) => {
							return match ty
								.integer_size()
								.unwrap()
								.cmp(&value.ty.integer_size().unwrap())
							{
								Ordering::Equal => Ok(value),
								Ordering::Greater => {
									self.use_value(value);
									Ok(ValueRef {
										ty,
										op: self.push_opcode(OpCode::$op { ty, value }),
									})
								},
								Ordering::Less => break 'supported_operations,
							}
						},
						_ => break 'supported_operations,
					}
				}
				unsupported_operation!("{} {} -> {}", stringify!($op), value.ty, ty)
			}
		)*}
	};
}

macro_rules! impl_binary_operators {
    ($(($name: ident, $op: ident, $map: expr $(, { $($id: ident: $val: expr),* })?)),+) => {
		impl<'a, 'l> SSABuilder<'a, 'l> {$(
			#[allow(clippy::redundant_closure_call)]
			pub fn $name(&mut self, lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l>) -> Result<ValueRef<'a, 'l>, String> {
				'supported_operations: {
					match (lhs.ty.is_integer(), rhs.ty.is_integer()) {
						(true, true) => {
							let (lhs, rhs) = self.equalize_integer_types(lhs, rhs)?;
							self.use_value(lhs);
							self.use_value(rhs);
							return Ok(ValueRef {
								ty: ($map)(&lhs.ty),
								op: self.push_opcode(OpCode::$op {
									lhs,
									rhs,
									$($($id: $val),*)?
								}),
							});
						}
						_ => break 'supported_operations,
					}
				};
				unsupported_operation!("{} {}, {}", stringify!($op), lhs.ty, rhs.ty)
			}
		)*}
	};
}

macro_rules! impl_const {
	($([$ty: ty; $ty_enum: ident]),*) => {$(
		impl UseConst for $ty {
			fn use_const<'a, 'l>(self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l> {
				let ty = &Type::$ty_enum;
				let bytes = self.to_le_bytes();
				let (data, id) = builder.heaps.blob_heap().intern(bytes.as_ref());

				let key = (ty, id);
				if let Some(value) = builder.constants.get(&key) {
					return *value;
				}

				let value = ValueRef {
					ty,
					op: unsafe { std::mem::transmute(builder.bump.alloc(OpCode::Const { ty, value: data }) ) }
				};
				builder.constants.insert(key, value);
				value
			}
		}
	)*};
}

impl_int_ext![(sext, SExt, true), (zext, ZExt, false)];
impl_binary_operators![
	(add, Add, |ty| ty),
	(sub, Sub, |ty| ty),
	(mul, Mul, |ty| ty),
	(div, Div, |ty| ty),
	(rem, Rem, |ty| ty),
	(eq, Cmp, |_| &Type::Bool, { cmp: Comparison::Eq }),
	(ne, Cmp, |_| &Type::Bool, { cmp: Comparison::Ne }),
	(lt, Cmp, |_| &Type::Bool, { cmp: Comparison::Lt }),
	(gt, Cmp, |_| &Type::Bool, { cmp: Comparison::Gt }),
	(le, Cmp, |_| &Type::Bool, { cmp: Comparison::Le }),
	(ge, Cmp, |_| &Type::Bool, { cmp: Comparison::Ge })
];

impl_const! {
	[i8; Int8],
	[i16; Int16],
	[i32; Int32],
	[i64; Int64],
	[u8; UInt8],
	[u16; UInt16],
	[u32; UInt32],
	[u64; UInt64],
	[f32; Float32],
	[f64; Float64]
}

impl UseConst for bool {
	fn use_const<'a, 'l>(self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l> {
		let ty = &Type::Bool;
		let bytes = [self as u8];
		let (data, id) = builder.heaps.blob_heap().intern(bytes.as_ref());

		let key = (ty, id);
		if let Some(value) = builder.constants.get(&key) {
			return *value;
		}

		let value = ValueRef {
			ty,
			op: unsafe {
				std::mem::transmute(builder.bump.alloc(OpCode::Const { ty, value: data }))
			},
		};
		builder.constants.insert(key, value);
		value
	}
}

impl UseConst for &str {
	fn use_const<'a, 'l>(self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l> {
		self.to_string().use_const(builder)
	}
}

impl UseConst for String {
	fn use_const<'a, 'l>(mut self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l> {
		if self.as_bytes().last() != Some(&b'\0') {
			self.push('\0');
		}
		self.into_bytes().use_const(builder)
	}
}

impl UseConst for Vec<u8> {
	fn use_const<'a, 'l>(self, builder: &mut SSABuilder<'a, 'l>) -> ValueRef<'a, 'l> {
		let ty = &Type::Pointer {
			mutable: false,
			ty: &Type::UInt8,
		};
		let (data, id) = builder.heaps.blob_heap().intern(self);
		let key = (ty, id);
		if let Some(value) = builder.constants.get(&key) {
			return *value;
		}
		let value = ValueRef {
			ty,
			op: unsafe {
				std::mem::transmute(builder.bump.alloc(OpCode::Const { ty, value: data }))
			},
		};
		builder.constants.insert(key, value);
		value
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BlockIndex(usize);

#[derive(Debug, Default)]
struct Block<'a, 'l> {
	opcodes: Vec<&'a OpCode<'a, 'l>>,
	in_transitions: BlockTransitions,
	out_transitions: BlockTransitions,
	used_values: HashSet<&'a OpCode<'a, 'l>, BuildNoHashHasher<usize>>,
}

#[derive(Debug, Default)]
enum BlockTransitions {
	#[default]
	None,
	Terminal,
	OtherBlocks(HashSet<BlockIndex, BuildNoHashHasher<usize>>),
}

impl Eq for Block<'_, '_> {}

impl PartialEq<Self> for Block<'_, '_> {
	fn eq(&self, other: &Self) -> bool {
		std::ptr::eq(self, other)
	}
}

impl Hash for Block<'_, '_> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(self as *const Self as usize)
	}
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone)]
enum OpCode<'a, 'l> {
	Param { ty: &'l Type<'l> },
	Func { func: &'l Function<'l> },
	Const { ty: &'l Type<'l>, value: &'l [u8] },
	SizeOf { ty: &'l Type<'l> },
	Alloca { ty: &'l Type<'l> },
	Add { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l> },
	Sub { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l> },
	Mul { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l> },
	Div { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l> },
	Rem { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l> },
	Cmp { lhs: ValueRef<'a, 'l>, rhs: ValueRef<'a, 'l>, cmp: Comparison, },
	Not { val: ValueRef<'a, 'l> },
	SExt { ty: &'l Type<'l>, value: ValueRef<'a, 'l> },
	ZExt { ty: &'l Type<'l>, value: ValueRef<'a, 'l> },
	Trunc { ty: &'l Type<'l>, value: ValueRef<'a, 'l> },

	Load { val: ValueRef<'a, 'l> },
	GEP { val: ValueRef<'a, 'l>, idx: ValueRef<'a, 'l> },
	Store { val: ValueRef<'a, 'l>, dst: ValueRef<'a, 'l> },
	Call { func: &'l Function<'l>, params: &'a [ValueRef<'a, 'l>] },
	CallInd { func: ValueRef<'a, 'l>, params: &'a [ValueRef<'a, 'l>] },

	Ret { val: Option<ValueRef<'a, 'l>> },
	Jp { target: BlockIndex },
	Br {
		condition: ValueRef<'a, 'l>,
		true_case: BlockIndex,
		false_case: BlockIndex,
	},
}

impl Eq for OpCode<'_, '_> {}

impl PartialEq<Self> for OpCode<'_, '_> {
	fn eq(&self, other: &Self) -> bool {
		std::ptr::eq(self, other)
	}
}

impl Hash for OpCode<'_, '_> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(self as *const Self as usize)
	}
}

impl<'a, 'l> From<ValueRef<'a, 'l>> for &'a OpCode<'a, 'l> {
	fn from(value: ValueRef<'a, 'l>) -> Self {
		value.op
	}
}

impl<'a, 'l> SSABuilder<'a, 'l> {
	#[tracing::instrument(skip_all)]
	pub fn build(self) -> Result<SSAData<'l>, String> {
		type Hasher = BuildNoHashHasher<usize>;
		use super::r#final::OpCode as FOpCode;

		let mut offset = 0;
		let mut referenced_constants: Vec<ConstantRef<'l>> = vec![];
		let mut referenced_functions: Vec<&'l Function<'l>> = vec![];
		let mut block_offsets = vec![0; self.blocks.len()];
		for (i, block) in self.blocks.iter().enumerate() {
			if discriminant(&block.in_transitions) == discriminant(&BlockTransitions::None) {
				dbg!(block);
				return Err(format!("Block {i} has no predecessors"));
			}
			if discriminant(&block.out_transitions) == discriminant(&BlockTransitions::None) {
				dbg!(block);
				return Err(format!("Block {i} has no terminator"));
			}
			block_offsets[i] = offset;
			offset += block.opcodes.len();
		}

		let mut index_of = {
			type ValueMap<'a, 'l> = HashMap<&'a OpCode<'a, 'l>, ValueIndex, Hasher>;
			let mut map: ValueMap<'a, 'l> = HashMap::default();
			let mut idx = 0usize;
			let mut param = 0usize;
			let referenced_constants = &mut referenced_constants;
			let referenced_functions = &mut referenced_functions;
			move |op: &'a OpCode<'a, 'l>| -> ValueIndex {
				*map.entry(op).or_insert_with(|| match op {
					OpCode::Param { .. } => {
						let new_param = param + 1;
						let param = std::mem::replace(&mut param, new_param);
						ValueIndex::parameter(param)
					},
					OpCode::Const { value, ty } => {
						match referenced_constants
							.iter()
							.position(|i| i.data.as_ptr() == value.as_ptr())
						{
							Some(i) => ValueIndex::constant(i),
							None => {
								let idx = ValueIndex::constant(referenced_constants.len());
								referenced_constants.push(ConstantRef { data: value, ty });
								idx
							},
						}
					},
					OpCode::Func { func, .. } => {
						match referenced_functions.iter().position(|i| std::ptr::eq(*i, *func)) {
							Some(i) => ValueIndex::function(i),
							None => {
								let idx = ValueIndex::function(referenced_functions.len());
								referenced_functions.push(*func);
								idx
							},
						}
					},
					_ => {
						let new_idx = idx + 1;
						let idx = std::mem::replace(&mut idx, new_idx);
						ValueIndex::local(idx)
					},
				})
			}
		};

		let mut opcodes = vec![];

		for block in self.blocks {
			for opcode in block.opcodes {
				let _idx = index_of(opcode);
				opcodes.push(match opcode {
					OpCode::Param { .. } | OpCode::Const { .. } | OpCode::Func { .. } => {
						unreachable!("{:?}", opcode);
					},
					OpCode::Alloca { ty } => FOpCode::Alloca { ty },
					OpCode::SizeOf { ty } => FOpCode::SizeOf { ty },
					OpCode::Add { lhs, rhs } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Add {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
						}
					},
					OpCode::Sub { lhs, rhs } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Sub {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
						}
					},
					OpCode::Mul { lhs, rhs } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Mul {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
						}
					},
					OpCode::Div { lhs, rhs } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Div {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
						}
					},
					OpCode::Rem { lhs, rhs } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Rem {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
						}
					},
					OpCode::Cmp { lhs, rhs, cmp } => {
						debug_assert_eq!(lhs.ty, rhs.ty);
						FOpCode::Cmp {
							lhs: index_of(lhs.op),
							rhs: index_of(rhs.op),
							cmp: *cmp,
						}
					},
					OpCode::Not { val } => {
						debug_assert_eq!(val.ty, &Type::Bool);
						FOpCode::Not {
							val: index_of(val.op),
						}
					},
					OpCode::SExt { ty, value } => {
						debug_assert_eq!(ty.is_signed_integer(), Some(true));
						debug_assert!(value.ty.is_integer());
						FOpCode::SExt {
							ty,
							value: index_of(value.op),
						}
					},
					OpCode::ZExt { ty, value } => {
						debug_assert_eq!(ty.is_signed_integer(), Some(false));
						debug_assert!(value.ty.is_integer());
						FOpCode::ZExt {
							ty,
							value: index_of(value.op),
						}
					},
					OpCode::Trunc { ty, value } => {
						debug_assert_eq!(ty.is_signed_integer(), Some(false));
						debug_assert!(value.ty.is_integer());
						FOpCode::Trunc {
							ty,
							value: index_of(value.op),
						}
					},
					OpCode::Load { val: value } => {
						debug_assert!(matches!(
							value.ty,
							Type::Pointer { .. } | Type::Reference { .. }
						));
						FOpCode::Load {
							value: index_of(value.op),
						}
					},
					OpCode::Store { val, dst } => {
						debug_assert! {
							matches! {
								dst.ty,
								| Type::Pointer { ty, .. }
								| Type::Reference { ty, .. } if *ty == val.ty,
							},
							"Invalid Store {}, {}",
							val.ty,
							dst.ty
						};
						FOpCode::Store {
							val: index_of(val.op),
							dst: index_of(dst.op),
						}
					},
					OpCode::GEP { val, idx } => {
						debug_assert! {
							matches! {
								val.ty,
								Type::Reference { ty: Type::Array { .. }, .. },
							},
							"Invalid GEP {}, {}",
							val.ty, idx.ty
						};
						FOpCode::GEP {
							val: index_of(val.op),
							idx: index_of(idx.op),
						}
					},
					OpCode::Call { func, params } => {
						let params: Vec<_> = params.iter().map(|v| index_of(v.op)).collect();
						FOpCode::Call {
							params: self.heaps.bump().alloc_slice_copy(&params),
							func: index_of(self.functions[&(*func as *const _ as usize)].op),
						}
					},
					OpCode::CallInd { func, params } => {
						let params: Vec<_> = params.iter().map(|v| index_of(v.op)).collect();
						FOpCode::CallInd {
							func: index_of(func.op),
							params: self.heaps.bump().alloc_slice_copy(&params),
						}
					},
					OpCode::Ret { val: value } => {
						//TODO Add validation
						FOpCode::Ret {
							value: value.map(|v| index_of(v.op)),
						}
					},
					OpCode::Jp { target } => FOpCode::Jp {
						target: ValueIndex::local(block_offsets[target.0]),
					},
					OpCode::Br {
						condition,
						true_case,
						false_case,
					} => FOpCode::Br {
						condition: index_of(condition.op),
						true_case: ValueIndex::local(block_offsets[true_case.0]),
						false_case: ValueIndex::local(block_offsets[false_case.0]),
					},
				});
			}
		}

		Ok(SSAData {
			opcodes,
			referenced_constants,
			referenced_functions,
		})
	}
}
