use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Range, Rem, Sub};

pub trait Node {
	fn range(&self) -> Range<usize>;
}

//region Expressions

#[derive(Debug, PartialEq)]
pub struct Ident<'l> {
	pub value: &'l str,
	pub range: Range<usize>,
}

impl Display for Ident<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.value)
	}
}

#[derive(Debug, PartialEq)]
pub enum Literal<'l> {
	Id(Ident<'l>),
	Uninit { range: Range<usize> },
	Char { value: char, range: Range<usize> },
	Float { value: f64, range: Range<usize> },
	Bool { value: bool, range: Range<usize> },
	String { value: &'l str, range: Range<usize> },
	Integer { value: Integer, range: Range<usize> },
}

impl Node for Literal<'_> {
	fn range(&self) -> Range<usize> {
		match self {
			| Literal::Id(id) => id.range.clone(),
			| Literal::Uninit { range, .. }
			| Literal::Char { range, .. }
			| Literal::Float { range, .. }
			| Literal::Bool { range, .. }
			| Literal::String { range, .. }
			| Literal::Integer { range, .. } => range.clone(),
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Integer {
	Any(i128),
	Int8(i8),
	Int16(i16),
	Int32(i32),
	Int64(i64),
	UInt8(u8),
	UInt16(u16),
	UInt32(u32),
	UInt64(u64),
}

// impl<'l> TryFrom<&'l str> for Integer {
// 	type Error = ParseError<'l>;
// 	fn try_from(mut value: &'l str) -> std::result::Result<Self, Self::Error> {
// 		let neg = value.as_bytes()[0] == b'-';
//
// 		value = &value[neg as usize..];
// 		let radix = match value.get(0..2) {
// 			Some("0b") => 2,
// 			Some("0o") => 8,
// 			Some("0x") => 16,
// 			_ => 10,
// 		};
//
// 		if radix != 10 {
// 			value = &value[2..];
// 		}
//
// 		match value.rfind(|c| c == 'i' || c == 'u') {
// 			None => i128::from_str_radix(value, radix)
// 				.map_err(|e| ParseError::from(e.to_string()))
// 				.map(|v| Integer::Any(if neg { -v } else { v })),
// 			Some(i) => match &value[i..] {
// 				"u8" => u8::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::UInt8(v)),
// 				"u16" => u16::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::UInt16(v)),
// 				"u32" => u32::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::UInt32(v)),
// 				"u64" => u64::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::UInt64(v)),
// 				"i8" => i8::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::Int8(if neg { -v } else { v })),
// 				"i16" => i16::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::Int16(if neg { -v } else { v })),
// 				"i32" => i32::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::Int32(if neg { -v } else { v })),
// 				"i64" => i64::from_str_radix(&value[..i], radix)
// 					.map_err(|e| ParseError::from(e.to_string()))
// 					.map(|v| Integer::Int64(if neg { -v } else { v })),
// 				_ => unreachable!("Invalid integer literal {:?}", value),
// 			},
// 		}
// 	}
// }

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
	Pos,
	Neg,
	Not,
	Addr,
	Deref,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Shl,
	Shr,
	Or,
	And,
	Xor,
	Eq,
	Ne,
	Gt,
	Lt,
	Ge,
	Le,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'l> {
	Block(Block<'l>),
	Literal(Literal<'l>),
	NewStruct(NewStruct<'l>),
	Cast {
		expr: Box<Expression<'l>>,
		to_ty: Type<'l>,
		range: Range<usize>,
	},
	FunctionCall(Box<FunctionCall<'l>>),
	Unary {
		operator: UnaryOperator,
		expr: Box<Expression<'l>>,
		range: Range<usize>,
	},
	Binary {
		operator: BinaryOperator,
		lhs: Box<Expression<'l>>,
		rhs: Box<Expression<'l>>,
		range: Range<usize>,
	},
}

impl<'l> Node for Expression<'l> {
	fn range(&self) -> Range<usize> {
		match self {
			Expression::Block(Block { range, .. }) => range.clone(),
			Expression::Literal(n) => n.range(),
			Expression::NewStruct(n) => n.range(),
			Expression::Cast { range, .. } => range.clone(),
			Expression::FunctionCall(n) => n.range(),
			Expression::Unary { range, .. } => range.clone(),
			Expression::Binary { range, .. } => range.clone(),
		}
	}
}

macro_rules! impl_binary_expr_variants {
    ($lhs: expr, $rhs: expr, $func: ident, $operator: ident, $operation_i: ident, $($id: ident),*) => {
		match(&$lhs, &$rhs) {
			$(
				(
					Expression::Literal(Literal::Integer { value: Integer::$id(lhs), range: lhs_r }),
					Expression::Literal(Literal::Integer { value: Integer::$id(rhs), range: rhs_r }),
				) => Expression::Literal(Literal::Integer { value: Integer::$id(lhs.$operation_i(*rhs)), range: lhs_r.start..rhs_r.start }),
			)*
			(
				Expression::Literal(Literal::Float { value: lhs, range: lhs_r }),
				Expression::Literal(Literal::Float { value: rhs, range: rhs_r }),
			) => Expression::Literal(Literal::Float { value: lhs.$func(rhs), range: lhs_r.start..rhs_r.start }),
			_ => {
				Expression::Binary {
					operator: BinaryOperator::$operator,
					range: $lhs.range().start..$rhs.range().end,
					lhs: Box::new($lhs),
					rhs: Box::new($rhs),
				}
			}
		}
	};
}

macro_rules! impl_binary_expr {
	($trait: ident, $func: ident, $operator: ident, $operation_i: ident) => {
		impl<'l> $trait for Expression<'l> {
			type Output = Expression<'l>;
			fn $func(self, rhs: Self) -> Self::Output {
				impl_binary_expr_variants! {
					self, rhs,
					$func, $operator, $operation_i,
					Any,
					Int8, Int16, Int32, Int64,
					UInt8, UInt16, UInt32, UInt64
				}
			}
		}
	};
}

impl_binary_expr!(Add, add, Add, wrapping_add);
impl_binary_expr!(Sub, sub, Sub, wrapping_sub);
impl_binary_expr!(Mul, mul, Mul, wrapping_mul);
impl_binary_expr!(Div, div, Div, wrapping_div);
impl_binary_expr!(Rem, rem, Mod, wrapping_rem);

#[derive(Debug, PartialEq)]
pub struct NewStruct<'l> {
	pub ty: Type<'l>,
	pub range: Range<usize>,
	pub values: Vec<(Ident<'l>, Expression<'l>)>,
}

impl Node for NewStruct<'_> {
	fn range(&self) -> Range<usize> {
		self.range.clone()
	}
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall<'l> {
	pub range: Range<usize>,
	pub func: Expression<'l>,
	pub params: Vec<Expression<'l>>,
}

impl Node for FunctionCall<'_> {
	fn range(&self) -> Range<usize> {
		self.range.clone()
	}
}

//endregion

//region Symbols
#[derive(Debug)]
pub struct SymbolDeclaration<'l> {
	pub public: bool,
	pub name: Ident<'l>,
	pub symbol: Symbol<'l>,
	pub attributes: Vec<Attribute<'l>>,
	pub range: Range<usize>,
}

#[derive(Debug)]
pub enum Symbol<'l> {
	Enum(Enum<'l>),
	// Union(Union<'l>),
	Struct(Struct<'l>),
	Function(Function<'l>),
}

#[derive(Debug)]
pub struct Enum<'l> {
	pub variants: Vec<EnumVariant<'l>>,
}

#[derive(Debug)]
pub struct EnumVariant<'l> {
	pub name: &'l str,
	pub discriminant: Option<i64>,
}

// #[derive(Debug)]
// pub struct Union<'l> {
//
// }

#[derive(Debug)]
pub struct Struct<'l> {
	pub members: Vec<StructMember<'l>>,
}

#[derive(Debug)]
pub struct StructMember<'l> {
	pub name: Ident<'l>,
	pub ty: Type<'l>,
}

#[derive(Debug)]
pub struct Function<'l> {
	pub return_ty: Type<'l>,
	pub params: Vec<FunctionParameter<'l>>,
	pub block: Option<Block<'l>>,
}

#[derive(Debug)]
pub struct FunctionParameter<'l> {
	pub name: Ident<'l>,
	pub ty: Type<'l>,
}

//endregion

//region Statements
#[derive(Debug, PartialEq)]
pub struct Block<'l> {
	pub range: Range<usize>,
	pub statements: Vec<Statement<'l>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'l> {
	If(If<'l>),
	While(While<'l>),
	VarDecl(VarDecl<'l>),
	Expression(Expression<'l>),
	Yield(Option<Expression<'l>>),
	Return {
		range: Range<usize>,
		expr: Option<Expression<'l>>,
	},
	Assignment(Expression<'l>, Expression<'l>),
}

#[derive(Debug, PartialEq)]
pub struct VarDecl<'l> {
	pub name: Ident<'l>,
	pub mutable: bool,
	pub ty: Option<Type<'l>>,
	pub value: Expression<'l>,
}

// impl<'l> VarDecl<'l> {
// 	pub fn new(
// 		name: &'l str,
// 		mutable: bool,
// 		ty: Option<Type<'l>>,
// 		value: Expression<'l>,
// 	) -> Result<'l, VarDecl<'l>> {
// 		if ty.is_none() && value == Expression::Literal(Literal::Uninit) {
// 			return Err(ParseError::User {
// 				error: format! {
// 					"Uninitialized variables need an explicitly defined type. Try `let {}{}: {{type}} = ?`",
// 					if mutable { "mut " } else { "" },
// 					name,
// 				},
// 			});
// 		}
//
// 		Ok(VarDecl {
// 			name,
// 			mutable,
// 			ty,
// 			value,
// 		})
// 	}
// }

#[derive(Debug, PartialEq)]
pub struct If<'l> {
	pub condition: Expression<'l>,
	pub block: Block<'l>,
	pub r#else: Option<Else<'l>>,
}

#[derive(Debug, PartialEq)]
pub enum Else<'l> {
	If(Box<If<'l>>),
	Block(Block<'l>),
}

#[derive(Debug, PartialEq)]
pub struct While<'l> {
	pub condition: Expression<'l>,
	pub block: Block<'l>,
}

//endregion

#[derive(Debug, PartialEq)]
pub enum Type<'l> {
	Id(Ident<'l>),
	Pointer(Box<Type<'l>>, bool),
	Reference(Box<Type<'l>>, bool),
	Array {
		base: Box<Type<'l>>,
		length: Option<Box<Expression<'l>>>,
	},
}

impl Node for Type<'_> {
	fn range(&self) -> Range<usize> {
		match self {
			Type::Id(id) => id.range.clone(),
			_ => unimplemented!(),
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct Attribute<'l> {
	pub id: &'l str,
	pub params: Vec<Expression<'l>>,
}

#[derive(Debug)]
pub struct NamespaceImport<'l>(pub &'l str);

#[derive(Debug)]
pub struct CompilationUnit<'l> {
	pub namespace: &'l str,
	pub imports: Vec<NamespaceImport<'l>>,
	pub declarations: Vec<SymbolDeclaration<'l>>,
}
