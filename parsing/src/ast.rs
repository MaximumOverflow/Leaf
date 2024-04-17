use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Not, Rem, Sub};

use lalrpop_util::lexer::Token;

type Result<'l, T> = std::result::Result<T, ParseError<'l>>;
pub type ParseError<'l> = lalrpop_util::ParseError<usize, Token<'l>, String>;

//region Expressions
#[derive(Debug, PartialEq)]
pub enum Literal<'l> {
	Uninit,
	Char(char),
	Id(&'l str),
	Decimal(f64),
	Boolean(bool),
	String(&'l str),
	Integer(Integer),
}

impl Literal<'_> {
	pub fn unescape_char(s: &str) -> Result<char> {
		let s = &s[1..(s.len() - 1)];
		match unescape::unescape(s) {
			Some(esc) => Ok(esc.chars().next().unwrap()),
			None => Err(ParseError::User {
				error: format!("{} is not a valid character.", s),
			}),
		}
	}

	pub fn unescape_string(s: &str) -> Result<String> {
		let s = &s[1..(s.len() - 1)];
		match unescape::unescape(s) {
			Some(esc) => Ok(esc),
			None => Err(ParseError::User {
				error: format!("{} is not a valid character.", s),
			}),
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

impl<'l> TryFrom<&'l str> for Integer {
	type Error = ParseError<'l>;
	fn try_from(mut value: &'l str) -> std::result::Result<Self, Self::Error> {
		let neg = value.as_bytes()[0] == b'-';

		value = &value[neg as usize..];
		let radix = match value.get(0..2) {
			Some("0b") => 2,
			Some("0o") => 8,
			Some("0x") => 16,
			_ => 10,
		};

		if radix != 10 {
			value = &value[2..];
		}

		match value.rfind(|c| c == 'i' || c == 'u') {
			None => {
				i128::from_str_radix(value, radix)
					.map_err(|e| ParseError::from(e.to_string()))
					.map(|v| Integer::Any(if neg { -v } else { v }))
			},
			Some(i) => match &value[i..] {
				"u8" => {
					u8::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::UInt8(v))
				}
				"u16" => {
					u16::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::UInt16(v))
				}
				"u32" => {
					u32::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::UInt32(v))
				}
				"u64" => {
					u64::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::UInt64(v))
				}
				"i8" => {
					i8::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::Int8(if neg { -v } else { v }))
				}
				"i16" => {
					i16::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::Int16(if neg { -v } else { v }))
				}
				"i32" => {
					i32::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::Int32(if neg { -v } else { v }))
				}
				"i64" => {
					i64::from_str_radix(&value[..i], radix)
						.map_err(|e| ParseError::from(e.to_string()))
						.map(|v| Integer::Int64(if neg { -v } else { v }))
				}
				_ => unreachable!("Invalid integer literal {:?}", value),
			}
		}
	}
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
	Pos,
	Neg,
	Not,
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
	Cast(Box<Expression<'l>>, Type<'l>),
	FunctionCall(Box<FunctionCall<'l>>),
	Unary(UnaryOperator, Box<Expression<'l>>),
	Binary(Box<Expression<'l>>, BinaryOperator, Box<Expression<'l>>),
}

macro_rules! impl_binary_expr {
	($trait: ident, $func: ident, $operator: ident, $operation_i: ident) => {
		impl<'l> $trait for Expression<'l> {
			type Output = Expression<'l>;
			fn $func(self, rhs: Self) -> Self::Output {
				match (&self, &rhs) {
					(
						Expression::Literal(Literal::Integer(Integer::Int8(lhs))),
						Expression::Literal(Literal::Integer(Integer::Int8(rhs))),
					) => {
						Expression::Literal(Literal::Integer(Integer::Int8(lhs.$operation_i(*rhs))))
					},

					(
						Expression::Literal(Literal::Integer(Integer::Int16(lhs))),
						Expression::Literal(Literal::Integer(Integer::Int16(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::Int16(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Integer(Integer::Int32(lhs))),
						Expression::Literal(Literal::Integer(Integer::Int32(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::Int32(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt8(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt8(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::UInt8(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt16(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt16(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::UInt16(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt32(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt32(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::UInt32(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt64(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt64(rhs))),
					) => Expression::Literal(Literal::Integer(Integer::UInt64(
						lhs.$operation_i(*rhs),
					))),

					(
						Expression::Literal(Literal::Decimal(lhs)),
						Expression::Literal(Literal::Decimal(rhs)),
					) => Expression::Literal(Literal::Decimal(lhs.$func(rhs))),

					_ => {
						Expression::Binary(Box::new(self), BinaryOperator::$operator, Box::new(rhs))
					},
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

impl<'l> Not for Expression<'l> {
	type Output = Expression<'l>;
	fn not(self) -> Self::Output {
		match self {
			Expression::Literal(Literal::Boolean(v)) => Expression::Literal(Literal::Boolean(!v)),
			_ => Expression::Unary(UnaryOperator::Neg, Box::new(self)),
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct NewStruct<'l> {
	pub ty: Type<'l>,
	pub values: HashMap<&'l str, (usize, Expression<'l>)>,
}

impl<'l> NewStruct<'l> {
	pub(crate) fn new(
		ty: Type<'l>,
		fields: impl IntoIterator<Item = (&'l str, Expression<'l>)>,
	) -> Self {
		let mut values = HashMap::new();
		for (i, (key, value)) in fields.into_iter().enumerate() {
			values.insert(key, (i, value));
		}
		Self { ty, values }
	}
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall<'l> {
	pub func: Expression<'l>,
	pub params: Vec<Expression<'l>>,
}

//endregion

//region Symbols
#[derive(Debug)]
pub struct SymbolDeclaration<'l> {
	pub public: bool,
	pub name: &'l str,
	pub symbol: Symbol<'l>,
	pub attributes: Vec<Attribute<'l>>,
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
	pub name: &'l str,
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
	pub name: &'l str,
	pub ty: Type<'l>,
}

//endregion

//region Statements
#[derive(Debug, PartialEq)]
pub struct Block<'l> {
	pub statements: Vec<Statement<'l>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'l> {
	If(If<'l>),
	While(While<'l>),
	VarDecl(VarDecl<'l>),
	Expression(Expression<'l>),
	Yield(Option<Expression<'l>>),
	Return(Option<Expression<'l>>),
	Assignment(Expression<'l>, Expression<'l>),
}

#[derive(Debug, PartialEq)]
pub struct VarDecl<'l> {
	pub name: &'l str,
	pub mutable: bool,
	pub ty: Option<Type<'l>>,
	pub value: Expression<'l>,
}

impl<'l> VarDecl<'l> {
	pub fn new(
		name: &'l str,
		mutable: bool,
		ty: Option<Type<'l>>,
		value: Expression<'l>,
	) -> Result<'l, VarDecl<'l>> {
		if ty.is_none() && value == Expression::Literal(Literal::Uninit) {
			return Err(ParseError::User {
				error: format! {
					"Uninitialized variables need an explicitly defined type. Try `let {}{}: {{type}} = ?`",
					if mutable { "mut " } else { "" },
					name,
				},
			});
		}

		Ok(VarDecl {
			name,
			mutable,
			ty,
			value,
		})
	}
}

#[derive(Debug, PartialEq)]
pub struct If<'l> {
	pub condition: Expression<'l>,
	pub block: Block<'l>,
	pub r#else: Option<Box<Else<'l>>>,
}

#[derive(Debug, PartialEq)]
pub enum Else<'l> {
	If(If<'l>),
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
	Id(&'l str),
	Pointer(Box<Type<'l>>, bool),
	Reference(Box<Type<'l>>, bool),
	Array {
		base: Box<Type<'l>>,
		length: Option<Box<Expression<'l>>>,
	},
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
