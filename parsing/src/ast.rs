use std::ops::{Add, Div, Mul, Sub, Rem};
use lalrpop_util::lexer::Token;
use std::collections::HashMap;

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

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
	Pos,
	Neg,
	Not,
	Deref,
}

#[derive(Debug, Eq, PartialEq)]
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
	Neq,
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
						Expression::Literal(Literal::Integer(Integer::Int8(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::Int8(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::Int16(lhs))),
						Expression::Literal(Literal::Integer(Integer::Int16(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::Int16(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::Int32(lhs))),
						Expression::Literal(Literal::Integer(Integer::Int32(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::Int32(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt8(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt8(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::UInt8(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt16(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt16(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::UInt16(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt32(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt32(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::UInt32(lhs.$operation_i(*rhs)))),

					(
						Expression::Literal(Literal::Integer(Integer::UInt64(lhs))),
						Expression::Literal(Literal::Integer(Integer::UInt64(rhs)))
					) => Expression::Literal(Literal::Integer(Integer::UInt64(lhs.$operation_i(*rhs)))),

					(Expression::Literal(Literal::Decimal(lhs)), Expression::Literal(Literal::Decimal(rhs)))
						=> Expression::Literal(Literal::Decimal(lhs.$func(rhs))),

					_ => Expression::Binary(Box::new(self), BinaryOperator::$operator, Box::new(rhs)),
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
	pub values: HashMap<&'l str, (usize, Expression<'l>)>,
}

impl<'l> NewStruct<'l> {
	pub(crate) fn new(ty: Type<'l>, fields: impl IntoIterator<Item=(&'l str, Expression<'l>)>) -> Self {
		let mut values = HashMap::new();
		for (i, (key, value)) in fields.into_iter().enumerate() {
			values.insert(key, (i, value));
		}
		Self { ty, values, }
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
	pub symbol: Symbol<'l>
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
	pub variants: Vec<EnumVariant<'l>>
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
	pub members: Vec<StructMember<'l>>
}

#[derive(Debug)]
pub struct StructMember<'l> {
	pub name: &'l str,
	pub ty: Type<'l>
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
	pub ty: Type<'l>
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
	pub fn new(name: &'l str, mutable: bool, ty: Option<Type<'l>>, value: Expression<'l>) -> Result<'l, VarDecl<'l>> {
		if ty.is_none() && value == Expression::Literal(Literal::Uninit) {
			return Err(ParseError::User {
				error: format! {
					"Uninitialized variables need an explicitly defined type. Try `let {}{}: {{type}} = ?`",
					if mutable { "mut " } else { "" },
					name,
				}
			});
		}

		Ok(VarDecl { name, mutable, ty, value })
	}
}

#[derive(Debug, PartialEq)]
pub struct If<'l> {
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

#[derive(Debug)]
pub struct NamespaceImport<'l>(pub &'l str);

#[derive(Debug)]
pub struct CompilationUnit<'l> {
	pub namespace: &'l str,
	pub imports: Vec<NamespaceImport<'l>>,
	pub declarations: Vec<SymbolDeclaration<'l>>,
}
