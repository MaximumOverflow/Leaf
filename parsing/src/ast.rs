use std::ops::{Add, Div, Mul, Sub, Rem};
use lalrpop_util::lexer::Token;
use std::collections::HashMap;

type Result<'l, T> = std::result::Result<T, ParseError<'l>>;
pub type ParseError<'l> = lalrpop_util::ParseError<usize, Token<'l>, String>;

//region Expressions
#[derive(Debug, PartialEq)]
pub enum Literal<'l> {
	Uninit,
	Id(&'l str),
	Integer(i64),
	Decimal(f64),
	Char(char),
	String(&'l str),
	Boolean(bool),
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
	Literal(Literal<'l>),
	NewStruct(NewStruct<'l>),
	Unary(UnaryOperator, Box<Expression<'l>>),
	Binary(Box<Expression<'l>>, BinaryOperator, Box<Expression<'l>>),
}

macro_rules! impl_binary_expr {
    ($trait: ident, $func: ident, $oper: ident, $op: tt) => {
		impl<'l> $trait for Expression<'l> {
			type Output = Expression<'l>;
			fn $func(self, rhs: Self) -> Self::Output {
				match (&self, &rhs) {
					(Expression::Literal(Literal::Integer(lhs)), Expression::Literal(Literal::Integer(rhs)))
						=> Expression::Literal(Literal::Integer(lhs $op rhs)),

					(Expression::Literal(Literal::Decimal(lhs)), Expression::Literal(Literal::Decimal(rhs)))
						=> Expression::Literal(Literal::Decimal(lhs $op rhs)),

					_ => Expression::Binary(Box::new(self), BinaryOperator::$oper, Box::new(rhs)),
				}
			}
		}
	};
}

impl_binary_expr!(Add, add, Add, +);
impl_binary_expr!(Sub, sub, Sub, -);
impl_binary_expr!(Mul, mul, Mul, *);
impl_binary_expr!(Div, div, Div, %);
impl_binary_expr!(Rem, rem, Mod, %);

#[derive(Debug, PartialEq)]
pub struct NewStruct<'l> {
	pub ty: Type<'l>,
	pub values: HashMap<&'l str, Expression<'l>>,
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
#[derive(Debug)]
pub struct Block<'l> {
	pub statements: Vec<Statement<'l>>,
}

#[derive(Debug)]
pub enum Statement<'l> {
	VarDecl(VarDecl<'l>),
	Yield(Option<Expression<'l>>),
	Return(Option<Expression<'l>>),
	Assignment(Expression<'l>, Expression<'l>),
}

#[derive(Debug)]
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

//endregion

#[derive(Debug, PartialEq)]
pub enum Type<'l> {
	Id(&'l str),
	Pointer(Box<Type<'l>>, bool),
	Reference(Box<Type<'l>>, bool),
}

#[derive(Debug)]
pub struct NamespaceImport<'l>(pub &'l str);

#[derive(Debug)]
pub struct CompilationUnit<'l> {
	pub namespace: &'l str,
	pub imports: Vec<NamespaceImport<'l>>,
	pub declarations: Vec<SymbolDeclaration<'l>>,
}
