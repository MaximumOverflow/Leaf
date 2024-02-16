use std::fmt::{Display, Formatter};
use lalrpop_util::lexer::Token;

type Result<'l, T> = std::result::Result<T, ParseError<'l>>;
pub type ParseError<'l> = lalrpop_util::ParseError<usize, Token<'l>, String>;

#[derive(Debug)]
pub struct Alias<'l> {
	pub id: &'l str,
	pub ty: Box<Type<'l>>,
}

#[derive(Debug)]
pub struct Enum<'l> {
	pub id: Option<&'l str>,
	pub variants: Vec<&'l str>,
}

#[derive(Debug)]
pub struct Struct<'l> {
	pub id: Option<&'l str>,
	pub members: Vec<Member<'l>>,
}

#[derive(Debug)]
pub struct Union<'l> {
	pub id: Option<&'l str>,
	pub members: Vec<Member<'l>>,
}

#[derive(Debug)]
pub struct Member<'l> {
	pub id: &'l str,
	pub ty: Type<'l>,
}

#[derive(Debug)]
pub struct Interface<'l> {
	pub id: Option<&'l str>,
	pub members: Vec<InterfaceMember<'l>>,
}

#[derive(Debug)]
pub enum InterfaceMember<'l> {
	Property {
		id: &'l str,
		get: bool,
		set: bool,
		ty: Type<'l>,
	},

	Function {
		id: &'l str,
		signature: FunctionSignature<'l>,
	},
}

#[derive(Debug)]
pub struct TemplateCall<'l> {
	pub callee: Expression<'l>,
	pub params: Vec<Expression<'l>>,
}

#[derive(Debug)]
pub enum Type<'l> {
	Id(&'l str),

	Ref {
		kind: &'l str,
		is_mut: bool,
		base: Box<Type<'l>>,
	},

	Array {
		elem: Box<Type<'l>>,
		size: Option<Expression<'l>>,
	},

	Template(TemplateCall<'l>),
}

impl Display for Type<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Type::Id(id) => f.write_str(id),
			Type::Ref { kind, is_mut, base } => {
				write!(
					f,
					"&'{}{}{}",
					kind,
					if *is_mut { " mut " } else { " " },
					base
				)
			},
			Type::Template(_) => f.write_str("template<...>"),
			Type::Array { elem, size } => match size {
				None => write!(f, "[{}]", elem),
				Some(size) => write!(f, "[{}; {}]", elem, size),
			},
		}
	}
}

#[derive(Debug)]
pub struct Function<'l> {
	pub signature: FunctionSignature<'l>,
	pub block: Block<'l>,
}

#[derive(Debug)]
pub struct FunctionSignature<'l> {
	pub id: Option<&'l str>,
	pub par: Vec<(&'l str, Type<'l>)>,
	pub ret_ty: Type<'l>,
}

#[derive(Debug)]
pub struct Block<'l> {
	pub statements: Vec<Statement<'l>>,
}

#[derive(Debug)]
pub enum Literal<'l> {
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

#[derive(Debug)]
pub enum Expression<'l> {
	Literal(Literal<'l>),
	FnCall(Box<FnCall<'l>>),
	TemplateCall(Box<TemplateCall<'l>>),
	Unary(UnaryOperator, Box<Expression<'l>>),
	Binary(Box<Expression<'l>>, BinaryOperator, Box<Expression<'l>>),
}

impl Display for Expression<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("<expr>")
	}
}

#[derive(Debug)]
pub enum UnaryOperator {
	Pos,
	Neg,
	Not,
	Deref,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FnCall<'l> {
	pub callee: Expression<'l>,
	pub params: Vec<Expression<'l>>,
}

#[derive(Debug)]
pub struct VariableDeclaration<'l> {
	pub id: &'l str,
	pub is_mut: bool,
	pub is_const: bool,
	pub expr: Expression<'l>,
	pub ty: Option<Type<'l>>,
}

impl<'l> VariableDeclaration<'l> {
	pub fn new(
		id: &'l str, is_mut: bool, is_const: bool, expr: Expression<'l>, ty: Option<Type<'l>>,
	) -> Result<'l, Self> {
		match is_mut && is_const {
			false => Ok(Self {
				id,
				ty,
				is_mut,
				is_const,
				expr,
			}),
			true => Err(ParseError::User {
				error: format!("Constants cannot be mutable. Try `{} := {}`.", id, expr),
			}),
		}
	}
}

#[derive(Debug)]
pub enum SymbolDeclaration<'l> {
	Alias(Alias<'l>),
	Enum(Enum<'l>),
	Union(Union<'l>),
	Struct(Struct<'l>),
	Interface(Interface<'l>),
	Function(Function<'l>),
}

#[derive(Debug)]
pub enum Statement<'l> {
	Block(Block<'l>),
	Expression(Expression<'l>),
	VarDecl(VariableDeclaration<'l>),
	//SymDecl(SymbolDeclaration<'l>),
	Return(Option<Expression<'l>>),
	Yield(Expression<'l>),
	CoYield(Expression<'l>),
}

#[derive(Debug)]
pub struct NamespaceImport<'l>(pub &'l str);

#[derive(Debug)]
pub struct CompilationUnit<'l> {
	pub namespace: &'l str,
	pub imports: Vec<NamespaceImport<'l>>,
	pub declarations: Vec<SymbolDeclaration<'l>>,
}
