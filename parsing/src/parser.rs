use std::fmt::Debug;
use std::ops::Range;
use std::slice::from_ref;

use ariadne::{Color, Config, Label, Report, ReportKind};
use winnow::combinator::{alt, delimited, opt, repeat};
use winnow::error::{ErrMode, ErrorKind};
use winnow::{Parser, PResult};
use winnow::stream::{ContainsToken, Stream};
use winnow::token::one_of;

use crate::ast::*;
use crate::lexer::{Token, TokenData, TokenStream};

#[derive(Debug, Clone, PartialEq)]
pub struct ParserError<'l> {
	pub code: &'static str,
	pub range: Range<usize>,
	pub message: Option<String>,
	pub err_tokens: &'l [TokenData<'l>],
	pub labels: Vec<Label<(&'l str, Range<usize>)>>,
}

impl<'l> ParserError<'l> {
	const UNKNOWN: &'static str = "P0000";
	const UNEXPECTED_EOF: &'static str = "P0002";
	const UNEXPECTED_TOKEN: &'static str = "P0001";

	pub fn to_report(self, file: &'l str) -> Report<'static, (&'l str, Range<usize>)> {
		let error = self.message.as_ref().map(|m| m.as_str()).unwrap_or("Parsing failed");
		let mut builder = Report::build(ReportKind::Error, file, self.range.start)
			.with_config(Config::default().with_underlines(true))
			.with_code(self.code)
			.with_message(error);

		builder = match self.err_tokens {
			[] => builder,
			[token] => builder.with_label(
				Label::new((file, token.range.clone()))
					.with_message(format!("Unexpected token `{}`", token.token))
					.with_color(Color::Red),
			),
			[start, .., end] => builder.with_label(
				Label::new((file, start.range.start..end.range.end))
					.with_message("Unexpected tokens")
					.with_color(Color::Red),
			),
		};

		for label in self.labels {
			builder = builder.with_label(label);
		}

		builder.finish()
	}
}

impl<'l> winnow::error::ParserError<TokenStream<'l>> for ParserError<'l> {
	fn from_error_kind(input: &TokenStream<'l>, _: ErrorKind) -> Self {
		match input.range().len() {
			0 if input.range().end != 0 => Self {
				code: Self::UNKNOWN,
				range: input.tokens()[input.range().end - 1].range.clone(),
				message: None,
				err_tokens: &[],
				labels: vec![],
			},
			0 => Self {
				code: Self::UNKNOWN,
				range: Default::default(),
				message: None,
				err_tokens: &[],
				labels: vec![],
			},
			1 => Self {
				code: Self::UNKNOWN,
				range: input.tokens()[input.range().start].range.clone(),
				err_tokens: &input.tokens()[input.range().clone()],
				message: None,
				labels: vec![],
			},
			_ => Self {
				code: Self::UNKNOWN,
				range: input.tokens()[input.range().start].range.start
					..input.tokens()[input.range().end - 1].range.end,
				err_tokens: &input.tokens()[input.range().clone()],
				message: None,
				labels: vec![],
			},
		}
	}

	fn append(
		mut self,
		input: &TokenStream<'l>,
		_: &<TokenStream<'l> as Stream>::Checkpoint,
		kind: ErrorKind,
	) -> Self {
		let mut error = Self::from_error_kind(input, kind);
		self.labels.extend(error.labels);
		error.labels = self.labels;
		if error.range.is_empty() {
			error.range = self.range;
		}
		error
	}
}

pub trait Parse<'l>
	where
		Self: Sized,
{
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>>;
}

impl<'l> Parse<'l> for Ident<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		match input.next_token() {
			Some(TokenData {
					 token: Token::Ident(ident),
					 range,
					 ..
				 }) => Ok(Ident {
				value: ident,
				range: range.clone(),
			}),
			None => return Err(unexpected_eof(input, None, false)),
			Some(data) => Err(ErrMode::Backtrack(ParserError {
				code: ParserError::UNEXPECTED_TOKEN,
				range: data.range.clone(),
				message: None,
				err_tokens: from_ref(data),
				labels: vec![],
			})),
		}
	}
}

impl<'l> Parse<'l> for Literal<'l> {
	#[rustfmt::skip]
	fn parse(input: &mut TokenStream<'l>) -> PResult<Literal<'l>, ParserError<'l>> {
		let literal = match input.next_token() {
			Some(data) => match data.token {
				Token::Ident(v) => Literal::Id(Ident { value: v, range: data.range.clone() }),
				Token::Int(v) => Literal::Integer { value: Integer::Any(v), range: data.range.clone() },
				Token::Int8(v) => Literal::Integer { value: Integer::Int8(v), range: data.range.clone() },
				Token::Int16(v) => Literal::Integer { value: Integer::Int16(v), range: data.range.clone() },
				Token::Int32(v) => Literal::Integer { value: Integer::Int32(v), range: data.range.clone() },
				Token::Int64(v) => Literal::Integer { value: Integer::Int64(v), range: data.range.clone() },
				Token::UInt8(v) => Literal::Integer { value: Integer::UInt8(v), range: data.range.clone() },
				Token::UInt16(v) => Literal::Integer { value: Integer::UInt16(v), range: data.range.clone() },
				Token::UInt32(v) => Literal::Integer { value: Integer::UInt32(v), range: data.range.clone() },
				Token::UInt64(v) => Literal::Integer { value: Integer::UInt64(v), range: data.range.clone() },
				Token::Float(v) => Literal::Float { value: v, range: data.range.clone() },
				Token::Float16(_v) => unimplemented!(),
				Token::Float32(_v) => unimplemented!(),
				Token::Float64(_v) => unimplemented!(),
				Token::CharLiteral(v) => Literal::Char { value: v, range: data.range.clone() },
				Token::BoolLiteral(v) => Literal::Bool { value: v, range: data.range.clone() },
				Token::StringLiteral(v) => Literal::String { value: &v[1..v.len() - 1], range: data.range.clone() },
				_ => {
					return Err(ErrMode::Backtrack(ParserError {
						code: ParserError::UNEXPECTED_TOKEN,
						range: data.range.clone(),
						message: None,
						err_tokens: from_ref(data),
						labels: vec![],
					}));
				}
			},
			None => return Err(unexpected_eof(input, None, false)),
		};
		Ok(literal)
	}
}

impl<'l> Parse<'l> for Expression<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let mut expr = Self::term(input)?;
		loop {
			let ops = one_of([
				Token::Add,
				Token::Sub,
				Token::Eq,
				Token::Ne,
				Token::Lt,
				Token::Gt,
				Token::Le,
				Token::Ge,
			]);
			let start = input.checkpoint();
			let len = input.eof_offset();

			match (ops, Self::term).parse_next(input) {
				Ok((op, term)) => {
					// infinite loop check: the parser must always consume
					if input.eof_offset() == len {
						return Err(unexpected_eof(input, None, true));
					}

					expr = match op.token {
						Token::Add => expr + term,
						Token::Sub => expr - term,
						Token::Eq => Expression::Binary {
							range: expr.range().start..term.range().end,
							operator: BinaryOperator::Eq,
							lhs: expr.into(),
							rhs: term.into(),
						},
						Token::Lt => Expression::Binary {
							range: expr.range().start..term.range().end,
							operator: BinaryOperator::Lt,
							lhs: expr.into(),
							rhs: term.into(),
						},
						Token::Gt => Expression::Binary {
							range: expr.range().start..term.range().end,
							operator: BinaryOperator::Gt,
							lhs: expr.into(),
							rhs: term.into(),
						},
						Token::Le => Expression::Binary {
							range: expr.range().start..term.range().end,
							operator: BinaryOperator::Le,
							lhs: expr.into(),
							rhs: term.into(),
						},
						Token::Ge => Expression::Binary {
							range: expr.range().start..term.range().end,
							operator: BinaryOperator::Ge,
							lhs: expr.into(),
							rhs: term.into(),
						},
						_ => unreachable!(),
					}
				}
				Err(ErrMode::Backtrack(_)) => {
					input.reset(&start);
					return Ok(expr);
				}
				Err(e) => {
					return Err(e);
				}
			}
		}
	}
}

impl<'l> Expression<'l> {
	fn term(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let mut expr = Self::factor(input)?;

		loop {
			let checkpoint = input.checkpoint();
			let start = input.start_char();
			let len = input.eof_offset();

			let op = match one_of([
				Token::Mul, Token::Div, Token::Mod,
				Token::OpenRound, Token::Period, Token::DoubleColon,
			]).parse_next(input) {
				Ok(op) => op,
				Err(ErrMode::Backtrack(_)) => {
					input.reset(&checkpoint);
					return Ok(expr);
				}
				Err(e) => {
					return Err(e);
				}
			};

			match op.token {
				Token::OpenRound => {
					let params = required(list(Token::Comma, Expression::parse)).parse_next(input)?;
					required(Token::CloseRound).parse_next(input)?;
					let end = input.start_char();

					if input.eof_offset() == len {
						unimplemented!()
					}

					expr = Self::FunctionCall(Box::new(FunctionCall {
						func: expr,
						params,
						range: start..end,
					}));
				}
				_ => match Self::factor(input) {
					Ok(term) => {
						// infinite loop check: the parser must always consume
						if input.eof_offset() == len {
							unimplemented!()
						}
						expr = match op.token {
							Token::Mul => expr * term,
							Token::Div => expr / term,
							Token::Mod => expr % term,
							Token::Period => Expression::Binary {
								operator: BinaryOperator::Access,
								range: expr.range().start..term.range().end,
								lhs: Box::new(expr),
								rhs: Box::new(term),
							},
							Token::DoubleColon => Expression::Binary {
								operator: BinaryOperator::StaticAccess,
								range: expr.range().start..term.range().end,
								lhs: Box::new(expr),
								rhs: Box::new(term),
							},
							_ => unreachable!(),
						}
					}
					Err(ErrMode::Backtrack(_)) => {
						input.reset(&checkpoint);
						return Ok(expr);
					}
					Err(e) => {
						return Err(e);
					}
				}
			}
		}
	}

	fn factor(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		alt((
			NewStruct::parse.map(|v| Expression::NewStruct(v)),
			Literal::parse.map(|l| Expression::Literal(l)),
			delimited(Token::OpenRound, Expression::parse, Token::CloseRound),
		))
			.parse_next(input)
	}
}

impl<'l> Parse<'l> for NewStruct<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let start = input.start_char();
		let id = Ident::parse(input)?;

		let members = delimited(
			Token::OpenCurly,
			list(
				Token::Comma,
				(
					Ident::parse,
					required(Token::Colon),
					required(Expression::parse),
				)
					.map(|(id, _, expr)| (id, expr)),
			),
			required(Token::CloseCurly),
		)
			.parse_next(input)?;

		let end = input.start_char();
		Ok(Self {
			ty: Type::Id(id),
			range: start..end,
			values: members,
		})
	}
}

impl<'l> Parse<'l> for Type<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let start = input.start_char();
		let ty = alt((
			(Token::Mul, Token::Mut, required(Type::parse))
				.map(|(_, _, t)| Type::Pointer(t.into(), true)),
			(Token::Mul, required(Type::parse)).map(|(_, t)| Type::Pointer(t.into(), false)),
			delimited(
				Token::OpenSquare,
				(
					required(Type::parse),
					required(Token::SemiColon),
					required(Expression::parse),
				),
				Token::CloseSquare,
			)
				.map(|(ty, _, exp)| Type::Array {
					base: ty.into(),
					length: Some(exp.into()),
				}),
			required(Ident::parse).map(|id| Type::Id(id)),
		))
			.parse_next(input);

		match ty {
			Ok(ty) => Ok(ty),
			Err(err) => match err {
				ErrMode::Backtrack(mut err) | ErrMode::Cut(mut err) => {
					err.labels.push(
						Label::new((input.file_name(), start..err.range.end))
							.with_message("Invalid type")
							.with_color(Color::Red),
					);
					Err(ErrMode::Cut(err))
				}
				_ => Err(err),
			},
		}
	}
}

impl<'l> Parse<'l> for Block<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let start = input.start_char();
		let statements = delimited(
			Token::OpenCurly,
			repeat(0.., Statement::parse),
			Token::CloseCurly,
		)
			.parse_next(input)?;
		let end = input.start_char();
		Ok(Self {
			statements,
			range: start..end,
		})
	}
}

impl<'l> Parse<'l> for Statement<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		alt((
			(VarDecl::parse, required(Token::SemiColon)).map(|(v, _)| Statement::VarDecl(v)),
			If::parse.map(|v| Statement::If(v)),
			While::parse.map(|v| Statement::While(v)),
			(
				Expression::parse,
				Token::Equal,
				Expression::parse,
				required(Token::SemiColon),
			)
				.map(|(lhs, _, rhs, _)| Statement::Assignment(lhs, rhs)),
			(
				Token::Return,
				opt(Expression::parse),
				required(Token::SemiColon),
			)
				.map(|(ret, expr, _)| match &expr {
					None => Statement::Return {
						expr,
						range: ret.range.clone(),
					},
					Some(e) => Statement::Return {
						range: ret.range.start..e.range().end,
						expr,
					},
				}),
			(Expression::parse, opt(Token::SemiColon))
				.map(|(expr, _)| Statement::Expression(expr))
		))
			.parse_next(input)
	}
}

impl<'l> Parse<'l> for VarDecl<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::Let.parse_next(input)?;
		let mutable = opt(Token::Mut).parse_next(input)?.is_some();
		let name = required(Ident::parse).parse_next(input)?;

		let (ty, value) = alt((
			(Token::Colon, Type::parse, Token::Equal, Expression::parse)
				.map(|(_, ty, _, val)| (Some(ty), val)),
			(Token::Equal, Expression::parse).map(|(_, val)| (None, val)),
			(
				required(Token::Colon),
				Type::parse,
				Token::Equal,
				Token::QuestionMark,
			).map(|(_, ty, _, val)| {
				(
					Some(ty),
					Expression::Literal(Literal::Uninit {
						range: val.range.clone(),
					}),
				)
			}),
		)).parse_next(input)?;

		Ok(Self {
			ty,
			name,
			mutable,
			value,
		})
	}
}

impl<'l> Parse<'l> for If<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::If.parse_next(input)?;

		let condition = required(Expression::parse).parse_next(input)?;
		let block = required(Block::parse).parse_next(input)?;

		let r#else = opt(alt((
			(Token::Else, If::parse).map(|(_, v)| Else::If(v.into())),
			(Token::Else, required(Block::parse)).map(|(_, v)| Else::Block(v)),
		)))
			.parse_next(input)?;

		Ok(Self {
			condition,
			block,
			r#else,
		})
	}
}

impl<'l> Parse<'l> for While<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::While.parse_next(input)?;
		let condition = required(Expression::parse).parse_next(input)?;
		let block = required(Block::parse).parse_next(input)?;
		Ok(Self { condition, block })
	}
}

impl<'l> Parse<'l> for SymbolDeclaration<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let start = input.start_char();
		let public = (opt(Token::Pub), Token::Def).parse_next(input)?.0.is_some();
		let (name, _) = (required(Ident::parse), Token::Colon).parse_next(input)?;

		let mut symbol = alt((
			Struct::parse.map(|s| Symbol::Struct(s)),
			Function::parse.map(|f| Symbol::Function(f)),
		))
			.parse_next(input);
		let end = input.start_char();

		if symbol.is_ok() {
			return Ok(Self {
				public,
				name,
				symbol: symbol.unwrap(),
				attributes: vec![],
				range: start..end,
			});
		}

		match &mut symbol {
			Err(ErrMode::Backtrack(err) | ErrMode::Cut(err)) => {
				err.labels.push(
					Label::new((input.file_name(), name.range.clone()))
						.with_message(format!("In symbol declaration `{name}`"))
						.with_color(Color::Cyan),
				);
			}
			_ => {}
		}
		Err(symbol.unwrap_err())
	}
}

impl<'l> Parse<'l> for Struct<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::Struct.parse_next(input)?;

		let members = delimited(
			required(Token::OpenCurly),
			list(Token::Comma, StructMember::parse),
			required(Token::CloseCurly),
		)
			.parse_next(input)?;

		Ok(Self { members })
	}
}

impl<'l> Parse<'l> for StructMember<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		match (Ident::parse, required(Token::Colon), required(Type::parse)).parse_next(input) {
			Ok((name, _, ty)) => Ok(Self { name, ty }),
			Err(err) => Err(err),
		}
	}
}

impl<'l> Parse<'l> for Function<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let params = delimited(
			Token::OpenRound,
			list(Token::Comma, FunctionParameter::parse),
			required(Token::CloseRound),
		)
			.parse_next(input)?;

		Token::ThinArrow.parse_next(input)?;
		let return_ty = required(Type::parse).parse_next(input)?;

		let block = alt((
			Token::SemiColon.map(|_| None),
			required(Block::parse).map(|b| Some(b)),
		))
			.parse_next(input)?;

		Ok(Self {
			params,
			return_ty,
			block,
		})
	}
}

impl<'l> Parse<'l> for FunctionParameter<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		match (Ident::parse, required(Token::Colon), required(Type::parse)).parse_next(input) {
			Ok((name, _, ty)) => Ok(Self { name, ty }),
			Err(err) => Err(err),
		}
	}
}

impl<'l> Parse<'l> for CompilationUnit<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::Namespace.parse_next(input)?;
		let namespace = match input.next_token() {
			None => return Err(unexpected_eof(input, None, true)),
			Some(data) => match data.token {
				Token::NamespaceLiteral(ns) => ns,
				_ => {
					return Err(ErrMode::Backtrack(ParserError {
						code: ParserError::UNEXPECTED_TOKEN,
						range: data.range.clone(),
						message: None,
						err_tokens: from_ref(data),
						labels: vec![],
					}));
				}
			},
		};
		Token::SemiColon.parse_next(input)?;

		let declarations = repeat(0.., SymbolDeclaration::parse).parse_next(input)?;

		Ok(Self {
			namespace,
			declarations,
			imports: vec![],
		})
	}
}

fn list<'l, T, O>(
	mut separator: Token<'l>,
	mut parser: T,
) -> impl Parser<TokenStream<'l>, Vec<O>, ParserError<'l>>
	where
		T: Parser<TokenStream<'l>, O, ParserError<'l>>,
{
	return move |input: &mut TokenStream<'l>| {
		let mut vec = vec![];
		loop {
			let mut checkpoint = input.checkpoint();
			match parser.parse_next(input) {
				Ok(e) => vec.push(e),
				Err(ErrMode::Backtrack(_)) => {
					input.reset(&checkpoint);
					break;
				}
				Err(err) => {
					return Err(err);
				}
			}
			checkpoint = input.checkpoint();
			match separator.parse_next(input) {
				Ok(_) => {}
				Err(ErrMode::Backtrack(_)) => {
					input.reset(&checkpoint);
					break;
				}
				Err(err) => {
					return Err(err);
				}
			}
		}
		Ok(vec)
	};
}

impl<'l> Parser<TokenStream<'l>, &'l TokenData<'l>, ParserError<'l>> for Token<'l> {
	fn parse_next(
		&mut self,
		input: &mut TokenStream<'l>,
	) -> PResult<&'l TokenData<'l>, ParserError<'l>> {
		let checkpoint = input.checkpoint();
		match input.next_token() {
			None => Err(unexpected_eof(input, Some(self), false)),
			Some(data) => match data.token == *self {
				true => Ok(data),
				false => {
					input.reset(&checkpoint);
					Err(ErrMode::Backtrack(ParserError {
						code: ParserError::UNEXPECTED_TOKEN,
						range: data.range.clone(),
						message: None,
						err_tokens: from_ref(data),
						labels: vec![],
					}))
				}
			},
		}
	}
}

pub fn required<'l, O, T>(mut parser: T) -> impl Parser<TokenStream<'l>, O, ParserError<'l>>
	where
		T: Parser<TokenStream<'l>, O, ParserError<'l>>,
{
	move |input: &mut TokenStream<'l>| match parser.parse_next(input) {
		Ok(ok) => Ok(ok),
		Err(ErrMode::Incomplete(v)) => Err(ErrMode::Incomplete(v)),
		Err(ErrMode::Cut(err) | ErrMode::Backtrack(err)) => Err(ErrMode::Cut(err)),
	}
}

impl<'l, const LEN: usize> ContainsToken<&'l TokenData<'l>> for [Token<'l>; LEN] {
	fn contains_token(&self, token: &'l TokenData<'l>) -> bool {
		self.contains(&token.token)
	}
}

fn unexpected_eof<'l>(
	stream: &TokenStream<'l>,
	expected: Option<&Token>,
	cut: bool,
) -> ErrMode<ParserError<'l>> {
	let range = stream
		.tokens()
		.get(stream.range().end.wrapping_sub(1))
		.map(|t| t.range.clone())
		.unwrap_or_default();

	let error = ParserError {
		code: ParserError::UNEXPECTED_EOF,
		range: stream
			.tokens()
			.get(stream.range().end.wrapping_sub(1))
			.map(|t| t.range.clone())
			.unwrap_or_default(),

		message: Some("Unexpected end of stream".to_string()),
		err_tokens: &[],
		labels: vec![Label::new((stream.file_name(), range.end..range.end))
			.with_message(match expected {
				None => "Expected additional tokens here".to_string(),
				Some(token) => format!("Expected token `{token}`, found EOF"),
			})
			.with_color(Color::Red)],
	};

	match cut {
		true => ErrMode::Cut(error),
		false => ErrMode::Backtrack(error),
	}
}
