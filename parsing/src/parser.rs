use std::error::Error;
use std::fmt::Debug;
use std::iter::Enumerate;
use std::ops::Range;
use std::slice::{from_ref, Iter};
use std::str::FromStr;

use ariadne::{Color, Config, Label, Report, ReportKind};
use derive_more::Display;
use logos::{Lexer, Logos};
use winnow::{Parser, PResult};
use winnow::combinator::{alt, delimited, opt, repeat};
use winnow::error::{ErrMode, ErrorKind, Needed};
use winnow::stream::{ContainsToken, Offset, Stream, StreamIsPartial};
use winnow::token::one_of;
use crate::ast::*;

fn parse_token<'l, T: FromStr>(lex: &mut Lexer<'l, Token<'l>>) -> Result<T, LexerError>
	where
		<T as FromStr>::Err: Error,
{
	match lex.slice().parse::<T>() {
		Ok(value) => Ok(value),
		Err(err) => Err(LexerError {
			code: "L0002",
			range: lex.span(),
			message: Some(err.to_string()),
		}),
	}
}

fn parse_token_skip_end_2<'l, T: FromStr>(lex: &mut Lexer<'l, Token<'l>>) -> Result<T, LexerError>
	where
		<T as FromStr>::Err: Error,
{
	match lex.slice()[0..lex.slice().len() - 2].parse::<T>() {
		Ok(value) => Ok(value),
		Err(err) => Err(LexerError {
			code: "L0002",
			range: lex.span(),
			message: Some(err.to_string()),
		}),
	}
}

fn parse_token_skip_end_3<'l, T: FromStr>(lex: &mut Lexer<'l, Token<'l>>) -> Result<T, LexerError>
	where
		<T as FromStr>::Err: Error,
{
	match lex.slice()[0..lex.slice().len() - 3].parse::<T>() {
		Ok(value) => Ok(value),
		Err(err) => Err(LexerError {
			code: "L0002",
			range: lex.span(),
			message: Some(err.to_string()),
		}),
	}
}

pub fn unescape_char<'l>(lex: &mut Lexer<'l, Token<'l>>) -> Result<char, LexerError> {
	let s = &lex.slice()[1..(lex.slice().len() - 1)];
	match unescape::unescape(s) {
		Some(esc) => Ok(esc.chars().next().unwrap()),
		None => Err(LexerError {
			code: "L0003",
			range: lex.span(),
			message: Some(format!("{} is not a valid character.", s)),
		}),
	}
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct LexerError {
	pub code: &'static str,
	pub range: Range<usize>,
	pub message: Option<String>,
}

#[rustfmt::skip]
#[derive(Logos, Debug, Clone, PartialEq, Display)]
#[logos(
skip r"[ \t\n\f\r]+",
skip r"//.*\n",
error = LexerError)
]
pub enum Token<'l> {
	#[token("(")]
	#[display("(")]
	OpenRound,
	#[token(")")]
	#[display(")")]
	CloseRound,
	#[token("[")]
	#[display("[")]
	OpenSquare,
	#[token("]")]
	#[display("]")]
	CloseSquare,
	#[token("{")]
	#[display("{{")]
	OpenCurly,
	#[token("}")]
	#[display("}}")]
	CloseCurly,
	#[token("?")]
	#[display("?")]
	QuestionMark,
	#[token(":")]
	#[display(":")]
	Colon,
	#[token(";")]
	#[display(";")]
	SemiColon,
	#[token(".")]
	#[display(".")]
	Period,
	#[token(",")]
	#[display(",")]
	Comma,
	#[token("=")]
	#[display("=")]
	Equal,
	#[token("::")]
	#[display("::")]
	DoubleColon,
	#[token("->")]
	#[display("->")]
	ThinArrow,

	#[token("+")]
	#[display("+")]
	Add,
	#[token("-")]
	#[display("-")]
	Sub,
	#[token("*")]
	#[display("*")]
	Mul,
	#[token("/")]
	#[display("/")]
	Div,
	#[token("%")]
	#[display("%")]
	Mod,
	#[token("!")]
	#[display("!")]
	Not,
	#[token("==")]
	#[display("==")]
	Eq,
	#[token("!=")]
	#[display("!=")]
	Ne,
	#[token("<")]
	#[display("<")]
	Lt,
	#[token(">")]
	#[display(">")]
	Gt,
	#[token("<=")]
	#[display("<=")]
	Le,
	#[token(">=")]
	#[display(">=")]
	Ge,

	#[token("@")]
	#[display("@")]
	At,
	#[token("as")]
	#[display("as")]
	As,
	#[token("pub")]
	#[display("pub")]
	Pub,
	#[token("def")]
	#[display("def")]
	Def,
	#[token("mut")]
	#[display("mut")]
	Mut,
	#[token("let")]
	#[display("let")]
	Let,
	#[token("struct")]
	#[display("struct")]
	Struct,
	#[token("enum")]
	#[display("enum")]
	Enum,
	#[token("union")]
	#[display("union")]
	Union,
	#[token("if")]
	#[display("if")]
	If,
	#[token("else")]
	#[display("else")]
	Else,
	#[token("for")]
	#[display("for")]
	For,
	#[token("while")]
	#[display("while")]
	While,
	#[token("loop")]
	#[display("loop")]
	Loop,
	#[token("return")]
	#[display("return")]
	Return,
	#[token("yield")]
	#[display("yield")]
	Yield,
	#[token("break")]
	#[display("break")]
	Break,
	#[token("namespace")]
	#[display("namespace")]
	Namespace,
	#[token("import")]
	#[display("import")]
	Import,

	#[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
	#[display("{}", _0)]
	Ident(&'l str),
	#[regex("[a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)+")]
	#[display("{}", _0)]
	NamespaceLiteral(&'l str),

	#[regex("'(\\.|[^\\'])'", unescape_char)]
	#[display("{}", _0)]
	CharLiteral(char),
	#[regex("\"(\\.|[^\"])*\"")]
	#[display("{}", _0)]
	StringLiteral(&'l str),
	#[token("true", parse_token)]
	#[token("false", parse_token)]
	#[display("{}", _0)]
	BoolLiteral(bool),
	#[regex("-?[0-9]+", parse_token)]
	// #[regex("-?0x[0-9a-fA-F]+", parse)]
	#[display("{}", _0)]
	Int(i128),
	#[regex("-?[0-9]+i8", parse_token_skip_end_2)]
	// #[regex("-?0x[0-9a-fA-F]+i8")]
	#[display("{}i8", _0)]
	Int8(i8),
	#[regex("-?[0-9]+i16", parse_token_skip_end_3)]
	// #[regex("-?0x[0-9a-fA-F]+i16")]
	#[display("{}i16", _0)]
	Int16(i16),
	#[regex("-?[0-9]+i32", parse_token_skip_end_3)]
	// #[regex("-?0x[0-9a-fA-F]+i32")]
	#[display("{}i32", _0)]
	Int32(i32),
	#[regex("-?[0-9]+i64", parse_token_skip_end_3)]
	// #[regex("-?0x[0-9a-fA-F]+i64")]
	#[display("{}i64", _0)]
	Int64(i64),
	#[regex("[0-9]+u8", parse_token_skip_end_2)]
	// #[regex("0x[0-9a-fA-F]+u8")]
	#[display("{}u8", _0)]
	UInt8(u8),
	#[regex("[0-9]+u16", parse_token_skip_end_3)]
	// #[regex("0x[0-9a-fA-F]+u16")]
	#[display("{}u16", _0)]
	UInt16(u16),
	#[regex("[0-9]+u32", parse_token_skip_end_3)]
	// #[regex("0x[0-9a-fA-F]+u32")]
	#[display("{}u32", _0)]
	UInt32(u32),
	#[regex("[0-9]+u64", parse_token_skip_end_3)]
	// #[regex("0x[0-9a-fA-F]+u64")]
	#[display("{}u64", _0)]
	UInt64(u64),
	#[regex("-?[0-9]*\\.[0-9]+", parse_token)]
	#[display("{}", _0)]
	Float(f64),
	#[regex("-?[0-9]*\\.[0-9]+f16", parse_token_skip_end_3)]
	#[display("{}f16", _0)]
	Float16(f32),
	#[regex("-?[0-9]*\\.[0-9]+f32", parse_token_skip_end_3)]
	#[display("{}f32", _0)]
	Float32(f32),
	#[regex("-?[0-9]*\\.[0-9]+f64", parse_token_skip_end_3)]
	#[display("{}f64", _0)]
	Float64(f64),
}

impl<'l> Token<'l> {
	pub fn lex_all(text: &'l str) -> Result<Vec<TokenData<'l>>, LexerError> {
		let mut tokens = vec![];
		let mut lexer = Token::lexer(text);
		while let Some(token) = lexer.next() {
			let token = match token {
				Ok(token) => token,
				Err(err) if err == LexerError::default() => {
					return Err(LexerError {
						code: "L0001",
						range: lexer.span(),
						message: match ExactSizeIterator::len(&lexer.span()) {
							1 => Some(format!("Unexpected char `{}`", &text[lexer.span()])),
							_ => Some(format!(
								"Unexpected char sequence `{}`",
								&text[lexer.span()]
							)),
						},
					});
				}
				Err(err) => return Err(err),
			};
			tokens.push(TokenData {
				token,
				text: lexer.slice(),
				range: lexer.span(),
			});
		}
		Ok(tokens)
	}
}

impl LexerError {
	pub fn to_report<'l>(self, file: &'l str) -> Report<'static, (&'l str, Range<usize>)> {
		Report::build(ReportKind::Error, file, self.range.start)
			.with_code(self.code)
			.with_message("Lexing failed")
			.with_label(
				Label::new((file, self.range.clone())).with_color(Color::Red).with_message(
					self.message.unwrap_or_else(|| {
						"The following input generated a lexer error".to_string()
					}),
				),
			)
			.finish()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenData<'l> {
	text: &'l str,
	token: Token<'l>,
	range: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct TokenStream<'l> {
	file_name: &'l str,
	range: Range<usize>,
	tokens: &'l [TokenData<'l>],
}

impl<'l> TokenStream<'l> {
	pub fn new(file_name: &'l str, tokens: &'l [TokenData<'l>]) -> Self {
		Self {
			tokens,
			file_name,
			range: 0..tokens.len(),
		}
	}

	pub fn start_char(&self) -> usize {
		if let Some(token) = self.tokens.get(self.range.start) {
			token.range.start
		} else {
			0
		}
	}
}

impl<'l> Offset<<Self as Stream>::Checkpoint> for TokenStream<'l> {
	fn offset_from(&self, start: &<Self as Stream>::Checkpoint) -> usize {
		self.range.start - start.range.start
	}
}

impl<'l> Stream for TokenStream<'l> {
	type Token = &'l TokenData<'l>;
	type Slice = &'l [TokenData<'l>];
	type IterOffsets = Enumerate<Iter<'l, TokenData<'l>>>;
	type Checkpoint = Self;

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn iter_offsets(&self) -> Self::IterOffsets {
		self.tokens[self.range.clone()].iter().enumerate()
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn eof_offset(&self) -> usize {
		self.range.len()
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn next_token(&mut self) -> Option<Self::Token> {
		if self.range.is_empty() {
			return None;
		}
		let i = self.range.start + 1;
		let i = std::mem::replace(&mut self.range.start, i);
		Some(&self.tokens[i])
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn offset_for<P>(&self, predicate: P) -> Option<usize>
		where
			P: Fn(Self::Token) -> bool,
	{
		self.tokens[self.range.clone()].iter().position(predicate)
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn offset_at(&self, _: usize) -> Result<usize, Needed> {
		unimplemented!()
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn next_slice(&mut self, offset: usize) -> Self::Slice {
		let (_, next) = self.tokens.split_at(offset);
		self.range.start = offset;
		next
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn checkpoint(&self) -> Self::Checkpoint {
		self.clone()
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn reset(&mut self, checkpoint: &Self::Checkpoint) {
		*self = checkpoint.clone();
	}

	#[cfg_attr(not(debug_assertions), inline(always))]
	fn raw(&self) -> &dyn Debug {
		self
	}
}

impl<'l> StreamIsPartial for TokenStream<'l> {
	type PartialState = ();
	fn complete(&mut self) -> Self::PartialState {}
	fn restore_partial(&mut self, _: Self::PartialState) {}
	fn is_partial_supported() -> bool {
		false
	}
}

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
		match input.range.len() {
			0 if input.range.end != 0 => Self {
				code: Self::UNKNOWN,
				range: input.tokens[input.range.end - 1].range.clone(),
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
				range: input.tokens[input.range.start].range.clone(),
				err_tokens: &input.tokens[input.range.clone()],
				message: None,
				labels: vec![],
			},
			_ => Self {
				code: Self::UNKNOWN,
				range: input.tokens[input.range.start].range.start
					..input.tokens[input.range.end - 1].range.end,
				err_tokens: &input.tokens[input.range.clone()],
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

impl<'l> Parse<'l> for Literal<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Literal<'l>, ParserError<'l>> {
		let literal = match input.next_token() {
			Some(data) => match data.token {
				Token::Ident(v) => Literal::Id(v),
				Token::Int(v) => Literal::Integer(Integer::Any(v)),
				Token::Int8(v) => Literal::Integer(Integer::Int8(v)),
				Token::Int16(v) => Literal::Integer(Integer::Int16(v)),
				Token::Int32(v) => Literal::Integer(Integer::Int32(v)),
				Token::Int64(v) => Literal::Integer(Integer::Int64(v)),
				Token::UInt8(v) => Literal::Integer(Integer::UInt8(v)),
				Token::UInt16(v) => Literal::Integer(Integer::UInt16(v)),
				Token::UInt32(v) => Literal::Integer(Integer::UInt32(v)),
				Token::UInt64(v) => Literal::Integer(Integer::UInt64(v)),
				Token::Float(v) => Literal::Float(v),
				Token::Float16(_v) => unimplemented!(),
				Token::Float32(_v) => unimplemented!(),
				Token::Float64(_v) => unimplemented!(),
				Token::CharLiteral(v) => Literal::Char(v),
				Token::BoolLiteral(v) => Literal::Bool(v),
				Token::StringLiteral(v) => Literal::String(v),
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
						Token::Eq => {
							Expression::Binary(expr.into(), BinaryOperator::Eq, term.into())
						}
						Token::Lt => {
							Expression::Binary(expr.into(), BinaryOperator::Lt, term.into())
						}
						Token::Gt => {
							Expression::Binary(expr.into(), BinaryOperator::Gt, term.into())
						}
						Token::Le => {
							Expression::Binary(expr.into(), BinaryOperator::Le, term.into())
						}
						Token::Ge => {
							Expression::Binary(expr.into(), BinaryOperator::Ge, term.into())
						}
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
			let ops = one_of([Token::Mul, Token::Div, Token::Mod]);
			let start = input.checkpoint();
			let len = input.eof_offset();

			match (ops, Self::factor).parse_next(input) {
				Ok((op, term)) => {
					// infinite loop check: the parser must always consume
					if input.eof_offset() == len {
						unimplemented!()
					}
					expr = match op.token {
						Token::Mul => expr * term,
						Token::Div => expr / term,
						Token::Mod => expr % term,
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
		let id = ident(false).parse_next(input)?;

		let members = delimited(
			Token::OpenCurly,
			list(
				Token::Comma,
				(
					ident(false),
					required(Token::Colon),
					required(Expression::parse),
				)
					.map(|(id, _, expr)| (id, expr)),
			),
			required(Token::CloseCurly),
		)
			.parse_next(input)?;

		Ok(Self::new(Type::Id(id), members))
	}
}

impl<'l> Parse<'l> for FunctionCall<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		let func = Expression::parse(input)?;
		let params = delimited(
			Token::OpenRound,
			list(Token::Comma, Expression::parse),
			required(Token::CloseRound),
		)
			.parse_next(input)?;
		Ok(Self { func, params })
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
			ident(true).map(|id| Type::Id(id)),
		))
			.parse_next(input);

		match ty {
			Ok(ty) => Ok(ty),
			Err(err) => match err {
				ErrMode::Backtrack(mut err) | ErrMode::Cut(mut err) => {
					err.labels.push(
						Label::new((input.file_name, start..err.range.end))
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
		let statements = delimited(
			Token::OpenCurly,
			repeat(0.., Statement::parse),
			Token::CloseCurly,
		)
			.parse_next(input)?;
		Ok(Self { statements })
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
				.map(|(_, expr, _)| Statement::Return(expr)),
			(FunctionCall::parse, required(Token::SemiColon))
				.map(|(c, _)| Statement::Expression(Expression::FunctionCall(c.into()))),
		))
			.parse_next(input)
	}
}

impl<'l> Parse<'l> for VarDecl<'l> {
	fn parse(input: &mut TokenStream<'l>) -> PResult<Self, ParserError<'l>> {
		Token::Let.parse_next(input)?;
		let mutable = opt(Token::Mut).parse_next(input)?.is_some();
		let name = ident(true).parse_next(input)?;

		let (ty, value) = alt((
			(Token::Colon, Type::parse, Token::Equal, Expression::parse)
				.map(|(_, ty, _, val)| (Some(ty), val)),
			(Token::Equal, Expression::parse).map(|(_, val)| (None, val)),
			(
				required(Token::Colon),
				Type::parse,
				Token::Equal,
				Token::QuestionMark,
			)
				.map(|(_, ty, _, _)| (Some(ty), Expression::Literal(Literal::Uninit))),
		))
			.parse_next(input)?;

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
		let (name, _) = (ident(true), Token::Colon).parse_next(input)?;
		let end = input.start_char();

		let mut symbol = alt((
			Struct::parse.map(|s| Symbol::Struct(s)),
			Function::parse.map(|f| Symbol::Function(f)),
		))
			.parse_next(input);

		if symbol.is_ok() {
			return Ok(Self {
				public,
				name,
				symbol: symbol.unwrap(),
				attributes: vec![],
			});
		}

		match &mut symbol {
			Err(ErrMode::Backtrack(err) | ErrMode::Cut(err)) => {
				err.labels.push(
					Label::new((input.file_name, start..end))
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
		match (ident(false), required(Token::Colon), required(Type::parse)).parse_next(input) {
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
		match (ident(false), required(Token::Colon), required(Type::parse)).parse_next(input) {
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

fn ident<'l>(required: bool) -> impl Parser<TokenStream<'l>, &'l str, ParserError<'l>> {
	return move |input: &mut TokenStream<'l>| match input.next_token() {
		Some(TokenData {
				 token: Token::Ident(ident),
				 ..
			 }) => Ok(*ident),
		None => return Err(unexpected_eof(input, None, required)),
		Some(data) => match required {
			false => Err(ErrMode::Backtrack(ParserError {
				code: ParserError::UNEXPECTED_TOKEN,
				range: data.range.clone(),
				message: None,
				err_tokens: from_ref(data),
				labels: vec![],
			})),
			true => Err(ErrMode::Cut(ParserError {
				code: ParserError::UNEXPECTED_TOKEN,
				range: data.range.clone(),
				message: None,
				err_tokens: from_ref(data),
				labels: vec![],
			})),
		},
	};
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

impl<'l> Parser<TokenStream<'l>, &'l Token<'l>, ParserError<'l>> for Token<'l> {
	fn parse_next(
		&mut self,
		input: &mut TokenStream<'l>,
	) -> PResult<&'l Token<'l>, ParserError<'l>> {
		match input.next_token() {
			None => Err(unexpected_eof(input, Some(self), false)),
			Some(data) => match data.token == *self {
				true => Ok(&data.token),
				false => Err(ErrMode::Backtrack(ParserError {
					code: ParserError::UNEXPECTED_TOKEN,
					range: data.range.clone(),
					message: None,
					err_tokens: from_ref(data),
					labels: vec![],
				})),
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
		.tokens
		.get(stream.range.end.wrapping_sub(1))
		.map(|t| t.range.clone())
		.unwrap_or_default();

	let error = ParserError {
		code: ParserError::UNEXPECTED_EOF,
		range: stream
			.tokens
			.get(stream.range.end.wrapping_sub(1))
			.map(|t| t.range.clone())
			.unwrap_or_default(),

		message: Some("Unexpected end of stream".to_string()),
		err_tokens: &[],
		labels: vec![Label::new((stream.file_name, range.end..range.end))
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
