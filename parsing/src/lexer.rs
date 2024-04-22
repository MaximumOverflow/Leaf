use std::error::Error;
use std::fmt::Debug;
use std::iter::Enumerate;
use std::ops::Range;
use std::slice::Iter;
use std::str::FromStr;
use ariadne::{Color, Label, Report, ReportKind};
use derive_more::Display;
use logos::{Lexer, Logos};
use winnow::error::Needed;
use winnow::stream::{Offset, Stream, StreamIsPartial};

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
				},
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
	pub text: &'l str,
	pub token: Token<'l>,
	pub range: Range<usize>,
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

	pub fn range(&self) -> &Range<usize> {
		&self.range
	}

	pub fn tokens(&self) -> &'l [TokenData<'l>] {
		self.tokens
	}

	pub fn file_name(&self) -> &'l str {
		self.file_name
	}

	pub fn start_char(&self) -> usize {
		if let Some(token) = self.tokens.get(self.range.start) {
			token.range.start
		} else {
			self.tokens.last().map(|t| t.range.end).unwrap_or_default()
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
