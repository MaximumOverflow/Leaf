#![allow(unused)]

use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut, Range};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use ariadne::{Cache, Color, Label, Report, ReportBuilder, Source, Span};
use fxhash::FxHashMap;
use leaf_parsing::ast::{Expression, Ident, Node, Type};
use crate::frontend::compilation_context::ReportSink;
use leaf_reflection::ValueRef;

// ErrorKind (flags), ErrorCode, Message
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FrontEndError(u8, u8, Cow<'static, str>);

impl Display for FrontEndError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "E{:02X}{:02X}", self.0, self.1)
	}
}

impl From<LexerError> for FrontEndError {
	fn from(value: LexerError) -> Self {
		Self {
			0: LEXER_ERROR.0,
			1: value.code[1..].parse().unwrap(),
			2: value.message.map(|m| Cow::from(m)).unwrap_or(LEXER_ERROR.2),
		}
	}
}

impl From<&LexerError> for FrontEndError {
	fn from(value: &LexerError) -> Self {
		Self {
			0: LEXER_ERROR.0,
			1: value.code[1..].parse().unwrap(),
			2: value.message.clone().map(|m| Cow::from(m)).unwrap_or(LEXER_ERROR.2),
		}
	}
}

impl From<ParserError<'_>> for FrontEndError {
	fn from(value: ParserError) -> Self {
		Self {
			0: PARSING_ERROR.0,
			1: value.code[1..].parse().unwrap(),
			2: value.message.map(|m| Cow::from(m)).unwrap_or(PARSING_ERROR.2),
		}
	}
}

impl From<&ParserError<'_>> for FrontEndError {
	fn from(value: &ParserError) -> Self {
		Self {
			0: PARSING_ERROR.0,
			1: value.code[1..].parse().unwrap(),
			2: value.message.clone().map(|m| Cow::from(m)).unwrap_or(PARSING_ERROR.2),
		}
	}
}

pub const NOT_IMPLEMENTED: FrontEndError = FrontEndError(!0, !0, Cow::Borrowed("Not implemented"));

pub const PARSING_ERROR: FrontEndError =
	FrontEndError(0b10000000, 0, Cow::Borrowed("Parsing error"));
pub const LEXER_ERROR: FrontEndError =
	FrontEndError(PARSING_ERROR.0, 0x1, Cow::Borrowed("Lexer error"));
pub const PARSER_ERROR: FrontEndError =
	FrontEndError(PARSING_ERROR.0, 0x2, Cow::Borrowed("Parser error"));
pub const COULD_NOT_RETRIEVE_SOURCE: FrontEndError = FrontEndError(
	PARSING_ERROR.0,
	0x3,
	Cow::Borrowed("Could not retrieve source code"),
);

pub const INVALID_TYPE: FrontEndError = FrontEndError(0b00000001, 0, Cow::Borrowed("Invalid type"));
pub const INVALID_FIELD_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x1, Cow::Borrowed("Invalid field type"));
pub const INVALID_RETURN_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x2, Cow::Borrowed("Invalid return type"));
pub const INVALID_PARAMETER_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x3, Cow::Borrowed("Invalid parameter type"));
pub const NOT_A_STRUCT: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x4, Cow::Borrowed("Not a struct"));
pub const NOT_AN_ARRAY: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x5, Cow::Borrowed("Not an array"));
pub const NOT_A_FUNCTION: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x6, Cow::Borrowed("Not a function"));

pub const NOT_FOUND: FrontEndError =
	FrontEndError(0b00000010, 0, Cow::Borrowed("Identifier not found"));
pub const MISSING_FIELD: FrontEndError =
	FrontEndError(NOT_FOUND.0, 0x04, Cow::Borrowed("Missing field"));
pub const FIELD_NOT_FOUND: FrontEndError =
	FrontEndError(NOT_FOUND.0, 0x02, Cow::Borrowed("Field not found"));
pub const VARIABLE_NOT_FOUND: FrontEndError =
	FrontEndError(NOT_FOUND.0, 0x03, Cow::Borrowed("Variable not found"));
pub const TYPE_NOT_FOUND: FrontEndError = FrontEndError(
	NOT_FOUND.0 | INVALID_TYPE.0,
	0x01,
	Cow::Borrowed("Type not found"),
);

pub const VALUE_ERROR: FrontEndError = FrontEndError(0b00000100, 0, Cow::Borrowed("Invalid value"));
pub const INVALID_INTEGER: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x1, Cow::Borrowed("Invalid integer"));
pub const VALUE_NOT_MUTABLE: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x2, Cow::Borrowed("Value is not mutable"));
pub const VALUE_NOT_ASSIGNABLE: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x3, Cow::Borrowed("Value is not assignable"));
//TODO find a valid category for this
pub const INVALID_PARAMETER_COUNT: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x4, Cow::Borrowed("Invalid parameter count"));

pub const DECLARATION_ERROR: FrontEndError =
	FrontEndError(0b00001000, 0, Cow::Borrowed("Declaration error"));
pub const DUPLICATE_DECLARATION: FrontEndError = FrontEndError(
	DECLARATION_ERROR.0,
	0x1,
	Cow::Borrowed("Duplicate declaration"),
);

pub type FrontEndReport = Report<'static, (Arc<str>, Range<usize>)>;

pub use ariadne::ReportKind;
use leaf_parsing::lexer::LexerError;
use leaf_parsing::parser::ParserError;

pub struct ReportData {
	file: Arc<str>,
	cache: ReportCache,
	sink: Option<ReportSink>,
	errors: AtomicUsize,
	advices: AtomicUsize,
	warnings: AtomicUsize,
}

impl ReportData {
	pub fn new(file: Arc<str>, cache: ReportCache, sink: Option<ReportSink>) -> Self {
		Self {
			file,
			cache,
			sink,
			errors: Default::default(),
			advices: Default::default(),
			warnings: Default::default(),
		}
	}

	pub fn new_advice(&self, offset: usize) -> FrontEndReportBuilder {
		self.advices.fetch_add(1, Ordering::Relaxed);
		FrontEndReportBuilder {
			file: self.file.clone(),
			sink: self.sink.clone(),
			cache: self.cache.clone(),
			kind: ReportKind::Advice,
			report: Report::build(ReportKind::Advice, self.file.clone(), offset),
		}
	}

	pub fn new_warning(&self, offset: usize) -> FrontEndReportBuilder {
		self.warnings.fetch_add(1, Ordering::Relaxed);
		FrontEndReportBuilder {
			file: self.file.clone(),
			sink: self.sink.clone(),
			cache: self.cache.clone(),
			kind: ReportKind::Warning,
			report: Report::build(ReportKind::Warning, self.file.clone(), offset),
		}
	}

	pub fn new_error(&self, offset: usize) -> FrontEndReportBuilder {
		self.errors.fetch_add(1, Ordering::Relaxed);
		FrontEndReportBuilder {
			file: self.file.clone(),
			sink: self.sink.clone(),
			cache: self.cache.clone(),
			kind: ReportKind::Error,
			report: Report::build(ReportKind::Error, self.file.clone(), offset),
		}
	}

	#[inline]
	pub fn errors(&self) -> usize {
		self.errors.load(Ordering::Relaxed)
	}

	#[inline]
	pub fn advices(&self) -> usize {
		self.advices.load(Ordering::Relaxed)
	}

	#[inline]
	pub fn warnings(&self) -> usize {
		self.warnings.load(Ordering::Relaxed)
	}
}

pub struct FrontEndReportBuilder {
	file: Arc<str>,
	cache: ReportCache,
	sink: Option<ReportSink>,
	kind: ReportKind<'static>,
	report: ReportBuilder<'static, (Arc<str>, Range<usize>)>,
}

impl FrontEndReportBuilder {
	pub fn add_label(&mut self, range: Range<usize>, color: Option<Color>, message: impl ToString) {
		let mut label = Label::new((self.file.clone(), range)).with_message(message);
		if let Some(color) = color {
			label = label.with_color(color);
		}
		self.report.add_label(label)
	}

	pub fn send(self) {
		if let Some(sink) = self.sink {
			let mut buffer = vec![];
			let report = self.report.finish();
			report.write_for_stdout(self.cache.clone(), &mut buffer).unwrap();
			sink.send((self.kind, buffer)).unwrap();
		}
	}
}

#[derive(Default, Clone)]
pub struct ReportCache(pub(crate) Arc<FxHashMap<Arc<str>, Source<Arc<str>>>>);

impl From<FxHashMap<Arc<str>, Source<Arc<str>>>> for ReportCache {
	fn from(value: FxHashMap<Arc<str>, Source<Arc<str>>>) -> Self {
		Self(Arc::new(value))
	}
}

impl Cache<Arc<str>> for ReportCache {
	type Storage = Arc<str>;

	fn fetch(&mut self, id: &Arc<str>) -> Result<&Source<Self::Storage>, Box<dyn Debug + '_>> {
		match self.0.get(id) {
			Some(source) => Ok(source),
			None => unreachable!(),
		}
	}

	fn display<'a>(&self, id: &'a Arc<str>) -> Option<Box<dyn Display + 'a>> {
		Some(Box::new(id.clone()))
	}
}

// pub fn unsupported_report(
// 	file: Arc<str>,
// 	node: &impl Node,
// 	message: Option<impl ToString>,
// ) -> FrontEndReportBuilder {
// 	let range = node.range();
// 	Report::build(ReportKind::Error, file.clone(), range.start).with_label(match message {
// 		None => Label::new((file, range))
// 			.with_message(format!(
// 				"Could not compile {}",
// 				std::any::type_name_of_val(node)
// 			))
// 			.with_color(Color::Red),
// 		Some(message) => Label::new((file, range)).with_message(message).with_color(Color::Red),
// 	})
// }

pub fn invalid_parameter_count(
	expected: usize,
	got: &[impl Node],
	range: Range<usize>,
	report_data: &ReportData,
) -> FrontEndReportBuilder {
	let mut report = report_data.new_error(range.start());
	report.add_label(
		range.clone(),
		Some(Color::Red),
		format!(
			"Expected {} {}, got {}",
			expected,
			match expected {
				1 => "parameter",
				_ => "parameters",
			},
			got.len()
		),
	);

	if got.len() > expected {
		let mut additional = got[expected].range();
		additional.end = additional.start;
		report.add_label(additional.clone(), None, "Additional parameters start here");
	} else if !got.is_empty() {
		let mut additional = got[0].range();
		additional.start = additional.end;
		additional.end += 1;
		report.add_label(
			additional.clone(),
			None,
			"Additional parameters should start here",
		);
	}

	report
}
