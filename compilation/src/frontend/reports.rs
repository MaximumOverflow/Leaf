#![allow(unused)]

use std::fmt::{Display, Formatter};
use std::ops::Range;
use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, Source};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FrontEndError(u8, u8);

impl Display for FrontEndError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "E{:02X}{:02X}", self.0, self.1)
	}
}

pub const PARSING_ERROR: FrontEndError = FrontEndError(0b10000000, 0);
pub const LEXER_ERROR: FrontEndError = FrontEndError(PARSING_ERROR.0, 0x1);
pub const PARSER_ERROR: FrontEndError = FrontEndError(PARSING_ERROR.0, 0x2);
pub const COULD_NOT_RETRIEVE_SOURCE: FrontEndError = FrontEndError(PARSING_ERROR.0, 0x3);

pub const TYPE_ERROR: FrontEndError = FrontEndError(0b00000001, 0);
pub const NOT_A_STRUCT: FrontEndError = FrontEndError(TYPE_ERROR.0, 0x3);
pub const INVALID_FIELD_TYPE: FrontEndError = FrontEndError(TYPE_ERROR.0, 0x1);
pub const INVALID_RETURN_TYPE: FrontEndError = FrontEndError(TYPE_ERROR.0, 0x2);

pub const NOT_FOUND: FrontEndError = FrontEndError(0b00000010, 0);
pub const MISSING_FIELD: FrontEndError = FrontEndError(NOT_FOUND.0, 0x04);
pub const FIELD_NOT_FOUND: FrontEndError = FrontEndError(NOT_FOUND.0, 0x02);
pub const VARIABLE_NOT_FOUND: FrontEndError = FrontEndError(NOT_FOUND.0, 0x03);
pub const TYPE_NOT_FOUND: FrontEndError = FrontEndError(NOT_FOUND.0 | TYPE_ERROR.0, 0x01);

pub const VALUE_ERROR: FrontEndError = FrontEndError(0b00000100, 0);
pub const INVALID_INTEGER: FrontEndError = FrontEndError(VALUE_ERROR.0, 0x1);

pub const DECLARATION_ERROR: FrontEndError = FrontEndError(0b00001000, 0);
pub const DUPLICATE_DECLARATION: FrontEndError = FrontEndError(DECLARATION_ERROR.0, 0x1);

pub struct ReportData<'l> {
	file: &'l str,
	advices: Vec<ReportBuilder<'static, (&'l str, Range<usize>)>>,
	warnings: Vec<ReportBuilder<'static, (&'l str, Range<usize>)>>,
	pub errors: ReportBuilder<'static, (&'l str, Range<usize>)>,
}

impl<'l> ReportData<'l> {
	pub fn new(file: &'l str) -> Self {
		Self {
			file,
			advices: vec![],
			warnings: vec![],
			errors: Report::build(ReportKind::Error, file, 0),
		}
	}

	pub fn add_error_label(
		&mut self,
		range: Range<usize>,
		message: impl ToString,
	) {
		self.errors.add_label(
			Label::new((self.file, range))
				.with_color(Color::Red)
				.with_message(message)
		)
	}

	#[rustfmt::skip]
	pub fn new_advice(&mut self, msg: Option<impl ToString>, offset: usize) -> &mut ReportBuilder<'static, (&'l str, Range<usize>)> {
		let idx = self.advices.len();
		let mut builder = Report::build(ReportKind::Advice, self.file, offset);
		if let Some(msg) = msg { builder = builder.with_message(msg) };
		self.advices.push(builder);
		&mut self.advices[idx]
	}

	#[rustfmt::skip]
	pub fn new_warning(&mut self, msg: Option<impl ToString>, offset: usize) -> &mut ReportBuilder<'static, (&'l str, Range<usize>)> {
		let idx = self.warnings.len();
		let mut builder = Report::build(ReportKind::Warning, self.file, offset);
		if let Some(msg) = msg { builder = builder.with_message(msg) };
		self.warnings.push(builder);
		&mut self.warnings[idx]
	}
}

pub fn generate_and_dump_report<'l>(report: ReportBuilder<'static, (&'l str, Range<usize>)>, file: &'l str, code: &str, err: FrontEndError) {
	use std::fmt::Write;
	let mut msg = String::new();
	let mut separator = "";

	if err.0 & PARSING_ERROR.0 != 0 {
		write!(msg, "{separator}Parsing error");
		separator = " | ";
	}

	if err.0 & TYPE_ERROR.0 != 0 {
		write!(msg, "{separator}Type error");
		separator = " | ";
	}

	if err.0 & NOT_FOUND.0 != 0 {
		write!(msg, "{separator}Missing symbol error");
		separator = " | ";
	}

	if err.0 & VALUE_ERROR.0 != 0 {
		write!(msg, "{separator}Value error");
		separator = " | ";
	}

	if err.0 & DECLARATION_ERROR.0 != 0 {
		write!(msg, "{separator}Declaration error");
		separator = " | ";
	}

	dump_report(report.with_message(msg).with_code(err).finish(), file, code)
}

pub fn dump_report<'l>(report: Report<(&'l str, Range<usize>)>, file: &'l str, code: &str) {
	report.print((file, Source::from(code))).unwrap()
}
