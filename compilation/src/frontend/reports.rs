#![allow(unused)]

use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::sync::Arc;
use ariadne::{Cache, Color, Label, Report, ReportBuilder, ReportKind, Source};
use fxhash::FxHashMap;
use leaf_parsing::ast::{Expression, Ident, Node, Type};
use leaf_reflection::ValueRef;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FrontEndError(u8, u8, &'static str);

impl Display for FrontEndError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "E{:02X}{:02X}", self.0, self.1)
	}
}

pub const NOT_IMPLEMENTED: FrontEndError = FrontEndError(!0, !0, "Not implemented");

pub const PARSING_ERROR: FrontEndError = FrontEndError(0b10000000, 0, "Parsing error");
pub const LEXER_ERROR: FrontEndError = FrontEndError(PARSING_ERROR.0, 0x1, "Lexer error");
pub const PARSER_ERROR: FrontEndError = FrontEndError(PARSING_ERROR.0, 0x2, "Parser error");
pub const COULD_NOT_RETRIEVE_SOURCE: FrontEndError =
	FrontEndError(PARSING_ERROR.0, 0x3, "Could not retrieve source code");

pub const INVALID_TYPE: FrontEndError = FrontEndError(0b00000001, 0, "Invalid type");
pub const INVALID_FIELD_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x1, "Invalid field type");
pub const INVALID_RETURN_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x2, "Invalid return type");
pub const INVALID_PARAMETER_TYPE: FrontEndError =
	FrontEndError(INVALID_TYPE.0, 0x3, "Invalid parameter type");
pub const NOT_A_STRUCT: FrontEndError = FrontEndError(INVALID_TYPE.0, 0x4, "Not a struct");
pub const NOT_AN_ARRAY: FrontEndError = FrontEndError(INVALID_TYPE.0, 0x5, "Not an array");
pub const NOT_A_FUNCTION: FrontEndError = FrontEndError(INVALID_TYPE.0, 0x6, "Not a function");

pub const NOT_FOUND: FrontEndError = FrontEndError(0b00000010, 0, "Identifier not found");
pub const MISSING_FIELD: FrontEndError = FrontEndError(NOT_FOUND.0, 0x04, "Missing field");
pub const FIELD_NOT_FOUND: FrontEndError = FrontEndError(NOT_FOUND.0, 0x02, "Field not found");
pub const VARIABLE_NOT_FOUND: FrontEndError =
	FrontEndError(NOT_FOUND.0, 0x03, "Variable not found");
pub const TYPE_NOT_FOUND: FrontEndError =
	FrontEndError(NOT_FOUND.0 | INVALID_TYPE.0, 0x01, "Type not found");

pub const VALUE_ERROR: FrontEndError = FrontEndError(0b00000100, 0, "Invalid value");
pub const INVALID_INTEGER: FrontEndError = FrontEndError(VALUE_ERROR.0, 0x1, "Invalid integer");
pub const VALUE_NOT_MUTABLE: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x2, "Value is not mutable");
pub const VALUE_NOT_ASSIGNABLE: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x3, "Value is not assignable");
//TODO find a valid category for this
pub const INVALID_PARAMETER_COUNT: FrontEndError =
	FrontEndError(VALUE_ERROR.0, 0x4, "Invalid parameter count");

pub const DECLARATION_ERROR: FrontEndError = FrontEndError(0b00001000, 0, "Declaration error");
pub const DUPLICATE_DECLARATION: FrontEndError =
	FrontEndError(DECLARATION_ERROR.0, 0x1, "Duplicate declaration");

pub type FrontEndReport = Report<'static, (Arc<str>, Range<usize>)>;
pub type FrontEndReportBuilder = ReportBuilder<'static, (Arc<str>, Range<usize>)>;

#[derive(Clone)]
pub struct ReportData<'a, 'l, 'ast> {
	file: Arc<str>,
	advices: Arc<RefCell<Vec<FrontEndReportBuilder>>>,
	warnings: Arc<RefCell<Vec<FrontEndReportBuilder>>>,
	pub variable_info: HashMap<
		ValueRef<'a, 'l>,
		(
			&'ast Ident<'ast>,
			Option<&'ast Type<'ast>>,
			&'ast Expression<'ast>,
		),
	>,
}

impl ReportData<'_, '_, '_> {
	pub fn new(file: &str) -> Self {
		Self {
			file: Arc::from(file),
			advices: Arc::new(RefCell::default()),
			warnings: Arc::new(RefCell::default()),
			variable_info: Default::default(),
		}
	}

	pub fn new_error(&self, offset: usize) -> FrontEndReportBuilder {
		let mut builder = Report::build(ReportKind::Error, self.file.clone(), offset);
		builder
	}

	pub fn new_advice(
		&self,
		msg: Option<impl ToString>,
		offset: usize,
		extend: impl FnOnce(&mut FrontEndReportBuilder),
	) {
		let mut advices = self.advices.borrow_mut();
		let idx = advices.len();
		let mut builder = Report::build(ReportKind::Advice, self.file.clone(), offset);
		if let Some(msg) = msg {
			builder = builder.with_message(msg)
		};
		extend(&mut builder);
		advices.push(builder);
	}

	pub fn new_warning(
		&self,
		msg: Option<impl ToString>,
		offset: usize,
		extend: impl FnOnce(&mut FrontEndReportBuilder),
	) {
		let mut warnings = self.warnings.borrow_mut();
		let idx = warnings.len();
		let mut builder = Report::build(ReportKind::Warning, self.file.clone(), offset);
		if let Some(msg) = msg {
			builder = builder.with_message(msg)
		};
		extend(&mut builder);
		warnings.push(builder);
	}

	pub fn file(&self) -> Arc<str> {
		self.file.clone()
	}
}

#[derive(Default)]
pub struct ReportCache<'l>(pub FxHashMap<Arc<str>, Source<&'l str>>);

impl<'l> Cache<Arc<str>> for ReportCache<'l> {
	type Storage = &'l str;

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

pub fn generate_and_dump_report(
	report: FrontEndReportBuilder,
	file: Arc<str>,
	code: &str,
	err: FrontEndError,
) -> FrontEndReport {
	let report = report.with_message(err.2).with_code(err).finish();
	dump_report(&report, file, code);
	report
}

pub fn dump_report<'l>(report: &FrontEndReport, file: Arc<str>, code: &str) {
	report.print((file.clone(), Source::from(code))).unwrap()
}

pub fn unsupported_report(
	file: Arc<str>,
	node: &impl Node,
	message: Option<impl ToString>,
) -> FrontEndReportBuilder {
	let range = node.range();
	Report::build(ReportKind::Error, file.clone(), range.start).with_label(match message {
		None => Label::new((file, range))
			.with_message(format!(
				"Could not compile {}",
				std::any::type_name_of_val(node)
			))
			.with_color(Color::Red),
		Some(message) => Label::new((file, range)).with_message(message).with_color(Color::Red),
	})
}

pub fn invalid_parameter_count(
	expected: usize,
	got: &[impl Node],
	range: Range<usize>,
	report_data: &ReportData,
) -> (FrontEndError, FrontEndReportBuilder) {
	let mut report = report_data.new_error(range.start).with_label(
		Label::new((report_data.file(), range.clone()))
			.with_color(Color::Red)
			.with_message(format!(
				"Expected {} {}, got {}",
				expected,
				match expected {
					1 => "parameter",
					_ => "parameters",
				},
				got.len()
			)),
	);

	if got.len() > expected {
		let mut additional = got[expected].range();
		additional.end = additional.start;
		report.add_label(
			Label::new((report_data.file(), additional.clone()))
				.with_message("Additional parameters start here"),
		);
	} else if !got.is_empty() {
		let mut additional = got[0].range();
		additional.start = additional.end;
		additional.end += 1;
		report.add_label(
			Label::new((report_data.file(), additional.clone()))
				.with_message("Additional parameters should start here"),
		);
	}

	(INVALID_PARAMETER_COUNT, report)
}
