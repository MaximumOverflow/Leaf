use std::sync::mpsc::Sender;
use ariadne::ReportKind;
use std::sync::Arc;

pub type ProgressEventSink = Arc<Sender<CompilationProgress>>;
pub type ReportSink = Arc<Sender<ReportEventData>>;
pub type ReportEventData = (ReportKind<'static>, Vec<u8>);

#[derive(Default)]
pub struct CompilationCallbacks {
	pub report_sink: Option<ReportSink>,
	pub progress_event_sink: Option<ProgressEventSink>,
}

pub enum CompilationProgress {
	ReadingFiles(usize),
	LexingFiles(usize),
	ParsingFiles(usize),
	DeclaringSymbols(usize),
	CompilingSymbols(usize),
	Finished,
}
