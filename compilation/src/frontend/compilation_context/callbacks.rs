use std::sync::mpsc::Sender;
use std::sync::Arc;

#[derive(Default)]
pub struct CompilationCallbacks {
	pub progress_events_sink: Option<Arc<Sender<CompilationProgress>>>,
}

pub enum CompilationProgress {
	ParsingFiles(usize, usize),
	DeclaringSymbols(usize, usize),
	CompilingSymbols(usize, usize),
	Finished,
}
