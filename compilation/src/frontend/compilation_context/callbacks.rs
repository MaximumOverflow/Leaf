#[derive(Default)]
pub struct CompilationCallbacks {
	pub progress_callback: Option<Box<dyn FnMut(CompilationProgress)>>,
}

pub enum CompilationProgress {
	ParsingFiles(usize, usize),
	DeclaringSymbols(usize, usize),
	CompilingSymbols(usize, usize),
	Finished,
}
