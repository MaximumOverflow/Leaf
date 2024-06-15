use std::ops::ControlFlow;
use std::sync::mpsc::{Receiver, TryRecvError};
use indicatif::{ProgressBar, ProgressStyle};
use leaf_compilation::frontend::compilation_context::{CompilationProgress};

pub struct Progress {
	i: usize,
	current: isize,
	bar: ProgressBar,
	bar_style: ProgressStyle,
	receiver: Receiver<CompilationProgress>,
}

impl Progress {
	pub fn new(receiver: Receiver<CompilationProgress>) -> Self {
		let bar = ProgressBar::new(0);
		bar.finish_and_clear();
		Self {
			i: 0,
			current: -1,
			bar,
			bar_style: ProgressStyle::with_template(
				"{spinner:.green} {msg:30} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {pos}/{len} ",
			)
			.unwrap()
			.progress_chars("=>-"),
			receiver,
		}
	}

	pub fn update(&mut self) -> ControlFlow<(), bool> {
		let mut received = false;
		loop {
			match self.receiver.try_recv() {
				Ok(progress) => unsafe {
					received = true;
					let phase: isize = std::mem::transmute(std::mem::discriminant(&progress));
					let count: isize =
						std::mem::transmute(std::mem::discriminant(&CompilationProgress::Finished));
					if self.current != phase {
						self.current = phase;
						if !self.bar.is_finished() {
							let message = self.bar.message();
							if self.bar.length() != Some(self.bar.position()) {
								self.bar.abandon_with_message(format! {
									"✕ {message}"
								});
							} else {
								self.bar.finish_with_message(format! {
									"✓ {message}"
								});
							}
							self.i = 0;
						}
						self.bar = ProgressBar::new(0).with_style(self.bar_style.clone());
						match progress {
							CompilationProgress::ReadingFiles(_) => {
								self.bar.set_message(format!(
									"[{}/{}] Reading files...",
									self.current + 1,
									count
								));
							},
							CompilationProgress::LexingFiles(_) => {
								self.bar.set_message(format!(
									"[{}/{}] Lexing files...",
									self.current + 1,
									count
								));
							},
							CompilationProgress::ParsingFiles(_) => {
								self.bar.set_message(format!(
									"[{}/{}] Parsing files...",
									self.current + 1,
									count
								));
							},
							CompilationProgress::DeclaringSymbols(_) => {
								self.bar.set_message(format!(
									"[{}/{}] Declaring symbols...",
									self.current + 1,
									count
								));
							},
							CompilationProgress::CompilingSymbols(_) => {
								self.bar.set_message(format!(
									"[{}/{}] Compiling symbols...",
									self.current + 1,
									count
								));
							},
							CompilationProgress::Finished => {},
						}
					}
					self.i += 1;
					match progress {
						| CompilationProgress::ReadingFiles(len)
						| CompilationProgress::LexingFiles(len)
						| CompilationProgress::ParsingFiles(len)
						| CompilationProgress::DeclaringSymbols(len)
						| CompilationProgress::CompilingSymbols(len) => {
							self.bar.set_length(len as u64);
							self.bar.set_position(self.i as u64);
						},
						CompilationProgress::Finished => {},
					}
				},
				Err(err) => {
					return match err {
						TryRecvError::Empty => ControlFlow::Continue(received),
						TryRecvError::Disconnected => ControlFlow::Break(()),
					}
				},
			}
		}
	}

	#[inline]
	pub fn suspend<F: FnOnce() -> R, R>(&self, f: F) -> R {
		self.bar.suspend(f)
	}
}
