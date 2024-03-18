pub mod types;
pub mod assembly;
pub mod functions;

use std::sync::{Arc, OnceLock};
pub use types::Type;
pub use assembly::Assembly;
pub use functions::Function;

fn name_or_empty(name: &str) -> Arc<str> {
	static EMPTY_STR: OnceLock<Arc<str>> = OnceLock::new();
	match name {
		"" => EMPTY_STR.get_or_init(|| Arc::from("")).clone(),
		_ => Arc::from(name),
	}
}
