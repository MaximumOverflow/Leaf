mod refs;
mod types;
mod functions;
mod read_write;
mod opcodes;
pub mod builders;

pub use refs::*;
pub use types::*;
pub use opcodes::*;
pub use functions::*;
use leaf_derive::MetadataWrite;
pub use read_write::*;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, MetadataWrite)]
pub struct MetadataOffsets {
	pub type_table: u64,
	pub field_table: u64,
	pub function_table: u64,
	pub parameter_table: u64,
	pub blob_heap: u64,
	pub string_heap: u64,
}
