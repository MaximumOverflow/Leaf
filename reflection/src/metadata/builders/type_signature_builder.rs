use crate::{BUG_ERR, ElementRef, Encoded, MetadataRead, MetadataWrite, TypeDefOrRef, TypeSignatureTag};
use std::io::{Cursor, Read, Seek};

#[derive(Default)]
pub struct TypeSignatureBuilder {
	bytes: Vec<u8>,
}

pub struct TypeSignatureBytes(pub(crate) Vec<u8>);

impl Into<Vec<u8>> for TypeSignatureBytes {
	fn into(self) -> Vec<u8> {
		self.0
	}
}

impl TypeSignatureBuilder {
	pub fn push_void(&mut self) {
		self.bytes.push(TypeSignatureTag::Void as u8)
	}

	pub fn push_char(&mut self) {
		self.bytes.push(TypeSignatureTag::Char as u8)
	}

	pub fn push_bool(&mut self) {
		self.bytes.push(TypeSignatureTag::Bool as u8)
	}

	//noinspection DuplicatedCode
	pub fn push_integer(&mut self, size: u64, signed: bool) {
		match signed {
			true => match size {
				8 => self.bytes.push(TypeSignatureTag::Integer8 as u8),
				16 => self.bytes.push(TypeSignatureTag::Integer16 as u8),
				32 => self.bytes.push(TypeSignatureTag::Integer32 as u8),
				64 => self.bytes.push(TypeSignatureTag::Integer64 as u8),
				_ => panic!("Integer too big"),
			},
			false => match size {
				8 => self.bytes.push(TypeSignatureTag::UInteger8 as u8),
				16 => self.bytes.push(TypeSignatureTag::UInteger16 as u8),
				32 => self.bytes.push(TypeSignatureTag::UInteger32 as u8),
				64 => self.bytes.push(TypeSignatureTag::UInteger64 as u8),
				_ => panic!("Integer too big"),
			},
		}
	}

	pub fn push_decimal(&mut self, size: u64) {
		match size {
			16 => self.bytes.push(TypeSignatureTag::Decimal16 as u8),
			32 => self.bytes.push(TypeSignatureTag::Decimal32 as u8),
			64 => self.bytes.push(TypeSignatureTag::Decimal64 as u8),
			_ => panic!("Decimal too big"),
		}
	}

	pub fn push_array(&mut self, size: u64) {
		let mut cursor = Cursor::new(&mut self.bytes);
		TypeSignatureTag::Array.write(&mut cursor).unwrap();
		Encoded(size).write(&mut cursor).unwrap();
	}

	pub fn push_ptr(&mut self, mutable: bool) {
		self.bytes.push(match mutable {
			true => TypeSignatureTag::MutPointer as u8,
			false => TypeSignatureTag::Pointer as u8,
		});
	}

	pub fn push_type(&mut self, ty: TypeDefOrRef) {
		let mut cursor = Cursor::new(&mut self.bytes);
		ty.write(&mut cursor).unwrap();
	}

	pub fn build(self) -> Result<TypeSignatureBytes, &'static str> {
		match self.depth() == 0 {
			false => Err("Type signature is incomplete."),
			true => Ok(TypeSignatureBytes(self.bytes)),
		}
	}

	pub fn build_into(self, vec: &mut Vec<u8>) -> Result<&[u8], &'static str> {
		match self.depth() == 0 {
			false => Err("Type signature is incomplete."),
			true => {
				let start = vec.len();
				vec.extend(self.bytes);
				Ok(&vec[start..])
			},
		}
	}

	pub fn depth(&self) -> isize {
		let mut depth = 1;
		let mut cursor = Cursor::new(&self.bytes);

		while cursor.stream_position().expect(BUG_ERR) < self.bytes.len() as u64 {
			match TypeSignatureTag::read(&mut cursor).expect(BUG_ERR) {
				TypeSignatureTag::Pointer | TypeSignatureTag::MutPointer => {},

				TypeSignatureTag::TypeDef | TypeSignatureTag::TypeRef => {
					depth -= 1;
					let mut byte = 0u8;
					cursor.read_exact(bytemuck::bytes_of_mut(&mut byte)).expect(BUG_ERR);
					let _ = ElementRef::<()>::read(&mut cursor).unwrap();
				},

				TypeSignatureTag::Array => {
					let mut byte = 0u8;
					cursor.read_exact(bytemuck::bytes_of_mut(&mut byte)).expect(BUG_ERR);
					let _ = Encoded::<u64>::read(&mut cursor).unwrap();
				},

				| TypeSignatureTag::Void
				| TypeSignatureTag::Char
				| TypeSignatureTag::Bool
				| TypeSignatureTag::Integer8
				| TypeSignatureTag::Integer16
				| TypeSignatureTag::Integer32
				| TypeSignatureTag::Integer64
				| TypeSignatureTag::UInteger8
				| TypeSignatureTag::UInteger16
				| TypeSignatureTag::UInteger32
				| TypeSignatureTag::UInteger64
				| TypeSignatureTag::Decimal16
				| TypeSignatureTag::Decimal32
				| TypeSignatureTag::Decimal64 => depth -= 1,
			}
		}

		depth
	}
}
