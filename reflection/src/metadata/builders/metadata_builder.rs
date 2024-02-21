use crate::{ElementRef, FieldDef, FunctionDef, MetadataOffsets, ParameterDef, SliceRef, TypeDef, TypeKind, MetadataWrite, FunctionBody, Opcode, TypeSignature, Encoded};
use crate::builders::{BlobHeapBuilder, StringHeapBuilder, TypeSignatureBytes};
use std::marker::PhantomData;
use std::io::{Cursor, Write};
use std::cmp::Ordering;
use std::sync::Arc;

pub struct MetadataBuilder {
	blobs: BlobHeapBuilder,
	strings: StringHeapBuilder,

	type_defs: Vec<TypeDef>,
	field_defs: Vec<FieldDef>,
	function_defs: Vec<FunctionDef>,
	parameter_defs: Vec<ParameterDef>,
}

impl Default for MetadataBuilder {
	fn default() -> Self {
		let mut strings = StringHeapBuilder::default();
		strings.alloc("");

		Self {
			strings,
			blobs: Default::default(),
			type_defs: vec![TypeDef::default()],
			field_defs: vec![FieldDef::default()],
			function_defs: vec![FunctionDef::default()],
			parameter_defs: vec![ParameterDef::default()],
		}
	}
}

impl MetadataBuilder {
	pub fn intern_str(&mut self, str: &str) -> SliceRef<str> {
		self.strings.alloc(str)
	}

	pub fn declare_type(
		&mut self, kind: TypeKind, name: &str, namespace: &str,
	) -> ElementRef<TypeDef> {
		let element_ref = ElementRef {
			ph: PhantomData,
			offset: Encoded(self.type_defs.len() as u64),
		};
		self.type_defs.push(TypeDef {
			kind,
			namespace: self.strings.alloc(namespace).into(),
			name: self.strings.alloc(name).into(),
			fields: SliceRef::null(),
		});
		element_ref
	}

	pub fn declare_function(&mut self, name: &str, namespace: &str) -> ElementRef<FunctionDef> {
		let element_ref = ElementRef {
			ph: PhantomData,
			offset: Encoded(self.function_defs.len() as u64),
		};
		self.function_defs.push(FunctionDef {
			namespace: self.strings.alloc(namespace),
			name: self.strings.alloc(name),
			return_ty: SliceRef::null(),
			params: SliceRef::null(),
			body: FunctionBody::default(),
		});
		element_ref
	}

	pub fn create_field(&mut self, name: &str, signature: impl AsRef<TypeSignature>) -> FieldDef {
		FieldDef {
			name: self.strings.alloc(name).into(),
			signature: self.blobs.alloc(signature.as_ref()).0,
		}
	}

	pub fn create_parameter(&mut self, name: &str, signature: impl AsRef<TypeSignature>) -> ParameterDef {
		ParameterDef {
			name: self.strings.alloc(name),
			signature: self.blobs.alloc(signature.as_ref()).0,
		}
	}

	pub fn set_fields<T: IntoIterator<Item = FieldDef>>(
		&mut self, ty: ElementRef<TypeDef>, fields: T,
	) where
		T::IntoIter: ExactSizeIterator,
	{
		let fields = fields.into_iter();
		let types = &mut self.type_defs[ty.offset.0 as usize..];
		let ty = &mut types[0];

		let len = ty.fields.len.0 as usize;
		let offset = ty.fields.offset.0 as usize;
		let difference = fields.len() as isize - len as isize;

		let diff = difference.unsigned_abs();
		ty.fields.len = Encoded(fields.len() as u64);

		self.field_defs.splice(offset..offset + len, fields);

		match difference.cmp(&0) {
			Ordering::Equal => {},
			Ordering::Less => types.iter_mut().skip(1).for_each(|ty| ty.fields.offset.0 -= diff as u64),
			Ordering::Greater => types.iter_mut().skip(1).for_each(|ty| ty.fields.offset.0 += diff as u64),
		}
	}

	pub fn set_parameters<T: IntoIterator<Item = ParameterDef>>(
		&mut self, func: ElementRef<FunctionDef>, params: T,
	) where
		T::IntoIter: ExactSizeIterator,
	{
		let params = params.into_iter();
		let funcs = &mut self.function_defs[func.offset.0 as usize..];
		let func = &mut funcs[0];

		let len = func.params.len.0 as usize;
		let offset = func.params.offset.0 as usize;
		let difference = params.len() as isize - len as isize;

		let diff = difference.unsigned_abs();
		func.params.len = Encoded(params.len() as u64);

		self.parameter_defs.splice(offset..offset + len, params);

		match difference.cmp(&0) {
			Ordering::Equal => {},
			Ordering::Less => funcs.iter_mut().skip(1).for_each(|ty| ty.params.offset.0 -= diff as u64),
			Ordering::Greater => funcs.iter_mut().skip(1).for_each(|ty| ty.params.offset.0 += diff as u64),
		}
	}

	pub fn set_return_type(&mut self, func: ElementRef<FunctionDef>, signature: &TypeSignature) {
		let func = &mut self.function_defs[func.offset.0 as usize];
		func.return_ty = self.blobs.alloc(&signature).0;
	}

	pub fn set_locals(&mut self, func: ElementRef<FunctionDef>, locals: &[TypeSignatureBytes]) {
		let mut local_refs = vec![];
		for (local, _) in locals.iter().map(|sig| self.blobs.alloc(&sig.0)) {
			local.write(&mut local_refs).unwrap();
		}
		let (local_refs, _) = self.blobs.alloc(&local_refs);

		let func = &mut self.function_defs[func.offset.0 as usize];
		func.body.locals = SliceRef {
			offset: local_refs.offset,
			len: Encoded(locals.len() as u64),
			ph: PhantomData,
		};
	}

	pub fn set_instructions(&mut self, func: ElementRef<FunctionDef>, opcodes: &[Opcode]) -> Arc<[u8]> {
		let mut data = vec![];
		opcodes.write(&mut data).unwrap();
		let func = &mut self.function_defs[func.offset.0 as usize];
		let (opcodes, buffer) = self.blobs.alloc(&data);
		func.body.opcodes = opcodes;
		buffer
	}

	pub fn build(self) -> Result<Vec<u8>, std::io::Error> {
		const PREFIX: &str = "leaf";

		let mut buffer = vec![];
		buffer.write_all(PREFIX.as_bytes())?;

		let mut offsets = MetadataOffsets::default();
		offsets.write(&mut buffer)?;

		if !self.type_defs.is_empty() {
			offsets.type_table = Encoded(buffer.len() as u64);
			for def in &self.type_defs {
				def.write(&mut buffer)?;
			}
		}

		if !self.field_defs.is_empty() {
			offsets.field_table = Encoded(buffer.len() as u64);
			for def in &self.type_defs {
				def.write(&mut buffer)?;
			}
		}

		if !self.function_defs.is_empty() {
			offsets.function_table = Encoded(buffer.len() as u64);
			for def in &self.type_defs {
				def.write(&mut buffer)?;
			}
		}

		if !self.parameter_defs.is_empty() {
			offsets.parameter_table = Encoded(buffer.len() as u64);
			for def in &self.type_defs {
				def.write(&mut buffer)?;
			}
		}

		if !self.strings.is_empty() {
			offsets.string_heap = Encoded(buffer.len() as u64);
			self.strings.build_into(&mut buffer);
		}

		if !self.blobs.is_empty() {
			offsets.blob_heap = Encoded(buffer.len() as u64);
			self.blobs.build_into(&mut buffer);
		}

		let mut stream = Cursor::new(&mut buffer[PREFIX.len()..]);
		offsets.write(&mut stream)?;
		Ok(buffer)
	}

	pub fn get_str(&self, str: SliceRef<str>) -> Option<&str> {
		self.strings.get_str(str)
	}

	pub fn get_blob(&self, blob: SliceRef<[u8]>) -> Option<&[u8]> {
		self.blobs.get_blob(blob)
	}

	pub fn get_str_ref(&self, str: &str) -> Option<SliceRef<str>> {
		self.strings.get_str_ref(str)
	}

	pub fn get_type(&self, ty: ElementRef<TypeDef>) -> Option<&TypeDef> {
		self.type_defs.get(ty.offset.0 as usize)
	}

	pub fn get_fn(&self, ty: ElementRef<FunctionDef>) -> Option<&FunctionDef> {
		self.function_defs.get(ty.offset.0 as usize)
	}

	pub fn get_fn_by_name(&self, namespace: &str, name: &str) -> Option<&FunctionDef> {
		self.function_defs.iter().find(|func| {
			let f_name = self.strings.get_str(func.name).unwrap_or_default();
			let f_namespace = self.strings.get_str(func.namespace).unwrap_or_default();
			f_name == name && f_namespace == namespace
		})
	}

	pub fn get_locals(&self, func: &FunctionDef) -> Option<impl Iterator<Item=&TypeSignature>> {
		let locals = self.blobs.get_blob_items(func.body.locals)?;
		Some(locals.filter_map(|local| self.get_blob(local)))
	}
}
