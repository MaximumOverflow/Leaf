use leaf_reflection::builders::TypeSignatureBytes;
use crate::frontend::type_resolver::TypeResolver;
use crate::frontend::CompilationUnit;
use leaf_parsing::ast::Statement;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use std::ops::RangeFrom;
use anyhow::Error;

use leaf_parsing::ast::Block as BlockNode;

pub struct Block;

impl Block {
	pub fn generate_opcodes<'l, 'u>(
		node: &'l BlockNode<'l>, unit: &'l mut CompilationUnit<'u>,
	) -> anyhow::Result<Vec<Opcode>, Error> {
		let mut opcodes = vec![];
		let locals = HashMap::new();
		let mut local_indices = 0..;
		generate_opcodes_recursive(node, unit, &mut local_indices, locals, &mut opcodes)?;
		Ok(opcodes)
	}

	pub fn generate_local_signatures<'l, 'u>(
		node: &'l BlockNode<'l>, unit: &'l mut CompilationUnit<'u>,
	) -> anyhow::Result<Vec<TypeSignatureBytes>, Error> {
		let mut locals = vec![];
		generate_local_signatures_recursive(node, unit, &mut locals)?;
		Ok(locals)
	}
}

fn generate_opcodes_recursive<'l, 'u>(
	node: &'l BlockNode<'l>, unit: &'l mut CompilationUnit<'u>,
	local_indices: &mut RangeFrom<usize>, mut locals: HashMap<&'l str, (usize, Vec<u8>)>,
	opcodes: &mut Vec<Opcode>,
) -> anyhow::Result<(), Error> {
	for statement in &node.statements {
		match statement {
			Statement::VarDecl(decl) => {
				let ty = match &decl.ty {
					None => unimplemented!(),
					Some(ty) => unit.create_type_signature(ty)?,
				};

				let local_id = local_indices.next().unwrap();
				locals.insert(decl.id, (local_id, ty.into()));
				Block::generate_expression(&decl.expr, opcodes, &locals)?;
				opcodes.push(Opcode::StoreLocal(local_id));
			},

			Statement::Block(child) => {
				let locals = locals.clone();
				generate_opcodes_recursive(child, unit, local_indices, locals, opcodes)?;
			},

			Statement::Return(None) => {
				opcodes.push(Opcode::Ret);
			},
			Statement::Return(Some(expr)) => {
				Block::generate_expression(expr, opcodes, &locals)?;
				opcodes.push(Opcode::Ret);
			},

			_ => unimplemented!("Unimplemented statement: {:#?}", statement),
		}
	}

	Ok(())
}

fn generate_local_signatures_recursive<'l, 'u>(
	block: &'l BlockNode<'l>, unit: &'l mut CompilationUnit<'u>,
	locals: &mut Vec<TypeSignatureBytes>,
) -> anyhow::Result<(), Error> {
	for statement in &block.statements {
		match statement {
			Statement::VarDecl(decl) => locals.push(match &decl.ty {
				None => unimplemented!(),
				Some(ty) => unit.create_type_signature(ty)?,
			}),

			Statement::Block(child) => {
				generate_local_signatures_recursive(&child, unit, locals)?;
			},
			_ => {},
		}
	}
	Ok(())
}
