use leaf_reflection::{ElementRef, MetadataRead, Opcode};
use crate::frontend::types::Type;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, OnceLock};
use std::collections::HashMap;
use std::io::Cursor;

pub struct FunctionDef {
    pub name: Arc<str>,
    pub namespace: Arc<str>,

    pub return_ty: Type,
    pub parameters: HashMap<Arc<str>, Parameter>,

    pub instructions: OnceLock<Arc<[u8]>>,
    pub locals: OnceLock<HashMap<Arc<str>, Local>>,
    pub metadata: ElementRef<leaf_reflection::FunctionDef>,
}

pub struct Local {
    pub r#type: Type,
    pub name: Arc<str>,
    pub index: usize,
}

pub struct Parameter {
    pub r#type: Type,
    pub name: Arc<str>,
    pub index: usize,
}

impl FunctionDef {
    pub fn name(&self) -> &Arc<str> {
        &self.name
    }
}

impl Debug for FunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        let mut name = format! {
            "function {}::{}",
            self.namespace,
            match &*self.name {
                "" => "<anonymous>",
                name => name,
            }
        };

        {
            write!(name,  "(")?;
            let mut comma = "";
            for param in self.parameters.values() {
                write!(name, "{}{}: {}", comma, param.name, param.r#type)?;
                comma = ", ";
            }
            write!(name,  ") -> {}", self.return_ty)?;
        }

        let mut dbg = f.debug_struct(&name);
        let Some(opcodes) = self.instructions.get() else {
            return dbg.finish_non_exhaustive();
        };

        let mut name = String::new();
        let mut cursor = Cursor::new(opcodes);
        while cursor.position() != opcodes.len() as u64 {
            name.clear();
            let position = cursor.position();
            match Opcode::read(&mut cursor) {
                Ok(opcode) => {
                    write!(name, "IR_{:#06X}", position)?;
                    dbg.field(&name, &format_args!("{:?}", opcode));
                }
                Err(err) => {
                    write!(name, "IR_{:#06X}", position)?;
                    dbg.field(&name, &err);
                    return dbg.finish();
                }
            }
        }

        dbg.finish()
    }
}

