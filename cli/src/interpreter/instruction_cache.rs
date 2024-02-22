use leaf_compilation::reflection::structured::Function;
use leaf_compilation::reflection::{MetadataRead, Opcode};
use std::collections::HashMap;
use std::io::Cursor;
use std::sync::Arc;
use anyhow::anyhow;
use std::rc::Rc;

#[derive(Default)]
pub struct InstructionCache {
    functions: HashMap<usize, Rc<[Opcode]>>,
}

impl InstructionCache {
    #[inline(never)]
    pub fn get_instructions(&mut self, function: &Arc<Function>) -> anyhow::Result<Rc<[Opcode]>> {
        let key = Arc::as_ptr(function) as usize;
        if let Some(opcodes) = self.functions.get(&key) {
            return Ok(opcodes.clone());
        }

        let Some(body) = function.body() else {
            return Err(anyhow!("Function has no body"));
        };

        let mut opcodes = vec![];
        let mut opcode_map = HashMap::new();

        let mut cursor = Cursor::new(body.opcodes());
        while cursor.position() != body.opcodes().len() as u64 {
            let position = cursor.position() as u32;
            let opcode = Opcode::read(&mut cursor)?;
            opcode_map.insert(position, opcodes.len() as u32);
            opcodes.push(opcode);
        }

        for opcode in &mut opcodes {
            match opcode {
                | Opcode::Jump(t0)
                | Opcode::CondJump(t0)
                | Opcode::CondJumpN(t0)=> {
                    *t0 = opcode_map[t0];
                },
                _ => {},
            }
        }

        let opcodes = self.functions.entry(key).or_insert(Rc::from(opcodes));
        Ok(opcodes.clone())
    }
}
