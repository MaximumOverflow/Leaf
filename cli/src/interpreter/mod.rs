use std::io::{Cursor, Error};
use leaf_compilation::reflection::structured::{Function, Type};
use crate::interpreter::value::Value;
use std::sync::Arc;
use anyhow::anyhow;
use leaf_compilation::reflection::{MetadataRead, Opcode};

mod value;

pub fn interpret(function: &Arc<Function>, mut params: Vec<Value>) -> anyhow::Result<Value> {
    let good_params = params
        .iter()
        .zip(function.parameters())
        .filter(|(v, p)| v.ty() == p.ty())
        .count();

    if good_params != function.parameters().len() {
        return Err(anyhow!("Invalid function parameters"));
    }

    let Some(body) = function.body() else {
        return Err(anyhow!("Function has no body"));
    };

    let mut locals = Vec::with_capacity(body.locals().len());
    for local in body.locals() {
        locals.push(Value::new_uninit(local.ty().clone()));
    }

    let mut stack = Vec::<Value>::new();

    let mut cursor = Cursor::new(body.opcodes());
    while cursor.position() != body.opcodes().len() as u64 {
        let opcode = Opcode::read(&mut cursor)?;
        match opcode {
            _ => return Err(anyhow!("Unimplemented opcode {:?}", opcode)),
        }
    }

    Err(anyhow!("Function exited unexpectedly"))
}

