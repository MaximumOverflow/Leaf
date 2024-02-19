use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::{Function, Type};
use leaf_compilation::reflection::{MetadataRead, Opcode};
use crate::interpreter::value::Value;
use anyhow::{anyhow, Context};
use std::io::Cursor;
use std::sync::Arc;

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
    const POP_ERR: &str = "Unexpected end of stack";
    const LOCAL_ERR: &str = "Invalid local id";

    let mut cursor = Cursor::new(body.opcodes());
    while cursor.position() != body.opcodes().len() as u64 {
        let opcode = Opcode::read(&mut cursor)?;

        macro_rules! binary_op {
            ($op: tt) => {
                {
                    let rhs = stack.pop().context(POP_ERR)?;
                    let lhs = stack.pop().context(POP_ERR)?;
                    match (lhs.ty().as_ref().as_ref(), rhs.ty().as_ref().as_ref()) {
                        (TypeVariant::Int(4), TypeVariant::Int(4)) => {
                            let lhs: i32 = lhs.try_into()?;
                            let rhs: i32 = rhs.try_into()?;
                            let res = Value::from(lhs $op rhs);
                            stack.push(res);
                        }
                        _ => unimplemented!(),
                    }
                }
            };
        }

        #[inline(never)]
        fn push_local(id: usize, stack: &mut Vec<Value>, locals: &mut Vec<Value>) -> anyhow::Result<()> {
            stack.push(locals.get(id).context(LOCAL_ERR)?.clone());
            Ok(())
        }

        #[inline(never)]
        fn store_local(id: usize, stack: &mut Vec<Value>, locals: &mut Vec<Value>) -> anyhow::Result<()> {
            let value = stack.pop().context(POP_ERR)?;
            let local = locals.get_mut(id).context(LOCAL_ERR)?;
            if local.ty() != value.ty() {
                return Err(anyhow!("Mismatched local type. Expected '{}', got '{}'", local.ty(), value.ty()));
            }
            *local = value;
            Ok(())
        }

        match opcode {
            Opcode::Ret => {
                if function.return_ty() == Type::void() {
                    return Ok(Value::from(()));
                }

                let value = stack.pop().context(POP_ERR)?;
                return match value.ty() == function.return_ty() {
                    true => Ok(value),
                    false => Err(anyhow!("Invalid return value type")),
                };
            }

            Opcode::Add => binary_op!(+),
            Opcode::Sub => binary_op!(-),
            Opcode::Mul => binary_op!(*),
            Opcode::Div => binary_op!(/),
            Opcode::Mod => binary_op!(%),

            Opcode::PushInt8(value) => stack.push(value.into()),
            Opcode::PushInt16(value) => stack.push(value.into()),
            Opcode::PushInt32(value) => stack.push(value.into()),
            Opcode::PushInt64(value) => stack.push(value.into()),
            Opcode::PushUInt8(value) => stack.push(value.into()),
            Opcode::PushUInt16(value) => stack.push(value.into()),
            Opcode::PushUInt32(value) => stack.push(value.into()),
            Opcode::PushUInt64(value) => stack.push(value.into()),
            Opcode::PushDecimal16(value) => stack.push(value.into()),
            Opcode::PushDecimal32(value) => stack.push(value.into()),
            Opcode::PushDecimal64(value) => stack.push(value.into()),

            Opcode::PushLocal0 => push_local(0, &mut stack, &mut locals)?,
            Opcode::PushLocal1 => push_local(1, &mut stack, &mut locals)?,
            Opcode::PushLocal2 => push_local(2, &mut stack, &mut locals)?,
            Opcode::PushLocal3 => push_local(3, &mut stack, &mut locals)?,
            Opcode::PushLocal4 => push_local(4, &mut stack, &mut locals)?,
            Opcode::PushLocal5 => push_local(5, &mut stack, &mut locals)?,
            Opcode::PushLocal6 => push_local(6, &mut stack, &mut locals)?,
            Opcode::PushLocal(id) => push_local(id, &mut stack, &mut locals)?,

            Opcode::StoreLocal0 => store_local(0, &mut stack, &mut locals)?,
            Opcode::StoreLocal1 => store_local(1, &mut stack, &mut locals)?,
            Opcode::StoreLocal2 => store_local(2, &mut stack, &mut locals)?,
            Opcode::StoreLocal3 => store_local(3, &mut stack, &mut locals)?,
            Opcode::StoreLocal4 => store_local(4, &mut stack, &mut locals)?,
            Opcode::StoreLocal5 => store_local(5, &mut stack, &mut locals)?,
            Opcode::StoreLocal6 => store_local(6, &mut stack, &mut locals)?,
            Opcode::StoreLocal(id) => store_local(id, &mut stack, &mut locals)?,

            Opcode::StoreParam(id) => {
                let value = stack.pop().context(POP_ERR)?;
                let param = params.get_mut(id).context(LOCAL_ERR)?;
                if param.ty() != value.ty() {
                    return Err(anyhow!("Mismatched parameter type. Expected '{}', got '{}'", param.ty(), value.ty()));
                }
                *param = value;
            },

            _ => return Err(anyhow!("Unimplemented opcode {:?}", opcode)),
        }
    }

    Err(anyhow!("Function exited unexpectedly"))
}

