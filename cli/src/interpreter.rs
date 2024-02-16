use leaf_compilation::reflection::{FunctionDef, MetadataRead, Opcode, TypeSignature, TypeSignatureTag};
use leaf_compilation::reflection::builders::MetadataBuilder;
use std::io::Cursor;
use half::f16;

#[derive(Debug, Clone)]
pub enum Value {
    Uninit,
    Int32(i32),
    Int64(i64),
    Dec16(f16),
    Dec32(f32),
    Dec64(f64),
}

pub fn interpret(metadata: &MetadataBuilder, function: &FunctionDef) -> anyhow::Result<Value> {
    let body = function.body();
    let ret_ty = metadata.get_blob(function.return_ty()).unwrap();
    let mut opcodes = Cursor::new(metadata.get_blob(body.opcodes()).unwrap());

    let mut stack = vec![];
    let mut locals: Vec<_> = match metadata.get_locals(function) {
        None => panic! {
            "Could not create locals for {}",
            metadata
                .get_str(function.name())
                .unwrap_or("[Failed to get function name too]")
        },
        Some(locals) => locals.map(|local| {
            (local, Value::Uninit)
        }).collect(),
    };

    loop {
        let offset = opcodes.position();
        let Some(opcode) = Option::<Opcode>::read(&mut opcodes)? else { break };

        if cfg!(debug_assertions) {
            eprintln!("IR_0x{:04X}: {:?}", offset, opcode);
        }

        match opcode {
            Opcode::Ret => {
                let value = stack.pop().unwrap();
                return Ok(check_type(value, ret_ty)?);
            }

            Opcode::Add => {
                let lhs = stack.pop().unwrap();
                let rhs = stack.pop().unwrap();
                match (&lhs, &rhs) {
                    (Value::Int32(lhs), Value::Int32(rhs)) => stack.push(Value::Int32(lhs + rhs)),
                    (Value::Int64(lhs), Value::Int64(rhs)) => stack.push(Value::Int64(lhs + rhs)),
                    (Value::Dec64(lhs), Value::Dec64(rhs)) => stack.push(Value::Dec64(lhs + rhs)),
                    _ => unimplemented!("{:?} {:?}, {:?}", opcode, lhs, rhs),
                }
            }

            Opcode::Mul => {
                let lhs = stack.pop().unwrap();
                let rhs = stack.pop().unwrap();
                match (&lhs, &rhs) {
                    (Value::Int32(lhs), Value::Int32(rhs)) => stack.push(Value::Int32(lhs * rhs)),
                    (Value::Int64(lhs), Value::Int64(rhs)) => stack.push(Value::Int64(lhs * rhs)),
                    (Value::Dec64(lhs), Value::Dec64(rhs)) => stack.push(Value::Dec64(lhs * rhs)),
                    _ => unimplemented!("{:?} {:?}, {:?}", opcode, lhs, rhs),
                }
            }

            Opcode::PushInt32(value) => stack.push(Value::Int32(value)),
            Opcode::PushDecimal64(value) => stack.push(Value::Dec64(value)),

            Opcode::PushLocal(local) => {
                let (_, ref mut local) = locals[local];
                stack.push(local.clone())
            }

            Opcode::ConvDecimal(size) => {
                let mut value = stack.pop().unwrap();
                match &value {
                    | Value::Int32(_)
                    | Value::Int64(_)
                    | Value::Dec16(_)
                    | Value::Dec32(_)
                    | Value::Dec64(_) => match size {
                        2 => value = Value::Dec16(value.try_into()?),
                        4 => value = Value::Dec32(value.try_into()?),
                        8 => value = Value::Dec64(value.try_into()?),
                        _ => panic!("Unsupported decimal size {}", size),
                    }

                    Value::Uninit => panic!("Attempted to use an uninitialized value."),
                }
                stack.push(value);
            }

            _=> unimplemented!("Unimplemented opcode {:?}", opcode),
        }
    }

    Err(anyhow::Error::msg("Function did not return correctly."))
}

fn check_type(value: Value, expected: &TypeSignature) -> anyhow::Result<Value> {
    const INT32: u8 = TypeSignatureTag::Integer32 as u8;
    const INT64: u8 = TypeSignatureTag::Integer64 as u8;
    const DEC16: u8 = TypeSignatureTag::Decimal16 as u8;
    const DEC32: u8 = TypeSignatureTag::Decimal32 as u8;
    const DEC64: u8 = TypeSignatureTag::Decimal64 as u8;
    match (&value, expected) {
        (Value::Int32(_), [INT32]) => Ok(value),
        (Value::Int64(_), [INT64]) => Ok(value),
        (Value::Dec16(_), [DEC16]) => Ok(value),
        (Value::Dec32(_), [DEC32]) => Ok(value),
        (Value::Dec64(_), [DEC64]) => Ok(value),
        (_, _) => Err(anyhow::Error::msg("Mismatched type signature.")),
    }
}

impl TryInto<f16> for Value {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<f16, Self::Error> {
        match self {
            Value::Dec16(v) => Ok(v),
            Value::Dec32(v) => Ok(f16::from_f32(v)),
            Value::Dec64(v) => Ok(f16::from_f64(v)),
            Value::Int32(v) => Ok(f16::from_f32(v as f32)),
            Value::Int64(v) => Ok(f16::from_f32(v as f32)),
            _ => Err(anyhow::Error::msg("Value cannot be cast to type f16")),
        }
    }
}

impl TryInto<f32> for Value {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<f32, Self::Error> {
        match self {
            Value::Int32(v) => Ok(v as f32),
            Value::Int64(v) => Ok(v as f32),
            Value::Dec16(v) => Ok(v.to_f32()),
            Value::Dec32(v) => Ok(v),
            Value::Dec64(v) => Ok(v as f32),
            _ => Err(anyhow::Error::msg("Value cannot be cast to type f32")),
        }
    }
}

impl TryInto<f64> for Value {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Value::Dec64(v) => Ok(v),
            Value::Dec32(v) => Ok(v as f64),
            Value::Dec16(v) => Ok(v.to_f64()),
            Value::Int32(v) => Ok(v as f64),
            Value::Int64(v) => Ok(v as f64),
            _ => Err(anyhow::Error::msg("Value cannot be cast to type f64")),
        }
    }
}
