use leaf_compilation::reflection::{FunctionDef, TypeSignature, TypeSignatureTag};
use leaf_compilation::reflection::builders::MetadataBuilder;
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
    unimplemented!()
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
