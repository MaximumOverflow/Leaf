#[derive(Debug)]
pub enum Type {
    Decimal(u32),
    Integer(u32),
    UInteger(u32),
    Struct(StructType),
}

#[derive(Debug)]
pub struct StructType {
    name: String,
    namespace: String,
}

impl Type {
    pub const INT8: &'static Type = &Type::Integer(1);
    pub const INT16: &'static Type = &Type::Integer(2);
    pub const INT32: &'static Type = &Type::Integer(4);
    pub const INT64: &'static Type = &Type::Integer(8);
    pub const UINT8: &'static Type = &Type::UInteger(1);
    pub const UINT16: &'static Type = &Type::UInteger(2);
    pub const UINT32: &'static Type = &Type::UInteger(4);
    pub const UINT64: &'static Type = &Type::UInteger(8);
    pub const DEC16: &'static Type = &Type::Decimal(2);
    pub const DEC32: &'static Type = &Type::Decimal(4);
    pub const DEC64: &'static Type = &Type::Decimal(8);
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Decimal(lhs), Type::Decimal(rhs)) => lhs == rhs,
            (Type::Integer(lhs), Type::Integer(rhs)) => lhs == rhs,
            (Type::UInteger(lhs), Type::UInteger(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}
