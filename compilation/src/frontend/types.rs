use leaf_reflection::{ElementRef, TypeSignature};
use std::fmt::{Debug, Display, Formatter};
use leaf_parsing::ast::Type as TypeNode;
use std::sync::{Arc, OnceLock, Weak};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Decimal(u32),
    Integer(u32),
    UInteger(u32),
    Struct(Weak<StructType>),
}

impl Type {
    pub const INT8: Type = Type::Integer(1);
    pub const INT16: Type = Type::Integer(2);
    pub const INT32: Type = Type::Integer(4);
    pub const INT64: Type = Type::Integer(8);
    pub const UINT8: Type = Type::UInteger(1);
    pub const UINT16: Type = Type::UInteger(2);
    pub const UINT32: Type = Type::UInteger(4);
    pub const UINT64: Type = Type::UInteger(8);
    pub const DEC16: Type = Type::Decimal(2);
    pub const DEC32: Type = Type::Decimal(4);
    pub const DEC64: Type = Type::Decimal(8);
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Decimal(lhs), Type::Decimal(rhs)) => lhs == rhs,
            (Type::Integer(lhs), Type::Integer(rhs)) => lhs == rhs,
            (Type::UInteger(lhs), Type::UInteger(rhs)) => lhs == rhs,
            (Type::Struct(lhs), Type::Struct(rhs)) => lhs.ptr_eq(&rhs),
            _ => false,
        }
    }
}

impl From<&Type> for Type {
    fn from(value: &Type) -> Self {
        value.clone()
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let discriminant = std::mem::discriminant(self);
        discriminant.hash(state);

        match self {
            Type::Void => {},
            | Type::Decimal(size)
            | Type::Integer(size)
            | Type::UInteger(size) => state.write_u32(*size),
            Type::Struct(ptr) => state.write_usize(ptr.as_ptr() as usize),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => f.write_str("void"),
            Type::Decimal(size) => match *size {
                2 => f.write_str("f16"),
                4 => f.write_str("f32"),
                8 => f.write_str("f64"),
                _ => unreachable!(),
            }
            Type::Integer(size) => match *size {
                1 => f.write_str("i8"),
                2 => f.write_str("i16"),
                4 => f.write_str("i32"),
                8 => f.write_str("i64"),
                _ => unreachable!(),
            }
            Type::UInteger(size) => match *size {
                1 => f.write_str("u8"),
                2 => f.write_str("u16"),
                4 => f.write_str("u32"),
                8 => f.write_str("u64"),
                _ => unreachable!(),
            }
            Type::Struct(data) => match data.upgrade() {
                None => f.write_str("struct"),
                Some(data) => write! {
                    f, "{}::{}",
                    data.namespace,
                    match &*data.name {
                        "" => "<anonymous>",
                        name => name,
                    }
                }
            }
        }
    }
}

pub struct StructType {
    pub name: Arc<str>,
    pub namespace: Arc<str>,
    pub fields: OnceLock<Vec<Field>>,
    pub metadata: ElementRef<leaf_reflection::TypeDef>,
}

impl Debug for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = format! {
            "struct {}::{}",
            self.namespace,
            match &*self.name {
                "" => "<anonymous>",
                name => name,
            }
        };

        let mut dbg = f.debug_struct(&name);
        let Some(fields) = self.fields.get() else {
            return dbg.finish_non_exhaustive();
        };

        for field in fields {
            dbg.field(&field.name, &format_args!("{}", field.r#type));
        }

        dbg.finish()
    }
}

#[derive(Debug)]
pub struct Field {
    pub name: Arc<str>,
    pub r#type: Type,
}

#[derive(Clone)]
pub enum TypeDef {
    Struct(Arc<StructType>),
}

impl TypeDef {
    pub fn name(&self) -> &Arc<str> {
        match self {
            TypeDef::Struct(s) => &s.name,
        }
    }
}

impl From<TypeDef> for Type {
    fn from(value: TypeDef) -> Self {
        Type::from(&value)
    }
}

impl From<&TypeDef> for Type {
    fn from(value: &TypeDef) -> Self {
        match value {
            TypeDef::Struct(v) => Type::Struct(Arc::downgrade(&v)),
        }
    }
}

impl Debug for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDef::Struct(data) => data.fmt(f),
        }
    }
}

pub trait TypeResolver
    where Type: for<'l> From<&'l <Self as TypeResolver>::Type>
{
    type Type;
    fn types(&self) -> &HashMap<Arc<str>, Self::Type>;
    fn resolve_type(&self, ast: &TypeNode) -> anyhow::Result<Type> {
        match ast {
            TypeNode::Id(id) => match *id {
                "void" => Ok(Type::Void),
                "i8" => Ok(Type::Integer(1)),
                "i16" => Ok(Type::Integer(2)),
                "i32" => Ok(Type::Integer(4)),
                "i64" => Ok(Type::Integer(8)),
                "u8" => Ok(Type::UInteger(1)),
                "u16" => Ok(Type::UInteger(2)),
                "u32" => Ok(Type::UInteger(4)),
                "u64" => Ok(Type::UInteger(8)),
                "f16" => Ok(Type::Decimal(2)),
                "f32" => Ok(Type::Decimal(4)),
                "f64" => Ok(Type::Decimal(8)),
                _ => match self.types().get(*id) {
                    Some(ty) => Ok(ty.into()),
                    None => Err(anyhow::Error::msg(format!("Type {:?} is not available in the current scope", id))),
                }
            }
            _ => unimplemented!(),
        }
    }
}

pub enum TypeSignatureBlob {
    Vec(Vec<u8>),
    Arc(Arc<TypeSignature>),
    Static(&'static TypeSignature),
    Small([u8; std::mem::size_of::<usize>()], usize),
}

impl AsRef<TypeSignature> for TypeSignatureBlob {
    fn as_ref(&self) -> &TypeSignature {
        match self {
            TypeSignatureBlob::Vec(v) => v,
            TypeSignatureBlob::Arc(v) => v,
            TypeSignatureBlob::Static(v) => v,
            TypeSignatureBlob::Small(data, len) => &data[..*len],
        }
    }
}

pub trait TypeSignatureBuilder {
    fn get_or_create_type_signature(&mut self, ty: &Type) -> TypeSignatureBlob;
}
