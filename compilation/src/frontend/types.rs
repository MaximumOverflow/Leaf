use leaf_parsing::ast::Type as TypeNode;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use std::sync::Arc;
use anyhow::anyhow;

pub trait TypeResolver {
    fn types(&self) -> &HashMap<Arc<str>, Arc<Type>>;
    fn resolve_type(&self, ast: &TypeNode) -> anyhow::Result<Arc<Type>> {
        match ast {
            TypeNode::Id(id) => match *id {
                "void" => Ok(Type::void().clone()),
                "i8"  => Ok(Type::i8().clone()),
                "i16" => Ok(Type::i16().clone()),
                "i32" => Ok(Type::i32().clone()),
                "i64" => Ok(Type::i64().clone()),
                "u8"  => Ok(Type::u8().clone()),
                "u16" => Ok(Type::u16().clone()),
                "u32" => Ok(Type::u32().clone()),
                "u64" => Ok(Type::u64().clone()),
                "f16" => Ok(Type::f16().clone()),
                "f32" => Ok(Type::f32().clone()),
                "f64" => Ok(Type::f64().clone()),
                _ => match self.types().get(*id) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(anyhow::Error::msg(format!("Type {:?} is not available in the current scope", id))),
                }
            }
            TypeNode::Pointer(ty, mutable) => {
                let ty = self.resolve_type(ty)?;
                Ok(ty.make_ptr(*mutable).clone())
            }
            _ => unimplemented!(),
        }
    }
}

pub fn invalid_type_err(expected: &Arc<Type>, got: Option<&Arc<Type>>) -> anyhow::Error {
    match got {
        None => anyhow!("Expected type '{}', got '?'", expected),
        Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
    }
}
