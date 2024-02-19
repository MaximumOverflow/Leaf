use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_parsing::ast::{BinaryOperator, Expression, Literal};
use crate::frontend::block::{Block, BlockRequirements};
use leaf_reflection::structured::types::TypeVariant;
use crate::frontend::types::TypeResolver;
use leaf_reflection::structured::Type;
use leaf_reflection::Opcode;
use std::sync::Arc;
use anyhow::anyhow;

pub fn compile_expression(block: &Block, expr: &Expression, builder: &mut FunctionBodyBuilder) -> anyhow::Result<Value> {
    match expr {
        Expression::Literal(literal) => match literal {
            Literal::Integer(integer) => {
                let required_bits = 64 - integer.leading_zeros();
                match required_bits <= 32 {
                    true => {
                        builder.push_opcode(Opcode::PushInt32(*integer as i32));
                        Ok(Value::Const(Type::i32().clone()))
                    }
                    false => {
                        builder.push_opcode(Opcode::PushInt64(*integer));
                        Ok(Value::Const(Type::i64().clone()))
                    }
                }
            }

            Literal::Id(id) => {
                match block.values().get(*id) {
                    Some(value) => Ok(value.clone()),
                    None => Err(anyhow::Error::msg(format!("Identifier {:?} not found in the current scope", id)))
                }
            }

            _ => unimplemented!(),
        }

        Expression::Binary(lhs, op, rhs) => {
            let lhs = compile_expression(block, lhs, builder)?;
            lhs.load(builder)?;

            let rhs = compile_expression(block, rhs, builder)?;
            rhs.load(builder)?;

            let rhs_type = rhs.ty();
            let lhs_type = lhs.ty();

            if lhs_type != rhs_type {
                unimplemented!();
            }

            match ((**lhs_type).as_ref(), (**rhs_type).as_ref()) {
                | (TypeVariant::Int(_), TypeVariant::Int(_))
                | (TypeVariant::UInt(_), TypeVariant::UInt(_)) => {
                    match op {
                        BinaryOperator::Add => builder.push_opcode(Opcode::Add),
                        BinaryOperator::Sub => builder.push_opcode(Opcode::Sub),
                        BinaryOperator::Mul => builder.push_opcode(Opcode::Mul),
                        BinaryOperator::Div => builder.push_opcode(Opcode::Div),
                        BinaryOperator::Mod => builder.push_opcode(Opcode::Mod),
                        _ => unimplemented!("{:?} {:?}, {:?}", op, lhs_type, rhs_type),
                    };
                    Ok(Value::Temp(lhs_type.clone()))
                }
                _ => unimplemented!("{:?} {:?}, {:?}", op, lhs_type, rhs_type),
            }
        }

        Expression::NewStruct(new_struct) => {
            let ty = block.resolve_type(&new_struct.ty)?;
            match ty.as_ref().as_ref() {
                TypeVariant::Struct(s_ty) => {
                    for field in s_ty.fields().iter().rev() {
                        let value = match new_struct.values.get(field.name().as_ref()) {
                            Some(value) => compile_expression(block, value, builder)?,
                            None => return Err(anyhow!("Missing field '{}'", field.name())),
                        };

                        if *value.ty() != field.ty() {
                            return Err(anyhow!("Expected value of type '{}', found {}", field.ty(), value.ty()));
                        }

                        value.load(builder)?;
                    }

                    let local = builder.declare_local(&ty).id();
                    for i in 0..s_ty.fields().len() {
                        builder.push_opcode(Opcode::PushLocalA(local));
                        builder.push_opcode(Opcode::StoreField(i));
                    }

                    builder.push_opcode(Opcode::PushLocal(local));
                    Ok(Value::Temp(ty))
                }
                _ => Err(anyhow!("Type '{}' is not a struct", ty)),
            }
        }

        _ => unimplemented!(),
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Temp(Arc<Type>),
    Const(Arc<Type>),
    Param(Arc<Type>, usize, bool),
    Local(Arc<Type>, usize, Mutability, bool),
}

#[repr(u8)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Mutability {
    Mutable = 1,
    Immutable = 0,
}

impl Value {
    pub fn ty(&self) -> &Arc<Type> {
        match self {
            Value::Temp(t) => t,
            Value::Const(t) => t,
            Value::Local(t, ..) => t,
            Value::Param(t, ..) => t,
        }
    }

    pub fn load(&self, opcodes: &mut FunctionBodyBuilder) -> anyhow::Result<usize> {
        match self {
            Value::Local(_, i, _, init) => match init {
                true => Ok(opcodes.push_opcode(Opcode::PushLocal(*i))),
                false => Err(anyhow!("Value is uninitialized"))
            },
            Value::Param(_, i, ..) => {
                Ok(opcodes.push_opcode(Opcode::PushParam(*i)))
            },
            _ => Ok(opcodes.ir_offset()),
        }
    }
}
