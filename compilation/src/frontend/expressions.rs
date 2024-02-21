use crate::frontend::block::{Block, BlockRequirements};
use leaf_parsing::ast::{BinaryOperator, Expression, Integer, Literal};
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_reflection::structured::types::TypeVariant;
use crate::frontend::types::{invalid_type_err, TypeResolver};
use leaf_reflection::structured::Type;
use leaf_reflection::{Encoded, Opcode};
use std::sync::Arc;
use anyhow::anyhow;

pub fn compile_expression(block: &Block, expr: &Expression, builder: &mut FunctionBodyBuilder) -> anyhow::Result<Value> {
    match expr {
        Expression::Literal(literal) => match literal {
            Literal::Integer(Integer::Any(integer)) => {
                let required_bits = 64 - integer.leading_zeros().min(64);
                match required_bits <= 32 {
                    true => {
                        builder.push_opcode(Opcode::PushInt32(Encoded(*integer as i32)));
                        Ok(Value::Const(Type::i32().clone()))
                    }
                    false => {
                        builder.push_opcode(Opcode::PushInt64(Encoded(*integer as i64)));
                        Ok(Value::Const(Type::i64().clone()))
                    }
                }
            }
            Literal::Integer(Integer::Int8(integer)) => {
                builder.push_opcode(Opcode::PushInt8(*integer));
                Ok(Value::Const(Type::i8().clone()))
            }
            Literal::Integer(Integer::Int16(integer)) => {
                builder.push_opcode(Opcode::PushInt16(Encoded(*integer)));
                Ok(Value::Const(Type::i16().clone()))
            }
            Literal::Integer(Integer::Int32(integer)) => {
                builder.push_opcode(Opcode::PushInt32(Encoded(*integer)));
                Ok(Value::Const(Type::i32().clone()))
            }
            Literal::Integer(Integer::Int64(integer)) => {
                builder.push_opcode(Opcode::PushInt64(Encoded(*integer)));
                Ok(Value::Const(Type::i64().clone()))
            }
            Literal::Integer(Integer::UInt8(integer)) => {
                builder.push_opcode(Opcode::PushUInt8(*integer));
                Ok(Value::Const(Type::u8().clone()))
            }
            Literal::Integer(Integer::UInt16(integer)) => {
                builder.push_opcode(Opcode::PushUInt16(Encoded(*integer)));
                Ok(Value::Const(Type::u16().clone()))
            }
            Literal::Integer(Integer::UInt32(integer)) => {
                builder.push_opcode(Opcode::PushUInt32(Encoded(*integer)));
                Ok(Value::Const(Type::u32().clone()))
            }
            Literal::Integer(Integer::UInt64(integer)) => {
                builder.push_opcode(Opcode::PushUInt64(Encoded(*integer)));
                Ok(Value::Const(Type::u64().clone()))
            }
            Literal::Boolean(boolean) => {
                builder.push_opcode(Opcode::PushBool(*boolean));
                Ok(Value::Const(Type::bool().clone()))
            }

            Literal::Id(id) => {
                match block.values().get(*id) {
                    Some(value) => Ok(value.clone()),
                    None => Err(anyhow::Error::msg(format!("Identifier {:?} not found in the current scope", id)))
                }
            }

            Literal::Uninit => Ok(Value::Uninit),

            _ => unimplemented!(),
        }

        Expression::Binary(lhs, op, rhs) => {
            let lhs = compile_expression(block, lhs, builder)?;
            lhs.load(builder)?;

            let rhs = compile_expression(block, rhs, builder)?;
            rhs.load(builder)?;

            let rhs_type = rhs.ty().unwrap();
            let lhs_type = lhs.ty().unwrap();

            if lhs_type != rhs_type {
                unimplemented!();
            }

            match ((**lhs_type).as_ref(), (**rhs_type).as_ref()) {
                | (TypeVariant::Int8, TypeVariant::Int8)
                | (TypeVariant::Int16, TypeVariant::Int16)
                | (TypeVariant::Int32, TypeVariant::Int32)
                | (TypeVariant::Int64, TypeVariant::Int64)
                | (TypeVariant::UInt8, TypeVariant::UInt8)
                | (TypeVariant::UInt16, TypeVariant::UInt16)
                | (TypeVariant::UInt32, TypeVariant::UInt32)
                | (TypeVariant::UInt64, TypeVariant::UInt64) => {
                    match op {
                        BinaryOperator::Add => builder.push_opcode(Opcode::Add),
                        BinaryOperator::Sub => builder.push_opcode(Opcode::Sub),
                        BinaryOperator::Mul => builder.push_opcode(Opcode::Mul),
                        BinaryOperator::Div => builder.push_opcode(Opcode::Div),
                        BinaryOperator::Mod => builder.push_opcode(Opcode::Mod),
                        BinaryOperator::Lt => builder.push_opcode(Opcode::Lt),
                        BinaryOperator::Gt => builder.push_opcode(Opcode::Gt),
                        BinaryOperator::Le => builder.push_opcode(Opcode::Le),
                        BinaryOperator::Ge => builder.push_opcode(Opcode::Ge),
                        BinaryOperator::Eq => builder.push_opcode(Opcode::Eq),
                        BinaryOperator::Neq => builder.push_opcode(Opcode::Neq),
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
                    let local = builder.declare_local(&ty).id();
                    let mut fields: Vec<_> = new_struct.values.iter().collect();
                    fields.sort_by_key(|(_, (i, _))| i);

                    for (name, (_, value)) in fields {
                        let Some(field_idx) = s_ty.fields().iter().position(|f| &**f.name() == *name) else {
                            return Err(anyhow!("Type '{}' does not contain field '{}'", ty, name));
                        };

                        let field = &s_ty.fields()[field_idx];
                        let field_ty = field.ty();

                        let value = compile_expression(block, value, builder)?;

                        if value == Value::Uninit {
                            continue;
                        }

                        if value.ty() != Some(&field_ty) {
                            return Err(invalid_type_err(&field_ty, value.ty()));
                        }

                        value.load(builder)?;
                        builder.push_local_address(local).unwrap();
                        builder.store_field(field_idx);
                    }

                    builder.push_local(local).unwrap();
                    Ok(Value::Temp(ty))
                }
                _ => Err(anyhow!("Type '{}' is not a struct", ty)),
            }
        }

        Expression::Cast(expr, ty) => {
            let expr = compile_expression(block, expr, builder)?;
            let ty = block.resolve_type(ty)?;
            unimplemented!("{:?} as {}", expr, ty);
        }

        _ => unimplemented!(),
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Uninit,
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
    pub fn ty(&self) -> Option<&Arc<Type>> {
        match self {
            | Value::Temp(t)
            | Value::Const(t)
            | Value::Local(t, ..)
            | Value::Param(t, ..) => Some(t),
            _ => None,
        }
    }

    pub fn load(&self, opcodes: &mut FunctionBodyBuilder) -> anyhow::Result<()> {
        match self {
            Value::Local(_, i, _, init) => match init {
                true => Ok(opcodes.push_local(*i).unwrap()),
                false => Err(anyhow!("Value is uninitialized"))
            },
            Value::Param(_, i, ..) => {
                Ok(opcodes.push_param_address(*i).unwrap())
            },
            _ => Ok(()),
        }
    }
}
