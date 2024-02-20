use crate::frontend::block::{Block, BlockRequirements, invalid_type_err};
use leaf_parsing::ast::{BinaryOperator, Expression, Integer, Literal};
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_reflection::structured::types::TypeVariant;
use crate::frontend::types::TypeResolver;
use leaf_reflection::structured::Type;
use leaf_reflection::Opcode;
use std::sync::Arc;
use anyhow::anyhow;

pub fn compile_expression(block: &Block, expr: &Expression, builder: &mut FunctionBodyBuilder) -> anyhow::Result<Value> {
    match expr {
        Expression::Literal(literal) => match literal {
            Literal::Integer(Integer::Any(integer)) => {
                let required_bits = 64 - integer.leading_zeros().min(64);
                match required_bits <= 32 {
                    true => {
                        builder.push_opcode(Opcode::PushInt32(*integer as i32));
                        Ok(Value::Const(Type::i32().clone()))
                    }
                    false => {
                        builder.push_opcode(Opcode::PushInt64(*integer as i64));
                        Ok(Value::Const(Type::i64().clone()))
                    }
                }
            }
            Literal::Integer(Integer::Int8(integer)) => {
                builder.push_opcode(Opcode::PushInt8(*integer));
                Ok(Value::Const(Type::i8().clone()))
            }
            Literal::Integer(Integer::Int16(integer)) => {
                builder.push_opcode(Opcode::PushInt16(*integer));
                Ok(Value::Const(Type::i16().clone()))
            }
            Literal::Integer(Integer::Int32(integer)) => {
                builder.push_opcode(Opcode::PushInt32(*integer));
                Ok(Value::Const(Type::i32().clone()))
            }
            Literal::Integer(Integer::Int64(integer)) => {
                builder.push_opcode(Opcode::PushInt64(*integer));
                Ok(Value::Const(Type::i64().clone()))
            }
            Literal::Integer(Integer::UInt8(integer)) => {
                builder.push_opcode(Opcode::PushUInt8(*integer));
                Ok(Value::Const(Type::u8().clone()))
            }
            Literal::Integer(Integer::UInt16(integer)) => {
                builder.push_opcode(Opcode::PushUInt16(*integer));
                Ok(Value::Const(Type::u16().clone()))
            }
            Literal::Integer(Integer::UInt32(integer)) => {
                builder.push_opcode(Opcode::PushUInt32(*integer));
                Ok(Value::Const(Type::u32().clone()))
            }
            Literal::Integer(Integer::UInt64(integer)) => {
                builder.push_opcode(Opcode::PushUInt64(*integer));
                Ok(Value::Const(Type::u64().clone()))
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
                        builder.push_local_address(local);
                        builder.store_field(field_idx);
                    }

                    builder.push_local(local);
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

    pub fn load(&self, opcodes: &mut FunctionBodyBuilder) -> anyhow::Result<usize> {
        match self {
            Value::Local(_, i, _, init) => match init {
                true => Ok(opcodes.push_local(*i).unwrap()),
                false => Err(anyhow!("Value is uninitialized"))
            },
            Value::Param(_, i, ..) => {
                Ok(opcodes.push_param_address(*i).unwrap())
            },
            _ => Ok(opcodes.ir_offset()),
        }
    }
}
