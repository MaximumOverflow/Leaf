use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_parsing::ast::{BinaryOperator, Expression, Literal};
use crate::frontend::block::{Block, BlockRequirements};
use leaf_reflection::structured::types::TypeVariant;
use leaf_reflection::structured::Type;
use leaf_reflection::Opcode;
use std::sync::Arc;

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
            let rhs = compile_expression(block, rhs, builder)?;
            rhs.load(builder);

            let lhs = compile_expression(block, lhs, builder)?;
            lhs.load(builder);

            let rhs_type = rhs.r#type();
            let lhs_type = lhs.r#type();

            if lhs_type != rhs_type {
                unimplemented!();
            }

            match ((**lhs_type).as_ref(), (**rhs_type).as_ref()) {
                | (TypeVariant::Int(_), TypeVariant::Int(_))
                | (TypeVariant::UInt(_), TypeVariant::UInt(_)) => {
                    match op {
                        BinaryOperator::Add => builder.add(),
                        BinaryOperator::Sub => builder.sub(),
                        BinaryOperator::Mul => builder.mul(),
                        BinaryOperator::Div => builder.div(),
                        BinaryOperator::Mod => builder.rem(),
                        _ => unimplemented!("{:?} {:?}, {:?}", op, lhs_type, rhs_type),
                    };
                    Ok(Value::Temp(lhs_type.clone()))
                }
                _ => unimplemented!("{:?} {:?}, {:?}", op, lhs_type, rhs_type),
            }
        }
        _ => unimplemented!(),
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Temp(Arc<Type>),
    Const(Arc<Type>),
    Local(Arc<Type>, usize),
    Param(Arc<Type>, usize),
}

impl Value {
    pub fn r#type(&self) -> &Arc<Type> {
        match self {
            Value::Temp(t) => t,
            Value::Const(t) => t,
            Value::Local(t, _) => t,
            Value::Param(t, _) => t,
        }
    }

    pub fn load(&self, opcodes: &mut FunctionBodyBuilder) {
        match self {
            Value::Local(_, i) => {
                opcodes.push_opcode(Opcode::PushLocal(*i));
            },
            Value::Param(_, i) => {
                opcodes.push_opcode(Opcode::PushParam(*i));
            },
            _ => {},
        }
    }
}
