use leaf_parsing::ast::{BinaryOperator, Expression, Literal};
use crate::frontend::block::{Block, BlockRequirements};
use crate::frontend::types::Type;
use leaf_reflection::Opcode;

pub fn compile_expression(block: &Block, expr: &Expression, opcodes: &mut Vec<Opcode>) -> anyhow::Result<Value> {
    match expr {
        Expression::Literal(literal) => match literal {
            Literal::Integer(integer) => {
                let required_bits = 64 - integer.leading_zeros();
                match required_bits <= 32 {
                    true => {
                        opcodes.push(Opcode::PushInt32(*integer as i32));
                        Ok(Value::Const(Type::INT32))
                    }
                    false => {
                        opcodes.push(Opcode::PushInt64(*integer));
                        Ok(Value::Const(Type::INT64))
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
            let mut rhs = compile_expression(block, rhs, opcodes)?;
            rhs.load(opcodes);

            let mut lhs = compile_expression(block, lhs, opcodes)?;
            lhs.load(opcodes);

            let rhs_type = rhs.r#type();
            let lhs_type = lhs.r#type();

            if lhs_type != rhs_type {
                unimplemented!();
            }

            match (&lhs_type, &rhs_type) {
                | (Type::Integer(_), Type::Integer(_))
                | (Type::UInteger(_), Type::UInteger(_)) => {
                    match op {
                        BinaryOperator::Add => opcodes.push(Opcode::Add),
                        BinaryOperator::Sub => opcodes.push(Opcode::Sub),
                        BinaryOperator::Mul => opcodes.push(Opcode::Mul),
                        BinaryOperator::Div => opcodes.push(Opcode::Div),
                        BinaryOperator::Mod => opcodes.push(Opcode::Mod),
                        _ => unimplemented!("{:?} {:?}, {:?}", op, lhs_type, rhs_type),
                    }
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
    Temp(Type),
    Const(Type),
    Local(Type, usize),
    Param(Type, usize),
}

impl Value {
    pub fn r#type(&self) -> &Type {
        match self {
            Value::Temp(t) => t,
            Value::Const(t) => t,
            Value::Local(t, _) => t,
            Value::Param(t, _) => t,
        }
    }

    pub fn load(&self, opcodes: &mut Vec<Opcode>) {
        match self {
            Value::Local(_, i) => opcodes.push(Opcode::PushLocal(*i)),
            Value::Param(_, i) => opcodes.push(Opcode::PushParam(*i)),
            _ => {},
        }
    }
}
