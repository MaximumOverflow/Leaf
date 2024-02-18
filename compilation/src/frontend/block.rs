use leaf_reflection::structured::functions::FunctionBodyBuilder;
use leaf_parsing::ast::{Block as BlockAST, Statement};
use crate::frontend::expressions::compile_expression;
use crate::frontend::expressions::Value;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use std::sync::Arc;

pub struct Block<'l> {
    parent: &'l mut dyn BlockRequirements,
    values: HashMap<Arc<str>, Value>,
}

pub trait BlockRequirements {
    fn expected_type(&self) -> &Arc<Type>;
    fn values(&self) -> &HashMap<Arc<str>, Value>;
    fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value>;
}

impl<'l> Block<'l> {
    pub fn new(parent: &'l mut dyn BlockRequirements) -> Self {
        Self { values: parent.values().clone(), parent }
    }
    pub fn compile(&self, block: &BlockAST, builder: &mut FunctionBodyBuilder) -> anyhow::Result<()> {
        for statement in &block.statements {
            match statement {
                Statement::Return(None) => {
                    let expected = self.parent.expected_type();
                    match expected == Type::void() {
                        false => {
                            builder.ret();
                        },
                        true => return Err(anyhow::Error::msg(format!(r#"Expected type "{}", got "void""#, expected))),
                    }
                }

                Statement::Return(Some(expr)) => {
                    let expected = self.parent.expected_type();
                    let expr = compile_expression(self, expr, builder)?;
                    if expr.r#type() != expected {
                        return Err(anyhow::Error::msg(format!(r#"Expected type "{}", got "{}""#, expected, expr.r#type())));
                    }
                    builder.ret();
                }

                _ => unimplemented!("{:?}", statement),
            }
        }
        Ok(())
    }
}

impl BlockRequirements for Block<'_> {
    fn expected_type(&self) -> &Arc<Type> {
        self.parent.expected_type()
    }

    fn values(&self) -> &HashMap<Arc<str>, Value> {
        &self.values
    }

    fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value> {
        &mut self.values
    }
}
