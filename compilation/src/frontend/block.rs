use leaf_parsing::ast::{Block as BlockAST, Expression, Literal, Statement};
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use crate::frontend::expressions::compile_expression;
use crate::frontend::types::TypeResolver;
use crate::frontend::expressions::Value;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use std::sync::Arc;

pub struct Block<'l> {
    parent: &'l mut dyn BlockRequirements,
    values: HashMap<Arc<str>, Value>,
}

pub trait BlockRequirements: TypeResolver {
    fn expected_type(&self) -> &Arc<Type>;
    fn values(&self) -> &HashMap<Arc<str>, Value>;
    fn values_mut(&mut self) -> &mut HashMap<Arc<str>, Value>;
}

impl<'l> Block<'l> {
    pub fn new(parent: &'l mut dyn BlockRequirements) -> Self {
        Self { values: parent.values().clone(), parent }
    }
    pub fn compile(&mut self, block: &BlockAST, builder: &mut FunctionBodyBuilder) -> anyhow::Result<()> {
        for statement in &block.statements {
            match statement {
                Statement::Return(None) => {
                    let expected = self.parent.expected_type();
                    match expected == Type::void() {
                        false => {
                            builder.push_opcode(Opcode::Ret);
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
                    builder.push_opcode(Opcode::Ret);
                }

                Statement::VarDecl(decl) => {
                    let ty = self.resolve_type(decl.ty.as_ref().unwrap())?;
                    let local = match builder.declare_local(decl.name, &ty) {
                        Ok(local) => local,
                        Err(_) => 'decl: {
                            let mut name = String::from(decl.name);
                            for _ in 0.. {
                                name.push('`');
                                if let Ok(local) = builder.declare_local(&name, &ty) {
                                    break 'decl local
                                }
                            }
                            unreachable!()
                        }
                    };

                    let id = local.id();
                    let name = local.name_arc().clone();

                    match decl.value {
                        Expression::Literal(Literal::Uninit) => {},
                        _ => {
                            compile_expression(self, &decl.value, builder)?;
                            builder.push_opcode(Opcode::StoreLocal(id));
                        }
                    }

                    self.values.insert(name, Value::Local(ty, id));
                }

                _ => unimplemented!("{:?}", statement),
            }
        }
        Ok(())
    }
}

impl TypeResolver for Block<'_> {
    fn types(&self) -> &HashMap<Arc<str>, Arc<Type>> {
        self.parent.types()
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
