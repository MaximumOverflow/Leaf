use leaf_parsing::ast::{Block as BlockAST, Expression, Literal, Statement};
use crate::frontend::expressions::{compile_expression, Mutability};
use leaf_reflection::structured::functions::FunctionBodyBuilder;
use crate::frontend::types::TypeResolver;
use crate::frontend::expressions::Value;
use leaf_reflection::structured::Type;
use std::collections::HashMap;
use leaf_reflection::Opcode;
use std::sync::Arc;
use anyhow::anyhow;

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
                        true => {
                            builder.push_opcode(Opcode::Ret);
                        },
                        false => {
                            return Err(invalid_type_err(expected, Some(Type::void())));
                        },
                    }
                }

                Statement::Return(Some(expr)) => {
                    let expected = self.parent.expected_type();
                    let expr = compile_expression(self, expr, builder)?;
                    expr.load(builder)?;

                    if expr.ty() != Some(expected){
                        return Err(invalid_type_err(expected, expr.ty()));
                    }
                    builder.push_opcode(Opcode::Ret);
                }

                Statement::VarDecl(decl) => {
                    let ty = self.resolve_type(decl.ty.as_ref().unwrap())?;
                    let local = builder.declare_local(&ty);
                    let id = local.id();

                    let initialized = match decl.value {
                        Expression::Literal(Literal::Uninit) => false,
                        _ => {
                            let value = compile_expression(self, &decl.value, builder)?;
                            if Some(&ty) != value.ty() {
                                return Err(invalid_type_err(&ty, value.ty()))
                            }

                            value.load(builder)?;
                            builder.push_opcode(Opcode::StoreLocal(id));
                            true
                        }
                    };

                    let mutability = match decl.mutable {
                        true => Mutability::Mutable,
                        false => Mutability::Immutable,
                    };

                    self.values.insert(Arc::from(decl.name), Value::Local(ty, id, mutability, initialized));
                }

                Statement::Assignment(to, value) => {
                    let target = compile_expression(self, to, builder)?;
                    let value = compile_expression(self, value, builder)?;
                    value.load(builder)?;

                    match &target {
                        Value::Local(ty, id, mutability, ..) => {
                            match mutability {
                                Mutability::Mutable => match Some(ty) == value.ty() {
                                    true => builder.push_opcode(Opcode::StoreLocal(*id)),
                                    false => return Err(invalid_type_err(&ty, value.ty())),
                                },
                                Mutability::Immutable => {
                                    let name = self.values.iter().find_map(|(name, value)| {
                                        match value {
                                            Value::Local(_, l_id, ..) if l_id == id => Some(name),
                                            _ => None,
                                        }
                                    }).unwrap();
                                    return Err(anyhow!("Local '{}' is not mutable", name))
                                },
                            };
                        }
                        _ => unimplemented!("{:?} = {:?}", target, value),
                    }
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

pub fn invalid_type_err(expected: &Arc<Type>, got: Option<&Arc<Type>>) -> anyhow::Error {
    match got {
        None => anyhow!("Expected type '{}', got '?'", expected),
        Some(ty) => anyhow!("Expected type '{}', got '{}'", expected, ty),
    }
}
