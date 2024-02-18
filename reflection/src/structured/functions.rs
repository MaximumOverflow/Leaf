use crate::{MetadataRead, MetadataWrite, Opcode};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, OnceLock};
use crate::structured::Type;
use std::io::Cursor;

pub struct Function {
    name: Arc<str>,
    namespace: Arc<str>,
    return_ty: Arc<Type>,
    parameters: Vec<Parameter>,
    body: OnceLock<Body>,
}

impl Function {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn name_arc(&self) -> &Arc<str> {
        &self.name
    }

    pub fn namespace(&self) -> &str {
        &self.namespace
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn return_ty(&self) -> &Arc<Type> {
        &self.return_ty
    }

    pub fn body(&self) -> Option<&Body> {
        self.body.get()
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        let mut name = format! {
            "function {}::{}",
            self.namespace,
            match &*self.name {
                "" => "<anonymous>",
                name => name,
            }
        };

        {
            write!(name,  "(")?;
            let mut comma = "";
            for param in &self.parameters {
                write!(name, "{}{}: {}", comma, param.name, param.ty)?;
                comma = ", ";
            }
            write!(name,  ") -> {}", self.return_ty)?;
        }

        let Some(body) = self.body.get() else {
            return f.write_str(&name);
        };

        let mut dbg = f.debug_struct(&name);
        dbg.field("locals", &body.locals);

        let mut name = String::new();
        let mut cursor = Cursor::new(&body.opcodes);

        while cursor.position() < body.opcodes.len() as u64 {
            name.clear();
            write!(name, "IR_{:#06X}", cursor.position())?;
            match Opcode::read(&mut cursor) {
                Ok(opcode) => {
                    dbg.field(&name, &format_args!("{:?}", opcode));
                },
                Err(err) => {
                    dbg.field(&name, &err);
                    return dbg.finish_non_exhaustive();
                }
            }
        }

        dbg.finish()
    }
}

pub struct Parameter {
    id: usize,
    name: Arc<str>,
    ty: Arc<Type>,
}

impl Parameter {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn name_arc(&self) -> &Arc<str> {
        &self.name
    }

    pub fn ty(&self) -> &Arc<Type> {
        &self.ty
    }
}

impl Debug for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Parameter");
        dbg.field("id", &self.id);
        dbg.field("name", &self.name);
        dbg.field("type", &format_args!("{}", self.ty));
        dbg.finish()
    }
}

pub struct Body {
    opcodes: Vec<u8>,
    locals: Vec<Local>,
}

impl Debug for Body {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Body");
        dbg.finish_non_exhaustive()
    }
}

pub struct Local {
    id: usize,
    ty: Arc<Type>,
    name: Arc<str>,
}

impl Debug for Local {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Local");
        dbg.field("id", &self.id);
        dbg.field("name", &self.name);
        dbg.field("type", &format_args!("{}", self.ty));
        dbg.finish()
    }
}

pub struct FunctionBuilder<T> {
    data: T,
}

pub type FunctionSignatureBuilder<'l> = FunctionBuilder<builder_data::Signature<'l>>;

impl<'l> FunctionSignatureBuilder<'l> {
    pub(crate) fn new(name: &str, namespace: Arc<str>, functions: &'l mut Vec<Arc<Function>>) -> FunctionSignatureBuilder<'l> {
        Self {
            data: builder_data::Signature {
                namespace,
                name: Arc::from(name),
                return_ty: Type::void().clone(),
                parameters: Default::default(),
                functions,
            },
        }
    }

    pub fn set_return_type(&mut self, ty: &Arc<Type>) {
        self.data.return_ty = ty.clone();
    }

    pub fn define_parameter(&mut self, name: &str, ty: &Arc<Type>) -> Result<usize, &Parameter> {
        if self.data.parameters.contains_key(name) {
            return Err(&self.data.parameters[name]);
        }

        let id = self.data.parameters.len();
        let parameter = Parameter {
            id,
            ty: ty.clone(),
            name: Arc::from(name),
        };

        self.data.parameters.insert(parameter.name.clone(), parameter);
        Ok(id)
    }

    pub fn declare(self) -> (Arc<Function>, FunctionBodyBuilder) {
        let mut parameters: Vec<Parameter> = self.data.parameters.into_values().collect();
        parameters.sort_by_key(|i| i.id);

        let func = Arc::new(Function {
            parameters,
            name: self.data.name,
            namespace: self.data.namespace,
            return_ty: self.data.return_ty,
            body: Default::default(),
        });

        let builder = FunctionBodyBuilder {
            data: builder_data::Body {
                func: func.clone(),
                opcodes: vec![],
                locals: Default::default(),
            },
        };

        self.data.functions.push(func.clone());
        (func, builder)
    }
}

pub type FunctionBodyBuilder = FunctionBuilder<builder_data::Body>;

impl FunctionBodyBuilder {
    pub fn declare_local(&mut self, name: &str, ty: &Arc<Type>) -> Result<usize, &Local> {
        if self.data.locals.contains_key(name) {
            return Err(&self.data.locals[name]);
        }

        let id = self.data.locals.len();
        let parameter = Local {
            id,
            ty: ty.clone(),
            name: Arc::from(name),
        };

        self.data.locals.insert(parameter.name.clone(), parameter);
        Ok(id)
    }

    pub fn define(self) -> Arc<Function> {
        let mut locals: Vec<_> = self.data.locals.into_values().collect();
        locals.sort_by_key(|i| i.id);

        self.data.func.body.set(Body {
            locals,
            opcodes: self.data.opcodes,
        }).unwrap();

        self.data.func
    }

    pub fn ret(&mut self) -> usize {
        self.push_opcode(Opcode::Ret)
    }

    pub fn add(&mut self) -> usize {
        self.push_opcode(Opcode::Add)
    }

    pub fn sub(&mut self) -> usize {
        self.push_opcode(Opcode::Sub)
    }

    pub fn mul(&mut self) -> usize {
        self.push_opcode(Opcode::Mul)
    }

    pub fn div(&mut self) -> usize {
        self.push_opcode(Opcode::Div)
    }

    pub fn rem(&mut self) -> usize {
        self.push_opcode(Opcode::Mod)
    }

    pub fn load_local(&mut self, name: &str) -> Option<usize> {
        let idx = self.data.locals.get(name)?.id;
        Some(self.push_opcode(Opcode::PushLocal(idx)))
    }

    pub fn load_parameter(&mut self, name: &str) -> Option<usize> {
        let idx = self.data.func.parameters.iter().find(|n| n.name.as_ref() == name)?.id;
        Some(self.push_opcode(Opcode::PushParam(idx)))
    }

    pub fn push_opcode(&mut self, opcode: Opcode) -> usize {
        let ir = self.data.opcodes.len();

        let mut cursor = Cursor::new(&mut self.data.opcodes);
        cursor.set_position(ir as u64);

        opcode.write(&mut cursor).unwrap();
        ir
    }
}

impl AsRef<Arc<Function>> for FunctionBodyBuilder {
    fn as_ref(&self) -> &Arc<Function> {
        &self.data.func
    }
}

pub mod builder_data {
    use std::collections::HashMap;
    use std::sync::Arc;
    use crate::Opcode;
    use crate::structured::functions::{Function, Local, Parameter};
    use crate::structured::Type;

    pub struct Signature<'l> {
        pub(super) name: Arc<str>,
        pub(super) namespace: Arc<str>,
        pub(super) return_ty: Arc<Type>,
        pub(super) parameters: HashMap<Arc<str>, Parameter>,
        pub(super) functions: &'l mut Vec<Arc<Function>>,
    }

    pub struct Body {
        pub(super) opcodes: Vec<u8>,
        pub(super) func: Arc<Function>,
        pub(super) locals: HashMap<Arc<str>, Local>,
    }
}
