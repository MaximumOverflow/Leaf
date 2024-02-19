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

impl Body {
    pub fn opcodes(&self) -> &[u8] {
        &self.opcodes
    }

    pub fn locals(&self) -> &[Local] {
        &self.locals
    }
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
}

impl Local {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn ty(&self) -> &Arc<Type> {
        &self.ty
    }
}

impl Debug for Local {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Local({}, {})", self.id, self.ty)
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
    pub fn declare_local(&mut self, ty: &Arc<Type>) -> &Local {
        let id = self.data.locals.len();
        self.data.locals.push(Local { id, ty: ty.clone() });
        &self.data.locals[id]
    }

    pub fn define(self) -> Arc<Function> {
        self.data.func.body.set(Body {
            locals: self.data.locals,
            opcodes: self.data.opcodes,
        }).unwrap();

        self.data.func
    }

    pub fn push_local(&mut self, local: usize) -> Option<usize> {
        if local >= self.data.locals.len() {
            return None;
        }

        let opcode = match local {
            0 => Opcode::PushLocal0,
            1 => Opcode::PushLocal1,
            2 => Opcode::PushLocal2,
            3 => Opcode::PushLocal3,
            4 => Opcode::PushLocal4,
            5 => Opcode::PushLocal5,
            6 => Opcode::PushLocal6,
            _ => Opcode::PushLocal(local),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn push_local_address(&mut self, local: usize) -> Option<usize> {
        if local >= self.data.locals.len() {
            return None;
        }

        let opcode = match local {
            0 => Opcode::PushLocalA0,
            1 => Opcode::PushLocalA1,
            2 => Opcode::PushLocalA2,
            3 => Opcode::PushLocalA3,
            4 => Opcode::PushLocalA4,
            5 => Opcode::PushLocalA5,
            6 => Opcode::PushLocalA6,
            _ => Opcode::PushLocalA(local),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn store_local(&mut self, local: usize) -> Option<usize> {
        if local >= self.data.locals.len() {
            return None;
        }

        let opcode = match local {
            0 => Opcode::StoreLocal0,
            1 => Opcode::StoreLocal1,
            2 => Opcode::StoreLocal2,
            3 => Opcode::StoreLocal3,
            4 => Opcode::StoreLocal4,
            5 => Opcode::StoreLocal5,
            6 => Opcode::StoreLocal6,
            _ => Opcode::StoreLocal(local),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn push_param(&mut self, param: usize) -> Option<usize> {
        if param >= self.data.func.parameters.len() {
            return None;
        }

        let opcode = match param {
            0 => Opcode::PushParam0,
            1 => Opcode::PushParam1,
            2 => Opcode::PushParam2,
            3 => Opcode::PushParam3,
            4 => Opcode::PushParam4,
            5 => Opcode::PushParam5,
            6 => Opcode::PushParam6,
            _ => Opcode::PushParam(param),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn push_param_address(&mut self, param: usize) -> Option<usize> {
        if param >= self.data.func.parameters.len() {
            return None;
        }

        let opcode = match param {
            0 => Opcode::PushParamA0,
            1 => Opcode::PushParamA1,
            2 => Opcode::PushParamA2,
            3 => Opcode::PushParamA3,
            4 => Opcode::PushParamA4,
            5 => Opcode::PushParamA5,
            6 => Opcode::PushParamA6,
            _ => Opcode::PushParamA(param),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn store_param(&mut self, param: usize) -> Option<usize> {
        if param >= self.data.func.parameters.len() {
            return None;
        }

        let opcode = match param {
            0 => Opcode::StoreParam0,
            1 => Opcode::StoreParam1,
            2 => Opcode::StoreParam2,
            3 => Opcode::StoreParam3,
            4 => Opcode::StoreParam4,
            5 => Opcode::StoreParam5,
            6 => Opcode::StoreParam6,
            _ => Opcode::StoreParam(param),
        };

        Some(self.push_opcode(opcode))
    }

    pub fn store_field(&mut self, field: usize) -> usize {
        let opcode = match field {
            0 => Opcode::StoreField0,
            1 => Opcode::StoreField1,
            2 => Opcode::StoreField2,
            3 => Opcode::StoreField3,
            4 => Opcode::StoreField4,
            5 => Opcode::StoreField5,
            6 => Opcode::StoreField6,
            _ => Opcode::StoreField(field),
        };

        self.push_opcode(opcode)
    }

    pub fn push_opcode(&mut self, opcode: Opcode) -> usize {
        let ir = self.data.opcodes.len();

        let mut cursor = Cursor::new(&mut self.data.opcodes);
        cursor.set_position(ir as u64);

        opcode.write(&mut cursor).unwrap();
        ir
    }

    pub fn ir_offset(&self) -> usize {
        self.data.opcodes.len()
    }
}

impl AsRef<Arc<Function>> for FunctionBodyBuilder {
    fn as_ref(&self) -> &Arc<Function> {
        &self.data.func
    }
}

pub mod builder_data {
    use std::sync::Arc;
    use crate::structured::Type;
    use std::collections::HashMap;
    use crate::structured::functions::{Function, Local, Parameter};

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
        pub(super) locals: Vec<Local>,
    }
}
