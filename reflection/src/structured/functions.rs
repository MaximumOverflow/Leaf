use crate::{MetadataRead, MetadataWrite, Opcode};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, OnceLock};
use crate::structured::Type;
use std::io::Cursor;
use crate::Encoded;
use paste::paste;

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
                    dbg.field(&name, &format_args!("{:02X?}", opcode));
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
                block: BlockIndex(0),
                func: func.clone(),
                locals: Default::default(),
                blocks: vec![vec![]],
            },
        };

        self.data.functions.push(func.clone());
        (func, builder)
    }
}

pub type FunctionBodyBuilder = FunctionBuilder<builder_data::Body>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BlockIndex(pub usize);

macro_rules! compress_opcode {
    ($expr: expr, $op: ident) => {
        paste! {
            match $expr {
                0 => Opcode::[<$op 0>],
                1 => Opcode::[<$op 1>],
                2 => Opcode::[<$op 2>],
                3 => Opcode::[<$op 3>],
                4 => Opcode::[<$op 4>],
                5 => Opcode::[<$op 5>],
                6 => Opcode::[<$op 6>],
                _ => Opcode::$op(Encoded($expr)),
            }
        }
    };
}

impl FunctionBodyBuilder {
    pub fn declare_local(&mut self, ty: &Arc<Type>) -> &Local {
        let id = self.data.locals.len();
        self.data.locals.push(Local { id, ty: ty.clone() });
        &self.data.locals[id]
    }

    pub fn define(self) -> Arc<Function> {
        let mut opcodes = unsafe {
            let size = self.data.blocks.iter().map(|i| i.len()).sum();
            let mut opcodes = Vec::with_capacity(size);
            opcodes.set_len(size);
            opcodes
        };

        // Merge all blocks and calculate their offset
        let mut offset = 0;
        let mut block_offsets = vec![0; self.data.blocks.len()];
        for (i, block) in self.data.blocks.iter().enumerate() {
            block_offsets[i] = offset;
            opcodes[offset..offset+block.len()].copy_from_slice(block);
            offset += block.len();
        }

        // Back-patch jumps
        offset = 0;
        while offset < opcodes.len() {
            let mut cursor = Cursor::new(&opcodes);
            cursor.set_position(offset as u64);

            let opcode = Opcode::read(&mut cursor).unwrap();
            let position = cursor.position() as usize;

            match opcode {
                | Opcode::Jump(b0)
                | Opcode::CondJump(b0)
                | Opcode::CondJumpN(b0) => {
                    let b0 = block_offsets[b0 as usize] as u32;
                    opcodes[offset+1..offset+5].copy_from_slice(bytemuck::bytes_of(&b0));
                }

                _ => {}
            }

            offset = position;
        }

        self.data.func.body.set(Body {
            opcodes,
            locals: self.data.locals,
        }).unwrap();

        self.data.func
    }

    pub fn add_block(&mut self) -> BlockIndex {
        let idx = BlockIndex(self.data.blocks.len());
        self.data.blocks.push(vec![]);
        idx
    }

    pub fn use_block(&mut self, block: BlockIndex) -> Result<BlockIndex, ()> {
        match block.0 < self.data.blocks.len() {
            false => Err(()),
            true => Ok(std::mem::replace(&mut self.data.block, block)),
        }
    }

    pub fn push_local(&mut self, local: usize) -> Result<(), &'static str> {
        match local >= self.data.locals.len() {
            true => Err("Local id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(local, PushLocal)))
        }
    }

    pub fn push_local_address(&mut self, local: usize) -> Result<(), &'static str> {
        match local >= self.data.locals.len() {
            true => Err("Local id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(local, PushLocalA)))
        }
    }

    pub fn store_local(&mut self, local: usize) -> Result<(), &'static str> {
        match local >= self.data.locals.len() {
            true => Err("Local id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(local, StoreLocal)))
        }
    }

    pub fn push_param(&mut self, param: usize) -> Result<(), &'static str> {
        match param >= self.data.locals.len() {
            true => Err("Parameter id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(param, PushParam)))
        }
    }

    pub fn push_param_address(&mut self, param: usize) -> Result<(), &'static str> {
        match param >= self.data.locals.len() {
            true => Err("Parameter id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(param, PushParamA)))
        }
    }

    pub fn store_param(&mut self, param: usize) -> Result<(), &'static str> {
        match param >= self.data.locals.len() {
            true => Err("Parameter id out of bounds"),
            false => Ok(self.push_opcode(compress_opcode!(param, StoreParam)))
        }
    }

    pub fn store_field(&mut self, field: usize) {
        self.push_opcode(compress_opcode!(field, StoreField))
    }

    pub fn jump(&mut self, block: BlockIndex) -> Result<(), &'static str> {
        match block.0 >= self.data.blocks.len() {
            true => Err("Block id out of bounds"),
            false => Ok(self.push_opcode(Opcode::Jump(block.0 as u32)))
        }
    }

    pub fn cond_jump(&mut self, block: BlockIndex) -> Result<(), &'static str> {
        match block.0 >= self.data.blocks.len() {
            true => Err("Block id out of bounds"),
            false => Ok(self.push_opcode(Opcode::CondJump(block.0 as u32)))
        }
    }

    pub fn cond_jump_n(&mut self, block: BlockIndex) -> Result<(), &'static str> {
        match block.0 >= self.data.blocks.len() {
            true => Err("Block id out of bounds"),
            false => Ok(self.push_opcode(Opcode::CondJumpN(block.0 as u32)))
        }
    }

    pub fn push_opcode(&mut self, opcode: Opcode) {
        let block = &mut  self.data.blocks[self.data.block.0];
        let ir = block.len();
        let mut cursor = Cursor::new(block);
        cursor.set_position(ir as u64);
        opcode.write(&mut cursor).unwrap();
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
    use crate::structured::functions::{BlockIndex, Function, Local, Parameter};

    pub struct Signature<'l> {
        pub(super) name: Arc<str>,
        pub(super) namespace: Arc<str>,
        pub(super) return_ty: Arc<Type>,
        pub(super) parameters: HashMap<Arc<str>, Parameter>,
        pub(super) functions: &'l mut Vec<Arc<Function>>,
    }

    pub struct Body {
        pub(super) block: BlockIndex,
        pub(super) func: Arc<Function>,
        pub(super) locals: Vec<Local>,
        pub(super) blocks: Vec<Vec<u8>>,
    }
}
