use crate::structured::functions::{Function, FunctionBuilder};
use crate::structured::functions::builder_data::Signature;
use crate::structured::types::{StructBuilder, Type};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, OnceLock};
use crate::{ElementRef, Encoded};

pub struct Assembly {
    types: OnceLock<Vec<Arc<Type>>>,
    functions: OnceLock<Vec<Arc<Function>>>,
}

impl Assembly {
    pub fn types(&self) -> &[Arc<Type>] {
        match self.types.get() {
            Some(types) => &types,
            None => panic!("Assembly has not been finalized"),
        }
    }

    pub fn functions(&self) -> &[Arc<Function>] {
        match self.functions.get() {
            Some(types) => &types,
            None => panic!("Assembly has not been finalized"),
        }
    }
}

impl Debug for Assembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Assembly");
        let types = match self.types.get() {
            None => return dbg.finish_non_exhaustive(),
            Some(types) => types.as_slice(),
        };

        let functions = match self.functions.get() {
            None => return dbg.finish_non_exhaustive(),
            Some(functions) => functions.as_slice(),
        };

        dbg.field("types", &types);
        dbg.field("functions", &functions);
        dbg.finish()
    }
}

pub struct AssemblyBuilder {
    assembly: Arc<Assembly>,
    types: Vec<Arc<Type>>,
    functions: Vec<Arc<Function>>,
}

impl AssemblyBuilder {
    pub fn new() -> Self {
        Self {
            types: vec![],
            functions: vec![],
            assembly: Arc::new(Assembly {
                types: Default::default(),
                functions: Default::default(),
            }),
        }
    }

    pub fn define_struct(&mut self, name: &str, namespace: &Arc<str>) -> StructBuilder {
        let builder = StructBuilder::new(name, namespace.clone(), ElementRef {
            offset: Encoded(self.types.len() as u64 + 1),
            ph: Default::default(),
        });
        self.types.push(builder.as_ref().clone());
        builder
    }

    pub fn define_function(&mut self, name: &str, namespace: &Arc<str>) -> FunctionBuilder<Signature> {
        FunctionBuilder::new(name, namespace.clone(), &mut self.functions)
    }

    pub fn build(self) -> Arc<Assembly> {
        self.assembly.types.set(self.types).unwrap();
        self.assembly.functions.set(self.functions).unwrap();
        self.assembly
    }
}
