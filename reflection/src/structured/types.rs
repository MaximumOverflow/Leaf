use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::sync::{Arc, OnceLock, Weak};
use std::hash::{Hash, Hasher};

pub struct Type {
    variant: TypeVariant,
    ptr_ty: OnceLock<Arc<Type>>,
    slice_ty: OnceLock<Arc<Type>>,
}

#[derive(Default)]
pub enum TypeVariant {
    #[default]
    Void,
    Char,
    Dec(u32),
    Int(u32),
    UInt(u32),
    Struct(StructType),
}

impl Type {
    pub fn is_or_contains(&self, ty: &Arc<Type>) -> bool {
        if self == &**ty {
            return true;
        }

        match &self.variant {
            TypeVariant::Struct(data) => {
                for field in data.fields() {
                    let field_ty = field.ty.upgrade().unwrap();
                    if field_ty.is_or_contains(ty) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub fn void() -> Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Void)).clone()
    }

    pub fn i8() -> Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int(1))).clone()
    }

    pub fn i16() -> Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int(2))).clone()
    }

    pub fn i32() -> Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int(4))).clone()
    }

    pub fn i64() -> Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int(8))).clone()
    }

    pub fn name(&self) -> &str {
        match &self.variant {
            TypeVariant::Void => "void",
            TypeVariant::Char => "char",
            TypeVariant::Dec(size) => match *size {
                2 => "f16",
                4 => "f32",
                8 => "f64",
                _ => unreachable!(),
            }
            TypeVariant::Int(size) => match *size {
                1 => "i8",
                2 => "i16",
                4 => "i32",
                8 => "i64",
                _ => unreachable!(),
            }
            TypeVariant::UInt(size) => match *size {
                1 => "u8",
                2 => "u16",
                4 => "u32",
                8 => "u64",
                _ => unreachable!(),
            }
            TypeVariant::Struct(data) => &data.name,
        }
    }

    pub fn namespace(&self) -> &str {
        match &self.variant {
            TypeVariant::Struct(data) => &data.namespace,
            _ => "",
        }
    }

    fn new(variant: TypeVariant) -> Arc<Type> {
        Arc::new(
            Self {
                variant,
                ptr_ty: Default::default(),
                slice_ty: Default::default(),
            }
        )
    }
}

impl AsRef<TypeVariant> for Type {
    fn as_ref(&self) -> &TypeVariant {
        &self.variant
    }
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const Type as usize)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.variant {
            TypeVariant::Struct(data) => {
                write! {
                    f, "{}::{}",
                    data.namespace,
                    match &*data.name {
                        "" => "<anonymous>",
                        name => name,
                    }
                }
            }
            _ => f.write_str(self.name()),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.variant {
            TypeVariant::Struct(data) => data.fmt(f),
            _ => Display::fmt(self, f)
        }
    }
}

pub struct StructType {
    name: Arc<str>,
    namespace: Arc<str>,
    fields: OnceLock<Vec<Field>>,
}

impl StructType {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn namespace(&self) -> &str {
        &self.namespace
    }

    pub fn fields(&self) -> &[Field] {
        match self.fields.get() {
            Some(fields) => &fields,
            None => panic!("Type has not been finalized"),
        }
    }
}

impl Debug for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = format! {
            "struct {}::{}",
            self.namespace,
            match &*self.name {
                "" => "<anonymous>",
                name => name,
            }
        };

        let mut dbg = f.debug_struct(&name);
        let Some(fields) = self.fields.get() else {
            return dbg.finish_non_exhaustive();
        };

        for field in fields {
            dbg.field(&field.name, &format_args!("{}", field.ty()));
        }

        dbg.finish()
    }
}

#[derive(Debug)]
pub struct Field {
    ty: Weak<Type>,
    name: Arc<str>,
}

impl Field {
    pub fn name(&self) -> &Arc<str> {
        &self.name
    }

    pub fn ty(&self) -> Arc<Type> {
        self.ty.upgrade().expect("The field's type has been dropped and is no longer available")
    }
}

pub struct StructBuilder {
    ty: Arc<Type>,
    fields: HashMap<Arc<str>, (Field, usize)>,
}

impl StructBuilder {
    pub(crate) fn new(name: &str, namespace: Arc<str>) -> StructBuilder {
        Self {
            ty: Type::new(
                TypeVariant::Struct(
                    StructType {
                        namespace,
                        name: Arc::from(name),
                        fields: Default::default(),
                    }
                )
            ),
            fields: HashMap::new(),
        }
    }

    pub fn define_field(&mut self, name: &str, ty: &Arc<Type>) -> Result<(), &Field> {
        if self.fields.contains_key(name) {
            return Err(&self.fields[name].0);
        }

        let idx = self.fields.len();
        let field = Field {
            name: Arc::from(name),
            ty: Arc::downgrade(&ty),
        };

        self.fields.insert(field.name.clone(), (field, idx));
        Ok(())
    }

    pub fn build(self) -> Arc<Type> {
        let TypeVariant::Struct(ty) = &self.ty.variant else {
            unreachable!()
        };

        let mut fields: Vec<(Field, usize)> = self.fields.into_values().collect();
        fields.sort_by_key(|i| i.1);

        let fields = fields.into_iter().map(|(f, _)| f).collect();
        ty.fields.set(fields).unwrap();
        self.ty
    }
}

impl AsRef<Arc<Type>> for StructBuilder {
    fn as_ref(&self) -> &Arc<Type> {
        &self.ty
    }
}
