use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, OnceLock, Weak};
use crate::{ElementRef, TypeDef};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use crate::structured::name_or_empty;

pub struct Type {
    this: Weak<Type>,
    variant: TypeVariant,
    ptr_ty: OnceLock<Arc<Type>>,
    mut_ptr_ty: OnceLock<Arc<Type>>,
    slice_ty: OnceLock<Arc<Type>>,
    type_def: ElementRef<TypeDef>,
}

#[derive(Default)]
pub enum TypeVariant {
    #[default]
    Void,
    Char,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float16,
    Float32,
    Float64,
    Struct(StructType),
    Pointer(Weak<Type>, bool),
    Reference(Weak<Type>, bool),
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

    pub fn void() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Void, ElementRef::default()))
    }

    pub fn char() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Char, ElementRef::default()))
    }

    pub fn bool() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Bool, ElementRef::default()))
    }

    pub fn i8() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int8, ElementRef::default()))
    }

    pub fn i16() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int16, ElementRef::default()))
    }

    pub fn i32() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int32, ElementRef::default()))
    }

    pub fn i64() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Int64, ElementRef::default()))
    }

    pub fn u8() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::UInt8, ElementRef::default()))
    }

    pub fn u16() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::UInt16, ElementRef::default()))
    }

    pub fn u32() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::UInt32, ElementRef::default()))
    }

    pub fn u64() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::UInt64, ElementRef::default()))
    }

    pub fn f16() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Float16, ElementRef::default()))
    }

    pub fn f32() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Float32, ElementRef::default()))
    }

    pub fn f64() -> &'static Arc<Type> {
        static TYPE: OnceLock<Arc<Type>> = OnceLock::new();
        TYPE.get_or_init(|| Self::new(TypeVariant::Float64, ElementRef::default()))
    }

    pub fn name(&self) -> Option<&str> {
        match &self.variant {
            TypeVariant::Void => Some("void"),
            TypeVariant::Char => Some("char"),
            TypeVariant::Bool => Some("bool"),
            TypeVariant::Int8 => Some("i8"),
            TypeVariant::Int16 => Some("i16"),
            TypeVariant::Int32 => Some("i32"),
            TypeVariant::Int64 => Some("i64"),
            TypeVariant::UInt8 => Some("u8"),
            TypeVariant::UInt16 => Some("u16"),
            TypeVariant::UInt32 => Some("u32"),
            TypeVariant::UInt64 => Some("u64"),
            TypeVariant::Float16 => Some("f16"),
            TypeVariant::Float32 => Some("f32"),
            TypeVariant::Float64 => Some("f64"),
            TypeVariant::Struct(data) => Some(&data.name),
            _ => None,
        }
    }

    pub fn name_arc(&self) -> Option<&Arc<str>> {
        match &self.variant {
            TypeVariant::Struct(data) => Some(&data.name),
            _ => None,
        }
    }

    pub fn namespace(&self) -> &str {
        match &self.variant {
            TypeVariant::Struct(data) => &data.namespace,
            _ => "",
        }
    }

    pub fn make_ptr(&self, mutable: bool) -> &Arc<Type> {
        match mutable {
            false => self.ptr_ty.get_or_init(|| {
                Type::new(TypeVariant::Pointer(self.this.clone(), false), ElementRef::default())
            }),
            true => self.mut_ptr_ty.get_or_init(|| {
                Type::new(TypeVariant::Pointer(self.this.clone(), true), ElementRef::default())
            }),
        }
    }

    fn new(variant: TypeVariant, type_def: ElementRef<TypeDef>) -> Arc<Type> {
        Arc::new_cyclic(|this| {
            Self {
                variant,
                type_def,
                this: this.clone(),
                ptr_ty: Default::default(),
                mut_ptr_ty: Default::default(),
                slice_ty: Default::default(),
            }
        })
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
            TypeVariant::Pointer(base, false) => {
                write!(f, "*{}", base.upgrade().unwrap())
            }
            TypeVariant::Pointer(base, true) => {
                write!(f, "*mut {}", base.upgrade().unwrap())
            }
            TypeVariant::Reference(base, false) => {
                write!(f, "&{}", base.upgrade().unwrap())
            }
            TypeVariant::Reference(base, true) => {
                write!(f, "&mut {}", base.upgrade().unwrap())
            }
            _ => write!(f, "{}", self.name().unwrap()),
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
    pub(crate) fn new(name: &str, namespace: Arc<str>, type_def: ElementRef<TypeDef>) -> StructBuilder {
        Self {
            ty: Type::new(
                TypeVariant::Struct(
                    StructType {
                        namespace,
                        name: name_or_empty(name),
                        fields: Default::default(),
                    }
                ),
                type_def
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
