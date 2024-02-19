use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::Type;
use std::mem::{MaybeUninit, size_of};
use std::fmt::{Debug, Formatter};
use std::any::{Any, TypeId};
use std::alloc::Layout;
use std::ptr::NonNull;
use std::sync::Arc;
use anyhow::anyhow;
use half::f16;

#[derive(Clone)]
pub struct Value {
    data: Data,
    ty: Arc<Type>,
}

#[derive(Debug, Clone)]
enum Data {
    Uninit,
    Large(Arc<dyn Any>),
    Small(MaybeUninit<u128>, usize, TypeId),
}

impl Value {
    pub fn new<T: 'static>(ty: Arc<Type>, value: T) -> Self {
        let layout = Layout::new::<T>();

        let dummy = NonNull::<u128>::dangling();
        let offset = dummy.as_ptr().align_offset(layout.align());
        let total_size = layout.size() + offset;

        let data = match !std::mem::needs_drop::<T>() && total_size <= size_of::<u128>() {
            true => Data::Small(
                unsafe {
                    let mut data = 0u128;
                    let ptr = (&mut data as *mut u128 as *mut u8).add(offset);
                    std::ptr::write(ptr as *mut T, value);
                    MaybeUninit::new(data)
                },
                offset,
                TypeId::of::<T>(),
            ),
            false => Data::Large(Arc::new(value))
        };
        Self { ty, data }
    }

    pub fn as_ref<T: 'static>(&self) -> Option<&T> {
        match &self.data {
            Data::Uninit => None,
            Data::Large(arc) => arc.downcast_ref(),
            Data::Small(data, offset, type_id) => unsafe {
                if TypeId::of::<T>() != *type_id {
                    return None;
                }

                let ptr = data.as_ptr().add(*offset) as *const T;
                Some(&*ptr)
            }
        }
    }

    pub unsafe fn as_ref_unchecked<T: 'static>(&self) -> Option<&T> {
        match &self.data {
            Data::Uninit => None,
            Data::Large(arc) => arc.downcast_ref(),
            Data::Small(data, offset, ..) => unsafe {
                let ptr = data.as_ptr().add(*offset) as *const T;
                Some(&*ptr)
            }
        }
    }

    pub fn new_uninit(ty: Arc<Type>) -> Self {
        Self { ty, data: Data::Uninit }
    }

    pub fn ty(&self) -> &Arc<Type> {
        &self.ty
    }
}

macro_rules! impl_try_into {
    ($($ty: ident),+) => {
        $(
            impl TryInto<$ty> for Value {
                type Error = anyhow::Error;
                fn try_into(self) -> Result<$ty, Self::Error> {
                    match &self.data {
                        Data::Uninit => Err(anyhow!("Value is not initialized")),
                        _ => match self.as_ref::<$ty>() {
                            Some(value) => Ok(*value),
                            None => Err(make_read_error(Some(&self.ty), Type::$ty())),
                        }
                    }
                }
            }
        )*
    };
}

macro_rules! impl_into_value {
    ($($ty: ident),+) => {
        $(
            impl From<$ty> for Value {
                fn from(value: $ty) -> Self {
                    Value::new(Type::$ty().clone(), value)
                }
            }
        )*
    };
}

impl_into_value!(i8, i16, i32, i64, u8, u16, u32, u64, f16, f32, f64);
impl_try_into!(char, i8, i16, i32, i64, u8, u16, u32, u64, f16, f32, f64);

impl From<()> for Value {
    fn from(value: ()) -> Self {
        Value::new(Type::void().clone(), value)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Value");
        dbg.field("type", &format_args!("{}", self.ty));
        match self.ty.as_ref().as_ref() {
            TypeVariant::Void => dbg.field("value", &()),
            TypeVariant::Char => dbg.field("value", self.as_ref::<char>().unwrap()),
            TypeVariant::Dec(size) => match *size {
                2 => dbg.field("value", self.as_ref::<f16>().unwrap()),
                4 => dbg.field("value", self.as_ref::<f32>().unwrap()),
                8 => dbg.field("value", self.as_ref::<f64>().unwrap()),
                _ => unreachable!(),
            }
            TypeVariant::Int(size) => match *size {
                1 => dbg.field("value", self.as_ref::<i8>().unwrap()),
                2 => dbg.field("value", self.as_ref::<i16>().unwrap()),
                4 => dbg.field("value", self.as_ref::<i32>().unwrap()),
                8 => dbg.field("value", self.as_ref::<i64>().unwrap()),
                _ => unreachable!(),
            }
            TypeVariant::UInt(size) => match *size {
                1 => dbg.field("value", self.as_ref::<u8>().unwrap()),
                2 => dbg.field("value", self.as_ref::<u16>().unwrap()),
                4 => dbg.field("value", self.as_ref::<u32>().unwrap()),
                8 => dbg.field("value", self.as_ref::<u64>().unwrap()),
                _ => unreachable!(),
            }
            TypeVariant::Pointer(..) | TypeVariant::Reference(..) => {
                match self.data {
                    Data::Small(..) => unsafe {
                        dbg.field("value", self.as_ref_unchecked::<*const u8>().unwrap())
                    }
                    _ => {
                        dbg.field("value", &"pointer")
                    }
                }
            }
            TypeVariant::Struct(_) => dbg.field("value", &self.data),
        };
        dbg.finish()
    }
}

fn make_read_error(ty: Option<&Arc<Type>>, expected: &Arc<Type>) -> anyhow::Error {
    match ty {
        None => anyhow!("Value cannot be cast to type '{}'", expected),
        Some(ty) => anyhow!("Value of type '{}' cannot be cast to type '{}'", ty, expected),
    }
}