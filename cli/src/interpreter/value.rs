use leaf_compilation::reflection::structured::Type;
use std::mem::{MaybeUninit, size_of};
use std::alloc::Layout;
use std::sync::Arc;
use anyhow::anyhow;
use half::f16;

#[derive(Debug)]
pub struct Value {
    data: Data,
    ty: Arc<Type>,
}

#[derive(Debug)]
enum Data {
    Uninit,
    Large(*mut u8, Layout),
    Small(MaybeUninit<u64>, Layout),
}

impl Value {
    pub fn new<T>(ty: Arc<Type>, value: T) -> Self {
        let layout = Layout::new::<T>();
        let data = match layout.size() < size_of::<u64>() {
            true => Data::Small(
                unsafe {
                    let mut data = 0u64;
                    std::ptr::write(&mut data as *mut u64 as *mut T, value);
                    MaybeUninit::new(data)
                },
                layout,
            ),
            false => Data::Large(
                unsafe {
                    let mut data = std::alloc::alloc(layout);
                    std::ptr::write(data as *mut T, value);
                    data
                },
                layout,
            )
        };
        Self { ty, data }
    }

    pub fn new_uninit(ty: Arc<Type>) -> Self {
        Self { ty, data: Data::Uninit }
    }

    pub fn ty(&self) -> &Arc<Type> {
        &self.ty
    }
}

impl Drop for Data {
    // FIXME: Add support for destructors
    fn drop(&mut self) {
        match self {
            Data::Uninit => {},
            Data::Small(_, _) => {}
            Data::Large(ptr, layout) => unsafe {
                std::alloc::dealloc(*ptr, *layout);
            }
        }
    }
}

macro_rules! impl_try_into_dec {
    ($($ty: ident),+) => {
        $(
            impl TryInto<$ty> for Value {
                type Error = anyhow::Error;
                fn try_into(self) -> Result<$ty, Self::Error> {
                    match self.ty.as_ref().as_ref() {
                        _ => Err(make_type_cast_error(Some(&self.ty), Type::$ty())),
                    }
                }
            }
        )*
    };
}

macro_rules! impl_try_into_int {
    ($($ty: ident),+) => {
        $(
            impl TryInto<$ty> for Value {
                type Error = anyhow::Error;
                fn try_into(self) -> Result<$ty, Self::Error> {
                    match self.ty.as_ref().as_ref() {
                        _ => Err(make_type_cast_error(Some(&self.ty), Type::$ty())),
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

impl_try_into_dec!(f16, f32, f64);
impl_try_into_dec!(i8, i16, i32, i64, u8, u16, u32, u64);
impl_into_value!(i8, i16, i32, i64, u8, u16, u32, u64, f16, f32, f64);

fn make_type_cast_error(ty: Option<&Arc<Type>>, expected: &Arc<Type>) -> anyhow::Error {
    match ty {
        None => anyhow!("Value cannot be cast to type '{}'", expected),
        Some(ty) => anyhow!("Value of type '{}' cannot be cast to type '{}'", ty, expected),
    }
}