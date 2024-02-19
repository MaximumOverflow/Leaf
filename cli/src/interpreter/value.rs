use leaf_compilation::reflection::structured::types::TypeVariant;
use leaf_compilation::reflection::structured::Type;
use std::mem::{MaybeUninit, size_of};
use std::fmt::{Debug, Formatter};
use std::alloc::Layout;
use std::sync::Arc;
use anyhow::anyhow;
use half::f16;

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

    pub unsafe fn read_copy<T>(&self) -> anyhow::Result<T> {
        let to_layout = Layout::new::<T>();
        match self.data {
            Data::Uninit => Err(anyhow!("Value is uninitialized")),
            Data::Large(ptr, layout) => match layout == to_layout {
                false => Err(anyhow!("Invalid layout")),
                true => {
                    let value = std::ptr::read_unaligned(ptr as *mut T);
                    Ok(value)
                },
            }
            Data::Small(data, layout) => match layout == to_layout {
                false => Err(anyhow!("Invalid layout")),
                true => {
                    let value = std::ptr::read_unaligned(data.as_ptr() as *mut T);
                    Ok(value)
                },
            }
        }
    }

    pub unsafe fn read<T>(mut self) -> anyhow::Result<T> {
        let to_layout = Layout::new::<T>();
        match self.data {
            Data::Uninit => Err(anyhow!("Value is uninitialized")),
            Data::Large(ptr, layout) => match layout == to_layout {
                false => Err(anyhow!("Invalid layout")),
                true => {
                    let value = std::ptr::read_unaligned(ptr as *mut T);
                    std::alloc::dealloc(ptr, layout);
                    self.data = Data::Uninit;
                    Ok(value)
                },
            }
            Data::Small(data, layout) => match layout == to_layout {
                false => Err(anyhow!("Invalid layout")),
                true => {
                    let value = std::ptr::read_unaligned(data.as_ptr() as *mut T);
                    self.data = Data::Uninit;
                    Ok(value)
                },
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

macro_rules! impl_try_into {
    ($($ty: ident),+) => {
        $(
            impl TryInto<$ty> for Value {
                type Error = anyhow::Error;
                fn try_into(self) -> Result<$ty, Self::Error> {
                    match &self.ty == Type::$ty() {
                        true => unsafe { self.read() },
                        false => Err(make_read_error(Some(&self.ty), Type::$ty())),
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
        unsafe {
            match self.ty.as_ref().as_ref() {
                TypeVariant::Void => dbg.field("value", &()),
                TypeVariant::Char => dbg.field("value", &self.read_copy::<char>().unwrap()),
                TypeVariant::Dec(size) => match *size {
                    2 => dbg.field("value", &self.read_copy::<f16>().unwrap()),
                    4 => dbg.field("value", &self.read_copy::<f32>().unwrap()),
                    8 => dbg.field("value", &self.read_copy::<f64>().unwrap()),
                    _ => unreachable!(),
                }
                TypeVariant::Int(size) => match *size {
                    1 => dbg.field("value", &self.read_copy::<i8>().unwrap()),
                    2 => dbg.field("value", &self.read_copy::<i16>().unwrap()),
                    4 => dbg.field("value", &self.read_copy::<i32>().unwrap()),
                    8 => dbg.field("value", &self.read_copy::<i64>().unwrap()),
                    _ => unreachable!(),
                }
                TypeVariant::UInt(size) => match *size {
                    1 => dbg.field("value", &self.read_copy::<u8>().unwrap()),
                    2 => dbg.field("value", &self.read_copy::<u16>().unwrap()),
                    4 => dbg.field("value", &self.read_copy::<u32>().unwrap()),
                    8 => dbg.field("value", &self.read_copy::<u64>().unwrap()),
                    _ => unreachable!(),
                }
                TypeVariant::Struct(_) => dbg.field("value", &self.data),
            };
        }
        dbg.finish()
    }
}

fn make_read_error(ty: Option<&Arc<Type>>, expected: &Arc<Type>) -> anyhow::Error {
    match ty {
        None => anyhow!("Value cannot be cast to type '{}'", expected),
        Some(ty) => anyhow!("Value of type '{}' cannot be cast to type '{}'", ty, expected),
    }
}