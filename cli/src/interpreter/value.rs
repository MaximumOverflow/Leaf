use leaf_compilation::reflection::structured::types::{LeafType, TypeVariant};
use leaf_compilation::reflection::structured::Type;
use std::fmt::{Debug, Formatter};
use std::alloc::Layout;
use std::sync::Arc;
use anyhow::anyhow;
use half::f16;

pub struct Value {
    data: Data,
    ty: Arc<Type>,
}

enum Data {
    Void,
    Float(f64),
    Int(i64),
    UInt(u64),
    Char(char),
    Bool(bool),
    Boxed(*mut u8, Layout),
    Uninit(*mut u8, Layout),
}

impl Value {
    pub fn new(ty: &Arc<Type>, value: &[u8], layout: Layout) -> anyhow::Result<Self> {
        if layout.size() != value.len() {
            return Err(anyhow!("Invalid buffer size. Expected {}, found {}", layout.size(), value.len()));
        }

        let mut v = Self::new_uninit(ty, layout)?;
        unsafe { v.init(|buf| Ok(buf.copy_from_slice(value)))? };
        Ok(v)
    }

    pub fn new_uninit(ty: &Arc<Type>, layout: Layout) -> anyhow::Result<Self> {
        macro_rules! assert_known_type_validity {
            ($layout: expr) => {
                if layout != $layout {
                    return Err(anyhow!("Layout or value mismatch"));
                }
            };
        }

        match ty.variant() {
            TypeVariant::Void => {
                assert_known_type_validity!(Layout::new::<()>());
                Ok(Self { ty: ty.clone(), data: Data::Void })
            }
            TypeVariant::Char => {
                assert_known_type_validity!(Layout::new::<char>());
                Ok(Self { ty: ty.clone(), data: Data::Char('\0') })
            }
            TypeVariant::Bool => {
                assert_known_type_validity!(Layout::new::<char>());
                Ok(Self { ty: ty.clone(), data: Data::Bool(false) })
            }
            TypeVariant::Int8 => {
                assert_known_type_validity!(Layout::new::<i8>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::Int16 => {
                assert_known_type_validity!(Layout::new::<i16>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::Int32 => {
                assert_known_type_validity!(Layout::new::<i32>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::Int64 => {
                assert_known_type_validity!(Layout::new::<i64>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::UInt8 => {
                assert_known_type_validity!(Layout::new::<u8>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::UInt16 => {
                assert_known_type_validity!(Layout::new::<u16>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::UInt32 => {
                assert_known_type_validity!(Layout::new::<u32>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::UInt64 => {
                assert_known_type_validity!(Layout::new::<u64>());
                Ok(Self { ty: ty.clone(), data: Data::Int(0) })
            }
            TypeVariant::Float16 => {
                assert_known_type_validity!(Layout::new::<f16>());
                Ok(Self { ty: ty.clone(), data: Data::Float(0.0) })
            }
            TypeVariant::Float32 => {
                assert_known_type_validity!(Layout::new::<f32>());
                Ok(Self { ty: ty.clone(), data: Data::Float(0.0) })
            }
            TypeVariant::Float64 => {
                assert_known_type_validity!(Layout::new::<f64>());
                Ok(Self { ty: ty.clone(), data: Data::Float(0.0) })
            }
            _ => {
                let data = unsafe {
                    let buffer = std::alloc::alloc(layout);
                    if buffer.is_null() {
                        return Err(anyhow!("Failed to allocate buffer"));
                    }
                    buffer
                };

                Ok(Self {
                    ty: ty.clone(),
                    data: Data::Uninit(data, layout),
                })
            }
        }
    }

    pub unsafe fn init<T>(&mut self, func: impl FnOnce(&mut [u8]) -> anyhow::Result<T>) -> anyhow::Result<T> {
       match self.ty.variant() {
           TypeVariant::Void => {
               func(&mut [])
           },
           TypeVariant::Bool => {
               let Data::Bool(i) = &mut self.data else { unreachable!() };
               let mut v = 0i8;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v != 0;
               Ok(res)
           }
           TypeVariant::Char => {
               let Data::Char(ch) = &mut self.data else { unreachable!() };
               let mut v = 0u32;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *ch = char::from_u32(v).unwrap();
               Ok(res)
           }
           TypeVariant::Int8 => {
               let Data::Int(i) = &mut self.data else { unreachable!() };
               let mut v = 0i8;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as i64;
               Ok(res)
           }
           TypeVariant::Int16 => {
               let Data::Int(i) = &mut self.data else { unreachable!() };
               let mut v = 0i16;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as i64;
               Ok(res)
           }
           TypeVariant::Int32 => {
               let Data::Int(i) = &mut self.data else { unreachable!() };
               let mut v = 0i32;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as i64;
               Ok(res)
           }
           TypeVariant::Int64 => {
               let Data::Int(i) = &mut self.data else { unreachable!() };
               func(bytemuck::bytes_of_mut(i))
           }
           TypeVariant::UInt8 => {
               let Data::UInt(i) = &mut self.data else { unreachable!() };
               let mut v = 0u8;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as u64;
               Ok(res)
           }
           TypeVariant::UInt16 => {
               let Data::UInt(i) = &mut self.data else { unreachable!() };
               let mut v = 0u16;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as u64;
               Ok(res)
           }
           TypeVariant::UInt32 => {
               let Data::UInt(i) = &mut self.data else { unreachable!() };
               let mut v = 0u32;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *i = v as u64;
               Ok(res)
           }
           TypeVariant::UInt64 => {
               let Data::UInt(i) = &mut self.data else { unreachable!() };
               func(bytemuck::bytes_of_mut(i))
           }
           TypeVariant::Float16 => {
               let Data::Float(f) = &mut self.data else { unreachable!() };
               let mut v = f16::default();
               let slice = std::slice::from_raw_parts_mut(&mut v as *mut f16 as *mut u8, 2);
               let res = func(slice)?;
               *f = v.to_f64();
               Ok(res)
           }
           TypeVariant::Float32 => {
               let Data::Float(f) = &mut self.data else { unreachable!() };
               let mut v = 0f32;
               let res = func(bytemuck::bytes_of_mut(&mut v))?;
               *f = v as f64;
               Ok(res)
           }
           TypeVariant::Float64 => {
               let Data::Float(f) = &mut self.data else { unreachable!() };
               func(bytemuck::bytes_of_mut(f))
           }
           _ => {
               match &self.data {
                   Data::Uninit(ptr, layout) => {
                       let slice = std::slice::from_raw_parts_mut(*ptr, layout.size());
                       let result = func(slice)?;
                       self.data = Data::Boxed(*ptr, *layout);
                       Ok(result)
                   }
                   _ => Err(anyhow!("Value is already initialized or should contain no data")),
               }
           },
       }
    }

    pub fn void() -> Self {
        Self {
            data: Data::Void,
            ty: <()>::leaf_type().clone(),
        }
    }

    pub fn ty(&self) -> &Arc<Type> {
        &self.ty
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        match &self.data {
            Data::Boxed(ptr, layout) => unsafe { std::alloc::dealloc(*ptr, *layout) }
            Data::Uninit(ptr, layout) => unsafe { std::alloc::dealloc(*ptr, *layout) }
            _ => {},
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Value");
        dbg.field("type", &format_args!("{}", self.ty));
        dbg.field("data", &self.data);
        dbg.finish()
    }
}

impl Debug for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Void => f.write_str("void"),
            Data::Float(v) => write!(f, "Data::Dec({})", v),
            Data::Int(v) => write!(f, "Data::Int({})", v),
            Data::UInt(v) => write!(f, "Data::UInt({})", v),
            Data::Char(v) => write!(f, "Data::Char({})", v),
            Data::Bool(v) => write!(f, "Data::Bool({})", v),
            Data::Boxed(ptr, layout) => write!(f, "Data::Boxed({:?}, {:?})", ptr, layout),
            Data::Uninit(ptr, layout) => write!(f, "Data::Uninit({:?}, {:?})", ptr, layout),
        }
    }
}

fn make_read_error(ty: Option<&Arc<Type>>, expected: &Arc<Type>) -> anyhow::Error {
    match ty {
        None => anyhow!("Value cannot be cast to type '{}'", expected),
        Some(ty) => anyhow!("Value of type '{}' cannot be cast to type '{}'", ty, expected),
    }
}