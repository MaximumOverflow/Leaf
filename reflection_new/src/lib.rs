mod metadata;
pub mod heaps;

pub use metadata::*;

#[cfg(feature = "write")]
pub mod write {
    use std::io::Error;

    pub trait Write<'l> {
        type Requirements;
        fn write<T: std::io::Write>(&'l self, stream: &mut T, req: Self::Requirements) -> Result<(), Error>;
    }

    impl Write<'_> for bool {
        type Requirements = ();
        fn write<T: std::io::Write>(&self, stream: &mut T, _: Self::Requirements) -> Result<(), Error> {
            stream.write_all(&[*self as u8])
        }
    }

    impl Write<'_> for usize {
        type Requirements = ();
        fn write<T: std::io::Write>(&self, stream: &mut T, _: Self::Requirements) -> Result<(), Error> {
            let mut n = *self;
            while n >= 0x80 {
                stream.write_all(&[0x80 | (n as u8)])?;
                n >>= 7;
            }
            stream.write_all(&[n as u8])
        }
    }
}
