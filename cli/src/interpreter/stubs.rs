pub trait ExternFunctionStub<P, R>
where
	Self: 'static,
{
	unsafe fn dyn_call(&self, params: &[u8]) -> R;
}

impl<P, R, F: Fn(P) -> R + 'static> ExternFunctionStub<P, R> for F {
	unsafe fn dyn_call(&self, params: &[u8]) -> R {
		let p0 = std::ptr::read_unaligned::<P>(params.as_ptr() as *const P);
		self(p0)
	}
}
