use bytemuck::{Pod, pod_read_unaligned};

pub trait ExternFunctionStub<P, R>
where
	Self: 'static,
	P: Pod,
	R: Pod,
{
	unsafe fn dyn_call(&self, params: &[u8]) -> R;
}

impl<P: Pod, R: Pod, F: Fn(P) -> R + 'static> ExternFunctionStub<P, R> for F {
	unsafe fn dyn_call(&self, params: &[u8]) -> R {
		let p0 = pod_read_unaligned::<P>(params);
		self(p0)
	}
}
