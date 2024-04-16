use bytemuck::{Pod, pod_read_unaligned};

pub trait ExternFunctionStub<P, R>
where
	Self: 'static,
	P: Pod,
	R: Pod,
{
	fn call(&self, params: P) -> R;
	fn dyn_call(&self, params: &[u8]) -> R;
}

impl<P: Pod, R: Pod, F: Fn(P) -> R + 'static> ExternFunctionStub<P, R> for F {
	fn call(&self, params: P) -> R {
		self(params)
	}

	fn dyn_call(&self, params: &[u8]) -> R {
		let p0 = pod_read_unaligned::<P>(params);
		self.call(p0)
	}
}
