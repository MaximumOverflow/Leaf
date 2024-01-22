use syn::{Data, DeriveInput};
use proc_macro::TokenStream;
use quote::quote;

pub fn derive(ast: DeriveInput) -> TokenStream {
	let name = &ast.ident;
	let mut writes = vec![];

	let fields = match ast.data {
		Data::Struct(data) => data.fields,
		Data::Enum(_) => panic!("Cannot #[derive(MetadataWrite)] on an enum."),
		Data::Union(_) => panic!("Cannot #[derive(MetadataWrite)] on a union."),
	};

	for field in &fields {
		let ident = field.ident.as_ref().unwrap();
		writes.push(quote!(crate::metadata::MetadataWrite::write(&self.#ident, stream)?;))
	}

	let implementation = quote! {
		impl crate::metadata::MetadataWrite for #name {
			fn write<T: std::io::Write>(&self, stream: &mut T) -> Result<(), std::io::Error> {
				#(#writes)*
				Ok(())
			}
		}
	};

	implementation.into()
}
