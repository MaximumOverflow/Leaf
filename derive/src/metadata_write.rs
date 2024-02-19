use quote::{format_ident, quote};
use proc_macro::{TokenStream};
use syn::{Data, DeriveInput};

pub fn derive(ast: DeriveInput) -> TokenStream {
	let name = &ast.ident;

	match &ast.data {
		Data::Struct(data) => {
			let mut writes = vec![];

			for field in &data.fields {
				let ident = field.ident.as_ref().unwrap();
				writes.push(quote!(crate::metadata::MetadataWrite::write(&self.#ident, stream)?;))
			}

			let implementation = quote! {
				impl crate::metadata::MetadataWrite for #name {
					fn write<T: std::io::Write>(&self, stream: &mut T) -> std::result::Result<(), std::io::Error> {
						#(#writes)*
						std::result::Result::Ok(())
					}
				}
			};

			implementation.into()
		},
		Data::Enum(data) => {
			let mut cases = vec![];
			let mut writes = vec![];
			let mut fields = vec![];
			let mut discriminants = vec![];

			for variant in &data.variants {
				writes.clear();
				let variant_name = &variant.ident;
				let Some((_, discriminant)) = &variant.discriminant else {
					panic!("Enum variant {} is lacking an explicit discriminant required by MetadataWrite", variant.ident)
				};

				let variant_value_name = format_ident!("DISCRIMINANT_{}", discriminants.len());
				discriminants.push(quote!(const #variant_value_name: usize = (#discriminant) as usize;));
				writes.push(quote!(crate::metadata::MetadataWrite::write(&#variant_value_name, stream)?;));

				if variant.fields.is_empty() {
					cases.push(quote!(#name::#variant_name => { #(#writes)* }))
				}
				else if variant.fields.iter().all(|f| f.ident.is_none()) {
					fields.clear();
					fields.extend((0..variant.fields.len()).map(|i| format_ident!("fld_{i}")));

					for field in &fields {
						writes.push(quote!(crate::metadata::MetadataWrite::write(#field, stream)?;))
					}

					cases.push(quote!(#name::#variant_name(#(#fields),*) => { #(#writes)* }))
				}
				else {
					unimplemented!();
				}
			}

			let implementation = quote! {
				impl crate::metadata::MetadataWrite for #name {
					#[allow(unused_parens)]
					fn write<T: std::io::Write>(&self, stream: &mut T) -> std::result::Result<(), std::io::Error> {
						#(#discriminants)*
						match self {
							#(#cases)*
						}
						std::result::Result::Ok(())
					}
				}
			};

			implementation.into()
		},
		Data::Union(_) => panic!("Cannot #[derive(MetadataWrite)] on a union"),
	}
}
