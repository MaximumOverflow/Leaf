use crate::utils::get_discriminant;
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

			let discriminant_ty = get_discriminant(&ast.attrs);
			let raw_discriminant = ast.attrs.iter()
				.filter_map(|a| a.path().get_ident())
				.any(|i| i == "raw_discriminant");

			for variant in &data.variants {
				writes.clear();
				let variant_name = &variant.ident;

				if variant.fields.is_empty() {
					continue;
				}

				if variant.fields.iter().all(|f| f.ident.is_none()) {
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

			let write_discriminant = match raw_discriminant {
				true => quote!(stream.write_all(bytemuck::bytes_of(&discriminant))?;),
				false => quote!(crate::metadata::MetadataWrite::write(&discriminant, stream)?;)
			};

			let implementation = quote! {
				impl crate::metadata::MetadataWrite for #name {
					#[allow(unused_parens)]
					fn write<T: std::io::Write>(&self, stream: &mut T) -> std::result::Result<(), std::io::Error> {
						let discriminant: #discriminant_ty = unsafe { std::mem::transmute(std::mem::discriminant(self)) };
						#write_discriminant

						match self {
							#(#cases)*
							_ => {},
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
