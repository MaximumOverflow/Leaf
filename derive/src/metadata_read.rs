use crate::utils::get_discriminant;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput};
use proc_macro::TokenStream;

pub fn derive(ast: DeriveInput) -> TokenStream {
	let name = &ast.ident;

	match &ast.data {
		Data::Struct(data) => {
			let mut reads = vec![];

			for field in &data.fields {
				let ident = field.ident.as_ref().unwrap();
				reads.push(quote!(#ident: crate::metadata::MetadataRead::read(stream)?,))
			}

			let implementation = quote! {
				impl crate::metadata::MetadataRead for #name {
					fn read<T: std::io::Read>(stream: &mut T) -> std::result::Result<Self, std::io::Error> {
						std::result::Result::Ok(
							#name {
								#(#reads)*
							}
						)
					}
				}
			};

			implementation.into()
		},
		Data::Enum(data) => {
			let mut cases = vec![];
			let mut reads = vec![];
			let mut discriminants = vec![];

			let discriminant_ty = get_discriminant(&ast.attrs);
			let raw_discriminant = ast
				.attrs
				.iter()
				.filter_map(|a| a.path().get_ident())
				.any(|i| i == "raw_discriminant");

			for variant in &data.variants {
				reads.clear();
				let variant_name = &variant.ident;

				let Some((_, discriminant)) = &variant.discriminant else {
					panic!("Enum variant '{}' is lacking an explicit discriminant required by MetadataRead", variant.ident)
				};

				let discriminant_name = format_ident!("DISCRIMINANT_{}", discriminants.len());
				discriminants
					.push(quote!(const #discriminant_name: #discriminant_ty = #discriminant;));

				if variant.fields.is_empty() {
					cases.push(quote!(#discriminant_name => #name::#variant_name,))
				} else if variant.fields.iter().all(|f| f.ident.is_none()) {
					reads.clear();
					for _ in 0..variant.fields.len() {
						reads.push(quote!(crate::metadata::MetadataRead::read(stream)?))
					}
					cases.push(quote!(#discriminant_name => #name::#variant_name(#(#reads),*),))
				} else {
					unimplemented!();
				}
			}

			let read_discriminant = match raw_discriminant {
				true => quote! {
					let mut discriminant: #discriminant_ty = std::default::Default::default();
					stream.read_exact(bytemuck::bytes_of_mut(&mut discriminant))?;
				},
				false => quote! {
					let discriminant = <#discriminant_ty as crate::metadata::MetadataRead>::read(stream)?;
				},
			};

			let implementation = quote! {
				impl crate::metadata::MetadataRead for #name {
					fn read<T: std::io::Read>(stream: &mut T) -> std::result::Result<Self, std::io::Error> {
						#(#discriminants)*
						#read_discriminant
						let value = match discriminant {
							#(#cases)*
							_ => return std::result::Result::Err(
								std::io::Error::new(
									std::io::ErrorKind::InvalidData,
									format!("Invalid enum discriminant {:#X}", discriminant)
								)
							),
						};
						std::result::Result::Ok(value)
					}
				}
			};

			implementation.into()
		},
		Data::Union(_) => panic!("Cannot #[derive(MetadataRead)] on a union"),
	}
}
