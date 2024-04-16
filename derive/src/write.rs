use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{Data, DeriveInput, GenericParam};

use crate::utils::get_discriminant;

pub fn derive(ast: DeriveInput) -> TokenStream {
	let name = &ast.ident;
	let mut generics = String::new();

	for param in ast.generics.params.iter() {
		match *param {
			GenericParam::Type(_) => unimplemented!(),
			GenericParam::Const(_) => unimplemented!(),
			GenericParam::Lifetime(_) => generics.push_str("'_, "),
		}
	}

	let ty: proc_macro2::TokenStream = match generics.len() == 0 {
		true => name.to_string().parse().unwrap(),
		false => {
			generics.drain(generics.len() - 2..);
			format!("{}<{}>", name, generics).parse().unwrap()
		},
	};

	match &ast.data {
		Data::Struct(data) => {
			let mut writes = vec![];

			for (i, field) in data.fields.iter().enumerate() {
				match field.ident.as_ref() {
					Some(ident) => {
						writes
							.push(quote!(crate::write::Write::write(&self.#ident, stream, req)?;));
					},
					None => {
						let ident: proc_macro2::TokenStream = format!("{}", i).parse().unwrap();
						writes
							.push(quote!(crate::write::Write::write(&self.#ident, stream, req)?;));
					},
				}
			}

			let implementation = quote! {
				impl crate::write::Write<'_> for #ty {
					type Requirements = ();
					fn write<T: std::io::Write>(&self, stream: &mut T, req: Self::Requirements) -> std::result::Result<(), std::io::Error> {
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
			let raw_discriminant = ast
				.attrs
				.iter()
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
						writes.push(quote!(crate::write::Write::write(#field, stream, req)?;))
					}

					cases.push(quote!(#name::#variant_name(#(#fields),*) => { #(#writes)* }))
				} else {
					unimplemented!();
				}
			}

			let write_discriminant = match raw_discriminant {
				true => quote!(stream.write_all(bytemuck::bytes_of(&discriminant))?;),
				false => quote!(crate::write::Write::write(&discriminant, stream, ())?;),
			};

			let implementation = quote! {
				impl crate::write::Write<'_> for #ty {
					type Requirements = ();
					#[allow(unused_parens)]
					fn write<T: std::io::Write>(&self, stream: &mut T, req: Self::Requirements) -> std::result::Result<(), std::io::Error> {
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
		Data::Union(_) => panic!("Cannot #[derive(Write)] on a union"),
	}
}
