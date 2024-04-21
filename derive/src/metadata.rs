use proc_macro::TokenStream;
use std::fmt::Write;

use darling::{FromDeriveInput, FromMeta};
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{Attribute, Data, DeriveInput, GenericParam};

#[rustfmt::skip]
pub fn get_discriminant(attrs: &[Attribute]) -> Ident {
	for attr in attrs {
		let Some(ident) = attr.path().get_ident() else { continue; };
		if ident == "repr" { return attr.parse_args::<Ident>().unwrap(); }
	}
	format_ident!("usize")
}

#[derive(Default, FromMeta)]
#[darling(default)]
pub struct Lifetimes {
	pub val: Option<Ident>,
	pub req: Option<Ident>,
}

#[derive(Default, FromDeriveInput)]
#[darling(default, attributes(metadata))]
pub struct Args {
	pub lifetimes: Lifetimes,
}

pub fn derive(ast: DeriveInput) -> TokenStream {
	let name = &ast.ident;
	let args = Args::from_derive_input(&ast).unwrap();

	let generics: Option<proc_macro2::TokenStream> = {
		let mut text = String::new();
		for param in &ast.generics.params {
			match param {
				GenericParam::Lifetime(param) => {
					if args.lifetimes.val.as_ref() == Some(&param.lifetime.ident) {
						text.push_str("'__val,");
					} else if args.lifetimes.req.as_ref() == Some(&param.lifetime.ident) {
						text.push_str("'__req,");
					} else {
						write!(text, "{},", param.lifetime).unwrap()
					}
				},
				GenericParam::Type(_) => unimplemented!(),
				GenericParam::Const(_) => unimplemented!(),
			}
		}

		match text.is_empty() {
			true => None,
			false => {
				text.insert(0, '<');
				text.push('>');
				Some(text.parse().unwrap())
			},
		}
	};

	let (read_impl, write_impl) = match &ast.data {
		Data::Union(_) => unimplemented!(),
		Data::Struct(data) => {
			let mut is_tuple = true;
			let mut fields = Vec::with_capacity(data.fields.len());
			let mut field_reads = Vec::with_capacity(data.fields.len());
			let mut field_writes = Vec::with_capacity(data.fields.len());

			for (i, field) in data.fields.iter().enumerate() {
				let (read_ident, write_ident) = match &field.ident {
					None => (format_ident!("__fld_{i}"), format!("{i}").parse().unwrap()),
					Some(ident) => {
						is_tuple = false;
						(ident.clone(), ident.to_token_stream())
					},
				};
				field_reads.push(quote! {
					let #read_ident = crate::serialization::MetadataRead::read(__stream, __req)?;
				});
				field_writes.push(quote! {
					crate::serialization::MetadataWrite::write(&self.#write_ident, __stream, __req)?;
				});
				fields.push(read_ident);
			}

			(
				match is_tuple {
					true => match fields.is_empty() {
						true => quote! {
							Ok(Self)
						},
						false => quote! {
							let __req = __req.into();
							#(#field_reads)*
							Ok(Self(#(#fields),*))
						},
					},
					false => quote! {
						let __req = __req.into();
						#(#field_reads)*
						Ok(Self { #(#fields),* })
					},
				},
				quote! {
					let __req = __req.into();
					#(#field_writes)*
					std::result::Result::Ok(())
				},
			)
		},
		Data::Enum(data) => {
			let discriminant_ty = get_discriminant(&ast.attrs);
			let mut read_operations = Vec::with_capacity(data.variants.len());
			let mut write_operations = Vec::with_capacity(data.variants.len());
			let mut const_discriminants = Vec::with_capacity(data.variants.len());
			for (i, variant) in data.variants.iter().enumerate() {
				let ident = &variant.ident;
				let Some((_, discriminant)) = &variant.discriminant else {
					panic!("Enum variants must specify their discriminants explicitly");
				};

				let const_ident = format_ident!("__DISCRIMINANT_{}", i);
				const_discriminants.push(quote! {
					const #const_ident: #discriminant_ty = #discriminant;
				});

				let mut is_tuple = true;
				let mut fields = Vec::with_capacity(variant.fields.len());
				let mut field_reads = Vec::with_capacity(variant.fields.len());
				let mut field_writes = Vec::with_capacity(variant.fields.len());
				for (i, field) in variant.fields.iter().enumerate() {
					let ident = match &field.ident {
						None => format_ident!("__fld_{i}"),
						Some(ident) => {
							is_tuple = false;
							ident.clone()
						},
					};
					field_reads.push(quote! {
						let #ident = crate::serialization::MetadataRead::read(__stream, __req)?;
					});
					field_writes.push(quote! {
						crate::serialization::MetadataWrite::write(#ident, __stream, __req)?;
					});
					fields.push(ident);
				}

				read_operations.push(match is_tuple {
					true => match fields.is_empty() {
						true => quote! {
							#discriminant => Ok(Self::#ident),
						},
						false => quote! {
							#discriminant => {
								#(#field_reads)*
								Ok(Self::#ident(#(#fields),*))
							},
						},
					},
					false => quote! {
						#discriminant => {
							#(#field_reads)*
							Ok(Self::#ident { #(#fields),* })
						},
					},
				});

				write_operations.push(match is_tuple {
					true => match fields.is_empty() {
						true => quote! {
							Self::#ident => std::result::Result::Ok(()),
						},
						false => quote! {
							Self::#ident(#(#fields),*) => {
								#(#field_writes)*
								std::result::Result::Ok(())
							},
						},
					},
					false => quote! {
						Self::#ident { #(#fields),* } => {
							#(#field_writes)*
							std::result::Result::Ok(())
						},
					},
				});
			}

			(
				quote! {
					#(#const_discriminants)*

					let __req = __req.into();
					let __disc = <#discriminant_ty as crate::serialization::MetadataRead>::read(__stream, __req)?;
					match __disc {
						#(#read_operations)*
						_ => std::result::Result::Err(std::io::Error::new(
							std::io::ErrorKind::InvalidData,
							std::format!("{__disc:#X?} is not a valid enum discriminant"),
						)),
					}
				},
				quote! {
					let __req = __req.into();
					let __disc: #discriminant_ty = unsafe {
						std::mem::transmute(std::mem::discriminant(self))
					};
					<#discriminant_ty as crate::serialization::MetadataWrite>::write(&__disc, __stream, __req)?;
					match self {
						#(#write_operations)*
					}
				},
			)
		},
	};

	return quote! {
		#[cfg(feature = "read")]
		impl<'__val: '__req, '__req> crate::serialization::MetadataRead<'__val, '__req> for #name #generics {
			type Requirements = &'__req crate::serialization::ReadRequirements<'__val>;
			fn read<__S: std::io::Read>(__stream: &mut __S, __req: impl std::convert::Into<Self::Requirements>) -> Result<Self, std::io::Error> {
				#read_impl
			}
		}

		#[cfg(feature = "write")]
		impl<'__val: '__req, '__req> crate::serialization::MetadataWrite<'__val, '__req> for #name #generics {
			type Requirements = &'__req crate::serialization::WriteRequirements<'__val>;
			fn write<__S: std::io::Write>(&self, __stream: &mut __S, __req: impl std::convert::Into<Self::Requirements>) -> Result<(), std::io::Error> {
				#write_impl
			}
		}
	}.into();
}
