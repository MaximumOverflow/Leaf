use quote::{format_ident, quote};
use syn::{Data, DeriveInput};
use proc_macro::TokenStream;

pub fn derive(ast: DeriveInput) -> TokenStream {
    let name = &ast.ident;

    match &ast.data {
        Data::Struct(_) => {
            panic!("Cannot #[derive(MetadataWrite)] on a struct")
        },
        Data::Enum(data) => {
            let mut cases = vec![];
            let mut reads = vec![];
            let mut discriminants = vec![];

            for variant in &data.variants {
                reads.clear();
                let variant_name = &variant.ident;

                let Some((_, discriminant)) = &variant.discriminant else {
                    panic!("Enum variant {} is lacking an explicit discriminant required by MetadataWrite", variant.ident)
                };

                let variant_value_name = format_ident!("DISCRIMINANT_{}", discriminants.len());
                discriminants.push(quote!(const #variant_value_name: usize = (#discriminant) as usize;));

                if variant.fields.is_empty() {
                    cases.push(quote!(#variant_value_name => #name::#variant_name,))
                }
                else if variant.fields.iter().all(|f| f.ident.is_none()) {
                    reads.clear();
                    for field in &variant.fields {
                        let ty = &field.ty;
                        reads.push(quote!(crate::metadata::MetadataRead::read(stream)?))
                    }
                    cases.push(quote!(#variant_value_name => #name::#variant_name(#(#reads),*),))
                }
                else {
                    unimplemented!();
                }
            }

            let implementation = quote! {
                impl crate::metadata::MetadataRead for #name {
                    fn read<T: std::io::Read>(stream: &mut T) -> std::result::Result<Self, std::io::Error> {
                        #(#discriminants)*
                        let discriminant = <usize as crate::metadata::MetadataRead>::read(stream)?;
                        let value = match discriminant {
                            #(#cases)*
                            _ => return std::result::Result::Err(
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    format!("Invalid opcode {:#X}", discriminant)
                                )
                            ),
                        };
                        std::result::Result::Ok(value)
                    }
                }
			};

            implementation.into()
        },
        Data::Union(_) => panic!("Cannot #[derive(MetadataWrite)] on a union"),
    }
}