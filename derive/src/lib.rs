mod metadata_write;
mod metadata_read;
mod utils;
mod write;

use proc_macro::TokenStream;

#[proc_macro_derive(MetadataRead, attributes(raw_discriminant))]
pub fn metadata_read(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata_read::derive(ast)
}

#[proc_macro_derive(MetadataWrite, attributes(raw_discriminant))]
pub fn metadata_write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata_write::derive(ast)
}

#[proc_macro_derive(Write, attributes(raw_discriminant))]
pub fn write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	write::derive(ast)
}
