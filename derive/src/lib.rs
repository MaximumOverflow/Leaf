mod metadata_write;

use proc_macro::TokenStream;

#[proc_macro_derive(MetadataWrite)]
pub fn metadata_write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata_write::derive(ast)
}
