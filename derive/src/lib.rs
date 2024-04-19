mod metadata;

use proc_macro::TokenStream;

#[proc_macro_derive(Metadata, attributes(metadata))]
pub fn write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata::derive(ast)
}
