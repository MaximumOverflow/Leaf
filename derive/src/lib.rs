mod metadata;
use proc_macro::TokenStream;

#[cfg(feature = "metadata")]
#[proc_macro_derive(Metadata, attributes(metadata))]
pub fn write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata::derive(ast)
}
