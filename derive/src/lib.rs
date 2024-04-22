mod metadata;
mod oparators;

use proc_macro::TokenStream;

#[cfg(feature = "metadata")]
#[proc_macro_derive(Metadata, attributes(metadata))]
pub fn write(ast: TokenStream) -> TokenStream {
	let ast = syn::parse(ast).unwrap();
	metadata::derive(ast)
}

#[proc_macro]
#[cfg(feature = "operators")]
pub fn impl_int_operators(s: TokenStream) -> TokenStream {
	oparators::impl_int_operators(s)
}
