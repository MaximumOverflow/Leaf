use quote::{format_ident, quote};
use proc_macro::{TokenStream};
use std::cmp::Ordering;
use proc_macro2::Ident;

//TODO Make a better implementation that doesn't generate all possible permutations
pub fn impl_int_operators(_: TokenStream) -> TokenStream {
	let int_sizes = [8, 16, 32, 64usize];
	let mut operator_cases = vec![];

	for operator in [
		format_ident!("Add"),
		format_ident!("Sub"),
		format_ident!("Mul"),
		format_ident!("Div"),
		format_ident!("Mod"),
	] {
		let mut permutations = vec![];
		generate_permutations(
			format_ident!("S{operator}"),
			"Int",
			&int_sizes,
			false,
			&mut permutations,
		);

		generate_permutations(
			format_ident!("U{operator}"),
			"UInt",
			&int_sizes,
			false,
			&mut permutations,
		);

		operator_cases.push(quote! {
			BinaryOperator::#operator => match (lhs_ty, rhs_ty) {
				#(#permutations)*
				_ => {}
			}
		})
	}

	{
		let mut permutations = vec![];
		generate_permutations(
			format_ident!("SCmp"),
			"Int",
			&int_sizes,
			true,
			&mut permutations,
		);

		generate_permutations(
			format_ident!("UCmp"),
			"UInt",
			&int_sizes,
			true,
			&mut permutations,
		);

		operator_cases.push(quote! {
			| BinaryOperator::Eq
			| BinaryOperator::Ne
			| BinaryOperator::Lt
			| BinaryOperator::Gt
			| BinaryOperator::Le
			| BinaryOperator::Ge => match (lhs_ty, rhs_ty) {
				#(#permutations)*
				_ => {}
			}
		});
	}

	let tokens = quote! {
		match operator {
			#(#operator_cases)*
			_ => {},
		}
	};
	tokens.into()
}

fn generate_permutations(
	opcode: Ident,
	ty_prefix: &str,
	sizes: &[usize],
	is_comparison: bool,
	permutations: &mut Vec<proc_macro2::TokenStream>,
) {
	let cmp_param = match is_comparison {
		false => None,
		true => Some(quote!(op_to_cmp(*operator))),
	};

	for lhs_size in sizes.iter().cloned() {
		let lhs_t = format_ident!("{ty_prefix}{lhs_size}");
		for rhs_size in sizes.iter().cloned() {
			let rhs_t = format_ident!("{ty_prefix}{rhs_size}");

			let ret_ty = match is_comparison {
				true => quote!(&Type::Bool),
				false => match lhs_size.cmp(&rhs_size) {
					Ordering::Less => quote!(&Type::#rhs_t),
					Ordering::Equal => quote!(&Type::#rhs_t),
					Ordering::Greater => quote!(&Type::#lhs_t),
				},
			};

			let operation = match lhs_size.cmp(&rhs_size) {
				Ordering::Less => quote! {
						let conv = body.temporary(&Type::#rhs_t);
						body.push_opcode(Opcode::SConv(lhs, conv, #rhs_size));
						let local = body.temporary(#ret_ty);
						body.push_opcode(Opcode::#opcode(lhs, conv, local, #cmp_param));
				},
				Ordering::Equal => quote! {
					let local = body.temporary(#ret_ty);
					body.push_opcode(Opcode::#opcode(lhs, rhs, local, #cmp_param));
				},
				Ordering::Greater => quote! {
					let conv = body.temporary(&Type::#lhs_t);
					body.push_opcode(Opcode::SConv(rhs, conv, #lhs_size));
					let local = body.temporary(#ret_ty);
					body.push_opcode(Opcode::#opcode(lhs, conv, local, #cmp_param));
				},
			};
			permutations.push(quote! {
				(Type::#lhs_t, Type::#rhs_t) => {
					#operation
					return Ok(ExpressionResult::Value {
						idx: local,
						ty: #ret_ty,
					});
				}
				(Type::Reference { ty: Type::#lhs_t, .. }, Type::#rhs_t) => {
					let load = body.temporary(&Type::#lhs_t);
					body.push_opcode(Opcode::Load(lhs, load));
					let lhs = load;
					#operation
					return Ok(ExpressionResult::Value {
						idx: local,
						ty: #ret_ty,
					});
				}
			});
		}
	}
}
