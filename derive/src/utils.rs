use quote::format_ident;
use syn::{Attribute, Ident};

pub fn get_discriminant(attrs: &[Attribute]) -> Ident {
    for attr in attrs {
        let Some(ident) = attr.path().get_ident() else {
            continue;
        };

        if ident == "repr" {
            return attr.parse_args::<Ident>().unwrap();
        }
    }

    format_ident!("usize")
}