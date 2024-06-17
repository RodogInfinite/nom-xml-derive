use std::collections::HashSet;

use syn::Ident;

pub(crate) mod fields_extraction;
pub(crate) mod update_field;

pub fn get_standard_library_types() -> HashSet<Ident> {
    let mut types = HashSet::new();
    types.insert(Ident::new("String", proc_macro2::Span::call_site()));
    types.insert(Ident::new("Vec", proc_macro2::Span::call_site()));
    types
}
