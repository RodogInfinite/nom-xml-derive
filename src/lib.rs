mod utils;

use utils::{get_standard_library_types, update_field::FieldsContext};

#[proc_macro_derive(ExtractFields, attributes(extract))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = match syn::parse(input) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };

    let struct_name = &ast.ident;
    let std_types = &get_standard_library_types();

    let mut params = FieldsContext::new(struct_name, std_types);
    let mut params = params.as_mut_refs();
    match params.get_fields(&ast) {
        Ok(()) => {}
        Err(e) => {
            return e.to_compile_error().into();
        }
    }

    let gen_update_fields = match params.generate_update_fields() {
        Ok(gen) => gen,
        Err(e) => return e.to_compile_error().into(),
    };

    gen_update_fields.into()
}
