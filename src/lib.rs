mod utils;

use std::collections::HashSet;

use utils::{
    fields_extraction::{
        get_fields, Attributed, FieldTypes, NonAttributed, OptionField, SubField, VecField,
    },
    get_standard_library_types,
    update_field::{generate_update_fields, FieldParameters},
};

#[proc_macro_derive(ExtractFields, attributes(extract))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = match syn::parse(input) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };

    let struct_name = &ast.ident;
    let std_types = &get_standard_library_types();
    let attributed_fields = &mut FieldTypes::<Attributed>::default();
    let non_attributed_fields = &mut FieldTypes::<NonAttributed>::default();
    let sub_fields = &mut FieldTypes::<SubField>::default();
    let sub_opt_fields = &mut FieldTypes::<OptionField>::default();
    let vec_fields = &mut FieldTypes::<VecField>::default();
    let vec_sub_fields = &mut FieldTypes::<SubField>::default();
    let vec_opt_fields = &mut FieldTypes::<OptionField>::default();
    let vec_opt_sub_fields = &mut FieldTypes::<SubField>::default();
    let attributed_opt_fields = &mut FieldTypes::<OptionField>::default();
    let non_attributed_opt_fields = &mut FieldTypes::<OptionField>::default();
    let field_set = &mut Vec::<String>::new();

    let mut params = FieldParameters {
        struct_name,
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        sub_opt_fields,
        vec_fields,
        vec_sub_fields,
        vec_opt_fields,
        vec_opt_sub_fields,
        attributed_opt_fields,
        non_attributed_opt_fields,
        std_types,
        field_set,
    };
    match get_fields(&mut params, &ast) {
        Ok(()) => {}
        Err(e) => {
            return e.to_compile_error().into();
        }
    }

    let gen_update_fields = match generate_update_fields(&mut params) {
        Ok(gen) => gen,
        Err(e) => return e.to_compile_error().into(),
    };

    gen_update_fields.into()
}
