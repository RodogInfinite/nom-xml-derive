mod utils;

use utils::{
    fields_extraction::{
        get_fields, Attributed, FieldTypes, NonAttributed, OptionField, SubField, VecField,
    },
    get_standard_library_types,
    update_field::generate_update_fields,
};

#[proc_macro_derive(ExtractFields, attributes(extract))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = match syn::parse(input) {
        Ok(parsed) => parsed,
        Err(e) => return e.to_compile_error().into(),
    };
    let name = &ast.ident;
    let std_types = &get_standard_library_types();
    let attributed_fields = &mut FieldTypes::<Attributed>::default();
    let non_attributed_fields = &mut FieldTypes::<NonAttributed>::default();
    let sub_fields = &mut FieldTypes::<SubField>::default();
    let sub_opt_fields = &mut FieldTypes::<OptionField>::default();
    let vec_fields = &mut FieldTypes::<VecField>::default();
    let attributed_opt_fields = &mut FieldTypes::<OptionField>::default();
    let non_attributed_opt_fields = &mut FieldTypes::<OptionField>::default();
    match get_fields(
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        sub_opt_fields,
        vec_fields,
        attributed_opt_fields,
        non_attributed_opt_fields,
        std_types,
        &ast,
    ) {
        Ok(()) => {}
        Err(e) => {
            return e.to_compile_error().into();
        }
    }
   

    let gen_update_fields = match generate_update_fields(
        name,
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        sub_opt_fields,
        vec_fields,
        attributed_opt_fields,
        non_attributed_opt_fields,
        std_types,
    ) {
        Ok(gen) => gen,
        Err(e) => return e.to_compile_error().into(),
    };

    gen_update_fields.into()
}
