mod utils;

use utils::{
    fields_extraction::{get_fields, Attributed, FieldTypes, NonAttributed, SubField, VecField},
    get_standard_library_types,
    update_field::generate_update_fields,
};

#[proc_macro_derive(ExtractFields, attributes(extract))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;

    let attributed_fields = &mut FieldTypes::<Attributed>::default();
    let non_attributed_fields = &mut FieldTypes::<NonAttributed>::default();
    let sub_fields = &mut FieldTypes::<SubField>::default();
    let vec_fields = &mut FieldTypes::<VecField>::default();

    match get_fields(
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        vec_fields,
        &ast,
    ) {
        Ok(()) => {}
        Err(e) => return e.to_compile_error().into(),
    }

    //TODO: Test unknown attribute error by adding an attribute to an author field and seeing if that is thrown if the attribute is not defined in the book struct
    let std_types = get_standard_library_types();
    let gen_update_fields = match generate_update_fields(
        name,
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        vec_fields,
        std_types,
    ) {
        Ok(gen) => gen,
        Err(e) => return e.to_compile_error().into(),
    };

    gen_update_fields.into()
}
