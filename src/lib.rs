mod utils;

use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;
use utils::fields_extraction::{get_fields, AttributedFields, NonAttributedFields};

#[proc_macro_derive(UpdateFields, attributes(update))]
pub fn derive_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;

    //dbg!(name);
    //dbg!(ast.data);

    let attributed_fields = &mut AttributedFields::default();
    let non_attributed_fields = &mut NonAttributedFields::default();

    match if let syn::Data::Struct(data_struct) = ast.data {
        get_fields(attributed_fields, non_attributed_fields, data_struct)
    } else {
        return syn::Error::new_spanned(&ast, "UpdateFields can only be derived for structs")
            .to_compile_error()
            .into();
    } {
        Ok(()) => {}
        Err(e) => return e.to_compile_error().into(),
    }

    fn is_numeric_type(ty: &Ident) -> bool {
        matches!(
            ty.to_string().as_str(),
            "i8" | "i16"
                | "i32"
                | "i64"
                | "i128"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "u128"
                | "f32"
                | "f64"
        )
    }

    pub fn generate_update_field_impl(
        struct_name: &Ident,
        non_attributed_fields: &mut NonAttributedFields,
        attributed_fields: &mut AttributedFields,
    ) -> TokenStream {
        let attributed_fields_match_arms = attributed_fields.fields.iter().map(|field_name| {
            quote! {
                stringify!(#field_name) => {
                    self.#field_name = attr_val.into();
                }
            }
        });

        let non_attributed_fields_match_arms = non_attributed_fields
            .fields
            .iter()
            .zip(non_attributed_fields.ty.iter())
            .map(|(field_name, field_type)| {
                if is_numeric_type(field_type) {
                    quote! {
                        stringify!(#field_name) => {
                            self.#field_name = value.parse::<#field_type>().unwrap();
                        }
                    }
                } else {
                    quote! {
                        stringify!(#field_name) => {
                            self.#field_name = value.to_string();
                        }
                    }
                }
            });

        quote! {
            impl UpdateField for #struct_name {
                fn update_field(&mut self, tag: &Tag, doc: &Document) {
                    let field_name = &tag.name.local_part;
                    if let Some(attributes) = &tag.attributes {
                        attributes.iter().for_each(|attr| {
                            if let Attribute::Instance { name, value: AttributeValue::Value(attr_val) } = attr {
                                match name.local_part.as_str() {
                                    #(#attributed_fields_match_arms,)*
                                    _ => {
                                        eprintln!("Unknown attribute: {}", name.local_part);
                                    }
                                }
                            }
                        });
                    }

                    if let Document::Nested(_) = &doc {
                        doc.iter_with_depth(1).for_each(|record| {
                            if let Document::Element(tag, inner_doc, _) = record {
                                self.update_field(tag, inner_doc);
                            } else {
                                eprintln!("Unknown field: {record:#?}");
                            }
                        });
                    } else if let Document::Content(Some(value)) = &doc {
                        match field_name.as_str() {
                            #(#non_attributed_fields_match_arms,)*
                            e => {
                                eprintln!("Unknown field: {}", e);
                            }
                        }
                    } else {
                        eprintln!("Content is missing");
                    }
                }
            }
        }
    }

    //dbg!(&gen);
    let gen_update_field =
        generate_update_field_impl(name, non_attributed_fields, attributed_fields);
    let combined_gen = quote::quote! {
        //#update_field_gen
        #gen_update_field
    };
    combined_gen.into()
}
