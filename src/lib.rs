// pub trait UpdateField {
// // fn update_field(&mut self, tag: &Tag, doc: &Document) {
// // let field_name = &tag.name.local_part;
// // if let Some(attributes) = &tag.attributes {
// // attributes.iter().for_each(|attr| {
// // if let Attribute::Instance { name, value } = attr {
// // match value {
// // AttributeValue::Value(attr_val) => {
// // if name.local_part == "isbn" {
// // self.isbn = attr_val.to_string();
// // }
// // }
// // AttributeValue::Values(values) => values.iter().for_each(|val| {
// // if name.local_part == "isbn" {
// // self.isbn = val.to_string();
// // }
// // }),
// // _ => {}
// // }
// // }
// // })
// // }
// // }
// }
mod utils;

use syn::Attribute;
use utils::fields_extraction::{get_fields, AttributedFields, NonAttributedFields};

use proc_macro::TokenStream;

#[proc_macro_derive(UpdateFields, attributes(update))]
pub fn derive_macro(input: TokenStream) -> TokenStream {
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

    let attributed_fields_fields = &attributed_fields.fields;
    let attributed_fields_ty = &attributed_fields.ty;
    let non_attributed_fields_fields = &non_attributed_fields.fields;
    let non_attributed_fields_ty = &non_attributed_fields.ty;

    let gen = quote::quote! {
        impl UpdateField for #name {
            fn update_field(&mut self, tag: &Tag, doc: &Document) {
                let field_name = &tag.name.local_part;
                if let Some(attributes) = &tag.attributes {
                    attributes.iter().for_each(|attr| {
                        if let Attribute::Instance { name, value: AttributeValue::Value(attr_val) } = attr {
                            match name.local_part.as_str() {
                                #(
                                    stringify!(#attributed_fields_fields) => {
                                        if name.local_part == stringify!(#attributed_fields_fields) {
                                            self.#attributed_fields_fields = attr_val.to_string();
                                        }
                                    }
                                )*
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
                        #(
                            stringify!(#non_attributed_fields_fields) => {
                                self.#non_attributed_fields_fields = value.to_string();
                            }
                        )*
                        e => {
                            eprintln!("Unknown field: {}", e);
                        }
                    }
                } else {
                    eprintln!("Content is missing");
                }
            }
        }
    };
    //dbg!(&gen);
    gen.into()
}
