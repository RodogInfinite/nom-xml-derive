use std::collections::HashSet;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use super::fields_extraction::{Attributed, FieldTypes, NonAttributed, SubField, VecField};

pub fn is_numeric_type(ty: &Ident) -> bool {
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

pub fn generate_update_fields(
    struct_name: &Ident,
    attributed_fields: &mut FieldTypes<Attributed>,
    non_attributed_fields: &mut FieldTypes<NonAttributed>,
    sub_fields: &mut FieldTypes<SubField>,
    vec_fields: &mut FieldTypes<VecField>,
    std_types: HashSet<Ident>,
) -> Result<TokenStream, syn::Error> {
    let attributed_fields_match_arms = attributed_fields.fields.iter().map(|field_name| {
        quote! {
            stringify!(#field_name) => {
                self.#field_name = attr_val.into();
            }
        }
    });

    let non_attributed_fields_match_arms: Result<Vec<TokenStream>, syn::Error> =
        non_attributed_fields
            .fields
            .iter()
            .zip(non_attributed_fields.tys.iter())
            .map(|(field_name, field_type)| {
                if std_types.contains(field_type) {
                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            stringify!(#field_name) => {

                                self.#field_name = value.to_string();
                            }
                        }),
                        _ => Err(syn::Error::new(
                            Span::call_site(),
                            format!("Unimplemented Type Found: `{field_type:?}`"),
                        )),
                    }
                } else if is_numeric_type(field_type) {
                    Ok(quote! {
                        stringify!(#field_name) => {
                            self.#field_name = value.parse::<#field_type>().unwrap();
                        }
                    })
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Unknown Field: `{field_name:?}`"),
                    ))
                }
            })
            .collect();

    let non_attributed_fields_match_arms = non_attributed_fields_match_arms?;
    let sub_field_names = sub_fields.fields.clone();
    let gen_sub_fields = sub_field_names.iter().map(|name| {
        quote! {
            stringify!(#name) => {
                update_fields(inner_doc, &mut self.#name);
            }
        }
    });

    let vec_field_names = vec_fields.fields.clone();
    let vec_field_types = vec_fields.tys.clone();
    let gen_nested = quote! {
        match (tag.name.local_part.as_str(), doc) {
            #((stringify!(#vec_field_names), Document::Nested(ref elements)) => {
                elements.iter().for_each(|element| {
                    if let Document::Element(_, doc, _) = element {
                        let mut nested_field = #vec_field_types::default();
                        if let Document::Nested(inner_elements) = doc.as_ref() {
                            inner_elements.iter().for_each(|inner_element| {
                                if let Document::Element(tag, content, _) = inner_element {
                                    nested_field.update_field(tag, content);
                                }
                            });
                            self.#vec_field_names.push(nested_field);
                            nested_field = #vec_field_types::default();
                        } else {
                            eprintln!("Content is missing in {}", stringify!(#vec_field_names));
                        }
                    }
                });
            },)*
            _ => {//eprintln!("Unknown fieldddd: {}", tag.name.local_part),
            doc.iter_with_depth(1)
                        .for_each(|element| {
            if let Document::Element(tag, inner_doc, _) = element {
                match tag.name.local_part.as_str() {
                    #(#gen_sub_fields,)*
                    _ => {
                        self.update_field(tag, inner_doc);
                    }
                }
            } else {
                eprintln!("Unknown field: {element:#?}");
            }});

        }
    };};

    Ok(quote! {
        impl UpdateField for #struct_name {
            fn update_field(&mut self, tag: &Tag, doc: &Document) {
                let field_name = &tag.name.local_part;
                if let Some(attributes) = &tag.attributes {
                    attributes
                        .iter()
                        .for_each(|attr| {
                            if let Attribute::Instance {
                                name,
                                value: AttributeValue::Value(attr_val),
                            } = attr {
                                match name.local_part.as_str() {
                                    #(#attributed_fields_match_arms,)*
                                    _ => {
                                        {
                                            // ::std::io::_eprint(
                                            //     format_args!("Unknown attribute: {0}\n", name.local_part),
                                            // );
                                        };
                                    }
                                }
                            }
                        });
                }
                if let Document::Nested(_) = &doc {



                    #gen_nested
                } else if let Document::Content(Some(value)) = &doc {
                    match field_name.as_str() {
                        #(#non_attributed_fields_match_arms,)*
                        _ => {
                            {
                                // ::std::io::_eprint(
                                //     format_args!("Unknown field: {0}\n", field_name),
                                // );
                            };
                        }
                    }
                } else {
                    {
                        // ::std::io::_eprint(format_args!("Content is missing\n"));
                    };
                }
            }
        }
    })
}