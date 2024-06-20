use std::collections::HashSet;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use super::fields_extraction::{
    Attributed, FieldTypes, NonAttributed, OptionField, SubField, VecField,
};

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

fn generate_attributed_fields(attributed_fields: &FieldTypes<Attributed>) -> Option<TokenStream> {
    if attributed_fields.fields.is_empty() {
        None
    } else {
        let gen_attributed_fields: Vec<TokenStream> = attributed_fields
            .fields
            .iter()
            .map(|field_name| {
                quote! {
                    stringify!(#field_name) => {
                        self.#field_name = attr_val.into();
                        Ok(())
                    }
                }
            })
            .collect();

        Some(quote! {
            if let Some(attributes) = &tag.attributes {
                attributes.iter().try_for_each(|attr| -> Result<()> {
                    if let Attribute::Instance {
                        name,
                        value: AttributeValue::Value(attr_val),
                    } = attr {
                        match name.local_part.as_str() {
                            #(#gen_attributed_fields,)*
                            e => Err(format!("Unknown attribute: {}\n{}", name.local_part, e).into()),
                        }
                    } else {
                        Err(format!("Unknown attribute: {:#?}", attributes).into())
                    }
                })?;
            }
        })
    }
}

fn generate_attribute_opt_fields(
    attributed_opt_fields: &FieldTypes<OptionField>,
) -> Option<TokenStream> {
    if attributed_opt_fields.fields.is_empty() {
        None
    } else {
        let gen_attributed_opt_fields: Vec<TokenStream> = attributed_opt_fields
            .fields
            .iter()
            .map(|field_name| {
                quote! {
                    stringify!(#field_name) => {
                        self.#field_name = Some(attr_val.into());
                        Ok(())
                    }
                }
            })
            .collect();

        Some(quote! {
            if let Some(attributes) = &tag.attributes {
                attributes.iter().try_for_each(|attr| -> Result<()> {
                    if let Attribute::Instance {
                        name,
                        value: AttributeValue::Value(attr_val),
                    } = attr {
                        match name.local_part.as_str() {
                            #(#gen_attributed_opt_fields,)*
                            e => Err(format!("Unknown attribute: {}\n{}", name.local_part, e).into()),
                        }
                    } else {
                        Err(format!("Unknown attribute: {:#?}", attributes).into())
                    }
                })?;
            }
        })
    }
}

fn generate_sub_fields(sub_fields: &FieldTypes<SubField>) -> Option<TokenStream> {
    if sub_fields.fields.is_empty() {
        None
    } else {
        let gen_sub_fields: Vec<TokenStream> = sub_fields
            .fields
            .iter()
            .map(|field_name| {
                quote! {
                    stringify!(#field_name) => {
                        Self::update_fields(inner_doc, &mut self.#field_name)?;
                    }
                }
            })
            .collect();

        Some(quote! {
            (_, Document::Nested(_)) => {
                doc.iter_with_depth(1)
                    .try_for_each(|element| -> Result<()> {
                        if let Document::Element(tag, inner_doc, _) = element {
                            match tag.name.local_part.as_str() {
                                #(#gen_sub_fields,)*
                                _ => {
                                    self.update_field(tag, inner_doc)?;
                                }
                            }
                            Ok(())
                        } else {
                            return Err(format!("Unknown field: {element:#?}").into());
                        }
                    })?;
                Ok(())
            }
        })
    }
}

fn generate_sub_opt_fields(sub_opt_fields: &FieldTypes<OptionField>) -> Option<TokenStream> {
    if sub_opt_fields.fields.is_empty() {
        None
    } else {
        let gen_sub_opt_fields: Vec<TokenStream> = sub_opt_fields
            .fields
            .iter()
            .map(|field_name| {
                quote! {
                    stringify!(#field_name) => {
                        Self::update_optional_fields(inner_doc, &mut self.#field_name)?;
                    }
                }
            })
            .collect();

        Some(quote! {
            (_, Document::Nested(_)) => {
                doc.iter_with_depth(1)
                    .try_for_each(|element| -> Result<()> {
                        if let Document::Element(tag, inner_doc, _) = element {
                            match tag.name.local_part.as_str() {
                                #(#gen_sub_opt_fields,)*
                                _ => {
                                    self.update_field(tag, inner_doc)?;
                                }
                            }
                            Ok(())
                        } else {
                            return Err(format!("Unknown field: {element:#?}").into());
                        }
                    })?;
                Ok(())
            }
        })
    }
}

fn generate_non_attributed_fields(
    non_attributed_fields: &FieldTypes<NonAttributed>,
    std_types: &HashSet<Ident>,
) -> Result<Option<TokenStream>, syn::Error> {
    if non_attributed_fields.fields.is_empty() {
        Ok(None)
    } else {
        let fields: Vec<TokenStream> = non_attributed_fields
            .fields
            .iter()
            .zip(non_attributed_fields.tys.iter())
            .map(|(field_name, field_type)| {
                if std_types.contains(field_type) {
                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (stringify!(#field_name), Document::Content(Some(value))) => {
                                self.#field_name = value.to_string();
                                Ok(())
                            }
                        }),
                        _ => Err(syn::Error::new(
                            Span::call_site(),
                            format!("Unimplemented Type Found: `{field_type:?}`"),
                        )),
                    }
                } else if is_numeric_type(field_type) {
                    Ok(quote! {
                        (stringify!(#field_name), Document::Content(Some(value))) => {
                            self.#field_name = value.parse::<#field_type>()?;
                            Ok(())
                        }
                    })
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Unknown Field: `{field_name:?}`"),
                    ))
                }
            })
            .collect::<Result<Vec<TokenStream>, syn::Error>>()?;

        if fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                #(#fields)*
            }))
        }
    }
}

fn generate_non_attributed_opt_fields(
    non_attributed_opt_fields: &FieldTypes<OptionField>,
    std_types: &HashSet<Ident>,
) -> Result<Option<TokenStream>, syn::Error> {
    if non_attributed_opt_fields.fields.is_empty() {
        Ok(None)
    } else {
        let fields: Vec<TokenStream> = non_attributed_opt_fields
            .fields
            .iter()
            .zip(non_attributed_opt_fields.tys.iter())
            .map(|(field_name, field_type)| {
                if std_types.contains(field_type) {
                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (stringify!(#field_name), Document::Content(Some(value))) => {
                                self.#field_name = Some(value.to_string());
                                Ok(())
                            }
                        }),
                        _ => Err(syn::Error::new(
                            Span::call_site(),
                            format!("Unimplemented Type Found: `{field_type:?}`"),
                        )),
                    }
                } else if is_numeric_type(field_type) {
                    Ok(quote! {
                        (stringify!(#field_name), Document::Content(Some(value))) => {
                            self.#field_name = Some(value.parse::<#field_type>()?);
                            Ok(())
                        }
                    })
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Unknown Field: `{field_name:?}`"),
                    ))
                }
            })
            .collect::<Result<Vec<TokenStream>, syn::Error>>()?;

        if fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                #(#fields)*
            }))
        }
    }
}

fn generate_vec_fields(vec_fields: &mut FieldTypes<VecField>) -> Option<TokenStream> {
    let vec_field_names = vec_fields.fields.clone();
    let vec_field_types = vec_fields.tys.clone();

    if vec_field_names.is_empty() {
        None
    } else {
        Some(quote! {
            #((stringify!(#vec_field_names), Document::Nested(elements)) => {
                elements.iter().try_for_each(|element| -> Result<()> {
                    if let Document::Element(_, doc, _) = element {
                        let mut nested_field = #vec_field_types::default();
                        if let Document::Nested(inner_elements) = doc.as_ref() {
                            inner_elements.iter().try_for_each(|inner_element| -> Result<()> {
                                if let Document::Element(tag, content, _) = inner_element {
                                    nested_field.update_field(tag, content)?;
                                }
                                Ok(())
                            })?;
                            self.#vec_field_names.push(nested_field);
                            return Ok(());
                        } else {
                            return Err(format!("Content is missing in {:?}", stringify!(#vec_field_names)).into());
                        }
                    }
                    Ok(())
                })?;
                Ok(())
            },)*
        })
    }
}
fn generate_vec_sub_fields(vec_sub_fields: &FieldTypes<SubField>) -> Option<TokenStream> {
    if vec_sub_fields.fields.is_empty() {
        None
    } else {
        let gen_sub_opt_fields: Vec<TokenStream> = vec_sub_fields
            .fields
            .iter()
            .zip(vec_sub_fields.tys.iter())
            .map(|(field_name, field_type)| {
                quote! {
                    (stringify!(#field_name), Document::Nested(elements)) => {
                        elements
                        .iter()
                        .try_for_each(|element| -> Result<()> {
                            let mut nested_field = #field_type::default(); 
                            if let Document::Element(tag, content, _) = element { 
                                    if let Document::Nested(inner_elements) = content.as_ref() {
                                        inner_elements.iter().try_for_each(
                                            |inner_element| -> Result<()> {
                                                if let Document::Element(inner_tag, inner_content, _) = inner_element {
                                                    nested_field.update_field(inner_tag, inner_content)?;
                                                }
                                                Ok(())
                                            },
                                        ).or_else(|_| {
                                            nested_field.update_field(tag,content)
                                        })?;
                                    }
                                } 
                                
                            self.#field_name.push(nested_field.clone()); 
                            Ok(())
                        })?;
                        Ok(())
                    }
                }
            })
            .collect();

        Some(quote! {
            #(#gen_sub_opt_fields,)*
        })
    }
}


fn generate_vec_opt_fields(vec_opt_fields: &mut FieldTypes<OptionField>) -> Option<TokenStream> {
    let vec_field_names = vec_opt_fields.fields.clone();
    let vec_field_types = vec_opt_fields.tys.clone();

    if vec_field_names.is_empty() {
        None
    } else {
        Some(quote! {
            #((stringify!(#vec_field_names), Document::Nested(elements)) => {
                elements.iter().try_for_each(|element| -> Result<()> {
                    if let Document::Element(_, doc, _) = element {
                        let mut nested_field = #vec_field_types::default();
                        if let Document::Nested(inner_elements) = doc.as_ref() {
                            inner_elements.iter().try_for_each(|inner_element| -> Result<()> {
                                if let Document::Element(tag, content, _) = inner_element {
                                    nested_field.update_field(tag, content)?;
                                }
                                Ok(())
                            })?;
                            self.#vec_field_names
                                .get_or_insert_with(Vec::new)
                                .push(nested_field);
                            return Ok(());
                        } else {
                            return Err(format!("Content is missing in {:?}", stringify!(#vec_field_names)).into());
                        }
                    }
                    Ok(())
                })?;
                Ok(())
            },)*
        })
    }
}
fn generate_vec_opt_sub_fields(vec_opt_sub_fields: &FieldTypes<SubField>) -> Option<TokenStream> {
    if vec_opt_sub_fields.fields.is_empty() {
        None
    } else {
        let gen_sub_opt_fields: Vec<TokenStream> = vec_opt_sub_fields
            .fields
            .iter()
            .zip(vec_opt_sub_fields.tys.iter())
            .map(|(field_name, field_type)| {
                quote! {
                    (stringify!(#field_name), Document::Nested(elements)) => {
                        let vec_field = self.#field_name.get_or_insert_with(Vec::new);
                        elements.iter().try_for_each(|element| -> Result<()> {
                            if let Document::Element(_, ref inner_doc, _) = element {
                                let mut nested_field = #field_type::default(); //TODO: Check this
                                if let Document::Nested(inner_elements) = inner_doc.as_ref() { 
                                    inner_elements.iter().try_for_each(
                                        |inner_element| -> Result<()> {
                                            if let Document::Element(tag, content, _) = inner_element { 
                                                nested_field.update_field(tag, content)?; 
                                            }
                                            Ok(())
                                        },
                                    )?;
                                    vec_field.push(nested_field); 
                                } else {
                                    return Err("Content is missing in Author authors".into()); 
                                }
                            }
                            Ok(())
                        },
                    )?;
                    Ok(())
                }
                }
            })
            .collect();

        Some(quote! {
            #(#gen_sub_opt_fields,)*
        })
    }
}

pub struct FieldParameters<'a> {
    pub struct_name: &'a Ident,
    pub attributed_fields: &'a mut FieldTypes<Attributed>,
    pub non_attributed_fields: &'a mut FieldTypes<NonAttributed>,
    pub sub_fields: &'a mut FieldTypes<SubField>,
    pub vec_sub_fields: &'a mut FieldTypes<SubField>,
    pub sub_opt_fields: &'a mut FieldTypes<OptionField>,
    pub vec_fields: &'a mut FieldTypes<VecField>,
    pub vec_opt_fields: &'a mut FieldTypes<OptionField>,
    pub vec_opt_sub_fields: &'a mut FieldTypes<SubField>,
    pub attributed_opt_fields: &'a mut FieldTypes<OptionField>,
    pub non_attributed_opt_fields: &'a mut FieldTypes<OptionField>,
    pub std_types: &'a HashSet<Ident>,
}

pub fn generate_update_fields(
    field_parameters: &mut FieldParameters,
) -> Result<TokenStream, syn::Error> {
    let FieldParameters {
        struct_name,
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        vec_sub_fields,
        sub_opt_fields,
        vec_fields,
        vec_opt_fields,
        vec_opt_sub_fields,
        attributed_opt_fields,
        non_attributed_opt_fields,
        std_types,
        
    } = field_parameters;
    let gen_attributed_fields = generate_attributed_fields(attributed_fields);
    let gen_attributed_opt_fields = generate_attribute_opt_fields(attributed_opt_fields);

    let gen_non_attributed_fields =
        generate_non_attributed_fields(non_attributed_fields, std_types)?;
    let gen_non_attributed_opt_fields =
        generate_non_attributed_opt_fields(non_attributed_opt_fields, std_types)?;

    let gen_sub_fields = generate_sub_fields(sub_fields);
    let gen_sub_opt_fields = generate_sub_opt_fields(sub_opt_fields);

    let gen_vec_fields = generate_vec_fields(vec_fields);
    let gen_vec_sub_fields = generate_vec_sub_fields(vec_sub_fields);
    let gen_vec_opt_fields = generate_vec_opt_fields(vec_opt_fields);
    let gen_vec_opt_sub_fields = generate_vec_opt_sub_fields(vec_opt_sub_fields);

    let arms: Vec<TokenStream> = vec![
        gen_non_attributed_fields,
        gen_non_attributed_opt_fields,
        gen_vec_fields,
        gen_vec_sub_fields,
        gen_vec_opt_fields,
        gen_vec_opt_sub_fields,
        gen_sub_fields,
        gen_sub_opt_fields,
    ]
    .into_iter()
    .flatten()
    .collect();

    let attribute_arms: Vec<TokenStream> = vec![gen_attributed_fields, gen_attributed_opt_fields]
        .into_iter()
        .flatten()
        .collect();

    let gen_impl = if !attribute_arms.is_empty() {
        quote! {
            impl UpdateField for #struct_name {
                fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<()> {
                    #(#attribute_arms)*
                    match (tag.name.local_part.as_str(), doc) {
                        #(#arms)*

                        _ => Err(format!("Content is missing or unknown field `{}` in {}", tag.name.local_part.as_str(),stringify!(#struct_name)).into()),
                    }
                }
            }
        }
    } else {
        quote! {
            impl UpdateField for #struct_name {
                fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<()> {
                    match (tag.name.local_part.as_str(), doc) {
                        #(#arms)*
                        _ => Err(format!("Content is missing or unknown field `{}` in {}", tag.name.local_part.as_str(),stringify!(#struct_name)).into()),
                    }
                }
            }
        }
    };

    if !attribute_arms.is_empty() || !arms.is_empty() {
        Ok(gen_impl)
    } else {
        Err(syn::Error::new(
            Span::call_site(),
            "Error generating update fields",
        ))
    }
}
