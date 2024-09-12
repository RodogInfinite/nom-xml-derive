use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use super::fields_extraction::{
    Attributed, AttributedOption, FieldTypes, NonAttributed, NonAttributedOptionField,
    OptionSubField, OptionVecField, OptionVecOptionField, OptionVecOptionSubField,
    OptionVecSubField, SubField, VecField, VecOptionField, VecOptionSubField, VecSubField,
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

pub trait GenerateFields {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error>;
}
pub trait Getter<State> {
    // fn fields(&self) -> &Vec<Ident>;
    // fn tys(&self) -> &Vec<Ident>;
    fn replacements(&self) -> &HashMap<Ident, String>;
    // fn state(&self) -> TypeId;
}

impl<State: 'static> Getter<State> for FieldTypes<State> {
    // fn fields(&self) -> &Vec<Ident> {
    //     &self.fields
    // }
    // fn tys(&self) -> &Vec<Ident> {
    //     &self.tys
    // }

    fn replacements(&self) -> &HashMap<Ident, String> {
        &self.replacements
    }

    // fn state(&self) -> TypeId {
    //     TypeId::of::<State>()
    // }
}

impl GenerateFields for FieldTypes<Attributed> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .map(|field_name| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    quote! {
                        #field => {
                            self.#field_name = attr_val.to_string();
                            Ok(())
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {
             #(#gen_fields,)*
            }))
        }
    }
}

impl GenerateFields for FieldTypes<AttributedOption> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .map(|field_name| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    quote! {
                        #field => {
                            self.#field_name = Some(attr_val.to_string());
                            Ok(())
                        }
                    }
                })
                .collect();
            Ok(Some(quote! {
             #(#gen_fields,)*
            }))
        }
    }
}

impl GenerateFields for FieldTypes<NonAttributed> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (#field, Document::Content(Some(value))) => {
                                self.#field_name = value.to_string();
                                Ok::<(),Box<dyn std::error::Error>>(())
                            }
                        }),
                        _ => {
                            if is_numeric_type(field_type) {
                                Ok(quote! {
                                    (#field, Document::Content(Some(value))) => {
                                        self.#field_name = value.parse::<#field_type>()?;
                                        Ok::<(),Box<dyn std::error::Error>>(())
                                    }
                                })
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Field: `{field_name:?}`"),
                                ))
                            }
                        }
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
}

impl GenerateFields for FieldTypes<NonAttributedOptionField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (#field, Document::Content(Some(value))) => {
                                self.#field_name = Some(value.to_string());
                                Ok::<(),Box<dyn std::error::Error>>(())
                            }
                        }),
                        _ => {
                            if is_numeric_type(field_type) {
                                Ok(quote! {
                                    (#field, Document::Content(Some(value))) => {
                                        self.#field_name = Some(value.parse::<#field_type>()?);
                                        Ok::<(),Box<dyn std::error::Error>>(())
                                    }
                                })
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Field: `{field_name:?}`"),
                                ))
                            }
                        }
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
}

impl GenerateFields for FieldTypes<SubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .map(|field_name| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    quote! {
                        (#field, Document::Nested(_)) => {
                            self.#field_name.update_fields(&doc)
                        }
                        (#field, Document::Empty) => {
                            if let Some(attributes) = &tag.attributes {
                                self.#field_name.update_attribute_fields(tag)
                            } else {
                                return Err(format!("No attribute found for EmptyTag found for `{:#?}`", stringify!(field)).into());
                             }
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {#(#gen_fields)*}))
        }
    }
}

impl GenerateFields for FieldTypes<OptionSubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .map(|field_name| {
                    let field = self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    quote! {
                        (#field, Document::Nested(_)) => {
                            self.#field_name.update_fields(&doc)
                        }

                        (#field, Document::Empty) => {
                            if let Some(attributes) = &tag.attributes {
                                self.#field_name.update_attribute_fields(tag)?;
                            }
                            self.#field_name.update_fields(&doc)
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {#(#gen_fields)*}))
        }
    }
}

impl GenerateFields for FieldTypes<VecField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        let gen_fields: Vec<TokenStream> = self
            .fields
            .iter()
            .zip(self.tys.iter())
            .map(|(field_name, field_type)| {
                let field = self
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) });

                match field_type.to_string().as_str() {
                    "String" => Ok(quote! {
                        (#field, Document::Content(Some(value))) => {
                            self.#field_name.push(value.to_string());
                            return Ok(());
                        }
                    }),
                    _ => {
                        if is_numeric_type(field_type) {
                            Ok(quote! {
                                (#field, Document::Content(Some(value))) => {
                                    self.#field_name.push(value.parse::<#field_type>()?);
                                    return Ok(());
                                }
                            })
                        } else {
                            Err(syn::Error::new(
                                Span::call_site(),
                                format!("Unknown Field: `{field_name:?}`"),
                            ))
                        }
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        if self.fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                (_, Document::Nested(elements)) => {
                    elements.iter().try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                        if let Document::Element(tag, doc, _) = element {
                            match (tag.name.local_part.as_str(),doc.as_ref()){
                                #(#gen_fields,)*
                            _=> {
                                return Err(format!("Unknown field: {:#?}", tag).into());

                            }
                        }}
                        Ok(())
                    })?;
                    Ok::<(),Box<dyn std::error::Error>>(())
                }
            }))
        }
    }
}

impl GenerateFields for FieldTypes<VecSubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_opt_sub_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    quote! {
                        (stringify!(#field_name), Document::Nested(elements)) => {
                            elements
                            .iter()
                            .try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                                let mut nested_field = #field_type::default();
                                if let Document::Element(tag, content, _) = element {
                                    nested_field.update_attribute_fields(tag)?;

                                    if let Document::Nested(inner_elements) = content.as_ref() {
                                        inner_elements.iter().try_for_each(
                                            |inner_element| -> Result<(),Box<dyn std::error::Error>> {
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
                            Ok::<(),Box<dyn std::error::Error>>(())
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {
                #(#gen_opt_sub_fields,)*
            }))
        }
    }
}

impl GenerateFields for FieldTypes<VecOptionField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        let gen_fields: Vec<TokenStream> = self
            .fields
            .iter()
            .zip(self.tys.iter())
            .map(|(field_name, field_type)| {
                let field = self
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) });

                match field_type.to_string().as_str() {
                    "String" => Ok(quote! {
                        (#field, Document::Content(Some(value))) => {
                            self.#field_name.push(Some(value.to_string()));
                            return Ok(());
                        }
                    }),
                    _ => {
                        if is_numeric_type(field_type) {
                            Ok(quote! {
                                (#field, Document::Content(Some(value))) => {
                                    self.#field_name.push(Some(value.parse::<#field_type>()?));
                                    return Ok(());
                                }
                            })
                        } else {
                            Err(syn::Error::new(
                                Span::call_site(),
                                format!("Unknown Field: `{field_name:?}`"),
                            ))
                        }
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        if self.fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                (_, Document::Nested(elements)) => {
                    elements.iter().try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                        if let Document::Element(tag, doc, _) = element {
                            match (tag.name.local_part.as_str(),doc.as_ref()){
                                #(#gen_fields,)*
                            _=> {
                                return Err(format!("Unknown field: {:#?}", tag).into());

                            }
                        }}
                        Ok(())
                    })?;
                    Ok::<(),Box<dyn std::error::Error>>(())
                }
            }))
        }
    }
}

impl GenerateFields for FieldTypes<OptionVecField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        let gen_fields: Vec<TokenStream> = self
            .fields
            .iter()
            .zip(self.tys.iter())
            .map(|(field_name, field_type)| {
                let field = self
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (#field, Document::Content(Some(value))) => {
                                self.#field_name.get_or_insert_with(Vec::new).push(value.to_string());
                                return Ok(());
                            }
                        }),
                        _ => {
                            if is_numeric_type(field_type) {
                                Ok(quote! {
                                    (#field, Document::Content(Some(value))) => {
                                        self.#field_name.get_or_insert_with(Vec::new).push(value.parse::<#field_type>()?);
                                        return Ok(());
                                    }
                                })
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Field: `{field_name:?}`"),
                                ))
                            }
                        }
                    }
            })
            .collect::<Result<Vec<_>, _>>()?;

        if self.fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                (_, Document::Nested(elements)) => {
                    elements.iter().try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                        if let Document::Element(tag, doc, _) = element {
                            match (tag.name.local_part.as_str(),doc.as_ref()){
                                #(#gen_fields,)*
                            _=> {
                                return Err(format!("Unknown field: {:#?}", tag).into());

                            }
                        }}
                        Ok(())
                    })?;
                    Ok::<(),Box<dyn std::error::Error>>(())
                }
            }))
        }
    }
}

//TODO: FIX THIS
impl GenerateFields for FieldTypes<OptionVecOptionField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        let gen_fields: Vec<TokenStream> = self
            .fields
            .iter()
            .zip(self.tys.iter())
            .map(|(field_name, field_type)| {
                let field = self
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) });

                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (#field, Document::Content(Some(value))) => {
                                self.#field_name.get_or_insert_with(Vec::new).push(Some(value.to_string()));
                                return Ok(());
                            }
                        }),
                        _ => {
                            if is_numeric_type(field_type) {
                                Ok(quote! {
                                    (#field, Document::Content(Some(value))) => {
                                        self.#field_name.get_or_insert_with(Vec::new).push(Some(value.parse::<#field_type>()?));
                                        return Ok(());
                                    }
                                })
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Field: `{field_name:?}`"),
                                ))
                            }
                        }
                    }
            })
            .collect::<Result<Vec<_>, _>>()?;

        if self.fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                (_, Document::Nested(elements)) => {
                    elements.iter().try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                        if let Document::Element(tag, doc, _) = element {
                            match (tag.name.local_part.as_str(),doc.as_ref()){
                                #(#gen_fields,)*
                            _=> {
                                return Err(format!("Unknown field: {:#?}", tag).into());

                            }
                        }}
                        Ok(())
                    })?;
                    Ok::<(),Box<dyn std::error::Error>>(())
                }
            }))
        }
    }
}

impl GenerateFields for FieldTypes<VecOptionSubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_sub_opt_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    quote! {
                        (stringify!(#field_name), Document::Nested(elements)) => {
                            elements
                            .iter()
                            .try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                                let mut nested_field = #field_type::default();
                                if let Document::Element(tag, content, _) = element {
                                    nested_field.update_attribute_fields(tag)?;
                                    if let Document::Nested(inner_elements) = content.as_ref() {
                                        inner_elements.iter().try_for_each(
                                            |inner_element| -> Result<(),Box<dyn std::error::Error>> {
                                                if let Document::Element(inner_tag, inner_content, _) = inner_element {
                                                    nested_field.update_field(inner_tag, inner_content)?;
                                                }
                                                Ok(())
                                            },
                                        ).or_else(|_| {
                                            nested_field.update_field(tag, content)
                                        })?;
                                        self.#field_name.push(Some(nested_field.clone()));
                                    } else {
                                        if let Some(_) = &tag.attributes {
                                            nested_field.update_attribute_fields(tag)?;
                                            self.#field_name.push(Some(nested_field.clone()));
                                        } else {
                                            self.#field_name.push(None);

                                        }
                                    }
                                }
                                Ok(())
                            })?;
                            Ok::<(),Box<dyn std::error::Error>>(())
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {
                #(#gen_sub_opt_fields,)*
            }))
        }
    }
}

impl GenerateFields for FieldTypes<OptionVecSubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_sub_opt_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    quote! {
                        (stringify!(#field_name), Document::Nested(elements)) => {
                            elements
                            .iter()
                            .try_for_each(|element| -> Result<(),Box<dyn std::error::Error>> {
                                let mut nested_field = #field_type::default();
                                if let Document::Element(tag, content, _) = element {
                                    nested_field.update_attribute_fields(tag)?;
                                    if let Document::Nested(inner_elements) = content.as_ref() {
                                        inner_elements.iter().try_for_each(
                                            |inner_element| -> Result<(),Box<dyn std::error::Error>> {
                                                if let Document::Element(inner_tag, inner_content, _) = inner_element {
                                                    nested_field.update_field(inner_tag, inner_content)?;
                                                }
                                                Ok(())
                                            },
                                        ).or_else(|_| {
                                            nested_field.update_field(tag, content)
                                        })?;
                                        self.#field_name.get_or_insert_with(Vec::new).push(nested_field);
                                    } else if tag.attributes.is_some() {
                                            nested_field.update_attribute_fields(tag)?;
                                            self.#field_name.get_or_insert_with(Vec::new).push(nested_field.clone());
                                        } else {
                                            self.#field_name.get_or_insert_with(Vec::new).push(nested_field.clone());
                                        }

                                }
                                Ok(())
                            })?;
                            Ok::<(),Box<dyn std::error::Error>>(())
                        }
                    }
                })
                .collect();

            Ok(Some(quote! {
                #(#gen_sub_opt_fields,)*
            }))
        }
    }
}

impl GenerateFields for FieldTypes<OptionVecOptionSubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_sub_opt_fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    let field = self
                            .replacements()
                            .get(field_name)
                            .map(|replacement| quote! { #replacement })
                            .unwrap_or_else(|| quote! { stringify!(#field_name) });
                    quote! {
                        (#field, Document::Nested(elements)) => {
                            let mut nested_field = #field_type::default();
                            let mut has_nested_elements = false;
                            elements.iter().try_for_each(|element| -> Result<(), Box<dyn std::error::Error>> {
                                match element {
                                    Document::Element(tag, content, _) =>{
                                        nested_field.update_attribute_fields(tag)?;
                                        match content.as_ref() {
                                            Document::Nested(_) => {
                                                has_nested_elements = true;
                                                nested_field.update_fields(content)?;
                                            }
                                            Document::Content(Some(_)) => {
                                                nested_field.update_field(tag, content)?;
                                            }
                                            Document::Empty => {

                                                if let Some(_) = tag.attributes.as_ref() {
                                                    nested_field.update_attribute_fields(tag)?;
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                    Document::Empty => {

                                        self.#field_name.get_or_insert_with(Vec::new).push(None);
                                    }
                                    _ => {}
                                }

                                if has_nested_elements {
                                    if nested_field == #field_type::default() {
                                        self.#field_name.get_or_insert_with(Vec::new).push(None);
                                    }
                                    else {
                                        self.#field_name.get_or_insert_with(Vec::new).push(Some(nested_field.clone()));
                                        nested_field = #field_type::default();
                                    }
                                }
                                Ok::<(),Box<dyn std::error::Error>>(())
                            })?;
                            if !has_nested_elements && nested_field != #field_type::default() { self.#field_name.get_or_insert_with(Vec::new).push(Some(nested_field.clone()));}
                            Ok::<(),Box<dyn std::error::Error>>(())
                        }
                        (#field, Document::Empty) => {self.#field_name.get_or_insert_with(Vec::new).push(None); Ok(())}
                    }
                    })
                .collect();

            Ok(Some(quote! {
                #(#gen_sub_opt_fields,)*
            }))
        }
    }
}

pub struct FieldsContext<'a> {
    pub struct_name: &'a Ident,
    pub attributed_fields: FieldTypes<Attributed>,
    pub attributed_opt_fields: FieldTypes<AttributedOption>,
    pub non_attributed_fields: FieldTypes<NonAttributed>,
    pub non_attributed_opt_fields: FieldTypes<NonAttributedOptionField>,
    pub sub_fields: FieldTypes<SubField>,
    pub vec_sub_fields: FieldTypes<VecSubField>,
    pub opt_sub_fields: FieldTypes<OptionSubField>,
    pub vec_fields: FieldTypes<VecField>,
    pub opt_vec_fields: FieldTypes<OptionVecField>,
    pub opt_vec_opt_fields: FieldTypes<OptionVecOptionField>,
    pub vec_opt_fields: FieldTypes<VecOptionField>,
    pub vec_opt_sub_fields: FieldTypes<VecOptionSubField>,
    pub opt_vec_sub_fields: FieldTypes<OptionVecSubField>,
    pub opt_vec_opt_sub_fields: FieldTypes<OptionVecOptionSubField>,
    pub std_types: &'a HashSet<Ident>,
}

impl<'a> FieldsContext<'a> {
    pub fn new(struct_name: &'a Ident, std_types: &'a HashSet<Ident>) -> Self {
        Self {
            struct_name,
            attributed_fields: FieldTypes::<Attributed>::default(),
            attributed_opt_fields: FieldTypes::<AttributedOption>::default(),
            non_attributed_fields: FieldTypes::<NonAttributed>::default(),
            non_attributed_opt_fields: FieldTypes::<NonAttributedOptionField>::default(),
            sub_fields: FieldTypes::<SubField>::default(),
            opt_sub_fields: FieldTypes::<OptionSubField>::default(),
            vec_fields: FieldTypes::<VecField>::default(),
            opt_vec_fields: FieldTypes::<OptionVecField>::default(),
            opt_vec_opt_fields: FieldTypes::<OptionVecOptionField>::default(),
            vec_sub_fields: FieldTypes::<VecSubField>::default(),
            opt_vec_sub_fields: FieldTypes::<OptionVecSubField>::default(),
            vec_opt_fields: FieldTypes::<VecOptionField>::default(),
            vec_opt_sub_fields: FieldTypes::<VecOptionSubField>::default(),
            opt_vec_opt_sub_fields: FieldTypes::<OptionVecOptionSubField>::default(),

            std_types,
        }
    }

    pub fn as_mut_refs(&mut self) -> FieldsContextRefs {
        FieldsContextRefs {
            struct_name: self.struct_name,
            attributed_fields: &mut self.attributed_fields,
            attributed_opt_fields: &mut self.attributed_opt_fields,
            non_attributed_fields: &mut self.non_attributed_fields,
            non_attributed_opt_fields: &mut self.non_attributed_opt_fields,
            sub_fields: &mut self.sub_fields,
            opt_vec_fields: &mut self.opt_vec_fields,
            opt_sub_fields: &mut self.opt_sub_fields,
            vec_fields: &mut self.vec_fields,
            vec_sub_fields: &mut self.vec_sub_fields,
            opt_vec_sub_fields: &mut self.opt_vec_sub_fields,
            opt_vec_opt_fields: &mut self.opt_vec_opt_fields,
            vec_opt_fields: &mut self.vec_opt_fields,
            vec_opt_sub_fields: &mut self.vec_opt_sub_fields,
            opt_vec_opt_sub_fields: &mut self.opt_vec_opt_sub_fields,

            std_types: self.std_types,
            field_ident: &None,
            field: None,
            attrs: None,
        }
    }
}

#[derive(Debug)]
pub struct FieldsContextRefs<'a> {
    pub struct_name: &'a Ident,
    pub attributed_fields: &'a mut FieldTypes<Attributed>,
    pub attributed_opt_fields: &'a mut FieldTypes<AttributedOption>,
    pub non_attributed_fields: &'a mut FieldTypes<NonAttributed>,
    pub non_attributed_opt_fields: &'a mut FieldTypes<NonAttributedOptionField>,
    pub sub_fields: &'a mut FieldTypes<SubField>,
    pub vec_sub_fields: &'a mut FieldTypes<VecSubField>,
    pub opt_sub_fields: &'a mut FieldTypes<OptionSubField>,
    pub vec_fields: &'a mut FieldTypes<VecField>,
    pub opt_vec_fields: &'a mut FieldTypes<OptionVecField>,
    pub vec_opt_fields: &'a mut FieldTypes<VecOptionField>,
    pub opt_vec_opt_fields: &'a mut FieldTypes<OptionVecOptionField>,
    pub vec_opt_sub_fields: &'a mut FieldTypes<VecOptionSubField>,
    pub opt_vec_opt_sub_fields: &'a mut FieldTypes<OptionVecOptionSubField>,
    pub opt_vec_sub_fields: &'a mut FieldTypes<OptionVecSubField>,

    pub std_types: &'a HashSet<Ident>,
    pub field_ident: &'a Option<Ident>,
    pub field: Option<&'a syn::Field>,
    pub attrs: Option<&'a [syn::Attribute]>,
}

impl<'a> FieldsContextRefs<'a> {
    pub fn generate_update_fields(&'a mut self) -> Result<TokenStream, syn::Error> {
        let struct_name = self.struct_name;
        let gen_attributed_fields = self.attributed_fields.generate_fields()?;

        let gen_attributed_opt_fields = self.attributed_opt_fields.generate_fields()?;

        let gen_non_attributed_fields = self.non_attributed_fields.generate_fields()?;
        let gen_non_attributed_opt_fields = self.non_attributed_opt_fields.generate_fields()?;

        let gen_sub_fields = self.sub_fields.generate_fields()?;
        let gen_opt_sub_fields = self.opt_sub_fields.generate_fields()?;

        let gen_vec_fields = self.vec_fields.generate_fields()?;
        let gen_opt_vec_fields = self.opt_vec_fields.generate_fields()?;
        let gen_vec_sub_fields = self.vec_sub_fields.generate_fields()?;
        let gen_vec_opt_fields = self.vec_opt_fields.generate_fields()?;
        let gen_opt_vec_opt_fields = self.opt_vec_opt_fields.generate_fields()?;
        let gen_vec_opt_sub_fields = self.vec_opt_sub_fields.generate_fields()?;
        let gen_opt_vec_sub_fields = self.opt_vec_sub_fields.generate_fields()?;
        let gen_opt_vec_opt_sub_fields = self.opt_vec_opt_sub_fields.generate_fields()?;

        let arms: Vec<TokenStream> = vec![
            gen_non_attributed_fields,
            gen_non_attributed_opt_fields,
            gen_vec_fields,
            gen_opt_vec_fields,
            gen_vec_sub_fields,
            gen_vec_opt_fields,
            gen_opt_vec_opt_fields,
            gen_vec_opt_sub_fields,
            gen_opt_vec_sub_fields,
            gen_opt_vec_opt_sub_fields,
            gen_sub_fields,
            gen_opt_sub_fields,
        ]
        .into_iter()
        .flatten()
        .collect();

        let non_attributed_fields: Vec<TokenStream> = self
            .non_attributed_fields
            .fields
            .iter()
            .map(|field_name| {
                self.non_attributed_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let non_attributed_opt_fields: Vec<TokenStream> = self
            .non_attributed_opt_fields
            .fields
            .iter()
            .map(|field_name| {
                self.non_attributed_opt_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let sub_fields: Vec<TokenStream> = self
            .sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let opt_sub_fields: Vec<TokenStream> = self
            .opt_sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.opt_sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let opt_vec_fields: Vec<TokenStream> = self
            .opt_vec_fields
            .fields
            .iter()
            .map(|field_name| {
                self.opt_vec_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let vec_sub_fields: Vec<TokenStream> = self
            .vec_sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.vec_sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let opt_vec_sub_fields: Vec<TokenStream> = self
            .opt_vec_sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.opt_vec_sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let vec_opt_sub_fields: Vec<TokenStream> = self
            .vec_opt_sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.vec_opt_sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();
        let opt_vec_opt_sub_fields: Vec<TokenStream> = self
            .opt_vec_opt_sub_fields
            .fields
            .iter()
            .map(|field_name| {
                self.opt_vec_opt_sub_fields
                    .replacements()
                    .get(field_name)
                    .map(|replacement| quote! { #replacement })
                    .unwrap_or_else(|| quote! { stringify!(#field_name) })
            })
            .collect();

        let update_fields_quote = quote! {
            match doc {
                Document::Element(tag, nested_doc, _) => {
                    match tag.name.local_part.as_str() {
                        #(#non_attributed_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                self.update_field(empty_tag,&Document::Empty)
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#non_attributed_opt_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                self.update_field(empty_tag,&Document::Empty)
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                self.update_field(empty_tag,&Document::Empty)
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#opt_sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Option<UserDefinedStruct>")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#vec_sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Vec<UserDefinedStruct>")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#opt_vec_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Option<Vec<T>>")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#opt_vec_sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Option<Vec<UserDefinedStruct>>")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#vec_opt_sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Vec<Option<UserDefinedStruct>>")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        #(#opt_vec_opt_sub_fields => {
                            if let Document::EmptyTag(empty_tag) = nested_doc.as_ref() {
                                unimplemented!("Attribute extraction for Vec<Option<UserDefinedStruct>> ")
                            }
                            else {
                                self.update_attribute_fields(tag)?;
                                self.update_field(tag,nested_doc)
                            }
                        })*
                        _ => {
                            if let Document::Nested(_) = nested_doc.as_ref() {
                                self.update_attribute_fields(tag)?;
                                self.update_fields(nested_doc)
                            } else {
                                self.update_attribute_fields(tag)?;
                                self.update_fields(nested_doc)
                            }
                        }
                    }
                }
                Document::Nested(elements) => {
                    elements
                        .iter_with_depth(0)
                        .try_for_each(|element| {
                            if let Document::Element(_,_,_) = element {
                                self.update_fields(element)
                            }
                            else {
                               unimplemented!("Document::Nested in update_fields is not implemented here")
                            }
                        })
                }
                _ => Ok(()),

            }
        };

        let attribute_arms: Vec<TokenStream> =
            vec![gen_attributed_fields, gen_attributed_opt_fields]
                .into_iter()
                .flatten()
                .collect();

        let attribute_arms = quote! {
            if let Some(attributes) = &tag.attributes {
                attributes.iter().try_for_each(|attr| -> Result<(),Box<dyn std::error::Error>> {
                    if let Attribute::Instance {
                        name,
                        value: AttributeValue::Value(attr_val),
                    } = attr {
                        match name.local_part.as_str() {
                            #(#attribute_arms)*
                            e => Err(format!("Unknown attribute: {}\n{}", name.local_part, e).into()),
                        }
                    } else {
                        Err(format!("Unknown attribute: {:#?}", attributes).into())
                    }
                })?;
            }
            Ok(())
        };

        let gen_impl = if !attribute_arms.is_empty() && !arms.is_empty() {
            quote! {
                impl UpdateFields for #struct_name {
                    fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<(),Box<dyn std::error::Error>> {

                        match (tag.name.local_part.as_str(), doc) {
                            #(#arms)*
                            _ => Err(format!("Content is missing or unknown field `{}` in {}", tag.name.local_part.as_str(),stringify!(#struct_name)).into()),
                        }?;
                        self.update_attribute_fields(tag)
                    }

                    fn update_attribute_fields(&mut self, tag: &Tag,) -> Result<(),Box<dyn std::error::Error>> {
                        #attribute_arms
                    }

                    fn update_fields(&mut self, doc: &Document) -> Result<(), Box<dyn std::error::Error>>
                    where
                        Self: std::fmt::Debug,
                    {
                        #update_fields_quote
                    }
                }
            }
        } else if !attribute_arms.is_empty() && arms.is_empty() {
            quote! {
                impl UpdateFields for #struct_name {
                    fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<(),Box<dyn std::error::Error>> {
                        self.update_attribute_fields(tag)
                    }

                    fn update_attribute_fields(&mut self, tag: &Tag,) -> Result<(),Box<dyn std::error::Error>> {
                        #attribute_arms
                    }

                    fn update_fields(&mut self, doc: &Document) -> Result<(), Box<dyn std::error::Error>>
                    where
                    Self: std::fmt::Debug,
                    {
                        #update_fields_quote
                    }
                }
            }
        } else {
            quote! {
                impl UpdateFields for #struct_name {
                    fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<(),Box<dyn std::error::Error>> {
                        match (tag.name.local_part.as_str(), doc) {
                            #(#arms)*
                            _ => Err(format!("Content is missing or unknown field `{}` in {}", tag.name.local_part.as_str(),stringify!(#struct_name)).into()),
                        }
                    }

                    fn update_fields(&mut self, doc: &Document) -> Result<(), Box<dyn std::error::Error>>
                    where
                        Self: std::fmt::Debug,
                    {
                        #update_fields_quote
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
}
