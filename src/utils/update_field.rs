use std::collections::HashSet;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use super::fields_extraction::{
    Attributed, AttributedOption, FieldTypes, NonAttributed, NonAttributedOptionField, OptionField, OptionVecSubField, SubField, SubOptionField, VecField, VecOptionField, VecOptionSubField, VecSubField
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

impl GenerateFields for FieldTypes<Attributed> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_attributed_fields: Vec<TokenStream> = self
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

            Ok(Some(quote! {
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
            }))
        }
    }
}

impl GenerateFields for FieldTypes<AttributedOption> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_attributed_opt_fields: Vec<TokenStream> = self
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
    
            Ok(Some(quote! {
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
            }))
        }
    }
}

impl GenerateFields for FieldTypes<SubField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_sub_fields: Vec<TokenStream> = self
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
    
            Ok(Some(quote! {
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
            }))
        }
    }
}

impl GenerateFields for FieldTypes<SubOptionField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let gen_sub_opt_fields: Vec<TokenStream> = self
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
    
            Ok(Some(quote! {
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
            }))
        }
    }
}

impl GenerateFields for FieldTypes<NonAttributed> {
    fn generate_fields(
        &self,
    ) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    
                        match field_type.to_string().as_str() {
                            "String" => Ok(quote! {
                                (stringify!(#field_name), Document::Content(Some(value))) => {
                                    self.#field_name = value.to_string();
                                    Ok(())
                                }
                            }),
                            _ => {
                                if is_numeric_type(field_type) {
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
                            }
                        }
                    }
                )
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

impl GenerateFields for FieldTypes<NonAttributedOptionField> {
    fn generate_fields(
        &self,
    ) -> Result<Option<TokenStream>, syn::Error> {
        if self.fields.is_empty() {
            Ok(None)
        } else {
            let fields: Vec<TokenStream> = self
                .fields
                .iter()
                .zip(self.tys.iter())
                .map(|(field_name, field_type)| {
                    match field_type.to_string().as_str() {
                        "String" => Ok(quote! {
                            (stringify!(#field_name), Document::Content(Some(value))) => {
                                self.#field_name = Some(value.to_string());
                                Ok(())
                            }
                        }),
                        _ => {
                            if is_numeric_type(field_type) {
                                Ok(quote! {
                                    (stringify!(#field_name), Document::Content(Some(value))) => {
                                        self.#field_name = Some(value.parse::<#field_type>()?);
                                        Ok(())
                                    }
                                })
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Field: `{field_name:?}` with type `{field_type:?}`"),
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

impl GenerateFields for FieldTypes<VecField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error>  {
        let vec_field_names = self.fields.clone();
        let vec_field_types = self.tys.clone();
    
        if vec_field_names.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
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
            }))
        }
    }
}


impl GenerateFields for FieldTypes<VecSubField> {
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
    
            Ok(Some(quote! {
                #(#gen_sub_opt_fields,)*
            }))
        }
    }
}


impl GenerateFields for FieldTypes<VecOptionField> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error> {
        let vec_field_names = self.fields.clone();
        let vec_field_types = self.tys.clone();
    
        if vec_field_names.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
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
    pub sub_opt_fields: FieldTypes<SubOptionField>,
    pub vec_fields: FieldTypes<VecField>,
    pub vec_opt_fields: FieldTypes<VecOptionField>,
    pub vec_opt_sub_fields: FieldTypes<VecOptionSubField>,
    pub opt_vec_sub_fields: FieldTypes<OptionVecSubField>,
    
    
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
            sub_opt_fields: FieldTypes::<SubOptionField>::default(),
            vec_fields: FieldTypes::<VecField>::default(),
            vec_sub_fields: FieldTypes::<VecSubField>::default(),
            opt_vec_sub_fields: FieldTypes::<OptionVecSubField>::default(),
            vec_opt_fields: FieldTypes::<VecOptionField>::default(),
            vec_opt_sub_fields: FieldTypes::<VecOptionSubField>::default(),
            
            
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
            sub_opt_fields: &mut self.sub_opt_fields,
            vec_fields: &mut self.vec_fields,
            vec_sub_fields: &mut self.vec_sub_fields,
            opt_vec_sub_fields: &mut self.opt_vec_sub_fields,
            vec_opt_fields: &mut self.vec_opt_fields,
            vec_opt_sub_fields: &mut self.vec_opt_sub_fields,
            
            std_types: self.std_types,
            field_ident: &None,
            field: None,
            attrs: None,

        }
    }

    
    
}
pub struct FieldsContextRefs<'a> {
    pub struct_name: &'a Ident,
    pub attributed_fields: &'a mut FieldTypes<Attributed>,
    pub attributed_opt_fields: &'a mut FieldTypes<AttributedOption>,
    pub non_attributed_fields: &'a mut FieldTypes<NonAttributed>,
    pub non_attributed_opt_fields: &'a mut FieldTypes<NonAttributedOptionField>,
    pub sub_fields: &'a mut FieldTypes<SubField>,
    pub vec_sub_fields: &'a mut FieldTypes<VecSubField>,
    pub sub_opt_fields: &'a mut FieldTypes<SubOptionField>,
    pub vec_fields: &'a mut FieldTypes<VecField>,
    pub vec_opt_fields: &'a mut FieldTypes<VecOptionField>,
    pub vec_opt_sub_fields: &'a mut FieldTypes<VecOptionSubField>,
    pub opt_vec_sub_fields: &'a mut FieldTypes<OptionVecSubField>,
    
    
    pub std_types: &'a HashSet<Ident>,
    pub field_ident: &'a Option<Ident>,
    pub field:  Option<&'a syn::Field>,
    pub attrs: Option<&'a [syn::Attribute]>,
}

impl<'a> FieldsContextRefs<'a> {
    
    pub fn generate_update_fields(
        &'a mut self,
    ) -> Result<TokenStream, syn::Error> {
        let struct_name = self.struct_name;
        let gen_attributed_fields = self.attributed_fields.generate_fields()?;
        let gen_attributed_opt_fields = self.attributed_opt_fields.generate_fields()?;

        let gen_non_attributed_fields =
            generate_non_attributed_fields(self.non_attributed_fields, self.std_types)?;
        let gen_non_attributed_opt_fields = 
        self.non_attributed_opt_fields.generate_fields()?;

        let gen_sub_fields = self.sub_fields.generate_fields()?;
        let gen_sub_opt_fields = self.sub_opt_fields.generate_fields()?;

        let gen_vec_fields = self.vec_fields.generate_fields()?;
        let gen_vec_sub_fields = self.vec_sub_fields.generate_fields()?;
        let gen_vec_opt_fields = self.vec_opt_fields.generate_fields()?;
        let gen_vec_opt_sub_fields = self.vec_opt_sub_fields.generate_fields()?;

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
}