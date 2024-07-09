use std::{
    any::TypeId,
    collections::{HashMap, HashSet},
};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use super::fields_extraction::{
    Attributed, AttributedOption, FieldTypes, NonAttributed, NonAttributedOptionField,
    OptionVecField, OptionVecOptionField, OptionVecOptionSubField, OptionVecSubField, SubField,
    SubOptionField, VecField, VecOptionField, VecOptionSubField, VecSubField,
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
pub(crate) enum SubFieldType {
    Vec,
    VecOption,
    OptionVec,
    OptionVecOption,
}
pub trait GenerateFields<State> {
    fn generate_fields(&self) -> Result<Option<TokenStream>, syn::Error>
    where
        Self: Sized + std::fmt::Debug,
    {
        if self.fields().is_empty() {
            Ok(None)
        } else {
            let gen_attributes_fields: Vec<TokenStream> = self
                .fields()
                .iter()
                .zip(self.tys().iter())
                .flat_map(
                    |(field_name, field_type)| -> Option<Result<TokenStream, syn::Error>> {
                        let field = self
                            .replacements()
                            .get(field_name)
                            .map(|replacement| quote! { #replacement })
                            .unwrap_or_else(|| quote! { stringify!(#field_name) });

                        self.generate_attribute_fields_quote(&field, field_name, field_type)
                            .transpose()
                    },
                )
                .collect::<Result<Vec<TokenStream>, syn::Error>>()?;
            let gen_fields_quote: Vec<TokenStream> = self
                .fields()
                .iter()
                .zip(self.tys().iter())
                .map(
                    |(field_name, field_type)| -> Result<TokenStream, syn::Error> {
                        let field = self
                            .replacements()
                            .get(field_name)
                            .map(|replacement| quote! { #replacement })
                            .unwrap_or_else(|| quote! { stringify!(#field_name) });

                        self.generate_fields_quote(&field, field_name, field_type)
                    },
                )
                .collect::<Result<Vec<TokenStream>, syn::Error>>()?;

            let field_names = self
                .fields()
                .iter()
                .map(|field_name| -> Result<TokenStream, syn::Error> {
                    Ok(self
                        .replacements()
                        .get(field_name)
                        .map(|replacement| quote! { #replacement })
                        .unwrap_or_else(|| quote! { stringify!(#field_name) }))
                })
                .collect::<Result<Vec<TokenStream>, syn::Error>>()?;
            let gen_fields = self.generate_return_quote(field_names, gen_fields_quote)?;

            if !gen_attributes_fields.is_empty() {
                let gen_attributes_quote =
                    self.generate_attributed_return_quote(gen_attributes_fields)?;
                Ok(Some(quote! {
                    #gen_attributes_quote
                    #gen_fields
                }))
            } else {
                Ok(Some(quote! { #gen_fields }))
            }
        }
    }

    fn generate_attribute_fields_quote(
        &self,
        field: &TokenStream,
        field_name: &Ident,
        field_type: &Ident,
    ) -> Result<Option<TokenStream>, syn::Error>
    where
        Self: std::fmt::Debug,
    {
        match self.state() {
            id if id == TypeId::of::<Attributed>() => match field_type.to_string().as_str() {
                "String" => Ok(Some(quote! {
                    #field => {
                        self.#field_name = attr_val.to_string();
                        Ok(())
                    }
                })),
                _ => {
                    if is_numeric_type(field_type) {
                        Ok(Some(quote! {
                            #field => {
                                self.#field_name = attr_val.parse::<#field_type>()?;
                                Ok(())
                            }
                        }))
                    } else {
                        Err(syn::Error::new(
                            Span::call_site(),
                            format!("Unknown Field: `{field_name:?}`"),
                        ))
                    }
                }
            },
            id if id == TypeId::of::<AttributedOption>() => match field_type.to_string().as_str() {
                "String" => Ok(Some(quote! {
                    #field => {
                        self.#field_name = Some(attr_val.to_string());
                        Ok(())
                    }
                })),
                _ => {
                    if is_numeric_type(field_type) {
                        Ok(Some(quote! {
                            #field=> {
                                self.#field_name = Some(attr_val.parse::<#field_type>()?);
                                Ok(())
                            }
                        }))
                    } else {
                        Err(syn::Error::new(
                            Span::call_site(),
                            format!("Unknown Field: `{field_name:?}` with type `{field_type:?}`"),
                        ))
                    }
                }
            },
            _ => Ok(None),
        }
    }

    fn generate_fields_quote(
        &self,
        field: &TokenStream,
        field_name: &Ident,
        field_type: &Ident,
    ) -> Result<TokenStream, syn::Error> {
        match self.state() {
            id if id == TypeId::of::<SubField>() => Ok(quote! {
                self.#field_name.update_fields(&doc)
            }),
            id if id == TypeId::of::<SubOptionField>() => Ok(quote! {
                self.#field_name.update_fields(&doc)
            }),
            id if id == TypeId::of::<NonAttributed>() => match field_type.to_string().as_str() {
                "String" => Ok(quote! {
                    (#field, Document::Content(Some(value))) => {
                        self.#field_name = value.to_string();
                        Ok(())
                    }
                }),
                _ => {
                    if is_numeric_type(field_type) {
                        Ok(quote! {
                            (#field, Document::Content(Some(value))) => {
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
            },
            id if id == TypeId::of::<NonAttributedOptionField>() => {
                match field_type.to_string().as_str() {
                    "String" => Ok(quote! {
                        (#field, Document::Content(Some(value))) => {
                            self.#field_name = Some(value.to_string());
                            Ok(())
                        }
                    }),
                    _ => {
                        if is_numeric_type(field_type) {
                            Ok(quote! {
                                (#field, Document::Content(Some(value))) => {
                                    self.#field_name = Some(value.parse::<#field_type>()?);
                                    Ok(())
                                }
                            })
                        } else {
                            Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "Unknown Field: `{field_name:?}` with type `{field_type:?}`"
                                ),
                            ))
                        }
                    }
                }
            }
            id if id == TypeId::of::<VecField>() => match field_type.to_string().as_str() {
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
            },
            id if id == TypeId::of::<VecOptionField>() => match field_type.to_string().as_str() {
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
            },
            id if id == TypeId::of::<OptionVecField>() => match field_type.to_string().as_str() {
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
            },
            id if id == TypeId::of::<OptionVecOptionField>() => {
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
            }
            id if id == TypeId::of::<VecSubField>() => {
                self.generate_vec_subfield_quote(field, field_name, field_type, SubFieldType::Vec)
            }
            id if id == TypeId::of::<VecOptionSubField>() => self.generate_vec_subfield_quote(
                field,
                field_name,
                field_type,
                SubFieldType::VecOption,
            ),
            id if id == TypeId::of::<OptionVecSubField>() => self.generate_vec_subfield_quote(
                field,
                field_name,
                field_type,
                SubFieldType::OptionVec,
            ),
            id if id == TypeId::of::<OptionVecOptionSubField>() => self
                .generate_vec_subfield_quote(
                    field,
                    field_name,
                    field_type,
                    SubFieldType::OptionVecOption,
                ),

            _ => Ok(TokenStream::new()),
        }
    }

    fn generate_return_quote(
        &self,
        field_names: Vec<TokenStream>,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
        let state_id = self.state();
        match state_id {
            _ if state_id == TypeId::of::<NonAttributed>()
                || state_id == TypeId::of::<NonAttributedOptionField>() =>
            {
                self.generate_non_attributed_return_quote(gen_fields)
            }
            _ if state_id == TypeId::of::<SubField>()
                || state_id == TypeId::of::<SubOptionField>() =>
            {
                self.generate_subfield_return_quote(field_names, gen_fields)
            }
            _ if state_id == TypeId::of::<VecField>()
                || state_id == TypeId::of::<VecOptionField>()
                || state_id == TypeId::of::<OptionVecOptionField>()
                || state_id == TypeId::of::<OptionVecField>() =>
            {
                self.generate_vec_return_quote(gen_fields)
            }
            _ if state_id == TypeId::of::<VecSubField>()
                || state_id == TypeId::of::<VecOptionSubField>()
                || state_id == TypeId::of::<OptionVecSubField>()
                || state_id == TypeId::of::<OptionVecOptionSubField>() =>
            {
                self.generate_vec_subfield_return_quote(gen_fields)
            }
            _ => Ok(None),
        }
    }

    fn generate_attributed_return_quote(
        &self,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
        Ok(Some(quote! {
            if let Some(attributes) = &tag.attributes {
                attributes.iter().try_for_each(|attr| -> Result<(),Box<dyn std::error::Error>> {
                    if let Attribute::Instance {
                        name,
                        value: AttributeValue::Value(attr_val),
                    } = attr {
                        match name.local_part.as_str() {
                            #(#gen_fields,)*
                            e => Err(format!("Unknown attribute: {}\n{}", name.local_part, e).into()),
                        }
                    } else {
                        Err(format!("Unknown attribute: {:#?}", attributes).into())
                    }
                })?;
            }
        }))
    }

    fn generate_non_attributed_return_quote(
        &self,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
        if gen_fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! {
                #(#gen_fields)*
            }))
        }
    }

    fn generate_subfield_return_quote(
        &self,
        field_names: Vec<TokenStream>,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
        Ok(Some(quote! {
            #((#field_names, Document::Nested(_)) => {
                #gen_fields
            })*

        }))
    }

    fn generate_vec_return_quote(
        &self,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
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
                Ok(())
            }
        }))
    }

    fn generate_vec_subfield_return_quote(
        &self,
        gen_fields: Vec<TokenStream>,
    ) -> Result<Option<TokenStream>, syn::Error> {
        Ok(Some(quote! {
            #(#gen_fields,)*
        }))
    }

    fn generate_vec_subfield_quote(
        &self,
        field: &TokenStream,
        field_name: &Ident,
        field_type: &Ident,
        subfield_type: SubFieldType,
    ) -> Result<TokenStream, syn::Error> {
        let (
            optional_doc_empty_outer_quote,
            optional_doc_empty_inner_quote,
            push_quote,
            handle_empty_quote,
        ) = match subfield_type {
            SubFieldType::Vec => (
                quote! {
                    (#field, Document::Empty) => {
                    return Err(
                        format!(
                            "Document::Empty found for `{field_name}`. You may want to change `{field_name}: Vec<{field_type}>` to `{field_name}: Vec<Option<{field_type}>>` to handle missing data",
                            field_name = stringify!(#field_name),
                            field_type = stringify!(#field_type)
                        ).into()
                    );
                }},
                quote! {unimplemented!("Document::Empty for Vec<field_type> is not yet implemented")},
                quote! { self.#field_name.push(nested_field.clone());},
                quote! {},
            ),
            SubFieldType::VecOption => (
                quote! { (#field, Document::Empty) => {self.#field_name.push(None); Ok(()) }},
                quote! { if let Some(_) = tag.attributes.as_ref() {
                    nested_field.update_attribute_fields(tag)?;
                } else{ self.#field_name.push(None);} },
                quote! { self.#field_name.push(Some(nested_field.clone())); },
                quote! {},
            ),
            SubFieldType::OptionVec => (
                quote! {},
                quote! {
                    if let Some(_) = tag.attributes.as_ref() {
                        nested_field.update_attribute_fields(tag)?;
                        self.#field_name.get_or_insert_with(Vec::new).push(nested_field.clone());
                    }
                },
                quote! { self.#field_name.get_or_insert_with(Vec::new).push(nested_field.clone()); },
                quote! { self.#field_name.get_or_insert_with(Vec::new).push(nested_field.clone()); },
            ),
            SubFieldType::OptionVecOption => (
                quote! {(#field, Document::Empty) => {self.#field_name.get_or_insert_with(Vec::new).push(None); Ok(())}},
                quote! {
                    if let Some(_) = tag.attributes.as_ref() {
                        nested_field.update_attribute_fields(tag)?;
                    }
                },
                quote! { self.#field_name.get_or_insert_with(Vec::new).push(Some(nested_field.clone())); },
                quote! { self.#field_name.get_or_insert_with(Vec::new).push(None); },
            ),
        };

        Ok(quote! {
            (#field, Document::Nested(elements)) => {
                let mut nested_field = #field_type::default();
                let mut has_nested_elements = false;

                elements.iter().try_for_each(|element| -> Result<(), Box<dyn std::error::Error>> {
                    match element {
                        Document::Element(tag, content, _) =>{
                            nested_field.update_attribute_fields(tag)?;
                            match content.as_ref() {
                                Document::Nested(inner_elements) => {
                                    has_nested_elements = true;
                                    nested_field.update_fields(content)?;
                                }
                                Document::Content(Some(_)) => {
                                    nested_field.update_field(tag, content)?;
                                }
                                Document::Empty => {
                                    #optional_doc_empty_inner_quote
                                }
                                _ => {}
                            }
                        }
                        Document::Empty => {
                            #handle_empty_quote
                        }
                        _ => {}
                    }

                    if has_nested_elements {
                        if nested_field == #field_type::default() {
                            #handle_empty_quote
                        }
                        else {
                            #push_quote
                            nested_field = #field_type::default();
                        }
                    }
                    Ok(())
                })?;
                if !has_nested_elements && nested_field != #field_type::default() { #push_quote}
                Ok(())
            }
            #optional_doc_empty_outer_quote

        })
    }

    fn fields(&self) -> &Vec<Ident>;
    fn tys(&self) -> &Vec<Ident>;
    fn replacements(&self) -> &HashMap<Ident, String>;
    fn state(&self) -> TypeId;
}

impl<State: 'static> GenerateFields<State> for FieldTypes<State> {
    fn fields(&self) -> &Vec<Ident> {
        &self.fields
    }
    fn tys(&self) -> &Vec<Ident> {
        &self.tys
    }

    fn replacements(&self) -> &HashMap<Ident, String> {
        &self.replacements
    }

    fn state(&self) -> TypeId {
        TypeId::of::<State>()
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
            sub_opt_fields: FieldTypes::<SubOptionField>::default(),
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
            sub_opt_fields: &mut self.sub_opt_fields,
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
    pub sub_opt_fields: &'a mut FieldTypes<SubOptionField>,
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
        let gen_sub_opt_fields = self.sub_opt_fields.generate_fields()?;

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
            gen_sub_opt_fields,
        ]
        .into_iter()
        .flatten()
        .collect();

        let attribute_arms: Vec<TokenStream> =
            vec![gen_attributed_fields, gen_attributed_opt_fields]
                .into_iter()
                .flatten()
                .collect();

        let gen_impl = if !attribute_arms.is_empty() {
            quote! {
                impl UpdateFields for #struct_name {
                    fn update_field(&mut self, tag: &Tag, doc: &Document) -> Result<(),Box<dyn std::error::Error>> {
                        #(#attribute_arms)*
                        match (tag.name.local_part.as_str(), doc) {
                            #(#arms)*
                            _ => Err(format!("Content is missing or unknown field `{}` in {}", tag.name.local_part.as_str(),stringify!(#struct_name)).into()),
                        }
                    }
                    fn update_attribute_fields(&mut self, tag: &Tag,) -> Result<(),Box<dyn std::error::Error>> {
                        #(#attribute_arms)*
                        Ok(())
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
