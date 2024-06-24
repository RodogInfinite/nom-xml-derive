use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

use proc_macro2::{Span, TokenTree};

use syn::{
    punctuated::Punctuated, token::PathSep, AngleBracketedGenericArguments, AttrStyle, Attribute,
    DeriveInput, Field, Fields, GenericArgument, Ident, MacroDelimiter, Meta, MetaList,
    PathArguments, PathSegment,
};

use crate::utils::update_field::is_numeric_type;

use super::update_field::FieldsContextRefs;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Attributed;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AttributedOption;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NonAttributed;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NonAttributedOptionField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SubField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SubOptionField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecOptionField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OptionVecOptionSubField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecSubField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecOptionSubField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]

pub struct OptionVecField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OptionVecSubField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OptionField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FieldTypes<State> {
    pub fields: Vec<Ident>,
    pub tys: Vec<Ident>,
    pub replacements: HashMap<Ident, String>,
    state: PhantomData<State>,
}

pub trait ExtractFields {
    fn extract_arguments(
        &mut self,
        arguments: &PathArguments,
        field_ident: &Ident,
        ty_ident: &Ident,
        replacement: &Option<String>,
    ) -> Result<(), syn::Error>;
}

impl<State> ExtractFields for FieldTypes<State> {
    fn extract_arguments(
        &mut self,
        arguments: &PathArguments,
        field_ident: &Ident,
        ty_ident: &Ident,
        replacement: &Option<String>,
    ) -> Result<(), syn::Error> {
        match arguments {
            PathArguments::None => {
                self.fields.push(field_ident.clone());
                self.tys.push(ty_ident.clone());
                if let Some(replacement) = replacement {
                    self.replacements
                        .insert(field_ident.clone(), replacement.clone());
                };
                Ok(())
            }
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: _,
                lt_token: _,
                args,
                gt_token: _,
            }) => args.iter().try_for_each(|arg| {
                if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                    qself: _,
                    path:
                        syn::Path {
                            leading_colon: _,
                            segments,
                        },
                })) = arg
                {
                    segments
                        .iter()
                        .try_for_each(|PathSegment { ident, arguments }| {
                            if !arguments.is_empty() {
                                todo!("Nested segment arguments have yet to be implemented");
                            }
                            match ident.to_string().as_str() {
                                "String" | "Vec" => {
                                    self.fields.push(field_ident.clone());
                                    self.tys.push(ident.clone());
                                    if let Some(replacement) = replacement {
                                        self
                                            .replacements
                                            .insert(field_ident.clone(), replacement.clone());
                                    };
                                    Ok(())
                                }
                                _ => {
                                    if is_numeric_type(ident) {
                                        self.fields.push(field_ident.clone());
                                        self.tys.push(ident.clone());
                                        if let Some(replacement) = replacement {
                                            self
                                                .replacements
                                                .insert(field_ident.clone(), replacement.clone());
                                        };
                                        Ok(())
                                    } else {
                                        Err(syn::Error::new(
                                            Span::call_site(),
                                            format!(
                                                "Unknown Nested Segment: `{ident:?}` `{arguments:?}`"
                                            ),
                                        ))
                                    }
                                }
                            }
                        })
                } else {
                    Ok(())
                }
            }),
            PathArguments::Parenthesized(_) => Err(syn::Error::new(
                Span::call_site(),
                format!("Parenthesized arguments not yet supported `{arguments:?}`"),
            )),
        }
    }
}

impl<'a> FieldsContextRefs<'a> {
    pub fn get_fields(&mut self, ast: &'a DeriveInput) -> Result<(), syn::Error> {
        if let syn::DeriveInput {
            attrs: _,
            vis: _,
            ident: _,
            generics: _,
            data:
                syn::Data::Struct(syn::DataStruct {
                    fields: Fields::Named(fields),
                    ..
                }),
        } = &ast
        {
            fields.named.iter().try_for_each(|field| {
                if let Field {
                    attrs,
                    vis: _,
                    mutability: _,
                    ident,
                    colon_token: _,
                    ty:
                        syn::Type::Path(syn::TypePath {
                            qself: _,
                            path:
                                syn::Path {
                                    leading_colon: _,
                                    segments,
                                },
                        }),
                } = field
                {
                    self.field_ident = ident;
                    self.field = Some(field);
                    self.attrs = Some(attrs);
                    self.parse_macro_attributes(attrs, segments)?;

                    Ok(())
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Expected syn::Field, found `{field:?}`"),
                    ))
                }
            })?;
        }

        Ok::<(), syn::Error>(())
    }
    fn parse_macro_attributes(
        &mut self,
        attrs: &[Attribute],
        segments: &Punctuated<PathSegment, PathSep>,
    ) -> Result<(), syn::Error> {
        let mut segment_extracted = false;

        attrs.iter().try_for_each(|attr| {
            if let Attribute {
                pound_token: _,
                style: AttrStyle::Outer,
                bracket_token: _,
                meta:
                    Meta::List(MetaList {
                        path,
                        delimiter: MacroDelimiter::Paren(_),
                        tokens,
                    }),
            } = &attr
            {
                path.segments.iter().try_for_each(|PathSegment { ident, arguments: _ }| {
                    if ident != "extract_from" {
                        Err(syn::Error::new(
                            Span::call_site(),
                            format!("Invalid attribute, expected `extract_from`, found `{ident}`"),
                        ))
                    } else {
                        tokens.clone().into_iter().try_for_each(|token| {
                            match token {
                                TokenTree::Ident(ident) => {
                                    match ident.to_string().as_str() {
                                        "attribute" => {
                                            self.extract_attribute_segments(segments, &None)?;
                                            segment_extracted = true;
                                            Ok(())
                                        }
                                        "tag" => Ok(()),
                                        _ => Err(syn::Error::new(
                                            Span::call_site(),
                                            format!(
                                                "Invalid tokens, expected `attribute` or `tag`, found `{ident}`"
                                            ),
                                        )),
                                    }
                                }
                                TokenTree::Punct(punct) => {
                                    if punct.as_char() == '=' {
                                        Ok(())
                                    } else {
                                        Err(syn::Error::new(
                                            Span::call_site(),
                                            format!(
                                                "Invalid tokens, expected `=`, found `{punct}`"
                                            ),
                                        ))
                                    }
                                }
                                TokenTree::Literal(lit) => {
                                    let lit_str = lit.to_string();
                                    if lit_str.starts_with('"') && lit_str.ends_with('"') {
                                        let lit_value = lit_str.trim_matches('"').to_string();
                                        self.extract_segments(segments, &Some(lit_value))?;
                                        segment_extracted = true;
                                        Ok(())
                                    } else {
                                        Err(syn::Error::new(
                                            lit.span(),
                                            format!("Expected a string literal enclosed in quotes, found `{}`", lit_str),
                                        ))
                                    }
                                }
                                _ => Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Expected TokenTree, found `{token}`"),
                                )),
                            }
                        })
                    }
                })
            } else {
                Err(syn::Error::new(
                    Span::call_site(),
                    format!("Expected syn::Attribute, found `{attr:?}`"),
                ))
            }
        })?;

        if !segment_extracted {
            self.extract_segments(segments, &None)
        } else {
            Ok(())
        }
    }
    fn extract_attribute_segments(
        &mut self,
        segments: &Punctuated<PathSegment, PathSep>,
        replacement: &Option<String>,
    ) -> Result<(), syn::Error> {
        segments.iter().try_for_each(
            |PathSegment {
                 ident,
                 arguments: _,
             }| {
                if let Some(field_ident) = &self.field_ident {
                    if let Some(attrs) = &self.attrs {
                        if !attrs.is_empty() {
                            if ident == "Option" {
                                self.attributed_opt_fields.fields.push(field_ident.clone());
                                if let Some(replacement) = &replacement {
                                    self.attributed_opt_fields
                                        .replacements
                                        .insert(field_ident.clone(), replacement.clone());
                                }
                            } else {
                                self.attributed_fields.fields.push(field_ident.clone());
                                if let Some(replacement) = &replacement {
                                    self.attributed_fields
                                        .replacements
                                        .insert(field_ident.clone(), replacement.clone());
                                }
                            }
                            Ok(())
                        } else {
                            Err(syn::Error::new(ident.span(), "Attributes are empty"))
                        }
                    } else {
                        Err(syn::Error::new(ident.span(), "Attributes not found"))
                    }
                } else {
                    Err(syn::Error::new(ident.span(), "Field identifier not found"))
                }
            },
        )
    }

    fn extract_segments(
        &mut self,
        segments: &Punctuated<PathSegment, PathSep>,
        replacement: &Option<String>,
    ) -> Result<(), syn::Error> {
        segments
            .iter()
            .try_for_each(|PathSegment { ident, arguments }| {
                if let Some(field_ident) = self.field_ident {
                    if let Some(attrs) = self.attrs {
                        let ty_ident = ident;
                        if ty_ident == "String" || is_numeric_type(ty_ident) {
                            self.non_attributed_fields.extract_arguments(
                                arguments,
                                field_ident,
                                ty_ident,
                                replacement,
                            )
                        } else if ty_ident == "Vec" {
                            extract_vec_field_arguments(
                                arguments,
                                field_ident,
                                ty_ident,
                                self,
                                replacement,
                            )
                        } else if ty_ident == "Option" {
                            extract_optional_arguments(
                                arguments,
                                field_ident,
                                ty_ident,
                                self,
                                replacement,
                            )
                        } else {
                            self.sub_fields.extract_arguments(
                                arguments,
                                field_ident,
                                ty_ident,
                                replacement,
                            )
                        }
                    } else {
                        Err(syn::Error::new(
                            Span::call_site(),
                            format!("Missing syn::Attribute, found `{:?}`", self.field),
                        ))
                    }
                } else {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Expected syn::Field, found `{:?}`", self.field),
                    ))
                }
            })
    }
}

fn extract_arguments<State>(
    arguments: &PathArguments,
    field_ident: &Ident,
    ty_ident: &Ident,
    fields: &mut FieldTypes<State>,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            fields.fields.push(field_ident.clone());
            fields.tys.push(ty_ident.clone());
            if let Some(replacement) = replacement {
                fields
                    .replacements
                    .insert(field_ident.clone(), replacement.clone());
            };
            Ok(())
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments
                    .iter()
                    .try_for_each(|PathSegment { ident, arguments }| {
                        if !arguments.is_empty() {
                            todo!("Nested segment arguments have yet to be implemented");
                        }
                        match ident.to_string().as_str() {
                            "String" | "Vec" => {
                                fields.fields.push(field_ident.clone());
                                fields.tys.push(ident.clone());
                                if let Some(replacement) = replacement {
                                    fields
                                        .replacements
                                        .insert(field_ident.clone(), replacement.clone());
                                };
                                Ok(())
                            }
                            _ => {
                                if is_numeric_type(ident) {
                                    fields.fields.push(field_ident.clone());
                                    fields.tys.push(ident.clone());
                                    if let Some(replacement) = replacement {
                                        fields
                                            .replacements
                                            .insert(field_ident.clone(), replacement.clone());
                                    };
                                    Ok(())
                                } else {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!(
                                            "Unknown Nested Segment: `{ident:?}` `{arguments:?}`"
                                        ),
                                    ))
                                }
                            }
                        }
                    })
            } else {
                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported `{arguments:?}`"),
        )),
    }
}

fn extract_optional_arguments(
    arguments: &PathArguments,
    field_ident: &Ident,
    ty_ident: &Ident,
    ctx: &mut FieldsContextRefs,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => match ty_ident.to_string().as_str() {
            "String" | "Vec" => {
                ctx.non_attributed_opt_fields
                    .fields
                    .push(field_ident.clone());
                ctx.non_attributed_opt_fields.tys.push(ty_ident.clone());
                if let Some(replacement) = replacement {
                    ctx.non_attributed_opt_fields
                        .replacements
                        .insert(field_ident.clone(), replacement.clone());
                };
                Ok(())
            }
            _ => {
                if is_numeric_type(field_ident) {
                    ctx.non_attributed_opt_fields
                        .fields
                        .push(field_ident.clone());
                    ctx.non_attributed_opt_fields.tys.push(field_ident.clone());
                    if let Some(replacement) = replacement {
                        ctx.non_attributed_opt_fields
                            .replacements
                            .insert(field_ident.clone(), replacement.clone());
                    };

                    Ok(())
                } else {
                    extract_arguments(
                        arguments,
                        field_ident,
                        ty_ident,
                        ctx.sub_opt_fields,
                        replacement,
                    )
                }
            }
        },
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments
                    .iter()
                    .try_for_each(|PathSegment { ident, arguments }| {
                        if !arguments.is_empty() {
                            match ident.to_string().as_str() {
                                "Vec" => {
                                    extract_optional_vec_field_arguments(
                                        arguments,
                                        field_ident,
                                        ty_ident,
                                        ctx,
                                        replacement,
                                    )
                                }
                                "Option" => {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!("nested arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
                                    ))
                                }
                                _=> {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!("Unknown Nested Segments: `{ident:?}` `{arguments:?}`"),
                                    ))
                                }
                            }

                        } else {
                            match ident.to_string().as_str() {
                                "String" | "Vec" => {
                                    ctx.non_attributed_opt_fields
                                        .fields
                                        .push(field_ident.clone());
                                    ctx.non_attributed_opt_fields.tys.push(ident.clone());
                                    if let Some(replacement) = replacement {
                                        ctx.non_attributed_opt_fields
                                            .replacements
                                            .insert(field_ident.clone(), replacement.clone());
                                    };
                                    Ok(())
                                }
                                _ => {
                                    if is_numeric_type(ident) {
                                        ctx.non_attributed_opt_fields
                                            .fields
                                            .push(field_ident.clone());
                                        ctx.non_attributed_opt_fields.tys.push(ident.clone());
                                        if let Some(replacement) = replacement {
                                            ctx.non_attributed_opt_fields
                                                .replacements
                                                .insert(field_ident.clone(), replacement.clone());
                                        };
                                        Ok(())
                                    } else {
                                        extract_arguments(
                                            arguments,
                                            field_ident,
                                            ty_ident,
                                            ctx.sub_opt_fields,
                                            replacement,
                                        )
                                    }
                                }
                            }
                        }
                    })
            } else {
                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported `{arguments:?}`"),
        )),
    }
}

fn extract_sub_optional_field_arguments(
    arguments: &PathArguments,
    field_ident: &Ident,
    _ty_ident: &Ident,
    ctx: &mut FieldsContextRefs,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            Err(syn::Error::new(
                Span::call_site(),
                format!("PathArguments::None not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
            ))
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments.iter().try_for_each(|PathSegment { ident, arguments }| {
                    if !arguments.is_empty() {
                        Err(syn::Error::new(
                            Span::call_site(),
                            format!("nested arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
                        ))
                    }
                    else {
                        match ident.to_string().as_str() {
                            "String" | "Vec" => {
                                extract_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx.vec_opt_fields,
                                    replacement,
                                )
                            }
                            _ => {
                                if is_numeric_type(ident) {
                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.vec_opt_fields,
                                        replacement,
                                    )
                                } else {
                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.vec_opt_sub_fields,
                                        replacement,

                                    )
                                }
                    }
                }
            }

                }
            )} else {

                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
        )),
    }
}
fn extract_vec_field_arguments(
    arguments: &PathArguments,
    field_ident: &Ident,
    ty_ident: &Ident,
    ctx: &mut FieldsContextRefs,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            Err(syn::Error::new(
                Span::call_site(),
                format!("PathArguments::None not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
            ))
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments.iter().try_for_each(|PathSegment { ident, arguments }| {
                    if !arguments.is_empty() {
                        match ident.to_string().as_str() {
                            "Option" => {
                                extract_sub_optional_field_arguments(
                                    arguments,
                                    field_ident,
                                    ty_ident,
                                    ctx,
                                    replacement,

                                )
                            }
                            "String"  => {
                                extract_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx.vec_fields,
                                    replacement,

                                )
                            }
                            _ => {
                                if is_numeric_type(ident) {

                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.vec_fields,
                                        replacement,


                                    )
                                } else {
                                    Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Nested Segment: `{ident:?}` `{arguments:?}`")))
                                }
                            }
                        }
                    }
                    else{
                        match ident.to_string().as_str() {
                            "String" | "Vec" => {
                                extract_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx.vec_fields,
                                    replacement,

                                )
                            }
                            _ => {
                                if is_numeric_type(ident) {

                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.vec_fields,
                                        replacement,
                                    )
                                } else {
                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.vec_sub_fields,
                                        replacement,
                                    )
                        }
                    }}}

                    }
                )
            } else {

                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
        )),
    }
}

fn extract_optional_vec_field_arguments(
    arguments: &PathArguments,
    field_ident: &Ident,
    _ty_ident: &Ident,
    ctx: &mut FieldsContextRefs,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            Err(syn::Error::new(
                Span::call_site(),
                format!("PathArguments::None not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
            ))
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments.iter().try_for_each(|PathSegment { ident, arguments }| {
                    if !arguments.is_empty() {
                        match ident.to_string().as_str() {
                            "String"  => {
                                extract_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx.opt_vec_sub_fields,
                                    replacement,
                                )
                            }
                            "Option" => {
                                extract_option_vec_option_field_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx,
                                    replacement,
                                )
                            }
                            _ => {
                                if is_numeric_type(ident) {

                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.opt_vec_sub_fields,
                                        replacement,
                                    )
                                } else {
                                    Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Nested Segment: `{ident:?}` `{arguments:?}`")))
                                }
                            }
                        }
                    }
                    else {
                        match ident.to_string().as_str() {
                            "String" | "Vec" => {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("opt_vec_fields not implemented yet: `{ident:?}` `{arguments:?}`")))
                            }
                            _ => {
                                if is_numeric_type(ident) {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!("opt_vec_fields not implemented yet: `{ident:?}` `{arguments:?}`")))
                                } else {
                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.opt_vec_sub_fields,
                                        replacement,
                                    )
                                }
                            }
                        }

                    }}
                )
            } else {

                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
        )),
    }
}

fn extract_option_vec_option_field_arguments(
    arguments: &PathArguments,
    field_ident: &Ident,
    _ty_ident: &Ident,
    ctx: &mut FieldsContextRefs,
    replacement: &Option<String>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            Err(syn::Error::new(
                Span::call_site(),
                format!("PathArguments::None not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
            ))
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: _,
            lt_token: _,
            args,
            gt_token: _,
        }) => args.iter().try_for_each(|arg| {
            if let GenericArgument::Type(syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            })) = arg
            {
                segments.iter().try_for_each(|PathSegment { ident, arguments }| {
                    if !arguments.is_empty() {
                        match ident.to_string().as_str() {
                            "String"  => {
                                extract_arguments(
                                    arguments,
                                    field_ident,
                                    ident,
                                    ctx.opt_vec_opt_sub_fields,
                                    replacement,
                                )
                            }
                            _ => {
                                if is_numeric_type(ident) {

                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.opt_vec_opt_sub_fields,
                                        replacement,
                                    )
                                } else {
                                    Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Unknown Nested Segment: `{ident:?}` `{arguments:?}`")))
                                }
                            }
                        }
                    }
                    else {
                        match ident.to_string().as_str() {
                            "String" | "Vec" => {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("opt_vec_fields not implemented yet: `{ident:?}` `{arguments:?}`")))
                            }
                            _ => {
                                if is_numeric_type(ident) {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!("opt_vec_fields not implemented yet: `{ident:?}` `{arguments:?}`")))
                                } else {
                                    extract_arguments(
                                        arguments,
                                        field_ident,
                                        ident,
                                        ctx.opt_vec_opt_sub_fields,
                                        replacement,
                                    )
                                }
                            }
                        }

                    }}
                )
            } else {

                Ok(())
            }
        }),
        PathArguments::Parenthesized(_) => Err(syn::Error::new(
            Span::call_site(),
            format!("Parenthesized arguments not yet supported in extract_optional_sub_field_arguments `{arguments:?}`"),
        )),
    }
}
