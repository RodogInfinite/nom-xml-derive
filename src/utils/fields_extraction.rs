use std::marker::PhantomData;

use proc_macro2::{Span, TokenTree};

use syn::{
    punctuated::Punctuated, token::PathSep, AngleBracketedGenericArguments, AttrStyle, Attribute,
    Field, Fields, GenericArgument, Ident, MacroDelimiter, Meta, MetaList, Path, PathArguments,
    PathSegment,
};

use crate::utils::update_field::is_numeric_type;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Attributed;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NonAttributed;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SubField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FieldTypes<State> {
    pub fields: Vec<Ident>,
    pub tys: Vec<Ident>,
    state: PhantomData<State>,
}

pub fn get_fields(
    attributed_fields: &mut FieldTypes<Attributed>,
    non_attributed_fields: &mut FieldTypes<NonAttributed>,
    sub_fields: &mut FieldTypes<SubField>,
    vec_fields: &mut FieldTypes<VecField>,

    ast: &syn::DeriveInput,
) -> Result<(), syn::Error> {
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
                let field_ident = ident;

                match check_attrs(attrs) {
                    Ok(()) => extract_segments(
                        segments,
                        field_ident,
                        field,
                        attrs,
                        attributed_fields,
                        non_attributed_fields,
                        sub_fields,
                        vec_fields,
                    ),
                    Err(e) => Err(e),
                }
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

fn check_attrs(attrs: &[Attribute]) -> Result<(), syn::Error> {
    attrs.iter().try_for_each(|attr| {
        if let Attribute {
            pound_token: _,
            style: AttrStyle::Outer,
            bracket_token: _,
            meta: Meta::List(MetaList {
                path: Path { segments, .. },
                delimiter: MacroDelimiter::Paren(_),
                tokens,
            }),
        } = &attr
        {
            segments.iter().try_for_each(|PathSegment { ident, arguments: _ }| {
                if ident != "extract" {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Invalid attribute, expected `extract`, found `{ident}`"),
                    ))
                } else {
                    tokens.clone().into_iter().try_for_each(|token| {

                        if let TokenTree::Ident(ident) = token {
                            if ident == "from_attribute" {
                                Ok(())
                            } else {
                                Err(syn::Error::new(
                                    Span::call_site(),
                                    format!("Invalid tokens, expected `from_attribute`, found `{ident}`"),
                                ))
                            }
                        } else {
                            Err(syn::Error::new(
                                Span::call_site(),
                                format!("Expected TokenTree, found `{token}`"),
                            ))
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
   })
}

fn extract_segments(
    segments: &Punctuated<PathSegment, PathSep>,
    field_ident: &Option<Ident>,
    field: &Field,
    attrs: &[Attribute],
    attributed_fields: &mut FieldTypes<Attributed>,
    non_attributed_fields: &mut FieldTypes<NonAttributed>,
    sub_fields: &mut FieldTypes<SubField>,
    vec_fields: &mut FieldTypes<VecField>,
) -> Result<(), syn::Error> {
    segments
        .iter()
        .try_for_each(|PathSegment { ident, arguments }| {
            if let Some(field_ident) = field_ident {
                if !attrs.is_empty() {
                    attributed_fields.fields.push(field_ident.clone());
                    attributed_fields.tys.push(ident.clone());
                    Ok(())
                } else {
                    let ty_ident = ident;

                    if ty_ident == "String" || is_numeric_type(ty_ident) {
                        extract_arguments(arguments, field_ident, ty_ident, non_attributed_fields)
                    } else if ty_ident == "Vec" {
                        extract_arguments(arguments, field_ident, ty_ident, vec_fields)
                    } else {
                        extract_arguments(arguments, field_ident, ty_ident, sub_fields)
                    }
                }
            } else {
                Err(syn::Error::new(
                    Span::call_site(),
                    format!("Expected syn::Field, found `{field:?}`"),
                ))
            }
        })
}

fn extract_arguments<State>(
    arguments: &PathArguments,
    field_ident: &Ident,
    ty_ident: &Ident,
    fields: &mut FieldTypes<State>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {
            fields.fields.push(field_ident.clone());
            fields.tys.push(ty_ident.clone());
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
                        match ty_ident.to_string().as_str() {
                            "Vec" => {
                                fields.fields.push(field_ident.clone());
                                fields.tys.push(ident.clone());
                                Ok(())
                            }
                            _ => Ok(()),
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
