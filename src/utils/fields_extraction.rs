use std::{collections::HashSet, marker::PhantomData};

use proc_macro2::{Span, TokenTree};

use syn::{
    punctuated::Punctuated, token::PathSep, AngleBracketedGenericArguments, AttrStyle, Attribute, DeriveInput, Field, Fields, GenericArgument, Ident, MacroDelimiter, Meta, MetaList, Path, PathArguments, PathSegment
};

use crate::utils::update_field::is_numeric_type;

use super::update_field::FieldParameters;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Attributed;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NonAttributed;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SubField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct VecField;
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OptionField;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FieldTypes<State> {
    pub fields: Vec<Ident>,
    pub tys: Vec<Ident>,
    state: PhantomData<State>,
}

pub fn get_fields<'a>(
    params: &mut FieldParameters<'a>,
    ast: &'a DeriveInput,
) -> Result<(), syn::Error> {
    let FieldParameters {
        struct_name: _,
        attributed_fields,
        non_attributed_fields,
        sub_fields,
        sub_opt_fields,
        vec_fields,
        attributed_opt_fields,
        non_attributed_opt_fields,
        std_types,
    } = params;
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
                        &mut FieldContext {
                            field_ident,
                            field,
                            attrs,
                            attributed_fields,
                            non_attributed_fields,
                            sub_fields,
                            sub_opt_fields,
                            vec_fields,
                            attributed_opt_fields,
                            non_attributed_opt_fields,
                        },
                        std_types,
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

struct FieldContext<'a> {
    field_ident: &'a Option<Ident>,
    field: &'a Field,
    attrs: &'a [Attribute],
    attributed_fields: &'a mut FieldTypes<Attributed>,
    non_attributed_fields: &'a mut FieldTypes<NonAttributed>,
    sub_fields: &'a mut FieldTypes<SubField>,
    sub_opt_fields: &'a mut FieldTypes<OptionField>,
    vec_fields: &'a mut FieldTypes<VecField>,
    attributed_opt_fields: &'a mut FieldTypes<OptionField>,
    non_attributed_opt_fields: &'a mut FieldTypes<OptionField>,
}

fn extract_segments(
    segments: &Punctuated<PathSegment, PathSep>,
    ctx: &mut FieldContext,
    std_types: &HashSet<Ident>,
) -> Result<(), syn::Error> {
  
    segments
        .iter()
        .try_for_each(|PathSegment { ident, arguments }| {
            if let Some(field_ident) = ctx.field_ident {
                if !ctx.attrs.is_empty() {
                    if ident == "Option" {
                        ctx.attributed_opt_fields.fields.push(field_ident.clone());
                        ctx.attributed_opt_fields.tys.push(field_ident.clone());
                    }else {
                        ctx.attributed_fields.fields.push(field_ident.clone());
                        ctx.attributed_fields.tys.push(field_ident.clone());
                    }
                    Ok(())
                } else {
                    let ty_ident = ident;
                   
                    if ty_ident == "String" || is_numeric_type(ty_ident) {
                        extract_arguments(
                            arguments,
                            field_ident,
                            ty_ident,
                            ctx.non_attributed_fields,
                            std_types,
                        )
                    } else if ty_ident == "Vec" {
                        extract_arguments(
                            arguments,
                            field_ident,
                            ty_ident,
                            ctx.vec_fields,
                            std_types,
                        )
                    } else if ty_ident == "Option" {

                        extract_optional_arguments(
                            arguments,
                            field_ident,
                            ty_ident,
                            ctx,
                            std_types,
                        )
                    } else {

                        extract_arguments(
                            arguments,
                            field_ident,
                            ty_ident,
                            ctx.sub_fields,
                            std_types,
                        )
                    }
                }
            } else {
                Err(syn::Error::new(
                    Span::call_site(),
                    format!("Expected syn::Field, found `{:?}`", ctx.field),
                ))
            }
        })
}

fn extract_arguments<State>(
    arguments: &PathArguments,
    field_ident: &Ident,
    ty_ident: &Ident,
    fields: &mut FieldTypes<State>,
    std_types: &HashSet<Ident>,
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
                        if !arguments.is_empty() {
                            todo!("Nested segment arguments has yet to be implemented");
                        }
                        if std_types.contains(ident) || std_types.contains(ty_ident) || is_numeric_type(ident){
                            fields.fields.push(field_ident.clone());
                            fields.tys.push(ident.clone());
                            Ok(())
                        } else {
                            Err(syn::Error::new(
                                Span::call_site(),
                                format!("Unknown Field: `{ident:?}`"),
                            ))
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
    ctx: &mut FieldContext,
    std_types: &HashSet<Ident>,
) -> Result<(), syn::Error> {
    match arguments {
        PathArguments::None => {

            if  std_types.contains(ty_ident) || is_numeric_type(field_ident){
                ctx.non_attributed_opt_fields.fields.push(field_ident.clone());
                ctx.non_attributed_opt_fields.tys.push(field_ident.clone());
                Ok(())
            } else {
               
                extract_arguments(
                    arguments,
                    field_ident,
                    ty_ident,
                    ctx.sub_opt_fields,
                    std_types,
                )
            }

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
                            todo!("Nested segment arguments has yet to be implemented");
                        }
                        if std_types.contains(ident)  || is_numeric_type(ident){
                            ctx.non_attributed_opt_fields.fields.push(field_ident.clone());
                            ctx.non_attributed_opt_fields.tys.push(ident.clone());
                            Ok(())
                        } else {
                            extract_arguments(
                                arguments,
                                field_ident,
                                ty_ident,
                                ctx.sub_opt_fields,
                                std_types,
                            )
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