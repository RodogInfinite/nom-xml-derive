use proc_macro2::{Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{
    AttrStyle, Attribute, Field, Fields, Ident, MacroDelimiter, Meta, MetaList, Path, PathSegment,
};

#[derive(Debug, Default)]
pub struct NonAttributedFields {
    pub fields: Vec<Ident>,
    pub ty: Vec<Ident>,
}

#[derive(Debug, Default)]
pub struct AttributedFields {
    pub fields: Vec<Ident>,
    pub ty: Vec<Ident>,
}

pub fn get_fields(
    attributed_fields: &mut AttributedFields,
    non_atributed_fields: &mut NonAttributedFields,
    data_struct: syn::DataStruct,
) -> Result<(), syn::Error> {
    // dbg!(&data_struct);
    if let Fields::Named(fields) = &data_struct.fields {
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
                    Ok(()) => {
                        segments.iter().try_for_each(
                            |PathSegment {
                                 ident,
                                 arguments: _,
                             }| {
                                if let Some(field_ident) = field_ident {
                                    if !attrs.is_empty() {
                                        attributed_fields.fields.push(field_ident.clone());
                                        attributed_fields.ty.push(ident.clone());
                                    } else {
                                        non_atributed_fields.fields.push(field_ident.clone());
                                        non_atributed_fields.ty.push(ident.clone());
                                    };
                                    Ok::<(), syn::Error>(())
                                } else {
                                    Err(syn::Error::new(
                                        Span::call_site(),
                                        format!("Expected syn::Field, found `{field:?}`"),
                                    ))
                                }
                            },
                        )?;
                        Ok(())
                    }
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

    Ok(())
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
                if ident != "update" {
                    Err(syn::Error::new(
                        Span::call_site(),
                        format!("Invalid attribute, expected `update`, found `{ident}`"),
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
