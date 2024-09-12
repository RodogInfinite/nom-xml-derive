# Nom-XML-Derive

`nom-xml-derive` is a crate for extracting data from [`nom-xml`](https://crates.io/crates/nom-xml) version 0.3.0+'s `Document` type into structs. Due to the nested nature of XML, the interface for extracting information from `nom-xml`'s `Document` type can be tedious to implement for users. This derive macro crate was established to reduce that burden on users.

## Let's look at an example:

If we wanted to extract the first book in a catalog, we would need to manually implement the UpdateFields trait for each struct:
```rust
use nom_xml::{
    attribute::{Attribute, AttributeValue},
    tag::Tag,
    Document, UpdateFields,
};

#[derive(Debug, Default)]
struct Book {
    isbn: String,
    authored_by: AuthoredBy,
    title: String,
    genre: String,
    ty: String,
    series_number: u8,
    description: String,
}
impl UpdateFields for Book {
    fn update_field(
        &mut self,
        tag: &Tag,
        doc: &Document,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let field_name = &tag.name.local_part;

        if let Some(attributes_vec) = &tag.attributes {
            for attr in attributes_vec.iter() {
                if let Attribute::Instance {
                    name,
                    value: AttributeValue::Value(attr_val),
                } = attr
                {
                    if name.local_part == "isbn" {
                        self.isbn = attr_val.to_string();
                    }
                }
            }
        }

        match &doc {
            Document::Content(Some(value)) => match field_name.as_str() {
                "title" => {
                    self.title = value.to_string();
                }
                "genre" => {
                    self.genre = value.to_string();
                }
                "type" => {
                    self.ty = value.to_string();
                }
                "series_number" => {
                    self.series_number = value.parse().unwrap_or_default();
                }
                "description" => {
                    self.description = value.to_string();
                }
                e => {
                    return Err(format!("Unknown field2: {}", e).into());
                }
            },
            Document::Nested(_) => {
                for element in doc.iter_with_depth(1) {
                    if let Document::Element(tag, inner_doc, _) = element {
                        if "authored_by" == tag.name.local_part {
                            self.authored_by.update_fields(inner_doc)?;
                        } else {
                            self.update_field(tag, inner_doc)?;
                        }
                    } else {
                        return Err(format!("Unknown field: {element:#?}").into());
                    }
                }
            }

            _ => {
                return Err("Content is missing".into());
            }
        }

        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
struct AuthoredBy {
    pen_name: String,
    authors: Vec<AuthorName>,
}

impl UpdateFields for AuthoredBy {
    fn update_field(
        &mut self,
        tag: &Tag,
        doc: &Document,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match (tag.name.local_part.as_str(), doc) {
            ("pen_name", Document::Content(Some(value))) => {
                self.pen_name = value.to_string();
                Ok(())
            }
            ("authors", Document::Nested(elements)) => {
                elements.iter().try_for_each(
                    |element| -> std::result::Result<(), Box<dyn std::error::Error>> {
                        if let Document::Element(_, inner_doc, _) = element {
                            let mut author_name = AuthorName::default();
                            if let Document::Nested(inner_elements) = inner_doc.as_ref() {
                                inner_elements.iter().try_for_each(
                                    |inner_element| -> Result<(), Box<dyn std::error::Error>> {
                                        if let Document::Element(tag, content, _) = inner_element {
                                            author_name.update_field(tag, content)?;
                                        }
                                        Ok(())
                                    },
                                )?;
                                self.authors.push(author_name);
                            } else {
                                return Err("Content is missing in Author authors".into());
                            }
                        }
                        Ok(())
                    },
                )?;
                Ok(())
            }
            _ => Err(format!("Unknown field in Author: {}", tag.name.local_part).into()),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct AuthorName {
    first_name: String,
    last_name: String,
}
impl UpdateFields for AuthorName {
    fn update_field(
        &mut self,
        tag: &Tag,
        doc: &Document,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let field_name = &tag.name.local_part;

        if let Document::Content(Some(value)) = &doc {
            match field_name.as_str() {
                "first_name" => {
                    self.first_name = value.to_string();
                    Ok(())
                }
                "last_name" => {
                    self.last_name = value.to_string();
                    Ok(())
                }
                e => Err(format!("Unknown field in AuthorName: {}", e).into()),
            }
        } else {
            Err("Content is missing in AuthorName".into())
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let data = r#"<catalog>
    <book isbn="978-0316129084">
        <authored_by>
            <pen_name>James S.A. Corey</pen_name>
            <authors>
                <author><first_name>Daniel</first_name><last_name>Abraham</last_name></author>
                <author><first_name>Ty</first_name><last_name>Franck</last_name></author>
            </authors>
        </authored_by>
        <title>Leviathan Wakes</title>
        <genre>Science Fiction</genre>
        <type>Novel</type>
        <series_number>1</series_number>
        <description>The first book in the Expanse series</description>
    </book>
    <book isbn="978-0316129060">
        <authored_by>
            <pen_name>James S.A. Corey</pen_name>
            <authors>
                <author><first_name>Daniel</first_name><last_name>Abraham</last_name></author>
                <author><first_name>Ty</first_name><last_name>Franck</last_name></author>
            </authors>
        </authored_by>
        <title>Caliban's War</title>
        <genre>Science Fiction</genre>
        <type>Novel</type>
        <series_number>2</series_number>
        <description>The second book in the Expanse series</description>
    </book>></catalog>"#
        .to_string();

    let (_, doc) = Document::parse_element_by_tag_name(&data, "book", &None)?;
    let mut book = Book::default();

    doc.iter_with_depth(0)
        .filter_map(|element| {
            if let Document::Element(tag, inner_doc, _) = element {
                Some((tag, inner_doc))
            } else {
                None
            }
        })
        .try_for_each(|(tag, inner_doc)| book.update_field(tag, inner_doc))?;

    println!("{book:#?}");
    Ok(())
}
```
---


Whereas if we use the `nom-xml-derive` crate with `nom-xml` we would simply decorate each struct with `#[derive(Debug, Default, Clone, ExtractFields, PartialEq)]` and subsequent "special" fields with `#[extract(<behavior>)]`. This results in those methods being generated at compile time significantly reducing the boilerplate needed:

```rust
use nom_xml::{
    attribute::{Attribute, AttributeValue},
    tag::Tag,
    Document, DocumentIteratorExt, UpdateFields,
};
use nom_xml_derive::ExtractFields;
use std::prelude::rust_2021::*;

#[derive(Debug, Default, Clone, ExtractFields, PartialEq)]
struct Book {
    #[extract(from_attribute)]
    isbn: String,
    authored_by: Option<AuthoredBy>,
    title: String,
    genre: String,
    #[extract(from_tag = "type")]
    ty: String,
    series_number: u8,
    description: Option<String>,
}

#[derive(Debug, Default, Clone, ExtractFields, PartialEq)]
struct AuthoredBy {
    pen_name: Option<String>,
    authors: Option<Vec<AuthorName>>,
}

#[derive(Debug, Default, Clone, ExtractFields, PartialEq)]
struct AuthorName {
    first_name: String,
    last_name: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let data = r#"<catalog>
    <book isbn="978-0316129084">
        <authored_by>
            <pen_name>James S.A. Corey</pen_name>
            <authors>
                <author><first_name>Daniel</first_name><last_name>Abraham</last_name></author>
                <author><first_name>Ty</first_name><last_name>Franck</last_name></author>
            </authors>
        </authored_by>
        <title>Leviathan Wakes</title>
        <genre>Science Fiction</genre>
        <type>Novel</type>
        <series_number>1</series_number>
        <description>The first book in the Expanse series</description>
    </book>
    <book isbn="978-0316129060">
        <authored_by>
            <pen_name>James S.A. Corey</pen_name>
            <authors>
                <author><first_name>Daniel</first_name><last_name>Abraham</last_name></author>
                <author><first_name>Ty</first_name><last_name>Franck</last_name></author>
            </authors>
        </authored_by>
        <title>Caliban's War</title>
        <genre>Science Fiction</genre>
        <type>Novel</type>
        <series_number>2</series_number>
        <description>The second book in the Expanse series</description>
    </book></catalog>"#
        .to_string();

    let (_, doc) = Document::parse_element_by_tag_name(&data, "book", &None)?;
    let mut book = Book::default();
    book.update_fields(&doc)?;
    println!("{book:#?}");

    Ok(())
}
```

Note: in the latter example, `book.update_fields(&doc)?;` is used in `main(){...}` instead of manually iterating over the document. This compile-time-generated method is essentially doing that work for you. For more, see the examples and tests in [`nom-xml`](https://github.com/RodogInfinite/NomExML).

# Currently Supported Field Types:
- `String`
- `Vec<String>`
- `Vec<Numerical Types>`
- `Vec<User Structs>`
- `Option` //should work with any variation of the above

Note: This is an extremely edge-case-heavy macro. If you're experiencing any errors, please submit an issue.