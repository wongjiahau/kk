use crate::{non_empty::NonEmpty, raw_ast::*};
use pretty::RcDoc;

trait ToDoc {
    fn to_doc(&self) -> RcDoc<()>;
    fn to_pretty(&self) -> String {
        let mut vec = Vec::new();
        let width = 80;
        self.to_doc().render(width, &mut vec).unwrap();
        String::from_utf8(vec).unwrap()
    }
}

fn brackets<'a>(opening: &'a str, doc: RcDoc<'a, ()>, closing: &'a str) -> RcDoc<'a, ()> {
    RcDoc::text(opening)
        .append(RcDoc::line_().append(doc).nest(2).group())
        .append(RcDoc::line_())
        .append(RcDoc::text(closing))
}

impl ToDoc for Statement {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Statement::Let(_) => todo!(),
            Statement::Type(_) => todo!(),
            Statement::Enum(enum_statement) => enum_statement.to_doc(),
            Statement::Import(_) => todo!(),
            Statement::Entry(_) => todo!(),
        }
    }
}

impl<T: ToDoc> NonEmpty<T> {
    fn intersperse<'a>(&'a self, separator: &'a str) -> RcDoc<'a, ()> {
        RcDoc::intersperse(
            vec![self.head.to_doc()]
                .into_iter()
                .chain(self.tail.iter().map(|element| element.to_doc())),
            RcDoc::text(separator).append(RcDoc::line()),
        )
    }
}

impl EnumStatement {
    pub fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(format!("class {} ", self.name.representation)).append(
            RcDoc::line()
                .append(RcDoc::text("= "))
                .append(RcDoc::intersperse(
                    self.constructors
                        .iter()
                        .map(|constructor| constructor.to_doc().nest(2)),
                    RcDoc::line().append(RcDoc::text("| ")),
                ))
                .nest(2)
                .group(),
        )
    }
}

impl EnumConstructorDefinition {
    pub fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.name.representation.clone()).append(match &self.payload {
            None => RcDoc::nil(),
            Some(payload) => payload.type_annotation.to_doc(),
        })
    }
}

impl ToDoc for TypeAnnotation {
    fn to_doc(&self) -> pretty::RcDoc {
        match self {
            TypeAnnotation::Parenthesized {
                left_parenthesis,
                type_annotation,
                right_parenthesis,
            } => brackets("(", type_annotation.to_doc(), ")"),
            TypeAnnotation::Scheme {
                type_variables,
                type_annotation,
            } => todo!(),
            TypeAnnotation::Quoted {
                opening_backtick,
                type_annotation,
                closing_backtick,
            } => todo!(),
            TypeAnnotation::Named {
                name,
                type_arguments,
            } => {
                let name = name.representation.clone();
                let doc = match type_arguments {
                    None => RcDoc::nil(),
                    Some(type_arguments) => brackets(
                        "<",
                        type_arguments.type_annotations.as_ref().intersperse(","),
                        ">",
                    ),
                };
                RcDoc::text(name).append(doc).group()
            }
            TypeAnnotation::Record {
                left_curly_bracket,
                key_type_annotation_pairs,
                right_curly_bracket,
            } => todo!(),
            TypeAnnotation::Array {
                hash_left_square_bracket,
                element_type,
                right_square_bracket,
            } => todo!(),
            TypeAnnotation::Underscore(_) => todo!(),
            TypeAnnotation::Function(_) => todo!(),
            TypeAnnotation::Unit {
                left_parenthesis,
                right_parenthesis,
            } => todo!(),
            TypeAnnotation::Keyword { identifier } => todo!(),
        }
    }
}

pub fn prettify_code(code: String) -> String {
    use crate::{parse::Parser, tokenize::Tokenizer};
    use itertools::Itertools;
    let statements = Parser::parse(&mut Tokenizer::new(code)).unwrap();

    statements
        .into_iter()
        .map(|statement| statement.to_pretty())
        .collect_vec()
        .join("\n\n")
}

#[cfg(test)]
mod formatter_test {

    #[cfg(test)]
    mod enum_statement {
        use crate::formatter::prettify_code;

        #[test]
        fn constructors_1() {
            insta::assert_snapshot!(prettify_code(
                "class Boolean = True | False | Hello | Yo | Wow | Foo | Bar | Spam | WhoLivesInAPineapple".to_string()
            ));
        }

        #[test]
        fn payload_parenthesized_1() {
            insta::assert_snapshot!(prettify_code(
                "class Boolean = Hello (WhoLivesInAPineappleUnderTheSea<Foo, Bar, Spam<Yo>, Pineapple<Walao<What<HowardStephen>>>>)"
                    .to_string()
            ));
        }
    }
}
