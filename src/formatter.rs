use crate::{non_empty::NonEmpty, raw_ast::*, simple_ast};
use itertools::Itertools;
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
        .append(
            RcDoc::line_()
                .append(doc)
                .nest(2)
                .append(RcDoc::line_())
                .group(),
        )
        .append(RcDoc::text(closing))
}

impl ToDoc for Statement {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Statement::Let(_) => todo!(),
            Statement::Type(type_alias_statement) => type_alias_statement.to_doc(),
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

fn intersperse_vec<'a>(elements: Vec<RcDoc<'a, ()>>, separator: &'a str) -> RcDoc<'a, ()> {
    RcDoc::intersperse(elements, RcDoc::text(separator).append(RcDoc::line()))
}

impl ToDoc for TypeAliasStatement {
    fn to_doc(&self) -> pretty::RcDoc {
        RcDoc::text("type ")
            .append(RcDoc::text(self.left.representation.as_str()))
            .append(self.type_variables_declaration.to_doc())
            .append(
                RcDoc::space()
                    .append(RcDoc::text("="))
                    .append(RcDoc::space())
                    .append(self.right.to_doc()),
            )
    }
}

impl ToDoc for Option<TypeVariablesDeclaration> {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            None => RcDoc::nil(),
            Some(type_variables_declaration) => brackets(
                "<",
                type_variables_declaration.type_variables.intersperse(","),
                ">",
            ),
        }
    }
}

impl ToDoc for Token {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.representation.as_str())
    }
}

impl ToDoc for simple_ast::Expression {
    fn to_doc(&self) -> RcDoc<()> {
        use simple_ast::*;
        match self {
            Expression::Array(array) => array.to_doc(),
            Expression::PrefixFunctionCall(prefix_function_call) => prefix_function_call.to_doc(),
            Expression::InfixFunctionCall(infix_function_call) => infix_function_call.to_doc(),
            Expression::OperatorCall(_) => todo!(),
            Expression::String(literal) => RcDoc::text(literal.content.clone()),
            Expression::Integer(token)
            | Expression::Character(token)
            | Expression::Float(token)
            | Expression::Identifier(token) => RcDoc::text(token.representation.clone()),
        }
    }
}

impl ToDoc for simple_ast::PrefixFunctionCall {
    fn to_doc(&self) -> RcDoc<()> {
        self.function.to_doc().append(
            // TODO: each keyword should has its own group
            RcDoc::concat(
                self.arguments
                    .iter()
                    .map(|element| RcDoc::line().append(element.to_doc())),
            )
            .nest(2)
            .group(),
        )
    }
}

impl ToDoc for simple_ast::InfixFunctionCall {
    fn to_doc(&self) -> RcDoc<()> {
        self.head.to_doc().append(
            RcDoc::concat(
                self.tail
                    .iter()
                    .map(|element| RcDoc::line().append(RcDoc::text(".").append(element.to_doc()))),
            )
            .nest(2)
            .group(),
        )
    }
}

impl ToDoc for simple_ast::Array {
    fn to_doc(&self) -> RcDoc<()> {
        use simple_ast::*;
        let (opening, closing) = match self.bracket {
            Bracket::Round => ("(", ")"),
            Bracket::Square => ("[", "]"),
            Bracket::Curly => ("{", "}"),
            Bracket::None => ("", ""),
        };
        brackets(
            opening,
            intersperse_vec(
                self.elements
                    .iter()
                    .map(|element| element.to_doc())
                    .collect(),
                ",",
            ),
            closing,
        )
    }
}

impl ToDoc for EnumStatement {
    fn to_doc(&self) -> RcDoc<()> {
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
                type_annotation, ..
            } => brackets("(", type_annotation.to_doc(), ")"),
            TypeAnnotation::Scheme {
                type_variables,
                type_annotation,
            } => brackets("<", type_variables.type_variables.intersperse(","), ">")
                .append(RcDoc::space().append(type_annotation.to_doc()).group()),
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
                key_type_annotation_pairs,
                ..
            } => brackets(
                "{",
                intersperse_vec(
                    key_type_annotation_pairs
                        .into_iter()
                        .map(|(key, type_annotation)| {
                            key.to_doc()
                                .append(RcDoc::text(":"))
                                .append(RcDoc::space())
                                .append(type_annotation.to_doc())
                        })
                        .collect_vec(),
                    ",",
                ),
                "}",
            ),
            TypeAnnotation::Array { .. } => todo!(),
            TypeAnnotation::Underscore(_) => todo!(),
            TypeAnnotation::Function(function_type_annotation) => {
                function_type_annotation.parameter.to_doc().append(
                    RcDoc::line()
                        .append(RcDoc::text("->"))
                        .append(RcDoc::space())
                        .append(function_type_annotation.return_type.to_doc())
                        .nest(2)
                        .group(),
                )
            }
            TypeAnnotation::Unit { .. } => RcDoc::text("()"),
            TypeAnnotation::Keyword { identifier } => {
                RcDoc::text(format!("#{}", identifier.representation))
            }
        }
    }
}

pub fn prettify_code(code: String) -> String {
    use crate::{parse_simple::Parser, tokenize::Tokenizer};
    let array = Parser::parse(&mut Tokenizer::new(code)).unwrap();

    array.to_pretty()
}

#[cfg(test)]
mod formatter_test {
    #[test]
    fn infix_function_call_1() {
        use crate::formatter::prettify_code;

        insta::assert_snapshot!(prettify_code(
            "hello .replace (x .call me spongebob (yes) you are squarepants) with (y) .to string .between 1 and 3 .who lives in a pineapple under the sea".to_string()
        ));
    }
}

// #[cfg(test)]
// mod formatter_test {

//     #[cfg(test)]
//     mod enum_statement {
//         use crate::formatter::prettify_code;

//         #[test]
//         fn constructors_1() {
//             insta::assert_snapshot!(prettify_code(
//                 "class Boolean = True | False | Hello | Yo | Wow | Foo | Bar | Spam | WhoLivesInAPineapple".to_string()
//             ));
//         }

//         #[test]
//         fn payload_parenthesized_1() {
//             insta::assert_snapshot!(prettify_code(
//                 "class Boolean = Hello (WhoLivesInAPineappleUnderTheSea<Foo, Bar, Spam<Yo>, Pineapple<Walao<What<HowardStephen>>>>)"
//                     .to_string()
//             ));
//         }
//     }

//     #[cfg(test)]
//     mod type_annotation {
//         use crate::formatter::prettify_code;

//         #[test]
//         fn record_1() {
//             insta::assert_snapshot!(prettify_code(
//                 "type People ={name:String,age:List<T>}".to_string()
//             ));
//         }

//         #[test]
//         fn record_2() {
//             insta::assert_snapshot!(prettify_code(
//                 "type People ={name:String,age:List<T>,foo:Foo,bar:Bar,life:Life,spam:Spam}"
//                     .to_string()
//             ));
//         }

//         #[test]
//         fn type_scheme_1() {
//             insta::assert_snapshot!(prettify_code("type People = <A> Hello<A>".to_string()));
//         }

//         #[test]
//         fn type_scheme_2() {
//             insta::assert_snapshot!(prettify_code(
//                 "type People = <WhoLivesInAPineappleUnderTheSea, SpongebobSquarepants,
//                 AbsorbentAndYellowAndPorousIsHe, SpongebobSquarepants> {name:String,age:List<T>,foo:Foo,bar:Bar,
//                 life:Life,spam:Spam,bottle:Bottle} "
//                     .to_string()
//             ));
//         }

//         #[test]
//         fn unit_1() {
//             insta::assert_snapshot!(prettify_code("type People = ()".to_string()));
//         }

//         #[test]
//         fn function_1() {
//             insta::assert_snapshot!(prettify_code("type People = A->B".to_string()));
//         }

//         #[test]
//         fn function_2() {
//             insta::assert_snapshot!(prettify_code(
//                 "type People = Foo->Bar->Spam->Spongebob->Squarepants->Patrick->Star->Crab"
//                     .to_string()
//             ));
//         }
//     }
// }
