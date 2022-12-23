use crate::{non_empty::NonEmpty, raw_ast::Token, simple_ast::*};
use itertools::Itertools;
use pretty::RcDoc;

pub trait ToDoc {
    fn to_doc(&self) -> RcDoc<()>;
    fn to_pretty(&self) -> String {
        let mut vec = Vec::new();
        let width = 80;
        self.to_doc().render(width, &mut vec).unwrap();
        String::from_utf8(vec).unwrap()
    }
}

fn brackets<'a>(
    opening: &'a str,
    doc: RcDoc<'a, ()>,
    closing: &'a str,
    add_space: bool,
) -> RcDoc<'a, ()> {
    let line = if add_space { RcDoc::line } else { RcDoc::line_ };
    RcDoc::text(opening)
        .append(line().append(doc).nest(2).append(line()).group())
        .append(RcDoc::text(closing))
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

impl ToDoc for Token {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.representation.as_str())
    }
}

impl ToDoc for Node {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Node::Array(array) => array.to_doc(),
            Node::PrefixFunctionCall(prefix_function_call) => prefix_function_call.to_doc(),
            Node::InfixFunctionCall(infix_function_call) => infix_function_call.to_doc(),
            Node::OperatorCall(operator_call) => operator_call.to_doc(),
            Node::Literal(literal) => literal.to_doc(),
        }
    }
}

impl ToDoc for Literal {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Literal::String(literal) => RcDoc::text(literal.content.clone()),
            Literal::Integer(token)
            | Literal::Character(token)
            | Literal::Float(token)
            | Literal::Identifier(token)
            | Literal::Keyword(token) => token.to_doc(),
        }
    }
}

impl ToDoc for OperatorCall {
    fn to_doc(&self) -> RcDoc<()> {
        self.left
            .to_doc()
            .append(RcDoc::space())
            .append(self.operator.to_doc())
            .append(
                RcDoc::line()
                    .nest(2)
                    .append(self.right.to_doc().nest(2))
                    .group(),
            )
    }
}

impl ToDoc for PrefixFunctionCall {
    fn to_doc(&self) -> RcDoc<()> {
        #[derive(Debug, Clone)]
        struct State {
            is_odd_group: bool,
            previous_argument_is_keyword: bool,
        }

        // Group each keyword as its onw group
        // For example:
        //
        //   f 0 g 1 2 z 3 4
        //
        // Will be grouped as:
        //
        //   (f 0) (g 1 2) (z 3 4)
        //
        // And formatted like:
        //
        //   f 0
        //     g 1 2
        //     z 3 4
        //
        let groups = self
            .arguments
            .iter()
            .scan(
                State {
                    is_odd_group: true,
                    previous_argument_is_keyword: false,
                },
                |state, element| {
                    let result = match element {
                        Node::Literal(Literal::Keyword(_))
                            if !state.previous_argument_is_keyword =>
                        {
                            (
                                State {
                                    previous_argument_is_keyword: true,
                                    is_odd_group: !state.is_odd_group,
                                },
                                element,
                            )
                        }
                        _ => (
                            State {
                                previous_argument_is_keyword: false,
                                is_odd_group: state.is_odd_group,
                            },
                            element,
                        ),
                    };
                    *state = result.0.clone();

                    Some(result)
                },
            )
            .group_by(|(state, _)| state.is_odd_group);

        self.function.to_doc().append(RcDoc::space()).append(
            RcDoc::intersperse(
                groups.into_iter().map(|(_, group)| {
                    RcDoc::intersperse(group.map(|(_, element)| element.to_doc()), RcDoc::line())
                        .nest(2)
                        .group()
                }),
                RcDoc::line(),
            )
            .nest(2)
            .group(),
        )
    }
}

impl ToDoc for InfixFunctionCall {
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

impl ToDoc for Array {
    fn to_doc(&self) -> RcDoc<()> {
        let (opening, closing, add_space) = match self.bracket.kind {
            BracketKind::Round => ("(", ")", false),
            BracketKind::Square => ("[", "]", false),
            BracketKind::Curly => ("{", "}", true),
        };
        brackets(
            opening,
            intersperse_vec(
                self.nodes.iter().map(|element| element.to_doc()).collect(),
                ",",
            ),
            closing,
            add_space,
        )
    }
}

pub fn prettify_code(code: String) -> String {
    use crate::{parse_simple::Parser, tokenize::Tokenizer};
    let array = Parser::parse(&mut Tokenizer::new(code)).unwrap();

    array.to_pretty()
}

impl ToDoc for TopLevelArray {
    fn to_doc(&self) -> RcDoc<()> {
        intersperse_vec(self.nodes.iter().map(|node| node.to_doc()).collect(), "\n")
    }
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

    #[test]
    fn prefix_function_call_keywords_should_be_grouped_1() {
        use crate::formatter::prettify_code;

        insta::assert_snapshot!(prettify_code(
            "if { krabby patty is alive } then { spongebob squarepants + patrick star .replace squidward with hello yo yo  yo yo yo y } else { clarinet }".to_string()
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
