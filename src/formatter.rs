use crate::{
    non_empty::NonEmpty,
    simple_ast::*,
    tokenize::{StringLiteral, Token},
};
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
            Node::SemicolonArray(array) => array.nodes.intersperse(";"),
            Node::CommentedNode(commented_node) => commented_node.to_doc(),
        }
    }
}

impl ToDoc for CommentedNode {
    fn to_doc(&self) -> RcDoc<()> {
        self.comment
            .to_doc()
            .append(RcDoc::hardline())
            .append(self.node.to_doc())
    }
}

impl ToDoc for TopLevelArray {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::intersperse(
            self.nodes
                .to_vector()
                .into_iter()
                .map(|node| node.to_pretty()),
            RcDoc::concat(vec![RcDoc::text(";"), RcDoc::hardline(), RcDoc::hardline()]),
        )
    }
}

impl ToDoc for StringLiteral {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(format!(
            "{quote}{content}{quote}",
            quote = self
                .start_quotes
                .to_vector()
                .into_iter()
                .map(|character| character.value)
                .join(""),
            content = self.content.clone()
        ))
    }
}

impl ToDoc for Literal {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Literal::String(literal) => literal.to_doc(),
            Literal::Integer(token)
            | Literal::Character(token)
            | Literal::Float(token)
            | Literal::Identifier(token)
            | Literal::Keyword(token) => token.to_doc(),
            Literal::InterpolatedString(literal) => literal.to_doc(),
        }
    }
}

impl ToDoc for InterpolatedString {
    fn to_doc(&self) -> RcDoc<()> {
        let quotes = self
            .start_quotes
            .to_vector()
            .iter()
            .map(|quote| quote.value)
            .join(",");
        RcDoc::text(quotes.clone())
            .append(RcDoc::concat(
                self.sections
                    .to_vector()
                    .iter()
                    .map(|section| section.to_doc()),
            ))
            .append(RcDoc::text(quotes))
    }
}

impl ToDoc for InterpolatedStringSection {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            InterpolatedStringSection::String(string) => RcDoc::text(string),
            InterpolatedStringSection::Expression(expression) => RcDoc::text("${")
                .append(RcDoc::line())
                .append(expression.to_doc())
                .append(RcDoc::line())
                .append(RcDoc::text("}"))
                .group(),
        }
    }
}

impl ToDoc for OperatorCall {
    fn to_doc(&self) -> RcDoc<()> {
        self.left
            .to_doc()
            .append(RcDoc::space())
            .append(self.operator.to_doc())
            .append(RcDoc::line().nest(2).append(self.right.to_doc().nest(2)))
            .group()
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
        let arguments = self.arguments.to_vector();
        let groups = arguments
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

impl ToDoc for SemicolonArray {
    fn to_doc(&self) -> RcDoc<()> {
        intersperse_vec(
            self.nodes
                .to_vector()
                .iter()
                .map(|node| node.to_doc())
                .collect(),
            ";",
        )
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
