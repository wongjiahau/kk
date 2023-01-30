use crate::{
    non_empty::NonEmpty,
    simple_ast::*,
    tokenize::{StringLiteral, Token},
};
use itertools::Itertools;
use pretty::{Arena, DocAllocator, DocBuilder};

pub trait ToDoc {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>>;
    fn to_pretty(&self) -> String {
        let arena = Arena::<()>::new();
        let mut vec = Vec::new();
        let width = 70;
        self.to_doc(&arena).render(width, &mut vec).unwrap();
        String::from_utf8(vec).unwrap()
    }
}

impl SExpr {
    pub fn list(nodes: Vec<SExpr>) -> SExpr {
        Self::List {
            opening: "(".to_string(),
            nodes,
            closing: ")".to_string(),
        }
    }

    pub fn atom<'a>(representation: &'a str) -> SExpr {
        Self::Atom {
            representation: representation.to_string(),
            is_keyword: false,
        }
    }
}

pub enum SExpr {
    List {
        opening: String,
        nodes: Vec<SExpr>,
        closing: String,
    },
    Atom {
        representation: String,
        is_keyword: bool,
    },
}
impl SExpr {
    fn is_keyword(&self) -> bool {
        match self {
            SExpr::Atom {
                is_keyword: true, ..
            } => true,
            _ => false,
        }
    }
}

impl ToDoc for SExpr {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            SExpr::Atom { representation, .. } => arena.text(representation),
            SExpr::List {
                opening,
                nodes,
                closing,
            } => {
                match nodes.split_first() {
                    None => arena.text(format!("{}{}", opening, closing)),
                    Some((head, tail)) => {
                        #[derive(Debug, Clone)]

                        struct KeywordGroup<'a, T> {
                            function: &'a T,
                            continuous_keywords: Vec<&'a T>,
                            arguments: Vec<&'a T>,
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
                        let mut groups = NonEmpty {
                            head: KeywordGroup {
                                function: head,
                                continuous_keywords: vec![],
                                arguments: vec![],
                            },
                            tail: vec![],
                        };
                        let mut previous_argument_is_keyword = true;
                        for argument in tail {
                            match argument.is_keyword() {
                                true => {
                                    if !previous_argument_is_keyword {
                                        previous_argument_is_keyword = true;
                                        groups.tail.push(KeywordGroup {
                                            function: argument,
                                            continuous_keywords: vec![],
                                            arguments: vec![],
                                        })
                                    } else {
                                        groups.last_mut().continuous_keywords.push(argument)
                                    }
                                }
                                _ => {
                                    previous_argument_is_keyword = false;
                                    groups.last_mut().arguments.push(argument)
                                }
                            }
                        }

                        let groups = groups.into_vector();
                        let internal = arena
                            .intersperse(
                                groups.into_iter().map(|keyword_group| {
                                    keyword_group
                                        .function
                                        .to_doc(arena)
                                        .append(arena.concat(
                                            keyword_group.continuous_keywords.iter().map(|token| {
                                                arena.softline().append(token.to_doc(arena))
                                            }),
                                        ))
                                        .append(
                                            arena
                                                .concat(keyword_group.arguments.iter().map(
                                                    |argument| {
                                                        arena
                                                            .line()
                                                            .append(argument.to_doc(arena))
                                                            .nest(2)
                                                    },
                                                ))
                                                .group(),
                                        )
                                }),
                                arena.line().nest(2),
                            )
                            .group();

                        arena
                            .text(opening)
                            .append(internal)
                            .append(arena.text(closing))
                    }
                }
            }
        }
    }
}

fn brackets<'a>(
    opening: &'a str,
    doc: DocBuilder<'a, Arena<'a, ()>>,
    closing: &'a str,
    add_space: bool,
    arena: &'a Arena<'a>,
) -> DocBuilder<'a, Arena<'a, ()>> {
    let line = if add_space {
        arena.line()
    } else {
        arena.line_()
    };
    arena
        .text(opening)
        .append(doc)
        .append(arena.text(closing))
        .group()
}

impl<T: ToDoc> NonEmpty<T> {
    fn intersperse<'a>(
        &'a self,
        separator: &'a str,
        arena: &'a Arena<'a>,
    ) -> DocBuilder<'a, Arena<'a, ()>> {
        arena.intersperse(
            vec![self.head.to_doc(arena)]
                .into_iter()
                .chain(self.tail.iter().map(|element| element.to_doc(arena))),
            arena.text(separator).append(arena.line()),
        )
    }
}

fn intersperse_vec<'a>(
    elements: Vec<DocBuilder<'a, Arena<'a, ()>>>,
    separator: &'a str,
    arena: &'a Arena<'a>,
) -> DocBuilder<'a, Arena<'a, ()>> {
    arena.intersperse(elements, arena.text(separator).append(arena.line()))
}

impl ToDoc for Token {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        arena.text(self.representation.as_str())
    }
}

impl ToDoc for MacroExpandedNode {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            MacroExpandedNode::List(list) => list.to_doc(arena),
            MacroExpandedNode::Literal(literal) => literal.to_doc(arena),
            MacroExpandedNode::Unit { .. } => arena.text("()"),
        }
    }
}

impl ToDoc for MacroExpandedList {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        arena
            .text("(")
            .append(
                arena.intersperse(
                    self.nodes
                        .to_vector()
                        .into_iter()
                        .map(|node| node.to_doc(arena)),
                    arena.line(),
                ),
            )
            .append(arena.text(")"))
            .group()
    }
}

impl ToDoc for Node {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            Node::List(List { nodes, bracket }) => {
                let brackets = match bracket.kind {
                    BracketKind::Round => ("(", ")"),
                    BracketKind::Square => ("[", "]"),
                    BracketKind::Curly => ("{", "}"),
                };
                match nodes.split_first() {
                    None => arena.text("()"),
                    Some((head, tail)) => {
                        #[derive(Debug, Clone)]

                        struct KeywordGroup<'a> {
                            function: &'a Node,
                            continuous_keywords: Vec<&'a Token>,
                            arguments: Vec<&'a Node>,
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
                        let mut groups = NonEmpty {
                            head: KeywordGroup {
                                function: head,
                                continuous_keywords: vec![],
                                arguments: vec![],
                            },
                            tail: vec![],
                        };
                        let mut previous_argument_is_keyword = true;
                        for argument in tail {
                            match argument {
                                Node::Literal(
                                    Literal::Keyword(keyword) | Literal::Identifier(keyword),
                                ) => {
                                    if !previous_argument_is_keyword {
                                        previous_argument_is_keyword = true;
                                        groups.tail.push(KeywordGroup {
                                            function: argument,
                                            continuous_keywords: vec![],
                                            arguments: vec![],
                                        })
                                    } else {
                                        groups.last_mut().continuous_keywords.push(keyword)
                                    }
                                }
                                _ => {
                                    previous_argument_is_keyword = false;
                                    groups.last_mut().arguments.push(argument)
                                }
                            }
                        }

                        let groups = groups.into_vector();
                        let internal = arena
                            .intersperse(
                                groups.into_iter().map(|keyword_group| {
                                    keyword_group
                                        .function
                                        .to_doc(arena)
                                        .append(arena.concat(
                                            keyword_group.continuous_keywords.iter().map(|token| {
                                                arena.softline().append(token.to_doc(arena))
                                            }),
                                        ))
                                        .append(
                                            arena
                                                .concat(keyword_group.arguments.iter().map(
                                                    |argument| {
                                                        arena
                                                            .line()
                                                            .append(argument.to_doc(arena))
                                                            .nest(2)
                                                    },
                                                ))
                                                .group(),
                                        )
                                }),
                                arena.line().nest(2),
                            )
                            .group();

                        let (opening, closing) = match bracket.kind {
                            BracketKind::Round => ("(", ")"),
                            BracketKind::Square => ("[", "]"),
                            BracketKind::Curly => ("{", "}"),
                        };
                        arena
                            .text(opening)
                            .append(internal)
                            .append(arena.text(closing))
                    }
                }
            }
            Node::Literal(literal) => literal.to_doc(arena),
            Node::Comment(comment) => arena.reflow(comment.representation.as_str()),
        }
    }
}

impl ToDoc for TopLevelArray {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        arena.intersperse(
            self.nodes
                .to_vector()
                .into_iter()
                .map(|node| node.to_pretty()),
            arena.concat(vec![arena.hardline(), arena.hardline()]),
        )
    }
}

impl ToDoc for StringLiteral {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        let quotes = self
            .start_quotes
            .to_vector()
            .into_iter()
            .map(|character| character.value)
            .join("");
        let quotes_len = quotes.len();
        let quotes_doc = arena.text(quotes);
        let comment = arena.text(&self.content);
        quotes_doc.clone().append(comment).append(quotes_doc)
    }
}

impl ToDoc for Literal {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            Literal::String(literal) => literal.to_doc(arena),
            Literal::Integer(token)
            | Literal::Character(token)
            | Literal::Float(token)
            | Literal::Identifier(token)
            | Literal::Keyword(token) => token.to_doc(arena),
            Literal::InterpolatedString(literal) => literal.to_doc(arena),
        }
    }
}

impl ToDoc for InterpolatedString {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        let quotes = self
            .start_quotes
            .to_vector()
            .iter()
            .map(|quote| quote.value)
            .join(",");
        arena
            .text(quotes.clone())
            .append(
                arena.concat(
                    self.sections
                        .to_vector()
                        .iter()
                        .map(|section| section.to_doc(arena)),
                ),
            )
            .append(arena.text(quotes))
    }
}

impl ToDoc for InterpolatedStringSection {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        match self {
            InterpolatedStringSection::String(string) => arena.text(string),
            InterpolatedStringSection::Expression(expression) => arena
                .text("${")
                .append(arena.line())
                .append(expression.to_doc(arena))
                .append(arena.line())
                .append(arena.text("}"))
                .group(),
        }
    }
}

impl ToDoc for List {
    fn to_doc<'a>(&'a self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        let (opening, closing, add_space) = match self.bracket.kind {
            BracketKind::Round => ("(", ")", false),
            BracketKind::Square => ("[", "]", false),
            BracketKind::Curly => ("{", "}", true),
        };
        brackets(
            opening,
            intersperse_vec(
                self.nodes
                    .iter()
                    .map(|element| element.to_doc(arena))
                    .collect(),
                ",",
                arena,
            ),
            closing,
            add_space,
            arena,
        )
    }
}

pub fn prettify_code<'a>(code: &'a str) -> String {
    use crate::{parse_simple::Parser, tokenize::Tokenizer};
    let array = Parser::parse(&mut Tokenizer::new(code.to_string())).unwrap();

    array.to_pretty()
}

// #[cfg(test)]
// mod formatter_test {
//     #[test]
//     fn infix_function_call_1() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code(
//             "hello .replace (x .call me spongebob (yes) you are squarepants) with (y) .to string .between 1 and 3 .who lives in a pineapple under the sea"
//         ));
//     }

//     #[test]
//     fn prefix_function_call_keywords_should_be_grouped_1() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code(
//             "if { krabby patty is alive } then { spongebob squarepants + patrick star .replace squidward with hello yo yo  yo yo yo y } else { clarinet }"
//         ));
//     }

//     #[test]
//     fn prefix_function_call_multiple_arguments_1() {
//         use crate::formatter::prettify_code;
//         insta::assert_snapshot!(prettify_code(
//                 "x = unwrap ~ some { x = some \"hi\" .!, y = some 2 .!, z = unwrap ~ some { x = none .!, y = some 2 .! } }"
//         ))
//     }

//     #[test]
//     fn prefix_function_call_continuous_keywords_should_be_in_one_line() {
//         use crate::formatter::prettify_code;
//         insta::assert_snapshot!(prettify_code(
//             "export let (a b => if (condition : Boolean) (t : () -> A) else (f : () -> A) : A)"
//         ))
//     }

//     #[test]
//     fn operator_1() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code(
//             "who + lives + in + a + pineapple + under + the + sea ? spongebob squarepants ! absorbent - and - yellow "
//         ));
//     }

//     #[test]
//     fn comments_more_than_two_quotes_should_be_forced_newline() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code("\"\"\"This is a comment \"\"\" hello world"));
//     }

//     #[test]
//     fn content_after_multiline_string_should_follow_indentation() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code(
//             "who lives in a pineapple under the sea (\"\"\"Hello world\"\"\" 234) (123) (456)"
//         ));
//     }

//     #[test]
//     fn comment_surrounding_whitespace_should_be_trimmed() {
//         use crate::formatter::prettify_code;

//         insta::assert_snapshot!(prettify_code(
//             "\"\"\"   This is a    comment   \"\"\" hello world"
//         ));
//     }
// }
