use crate::{
    non_empty::NonEmpty,
    raw_ast::Position,
    tokenize::{Character, StringLiteral, Token},
};

#[derive(Debug, Clone)]
pub enum Node {
    List(List),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct CommentedNode {
    pub comment: Comment,
    pub node: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Comment(pub StringLiteral);

impl Comment {
    fn position(&self) -> Position {
        self.0.position()
    }
}

#[derive(Debug, Clone)]
pub struct InterpolatedString {
    pub start_quotes: NonEmpty<Character>,
    pub sections: NonEmpty<InterpolatedStringSection>,
    pub end_quotes: NonEmpty<Character>,
}

impl InterpolatedString {
    pub fn position(&self) -> Position {
        self.start_quotes
            .first()
            .position()
            .join(self.end_quotes.last().position())
    }
}

#[derive(Debug, Clone)]
pub enum InterpolatedStringSection {
    String(String),
    Expression(Box<Node>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(StringLiteral),
    InterpolatedString(InterpolatedString),
    Integer(Token),
    Float(Token),
    Character(Token),
    Identifier(Token),
    Keyword(Token),
}

#[derive(Debug, Clone)]
pub struct SemicolonArray {
    pub nodes: Box<NonEmpty<Node>>,
}

pub struct TopLevelArray {
    pub nodes: NonEmpty<Node>,
}

impl SemicolonArray {
    fn position(&self) -> Position {
        self.nodes
            .first()
            .position()
            .join(self.nodes.last().position())
    }
}

#[derive(Debug, Clone)]
pub struct List {
    pub nodes: Vec<Node>,
    pub bracket: Bracket,
}

impl List {
    pub fn new(nodes: Vec<Node>) -> List {
        List {
            nodes,
            bracket: Bracket::dummy(),
        }
    }
    pub fn position(&self) -> Position {
        match self.nodes.split_first() {
            None => self
                .bracket
                .opening
                .position
                .join(self.bracket.closing.position),
            Some((head, tail)) => head
                .position()
                .join_maybe(tail.get(0).map(|node| node.position())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Bracket {
    pub kind: BracketKind,
    pub opening: Token,
    pub closing: Token,
}

impl Bracket {
    pub fn dummy() -> Self {
        Self {
            kind: BracketKind::Round,
            opening: Token::dummy(),
            closing: Token::dummy(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BracketKind {
    Round,
    Square,
    Curly,
}

#[derive(Debug, Clone)]
pub struct InfixFunctionCall {
    pub head: Box<Node>,
    pub tail: Vec<Node>,
}
impl InfixFunctionCall {
    fn position(&self) -> Position {
        self.head
            .position()
            .join_maybe(self.tail.last().map(|node| node.position()))
    }
}

#[derive(Debug, Clone)]
pub struct PrefixFunctionCall {
    pub function: Box<Node>,
    pub arguments: Box<NonEmpty<Node>>,
}
impl PrefixFunctionCall {
    fn position(&self) -> Position {
        self.function
            .position()
            .join(self.arguments.last().position())
    }
}

#[derive(Debug, Clone)]
pub struct OperatorCall {
    pub left: Box<Node>,
    pub operator: Token,
    pub right: Box<Node>,
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::List(array) => array.position(),
            Node::Literal(literal) => literal.position(),
        }
    }
}

impl CommentedNode {
    pub fn position(&self) -> Position {
        self.comment.position().join(self.node.position())
    }
}

impl Literal {
    pub fn position(&self) -> Position {
        match self {
            Literal::String(string_literal) => string_literal.position(),
            Literal::InterpolatedString(literal) => literal.position(),
            Literal::Integer(token)
            | Literal::Float(token)
            | Literal::Character(token)
            | Literal::Identifier(token)
            | Literal::Keyword(token) => token.position,
        }
    }
}
