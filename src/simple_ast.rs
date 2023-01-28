use crate::{
    non_empty::NonEmpty,
    raw_ast::Position,
    tokenize::{Character, StringLiteral, Token},
    unify::Positionable,
};

#[derive(Debug, Clone)]
pub enum Node {
    List(List),
    Literal(Literal),
    Comment(Token),
}

#[derive(Debug, Clone)]
pub enum MacroExpandedNode {
    List(MacroExpandedList),
    Literal(Literal),
    Unit { bracket: Bracket },
}
impl Positionable for MacroExpandedNode {
    fn position(&self) -> Position {
        match self {
            MacroExpandedNode::List(list) => list.position(),
            MacroExpandedNode::Literal(literal) => literal.position(),
            MacroExpandedNode::Unit { bracket } => bracket.position(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MacroExpandedList {
    pub nodes: Box<NonEmpty<MacroExpandedNode>>,
}
impl MacroExpandedList {
    pub fn position(&self) -> Position {
        self.nodes.position()
    }

    pub fn head(&self) -> &MacroExpandedNode {
        &self.nodes.head
    }

    pub fn tail(&self) -> Vec<MacroExpandedNode> {
        self.nodes.tail().to_vec()
    }

    pub fn init(&self) -> Vec<&MacroExpandedNode> {
        self.nodes.init()
    }

    pub fn last(&self) -> &MacroExpandedNode {
        self.nodes.last()
    }

    pub fn first(&self) -> &MacroExpandedNode {
        self.nodes.first()
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

pub struct TopLevelArray {
    pub nodes: NonEmpty<Node>,
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

    fn position(&self) -> Position {
        self.opening.position.join(self.closing.position)
    }
}

#[derive(Debug, Clone)]
pub enum BracketKind {
    Round,
    Square,
    Curly,
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::List(array) => array.position(),
            Node::Literal(literal) => literal.position(),
            Node::Comment(comment) => comment.position,
        }
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
