use crate::{
    non_empty::NonEmpty,
    raw_ast::{Position, StringLiteral, Token},
};

#[derive(Debug, Clone)]
pub enum Node {
    Array(Array),
    PrefixFunctionCall(PrefixFunctionCall),
    InfixFunctionCall(InfixFunctionCall),
    /// Operator call are right-associative
    OperatorCall(OperatorCall),
    Literal(Literal),
    SemicolonArray(SemicolonArray),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(StringLiteral),
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
impl SemicolonArray {
    fn position(&self) -> Position {
        self.nodes
            .first()
            .position()
            .join(self.nodes.last().position())
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub nodes: Vec<Node>,
    pub bracket: Bracket,
    pub has_trailing_comma: bool,
}

impl Array {
    pub fn position(&self) -> Position {
        self.bracket
            .opening
            .position
            .join(self.bracket.closing.position)
    }
}

#[derive(Debug, Clone)]
pub struct Bracket {
    pub kind: BracketKind,
    pub opening: Token,
    pub closing: Token,
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
    pub arguments: Vec<Node>,
}
impl PrefixFunctionCall {
    fn position(&self) -> Position {
        self.function
            .position()
            .join_maybe(self.arguments.last().map(|argument| argument.position()))
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
            Node::Array(array) => array.position(),
            Node::PrefixFunctionCall(prefix_function_call) => prefix_function_call.position(),
            Node::InfixFunctionCall(infix_function_call) => infix_function_call.position(),
            Node::OperatorCall(_) => todo!(),
            Node::Literal(literal) => literal.position(),
            Node::SemicolonArray(array) => array.position(),
        }
    }
}

impl Literal {
    pub fn position(&self) -> Position {
        match self {
            Literal::String(string_literal) => string_literal.position(),
            Literal::Integer(token)
            | Literal::Float(token)
            | Literal::Character(token)
            | Literal::Identifier(token)
            | Literal::Keyword(token) => token.position,
        }
    }
}
