use crate::raw_ast::{StringLiteral, Token};

#[derive(Debug)]
pub enum Expression {
    Array(Array),
    PrefixFunctionCall(PrefixFunctionCall),
    InfixFunctionCall(InfixFunctionCall),
    /// Operator call are right-associative
    OperatorCall(OperatorCall),
    String(StringLiteral),
    Integer(Token),
    Float(Token),
    Character(Token),
    Identifier(Token),
}

#[derive(Debug)]
pub struct Array {
    pub elements: Vec<Expression>,
    pub bracket: Bracket,
}

#[derive(Debug)]
pub enum Bracket {
    Round,
    Square,
    Curly,
    /// For top-level elements only
    None,
}

#[derive(Debug)]
pub struct InfixFunctionCall {
    pub head: Box<Expression>,
    pub tail: Vec<Expression>,
}

#[derive(Debug)]
pub struct PrefixFunctionCall {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub struct OperatorCall {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}
