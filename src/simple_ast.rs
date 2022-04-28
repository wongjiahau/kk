use crate::{non_empty::NonEmpty, raw_ast::Token};

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(Token),
    Object {
        left_parenthesis: Token,
        pairs: Vec<(Token, Pattern)>,
        right_parenthesis: Token,
    },
    Variant {
        left: Box<Pattern>,
        tag: Token,
        right: Box<Pattern>,
    },
    TagOnlyVariant(Token),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Object(Object),
    ObjectAccess(ObjectAccess),
    Array(Array),
    FunctionCall(FunctionCall),
    Function(Function),
    String(Token),
    Identifier(Token),
    Branch(Box<Branch>),
    Number(Number),
    Variant(Variant),
    TagOnlyVariant(Token),
    Conditional(Conditional),
    Parenthesized(Parenthesized),

    /// These are internal operations that cannot be called directly from userspace
    InternalOp(Box<InternalOp>),
    EffectHandlerNode(EffectHandlerNode),
    /// The syntax for perform is bang `!`.
    Perform(Box<PerformEffect>),
}

#[derive(Debug, Clone)]
pub struct PerformEffect {
    pub name: Token,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct EffectHandlerNode {
    pub handler: Handler,
    /// This is what goes belows the handler in the AST
    pub handled: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Handler {
    pub name: Token,
    pub function: Function,
}

#[derive(Debug, Clone)]
pub struct Parenthesized {
    pub left_parenthesis: Token,
    pub expression: Box<Expression>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub pattern: Box<Pattern>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub default: Box<Expression>,
    pub branches: Vec<Branch>,
}

/// deprecated: this can cause the type system to be unsound
#[derive(Debug, Clone)]
pub struct Branch {
    pub condition: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub value: Box<Expression>,
    pub cases: Vec<MatchCase>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum InternalOp {
    Add(Expression, Expression),
    Multiply(Expression, Expression),
    LessThan(Expression, Expression),
}

#[derive(Debug, Clone)]
pub struct ObjectAccess {
    pub object: Box<Expression>,
    pub property: Token,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub tag: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub left_parenthesis: Token,
    pub pairs: Vec<ObjectPair>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct ObjectPair {
    pub key: Pattern,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub left_parenthesis: Token,
    pub values: Vec<Expression>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub argument: Box<Expression>,
    pub function: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub left_curly_bracket: Token,
    pub branches: NonEmpty<FunctionBranch>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub parameter: Box<Pattern>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Number {
    Int64(i64),
    Float64(f64),
}
