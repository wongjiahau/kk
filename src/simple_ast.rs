use crate::raw_ast::Token;

/// TODO: Change Pattern into a different type
pub type Pattern = Expression;

#[derive(Debug, Clone)]
pub enum Expression {
    Object(Object),
    ObjectAccess(ObjectAccess),
    Array(Array),
    Tuple(Tuple),
    FunctionCall(FunctionCall),
    Function(Function),
    String(String),
    Identifier(String),
    Number(Number),
    Variant(Variant),
    TagOnlyVariant(String),
    Match(Match),

    /// These are internal operations that cannot be called directly from userspace
    InternalOp(Box<InternalOp>),
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
}

#[derive(Debug, Clone)]
pub struct ObjectAccess {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub tag: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub left_curly_bracket: Token,
    pub pairs: Vec<ObjectPair>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct ObjectPair {
    pub pattern: Option<Pattern>,
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
    pub parameter: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Number {
    Int32(i32),
    Float32(f32),
}
