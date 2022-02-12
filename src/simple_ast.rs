use crate::raw_ast::Token;

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
    TagOnlyVariant(TagOnlyVariant),
}

pub struct ObjectAccess {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct TagOnlyVariant {
    pub tag: String,
}

pub struct Variant {
    pub tag: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct Object {
    pub left_curly_bracket: Token,
    pub pairs: Vec<ObjectPair>,
    pub right_curly_bracket: Token,
}

pub struct ObjectPair {
    pub pattern: Expression,
    pub value: Expression,
}

pub struct Tuple {
    pub left_parenthesis: Token,
    pub values: Vec<Expression>,
    pub right_parenthesis: Token,
}

pub struct Array {
    pub values: Vec<Expression>,
}

pub struct FunctionCall {
    pub left_argument: Box<Expression>,
    pub function: Box<Expression>,
    pub right_argument: Box<Expression>,
}

pub struct Function {
    pub parameter: Box<Expression>,
    pub body: Box<Expression>,
}

pub enum Number {
    Int32(i32),
    Float32(f32),
}
