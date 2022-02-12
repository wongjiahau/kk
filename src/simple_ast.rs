pub enum Expression {
    Object(Object),
    ObjectAccess(ObjectAccess),
    Array(Array),
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
    pub pairs: Vec<(Expression, Expression)>,
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
