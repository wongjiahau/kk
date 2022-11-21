use crate::{inferred_ast, non_empty::NonEmpty, raw_ast::Token};

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
    Number(Number),
    Variant(Variant),
    TagOnlyVariant(Token),
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
pub enum InternalOp {
    Add(Expression, Expression),
    Multiply(Expression, Expression),
    LessThan(Expression, Expression),
    ReadFile {
        filename: Expression,
        callback: Expression,
    },
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

impl From<inferred_ast::InferredExpression> for Expression {
    fn from(expression: inferred_ast::InferredExpression) -> Self {
        match expression {
            inferred_ast::InferredExpression::Float { representation } => {
                Expression::Number(Number::Float64(representation.parse().unwrap()))
            }
            inferred_ast::InferredExpression::Integer { representation } => {
                Expression::Number(Number::Int64(representation.parse().unwrap()))
            }
            inferred_ast::InferredExpression::InterpolatedString { sections } => todo!(),
            inferred_ast::InferredExpression::Character { representation } => todo!(),
            inferred_ast::InferredExpression::Variable(_) => todo!(),
            inferred_ast::InferredExpression::EnumConstructor {
                constructor_name,
                payload,
            } => todo!(),
            inferred_ast::InferredExpression::BranchedFunction(_) => todo!(),
            inferred_ast::InferredExpression::FunctionCall(_) => todo!(),
            inferred_ast::InferredExpression::Record { key_value_pairs } => todo!(),
            inferred_ast::InferredExpression::RecordAccess {
                expression,
                property_name,
            } => todo!(),
            inferred_ast::InferredExpression::RecordUpdate {
                expression,
                updates,
            } => todo!(),
            inferred_ast::InferredExpression::Array { elements } => todo!(),
            inferred_ast::InferredExpression::Block {
                statements,
                return_value,
            } => todo!(),
            inferred_ast::InferredExpression::Unit => todo!(),
            inferred_ast::InferredExpression::String(_) => todo!(),
            inferred_ast::InferredExpression::Keyword(_) => todo!(),
        }
    }
}
