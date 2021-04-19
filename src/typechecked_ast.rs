use crate::{
    ast::{InfinitePatternKind, Position, Token},
    non_empty::NonEmpty,
    unify::join_position,
};
/// The syntax tree here represents the syntax tree that is type-checked
/// Which contain information necessary for the transpilation
/// For example, multiple-dispatch requires the typechecker
/// to point out which exact function is being used
///
/// NOTE for developers:
/// (1) To reduce the problem of impedance mismatched,
///     please use the same property name as in ast.rs whenever possible

#[derive(Debug, Clone)]
pub enum TypecheckedStatement {
    Let {
        left: Identifier,
        right: TypecheckedExpression,
    },
    Do {
        expression: TypecheckedExpression,
    },
}

#[derive(Debug, Clone)]
pub struct Identifier {
    /// This is needed for disambiguating overloaded functions
    pub uid: usize,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub enum TypecheckedExpression {
    Null,
    Boolean(bool),
    Float {
        representation: String,
    },
    Integer {
        representation: String,
    },
    String {
        representation: String,
    },
    InterpolatedString {
        sections: Vec<TypecheckedInterpolatedStringSection>,
    },
    Character {
        representation: String,
    },
    Variable(Identifier),
    EnumConstructor {
        constructor_name: String,
        payload: Option<Box<TypecheckedExpression>>,
    },
    BranchedFunction(Box<TypecheckedBranchedFunction>),
    FunctionCall(Box<TypecheckedFunctionCall>),
    Record {
        key_value_pairs: Vec<(PropertyName, TypecheckedExpression)>,
    },
    RecordAccess {
        expression: Box<TypecheckedExpression>,
        property_name: PropertyName,
    },
    RecordUpdate {
        expression: Box<TypecheckedExpression>,
        updates: Vec<TypecheckedRecordUpdate>,
    },
    Array {
        elements: Vec<TypecheckedExpression>,
    },
    Javascript {
        code: String,
    },
    If {
        condition: Box<TypecheckedExpression>,
        if_true: Box<TypecheckedExpression>,
        if_false: Box<TypecheckedExpression>,
    },
    Promise(Box<TypecheckedExpression>),
}

#[derive(Debug, Clone)]
pub enum TypecheckedInterpolatedStringSection {
    String(String),
    Expression(Box<TypecheckedExpression>),
}

/// This is to ensure that we transpile property name properly.
#[derive(Debug, Clone)]
pub struct PropertyName(pub Token);

#[derive(Debug, Clone)]
pub enum TypecheckedRecordUpdate {
    ValueUpdate {
        property_name: PropertyName,
        new_value: TypecheckedExpression,
    },
    FunctionalUpdate {
        property_name: PropertyName,
        function: TypecheckedExpression,
    },
}
#[derive(Debug, Clone)]
pub struct TypecheckedFunctionCall {
    pub function: Box<TypecheckedExpression>,
    pub first_argument: Box<TypecheckedExpression>,
    pub rest_arguments: Vec<TypecheckedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypecheckedBranchedFunction {
    pub branches: Box<NonEmpty<TypecheckedFunctionBranch>>,
}

#[derive(Debug, Clone)]
pub struct TypecheckedFunctionBranch {
    pub parameters: Box<NonEmpty<TypecheckedDestructurePattern>>,
    pub body: Box<TypecheckedExpression>,
}

#[derive(Debug, Clone)]
pub enum TypecheckedDestructurePattern {
    Infinite {
        kind: InfinitePatternKind,
        token: Token,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Null(Token),
    Underscore(Token),
    Identifier(Box<Identifier>),
    EnumConstructor {
        constructor_name: Token,
        payload: Option<TypecheckedDestructurePatternEnumConstructorPayload>,
    },
    Record {
        left_curly_bracket: Token,
        key_pattern_pairs: Vec<(PropertyName, TypecheckedDestructurePattern)>,
        right_curly_bracket: Token,
    },
    Array {
        left_square_bracket: Token,
        spread: Option<TypecheckedDesturcturePatternArraySpread>,
        right_square_bracket: Token,
    },
    Tuple {
        values: Box<NonEmpty<TypecheckedDestructurePattern>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypecheckedDestructurePatternEnumConstructorPayload {
    pub left_parenthesis: Token,
    pub pattern: Box<TypecheckedDestructurePattern>,
    pub right_parenthesis: Token,
}

impl TypecheckedDestructurePattern {
    pub fn position(&self) -> Position {
        match self {
            TypecheckedDestructurePattern::Infinite { token, .. }
            | TypecheckedDestructurePattern::Boolean { token, .. }
            | TypecheckedDestructurePattern::Null(token)
            | TypecheckedDestructurePattern::Underscore(token) => token.position,
            TypecheckedDestructurePattern::Identifier(identifier) => identifier.token.position,
            TypecheckedDestructurePattern::EnumConstructor {
                constructor_name,
                payload,
                ..
            } => match payload {
                None => constructor_name.position,
                Some(payload) => join_position(
                    constructor_name.position,
                    payload.right_parenthesis.position,
                ),
            },
            TypecheckedDestructurePattern::Record {
                left_curly_bracket,
                right_curly_bracket,
                ..
            } => join_position(left_curly_bracket.position, right_curly_bracket.position),
            TypecheckedDestructurePattern::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => join_position(left_square_bracket.position, right_square_bracket.position),
            TypecheckedDestructurePattern::Tuple { values } => {
                join_position(values.first().position(), values.last().position())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypecheckedDesturcturePatternArraySpread {
    pub first_element: Box<TypecheckedDestructurePattern>,
    pub rest_elements: Box<TypecheckedDestructurePattern>,
}
