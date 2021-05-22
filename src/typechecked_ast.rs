use crate::{
    ast::{InfinitePatternKind, Position, Token, Type, TypecheckedConstraint},
    module::{ModuleUid, SymbolUid},
    non_empty::NonEmpty,
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
    ImportStatement(TypecheckedImportStatement),
    Let {
        exported: bool,
        left: TypecheckedDestructurePattern,
        right: TypecheckedExpression,
    },
    Expression(TypecheckedExpression),
}

#[derive(Debug, Clone)]
pub struct TypecheckedImportStatement {
    pub module_uid: ModuleUid,

    /// The name of the symbol being imported
    pub imported_name: Identifier,

    /// The name of the symbol being imported as
    pub imported_as: Identifier,
}
/// Identifier is also known as variable
#[derive(Debug, Clone)]
pub struct Identifier {
    /// This is needed for disambiguating overloaded functions
    pub uid: SymbolUid,
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

    /// This should be replace with a dictionary during the constraint resolvation.
    ConstrainedVariable {
        identifier: Identifier,
        constraints: NonEmpty<TypecheckedConstraint>,
    },
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
    Block {
        statements: Vec<TypecheckedStatement>,
        return_value: Box<TypecheckedExpression>,
    },
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
pub struct TypecheckedDestructurePattern {
    pub type_value: Type,
    pub kind: TypecheckedDestructurePatternKind,
}

#[derive(Debug, Clone)]
pub enum TypecheckedDestructurePatternKind {
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
        patterns: Box<NonEmpty<TypecheckedDestructurePattern>>,
    },
    Or {
        patterns: Box<NonEmpty<TypecheckedDestructurePattern>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypecheckedDestructurePatternEnumConstructorPayload {
    pub left_parenthesis: Token,
    pub pattern: Box<TypecheckedDestructurePattern>,
    pub right_parenthesis: Token,
}

impl TypecheckedDestructurePatternKind {
    pub fn position(&self) -> Position {
        match self {
            TypecheckedDestructurePatternKind::Infinite { token, .. }
            | TypecheckedDestructurePatternKind::Boolean { token, .. }
            | TypecheckedDestructurePatternKind::Null(token)
            | TypecheckedDestructurePatternKind::Underscore(token) => token.position,
            TypecheckedDestructurePatternKind::Identifier(identifier) => identifier.token.position,
            TypecheckedDestructurePatternKind::EnumConstructor {
                constructor_name,
                payload,
                ..
            } => match payload {
                None => constructor_name.position,
                Some(payload) => constructor_name
                    .position
                    .join(payload.right_parenthesis.position),
            },
            TypecheckedDestructurePatternKind::Record {
                left_curly_bracket,
                right_curly_bracket,
                ..
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            TypecheckedDestructurePatternKind::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
            TypecheckedDestructurePatternKind::Tuple { patterns }
            | TypecheckedDestructurePatternKind::Or { patterns } => patterns.position(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypecheckedDesturcturePatternArraySpread {
    pub first_element: Box<TypecheckedDestructurePattern>,
    pub rest_elements: Box<TypecheckedDestructurePattern>,
}
