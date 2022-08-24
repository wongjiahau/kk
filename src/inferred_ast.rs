use crate::{
    module::{ModuleUid, SymbolUid},
    non_empty::NonEmpty,
    raw_ast::{InfinitePatternKind, Position, Token},
    typ::{InstantiatedConstraint, Type},
    unify::InferExpressionResult,
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
pub enum InferredStatement {
    ImportStatement(InferredImportStatement),
    Let {
        exported: bool,
        left: InferredDestructurePattern,
        right: InferredExpression,
    },
    Expression(InferredExpression),
}

#[derive(Debug, Clone)]
pub struct InferredImportStatement {
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
pub enum InferredExpression {
    Unit,
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
        sections: Vec<InferredInterpolatedStringSection>,
    },
    Character {
        representation: String,
    },
    Variable(Identifier),

    /// This should be replace with a dictionary during the constraint resolvation.
    ConstrainedVariable {
        identifier: Identifier,
        constraints: NonEmpty<InstantiatedConstraint>,
    },
    EnumConstructor {
        constructor_name: String,
        payload: Option<Box<InferredExpression>>,
    },
    BranchedFunction(Box<InferredBranchedFunction>),
    FunctionCall(Box<InferredFunctionCall>),
    Record {
        key_value_pairs: Vec<(PropertyName, InferredExpression)>,
    },
    RecordAccess {
        expression: Box<InferredExpression>,
        property_name: PropertyName,
    },
    RecordUpdate {
        expression: Box<InferredExpression>,
        updates: Vec<InferredRecordUpdate>,
    },
    Array {
        elements: Vec<InferredExpression>,
    },
    Javascript {
        code: String,
    },
    Block {
        statements: Vec<InferredStatement>,
        return_value: Box<InferredExpression>,
    },
    PerformEffect {
        effect_name: Token,
        argument: Box<InferredExpression>,
    },
}

#[derive(Debug, Clone)]
pub enum InferredInterpolatedStringSection {
    String(String),
    Expression(Box<InferExpressionResult>),
}

/// This is to ensure that we transpile property name properly.
#[derive(Debug, Clone)]
pub struct PropertyName(pub Token);

#[derive(Debug, Clone)]
pub enum InferredRecordUpdate {
    ValueUpdate {
        property_name: PropertyName,
        new_value: InferredExpression,
    },
    FunctionalUpdate {
        property_name: PropertyName,
        function: InferredExpression,
    },
}
#[derive(Debug, Clone)]
pub struct InferredFunctionCall {
    pub function: Box<InferredExpression>,
    pub argument: Box<InferredExpression>,
}

#[derive(Debug, Clone)]
pub struct InferredBranchedFunction {
    pub branches: Box<NonEmpty<InferredFunctionBranch>>,
}

#[derive(Debug, Clone)]
pub struct InferredFunctionBranch {
    pub parameter: Box<InferredDestructurePattern>,
    pub body: Box<InferExpressionResult>,
}

#[derive(Debug, Clone)]
pub struct InferredDestructurePattern {
    pub type_value: Type,
    pub kind: InferredDestructurePatternKind,
}

#[derive(Debug, Clone)]
pub enum InferredDestructurePatternKind {
    Infinite {
        kind: InfinitePatternKind,
        token: Token,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
    },
    Underscore(Token),
    Identifier(Box<Identifier>),
    EnumConstructor {
        constructor_name: Token,
        payload: Option<Box<InferredDestructurePattern>>,
    },
    Record {
        left_curly_bracket: Token,
        key_pattern_pairs: Vec<(PropertyName, InferredDestructurePattern)>,
        right_curly_bracket: Token,
    },
    Array {
        left_square_bracket: Token,
        spread: Option<InferredDesturcturePatternArraySpread>,
        right_square_bracket: Token,
    },
    Tuple {
        patterns: Box<NonEmpty<InferredDestructurePattern>>,
    },
    Or {
        patterns: Box<NonEmpty<InferredDestructurePattern>>,
    },
}

#[derive(Debug, Clone)]
pub struct InferredDestructurePatternEnumConstructorPayload {
    pub left_parenthesis: Token,
    pub pattern: Box<InferredDestructurePattern>,
    pub right_parenthesis: Token,
}

impl InferredDestructurePatternKind {
    pub fn position(&self) -> Position {
        match self {
            InferredDestructurePatternKind::Infinite { token, .. }
            | InferredDestructurePatternKind::Boolean { token, .. }
            | InferredDestructurePatternKind::Underscore(token) => token.position,
            InferredDestructurePatternKind::Identifier(identifier) => identifier.token.position,
            InferredDestructurePatternKind::Unit {
                left_parenthesis,
                right_parenthesis,
            } => left_parenthesis.position.join(right_parenthesis.position),
            InferredDestructurePatternKind::EnumConstructor {
                constructor_name,
                payload,
                ..
            } => match payload {
                None => constructor_name.position,
                Some(payload) => constructor_name.position.join(payload.kind.position()),
            },
            InferredDestructurePatternKind::Record {
                left_curly_bracket,
                right_curly_bracket,
                ..
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            InferredDestructurePatternKind::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
            InferredDestructurePatternKind::Tuple { patterns }
            | InferredDestructurePatternKind::Or { patterns } => patterns.position(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InferredDesturcturePatternArraySpread {
    pub first_element: Box<InferredDestructurePattern>,
    pub rest_elements: Box<InferredDestructurePattern>,
}
