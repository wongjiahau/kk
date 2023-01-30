use crate::{
    module::Access,
    non_empty::NonEmpty,
    tokenize::{Character, StringLiteral, Token},
    unify::Positionable,
};
/// The syntax tree here represents raw syntax tree that is not type checked

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),

    /// This represent type alias definition.
    Type(TypeAliasStatement),

    /// This represents named sum types (a.k.a tagged union).
    Enum(EnumStatement),

    Import(ImportStatement),

    /// This represents the entry points of a module.
    /// Will be ignored for imported modules.
    Entry(EntryStatement),
}

#[derive(Debug, Clone)]
pub struct ImportStatement {
    pub access: Access,
    pub keyword_import: Token,
    pub url: StringLiteral,
    pub specification: Option<ImportStatementSpecification>,
}

#[derive(Debug, Clone)]
pub struct ImportStatementSpecification {
    pub left_curly_bracket: Token,
    pub aliases: Vec<ImportSpecificationAlias>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct ImportSpecificationAlias {
    pub name: Token,
    pub alias: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct EntryStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeVariablesDeclaration {
    pub type_variables: NonEmpty<Token>,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub access: Access,
    pub keyword_let: Token,
    pub name: Token,
    pub doc_string: Option<StringLiteral>,
    pub type_annotation: TypeAnnotation,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub pattern: DestructurePattern,
    pub type_annotation: TypeAnnotation,
}
impl Parameter {
    pub fn position(&self) -> Position {
        self.pattern
            .position()
            .join(self.type_annotation.position())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
    pub name: Token,
    pub parameters: NonEmpty<Parameter>,
    pub return_type: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct TypeAliasStatement {
    pub access: Access,
    pub keyword_type: Token,
    pub left: Token,
    pub right: TypeAnnotation,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
}

#[derive(Debug, Clone)]
pub struct EnumStatement {
    pub access: Access,
    pub keyword_enum: Token,
    pub name: Token,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
    pub constructors: Vec<EnumConstructorDefinition>,
}

impl EnumStatement {
    pub fn type_variables(&self) -> Vec<Token> {
        get_type_variables(&self.type_variables_declaration)
    }
}

#[derive(Debug, Clone)]
pub struct EnumConstructorDefinition {
    pub name: Token,
    pub payload: Option<Box<EnumConstructorDefinitionPayload>>,
}

#[derive(Debug, Clone)]
pub struct EnumConstructorDefinitionPayload {
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum SymbolSource {
    BuiltIn,
    UserDefined {
        filename: String,
        position: Position,
    },
}

#[derive(Debug, Clone)]
pub struct NamedTypeAnnotationArguments {
    pub left_angular_bracket: Token,
    pub arguments: Vec<TypeAnnotation>,
    pub right_angular_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct TypeConstraintsAnnotation {
    pub left_curly_bracket: Token,
    pub right_curly_bracket: Token,
    pub type_constraints: Vec<TypeConstraintAnnotation>,
}

impl TypeConstraintsAnnotation {
    pub fn position(&self) -> Position {
        self.left_curly_bracket
            .position
            .join(self.right_curly_bracket.position)
    }
}

#[derive(Debug, Clone)]
pub struct TypeConstraintAnnotation {
    pub identifier: Token,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    Parenthesized {
        left_parenthesis: Token,
        type_annotation: Box<TypeAnnotation>,
        right_parenthesis: Token,
    },
    Scheme {
        type_variables: TypeVariablesDeclaration,
        type_annotation: Box<TypeAnnotation>,
    },
    Named {
        name: Token,
        type_arguments: Option<TypeArguments>,
    },
    Record {
        prefix: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
    },
    Array {
        hash_left_square_bracket: Token,
        element_type: Box<TypeAnnotation>,
        right_square_bracket: Token,
    },
    Underscore(Token),
    Function(FunctionTypeAnnotation),
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
    },
    Keyword {
        identifier: Token,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionTypeAnnotation {
    pub parameter: Box<TypeAnnotation>,
    pub return_type: Box<TypeAnnotation>,
    pub type_constraints: Option<TypeConstraintsAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfinitePatternKind {
    String,
    Character,
    Integer,
}

pub type OrDestructurePattern = Box<NonEmpty<DestructurePattern>>;

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    Or(OrDestructurePattern),
    Infinite {
        kind: InfinitePatternKind,
        token: Token,
    },
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
    },
    Underscore(Token),
    Identifier(Token),
    EnumConstructor {
        name: Token,
        payload: Option<Box<DestructurePattern>>,
    },
    Record {
        prefix: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
    },
    Parenthesized {
        left_parenthesis: Token,
        type_annotation: Option<TypeAnnotation>,
        pattern: Box<DestructurePattern>,
        right_parenthesis: Token,
    },
    Array {
        left_square_bracket: Token,
        right_square_bracket: Token,
        spread: Option<DestructurePatternArraySpread>,
    },
    Tuple(DestructurePatternTuple),
}

#[derive(Debug, Clone)]
pub struct DestructurePatternEnumConstructorPayload {
    pub left_parenthesis: Token,
    pub pattern: DestructurePattern,
    pub right_parenthesis: Token,
}
#[derive(Debug, Clone)]
pub struct DestructurePatternTuple {
    pub parentheses: Option<(/*left*/ Token, /*right*/ Token)>,
    pub values: Box<NonEmpty<DestructurePattern>>,
}

#[derive(Debug, Clone)]
pub struct DestructurePatternArraySpread {
    pub first_element: Box<DestructurePattern>,
    pub spread_token: Token,
    pub rest_elements: Box<DestructurePattern>,
}

#[derive(Debug, Clone)]
pub struct DestructuredRecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub as_value: Option<DestructurePattern>,
}
impl DestructuredRecordKeyValue {
    pub fn position(&self) -> Position {
        self.key
            .position
            .join_maybe(self.as_value.as_ref().map(|node| node.position()))
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Statements {
        current: Box<Expression>,
        next: Box<Expression>,
    },
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
    },
    Tuple {
        left_parenthesis: Token,
        elements: Box<NonEmpty<Expression>>,
        right_parenthesis: Token,
    },
    Parenthesized {
        left_parenthesis: Token,
        right_parenthesis: Token,
        value: Box<Expression>,
    },
    Float(Token),
    Integer(Token),
    String(StringLiteral),
    InterpolatedString {
        start_quotes: NonEmpty<Character>,
        sections: NonEmpty<InterpolatedStringSection>,
        end_quotes: NonEmpty<Character>,
    },
    Character(Token),
    Identifier(Token),
    Keyword(Token),
    EnumConstructor {
        name: Token,
        payload: Option<Box<Expression>>,
    },

    Function(Box<Function>),

    FunctionCall(Box<FunctionCall>),
    Record {
        prefix: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<RecordKeyValue>,
    },
    RecordAccess {
        expression: Box<Expression>,
        property_name: Token,
    },
    RecordUpdate {
        expression: Box<Expression>,
        updates: Vec<RecordUpdate>,
    },
    Array {
        hash_left_square_bracket: Token,
        elements: Vec<Expression>,
        right_square_bracket: Token,
    },
    Let {
        left: DestructurePattern,
        right: Box<Expression>,
        type_annotation: Option<TypeAnnotation>,
        body: Box<Expression>,
    },
    CpsBang {
        argument: Box<Expression>,
        bang: Token,
    },
    TildeClosure(TildeClosure),
    IntrinsicFunctionCall(IntrinsicFunctionCall),
}

#[derive(Debug, Clone)]
pub struct IntrinsicFunctionCall {
    pub function_name: Token,
    pub argument: Box<Expression>,
}
impl IntrinsicFunctionCall {
    pub fn position(&self) -> Position {
        self.function_name.position.join(self.argument.position())
    }
}

#[derive(Debug, Clone)]
pub struct TildeClosure {
    pub tilde: Token,
    pub bind_function: Box<Expression>,

    /// The expression desugared into CPS
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExpressionEnumConstructorPayload {
    pub left_parenthesis: Token,
    pub expression: Expression,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub enum RecordUpdate {
    /// For example, `x.{ a 3 }`
    ValueUpdate {
        property_name: Token,
        new_value: Expression,
    },
}

#[derive(Debug, Clone)]
pub struct RecordKeyValue {
    pub key: Token,
    pub value: Expression,
}
impl RecordKeyValue {
    pub fn position(&self) -> Position {
        self.key.position.join(self.value.position())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub argument: Box<Expression>,
    pub type_arguments: Option<TypeArguments>,
}
impl FunctionCall {
    pub fn position(&self) -> Position {
        self.function.position().join(self.argument.position())
    }
}

#[derive(Debug, Clone)]
pub struct TypeArguments {
    pub type_annotations: Box<NonEmpty<TypeAnnotation>>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallRestArguments {
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub branches: NonEmpty<FunctionBranch>,
}

#[derive(Debug, Clone)]
pub struct ArrowFunction {
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
    pub parameters: ArrowFunctionParameters,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub body: Box<Expression>,
}

impl ArrowFunction {
    pub fn type_variables(&self) -> Vec<Token> {
        get_type_variables(&self.type_variables_declaration)
    }
}

fn get_type_variables(type_variables_declaration: &Option<TypeVariablesDeclaration>) -> Vec<Token> {
    match type_variables_declaration {
        Some(declaration) => declaration.type_variables.clone().into_vector(),
        None => vec![],
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParameters {
    pub parameters: Box<NonEmpty<FunctionParameter>>,
}

#[derive(Debug, Clone)]
pub enum ArrowFunctionParameters {
    WithoutParenthesis(DestructurePattern),
    WithParenthesis(Box<FunctionParameters>),
}

impl ArrowFunctionParameters {
    pub fn parameters(self) -> NonEmpty<FunctionParameter> {
        match self {
            ArrowFunctionParameters::WithParenthesis(parameters) => *parameters.parameters,
            ArrowFunctionParameters::WithoutParenthesis(pattern) => NonEmpty {
                head: FunctionParameter {
                    pattern,
                    type_annotation: Box::new(None),
                },
                tail: vec![],
            },
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            ArrowFunctionParameters::WithoutParenthesis(_) => 1,
            ArrowFunctionParameters::WithParenthesis(parameters) => parameters.parameters.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub pattern: DestructurePattern,
    pub type_annotation: Box<Option<TypeAnnotation>>,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub parameter: Box<DestructurePattern>,
    pub body: Box<Expression>,
}

// #[derive(Debug, Clone)]
// pub enum FunctionParameters {
//     NoParenthesis {
//         parameter: FunctionParameter,
//     },
//     WithParenthesis {
//         left_parenthesis: Token,
//         parameters: Box<NonEmpty<FunctionParameter>>,
//         right_parenthesis: Token,
//     },
// }

#[derive(Debug, Clone)]
pub struct FunctionBranchRestArguments {
    pub left_parenthesis: Token,
    pub rest_arguments: Vec<FunctionParameter>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct InterpolatedString {
    pub start_quotes: NonEmpty<Character>,
    pub sections: NonEmpty<InterpolatedStringSection>,
    pub end_quotes: NonEmpty<Character>,
}

#[derive(Debug, Clone)]
pub enum InterpolatedStringSection {
    String(String),
    Expression(Box<Expression>),
}

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Position {
    /// First line is zero.
    pub line_start: usize,
    pub line_end: usize,

    /// First column is zero.
    pub column_start: usize,
    pub column_end: usize,

    /// First character is zero.
    pub character_index_start: usize,
    pub character_index_end: usize,
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Position ({}:{})", self.line_start, self.column_start)
    }
}

impl Position {
    pub fn dummy() -> Position {
        Position {
            line_start: 0,
            line_end: 0,
            column_start: 0,
            column_end: 0,
            character_index_start: 0,
            character_index_end: 0,
        }
    }
}
impl ImportStatementSpecification {
    pub fn position(&self) -> Position {
        self.left_curly_bracket
            .position
            .join(self.right_curly_bracket.position)
    }
}

impl TypeVariablesDeclaration {
    pub fn position(&self) -> Position {
        self.type_variables
            .head
            .position
            .join(self.type_variables.last().position)
    }
}
impl FunctionBranch {
    pub fn position(&self) -> Position {
        self.parameter.position().join(self.body.position())
    }
}
