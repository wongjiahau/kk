use std::path::PathBuf;

use crate::{
    module::Access,
    non_empty::NonEmpty,
    qualified_ast::{self, ResolvedName},
    tokenize::{Character, Position, RawIdentifier, StringLiteral, Token},
    unify::Positionable,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),

    /// This represent type alias definition.
    TypeAlias(TypeAliasStatement),

    /// This represents named sum types (a.k.a tagged union).
    Enum(EnumStatement),

    ModuleAlias(ModuleAliasStatement),
    ModuleDefinition(ModuleDefinitionStatement),

    /// This represents the entry points of a module.
    /// Will be ignored for imported modules.
    Entry(EntryStatement),
}

#[derive(Debug, Clone)]
pub struct ModuleAliasStatement {
    pub keyword_module: Token,
    pub left: ModuleDestructurePattern,
    pub right: RawName,
}

#[derive(Debug, Clone)]
pub struct ModuleDefinitionStatement {
    pub name: RawIdentifier,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum ModuleDestructurePattern {
    Identifier(RawIdentifier),
    Record {
        left_parenthesis: Token,
        spread: Option<Token>,
        pairs: Vec<ModuleDestructurePatternPair>,
        right_parenthesis: Token,
    },
}

#[derive(Debug, Clone)]
pub struct ModuleDestructurePatternPair {
    pub name: RawIdentifier,
    pub pattern: Option<ModuleDestructurePattern>,
}

#[derive(Debug, Clone)]
pub struct EntryStatement {
    pub keyword_entry: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeVariablesDeclaration {
    pub left_angular_bracket: Token,
    pub type_variables: NonEmpty<Token>,
    pub right_angular_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub access: Access,
    pub keyword_let: Token,
    pub name: RawIdentifier,
    pub doc_string: Option<StringLiteral>,
    pub type_annotation: TypeAnnotation,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub pattern: DestructurePattern,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct TypeAliasStatement {
    pub access: Access,
    pub keyword_type: Token,
    pub name: RawIdentifier,
    pub right: TypeAnnotation,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
}

#[derive(Debug, Clone)]
pub struct EnumStatement {
    pub access: Access,
    pub keyword_type: Token,
    pub name: RawIdentifier,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
    pub constructors: Vec<EnumConstructorDefinition>,
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
pub struct TypeConstraintsAnnotation {
    pub left_parenthesis: Token,
    pub right_parenthesis: Token,
    pub type_constraints: Vec<TypeConstraintAnnotation>,
}

impl TypeConstraintsAnnotation {
    pub fn position(&self) -> Position {
        self.left_parenthesis
            .position
            .join(self.right_parenthesis.position)
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
    Quoted {
        opening_backtick: Token,
        type_annotation: Box<TypeAnnotation>,
        closing_backtick: Token,
    },
    Named {
        name: RawName,
        type_arguments: Option<TypeArguments>,
    },
    Record {
        left_parenthesis: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        right_parenthesis: Token,
    },
    Array {
        left_square_bracket: Token,
        element_type: Box<TypeAnnotation>,
        right_square_bracket: Token,
    },
    Underscore(Token),
    Function(FunctionTypeAnnotation),
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
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
        left_parenthesis: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
        right_parenthesis: Token,
    },
    Parenthesized {
        left_parenthesis: Token,
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
pub struct DestructuredRecordSpread {
    double_period: Token,
    identifier: Token,
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
    pub as_value: Option<DestructurePattern>,
}

#[derive(Debug, Clone)]
pub struct RawName {
    pub qualifier: Option<RawModuleName>,
    pub name: RawIdentifier,
}

#[derive(Debug, Clone)]
pub struct RawModuleName {
    pub root: RawModuleNameRoot,
    /// For example:
    ///
    /// "./hello.kk" has empty segment
    ///
    /// "./hello.kk"::a::b has two segments, which is "a" and "b"
    pub segments: Vec<RawIdentifier>,
}

#[derive(Debug, Clone)]
pub enum RawModuleNameRoot {
    Filepath(StringLiteral),
    Identifier(RawIdentifier),
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
    Identifier(RawName),
    EnumConstructor {
        name: RawName,
        payload: Option<Box<Expression>>,
    },

    /// This cannot be constructed directly from syntax, it is only for internal usage
    Function(Box<Function>),

    FunctionCall(Box<FunctionCall>),
    Record {
        left_parenthesis: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<RecordKeyValue>,
        right_parenthesis: Token,
    },
    RecordAccess {
        expression: Box<Expression>,
        property_name: Token,
    },
    RecordUpdate {
        expression: Box<Expression>,
        left_parenthesis: Token,
        updates: Vec<RecordUpdate>,
        right_parenthesis: Token,
    },
    Array {
        left_square_bracket: Token,
        elements: Vec<Expression>,
        right_square_bracket: Token,
    },
    Let {
        keyword_let: Token,
        left: DestructurePattern,
        right: Box<Expression>,
        type_annotation: Option<TypeAnnotation>,
        body: Box<Expression>,
    },
    /// CPS = Continuation Passing Style
    CpsClosure {
        tilde: Token,
        expression: Box<Expression>,
    },
    CpsBang {
        argument: Box<Expression>,
        bang: Token,
        function: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct ModuleAccessChild {
    identifier: Token,
    next: Box<Option<ModuleAccessChild>>,
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

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub argument: Box<Expression>,
    pub type_arguments: Option<TypeArguments>,
}

#[derive(Debug, Clone)]
pub struct TypeArguments {
    pub left_angular_bracket: Token,
    pub type_annotations: Box<NonEmpty<TypeAnnotation>>,
    pub right_angular_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub left_curly_bracket: Token,
    pub branches: NonEmpty<FunctionBranch>,
    pub right_curly_bracket: Token,
}

pub fn get_type_variables(
    type_variables_declaration: &Option<TypeVariablesDeclaration>,
) -> Vec<Token> {
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
pub struct FunctionParameter {
    pub pattern: DestructurePattern,
    pub type_annotation: Box<Option<TypeAnnotation>>,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub parameter: Box<DestructurePattern>,
    pub body: Box<Expression>,
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

impl RawName {
    pub fn append_segment(self, name: RawIdentifier) -> Self {
        RawName {
            name,
            qualifier: self.qualifier,
            segments: self
                .segments
                .into_iter()
                .chain(vec![name].into_iter())
                .collect(),
        }
    }
}
impl RawModuleNameRoot {
    pub fn position(&self) -> Position {
        match self {
            RawModuleNameRoot::Filepath(filepath) => filepath.position(),
            RawModuleNameRoot::Identifier(identifier) => identifier.position.clone(),
        }
    }
}
