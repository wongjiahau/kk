use itertools::Itertools;

use crate::{
    module::Access,
    non_empty::NonEmpty,
    parse::ParseError,
    tokenize::Character,
    unify::{FunctionCallLike, FunctionCallLikeComponent, Positionable},
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
    /// Function declaration
    Function(FunctionStatement),
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) keyword_fn: Token,
    pub(crate) components: FunctionCallLike<Parameter>,
    pub(crate) return_type: TypeAnnotation,
    pub(crate) type_variables: Option<TypeVariablesDeclaration>,
    pub(crate) associativity: Option<FunctionSignatureAssociativity>,
}

#[derive(Debug, Clone)]
pub(crate) enum FunctionSignatureAssociativity {
    LeftAssociative,
    RightAssociative,
}

impl FunctionSignature {
    fn parameters(&self) -> Vec<Parameter> {
        self.components
            .components
            .clone()
            .into_vector()
            .into_iter()
            .filter_map(|component| match component {
                FunctionCallLikeComponent::Identifier(_) => None,
                FunctionCallLikeComponent::Other(parameter) => Some(parameter),
            })
            .collect_vec()
    }

    fn type_annotation(&self) -> TypeAnnotation {
        let parameters = self.parameters();
        parameters
            .clone()
            .into_iter()
            .rev()
            .fold(self.return_type.clone(), |result, parameter| {
                let type_annotation = TypeAnnotation::Function(FunctionTypeAnnotation {
                    parameter: Box::new(parameter.type_annotation),
                    return_type: Box::new(result),
                    type_constraints: None,
                });
                self.type_variables
                    .clone()
                    .map(|type_variables| TypeAnnotation::Scheme {
                        type_variables,
                        type_annotation: Box::new(type_annotation.clone()),
                    })
                    .unwrap_or(type_annotation)
            })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionStatement {
    pub access: Access,
    pub type_variables: Vec<TypeArguments>,
    pub signature: FunctionSignature,
    pub body: Expression,
    pub right_curly_bracket: Token,
}
impl FunctionStatement {
    pub(crate) fn position(&self) -> Position {
        self.signature
            .keyword_fn
            .position
            .join(self.right_curly_bracket.position)
    }

    pub(crate) fn into_let_statement(self) -> LetStatement {
        let parameters = self.signature.parameters();
        LetStatement {
            access: self.access,
            keyword_let: self.signature.keyword_fn.clone(),
            name: self.signature.components.as_one_token(),
            doc_string: None,
            type_annotation: self.signature.type_annotation(),

            expression: {
                let last_pattern = parameters
                    .last()
                    .map(|parameter| parameter.pattern.clone())
                    .unwrap_or(DestructurePattern::Unit {
                        left_parenthesis: Token::dummy_identifier("(".to_string()),
                        right_parenthesis: Token::dummy_identifier(")".to_string()),
                    });
                let innermost_function = Expression::Function(Box::new(Function {
                    branches: NonEmpty::new(
                        FunctionBranch {
                            parameter: Box::new(last_pattern),
                            body: Box::new(self.body),
                            right_square_bracket: self.right_curly_bracket,
                        },
                        vec![],
                    ),
                }));
                parameters.into_iter().rev().skip(1).fold(
                    innermost_function,
                    |result, parameter| {
                        Expression::Function(Box::new(Function {
                            branches: NonEmpty::new(
                                FunctionBranch {
                                    parameter: Box::new(parameter.pattern),
                                    body: Box::new(result),
                                    right_square_bracket: Token::dummy(),
                                },
                                vec![],
                            ),
                        }))
                    },
                )
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionStatementComponent {
    Parameter(Parameter),
    Identifier(Token),
}
impl FunctionStatementComponent {
    pub(crate) fn position(&self) -> Position {
        match self {
            FunctionStatementComponent::Parameter(parameter) => parameter.position(),
            FunctionStatementComponent::Identifier(identifier) => identifier.position.clone(),
        }
    }
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
    pub keyword_entry: Token,
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

impl Positionable for Parameter {
    fn position(&self) -> Position {
        self.pattern
            .position()
            .join(self.type_annotation.position())
    }
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
    pub keyword_class: Token,
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
    Quoted {
        opening_backtick: Token,
        type_annotation: Box<TypeAnnotation>,
        closing_backtick: Token,
    },
    Named {
        name: Token,
        type_arguments: Option<TypeArguments>,
    },
    Record {
        left_curly_bracket: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        right_curly_bracket: Token,
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
        left_curly_bracket: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
        right_curly_bracket: Token,
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
        left_curly_bracket: Token,
        wildcard: Option<Token>,
        key_value_pairs: Vec<RecordKeyValue>,
        right_curly_bracket: Token,
    },
    RecordAccess {
        expression: Box<Expression>,
        property_name: Token,
    },
    RecordUpdate {
        expression: Box<Expression>,
        left_curly_bracket: Token,
        updates: Vec<RecordUpdate>,
        right_curly_bracket: Token,
    },
    Array {
        hash_left_square_bracket: Token,
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
    CpsBang {
        argument: Box<Expression>,
        bang: Token,
    },
    TildeClosure(TildeClosure),
    InnateFunctionCall(InnateFunctionCall),
    Pass,
}

#[derive(Debug, Clone)]
pub struct InnateFunctionCall {
    pub function_name: Token,
    pub argument: Box<Expression>,
}
impl InnateFunctionCall {
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

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub argument: Box<Expression>,
    pub type_arguments: Option<TypeArguments>,
}

#[derive(Debug, Clone)]
pub struct NewFunctionCall {
    pub name: FunctionName,
    pub arguments: Vec<Expression>,
}

impl NewFunctionCall {
    pub(crate) fn position(&self) -> Position {
        self.name
            .position()
            .join_maybe(self.arguments.last().map(|argument| argument.position()))
    }
}

#[derive(Debug, Clone)]
pub struct FunctionName(pub(crate) NonEmpty<(usize, Token)>);
impl FunctionName {
    fn position(&self) -> Position {
        self.0.head.1.position.join(self.0.last().1.position)
    }
}

impl FunctionCall {
    pub fn position(&self) -> Position {
        self.function.position().join(self.argument.position())
    }
}

#[derive(Debug, Clone)]
pub struct TypeArguments {
    pub left_angular_bracket: Token,
    pub type_annotations: Box<NonEmpty<TypeAnnotation>>,
    pub right_angular_bracket: Token,
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
    pub right_square_bracket: Token,
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
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
    pub representation: String,
}

impl Token {
    pub fn dummy() -> Token {
        Token::dummy_identifier("".to_string())
    }
    pub fn dummy_identifier(representation: String) -> Token {
        Token {
            token_type: TokenType::Identifier,
            position: Position {
                line_start: 0,
                line_end: 0,
                character_index_start: 0,
                character_index_end: 0,
                column_start: 0,
                column_end: 0,
            },
            representation,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    // Keywords
    KeywordEntry,
    KeywordLet,
    KeywordType,
    KeywordImport,
    KeywordPublic,
    KeywordFn,
    KeywordExport,
    KeywordGiven,
    KeywordClass,
    KeywordInnate,

    Whitespace,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    HashLeftSquareBracket,
    LeftSquareBracket,
    RightSquareBracket,
    Backslash,
    Newline,
    /// Also known as Exclamation Mark (!)
    Bang,
    Tilde,
    Colon,

    /// Also known as Left Angular Bracket (<)
    LessThan,

    /// Also known as Right Angular Bracket (>)
    MoreThan,
    Equals,
    Period,
    DoublePeriod,
    Comma,
    Semicolon,
    ArrowRight,
    Pipe,
    Underscore,
    Identifier,
    Operator,
    String(StringLiteral),
    InterpolatedString(InterpolatedString),
    Character,
    Integer,
    Float,
    DoubleColon,

    /// Comments starts with double slash
    Comment,

    /// Multiline comment starts with (/*) and ends with (*/)
    MultilineComment,

    Other(char),
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub start_quotes: NonEmpty<Character>,
    pub content: String,
    pub end_quotes: NonEmpty<Character>,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
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

impl StringLiteral {
    pub fn position(&self) -> Position {
        self.start_quotes
            .first()
            .position()
            .join(self.end_quotes.last().position())
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
