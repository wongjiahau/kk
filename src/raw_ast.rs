use crate::{non_empty::NonEmpty, tokenize::Character, unify::Positionable};
/// The syntax tree here represents raw syntax tree that is not type checked

#[derive(Debug, Clone)]
pub enum Statement {
    Interface(InterfaceStatement),
    Implement(ImplementStatement),
    Let(LetStatement),

    /// This represents the entry points of a module.
    /// Will be ignored for imported modules.
    Expression(Expression),

    /// This represent type alias definition.
    Type(TypeAliasStatement),

    /// This represents named sum types (a.k.a tagged union).
    Enum(EnumStatement),

    Module(ModuleStatement),

    Entry(EntryStatement),
}

#[derive(Debug, Clone)]
pub struct ModuleStatement {
    pub keyword_export: Option<Token>,
    pub keyword_module: Token,
    pub left: ModuleDestructurePattern,
    pub right: ModuleValue,
}

#[derive(Debug, Clone)]
pub enum ModuleDestructurePattern {
    Identifier(Token),
    Record {
        left_square_bracket: Token,
        spread: Option<Token>,
        pairs: Vec<ModuleDestructurePatternPair>,
        right_square_bracket: Token,
    },
}

#[derive(Debug, Clone)]
pub struct ModuleDestructurePatternPair {
    pub name: Token,
    pub pattern: Option<ModuleDestructurePattern>,
}

#[derive(Debug, Clone)]
pub enum ModuleValue {
    Import { keyword_import: Token, url: Token },
    Name(Token),
}

#[derive(Debug, Clone)]
pub struct EntryStatement {
    pub keyword_entry: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub left: DestructurePattern,
    pub type_annotation: Option<TypeAnnotation>,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct ImplementStatement {
    pub keyword_export: Option<Token>,
    pub keyword_implements: Token,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,

    pub interface_name: Token,

    /// The type where we are implementing the interface for.    
    /// For example, in `implements Foo<Bar>`, `Bar` is the considered the subject type
    pub for_types: TypeArguments,
    pub left_curly_bracket: Token,
    pub definitions: Vec<ImplementDefinition>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct ImplementDefinition {
    pub keyword_let: Token,
    pub name: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct InterfaceStatement {
    pub keyword_export: Option<Token>,
    pub keyword_interface: Token,
    pub name: Token,
    pub type_variables_declaration: TypeVariablesDeclaration,
    pub left_curly_bracket: Token,
    pub definitions: Vec<InterfaceDefinition>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct TypeVariablesDeclaration {
    pub left_angular_bracket: Token,
    pub type_variables: NonEmpty<Token>,
    pub right_angular_bracket: Token,

    /// List of constraints that the declared type variables must satisfy
    pub constraints: Vec<TypeVariableConstraint>,
}

#[derive(Debug, Clone)]
pub struct TypeVariableConstraint {
    pub interface_name: Token,
    pub type_variables: NonEmpty<Token>,
}

#[derive(Debug, Clone)]
pub struct InterfaceDefinition {
    pub keyword_let: Token,
    pub name: Token,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub keyword_export: Option<Token>,
    pub keyword_let: Token,
    pub name: Token,
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
    pub keyword_export: Option<Token>,
    pub keyword_type: Token,
    pub left: Token,
    pub right: TypeAnnotation,
    pub type_variables_declaration: Option<TypeVariablesDeclaration>,
}

#[derive(Debug, Clone)]
pub struct EnumStatement {
    pub keyword_export: Option<Token>,
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
    pub left_square_bracket: Token,
    pub right_square_bracket: Token,
    pub type_constraints: Vec<TypeConstraintAnnotation>,
}

impl TypeConstraintsAnnotation {
    pub fn position(&self) -> Position {
        self.left_square_bracket
            .position
            .join(self.right_square_bracket.position)
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
        left_square_bracket: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        right_square_bracket: Token,
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

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    Or(Box<NonEmpty<DestructurePattern>>),
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
        wildcard: Option<Token>,
        left_curly_bracket: Token,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
        right_curly_bracket: Token,
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
    Parenthesized {
        left_parenthesis: Token,
        right_parenthesis: Token,
        value: Box<Expression>,
    },
    Float(Token),
    Integer(Token),
    String(Token),
    InterpolatedString {
        start_quote: Box<Character>,
        sections: NonEmpty<InterpolatedStringSection>,
        end_quote: Box<Character>,
    },
    Character(Token),
    Identifier(Token),
    Quoted {
        opening_backtick: Token,
        expression: Box<Expression>,
        closing_backtick: Token,
    },
    EnumConstructor {
        name: Token,
        payload: Option<Box<Expression>>,
    },

    /// This cannot be constructed directly from syntax, it is only for internal usage
    Lambda(Box<Lambda>),

    FunctionCall(Box<FunctionCall>),
    Record {
        wildcard: Option<Token>,
        left_square_bracket: Token,
        key_value_pairs: Vec<RecordKeyValue>,
        right_square_bracket: Token,
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
    UnsafeJavascript {
        code: Token,
    },
}

#[derive(Debug, Clone)]
pub enum Block {
    WithBrackets {
        left_curly_bracket: Token,
        statements: Vec<Statement>,
        right_curly_bracket: Token,
    },
    WithoutBrackets {
        statements: Box<NonEmpty<Statement>>,
    },
}

impl Block {
    pub fn statements(self) -> Vec<Statement> {
        match self {
            Block::WithBrackets { statements, .. } => statements,
            Block::WithoutBrackets { statements } => statements.into_vector(),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Block::WithBrackets {
                left_curly_bracket,
                statements,
                right_curly_bracket,
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            Block::WithoutBrackets { statements } => statements
                .first()
                .position()
                .join(statements.last().position()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub keyword_case: Token,
    pub pattern: DestructurePattern,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct WithExpression {
    pub keyword_with: Token,
    pub left_patterns: Box<NonEmpty<DestructurePattern>>,
    pub binary_function_name: Token,
    pub right: Box<Expression>,
    pub body: Box<Expression>,
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

    /// For example, `x.{ a.square() }`
    FunctionalUpdate {
        property_name: Token,
        function: Expression,
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
pub struct FunctionCallRestArguments {
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub left_curly_bracket: Token,
    pub branches: NonEmpty<FunctionBranch>,
    pub right_curly_bracket: Token,
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
    pub start_token: Token,
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
    KeywordWhere,
    KeywordInterface,
    KeywordImplements,
    KeywordEntry,
    KeywordLet,
    KeywordType,
    KeywordModule,
    KeywordImport,
    KeywordExport,
    Whitespace,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Newline,
    /// Also known as Exclamation Mark (!)
    Bang,
    Colon,

    /// Also known as Left Angular Bracket (<)
    LessThan,

    /// Also known as Right Angular Bracket (>)
    MoreThan,
    Equals,
    Period,
    TriplePeriod,
    Comma,
    Semicolon,
    Minus,
    ArrowRight,
    Pipe,
    Slash,
    Underscore,
    Identifier,
    Operator,
    /// Used for construction of tagged union
    Tag,
    String,
    InterpolatedString {
        start_quote: Box<Character>,
        sections: NonEmpty<InterpolatedStringSection>,
        end_quote: Box<Character>,
    },
    Character,
    Integer,
    Float,
    DoubleColon,
    Backtick,
    TripleBacktick,

    /// Comments starts with double slash
    Comment,

    /// Multiline comment starts with (/*) and ends with (*/)
    MultilineComment {
        /// Note that these characters excludeds opening and closing triple-hash
        characters: Vec<Character>,
    },

    JavascriptCode,
    Asterisk,
    Other(char),
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
impl ModuleValue {
    pub fn position(&self) -> Position {
        match self {
            ModuleValue::Import {
                keyword_import,
                url,
            } => keyword_import.position.join(url.position),
            ModuleValue::Name(token) => token.position,
        }
    }
}
