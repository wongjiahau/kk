use crate::{module::SymbolUid, non_empty::NonEmpty, tokenize::Character};
/// The syntax tree here represents raw syntax tree that is not type checked

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),

    /// This represents the entry points of a module.
    /// Will be ignored for imported modules.
    Expression(Expression),

    /// This represent type alias definition.
    Type(TypeAliasStatement),

    /// This represents named sum types (a.k.a tagged union).
    Enum(EnumStatement),

    Import(ImportStatement),
}

#[derive(Debug, Clone)]
pub struct FunctionStatement {
    pub keyword_export: Option<Token>,
    pub name: Token,
    pub arrow_function: ArrowFunction,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub keyword_export: Option<Token>,
    pub keyword_let: Token,
    pub left: DestructurePattern,
    pub type_variables: Vec<Token>,
    pub type_annotation: Option<TypeAnnotation>,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeAliasStatement {
    pub keyword_export: Option<Token>,
    pub keyword_type: Token,
    pub left: Token,
    pub right: TypeAnnotation,
    pub type_variables: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct EnumStatement {
    pub keyword_export: Option<Token>,
    pub keyword_enum: Token,
    pub name: Token,
    pub type_variables: Vec<Token>,
    pub constructors: Vec<EnumConstructorDefinition>,
    pub right_curly_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct ImportStatement {
    pub keyword_import: Token,
    pub url: Token,
    pub import_type: ImportType,
}

#[derive(Debug, Clone)]
pub enum ImportType {
    /// This means import selected exported symbols
    Selected {
        imported_names: NonEmpty<ImportedName>,
    },

    /// This means to import all exported symbols
    All { asterisk: Token },
}

#[derive(Debug, Clone)]
pub struct ImportedName {
    pub name: Token,
    pub alias_as: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct EnumConstructorDefinition {
    pub name: Token,
    pub payload: Option<Box<EnumConstructorDefinitionPayload>>,
}

#[derive(Debug, Clone)]
pub struct EnumConstructorDefinitionPayload {
    pub left_parenthesis: Token,
    pub type_annotation: TypeAnnotation,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Underscore,

    /// Type variable that is declared. Cannot be substituted before instantiation.
    /// Note that a type variable is only explicit within its own scope.
    /// This is also commonly known as Rigid Type Variable.
    ExplicitTypeVariable {
        name: String,
    },

    /// Type variable that is implicitly created  for unification.
    ImplicitTypeVariable {
        name: String,
    },
    Record {
        key_type_pairs: Vec<(String, Type)>,
    },

    /// Also known as enum type  
    /// Also known as nominal type
    Named {
        /// This is needed to differentiate two named types that has the same name
        ///  which are declared in different modules
        /// Also needed for looking up constructors for this type
        symbol_uid: SymbolUid,
        name: String,
        type_arguments: Vec<(String, Type)>,
    },
    BuiltInOneArgumentType {
        kind: BuiltInOneArgumentTypeKind,
        type_argument: Box<Type>,
    },
    Function(FunctionType),
    TypeScheme(Box<TypeScheme>),
    Tuple(Box<NonEmpty<Type>>),
    Boolean,
    Float,
    Integer,
    String,
    Character,
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInOneArgumentTypeKind {
    Quoted,
    Array,
}

#[derive(Debug, Clone)]
pub struct TagType {
    pub tagname: String,
    pub payload: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub type_variables: NonEmpty<String>,
    pub type_value: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters_types: Box<NonEmpty<Type>>,
    pub return_type: Box<Type>,
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
pub enum TypeAnnotation {
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
        left_square_bracket: Token,
        element_type: Box<TypeAnnotation>,
        right_square_bracket: Token,
    },
    Underscore(Token),
    Function {
        parameters: FunctionParameters,
        return_type: Box<TypeAnnotation>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfinitePatternKind {
    String,
    Character,
    Integer,
}

#[derive(Debug, Clone)]
pub enum DestructurePattern {
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
    Identifier(Token),
    EnumConstructor {
        name: Token,
        payload: Option<Box<DestructurePatternEnumConstructorPayload>>,
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
    Null(Token),
    Boolean {
        token: Token,
        value: bool,
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
        payload: Option<Box<ExpressionEnumConstructorPayload>>,
    },

    /// This cannot be constructed directly from syntax, it is only for internal usage
    BranchedFunction(Box<BranchedFunction>),

    /// Typescript-styled function
    ArrowFunction(Box<ArrowFunction>),
    FunctionCall(Box<FunctionCall>),
    Record {
        wildcard: Option<Token>,
        left_curly_bracket: Token,
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
        left_square_bracket: Token,
        elements: Vec<Expression>,
        right_square_bracket: Token,
    },
    With(WithExpression),
    If {
        keyword_if: Token,
        condition: Box<Expression>,
        if_true: Box<Expression>,
        keyword_else: Token,
        if_false: Box<Expression>,
    },
    Switch {
        keyword_switch: Token,
        expression: Box<Expression>,
        left_curly_bracket: Token,
        cases: Box<NonEmpty<SwitchCase>>,
        right_curly_bracket: Token,
    },
    Block(Block),
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
    pub first_argument: Box<Expression>,
    pub rest_arguments: Option<FunctionCallRestArguments>,
    pub type_arguments: Option<TypeArguments>,
}

#[derive(Debug, Clone)]
pub struct TypeArguments {
    pub left_angular_bracket: Token,
    pub arguments: Vec<TypeAnnotation>,
    pub right_angular_bracket: Token,
}

#[derive(Debug, Clone)]
pub struct FunctionCallRestArguments {
    pub left_parenthesis: Token,
    pub arguments: Vec<Expression>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct BranchedFunction {
    pub branches: NonEmpty<FunctionBranch>,
}

#[derive(Debug, Clone)]
pub struct ArrowFunction {
    pub type_variables: Vec<Token>,
    pub parameters: ArrowFunctionParameters,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionParameters {
    pub left_parenthesis: Token,
    pub parameters: Box<NonEmpty<FunctionParameter>>,
    pub right_parenthesis: Token,
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
    pub parameters: NonEmpty<DestructurePattern>,
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
    KeywordIf,
    KeywordElse,
    KeywordSwitch,
    KeywordCase,
    KeywordWith,
    KeywordLet,
    KeywordType,
    KeywordEnum,
    KeywordDo,
    KeywordNull,
    KeywordTrue,
    KeywordFalse,
    KeywordImport,
    KeywordFrom,
    KeywordAs,
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
    LessThan,
    MoreThan,
    Equals,
    Period,
    TriplePeriod,
    Comma,
    Minus,
    FatArrowRight,
    Pipe,
    Slash,
    Underscore,
    Identifier,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
