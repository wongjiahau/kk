use crate::non_empty::NonEmpty;
/// The syntax tree here represents raw syntax tree that is not type checked

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),

    /// This represent type alias definition.
    Type(TypeStatement),

    /// This represents named sum types (a.k.a tagged union).
    Enum(EnumStatement),

    /// This represents the entry points of a module.
    Do(DoStatement),
    Import(ImportStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub keyword_export: Option<Token>,
    pub keyword_let: Token,
    pub left: Token,
    pub type_variables: Vec<Token>,
    pub type_annotation: TypeAnnotation,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct TypeStatement {
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
    pub constructors: Vec<EnumConstructorDefinition>,
    pub type_variables: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct ImportStatement {
    pub keyword_import: Token,
    pub url: Token,
    pub imported_names: NonEmpty<ImportedName>,
}

#[derive(Debug, Clone)]
pub struct DoStatement {
    pub keyword_do: Token,
    pub expression: Expression,
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

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Underscore,

    /// Type variable that is declared. Cannot be substituted before instantiation.
    /// Note that a type variable is only explicit within its own scope
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
        symbol_uid: usize,
        name: String,
        type_arguments: Vec<(String, Type)>,
    },
    Quoted(Box<Type>),
    Function(FunctionType),
    Tuple(Box<NonEmpty<Type>>),
    Boolean,
    Float,
    Integer,
    String,
    Character,
    Null,
    Array(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct TagType {
    pub tagname: String,
    pub payload: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct TypeScheme {
    pub type_variables: Vec<String>,
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
        start_token: Token,
        parameters_types: Box<NonEmpty<TypeAnnotation>>,
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
    Character(Token),
    Variable(Token),
    Quoted {
        opening_backtick: Token,
        expression: Box<Expression>,
        closing_backtick: Token,
    },
    EnumConstructor {
        name: Token,
        payload: Option<Box<ExpressionEnumConstructorPayload>>,
    },
    Function(Box<Function>),
    FunctionCall(Box<FunctionCall>),
    Record {
        left_curly_bracket: Token,
        key_value_pairs: Vec<RecordKeyValue>,
        right_curly_bracket: Token,
    },
    RecordAccessOrFunctionCall {
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
        left: Box<DestructurePattern>,
        type_annotation: Option<TypeAnnotation>,
        right: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Option<Box<Function>>,
    },
    UnsafeJavascript {
        code: Token,
    },
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
pub struct Function {
    pub branches: NonEmpty<FunctionBranch>,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub start_token: Token,
    pub parameters: NonEmpty<DestructurePattern>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum FunctionParameters {
    NoParenthesis {
        parameter: FunctionParameter,
    },
    WithParenthesis {
        left_parenthesis: Token,
        parameters: NonEmpty<FunctionParameter>,
        right_parenthesis: Token,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionBranchRestArguments {
    pub left_parenthesis: Token,
    pub rest_arguments: Vec<FunctionParameter>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub destructure_pattern: DestructurePattern,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    KeywordLet,
    KeywordType,
    KeywordEnum,
    KeywordDo,
    KeywordElse,
    KeywordNull,
    KeywordTrue,
    KeywordFalse,
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
    Colon,
    LessThan,
    MoreThan,
    Equals,
    Period,
    Spread,
    Comma,
    Minus,
    FatArrowRight,
    ThinArrowRight,
    Pipe,
    Underscore,
    Identifier,
    String,
    Character,
    Integer,
    Float,
    DoubleColon,
    Backtick,

    /// Comments starts with hash (#)
    Comment,

    /// Multiline comment starts and ends with triple hash (###)
    MultilineComment,

    JavascriptCode,
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
