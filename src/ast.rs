#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        left: Token,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
    TypeAlias {
        left: Token,
        right: TypeAnnotation,
        type_variables: Vec<TypeVariable>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub token: Token,
}

#[derive(Debug, Clone)]
pub enum Type {
    String,
    Number,
    Underscore,
    TypeVariable {
        name: String,
    },
    Record {
        key_type_pairs: Vec<(String, Type)>,
    },
    Compound {
        name: String,
        arguments: Vec<Type>,
    },
    Alias {
        name: String,
    },
    Function(FunctionType),
    Union {
        tags: Vec<TagType>,
        bound: UnionTypeBound,
        catch_all: bool,
    },
}

#[derive(Debug, Clone)]
pub enum UnionTypeBound {
    Exact,
    AtLeast,
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

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub arguments_types: Vec<Type>,
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
pub enum TypeAnnotation {
    Name(Token),
    Record {
        open_curly_bracket: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        closing_curly_bracket: Token,
    },
    Tag {
        token: Token,
        payload: Option<Box<TypeAnnotation>>,
    },
    Underscore,
    Union {
        type_annotations: Vec<TypeAnnotation>,
    },
    Function {
        start_token: Token,
        arguments_types: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },
}

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    Underscore(Token),
    Identifier(Token),
    Tag {
        token: Token,
        payload: Option<Box<DestructurePattern>>,
    },
    Record {
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
    },
}

#[derive(Debug, Clone)]
pub struct DestructuredRecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub as_value: Option<DestructurePattern>,
    pub spread: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub value: ExpressionValue,
    pub inferred_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ExpressionValue {
    Number(Token),
    String(Token),
    Variable(Token),
    Tag {
        token: Token,
        payload: Option<Box<Expression>>,
    },
    Function(Function),
    FunctionCall(FunctionCall),
    Record {
        open_curly_bracket: Token,
        spread: Option<Box<Expression>>,
        key_value_pairs: Vec<RecordKeyValue>,
        closing_curly_bracket: Token,
    },
    Array(Vec<Expression>),
    Let {
        left: DestructurePattern,
        right: Box<Expression>,
        else_return: Option<Box<Expression>>,
        return_value: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct RecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub first_branch: FunctionBranch,
    pub branches: Vec<FunctionBranch>,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub start_token: Token,
    pub arguments: Vec<FunctionArgument>,
    pub body: Box<Expression>,
    pub return_type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
    pub destructure_pattern: DestructurePattern,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    InvalidChar {
        position: Position,
        error: String,
    },
    InvalidToken {
        invalid_token: Token,
        error: String,
        suggestion: Option<String>,
    },
    UnexpectedEOF {
        error: String,
        suggestion: Option<String>,
    },
    FunctionMissingFirstBranch {
        token: Token,
        error: String,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
    pub representation: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    KeywordLet,
    KeywordType,
    KeywordElse,
    Whitespace,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Newline,
    Colon,
    Semicolon,
    Pipe,
    LessThan,
    MoreThan,
    Equals,
    Period,
    Spread,
    Comma,
    Plus,
    Minus,
    ArrowRight,
    Backslash,
    Underscore,
    Tag,
    Identifier,
    String,
    Number,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Source {
    File { path: String },
    NonFile { env_name: String },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub source: Source,
    pub position: Position,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
