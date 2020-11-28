#[derive(Debug)]
pub enum Statement {
    Let {
        left: DestructurePattern,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
    TypeAlias {
        left: Token,
        right: TypeAnnotation,
        type_variables: Vec<TypeVariable>,
    },
}

#[derive(Debug)]
pub struct TypeVariable {
    pub token: Token,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub representation: TypeRepresentation,

    /// If None means not determined yet
    pub value: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    NotInferred,
    String,
    Number,
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
    Tag {
        tagname: String,
        payload: Option<Box<Type>>,
    },
}

#[derive(Debug)]
pub enum SymbolSource {
    BuiltIn,
    UserDefined {
        filename: String,
        position: Position,
    },
}

#[derive(Debug, Clone)]
pub enum TypeRepresentation {
    Name(Token),
    Record {
        key_value_pairs: Vec<(Token, TypeAnnotation)>,
    },
    Tag {
        token: Token,
        payload: Option<Box<TypeAnnotation>>,
    },
    Underscore,
    Union {
        type_annotations: Vec<TypeAnnotation>,
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

#[derive(Debug)]
pub struct Expression {
    pub value: ExpressionValue,
    pub inferred_type: Option<Type>,
}

#[derive(Debug)]
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
        spread: Option<Box<Expression>>,
        key_value_pairs: Vec<RecordKeyValue>,
    },
    Array(Vec<Expression>),
    Let {
        left: DestructurePattern,
        right: Box<Expression>,
        else_return: Option<Box<Expression>>,
        return_value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct RecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub arguments: Vec<FunctionCallArgument>,
}

#[derive(Debug)]
pub struct FunctionCallArgument {
    pub argument_name: Option<Token>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Function {
    pub first_branch: FunctionBranch,
    pub branches: Vec<FunctionBranch>,
}

#[derive(Debug)]
pub struct FunctionBranch {
    pub arguments: Vec<FunctionArgument>,
    pub body: Box<Expression>,
    pub return_type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug)]
pub struct FunctionArgument {
    pub destructure_pattern: DestructurePattern,
    pub type_annotation: Option<TypeAnnotation>,
    pub default_value: Option<Expression>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
