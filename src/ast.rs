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
        type_variables: Vec<Token>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub token: Token,
}

#[derive(Debug, Clone)]
pub enum Type {
    Underscore,
    TypeVariable { name: String },
    Record { key_type_pairs: Vec<(String, Type)> },
    Named { name: String, arguments: Vec<Type> },
    Function(FunctionType),
    Union(UnionType),
}

#[derive(Debug, Clone)]
pub struct UnionType {
    pub tags: Vec<TagType>,
    pub bound: UnionTypeBound,
    pub catch_all: bool,
}

#[derive(Debug, Clone)]
pub enum UnionTypeBound {
    Exact,
    AtLeast,
    AtMost,
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
    Named {
        name: Token,
        arguments: Vec<TypeAnnotation>,
    },
    Record {
        left_curly_bracket: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        right_curly_bracket: Token,
    },
    Tag {
        token: Token,
        payload: Option<TagTypeAnnotationPayload>,
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
pub struct TagTypeAnnotationPayload {
    pub left_parenthesis: Token,
    pub payload: Box<TypeAnnotation>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    String(Token),
    Number(Token),
    Underscore(Token),
    Identifier(Token),
    Tag {
        token: Token,
        payload: Option<Box<DestructurePatternTagPayload>>,
    },
    Record {
        left_curly_bracket: Token,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
        right_curly_bracket: Token,
    },
}

#[derive(Debug, Clone)]
pub struct DestructurePatternTagPayload {
    pub left_parenthesis: Token,
    pub destructure_pattern: DestructurePattern,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct DestructuredRecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub as_value: Option<DestructurePattern>,
    pub spread: Option<Token>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Number(Token),
    String(Token),
    Variable(Token),
    Tag {
        token: Token,
        payload: Option<Box<TagPayload>>,
    },
    Function(Box<Function>),
    FunctionCall(FunctionCall),
    Record {
        left_curly_bracket: Token,
        spread: Option<Box<Expression>>,
        key_value_pairs: Vec<RecordKeyValue>,
        right_curly_bracket: Token,
    },
    Array(Vec<Expression>),
    Let {
        let_keyword: Token,
        left: DestructurePattern,
        right: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Option<Box<Expression>>,
    },
}

#[derive(Debug, Clone)]
pub struct TagPayload {
    pub left_parenthesis: Token,
    pub value: Expression,
    pub right_parenthesis: Token,
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
    ExpectedDestructurePattern {
        token: Token,
    },
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
