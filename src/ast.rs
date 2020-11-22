#[derive(Debug)]
pub enum Statement {
    Let {
        left: DestructurePattern,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub _type: Type,

    /// If not defined means not verified
    pub source: Option<SymbolSource>,
}

#[derive(Debug)]
pub enum SymbolSource {
    BuiltIn,
    UserDefined {
        filename: String,
        position: Position,
    },
}

#[derive(Debug)]
pub enum Type {
    Number,
    String,
    Name(Token),
    Tag(Token),
    Underscore,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    Tag(Token),
    Function(Function),
    FunctionCall(FunctionCall),
    Record {
        key_value_pairs: Vec<RecordKeyValue>,
    },
    Let {
        left: DestructurePattern,
        right: Box<Expression>,
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
    Comma,
    ArrowRight,
    Backslash,
    Underscore,
    Tag,
    Identifier,
    String,
    Number,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
