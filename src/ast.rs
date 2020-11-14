#[derive(Debug)]
pub enum Statement {
    Let {
        left: DestructurePattern,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
}

#[derive(Debug)]
pub enum TypeAnnotation {}

#[derive(Debug)]
pub enum DestructurePattern {
    Identifier(Token),
}

#[derive(Debug)]
pub enum Expression {
    String(Token),
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
    Tag(String),
    Identifier(String),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
