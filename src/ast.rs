#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        left: Token,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
    Type {
        left: Token,
        right: TypeAnnotation,
        type_variables: Vec<Token>,
    },
    Enum {
        name: Token,
        tags: Vec<EnumTag>,
        type_variables: Vec<Token>,
    },
}

#[derive(Debug, Clone)]
pub struct EnumTag {
    pub tagname: Token,
    pub payload: Option<EnumTagPayload>,
}

#[derive(Debug, Clone)]
pub struct EnumTagPayload {
    pub left_parenthesis: Token,
    pub type_annotation: Box<TypeAnnotation>,
    pub right_parenthesis: Token,
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
    Tuple(Vec<Type>),
    Boolean,
    Number,
    String,
    Null,
    Array(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct TypeEnumTag {
    tagname: String,
    payload: Option<Type>,
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
    pub first_argument_type: Box<Type>,
    pub rest_arguments_types: Vec<Type>,
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
    Named {
        name: Token,
        arguments: Option<NamedTypeAnnotationArguments>,
    },
    Record {
        left_curly_bracket: Token,
        key_type_annotation_pairs: Vec<(Token, TypeAnnotation)>,
        right_curly_bracket: Token,
    },
    Underscore(Token),
    Union {
        type_annotations: Vec<TypeAnnotation>,
    },
    Function {
        start_token: Token,
        first_argument_type: Box<TypeAnnotation>,
        rest_arguments_types: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },
}

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    String(Token),
    Number(Token),
    Boolean {
        token: Token,
        value: bool,
    },
    Null(Token),
    Underscore(Token),
    Identifier(Token),
    Tag {
        tagname: Token,
        payload: Option<Box<DestructurePatternTagPayload>>,
    },
    Record {
        left_curly_bracket: Token,
        key_value_pairs: Vec<DestructuredRecordKeyValue>,
        right_curly_bracket: Token,
    },
    Array {
        left_square_bracket: Token,
        right_square_bracket: Token,
        initial_elements: Vec<DestructurePattern>,
        spread: Option<DestructurePatternArraySpread>,
        tail_elements: Vec<DestructurePattern>,
    },
    Tuple {
        left_parenthesis: Token,
        values: Vec<DestructurePattern>,
        right_parenthesis: Token,
    },
}

#[derive(Debug, Clone)]
pub struct DestructurePatternArraySpread {
    pub spread_symbol: Token,
    pub binding: Option<Token>,
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
    Null(Token),
    Boolean {
        token: Token,
        value: bool,
    },
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
    Array {
        left_square_bracket: Token,
        elements: Vec<Expression>,
        right_square_bracket: Token,
    },
    Let {
        let_keyword: Token,
        left: Box<DestructurePattern>,
        type_annotation: Option<TypeAnnotation>,
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
    pub first_argument: Box<Expression>,
    pub rest_arguments: Option<FunctionCallRestArguments>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallRestArguments {
    pub left_parenthesis: Token,
    pub arguments: Vec<Expression>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub first_branch: FunctionBranch,
    pub branches: Vec<FunctionBranch>,
}

#[derive(Debug, Clone)]
pub struct FunctionBranch {
    pub start_token: Token,
    pub first_argument: Box<FunctionArgument>,
    pub rest_arguments: Option<FunctionBranchRestArguments>,
    pub body: Box<Expression>,
    pub return_type_annotation: Option<TypeAnnotation>,
}

impl FunctionBranch {
    pub fn rest_arguments(&self) -> Vec<FunctionArgument> {
        match &self.rest_arguments {
            Some(FunctionBranchRestArguments { rest_arguments, .. }) => rest_arguments.clone(),
            None => vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBranchRestArguments {
    pub left_parenthesis: Token,
    pub rest_arguments: Vec<FunctionArgument>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
    pub destructure_pattern: DestructurePattern,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    ExpectedTypeAnnotation {
        actual_token: Token,
    },
    UnterminatedString {
        position: Position,
    },
    ArrayCannotContainMoreThanOneSpread {
        extraneous_spread: Token,
    },
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
    KeywordEnum,
    KeywordElse,
    KeywordNull,
    KeywordTrue,
    KeywordFalse,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
    pub character_index_start: usize,
    pub character_index_end: usize,
}
