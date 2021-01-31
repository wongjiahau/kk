use crate::non_empty::NonEmpty;
/// The syntax tree here represents raw syntax tree that is not type checked

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        keyword_let: Token,
        left: Token,
        type_variables: Vec<Token>,
        right: Expression,
        type_annotation: Option<TypeAnnotation>,
    },
    Type {
        keyword_type: Token,
        left: Token,
        right: TypeAnnotation,
        type_variables: Vec<Token>,
    },
    Enum {
        keyword_enum: Token,
        name: Token,
        constructors: Vec<EnumConstructor>,
        type_variables: Vec<Token>,
    },
    Do {
        keyword_do: Token,
        expression: Expression,
    },
}

#[derive(Debug, Clone)]
pub struct EnumConstructor {
    pub name: Token,
    pub left_parenthesis: Token,
    pub payload: Option<Box<TypeAnnotation>>,
    pub right_parenthesis: Token,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub token: Token,
}

#[derive(Debug, Clone)]
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
    Named {
        name: String,
        type_arguments: Vec<(String, Type)>,
    },
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum DestructurePattern {
    String(Token),
    Character(Token),
    Integer(Token),
    Boolean {
        token: Token,
        value: bool,
    },
    Null(Token),
    Underscore(Token),
    Identifier(Token),
    EnumConstructor {
        scoped_name: ScopedName,
        left_parenthesis: Token,
        payload: Option<Box<DestructurePattern>>,
        right_parenthesis: Token,
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
    EnumConstructor {
        scoped_name: ScopedName,
        left_parenthesis: Token,
        payload: Option<Box<Expression>>,
        right_parenthesis: Token,
    },
    Function(Box<Function>),
    FunctionCall(Box<FunctionCall>),
    Record {
        left_curly_bracket: Token,
        spread: Option<Box<Expression>>,
        key_value_pairs: Vec<RecordKeyValue>,
        right_curly_bracket: Token,
    },
    RecordAccess {
        expression: Box<Expression>,
        property_name: Token,
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
}

#[derive(Debug, Clone)]
pub struct RecordKeyValue {
    pub key: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function_name: Token,
    pub first_argument: Box<Expression>,
    pub rest_arguments: Option<FunctionCallRestArguments>,
    pub type_arguments: Option<TypeArguments>,
}

#[derive(Debug, Clone)]
pub struct TypeArguments {
    pub left_angular_bracket: Token,
    pub substitutions: Vec<(/*type_variable_name*/ Token, TypeAnnotation)>,
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
    pub parameters: FunctionParameters,
    pub body: Box<Expression>,
    pub return_type_annotation: Option<TypeAnnotation>,
}

impl FunctionBranch {
    pub fn parameters(&self) -> NonEmpty<FunctionParameter> {
        match &self.parameters {
            FunctionParameters::NoParenthesis { parameter } => NonEmpty {
                head: parameter.clone(),
                tail: vec![],
            },
            FunctionParameters::WithParenthesis { parameters, .. } => parameters.clone(),
        }
    }
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
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
    pub representation: String,
}

#[derive(Debug, Clone)]
pub struct ScopedName {
    pub namespaces: Vec<Token>,
    pub name: Token,
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
    Backslash,
    Underscore,
    Identifier,
    String,
    Character,
    Integer,
    Float,
    ScopeResolution,
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
