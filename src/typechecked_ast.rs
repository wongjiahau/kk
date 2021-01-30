use crate::non_empty::NonEmpty;
/// The syntax tree here represents the syntax tree that is type-checked
/// Which contain information necessary for the transpilation
/// For example, multiple-dispatch requires the typechecker
/// to point out which exact function is being used
///
/// NOTE for developers:
/// (1) To reduce the problem of impedance mismatched,
///     please use the same property name as in ast.rs whenever possible

#[derive(Debug, Clone)]
pub enum TypecheckedStatement {
    Let {
        left: Variable,
        right: TypecheckedExpression,
    },
    Do {
        expression: TypecheckedExpression,
    },
}

#[derive(Debug, Clone)]
pub struct Variable {
    /// This is needed for disambiguating overloaded functions
    pub uid: usize,
    pub representation: String,
}

#[derive(Debug, Clone)]
pub enum TypecheckedExpression {
    Null,
    Boolean(bool),
    Number {
        representation: String,
    },
    String {
        representation: String,
    },
    Variable(Variable),
    EnumConstructor {
        constructor_name: String,
        payload: Option<Box<TypecheckedExpression>>,
    },
    Function(Box<TypecheckedFunction>),
    FunctionCall(Box<TypecheckedFunctionCall>),
    Record {
        key_value_pairs: Vec<(String, TypecheckedExpression)>,
    },
    RecordAccess {
        expression: Box<TypecheckedExpression>,
        property_name: String,
    },
    Array {
        elements: Vec<TypecheckedExpression>,
    },
}

#[derive(Debug, Clone)]
pub struct TypecheckedFunctionCall {
    pub function: Box<TypecheckedExpression>,
    pub first_argument: Box<TypecheckedExpression>,
    pub rest_arguments: Vec<TypecheckedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypecheckedFunction {
    pub branches: Box<NonEmpty<TypecheckedFunctionBranch>>,
}

#[derive(Debug, Clone)]
pub struct TypecheckedFunctionBranch {
    pub parameters: Box<NonEmpty<TypecheckedDestructurePattern>>,
    pub body: Box<TypecheckedExpression>,
}

#[derive(Debug, Clone)]
pub enum TypecheckedDestructurePattern {
    String {
        representation: String,
    },
    Number {
        representation: String,
    },
    Boolean(bool),
    Null,
    Underscore,
    Variable(Variable),
    EnumConstructor {
        constructor_name: String,
        payload: Option<Box<TypecheckedDestructurePattern>>,
    },
    Record {
        key_pattern_pairs: Vec<(String, TypecheckedDestructurePattern)>,
    },
    Array {
        spread: Option<TypecheckedDesturcturePatternArraySpread>,
    },
    Tuple {
        values: Box<NonEmpty<TypecheckedDestructurePattern>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypecheckedDesturcturePatternArraySpread {
    pub first_element: Box<TypecheckedDestructurePattern>,
    pub rest_elements: Box<TypecheckedDestructurePattern>,
}