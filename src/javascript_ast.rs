pub mod javascript {
    use crate::{
        non_empty::NonEmpty,
        tail_call_optimisation::{eliminate_tail_call, TailCall},
    };

    #[derive(Debug, Clone)]
    pub enum Statement {
        Assignment {
            is_declaration: bool,
            assignment: Assignment,
        },
        Expression(Expression),
        Return(Expression),
        If {
            condition: Expression,
            if_true: Vec<Statement>,
        },
        While {
            condition: Expression,
            body: Vec<Statement>,
        },
    }

    #[derive(Debug, Clone)]
    pub struct Assignment {
        pub left: AssignmentLeft,
        pub right: Expression,
    }

    #[derive(Debug, Clone)]
    pub enum AssignmentLeft {
        Variable(Identifier),
        Object {
            name: Identifier,
            accesses: NonEmpty<Expression>,
        },
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Identifier(pub String);

    #[derive(Debug, Clone)]
    pub enum Expression {
        /// Namely, something like (a, b, c), but not that this is not a tuple in JavaScript.
        /// Refer https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comma_Operator
        Sequence(Box<NonEmpty<Expression>>),
        LogicalOr {
            left: Box<Expression>,
            right: Box<Expression>,
        },
        LogicalAnd {
            left: Box<Expression>,
            right: Box<Expression>,
        },
        Equals {
            left: Box<Expression>,
            right: Box<Expression>,
        },
        Null,
        Boolean(bool),
        Variable(Identifier),
        String(String),
        Number {
            representation: String,
        },
        StringConcat(Vec<Expression>),
        Array(Vec<Expression>),
        FunctionCall {
            function: Box<Expression>,
            arguments: Vec<Expression>,
        },
        Function(Function),
        Object(Vec<ObjectKeyValue>),
        ObjectWithSpread {
            spread: Box<Expression>,
            key_values: Vec<ObjectKeyValue>,
        },
        Conditional {
            condition: Box<Expression>,
            if_true: Box<Expression>,
            if_false: Box<Expression>,
        },
        MemberAccess {
            object: Box<Expression>,
            property: Box<Expression>,
        },
        UnsafeJavascriptCode(String),
        Assignment(Box<Assignment>),
    }

    #[derive(Debug, Clone)]
    pub struct Function {
        pub parameters: Vec<Identifier>,
        pub body: Vec<Statement>,
    }

    #[derive(Debug, Clone)]
    pub struct ObjectKeyValue {
        pub key: Identifier,
        pub value: Option<Box<Expression>>,
    }

    pub fn print_multiple<T: Printable>(vector: Vec<T>, join_by: &str) -> String {
        vector
            .into_iter()
            .map(Printable::print)
            .collect::<Vec<String>>()
            .join(join_by)
    }

    pub trait Printable {
        fn print(self) -> String;
    }

    impl Printable for Statement {
        fn print(self) -> String {
            match self {
                Statement::Assignment {
                    is_declaration,
                    assignment,
                } => {
                    format!(
                        "{} {}",
                        if is_declaration { "var" } else { "" },
                        assignment.print(),
                    )
                }
                Statement::Expression(expression) => expression.print(),
                Statement::Return(expression) => {
                    format!("return {}", expression.print())
                }
                Statement::If { condition, if_true } => {
                    format!(
                        "if({}){{{}}}",
                        condition.print(),
                        print_multiple(if_true, ";\n\n")
                    )
                }
                Statement::While { condition, body } => {
                    format!(
                        "while({}){{{}}}",
                        condition.print(),
                        print_multiple(body, ";\n\n")
                    )
                }
            }
        }
    }
    impl Printable for AssignmentLeft {
        fn print(self) -> String {
            match self {
                AssignmentLeft::Variable(identifier) => identifier.print(),
                AssignmentLeft::Object { name, accesses } => {
                    format!(
                        "{}{}",
                        name.print(),
                        accesses
                            .map(|access| access.print())
                            .map(|expression| { format!("[{}]", expression) })
                            .into_vector()
                            .join("")
                    )
                }
            }
        }
    }
    impl Printable for Assignment {
        fn print(self) -> String {
            let right = match (&self.left, &self.right) {
                (AssignmentLeft::Variable(identifier), Expression::Function(function)) => {
                    if function.body.in_tail_position(identifier) {
                        Expression::Function(eliminate_tail_call(identifier, function.clone()))
                    } else {
                        self.right
                    }
                }
                _ => self.right,
            };
            let right = right.print();
            let left = self.left.print();
            format!("{} = {}", left, right)
        }
    }
    impl Printable for Expression {
        fn print(self) -> String {
            match self {
                Expression::LogicalOr { left, right } => {
                    format!("({}) || ({})", left.print(), right.print())
                }
                Expression::LogicalAnd { left, right } => {
                    format!("({}) && ({})", left.print(), right.print())
                }
                Expression::Equals { left, right } => {
                    format!("({}) === ({})", left.print(), right.print())
                }
                Expression::Null => "null".to_string(),
                Expression::Boolean(value) => (if value { "true" } else { "false" }).to_string(),
                Expression::Variable(identifier) => identifier.print(),
                Expression::String(string) => {
                    if string.starts_with('"') {
                        string
                    } else {
                        format!("(\"{}\")", string)
                    }
                }
                Expression::Number { representation } => representation,
                Expression::StringConcat(expressions) => {
                    let result = expressions
                        .into_iter()
                        .map(|expression| format!("({})", expression.print()))
                        .collect::<Vec<String>>()
                        .join("+");
                    format!("({})", result)
                }
                Expression::Array(expressions) => {
                    let elements = print_multiple(expressions, ",");
                    format!("[{}]", elements)
                }
                Expression::FunctionCall {
                    function,
                    arguments,
                } => {
                    format!(
                        "(({})({}))",
                        function.print(),
                        print_multiple(arguments, ", ")
                    )
                }
                Expression::Function(Function { parameters, body }) => {
                    format!(
                        "(({}) => {{{}}})",
                        print_multiple(parameters, ","),
                        print_multiple(body, ";\n\n")
                    )
                }
                Expression::Object(key_values) => {
                    format!("{{{}}}", print_multiple(key_values, ","),)
                }
                Expression::ObjectWithSpread { spread, key_values } => {
                    format!(
                        "{{...{}, {}}}",
                        spread.print(),
                        print_multiple(key_values, ",")
                    )
                }
                Expression::Conditional {
                    condition,
                    if_true,
                    if_false,
                } => {
                    format!(
                        "(({}) ? ({}) : ({}))",
                        condition.print(),
                        if_true.print(),
                        if_false.print()
                    )
                }
                Expression::MemberAccess { object, property } => {
                    format!("({})[{}]", object.print(), property.print())
                }
                Expression::UnsafeJavascriptCode(code) => code,
                Expression::Sequence(expressions) => {
                    format!("({})", print_multiple(expressions.into_vector(), ", "))
                }
                Expression::Assignment(assignment) => {
                    format!("({})", assignment.print())
                }
            }
        }
    }
    impl Printable for ObjectKeyValue {
        fn print(self) -> String {
            format!(
                "{}: {}",
                self.key.clone().print(),
                match self.value {
                    Some(value) => value.print(),
                    None => self.key.print(),
                }
            )
        }
    }
    impl Printable for Identifier {
        fn print(self) -> String {
            self.0
        }
    }
}
