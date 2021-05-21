use crate::{
    ast::InfinitePatternKind,
    non_empty::NonEmpty,
    typechecked_ast::*,
    unify::{TypecheckedModule, UnifyProgramResult},
};

mod javascript {
    use crate::non_empty::NonEmpty;

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

    #[derive(Debug, Clone)]
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
        ArrowFunction {
            parameters: Vec<Identifier>,
            body: Vec<Statement>,
        },
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
    pub struct ObjectKeyValue {
        pub key: Identifier,
        pub value: Option<Box<Expression>>,
    }

    pub fn print_statements(statements: Vec<Statement>) -> String {
        statements
            .into_iter()
            .map(Printable::print)
            .collect::<Vec<String>>()
            .join(";\n")
    }

    trait Printable {
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
                    format!("if({}){{{}}}", condition.print(), print_statements(if_true))
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
            let right = self.right.print();
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
                    let elements = expressions
                        .into_iter()
                        .map(Printable::print)
                        .collect::<Vec<String>>()
                        .join(",");
                    format!("[{}]", elements)
                }
                Expression::FunctionCall {
                    function,
                    arguments,
                } => {
                    format!(
                        "(({})({}))",
                        function.print(),
                        arguments
                            .into_iter()
                            .map(Printable::print)
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
                Expression::ArrowFunction { parameters, body } => {
                    format!(
                        "(({}) => {{{}}})",
                        parameters
                            .into_iter()
                            .map(Printable::print)
                            .collect::<Vec<String>>()
                            .join(","),
                        print_statements(body)
                    )
                }
                Expression::Object(key_values) => {
                    format!(
                        "{{{}}}",
                        key_values
                            .into_iter()
                            .map(Printable::print)
                            .collect::<Vec<String>>()
                            .join(",")
                    )
                }
                Expression::ObjectWithSpread { spread, key_values } => {
                    format!(
                        "{{...{}, {}}}",
                        spread.print(),
                        key_values
                            .into_iter()
                            .map(Printable::print)
                            .collect::<Vec<String>>()
                            .join(",")
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
                    format!(
                        "({})",
                        expressions.map(Printable::print).into_vector().join(", ")
                    )
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

const KK_MODULE: &str = "KK_MODULE";
const ENUM_TAG_NAME: &str = "$";
const ENUM_PAYLOAD_NAME: &str = "_";

pub fn transpile_program(unify_project_result: UnifyProgramResult) -> String {
    // TODO: move this to a file
    let built_in_library =
        javascript::Statement::Expression(javascript::Expression::UnsafeJavascriptCode(
            " const print_0 = (x) => console.log(x); ".to_string(),
        ));

    let global_module_dictionary_declaration = javascript::Statement::Assignment {
        is_declaration: true,
        assignment: javascript::Assignment {
            left: javascript::AssignmentLeft::Variable(javascript::Identifier(
                KK_MODULE.to_string(),
            )),
            right: javascript::Expression::Object(vec![]),
        },
    };
    let imported_modules = unify_project_result
        .imported_modules
        .into_iter()
        .map(|(_, module)| transpile_module(module))
        .collect::<Vec<javascript::Statement>>();

    let entry_module = transpile_module(unify_project_result.entrypoint);

    let statements = vec![built_in_library, global_module_dictionary_declaration]
        .into_iter()
        .chain(imported_modules)
        .chain(vec![entry_module])
        .collect::<Vec<javascript::Statement>>();

    javascript::print_statements(statements)
}

pub fn transpile_module(module: TypecheckedModule) -> javascript::Statement {
    let module_uid = module.module.meta.uid.string_value();
    let statements = transpile_statements(module.statements.clone());
    let exported_symbols = module
        .statements
        .iter()
        .flat_map(|statement| match statement {
            TypecheckedStatement::Let { exported, left, .. } if *exported => {
                get_destructure_pattern_bindings(left.kind.clone())
            }
            _ => vec![],
        })
        .map(transpile_identifier)
        .map(|identifier| javascript::ObjectKeyValue {
            key: identifier,
            value: None,
        })
        .collect::<Vec<javascript::ObjectKeyValue>>();

    javascript::Statement::Assignment {
        is_declaration: false,
        assignment: javascript::Assignment {
            left: javascript::AssignmentLeft::Object {
                name: javascript::Identifier(KK_MODULE.to_string()),
                accesses: NonEmpty {
                    head: javascript::Expression::String(module_uid),
                    tail: vec![],
                },
            },
            right: javascript::Expression::FunctionCall {
                arguments: vec![],
                function: Box::new(javascript::Expression::ArrowFunction {
                    parameters: vec![],
                    body: statements
                        .into_iter()
                        .chain(vec![javascript::Statement::Return(
                            javascript::Expression::Object(exported_symbols),
                        )])
                        .collect(),
                }),
            },
        },
    }
}

pub fn get_destructure_pattern_bindings(
    destructure_pattern: TypecheckedDestructurePatternKind,
) -> Vec<Identifier> {
    match destructure_pattern {
        TypecheckedDestructurePatternKind::Infinite { .. }
        | TypecheckedDestructurePatternKind::Boolean { .. }
        | TypecheckedDestructurePatternKind::Null(_)
        | TypecheckedDestructurePatternKind::Underscore(_) => vec![],
        TypecheckedDestructurePatternKind::Identifier(name) => vec![*name],
        TypecheckedDestructurePatternKind::EnumConstructor { payload, .. } => match payload {
            None => vec![],
            Some(payload) => get_destructure_pattern_bindings(payload.pattern.kind),
        },
        TypecheckedDestructurePatternKind::Record {
            key_pattern_pairs, ..
        } => key_pattern_pairs
            .into_iter()
            .flat_map(|(_, pattern)| get_destructure_pattern_bindings(pattern.kind))
            .collect(),
        TypecheckedDestructurePatternKind::Array { .. } => {
            panic!("")
        }
        TypecheckedDestructurePatternKind::Tuple { patterns } => patterns
            .into_vector()
            .into_iter()
            .flat_map(|pattern| get_destructure_pattern_bindings(pattern.kind))
            .collect(),
        TypecheckedDestructurePatternKind::Or { patterns } => {
            get_destructure_pattern_bindings(patterns.first().kind.clone())
        }
    }
}

pub fn transpile_statements(statements: Vec<TypecheckedStatement>) -> Vec<javascript::Statement> {
    statements
        .into_iter()
        .flat_map(transpile_statement)
        .collect()
}

pub fn transpile_statement(statement: TypecheckedStatement) -> Vec<javascript::Statement> {
    match statement {
        TypecheckedStatement::Expression(expression) => {
            vec![javascript::Statement::Expression(transpile_expression(
                expression,
            ))]
        }
        TypecheckedStatement::Let { left, right, .. } => {
            let TranspiledDestructurePattern {
                bindings,
                conditions: _, // Note: conditions can be ignored as we assume that the type checker already checked for case exhaustiveness
            } = transpile_destructure_pattern(left.kind, transpile_expression(right));
            bindings
                .into_iter()
                .map(|binding| javascript::Statement::Assignment {
                    is_declaration: true,
                    assignment: binding,
                })
                .collect()
        }
        TypecheckedStatement::ImportStatement(TypecheckedImportStatement {
            module_uid,
            imported_name,
            imported_as,
        }) => {
            vec![javascript::Statement::Assignment {
                is_declaration: true,
                assignment: javascript::Assignment {
                    left: javascript::AssignmentLeft::Variable(transpile_identifier(imported_as)),
                    right: javascript::Expression::MemberAccess {
                        object: Box::new(javascript::Expression::MemberAccess {
                            object: Box::new(javascript::Expression::Variable(
                                javascript::Identifier(KK_MODULE.to_string()),
                            )),
                            property: Box::new(javascript::Expression::String(
                                module_uid.string_value(),
                            )),
                        }),
                        property: Box::new(javascript::Expression::String(
                            transpile_identifier(imported_name).0,
                        )),
                    },
                },
            }]
        }
    }
}

fn transpile_identifier(identifier: Identifier) -> javascript::Identifier {
    // Note that when we transpile identifier, we omit the module_uid and only use the index
    javascript::Identifier(format!(
        "{}_{}",
        identifier.token.representation, identifier.uid.index
    ))
}

pub fn transpile_expression(expression: TypecheckedExpression) -> javascript::Expression {
    match expression {
        TypecheckedExpression::Null => javascript::Expression::Null,
        TypecheckedExpression::Boolean(value) => javascript::Expression::Boolean(value),
        TypecheckedExpression::String { representation }
        | TypecheckedExpression::Character { representation } => {
            javascript::Expression::String(representation)
        }
        TypecheckedExpression::Float { representation }
        | TypecheckedExpression::Integer { representation } => {
            javascript::Expression::Number { representation }
        }
        TypecheckedExpression::InterpolatedString { sections } => {
            javascript::Expression::StringConcat(
                sections
                    .into_iter()
                    .map(|section| match section {
                        TypecheckedInterpolatedStringSection::String(string) => {
                            javascript::Expression::String(string)
                        }
                        TypecheckedInterpolatedStringSection::Expression(expression) => {
                            transpile_expression(*expression)
                        }
                    })
                    .collect::<Vec<javascript::Expression>>(),
            )
        }
        TypecheckedExpression::Variable(variable) => {
            javascript::Expression::Variable(transpile_identifier(variable))
        }
        TypecheckedExpression::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => javascript::Expression::Object(vec![
            javascript::ObjectKeyValue {
                key: javascript::Identifier(ENUM_TAG_NAME.to_string()),
                value: Some(Box::new(javascript::Expression::String(constructor_name))),
            },
            javascript::ObjectKeyValue {
                key: javascript::Identifier(ENUM_PAYLOAD_NAME.to_string()),
                value: Some(match payload {
                    Some(payload) => Box::new(transpile_expression(*payload)),
                    None => Box::new(javascript::Expression::Null),
                }),
            },
        ]),
        TypecheckedExpression::RecordAccess {
            expression,
            property_name,
        } => javascript::Expression::MemberAccess {
            object: Box::new(transpile_expression(*expression)),
            property: Box::new(javascript::Expression::String(transpile_property_name(
                property_name,
            ))),
        },
        TypecheckedExpression::Record { key_value_pairs } => javascript::Expression::Object(
            key_value_pairs
                .into_iter()
                .map(|(key, value)| javascript::ObjectKeyValue {
                    key: javascript::Identifier(transpile_property_name(key)),
                    value: Some(Box::new(transpile_expression(value))),
                })
                .collect::<Vec<javascript::ObjectKeyValue>>(),
        ),
        TypecheckedExpression::BranchedFunction(function) => {
            let TypecheckedBranchedFunction { branches } = *function;
            let number_of_args = branches.first().parameters.len();
            let parameters = (0..number_of_args)
                .map(build_function_argument)
                .collect::<Vec<javascript::Identifier>>();
            let body = branches
                .map(transpile_function_branch)
                .into_vector()
                .into_iter()
                .flatten()
                .collect();
            javascript::Expression::ArrowFunction { parameters, body }
        }
        TypecheckedExpression::FunctionCall(function) => {
            let TypecheckedFunctionCall {
                function,
                first_argument,
                rest_arguments,
            } = *function;
            let function = transpile_expression(*function);
            let arguments = vec![*first_argument]
                .into_iter()
                .chain(rest_arguments.into_iter())
                .map(transpile_expression)
                .collect::<Vec<javascript::Expression>>();

            javascript::Expression::FunctionCall {
                function: Box::new(function),
                arguments,
            }
        }
        TypecheckedExpression::Array { elements, .. } => {
            javascript::Expression::Array(elements.into_iter().map(transpile_expression).collect())
        }
        TypecheckedExpression::RecordUpdate {
            expression,
            updates,
        } => {
            let temporary_identifier = javascript::Identifier("$".to_string());
            let temporary_variable = javascript::Expression::Variable(temporary_identifier.clone());
            let updates = updates
                .into_iter()
                .map(|update| match update {
                    TypecheckedRecordUpdate::ValueUpdate {
                        property_name,
                        new_value,
                    } => javascript::ObjectKeyValue {
                        key: javascript::Identifier(transpile_property_name(property_name)),
                        value: Some(Box::new(transpile_expression(new_value))),
                    },
                    TypecheckedRecordUpdate::FunctionalUpdate {
                        property_name,
                        function,
                    } => {
                        let property_name = transpile_property_name(property_name);
                        javascript::ObjectKeyValue {
                            key: javascript::Identifier(property_name.clone()),
                            value: Some(Box::new(javascript::Expression::FunctionCall {
                                function: Box::new(transpile_expression(function)),
                                arguments: vec![javascript::Expression::MemberAccess {
                                    object: Box::new(temporary_variable.clone()),
                                    property: Box::new(javascript::Expression::String(
                                        property_name,
                                    )),
                                }],
                            })),
                        }
                    }
                })
                .collect::<Vec<javascript::ObjectKeyValue>>();

            javascript::Expression::FunctionCall {
                function: Box::new(javascript::Expression::ArrowFunction {
                    parameters: vec![temporary_identifier],
                    body: vec![javascript::Statement::Return(
                        javascript::Expression::ObjectWithSpread {
                            spread: Box::new(temporary_variable),
                            key_values: updates,
                        },
                    )],
                }),
                arguments: vec![transpile_expression(*expression)],
            }
        }
        TypecheckedExpression::Javascript { code } => {
            javascript::Expression::UnsafeJavascriptCode(code)
        }
        TypecheckedExpression::If {
            condition,
            if_true,
            if_false,
        } => javascript::Expression::Conditional {
            condition: Box::new(transpile_expression(*condition)),
            if_true: Box::new(transpile_expression(*if_true)),
            if_false: Box::new(transpile_expression(*if_false)),
        },
        TypecheckedExpression::Block {
            statements,
            return_value,
        } => javascript::Expression::FunctionCall {
            arguments: vec![],
            function: Box::new(javascript::Expression::ArrowFunction {
                parameters: vec![],
                body: transpile_statements(statements)
                    .into_iter()
                    .chain(vec![javascript::Statement::Return(transpile_expression(
                        *return_value,
                    ))])
                    .collect(),
            }),
        },
        TypecheckedExpression::ConstrainedVariable {
            identifier,
            constraints,
        } => {
            todo!("Use a different AST for transpiling, because transpiling should not has ConstraintVariable,
            they should be solved as dictionary.
            Suggested naming: 

            Raw -> Inferred -> Solved
            ")
        }
    }
}

fn transpile_property_name(property_name: PropertyName) -> String {
    format!("${}", property_name.0.representation)
}

pub fn build_function_argument(index: usize) -> javascript::Identifier {
    javascript::Identifier(format!("_{}", index))
}

pub fn transpile_function_branch(
    function_branch: TypecheckedFunctionBranch,
) -> Vec<javascript::Statement> {
    let transpiled_destructure_pattern = function_branch
        .parameters
        .into_vector()
        .into_iter()
        .enumerate()
        .map(|(index, pattern)| {
            transpile_destructure_pattern(
                pattern.kind,
                javascript::Expression::Variable(build_function_argument(index)),
            )
        })
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

    let body = transpiled_destructure_pattern
        .bindings
        .into_iter()
        .map(|binding| javascript::Statement::Assignment {
            is_declaration: true,
            assignment: binding,
        })
        .chain(vec![javascript::Statement::Return(transpile_expression(
            *function_branch.body,
        ))])
        .collect::<Vec<javascript::Statement>>();
    match join_expressions(transpiled_destructure_pattern.conditions, |left, right| {
        javascript::Expression::LogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
        }
    }) {
        None => body,
        Some(condition) => {
            vec![javascript::Statement::If {
                condition,
                if_true: body,
            }]
        }
    }
}

/// Join a list of experssions with the given join function.
/// Concept:
/// ```
/// join_expressions([a, b, c], &&) === Some(a && b && c)
/// ```
fn join_expressions<Join>(
    expressions: Vec<javascript::Expression>,
    join: Join,
) -> Option<javascript::Expression>
where
    Join: Fn(javascript::Expression, javascript::Expression) -> javascript::Expression,
{
    match expressions.split_first() {
        None => None,
        Some((first, tail)) => {
            let condition = match tail.split_first() {
                None => first.clone(),
                Some((second, tail)) => {
                    let init = join(first.clone(), second.clone());
                    tail.iter()
                        .fold(init, |result, expression| join(result, expression.clone()))
                }
            };
            Some(condition)
        }
    }
}

#[derive(Debug)]
pub struct TranspiledDestructurePattern {
    conditions: Vec<javascript::Expression>,
    bindings: Vec<javascript::Assignment>,
}

#[derive(Debug)]
struct Binding {
    name: Identifier,
    expression: TypecheckedExpression,
}

pub fn transpile_destructure_pattern(
    destructure_pattern: TypecheckedDestructurePatternKind,
    from_expression: javascript::Expression,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        TypecheckedDestructurePatternKind::Infinite { token, kind } => {
            TranspiledDestructurePattern {
                conditions: vec![javascript::Expression::Equals {
                    left: Box::new(from_expression),
                    right: match kind {
                        InfinitePatternKind::String | InfinitePatternKind::Character => {
                            Box::new(javascript::Expression::String(token.representation))
                        }
                        InfinitePatternKind::Integer => Box::new(javascript::Expression::Number {
                            representation: token.representation,
                        }),
                    },
                }],
                bindings: vec![],
            }
        }
        TypecheckedDestructurePatternKind::Boolean { value, .. } => TranspiledDestructurePattern {
            conditions: vec![javascript::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(javascript::Expression::Boolean(value)),
            }],
            bindings: vec![],
        },
        TypecheckedDestructurePatternKind::Null { .. } => TranspiledDestructurePattern {
            conditions: vec![javascript::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(javascript::Expression::Null),
            }],
            bindings: vec![],
        },
        TypecheckedDestructurePatternKind::Underscore { .. } => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        TypecheckedDestructurePatternKind::Array { .. } => {
            panic!();
            // match spread {
            //     None => TranspiledDestructurePattern {
            //         conditions: vec![format!("{}.length === 0", from_expression)],
            //         bindings: vec![],
            //     },
            //     Some(spread) => join_transpiled_destructure_patterns(vec![
            //         TranspiledDestructurePattern {
            //             conditions: vec![format!("{}.length > 0", from_expression)],
            //             bindings: vec![],
            //         },
            //         transpile_destructure_pattern(
            //             spread.first_element.kind,
            //             format!("{}[0]", from_expression),
            //         ),
            //         transpile_destructure_pattern(
            //             spread.rest_elements.kind,
            //             format!("{}.slice(1)", from_expression),
            //         ),
            //     ]),
            // }
        }
        TypecheckedDestructurePatternKind::Identifier(variable) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![javascript::Assignment {
                left: javascript::AssignmentLeft::Variable(transpile_identifier(*variable)),
                right: from_expression,
            }],
        },
        TypecheckedDestructurePatternKind::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => {
            let match_enum_tagname = javascript::Expression::Equals {
                left: Box::new(javascript::Expression::MemberAccess {
                    object: Box::new(from_expression.clone()),
                    property: Box::new(javascript::Expression::String(ENUM_TAG_NAME.to_string())),
                }),
                right: Box::new(javascript::Expression::String(
                    constructor_name.representation,
                )),
            };
            let first = TranspiledDestructurePattern {
                conditions: vec![match_enum_tagname],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(payload) => Some(transpile_destructure_pattern(
                    payload.pattern.kind,
                    javascript::Expression::MemberAccess {
                        object: Box::new(from_expression),
                        property: Box::new(javascript::Expression::String(
                            ENUM_PAYLOAD_NAME.to_string(),
                        )),
                    },
                )),
            };
            match rest {
                None => first,
                Some(rest) => join_transpiled_destructure_pattern(first, rest),
            }
        }
        TypecheckedDestructurePatternKind::Record {
            key_pattern_pairs, ..
        } => key_pattern_pairs.into_iter().fold(
            TranspiledDestructurePattern {
                bindings: vec![],
                conditions: vec![],
            },
            |result, (key, destructure_pattern)| {
                join_transpiled_destructure_pattern(
                    result,
                    transpile_destructure_pattern(
                        destructure_pattern.kind,
                        javascript::Expression::MemberAccess {
                            object: Box::new(from_expression.clone()),
                            property: Box::new(javascript::Expression::String(
                                transpile_property_name(key),
                            )),
                        },
                    ),
                )
            },
        ),
        TypecheckedDestructurePatternKind::Tuple { .. } => {
            panic!("Compiler error, should not reach here")
        }

        // The current transpilation for OR patterns is a bit hacky as we are abusing assignment expression
        // in Javascript.
        // See the description of `into_in_place_assignment` for more information.
        TypecheckedDestructurePatternKind::Or { patterns } => {
            let in_place_assignments = patterns
                .map(|pattern| transpile_destructure_pattern(pattern.kind, from_expression.clone()))
                .map(into_in_place_assignment)
                .into_vector();

            /// Turn a `transpiled_destructure_pattern` into an in-place assignment.
            /// For example:
            /// ```
            /// X({a, b})
            /// ```
            /// Should be converted into
            /// ```
            /// (bar.$==="X" && ((a = bar._.a, b = bar._._b) || true))
            /// ```
            fn into_in_place_assignment(
                pattern: TranspiledDestructurePattern,
            ) -> javascript::Expression {
                let assignments = {
                    match pattern
                        .bindings
                        .into_iter()
                        .map(|binding| javascript::Expression::Assignment(Box::new(binding)))
                        .collect::<Vec<javascript::Expression>>()
                        .split_first()
                    {
                        None => javascript::Expression::Boolean(true),
                        Some((head, tail)) => javascript::Expression::LogicalOr {
                            left: Box::new(javascript::Expression::Sequence(Box::new(NonEmpty {
                                head: head.clone(),
                                tail: tail.to_vec(),
                            }))),
                            right: Box::new(javascript::Expression::Boolean(true)),
                        },
                    }
                };
                match join_expressions(pattern.conditions, |left, right| {
                    javascript::Expression::LogicalAnd {
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                }) {
                    None => assignments,
                    Some(conditions) => javascript::Expression::LogicalAnd {
                        left: Box::new(conditions),
                        right: Box::new(assignments),
                    },
                }
            }
            match join_expressions(in_place_assignments, |left, right| {
                javascript::Expression::LogicalOr {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }) {
                Some(condition) => TranspiledDestructurePattern {
                    conditions: vec![condition],
                    bindings: vec![], // Note that we dont return any bindings here, because they are already embedded into `condition`
                },
                None => TranspiledDestructurePattern {
                    conditions: vec![],
                    bindings: vec![],
                },
            }
        }
    }
}

fn join_transpiled_destructure_pattern(
    a: TranspiledDestructurePattern,
    b: TranspiledDestructurePattern,
) -> TranspiledDestructurePattern {
    TranspiledDestructurePattern {
        conditions: a
            .conditions
            .into_iter()
            .chain(b.conditions.into_iter())
            .collect(),
        bindings: a
            .bindings
            .into_iter()
            .chain(b.bindings.into_iter())
            .collect(),
    }
}
