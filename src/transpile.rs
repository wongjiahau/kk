use crate::inferred_ast::*;
use crate::{non_empty::NonEmpty, raw_ast::InfinitePatternKind, unify::UnifyProgramResult};

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
        pub left: Identifier,
        pub right: Expression,
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
            .join(";\n\n")
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
            self.0.replace(" ", "_").replace("+", "")
        }
    }
}

const ENUM_TAG_NAME: &str = "$";
const ENUM_PAYLOAD_NAME: &str = "_";

pub fn transpile_program(unify_project_result: UnifyProgramResult) -> String {
    // TODO: move this to a file
    let built_in_library =
        javascript::Statement::Expression(javascript::Expression::UnsafeJavascriptCode(
            " const print_0 = (x) => console.log(x); ".to_string(),
        ));

    let statements = vec![built_in_library]
        .into_iter()
        .chain(
            unify_project_result
                .imported_modules
                .clone()
                .into_iter()
                .flat_map(|(_, imported_module)| imported_module.statements)
                .chain(unify_project_result.entrypoint.statements)
                .map(transpile_statement)
                .flatten(),
        )
        .collect::<Vec<_>>();

    javascript::print_statements(statements)
}

pub fn transpile_statements(statements: Vec<InferredStatement>) -> Vec<javascript::Statement> {
    statements
        .into_iter()
        .flat_map(transpile_statement)
        .collect()
}

pub fn transpile_statement(statement: InferredStatement) -> Vec<javascript::Statement> {
    match statement {
        InferredStatement::Expression(expression) => {
            vec![javascript::Statement::Expression(transpile_expression(
                expression,
            ))]
        }
        InferredStatement::Let { left, right, .. } => {
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
    }
}

fn transpile_identifier(identifier: Identifier) -> javascript::Identifier {
    // Note that when we transpile identifier, we omit the module_uid and only use the index
    javascript::Identifier(format!(
        "{}_{}",
        identifier.token.representation, identifier.uid.index
    ))
}

pub fn transpile_expression(expression: InferredExpression) -> javascript::Expression {
    match expression {
        InferredExpression::String(string_literal) => {
            javascript::Expression::String(string_literal.content)
        }
        InferredExpression::Character { representation } => {
            javascript::Expression::String(representation)
        }
        InferredExpression::Float { representation }
        | InferredExpression::Integer { representation } => {
            javascript::Expression::Number { representation }
        }
        InferredExpression::InterpolatedString { sections } => {
            javascript::Expression::StringConcat(
                sections
                    .into_iter()
                    .map(|section| match section {
                        InferredInterpolatedStringSection::String(string) => {
                            javascript::Expression::String(string)
                        }
                        InferredInterpolatedStringSection::Expression(expression) => {
                            transpile_expression(expression.expression)
                        }
                    })
                    .collect::<Vec<javascript::Expression>>(),
            )
        }
        InferredExpression::Variable(variable) => {
            javascript::Expression::Variable(transpile_identifier(variable))
        }
        InferredExpression::EnumConstructor {
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
        InferredExpression::RecordAccess {
            expression,
            property_name,
        } => javascript::Expression::MemberAccess {
            object: Box::new(transpile_expression(*expression)),
            property: Box::new(javascript::Expression::String(transpile_property_name(
                property_name,
            ))),
        },
        InferredExpression::Record { key_value_pairs } => javascript::Expression::Object(
            key_value_pairs
                .into_iter()
                .map(|(key, value)| javascript::ObjectKeyValue {
                    key: javascript::Identifier(transpile_property_name(key)),
                    value: Some(Box::new(transpile_expression(value))),
                })
                .collect::<Vec<javascript::ObjectKeyValue>>(),
        ),
        InferredExpression::BranchedFunction(function) => {
            let InferredBranchedFunction { branches } = *function;
            let parameters = vec![build_function_argument(0)];
            let body = branches
                .map(transpile_function_branch)
                .into_vector()
                .into_iter()
                .flatten()
                .collect();
            javascript::Expression::ArrowFunction { parameters, body }
        }
        InferredExpression::FunctionCall(function) => {
            let InferredFunctionCall { function, argument } = *function;
            let function = transpile_expression(*function);
            let arguments = vec![transpile_expression(*argument)];

            javascript::Expression::FunctionCall {
                function: Box::new(function),
                arguments,
            }
        }
        InferredExpression::Array { elements, .. } => {
            javascript::Expression::Array(elements.into_iter().map(transpile_expression).collect())
        }
        InferredExpression::RecordUpdate {
            expression,
            updates,
        } => {
            let temporary_identifier = javascript::Identifier("$".to_string());
            let temporary_variable = javascript::Expression::Variable(temporary_identifier.clone());
            let updates = updates
                .into_iter()
                .map(|update| match update {
                    InferredRecordUpdate::ValueUpdate {
                        property_name,
                        new_value,
                    } => javascript::ObjectKeyValue {
                        key: javascript::Identifier(transpile_property_name(property_name)),
                        value: Some(Box::new(transpile_expression(new_value))),
                    },
                    InferredRecordUpdate::FunctionalUpdate {
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
        InferredExpression::Block {
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
        InferredExpression::Unit => javascript::Expression::Null,
        InferredExpression::Keyword(keyword) => {
            javascript::Expression::String(keyword.representation)
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
    function_branch: InferredFunctionBranch,
) -> Vec<javascript::Statement> {
    let transpiled_destructure_pattern = transpile_destructure_pattern(
        function_branch.parameter.kind,
        javascript::Expression::Variable(build_function_argument(0)),
    );

    let body = transpiled_destructure_pattern
        .bindings
        .into_iter()
        .map(|binding| javascript::Statement::Assignment {
            is_declaration: true,
            assignment: binding,
        })
        .chain(vec![javascript::Statement::Return(transpile_expression(
            function_branch.body.expression,
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

pub fn transpile_destructure_pattern(
    destructure_pattern: InferredDestructurePatternKind,
    from_expression: javascript::Expression,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        InferredDestructurePatternKind::Infinite { token, kind } => TranspiledDestructurePattern {
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
        },
        InferredDestructurePatternKind::Boolean { value, .. } => TranspiledDestructurePattern {
            conditions: vec![javascript::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(javascript::Expression::Boolean(value)),
            }],
            bindings: vec![],
        },
        InferredDestructurePatternKind::Unit { .. } => TranspiledDestructurePattern {
            conditions: vec![javascript::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(javascript::Expression::Null),
            }],
            bindings: vec![],
        },
        InferredDestructurePatternKind::Underscore { .. } => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        InferredDestructurePatternKind::Array { .. } => {
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
        InferredDestructurePatternKind::Identifier(variable) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![javascript::Assignment {
                left: transpile_identifier(*variable),
                right: from_expression,
            }],
        },
        InferredDestructurePatternKind::EnumConstructor {
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
                    payload.kind,
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
        InferredDestructurePatternKind::Record {
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
        InferredDestructurePatternKind::Tuple { .. } => {
            panic!("Compiler error, should not reach here")
        }

        // The current transpilation for OR patterns is a bit hacky as we are abusing assignment expression
        // in Javascript.
        // See the description of `into_in_place_assignment` for more information.
        InferredDestructurePatternKind::Or { patterns } => {
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
