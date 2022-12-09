use crate::inferred_ast::*;
use crate::{non_empty::NonEmpty, raw_ast::InfinitePatternKind, unify::UnifyProgramResult};

pub mod interpretable {
    use crate::{innate_function::InnateFunction, non_empty::NonEmpty};

    #[derive(Debug, Clone)]
    pub enum Statement {
        Assignment {
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
        Float(f64),
        Int(i64),
        StringConcat(Vec<Expression>),
        Array(Vec<Expression>),
        FunctionCall {
            function: Box<Expression>,
            argument: Box<Expression>,
        },
        ArrowFunction {
            parameter: Identifier,
            body: Vec<Statement>,
        },
        Object(Vec<ObjectKeyValue>),

        EnumConstructor {
            tag: String,
            payload: Option<Box<Expression>>,
        },
        HasTag {
            expression: Box<Expression>,
            tag: String,
        },
        GetEnumPayload(Box<Expression>),
        ObjectWithSpread {
            spread: Box<Expression>,
            key_values: Vec<ObjectKeyValue>,
        },
        MemberAccess {
            object: Box<Expression>,
            property: String,
        },
        TupleAccess {
            tuple: Box<Expression>,
            index: usize,
        },
        Assignment(Box<Assignment>),
        Tuple(Box<NonEmpty<Expression>>),
        InnateFunctionCall {
            function: InnateFunction,
            argument: Box<Expression>,
        },
    }

    #[derive(Debug, Clone)]
    pub struct ObjectKeyValue {
        pub key: Identifier,
        pub value: Box<Expression>,
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
                Statement::Assignment { assignment } => {
                    format!("{}", assignment.print(),)
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
                Expression::FunctionCall { function, argument } => {
                    format!("(({})({}))", function.print(), argument.print())
                }
                Expression::ArrowFunction { parameter, body } => {
                    format!(
                        "(({}) => {{{}}})",
                        parameter.print(),
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
                    format!("({})[{}]", object.print(), property)
                }
                Expression::Sequence(expressions) => {
                    format!(
                        "({})",
                        expressions.map(Printable::print).into_vector().join(", ")
                    )
                }
                Expression::Assignment(assignment) => {
                    format!("({})", assignment.print())
                }
                _ => todo!(),
            }
        }
    }
    impl Printable for ObjectKeyValue {
        fn print(self) -> String {
            format!("{}: {}", self.key.clone().print(), self.value.print())
        }
    }
    impl Printable for Identifier {
        fn print(self) -> String {
            self.0.replace(" ", "_").replace("+", "")
        }
    }
}

pub fn transpile_program(
    unify_project_result: UnifyProgramResult,
) -> Vec<interpretable::Statement> {
    unify_project_result
        .imported_modules
        .clone()
        .into_iter()
        .flat_map(|(_, imported_module)| imported_module.statements)
        .chain(unify_project_result.entrypoint.statements)
        .map(transpile_statement)
        .flatten()
        .collect::<Vec<_>>()
}

pub fn transpile_statements(statements: Vec<InferredStatement>) -> Vec<interpretable::Statement> {
    statements
        .into_iter()
        .flat_map(transpile_statement)
        .collect()
}

pub fn transpile_statement(statement: InferredStatement) -> Vec<interpretable::Statement> {
    match statement {
        InferredStatement::Expression(expression) => {
            vec![interpretable::Statement::Expression(transpile_expression(
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
                .map(|binding| interpretable::Statement::Assignment {
                    assignment: binding,
                })
                .collect()
        }
    }
}

fn transpile_identifier(identifier: Identifier) -> interpretable::Identifier {
    // Note that when we transpile identifier, we omit the module_uid and only use the index
    interpretable::Identifier(format!(
        "{}_{}",
        identifier.token.representation, identifier.uid.index
    ))
}

pub fn transpile_expression(expression: InferredExpression) -> interpretable::Expression {
    match expression {
        InferredExpression::String(string_literal) => {
            interpretable::Expression::String(string_literal.content)
        }
        InferredExpression::Character { representation } => {
            interpretable::Expression::String(representation)
        }
        InferredExpression::Float { representation } => {
            interpretable::Expression::Float(representation.parse().unwrap())
        }
        InferredExpression::Integer { representation } => {
            interpretable::Expression::Int(representation.parse().unwrap())
        }
        InferredExpression::InterpolatedString { sections } => {
            interpretable::Expression::StringConcat(
                sections
                    .into_iter()
                    .map(|section| match section {
                        InferredInterpolatedStringSection::String(string) => {
                            interpretable::Expression::String(string)
                        }
                        InferredInterpolatedStringSection::Expression(expression) => {
                            transpile_expression(expression.expression)
                        }
                    })
                    .collect::<Vec<interpretable::Expression>>(),
            )
        }
        InferredExpression::Variable(variable) => {
            interpretable::Expression::Variable(transpile_identifier(variable))
        }
        InferredExpression::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => interpretable::Expression::EnumConstructor {
            tag: constructor_name,
            payload: payload.map(|payload| Box::new(transpile_expression(*payload))),
        },
        InferredExpression::RecordAccess {
            expression,
            property_name,
        } => interpretable::Expression::MemberAccess {
            object: Box::new(transpile_expression(*expression)),
            property: transpile_property_name(property_name),
        },
        InferredExpression::Record { key_value_pairs } => interpretable::Expression::Object(
            key_value_pairs
                .into_iter()
                .map(|(key, value)| interpretable::ObjectKeyValue {
                    key: interpretable::Identifier(transpile_property_name(key)),
                    value: Box::new(transpile_expression(value)),
                })
                .collect::<Vec<interpretable::ObjectKeyValue>>(),
        ),
        InferredExpression::BranchedFunction(function) => {
            let InferredBranchedFunction { branches } = *function;
            let parameter = build_function_argument(0);
            let body = branches
                .map(transpile_function_branch)
                .into_vector()
                .into_iter()
                .flatten()
                .collect();
            interpretable::Expression::ArrowFunction { parameter, body }
        }
        InferredExpression::FunctionCall(function) => {
            let InferredFunctionCall { function, argument } = *function;
            let function = transpile_expression(*function);
            let argument = transpile_expression(*argument);

            interpretable::Expression::FunctionCall {
                function: Box::new(function),
                argument: Box::new(argument),
            }
        }
        InferredExpression::Array { elements, .. } => interpretable::Expression::Array(
            elements.into_iter().map(transpile_expression).collect(),
        ),
        InferredExpression::RecordUpdate {
            expression,
            updates,
        } => {
            let temporary_identifier = interpretable::Identifier("$".to_string());
            let temporary_variable =
                interpretable::Expression::Variable(temporary_identifier.clone());
            let updates = updates
                .into_iter()
                .map(|update| match update {
                    InferredRecordUpdate::ValueUpdate {
                        property_name,
                        new_value,
                    } => interpretable::ObjectKeyValue {
                        key: interpretable::Identifier(transpile_property_name(property_name)),
                        value: Box::new(transpile_expression(new_value)),
                    },
                    InferredRecordUpdate::FunctionalUpdate {
                        property_name,
                        function,
                    } => {
                        let property_name = transpile_property_name(property_name);
                        interpretable::ObjectKeyValue {
                            key: interpretable::Identifier(property_name.clone()),
                            value: Box::new(interpretable::Expression::FunctionCall {
                                function: Box::new(transpile_expression(function)),
                                argument: Box::new(interpretable::Expression::MemberAccess {
                                    object: Box::new(temporary_variable.clone()),
                                    property: property_name,
                                }),
                            }),
                        }
                    }
                })
                .collect::<Vec<interpretable::ObjectKeyValue>>();

            interpretable::Expression::FunctionCall {
                function: Box::new(interpretable::Expression::ArrowFunction {
                    parameter: temporary_identifier,
                    body: vec![interpretable::Statement::Return(
                        interpretable::Expression::ObjectWithSpread {
                            spread: Box::new(temporary_variable),
                            key_values: updates,
                        },
                    )],
                }),
                argument: Box::new(transpile_expression(*expression)),
            }
        }
        InferredExpression::Block {
            statements,
            return_value,
        } => interpretable::Expression::FunctionCall {
            argument: Box::new(interpretable::Expression::Null),
            function: Box::new(interpretable::Expression::ArrowFunction {
                parameter: interpretable::Identifier("temp".to_string()),
                body: transpile_statements(statements)
                    .into_iter()
                    .chain(vec![interpretable::Statement::Return(
                        transpile_expression(*return_value),
                    )])
                    .collect(),
            }),
        },
        InferredExpression::Unit => interpretable::Expression::Null,
        InferredExpression::Keyword(keyword) => {
            interpretable::Expression::String(keyword.representation)
        }
        InferredExpression::Tuple(elements) => {
            interpretable::Expression::Tuple(Box::new(elements.map(transpile_expression)))
        }
        InferredExpression::InnateFunctionCall { function, argument } => {
            interpretable::Expression::InnateFunctionCall {
                function,
                argument: Box::new(transpile_expression(*argument)),
            }
        }
    }
}

fn transpile_property_name(property_name: PropertyName) -> String {
    format!("{}", property_name.0.representation)
}

pub fn build_function_argument(index: usize) -> interpretable::Identifier {
    interpretable::Identifier(format!("_{}", index))
}

pub fn transpile_function_branch(
    function_branch: InferredFunctionBranch,
) -> Vec<interpretable::Statement> {
    let transpiled_destructure_pattern = transpile_destructure_pattern(
        function_branch.parameter.kind,
        interpretable::Expression::Variable(build_function_argument(0)),
    );

    let body = transpiled_destructure_pattern
        .bindings
        .into_iter()
        .map(|binding| interpretable::Statement::Assignment {
            assignment: binding,
        })
        .chain(vec![interpretable::Statement::Return(
            transpile_expression(function_branch.body.expression),
        )])
        .collect::<Vec<interpretable::Statement>>();
    match join_expressions(transpiled_destructure_pattern.conditions, |left, right| {
        interpretable::Expression::LogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
        }
    }) {
        None => body,
        Some(condition) => {
            vec![interpretable::Statement::If {
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
    expressions: Vec<interpretable::Expression>,
    join: Join,
) -> Option<interpretable::Expression>
where
    Join: Fn(interpretable::Expression, interpretable::Expression) -> interpretable::Expression,
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
    conditions: Vec<interpretable::Expression>,
    bindings: Vec<interpretable::Assignment>,
}

pub fn transpile_destructure_pattern(
    destructure_pattern: InferredDestructurePatternKind,
    from_expression: interpretable::Expression,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        InferredDestructurePatternKind::Infinite { token, kind } => TranspiledDestructurePattern {
            conditions: vec![interpretable::Expression::Equals {
                left: Box::new(from_expression),
                right: match kind {
                    InfinitePatternKind::String | InfinitePatternKind::Character => {
                        Box::new(interpretable::Expression::String(token.representation))
                    }
                    InfinitePatternKind::Integer => Box::new(interpretable::Expression::Int(
                        token.representation.parse().unwrap(),
                    )),
                },
            }],
            bindings: vec![],
        },
        InferredDestructurePatternKind::Boolean { value, .. } => TranspiledDestructurePattern {
            conditions: vec![interpretable::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(interpretable::Expression::Boolean(value)),
            }],
            bindings: vec![],
        },
        InferredDestructurePatternKind::Unit { .. } => TranspiledDestructurePattern {
            conditions: vec![interpretable::Expression::Equals {
                left: Box::new(from_expression),
                right: Box::new(interpretable::Expression::Null),
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
            bindings: vec![interpretable::Assignment {
                left: transpile_identifier(*variable),
                right: from_expression,
            }],
        },
        InferredDestructurePatternKind::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => {
            let match_enum_tagname = interpretable::Expression::HasTag {
                expression: Box::new(from_expression.clone()),
                tag: constructor_name.representation,
            };
            let first = TranspiledDestructurePattern {
                conditions: vec![match_enum_tagname],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(payload) => Some(transpile_destructure_pattern(
                    payload.kind,
                    interpretable::Expression::GetEnumPayload(Box::new(from_expression)),
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
                        interpretable::Expression::MemberAccess {
                            object: Box::new(from_expression.clone()),
                            property: transpile_property_name(key),
                        },
                    ),
                )
            },
        ),
        InferredDestructurePatternKind::Tuple { patterns } => {
            patterns.into_vector().into_iter().enumerate().fold(
                TranspiledDestructurePattern {
                    conditions: vec![],
                    bindings: vec![],
                },
                |result, (index, pattern)| {
                    join_transpiled_destructure_pattern(
                        result,
                        transpile_destructure_pattern(
                            pattern.kind,
                            interpretable::Expression::TupleAccess {
                                tuple: Box::new(from_expression.clone()),
                                index,
                            },
                        ),
                    )
                },
            )
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
            ) -> interpretable::Expression {
                let assignments = {
                    match pattern
                        .bindings
                        .into_iter()
                        .map(|binding| interpretable::Expression::Assignment(Box::new(binding)))
                        .collect::<Vec<interpretable::Expression>>()
                        .split_first()
                    {
                        None => interpretable::Expression::Boolean(true),
                        Some((head, tail)) => interpretable::Expression::LogicalOr {
                            left: Box::new(interpretable::Expression::Sequence(Box::new(
                                NonEmpty {
                                    head: head.clone(),
                                    tail: tail.to_vec(),
                                },
                            ))),
                            right: Box::new(interpretable::Expression::Boolean(true)),
                        },
                    }
                };
                match join_expressions(pattern.conditions, |left, right| {
                    interpretable::Expression::LogicalAnd {
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                }) {
                    None => assignments,
                    Some(conditions) => interpretable::Expression::LogicalAnd {
                        left: Box::new(conditions),
                        right: Box::new(assignments),
                    },
                }
            }
            match join_expressions(in_place_assignments, |left, right| {
                interpretable::Expression::LogicalOr {
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
