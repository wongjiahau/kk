use crate::ast::*;

pub fn transpile_statements(statements: Vec<Statement>) -> String {
    let built_in_library = "
        const print = (x) => console.log(x)
        "
    .to_string();
    let user_defined = statements
        .into_iter()
        .map(transpile_statement)
        .collect::<Vec<String>>();
    format!("{};{}", built_in_library, user_defined.join(";"))
}

pub fn transpile_statement(statement: Statement) -> String {
    match statement {
        Statement::Type { .. } | Statement::Enum { .. } => "".to_string(),
        Statement::Do { expression, .. } => {
            format!("({});", transpile_expression(expression))
        }
        Statement::Let { left, right, .. } => match right {
            Expression::Let { .. } => {
                format!(
                    "const {} = (()=>{{{}}})()",
                    left.representation,
                    transpile_expression(right)
                )
            }
            _ => {
                format!(
                    "const {} = {}",
                    left.representation,
                    transpile_expression(right)
                )
            }
        },
    }
}

pub fn transpile_expression(expression: Expression) -> String {
    match expression {
        Expression::Null(_) => "null".to_string(),
        Expression::Boolean { value, .. } => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Expression::String(s) => s.representation,
        Expression::Number(n) => n.representation,
        Expression::Variable(v) => v.representation,
        Expression::EnumConstructor {
            scoped_name,
            payload,
            ..
        } => format!(
            "{{$:'{}',_:{}}}",
            scoped_name.name.representation,
            match payload {
                Some(payload) => transpile_expression(*payload),
                None => "null".to_string(),
            }
        ),
        Expression::RecordAccess {
            expression,
            property_name,
        } => format!(
            "({}).{}",
            transpile_expression(*expression),
            property_name.representation
        ),
        Expression::Record {
            spread,
            key_value_pairs,
            ..
        } => format!(
            "{{{}{}}}",
            match spread {
                None => "".to_string(),
                Some(expression) => format!("...({}),", transpile_expression(*expression)),
            },
            key_value_pairs
                .into_iter()
                .map(|RecordKeyValue { key, value, .. }| {
                    format!("{}: {}", key.representation, transpile_expression(value))
                })
                .collect::<Vec<String>>()
                .join(",")
        ),
        Expression::Function(function) => {
            let Function {
                first_branch,
                branches,
            } = *function;
            let number_of_args = first_branch.rest_arguments().len() + 1;
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = vec![first_branch]
                .into_iter()
                .chain(branches.into_iter())
                .map(transpile_function_branch)
                .collect::<Vec<String>>()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
        Expression::FunctionCall(function) => {
            let FunctionCall {
                function_name,
                first_argument,
                rest_arguments,
                type_arguments: _,
            } = *function;
            format!(
                "{}({})",
                function_name.representation,
                vec![*first_argument]
                    .into_iter()
                    .chain(match rest_arguments {
                        None => vec![].into_iter(),

                        Some(FunctionCallRestArguments { arguments, .. }) => arguments.into_iter(),
                    })
                    .map(transpile_expression)
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
        Expression::Array { elements, .. } => format!(
            "[{}]",
            elements
                .into_iter()
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),
        Expression::Let {
            left,
            right,
            false_branch: else_return,
            true_branch: return_value,
            ..
        } => {
            let temp_placeholder = "$TEMP".to_string(); //TODO: get from symbol table to prevent name clashing
            let TranspiledDestructurePattern {
                bindings,
                conditions,
            } = transpile_function_destructure_pattern(*left, temp_placeholder.clone());
            let init = format!(
                "var {} = {}",
                temp_placeholder,
                transpile_expression(*right)
            );
            let return_now = match *return_value {
                Expression::Let { .. } => "",
                _ => "return ",
            };
            let return_value = transpile_expression(*return_value);
            if conditions.is_empty() {
                format!(
                    "{};{};{}{}",
                    init,
                    bindings.join(";"),
                    return_now,
                    return_value
                )
            } else {
                let else_return = match else_return {
                    Some(function) => format!(
                        "({})({})",
                        transpile_expression(Expression::Function(function)),
                        temp_placeholder
                    ),
                    None => temp_placeholder,
                };
                format!(
                    "{};if({}){{{};{}{}}}else{{return {}}}",
                    init,
                    conditions.join(" && "),
                    bindings.join(";"),
                    return_now,
                    return_value,
                    else_return
                )
            }
        }
    }
}

pub fn transpile_function_branch(function_branch: FunctionBranch) -> String {
    let transpiled_destructure_pattern = vec![*function_branch.first_argument.clone()]
        .into_iter()
        .chain(function_branch.rest_arguments().into_iter())
        .enumerate()
        .map(|(index, argument)| {
            transpile_function_destructure_pattern(
                argument.destructure_pattern,
                format!("_{}", index),
            )
        })
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

    let return_now = match *function_branch.body {
        Expression::Let { .. } => "",
        _ => "return ",
    };
    let body = transpile_expression(*function_branch.body);
    let conditions = {
        if transpiled_destructure_pattern.conditions.is_empty() {
            "".to_string()
        } else {
            format!(
                "if({})",
                transpiled_destructure_pattern.conditions.join(" && "),
            )
        }
    };
    format!(
        "{}{{{};{}{}}}",
        conditions,
        transpiled_destructure_pattern.bindings.join(";"),
        return_now,
        body
    )
}

#[derive(Debug)]
pub struct TranspiledDestructurePattern {
    conditions: Vec<String>,
    bindings: Vec<String>,
}

pub fn transpile_function_destructure_pattern(
    destructure_pattern: DestructurePattern,
    from_expression: String,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        DestructurePattern::Number(token)
        | DestructurePattern::String(token)
        | DestructurePattern::Boolean { token, .. }
        | DestructurePattern::Null(token) => TranspiledDestructurePattern {
            conditions: vec![format!("{} === {}", token.representation, from_expression)],
            bindings: vec![],
        },
        DestructurePattern::Underscore(_) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        DestructurePattern::Array { spread, .. } => match spread {
            None => TranspiledDestructurePattern {
                conditions: vec![format!("{}.length === 0", from_expression)],
                bindings: vec![],
            },
            Some(spread) => join_transpiled_destructure_patterns(vec![
                TranspiledDestructurePattern {
                    conditions: vec![format!("{}.length > 0", from_expression)],
                    bindings: vec![],
                },
                transpile_function_destructure_pattern(
                    *spread.left,
                    format!("{}[0]", from_expression),
                ),
                transpile_function_destructure_pattern(
                    *spread.right,
                    format!("{}.slice(1)", from_expression),
                ),
            ]),
        },
        DestructurePattern::Identifier(identifier) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                identifier.representation, from_expression
            )],
        },
        DestructurePattern::EnumConstructor {
            scoped_name,
            payload,
            ..
        } => {
            let first = TranspiledDestructurePattern {
                conditions: vec![format!(
                    "{}.$==='{}'",
                    from_expression, scoped_name.name.representation
                )],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(payload) => Some(transpile_function_destructure_pattern(
                    *payload,
                    format!("{}._", from_expression),
                )),
            };
            match rest {
                None => first,
                Some(rest) => join_transpiled_destructure_pattern(first, rest),
            }
        }
        DestructurePattern::Record {
            key_value_pairs, ..
        } => key_value_pairs.into_iter().fold(
            TranspiledDestructurePattern {
                bindings: vec![],
                conditions: vec![],
            },
            |result, DestructuredRecordKeyValue { key, as_value, .. }| {
                join_transpiled_destructure_pattern(
                    result,
                    match as_value {
                        None => TranspiledDestructurePattern {
                            bindings: vec![format!(
                                "var {} = {}.{}",
                                key.representation, from_expression, key.representation
                            )],
                            conditions: vec![],
                        },
                        Some(destructure_pattern) => transpile_function_destructure_pattern(
                            destructure_pattern,
                            format!("{}.{}", from_expression, key.representation),
                        ),
                    },
                )
            },
        ),
        DestructurePattern::Tuple { .. } => panic!("Compiler error, should not reach here"),
    }
}

fn join_transpiled_destructure_patterns(
    xs: Vec<TranspiledDestructurePattern>,
) -> TranspiledDestructurePattern {
    xs.into_iter().fold(
        TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        join_transpiled_destructure_pattern,
    )
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
