use crate::ast::*;

pub fn transpile_statements(statements: Vec<Statement>) -> String {
    statements.into_iter().map(transpile_statement).collect()
}

pub fn transpile_statement(statement: Statement) -> String {
    match statement {
        Statement::TypeAlias { .. } => "".to_string(),
        Statement::Let { left, right, .. } => match right {
            Expression::Let { .. } => {
                return format!(
                    "const {} = (()=>{{{}}})()",
                    left.representation,
                    transpile_expression(right)
                )
            }
            _ => {
                return format!(
                    "const {} = {}",
                    left.representation,
                    transpile_expression(right)
                )
            }
        },
    }
}

fn transpile_tag(tag: String) -> String {
    tag.as_str()[1..].to_string()
}

pub fn transpile_expression(expression: Expression) -> String {
    match expression {
        Expression::String(s) => s.representation,
        Expression::Number(n) => n.representation,
        Expression::Variable(v) => v.representation,
        Expression::Tag { token, payload } => match payload {
            Some(payload) => format!(
                "{{$:'{}',_:{}}}",
                transpile_tag(token.representation),
                transpile_expression(payload.value)
            ),
            None => format!("{{$:'{}'}}", transpile_tag(token.representation)),
        },
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
            let number_of_args = first_branch.arguments.len();
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = vec![first_branch]
                .into_iter()
                .chain(branches.into_iter())
                .map(transpile_function_branch)
                .collect::<Vec<String>>()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
        Expression::FunctionCall(FunctionCall {
            function,
            arguments,
        }) => format!(
            "({})({})",
            transpile_expression(*function),
            arguments
                .into_iter()
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),
        Expression::Array(values) => format!(
            "[{}]",
            values
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
            } = transpile_function_destructure_pattern(left, temp_placeholder.clone());
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
                    Some(expression) => format!(
                        "({})({})",
                        transpile_expression(*expression),
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
    let transpiled_destructure_pattern = function_branch
        .arguments
        .into_iter()
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
        DestructurePattern::String(token) => TranspiledDestructurePattern {
            conditions: vec![format!("{} === {}", token.representation, from_expression)],
            bindings: vec![],
        },
        DestructurePattern::Number(token) => TranspiledDestructurePattern {
            conditions: vec![format!("{} === {}", token.representation, from_expression)],
            bindings: vec![],
        },
        DestructurePattern::Underscore(_) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        DestructurePattern::Identifier(identifier) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                identifier.representation, from_expression
            )],
        },
        DestructurePattern::Tag { token, payload } => {
            let first = TranspiledDestructurePattern {
                conditions: vec![format!(
                    "{}.$==='{}'",
                    from_expression,
                    transpile_tag(token.representation)
                )],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(payload) => Some(transpile_function_destructure_pattern(
                    payload.destructure_pattern,
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
