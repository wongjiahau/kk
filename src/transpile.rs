use crate::typechecked_ast::*;

pub fn transpile_statements(statements: Vec<TypecheckedStatement>) -> String {
    let built_in_library = "
        const print_0 = (x) => console.log(x)
        "
    .to_string();
    let user_defined = statements
        .into_iter()
        .map(transpile_statement)
        .collect::<Vec<String>>();
    format!("{};{}", built_in_library, user_defined.join(";"))
}

pub fn transpile_statement(statement: TypecheckedStatement) -> String {
    match statement {
        TypecheckedStatement::Do { expression, .. } => {
            format!("({});", transpile_expression(expression))
        }
        TypecheckedStatement::Let { left, right, .. } => {
            format!(
                "const {} = {}",
                transpile_variable(left),
                transpile_expression(right)
            )
        }
    }
}

fn transpile_variable(variable: Variable) -> String {
    format!("{}_{}", variable.representation, variable.uid)
}

pub fn transpile_expression(expression: TypecheckedExpression) -> String {
    match expression {
        TypecheckedExpression::Null => "null".to_string(),
        TypecheckedExpression::Boolean(value) => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        TypecheckedExpression::String { representation } => representation,
        TypecheckedExpression::Number { representation } => representation,
        TypecheckedExpression::Variable(variable) => transpile_variable(variable),
        TypecheckedExpression::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => format!(
            "{{$:'{}',_:{}}}",
            constructor_name,
            match payload {
                Some(payload) => transpile_expression(*payload),
                None => "null".to_string(),
            }
        ),
        TypecheckedExpression::RecordAccess {
            expression,
            property_name,
        } => format!("({}).{}", transpile_expression(*expression), property_name),
        TypecheckedExpression::Record { key_value_pairs } => format!(
            "{{{}}}",
            key_value_pairs
                .into_iter()
                .map(|(key, value)| { format!("{}: {}", key, transpile_expression(value)) })
                .collect::<Vec<String>>()
                .join(",")
        ),
        TypecheckedExpression::Function(function) => {
            let TypecheckedFunction {
                first_branch,
                rest_branches,
            } = *function;
            let number_of_args = first_branch.rest_arguments.len() + 1;
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = vec![first_branch]
                .into_iter()
                .chain(rest_branches.into_iter())
                .map(transpile_function_branch)
                .collect::<Vec<String>>()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
        TypecheckedExpression::FunctionCall(function) => {
            let TypecheckedFunctionCall {
                function,
                first_argument,
                rest_arguments,
            } = *function;
            format!(
                "({})({})",
                transpile_expression(*function),
                vec![*first_argument]
                    .into_iter()
                    .chain(rest_arguments.into_iter())
                    .map(transpile_expression)
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
        TypecheckedExpression::Array { elements, .. } => format!(
            "[{}]",
            elements
                .into_iter()
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),
        // TypecheckedExpression::Let {
        //     left,
        //     right,
        //     false_branch: else_return,
        //     true_branch: return_value,
        //     ..
        // } => {
        //     let temp_placeholder = "$TEMP".to_string(); //TODO: get from symbol table to prevent name clashing
        //     let TranspiledDestructurePattern {
        //         bindings,
        //         conditions,
        //     } = transpile_function_destructure_pattern(*left, temp_placeholder.clone());
        //     let init = format!(
        //         "var {} = {}",
        //         temp_placeholder,
        //         transpile_expression(*right)
        //     );
        //     let return_now = match *return_value {
        //         TypecheckedExpression::Let { .. } => "",
        //         _ => "return ",
        //     };
        //     let return_value = transpile_expression(*return_value);
        //     if conditions.is_empty() {
        //         format!(
        //             "{};{};{}{}",
        //             init,
        //             bindings.join(";"),
        //             return_now,
        //             return_value
        //         )
        //     } else {
        //         let else_return = match else_return {
        //             Some(function) => format!(
        //                 "({})({})",
        //                 transpile_expression(TypecheckedExpression::Function(function)),
        //                 temp_placeholder
        //             ),
        //             None => temp_placeholder,
        //         };
        //         format!(
        //             "{};if({}){{{};{}{}}}else{{return {}}}",
        //             init,
        //             conditions.join(" && "),
        //             bindings.join(";"),
        //             return_now,
        //             return_value,
        //             else_return
        //         )
        //     }
        // }
    }
}

pub fn transpile_function_branch(function_branch: TypecheckedFunctionBranch) -> String {
    let transpiled_destructure_pattern = vec![*function_branch.first_argument.clone()]
        .into_iter()
        .chain(function_branch.rest_arguments.into_iter())
        .enumerate()
        .map(|(index, argument)| {
            transpile_function_destructure_pattern(argument, format!("_{}", index))
        })
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

    // let return_now = match *function_branch.body {
    //     TypecheckedExpression::Let { .. } => "",
    //     _ => "return ",
    // };
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
        "{}{{{};return {}}}",
        conditions,
        transpiled_destructure_pattern.bindings.join(";"),
        // return_now,
        body
    )
}

#[derive(Debug)]
pub struct TranspiledDestructurePattern {
    conditions: Vec<String>,
    bindings: Vec<String>,
}

pub fn transpile_function_destructure_pattern(
    destructure_pattern: TypecheckedDestructurePattern,
    from_expression: String,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        TypecheckedDestructurePattern::Number { representation }
        | TypecheckedDestructurePattern::String { representation } => {
            TranspiledDestructurePattern {
                conditions: vec![format!("{} === {}", from_expression, representation)],
                bindings: vec![],
            }
        }
        TypecheckedDestructurePattern::Boolean(value) => TranspiledDestructurePattern {
            conditions: vec![format!(
                "{} === {}",
                from_expression,
                if value { "true" } else { "false" }
            )],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Null => TranspiledDestructurePattern {
            conditions: vec![format!("{} === null", from_expression)],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Underscore => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Array { spread, .. } => match spread {
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
                    *spread.first_element,
                    format!("{}[0]", from_expression),
                ),
                transpile_function_destructure_pattern(
                    *spread.rest_elements,
                    format!("{}.slice(1)", from_expression),
                ),
            ]),
        },
        TypecheckedDestructurePattern::Variable(variable) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                transpile_variable(variable),
                from_expression
            )],
        },
        TypecheckedDestructurePattern::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => {
            let first = TranspiledDestructurePattern {
                conditions: vec![format!("{}.$==='{}'", from_expression, constructor_name)],
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
        TypecheckedDestructurePattern::Record {
            key_pattern_pairs, ..
        } => key_pattern_pairs.into_iter().fold(
            TranspiledDestructurePattern {
                bindings: vec![],
                conditions: vec![],
            },
            |result, (key, destructure_pattern)| {
                join_transpiled_destructure_pattern(
                    result,
                    transpile_function_destructure_pattern(
                        destructure_pattern,
                        format!("{}.{}", from_expression, key),
                    ),
                )
            },
        ),
        TypecheckedDestructurePattern::Tuple { .. } => {
            panic!("Compiler error, should not reach here")
        }
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
