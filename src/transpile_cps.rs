use crate::typechecked_ast::*;

pub fn transpile_statements(statements: Vec<InferredStatement>) -> String {
    let built_in_library = "
        const kk_apply_functional_updates = (record, functional_updates) =>
            functional_updates.reduce((record, {key, func}, index) => {
                return Promise.all([record, func]).then(([record, func]) => {
                    return func(record[key]).then(result => ({
                        ...record,
                        [key]: result
                    }))
                })
            }, record)
        const print_0 = (x) => { console.log(x) }
        const read_file_1 = (filename) => new Promise(resolve => require('fs').readFile(filename, (err, content) => {
            if(err) {
                resolve({$: 'Error', _: err.toString()})
            }
            else resolve({$: 'Ok', _: content.toString()})
        }))
        "
    .to_string();
    let user_defined = statements
        .into_iter()
        .map(transpile_statement)
        .collect::<Vec<String>>();
    format!(
        "(async () => {{{};\n{}}})()",
        built_in_library,
        user_defined.join(";\n\n")
    )
}

pub fn transpile_statement(statement: InferredStatement) -> String {
    match statement {
        InferredStatement::Do { expression, .. } => {
            format!("await {};", transpile_expression(expression))
        }
        InferredStatement::Let { left, right, .. } => {
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

pub fn transpile_expression(expression: InferredExpression) -> String {
    match expression {
        InferredExpression::Null => ("null".to_string()),
        InferredExpression::Boolean(value) => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        InferredExpression::String { representation }
        | InferredExpression::Character { representation }
        | InferredExpression::Float { representation }
        | InferredExpression::Integer { representation } => representation,
        InferredExpression::Variable(variable) => transpile_variable(variable),
        InferredExpression::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => format!(
            "Promise.all([{}]).then(([_]) => ({{$:'{}',_}}))",
            match payload {
                Some(payload) => transpile_expression(*payload),
                None => "null".to_string(),
            },
            constructor_name,
        ),
        InferredExpression::RecordAccess {
            expression,
            property_name,
        } => format!(
            "Promise.all([{}]).then(([{{{property_name}}}]) => {property_name})",
            transpile_expression(*expression),
            property_name = format!("${}", property_name),
        ),
        InferredExpression::RecordUpdate {
            expression,
            updates,
        } => {
            type KeyValue = Vec<(String, (InferredExpression, bool))>;
            let (value_updates, functional_updates): (KeyValue, KeyValue) = updates
                .into_iter()
                .map(|update| match update {
                    InferredRecordUpdate::ValueUpdate {
                        property_name,
                        new_value,
                    } => (property_name, (new_value, true)),
                    InferredRecordUpdate::FunctionalUpdate {
                        property_name,
                        function,
                    } => (property_name, (function, false)),
                })
                .partition(|(_, (_, is_value_update))| *is_value_update);

            let (value_updates_keys, value_updates_values): (
                Vec<String>,
                Vec<(InferredExpression, bool)>,
            ) = value_updates.into_iter().unzip();
            format!(
                "Promise.all([{},{}]).then(([record,{value_update_keys}]) =>
                    kk_apply_functional_updates(
                        {{...record, {value_update_keys}}},
                        [{functional_updates}]
                    )
                )
                ",
                transpile_expression(*expression),
                value_updates_values
                    .into_iter()
                    .map(|(expression, _)| transpile_expression(expression))
                    .collect::<Vec<String>>()
                    .join(","),
                value_update_keys = value_updates_keys
                    .into_iter()
                    .map(|key| format!("${}", key))
                    .collect::<Vec<String>>()
                    .join(","),
                functional_updates = functional_updates
                    .into_iter()
                    .map(|(key, (function, _))| {
                        format!(
                            "{{key: '${}', func: {}}}",
                            key,
                            transpile_expression(function)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
        InferredExpression::Record { key_value_pairs } => {
            let (keys, values): (Vec<String>, Vec<InferredExpression>) =
                key_value_pairs.into_iter().unzip();
            let keys = keys
                .into_iter()
                .map(|key| format!("${}", key))
                .collect::<Vec<String>>()
                .join(",");
            format!(
                "Promise.all([{}]).then(([{keys}]) => ({{{keys}}}))",
                values
                    .into_iter()
                    .map(|value| { transpile_expression(value) })
                    .collect::<Vec<String>>()
                    .join(","),
                keys = keys
            )
        }
        InferredExpression::Function(function) => {
            let InferredFunction { branches } = *function;
            let number_of_args = branches.first().parameters.len();
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = branches
                .map(transpile_function_branch)
                .into_vector()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
        InferredExpression::FunctionCall(function) => {
            let InferredFunctionCall {
                function,
                first_argument,
                rest_arguments,
            } = *function;
            format!(
                "Promise.all([{}]).then(args => ({})(...args))",
                vec![*first_argument]
                    .into_iter()
                    .chain(rest_arguments.into_iter())
                    .map(transpile_expression)
                    .collect::<Vec<String>>()
                    .join(","),
                transpile_expression(*function),
            )
        }
        InferredExpression::Array { elements, .. } => format!(
            "Promise.all([{}])",
            elements
                .into_iter()
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),
        InferredExpression::Javascript { code } => code,
    }
}

pub fn transpile_function_branch(function_branch: InferredFunctionBranch) -> String {
    let transpiled_destructure_pattern = function_branch
        .parameters
        .into_vector()
        .into_iter()
        .enumerate()
        .map(|(index, pattern)| transpile_destructure_pattern(pattern, format!("_{}", index)))
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

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

pub fn transpile_destructure_pattern(
    destructure_pattern: InferredDestructurePattern,
    from_expression: String,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        InferredDestructurePattern::Integer { representation }
        | InferredDestructurePattern::String { representation }
        | InferredDestructurePattern::Character { representation } => {
            TranspiledDestructurePattern {
                conditions: vec![format!("{} === {}", from_expression, representation)],
                bindings: vec![],
            }
        }
        InferredDestructurePattern::Boolean(value) => TranspiledDestructurePattern {
            conditions: vec![format!(
                "{} === {}",
                from_expression,
                if value { "true" } else { "false" }
            )],
            bindings: vec![],
        },
        InferredDestructurePattern::Null => TranspiledDestructurePattern {
            conditions: vec![format!("{} === null", from_expression)],
            bindings: vec![],
        },
        InferredDestructurePattern::Underscore => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        InferredDestructurePattern::Array { spread, .. } => match spread {
            None => TranspiledDestructurePattern {
                conditions: vec![format!("{}.length === 0", from_expression)],
                bindings: vec![],
            },
            Some(spread) => join_transpiled_destructure_patterns(vec![
                TranspiledDestructurePattern {
                    conditions: vec![format!("{}.length > 0", from_expression)],
                    bindings: vec![],
                },
                transpile_destructure_pattern(
                    *spread.first_element,
                    format!("{}[0]", from_expression),
                ),
                transpile_destructure_pattern(
                    *spread.rest_elements,
                    format!("{}.slice(1)", from_expression),
                ),
            ]),
        },
        InferredDestructurePattern::Variable(variable) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                transpile_variable(variable),
                from_expression
            )],
        },
        InferredDestructurePattern::EnumConstructor {
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
                Some(payload) => Some(transpile_destructure_pattern(
                    *payload,
                    format!("{}._", from_expression),
                )),
            };
            match rest {
                None => first,
                Some(rest) => join_transpiled_destructure_pattern(first, rest),
            }
        }
        InferredDestructurePattern::Record {
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
                        destructure_pattern,
                        format!("{}.${}", from_expression, key),
                    ),
                )
            },
        ),
        InferredDestructurePattern::Tuple { .. } => {
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
