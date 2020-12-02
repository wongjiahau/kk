use crate::ast::*;

use crate::environment::*;

pub struct Program {
    pub statements: Vec<Statement>,
    pub source: Source,
}

pub fn unify_program(program: Program) -> Result<(), UnifyError> {
    // 1. TODO: Populate environment with imported symbols
    let mut environment: Environment = Environment::new_root(program.source);

    // 2. Type check this program
    for statement in program.statements {
        let _ = unify_statement(&mut environment, statement)?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum UnifyError {
    RecordKeyTypeMismatch {
        location: Location,
        key: String,
        expected_key_type: Type,
        actual_key_type: Type,
        expected_record_type: Vec<(String, Type)>,
        actual_record_type: Vec<(String, Type)>,
    },
    RecordExtraneousKeys {
        extraneous_keys: Vec<String>,
        location: Location,
        expected_type: Type,
    },
    RecordMissingKeys {
        missing_keys: Vec<String>,
        location: Location,
        expected_type: Type,
    },
    CannotInvokeNonFunction {
        location: Location,
        actual_type: Type,
    },
    DuplicatedIdentifier {
        name: String,
        first_declared_at: Declaration,
        then_declared_at: Declaration,
    },
    InvalidFunctionArgumentLength {
        location: Location,
        expected_length: usize,
        actual_length: usize,
    },
    UnknownTypeSymbol {
        location: Location,
    },
    UnknownValueSymbol {
        location: Location,
    },
    TypeMismatch {
        location: Location,
        expected_type: Type,
        actual_type: Type,
    },
    CannotDestructure {
        destructure_pattern: DestructurePattern,
        expression_type: Type,
    },
}

pub fn unify_statement(
    environment: &mut Environment,
    statement: Statement,
) -> Result<(), UnifyError> {
    match statement {
        Statement::Let {
            left,
            right,
            type_annotation,
        } => {
            let type_annotation_type =
                optional_type_annotation_to_type(environment, &type_annotation)?;
            let right_type = infer_expression_type(environment, &right.value)?;

            // 1. Check if right matches type annotation
            let right_type = match type_annotation_type {
                Some(type_annotation_type) => {
                    let location = get_expression_location(environment, &right.value);
                    let right_type =
                        unify_type(environment, type_annotation_type, right_type, location)?;
                    right_type
                }
                None => right_type,
            };

            // 2. Unify left with right_type and populate parent environment with bindings found in left
            unify_destructure_pattern(environment, &left, &right_type)?;
            Ok(())
        }
        _ => panic!(),
    }
}

pub fn get_expression_location(
    environment: &Environment,
    expression_value: &ExpressionValue,
) -> Location {
    let position = get_expression_position(expression_value);
    Location {
        source: environment.source.clone(),
        position,
    }
}

pub fn get_expression_position(expression_value: &ExpressionValue) -> Position {
    match expression_value {
        ExpressionValue::String(token)
        | ExpressionValue::Number(token)
        | ExpressionValue::Variable(token) => token.position.clone(),
        ExpressionValue::Record {
            open_curly_bracket,
            closing_curly_bracket,
            ..
        } => Position {
            line_start: open_curly_bracket.position.line_start,
            column_start: open_curly_bracket.position.column_start,
            line_end: closing_curly_bracket.position.line_end,
            column_end: closing_curly_bracket.position.column_end,
        },
        ExpressionValue::FunctionCall(function_call) => {
            let first_argument = function_call.arguments.first().unwrap();
            let last_argument = function_call.arguments.last().unwrap();
            let first_position = get_expression_position(&first_argument.value);
            let last_position = get_expression_position(&last_argument.value);
            Position {
                line_start: first_position.line_start,
                column_start: first_position.column_start,
                line_end: last_position.line_end,
                column_end: last_position.column_end,
            }
        }
        other => panic!("{:#?}", other),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: Type,
    actual: Type,
    location: Location,
) -> Result<Type, UnifyError> {
    let expected = environment.resolve_type_alias(expected)?;
    let actual = environment.resolve_type_alias(actual)?;

    match (expected.clone(), actual.clone()) {
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::ImplicitTypeVariable { name: _ }, Type::ImplicitTypeVariable { name: _ }) => {
            panic!("mismatch?")
        }
        (Type::ImplicitTypeVariable { name }, other_type) => {
            environment.specialized_type_variable(name, other_type.clone());
            Ok(other_type)
        }
        (
            Type::Record {
                key_type_pairs: mut expected_key_type_pairs,
            },
            Type::Record {
                key_type_pairs: mut actual_key_type_pairs,
            },
        ) => {
            let mut expected_keys = expected_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key);

            let mut actual_keys = actual_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key);

            // 1. Find for missing keys
            let missing_keys: Vec<String> = expected_keys
                .clone()
                .filter(|expected_key| !actual_keys.any(|actual_key| *expected_key == actual_key))
                .collect();

            if !missing_keys.is_empty() {
                return Err(UnifyError::RecordMissingKeys {
                    missing_keys,
                    expected_type: Type::Record {
                        key_type_pairs: expected_key_type_pairs,
                    },
                    location,
                });
            }

            // 2. Find for extraneous keys
            let extraneous_keys: Vec<String> = actual_keys
                .filter(|actual_key| !expected_keys.any(|expected_key| expected_key == *actual_key))
                .collect();

            if !extraneous_keys.is_empty() {
                return Err(UnifyError::RecordExtraneousKeys {
                    extraneous_keys,
                    expected_type: Type::Record {
                        key_type_pairs: expected_key_type_pairs,
                    },
                    location,
                });
            }

            // 3. If no missing keys and no extraneous keys, that means all keys are present
            //    Therefore we can happily zip them and expect them to align properly after sorting
            expected_key_type_pairs.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

            actual_key_type_pairs.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

            let zipped = expected_key_type_pairs
                .clone()
                .into_iter()
                .zip(actual_key_type_pairs.clone().into_iter());

            let mut key_type_pairs: Vec<(String, Type)> = Vec::new();
            for ((key, expected_type), (_, actual_type)) in zipped {
                // TODO: Should pass in more specific location here
                match unify_type(
                    environment,
                    expected_type.clone(),
                    actual_type.clone(),
                    location.clone(),
                ) {
                    Ok(type_value) => key_type_pairs.push((key, type_value)),
                    Err(UnifyError::TypeMismatch {
                        expected_type,
                        actual_type,
                        ..
                    }) => {
                        return Err(UnifyError::RecordKeyTypeMismatch {
                            key,
                            expected_key_type: expected_type,
                            actual_key_type: actual_type,
                            expected_record_type: expected_key_type_pairs,
                            actual_record_type: actual_key_type_pairs,
                            location,
                        })
                    }
                    Err(other) => return Err(other),
                }
            }

            Ok(Type::Record { key_type_pairs })
        }
        _ => Err(UnifyError::TypeMismatch {
            location,
            expected_type: expected,
            actual_type: actual,
        }),
    }
}

pub fn infer_expression_type(
    environment: &mut Environment,
    expression: &ExpressionValue,
) -> Result<Type, UnifyError> {
    match expression {
        ExpressionValue::String(_) => Ok(Type::String),
        ExpressionValue::Number(_) => Ok(Type::Number),
        ExpressionValue::Variable(variable) => {
            if let Some(symbol) = environment.get_value_symbol(&variable.representation) {
                Ok(symbol.actual_type)
            } else {
                Err(UnifyError::UnknownValueSymbol {
                    location: Location {
                        source: environment.source.clone(),
                        position: variable.position.clone(),
                    },
                })
            }
        }
        ExpressionValue::Function(function) => {
            let first_function_branch_type =
                unify_function_branch(environment, &function.first_branch)?;

            let mut result_function_type = first_function_branch_type;

            for function_branch in &function.branches {
                result_function_type =
                    unify_function_type(environment, result_function_type, function_branch)?;
            }

            Ok(Type::Function(result_function_type))
        }
        ExpressionValue::FunctionCall(function_call) => {
            let function_type = infer_expression_type(environment, &function_call.function.value)?;
            match function_type {
                Type::Function(mut function_type) => {
                    // instatiate type variables with a non-conflicting name in current environment
                    for type_variable_name in function_type.type_variables.clone() {
                        let new_name = environment.instantiate_type_variable();
                        function_type = rewrite_function_type_type_variable(
                            type_variable_name,
                            new_name,
                            function_type,
                        );
                    }

                    // tally argument lengths
                    if function_type.arguments_types.len() != function_call.arguments.len() {
                        return Err(UnifyError::InvalidFunctionArgumentLength {
                            location: get_expression_location(
                                environment,
                                &function_call.function.value,
                            ),
                            expected_length: function_type.arguments_types.len(),
                            actual_length: function_call.arguments.len(),
                        });
                    }

                    let mut value_types: Vec<(ExpressionValue, Type)> = Vec::new();
                    for argument in &function_call.arguments {
                        value_types.push((
                            argument.value.clone(),
                            infer_expression_type(environment, &argument.value)?,
                        ))
                    }

                    // unify argument
                    let argument_types_value_types = function_type
                        .arguments_types
                        .into_iter()
                        .zip(value_types.into_iter());
                    for (argument_type, (value, value_type)) in argument_types_value_types {
                        unify_type(
                            environment,
                            argument_type,
                            value_type,
                            get_expression_location(environment, &value),
                        )?;
                    }

                    let return_type = environment.resolve_type_alias(*function_type.return_type)?;
                    Ok(return_type)
                }
                other => Err(UnifyError::CannotInvokeNonFunction {
                    actual_type: other,
                    location: get_expression_location(environment, &function_call.function.value),
                }),
            }
        }
        ExpressionValue::Record {
            key_value_pairs, ..
        } => {
            let mut key_type_pairs: Vec<(String, Type)> = Vec::new();
            for RecordKeyValue { key, value, .. } in key_value_pairs {
                // TODO: match with type annotation
                let value_type = infer_expression_type(environment, &value.value)?;
                key_type_pairs.push((key.representation.clone(), value_type))
            }
            Ok(Type::Record { key_type_pairs })
        }
        other => panic!("{:#?}", other),
    }
}

pub fn rewrite_function_type_type_variable(
    from_name: String,
    to_name: String,
    function_type: FunctionType,
) -> FunctionType {
    FunctionType {
        type_variables: function_type
            .type_variables
            .into_iter()
            .map(|type_variable_name| {
                if type_variable_name == from_name {
                    to_name.clone()
                } else {
                    type_variable_name
                }
            })
            .collect(),
        arguments_types: function_type
            .arguments_types
            .into_iter()
            .map(|argument_type| {
                rewrite_type_variable_as_type_alias(
                    from_name.clone(),
                    to_name.clone(),
                    argument_type,
                )
            })
            .collect(),
        return_type: Box::new(rewrite_type_variable_as_type_alias(
            from_name,
            to_name,
            *function_type.return_type,
        )),
    }
}

pub fn rewrite_type_variable_as_type_alias(
    from_name: String,
    to_name: String,
    type_value: Type,
) -> Type {
    match type_value {
        Type::Function(function_type) => Type::Function(rewrite_function_type_type_variable(
            from_name,
            to_name,
            function_type,
        )),
        Type::ImplicitTypeVariable { name } => {
            if name == from_name {
                Type::Alias { name: to_name }
            } else {
                Type::ImplicitTypeVariable { name }
            }
        }
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_type_variable_as_type_alias(
                            from_name.clone(),
                            to_name.clone(),
                            type_value,
                        ),
                    )
                })
                .collect(),
        },
        Type::String => Type::String,
        Type::Number => Type::Number,
        Type::Alias { name } => Type::Alias { name },
        other => panic!("{:#?}", other),
    }
}

pub fn unify_function_type(
    environment: &mut Environment,
    expected: FunctionType,
    actual: &FunctionBranch,
) -> Result<FunctionType, UnifyError> {
    let actual_function_type = unify_function_branch(environment, actual)?;
    if expected.arguments_types.len() != actual_function_type.arguments_types.len() {
        return Err(UnifyError::InvalidFunctionArgumentLength {
            expected_length: expected.arguments_types.len(),
            actual_length: actual_function_type.arguments_types.len(),
            location: Location {
                source: environment.source.clone(),
                position: get_function_branch_position(actual),
            },
        });
    }
    // unify argument types
    // unify return type
    panic!()
}

pub fn get_function_branch_position(function_branch: &FunctionBranch) -> Position {
    let body_position = get_expression_position(&function_branch.body.value);
    Position {
        line_start: function_branch.start_token.position.line_start,
        column_start: function_branch.start_token.position.column_start,
        line_end: body_position.line_end,
        column_end: body_position.column_end,
    }
}

pub fn unify_function_branch(
    environment: &mut Environment,
    function_branch: &FunctionBranch,
) -> Result<FunctionType, UnifyError> {
    let mut environment = Environment::new(&environment);
    let mut arguments_types = Vec::<Type>::new();
    for argument in &function_branch.arguments {
        arguments_types.push(unify_function_argument(&mut environment, &argument)?)
    }
    let body_type = infer_expression_type(&mut environment, &function_branch.body.value)?;
    let return_type = optional_type_annotation_to_type(
        &mut environment,
        &function_branch.return_type_annotation,
    )?;

    let type_variables = environment.get_implicit_type_variables();
    match return_type {
        Some(return_type) => {
            let location = get_expression_location(&environment, &function_branch.body.value);
            let return_type = unify_type(&mut environment, return_type, body_type, location)?;
            Ok(FunctionType {
                arguments_types,
                return_type: Box::new(return_type),
                type_variables,
            })
        }
        None => Ok(FunctionType {
            arguments_types,
            return_type: Box::new(body_type),
            type_variables,
        }),
    }
}

pub fn unify_function_argument(
    environment: &mut Environment,
    function_argument: &FunctionArgument,
) -> Result<Type, UnifyError> {
    let type_annotation_type =
        optional_type_annotation_to_type(environment, &function_argument.type_annotation)?;

    match type_annotation_type {
        Some(type_annotation_type) => Ok(unify_destructure_pattern(
            environment,
            &function_argument.destructure_pattern,
            &type_annotation_type,
        )?),
        None => infer_destructure_pattern(environment, &function_argument.destructure_pattern),
    }
}

pub fn optional_type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: &Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match type_annotation {
        Some(type_annotation) => Ok(Some(type_annotation_to_type(environment, type_annotation)?)),
        None => Ok(None),
    }
}

pub fn type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: &TypeAnnotation,
) -> Result<Type, UnifyError> {
    match &type_annotation.representation {
        TypeRepresentation::Name(token) => {
            if let Ok(ty) = environment.get_type_symbol(&token) {
                Ok(ty.type_value)
            } else {
                Err(UnifyError::UnknownTypeSymbol {
                    location: Location {
                        source: environment.source.clone(),
                        position: token.position.clone(),
                    },
                })
            }
        }
        TypeRepresentation::Record {
            key_type_annotation_pairs,
        } => {
            let mut key_type_pairs: Vec<(String, Type)> = Vec::new();
            for (key, type_annotation) in key_type_annotation_pairs {
                let type_value = type_annotation_to_type(environment, &type_annotation)?;
                key_type_pairs.push((key.representation.clone(), type_value))
            }
            Ok(Type::Record { key_type_pairs })
        }
        _ => panic!(),
    }
}

pub fn infer_destructure_pattern(
    environment: &mut Environment,
    destructure_pattern: &DestructurePattern,
) -> Result<Type, UnifyError> {
    match destructure_pattern {
        DestructurePattern::Identifier(identifier) => {
            environment.introduce_type_variable(identifier)
        }
        DestructurePattern::Tag { token, payload } => Ok(Type::Tag {
            tagname: token.representation.clone(),
            payload: match payload {
                None => None,
                Some(payload) => Some(Box::new(infer_destructure_pattern(environment, payload)?)),
            },
        }),
        DestructurePattern::Record { key_value_pairs } => {
            let mut key_type_pairs: Vec<(String, Type)> = Vec::new();
            for DestructuredRecordKeyValue { key, as_value, .. } in key_value_pairs {
                // TODO: spread
                // TODO: match against type annotation
                let name = key.representation.clone();
                match as_value {
                    Some(destructure_pattern) => {
                        let type_value =
                            infer_destructure_pattern(environment, destructure_pattern)?;
                        key_type_pairs.push((name, type_value))
                    }
                    None => {
                        let type_variable = environment.introduce_type_variable(&key)?;
                        key_type_pairs.push((name, type_variable))
                    }
                }
            }
            Ok(Type::Record { key_type_pairs })
        }
        _ => panic!(),
    }
}

pub fn unify_destructure_pattern(
    environment: &mut Environment,
    destructure_pattern: &DestructurePattern,
    expression_type: &Type,
) -> Result<Type, UnifyError> {
    match (destructure_pattern, expression_type) {
        (
            DestructurePattern::Tag {
                payload: None,
                token,
            },
            Type::Tag {
                tagname,
                payload: None,
            },
        ) => {
            if token.representation == *tagname {
                Ok(Type::Tag {
                    tagname: token.representation.clone(),
                    payload: None,
                })
            } else {
                Err(UnifyError::CannotDestructure {
                    expression_type: expression_type.clone(),
                    destructure_pattern: destructure_pattern.clone(),
                })
            }
        }
        (DestructurePattern::Identifier(token), type_value) => {
            environment.insert_value_symbol(
                token,
                ValueSymbol {
                    actual_type: type_value.clone(),
                    declaration: Declaration::UserDefined(
                        environment.source.clone(),
                        token.clone(),
                    ),
                    usage_references: vec![],
                },
            )?;
            Ok(type_value.clone())
        }
        (_, _) => Err(UnifyError::CannotDestructure {
            expression_type: expression_type.clone(),
            destructure_pattern: destructure_pattern.clone(),
        }),
    }
}
