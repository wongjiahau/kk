use crate::ast::*;
use std::collections::HashSet;

use crate::environment::*;

pub struct Program {
    pub statements: Vec<Statement>,
    pub source: Source,
}

pub fn unify_program(program: Program) -> Result<(), UnifyError> {
    // 1. TODO: Populate environment with imported symbols
    let mut environment: Environment = Environment::new_root(program.source);

    // 2. Type check this program
    let _ = program
        .statements
        .into_iter()
        .map(|statement| infer_statement(&mut environment, statement))
        .collect::<Result<Vec<()>, UnifyError>>()?;
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

pub fn infer_statement(
    environment: &mut Environment,
    statement: Statement,
) -> Result<(), UnifyError> {
    // println!("{:#?}", environment);
    match statement {
        Statement::Let {
            left,
            right,
            type_annotation,
        } => {
            let type_annotation_type =
                optional_type_annotation_to_type(environment, &type_annotation)?;
            let right_type = infer_expression_type(environment, &right.value)?;

            // println!("right_type {:#?}", right_type);

            // 1. Check if right matches type annotation
            let right_type = match type_annotation_type {
                Some(type_annotation_type) => {
                    let location = get_expression_location(environment, &right.value);
                    unify_type(environment, &type_annotation_type, &right_type, location)?;
                    right_type
                }
                None => right_type,
            };

            // 2. Generalize if possible
            let right_type_scheme = generalize_type(right_type);

            // 3. Unify left with right_type and populate parent environment with bindings found in left
            environment.insert_value_symbol(
                &left,
                ValueSymbol {
                    declaration: Declaration::UserDefined(environment.source.clone(), left.clone()),
                    actual_type: right_type_scheme,
                    usage_references: vec![],
                },
            )?;

            Ok(())
        }
        _ => panic!(),
    }
}

pub fn generalize_type(type_value: Type) -> TypeScheme {
    let type_variables = get_free_type_variables_in_type(&type_value);
    TypeScheme {
        type_variables: type_variables.into_iter().collect(),
        type_value,
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
        ExpressionValue::Function(function) => {
            let start_position = function.first_branch.start_token.position.clone();
            let end_position = match function.branches.last() {
                Some(last_branch) => get_expression_position(&last_branch.body.value),
                None => get_expression_position(&function.first_branch.body.value),
            };
            Position {
                line_start: start_position.line_start,
                column_start: start_position.column_start,
                line_end: end_position.line_end,
                column_end: end_position.column_end,
            }
        }
        other => panic!("{:#?}", other),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    location: Location,
) -> Result<(), UnifyError> {
    match unify_type_(environment, expected, actual, location) {
        Err(UnifyError::TypeMismatch {
            location,
            expected_type,
            actual_type,
        }) => {
            println!("{:#?}", environment);
            Err(UnifyError::TypeMismatch {
                location,
                expected_type: environment.apply_subtitution_to_type(&expected_type),
                actual_type: environment.apply_subtitution_to_type(&actual_type),
            })
        }
        other => other,
    }
}

pub fn unify_type_(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    location: Location,
) -> Result<(), UnifyError> {
    match (expected.clone(), actual.clone()) {
        (Type::String, Type::String) => Ok(()),
        (Type::Number, Type::Number) => Ok(()),
        (Type::TypeVariable { name }, other_type) | (other_type, Type::TypeVariable { name }) => {
            if type_variable_occurs_in_type(&name, &other_type) {
                panic!("circular type substitution found")
            } else {
                environment.update_substitution(name, other_type, location)
            }
        }
        (Type::Function(expected_function), Type::Function(actual_function)) => {
            unify_function_type(environment, &expected_function, &actual_function, location)
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

            zipped
                .into_iter()
                .map(|((key, expected_type), (_, actual_type))| {
                    // TODO: Should pass in more specific location here
                    match unify_type(environment, &expected_type, &actual_type, location.clone()) {
                        Ok(_) => Ok(()),
                        Err(UnifyError::TypeMismatch {
                            expected_type,
                            actual_type,
                            ..
                        }) => Err(UnifyError::RecordKeyTypeMismatch {
                            key,
                            expected_key_type: expected_type,
                            actual_key_type: actual_type,
                            expected_record_type: expected_key_type_pairs.clone(),
                            actual_record_type: actual_key_type_pairs.clone(),
                            location: location.clone(),
                        }),
                        Err(other) => Err(other),
                    }
                })
                .collect::<Result<Vec<()>, UnifyError>>()?;
            Ok(())
        }
        _ => Err(UnifyError::TypeMismatch {
            location,
            expected_type: expected.clone(),
            actual_type: actual.clone(),
        }),
    }
}

pub fn infer_expression_type(
    environment: &mut Environment,
    expression: &ExpressionValue,
) -> Result<Type, UnifyError> {
    let result = match expression {
        ExpressionValue::String(_) => Ok(Type::String),
        ExpressionValue::Number(_) => Ok(Type::Number),
        ExpressionValue::Variable(variable) => {
            if let Some(symbol) = environment.get_value_symbol(&variable.representation) {
                struct TypeVariableSubstitution {
                    from_type_variable: String,
                    to_type_variable: String,
                }
                let type_variables: Vec<TypeVariableSubstitution> = symbol
                    .actual_type
                    .type_variables
                    .into_iter()
                    .map(|from_type_variable| TypeVariableSubstitution {
                        from_type_variable,
                        to_type_variable: environment.get_next_type_variable_name(),
                    })
                    .collect();

                let type_value = type_variables.into_iter().fold(
                    symbol.actual_type.type_value,
                    |result,
                     TypeVariableSubstitution {
                         from_type_variable,
                         to_type_variable,
                     }| {
                        substitute_type_variable_in_type(
                            &from_type_variable,
                            &Type::TypeVariable {
                                name: to_type_variable,
                            },
                            &result,
                        )
                    },
                );

                Ok(type_value)
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
                infer_function_branch(environment, &function.first_branch)?;

            // TODO: Unify function branches
            // let mut result_function_type = first_function_branch_type;
            // let mut result_substitution = Substitution::new()
            for function_branch in &function.branches {
                let actual_function_type = infer_function_branch(environment, function_branch)?;
                let position = get_function_branch_position(function_branch);
                unify_function_type(
                    environment,
                    &first_function_branch_type,
                    &actual_function_type,
                    Location {
                        source: environment.source.clone(),
                        position,
                    },
                )?;
            }

            Ok(Type::Function(first_function_branch_type))
        }
        ExpressionValue::FunctionCall(function_call) => {
            let expected_function_type =
                match infer_expression_type(environment, &function_call.function.value)? {
                    Type::Function(function) => Ok(Type::Function(function)),
                    Type::TypeVariable { name } => Ok(Type::TypeVariable { name }),
                    other => Err(UnifyError::CannotInvokeNonFunction {
                        location: get_expression_location(
                            environment,
                            &function_call.function.value,
                        ),
                        actual_type: other.clone(),
                    }),
                }?;

            let arguments_types = infer_expressions(environment, &function_call.arguments)?;

            let return_type = Type::TypeVariable {
                name: environment.get_next_type_variable_name(),
            };
            let actual_function_type = Type::Function(FunctionType {
                arguments_types,
                return_type: Box::new(return_type.clone()),
            });

            unify_type(
                environment,
                &expected_function_type,
                &actual_function_type,
                get_expression_location(
                    environment,
                    &ExpressionValue::FunctionCall(function_call.clone()),
                ),
            )?;

            Ok(return_type)

            // match expected_function_type {
            //     Type::Function(mut function_type) => {
            //         // instatiate type variables with a non-conflicting name in current environment
            //         for type_variable_name in function_type.type_variables.clone() {
            //             let new_name = environment.instantiate_type_variable();
            //             function_type = rewrite_function_type_type_variable(
            //                 type_variable_name,
            //                 new_name,
            //                 function_type,
            //             );
            //         }

            //         // tally argument lengths
            //         if function_type.arguments_types.len() != function_call.arguments.len() {
            //             return Err(UnifyError::InvalidFunctionArgumentLength {
            //                 location: get_expression_location(
            //                     environment,
            //                     &function_call.function.value,
            //                 ),
            //                 expected_length: function_type.arguments_types.len(),
            //                 actual_length: function_call.arguments.len(),
            //             });
            //         }

            //         // unify argument
            //         let argument_types_value_types = function_type
            //             .arguments_types
            //             .into_iter()
            //             .zip(value_types.into_iter());

            //         let _ = argument_types_value_types
            //             .into_iter()
            //             .map(|(argument_type, (value, value_type))| {
            //                 unify_type(
            //                     environment,
            //                     argument_type,
            //                     value_type,
            //                     get_expression_location(environment, &value),
            //                 )
            //             })
            //             .collect::<Result<Vec<_>, UnifyError>>()?;

            //         let return_type = environment.resolve_type_alias(*function_type.return_type)?;
            //         Ok(return_type)
            //     }
            //     Type::ImplicitTypeVariable { name } => {
            //         let function_type = Type::Function(FunctionType {
            //             arguments_types: value_types
            //                 .into_iter()
            //                 .map(|(_, type_value)| type_value)
            //                 .collect(),
            //             return_type: Box::new(environment.introduce_type_variable(None)?),
            //             type_variables: Vec::new(),
            //         });
            //         environment.specialized_type_variable(name, function_type.clone());
            //         Ok(function_type)
            //     }
            //     other => Err(UnifyError::CannotInvokeNonFunction {
            //         actual_type: other,
            //         location: get_expression_location(environment, &function_call.function.value),
            //     }),
            // }
        }
        ExpressionValue::Record {
            key_value_pairs, ..
        } => {
            let key_type_pairs = key_value_pairs
                .into_iter()
                .map(|RecordKeyValue { key, value, .. }| {
                    let value_type = infer_expression_type(environment, &value.value)?;
                    Ok((key.representation.clone(), value_type))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;

            Ok(Type::Record { key_type_pairs })
        }
        other => panic!("{:#?}", other),
    }?;
    Ok(environment.apply_subtitution_to_type(&result))
}

pub fn infer_expressions(
    environment: &mut Environment,
    expressions: &Vec<Expression>,
) -> Result<Vec<Type>, UnifyError> {
    let value_types = expressions
        .into_iter()
        .map(|argument| Ok(infer_expression_type(environment, &argument.value)?))
        .collect::<Result<Vec<Type>, UnifyError>>()?;
    Ok(value_types)
}

// pub fn rewrite_function_type_type_variable(
//     from_name: String,
//     to_name: String,
//     function_type: FunctionType,
// ) -> FunctionType {
//     FunctionType {
//         type_variables: function_type
//             .type_variables
//             .into_iter()
//             .map(|type_variable_name| {
//                 if type_variable_name == from_name {
//                     to_name.clone()
//                 } else {
//                     type_variable_name
//                 }
//             })
//             .collect(),
//         arguments_types: function_type
//             .arguments_types
//             .into_iter()
//             .map(|argument_type| {
//                 rewrite_type_variable_as_type_alias(
//                     from_name.clone(),
//                     to_name.clone(),
//                     argument_type,
//                 )
//             })
//             .collect(),
//         return_type: Box::new(rewrite_type_variable_as_type_alias(
//             from_name,
//             to_name,
//             *function_type.return_type,
//         )),
//     }
// }

// pub fn rewrite_type_variable_as_type_alias(
//     from_name: String,
//     to_name: String,
//     type_value: Type,
// ) -> Type {
//     match type_value {
//         Type::Function(function_type) => Type::Function(rewrite_function_type_type_variable(
//             from_name,
//             to_name,
//             function_type,
//         )),
//         Type::ImplicitTypeVariable { name } => {
//             if name == from_name {
//                 Type::Alias { name: to_name }
//             } else {
//                 Type::ImplicitTypeVariable { name }
//             }
//         }
//         Type::Record { key_type_pairs } => Type::Record {
//             key_type_pairs: key_type_pairs
//                 .into_iter()
//                 .map(|(key, type_value)| {
//                     (
//                         key,
//                         rewrite_type_variable_as_type_alias(
//                             from_name.clone(),
//                             to_name.clone(),
//                             type_value,
//                         ),
//                     )
//                 })
//                 .collect(),
//         },
//         Type::String => Type::String,
//         Type::Number => Type::Number,
//         Type::Alias { name } => Type::Alias { name },
//         other => panic!("{:#?}", other),
//     }
// }

pub fn unify_function_type(
    environment: &mut Environment,
    expected_function: &FunctionType,
    actual_function: &FunctionType,
    location: Location,
) -> Result<(), UnifyError> {
    // compare arguments length
    if expected_function.arguments_types.len() != actual_function.arguments_types.len() {
        return Err(UnifyError::InvalidFunctionArgumentLength {
            location,
            expected_length: expected_function.arguments_types.len(),
            actual_length: actual_function.arguments_types.len(),
        });
    }

    // unify every argument type
    let zipped = expected_function
        .clone()
        .arguments_types
        .into_iter()
        .zip(actual_function.arguments_types.clone().into_iter());

    zipped
        .map(|(expected_type, actual_type)| {
            unify_type(environment, &expected_type, &actual_type, location.clone())
        })
        .collect::<Result<Vec<()>, UnifyError>>()?;

    // unify return type
    unify_type(
        environment,
        expected_function.return_type.as_ref(),
        actual_function.return_type.as_ref(),
        location.clone(),
    )
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

pub fn infer_function_branch(
    environment: &mut Environment,
    function_branch: &FunctionBranch,
) -> Result<FunctionType, UnifyError> {
    // Initialize new environment for this function branch
    let mut environment = Environment::new(&environment);

    let arguments_types = function_branch
        .arguments
        .iter()
        .map(|argument| Ok(unify_function_argument(&mut environment, &argument)?))
        .collect::<Result<Vec<Type>, UnifyError>>()?;

    let body_type = infer_expression_type(&mut environment, &function_branch.body.value)?;

    let return_type = optional_type_annotation_to_type(
        &mut environment,
        &function_branch.return_type_annotation,
    )?;

    let result = match return_type {
        Some(return_type) => {
            let location = get_expression_location(&environment, &function_branch.body.value);
            unify_type(&mut environment, &return_type, &body_type, location)?;

            panic!("not implemented")

            // let result = FunctionType {
            //     arguments_types,
            //     return_type: Box::new(return_type),
            // };
            // Ok(substitution1
            //     .compose(substitution2)
            //     .apply_to(Type::Function(result)))
        }
        None => Ok(FunctionType {
            arguments_types,
            return_type: Box::new(body_type),
        }),
    }?;

    Ok(environment.apply_subtitution_to_function_type(&result))

    // // remap instantiated type variables
    // let arguments_types = function_type
    //     .arguments_types
    //     .into_iter()
    //     .map(|argument_type| environment.resolve_type_alias(argument_type))
    //     .collect::<Result<Vec<Type>, UnifyError>>()?;

    // let return_type = environment.resolve_type_alias(*function_type.return_type)?;

    // println!("{:#?}", environment);

    // Ok(FunctionType {
    //     arguments_types,
    //     return_type: Box::new(return_type),
    // })
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
            if let Ok(symbol) = environment.get_type_symbol(&token) {
                if symbol.type_scheme.type_variables.is_empty() {
                    Ok(symbol.type_scheme.type_value)
                } else {
                    panic!("Type variable needs to be specified")
                }
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
            let key_type_pairs = key_type_annotation_pairs
                .into_iter()
                .map(|(key, type_annotation)| {
                    let type_value = type_annotation_to_type(environment, &type_annotation)?;
                    Ok((key.representation.clone(), type_value))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
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
            environment.introduce_type_variable(Some(&identifier))
        }
        DestructurePattern::Tag { token, payload } => Ok(Type::Tag {
            tagname: token.representation.clone(),
            payload: match payload {
                None => None,
                Some(payload) => Some(Box::new(infer_destructure_pattern(environment, payload)?)),
            },
        }),
        DestructurePattern::Record { key_value_pairs } => {
            let key_type_pairs: Vec<(String, Type)> = key_value_pairs
                .iter()
                .map(|DestructuredRecordKeyValue { key, as_value, .. }| {
                    // TODO: spread
                    // TODO: match against type annotation
                    let name = key.representation.clone();
                    match as_value {
                        Some(destructure_pattern) => {
                            let type_value =
                                infer_destructure_pattern(environment, destructure_pattern)?;

                            Ok((name, type_value))
                        }
                        None => {
                            let type_variable = environment.introduce_type_variable(Some(&key))?;
                            Ok((name, type_variable))
                        }
                    }
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
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
                    actual_type: TypeScheme {
                        type_variables: vec![],
                        type_value: type_value.clone(),
                    },
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

fn substitute_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: &Type,
) -> Type {
    match in_type {
        Type::TypeVariable { name } => {
            if *name == *from_type_variable {
                to_type.clone()
            } else {
                in_type.clone()
            }
        }
        Type::Function(FunctionType {
            arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            arguments_types: arguments_types
                .iter()
                .map(|argument_type| {
                    substitute_type_variable_in_type(from_type_variable, to_type, argument_type)
                })
                .collect(),
            return_type: Box::new(substitute_type_variable_in_type(
                from_type_variable,
                to_type,
                return_type.as_ref(),
            )),
        }),
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        substitute_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
        Type::Compound { .. } => panic!(),
        _ => in_type.clone(),
    }
}

/**
 * Type variable quantified by the type scheme will not be substituted
 */
// fn substitute_type_variable_in_type_scheme(
//     from_type_variable: String,
//     to_type: Type,
//     in_type_scheme: TypeScheme,
// ) -> TypeScheme {
// }

fn get_free_type_variables_in_type(type_value: &Type) -> HashSet<String> {
    match type_value {
        Type::TypeVariable { name } => {
            let mut result: HashSet<String> = HashSet::new();
            result.insert(name.clone());
            result
        }
        Type::Function(FunctionType {
            arguments_types,
            return_type,
        }) => {
            let mut result: HashSet<String> = HashSet::new();
            let type_variables: Vec<HashSet<String>> = arguments_types
                .iter()
                .map(get_free_type_variables_in_type)
                .collect();
            type_variables
                .into_iter()
                .for_each(|type_variables| result.extend(type_variables));

            result.extend(get_free_type_variables_in_type(return_type.as_ref()));

            result
        }
        Type::String | Type::Number => HashSet::new(),
        Type::Record { key_type_pairs } => {
            let type_variables = key_type_pairs
                .iter()
                .map(|(_, type_value)| get_free_type_variables_in_type(&type_value))
                .collect::<Vec<HashSet<String>>>();
            type_variables
                .into_iter()
                .fold(HashSet::new(), |result, type_variables| {
                    result.into_iter().chain(type_variables).collect()
                })
        }
        other => panic!("get_free_Type_variables_in_type({:#?})", other),
    }
}

/**
 * To check whether a type variable occur in a type.
 * This is to prevent absurd unification.
 * For example, the unification of A with (A -> B) should not
 * produce the subtituion of {A = A -> B}
 */
fn type_variable_occurs_in_type(type_variable: &str, typ: &Type) -> bool {
    get_free_type_variables_in_type(typ).contains(type_variable)
}

// let rec ftv_type (t : 'a typ) : StringSet.t =
//   match t with
//   | TyCon _ -> StringSet.empty
//   | TyVar(name, _) -> StringSet.singleton name
//   | TyArr(args, ret, _) ->
//     List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
//                     args
//                     (ftv_type ret)
//   | TyApp(typ, args, _) ->
//     List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
//                     args
//                     (ftv_type typ)
// ;;
// let ftv_scheme (s : 'a scheme) : StringSet.t =
//   match s with
//   | SForall(args, typ, _) ->
//      StringSet.diff (ftv_typ typ) (StringSet.of_list args)
// let ftv_env (e : 'a typ envt) : StringSet.t = ...
