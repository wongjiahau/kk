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
            let type_annotation_type = type_annotation_to_type(environment, &type_annotation)?;
            let right_type = infer_expression_type(environment, &right.value)?;

            // 1. Check if right matches type annotation
            let right_type = match type_annotation_type {
                Some(type_annotation_type) => {
                    let location = get_expression_location(environment, &right.value);
                    let (_, right_type) =
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
        other => panic!("{:#?}", other),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: Type,
    actual: Type,
    location: Location,
) -> Result<(Type, Type), UnifyError> {
    let expected = environment.resolve_type_alias(expected)?;
    let actual = environment.resolve_type_alias(actual)?;

    match (expected.clone(), actual.clone()) {
        (Type::String, Type::String) => Ok((expected, actual)),
        (Type::Number, Type::Number) => Ok((expected, actual)),
        (Type::ImplicitTypeVariable { name: _ }, Type::ImplicitTypeVariable { name: _ }) => {
            panic!("mismatch?")
        }
        (Type::ImplicitTypeVariable { name }, other_type) => {
            environment.specialized_type_variable(name, other_type.clone());
            Ok((other_type.clone(), other_type))
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

            println!("{:#?}", first_function_branch_type);
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
                Type::Function(function_type) => {
                    let mut function_type = function_type.clone();
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
                    actual_type: other.clone(),
                    location: get_expression_location(environment, &function_call.function.value),
                }),
            }
        }
        _ => panic!(),
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
            from_name.clone(),
            to_name.clone(),
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
    let return_type =
        type_annotation_to_type(&mut environment, &function_branch.return_type_annotation)?;

    let type_variables = environment.get_implicit_type_variables();
    match return_type {
        Some(return_type) => {
            let location = get_expression_location(&environment, &function_branch.body.value);
            let (return_type, _) = unify_type(&mut environment, return_type, body_type, location)?;
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
        type_annotation_to_type(environment, &function_argument.type_annotation)?;

    match type_annotation_type {
        Some(type_annotation_type) => Ok(unify_destructure_pattern(
            environment,
            &function_argument.destructure_pattern,
            &type_annotation_type,
        )?),
        None => infer_destructure_pattern(environment, &function_argument.destructure_pattern),
    }
}

pub fn type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: &Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match type_annotation {
        None => Ok(None),
        Some(type_annotation) => match &type_annotation.representation {
            TypeRepresentation::Name(token) => {
                if let Ok(ty) = environment.get_type_symbol(&token) {
                    Ok(Some(ty.type_value.clone()))
                } else {
                    Err(UnifyError::UnknownTypeSymbol {
                        location: Location {
                            source: environment.source.clone(),
                            position: token.position.clone(),
                        },
                    })
                }
            }
            _ => panic!(),
        },
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
        // DestructurePattern::Record { key_value_pairs } => {
        //     let result: Vec<(Token, InferDestructurePatternResult)> = key_value_pairs
        //         .into_iter()
        //         .map(
        //             |DestructuredRecordKeyValue {
        //                  key,
        //                  type_annotation,
        //                  as_value,
        //                  spread,
        //              }| {
        //                 let InferDestructurePatternResult {
        //                     mut symbols,
        //                     inferred_type,
        //                 } = match as_value {
        //                     Some(as_value) => infer_destructure_pattern(environment, as_value),
        //                     None => InferDestructurePatternResult {
        //                         inferred_type: Type::NotInferred,
        //                         symbols: vec![Symbol {
        //                             name: key.clone().representation.to_string(),
        //                             declaration: Some(key.clone()),
        //                             annotated_type: type_annotation,
        //                             actual_type: None,
        //                             usage_references: vec![],
        //                         }],
        //                     },
        //                 };
        //                 match spread {
        //                     None => (),
        //                     Some(token) => {
        //                         symbols.push(Symbol {
        //                             name: token.representation.to_string(),
        //                             declaration: Some(token),
        //                             annotated_type: None,
        //                             actual_type: None,
        //                             usage_references: vec![],
        //                         });
        //                     }
        //                 }
        //                 (
        //                     key,
        //                     InferDestructurePatternResult {
        //                         symbols,
        //                         inferred_type,
        //                     },
        //                 )
        //             },
        //         )
        //         .collect();
        //     InferDestructurePatternResult {
        //         symbols: result
        //             .clone()
        //             .into_iter()
        //             .flat_map(|(_, InferDestructurePatternResult { symbols, .. })| symbols)
        //             .collect(),
        //         inferred_type: Type::Record {
        //             key_type_pairs: result
        //                 .into_iter()
        //                 .map(
        //                     |(key, InferDestructurePatternResult { inferred_type, .. })| {
        //                         (key.representation, inferred_type)
        //                     },
        //                 )
        //                 .collect(),
        //         },
        //     }
        // }
        // DestructurePattern::Underscore(_) => InferDestructurePatternResult {
        //     symbols: vec![],
        //     inferred_type: Type::NotInferred,
        // },
    }
}
