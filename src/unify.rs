use crate::ast::*;
use std::collections::HashSet;
use std::iter;

use crate::environment::*;
use crate::pattern::*;
use crate::stringify_error::stringify_type;

pub struct Program {
    pub statements: Vec<Statement>,
    pub source: Source,
}

pub fn unify_program(program: Program) -> Result<(), UnifyError> {
    // 1. TODO: Populate environment with imported symbols
    let mut environment: Environment = Environment::new(&program.source);

    // 2. Type check this program
    let _ = program
        .statements
        .into_iter()
        .map(|statement| infer_statement(&mut environment, statement))
        .collect::<Result<Vec<()>, UnifyError>>()?;
    Ok(())
}

#[derive(Debug)]
pub struct UnifyError {
    pub position: Position,
    pub kind: UnifyErrorKind,
}

#[derive(Debug)]
pub enum UnifyErrorKind {
    WrongTypeAnnotation {
        expected_type: Type,
    },
    NotExpectingArray {
        expected_type: Type,
    },
    NotExpectingFunction {
        expected_type: Type,
    },
    DuplicatedRecordKey,
    UnknownTypeParameterName {
        expected_names: Vec<String>,
    },
    TypeArgumentNameMismatch {
        expected_name: String,
    },
    InfiniteTypeDetected,
    DoBodyMustHaveNullType,
    NoSuchPropertyOnThisRecord {
        expected_keys: Vec<String>,
    },
    CannotAccessPropertyOfNonRecord {
        actual_type: Type,
    },
    UnusedVariale,
    MissingCases(Vec<TypedDestructurePattern>),
    ThisTagDoesNotRequirePayload,
    ThisTagRequiresPaylod {
        payload_type: Type,
    },
    UnreachableCase,
    TypeArgumentsLengthMismatch {
        actual_length: usize,
        expected_type_parameter_names: Vec<String>,
    },
    LetElseMustBeSingleArgumentFunction {
        actual_type: Type,
    },
    LetElseMustBeFunction {
        actual_type: Type,
    },
    FunctionTypeArgumentMismatch {
        argument_index: usize,
        unify_error: Box<UnifyError>,
        expected_function_type: FunctionType,
        actual_function_type: FunctionType,
    },
    FunctionReturnTypeMismatch {
        unify_error: Box<UnifyError>,
        expected_function_type: FunctionType,
        actual_function_type: FunctionType,
    },
    CannotUnionNonTagType {
        the_non_union_type: Type,
    },
    RecordKeyTypeMismatch {
        key: String,
        expected_key_type: Type,
        actual_key_type: Type,
        expected_record_type: Vec<(String, Type)>,
        actual_record_type: Vec<(String, Type)>,
    },
    RecordExtraneousKey {
        expected_keys: Vec<String>,
    },
    RecordMissingKeys {
        missing_keys: Vec<String>,
    },
    CannotInvokeNonFunction {
        actual_type: Type,
    },
    DuplicatedIdentifier {
        name: String,
        first_declared_at: Declaration,
        then_declared_at: Declaration,
    },
    InvalidFunctionArgumentLength {
        expected_length: usize,
        actual_length: usize,
    },
    UnknownTypeSymbol,
    UnknownValueSymbol,
    UnknownConstructorSymbol,
    TypeMismatch {
        expected_type: Type,
        actual_type: Type,
    },
    TagTypeMismatch {
        expected_tag_type: TagType,
        actual_tag_type: TagType,
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
    match statement {
        Statement::Do { expression } => {
            match infer_expression_type(
                environment,
                // NOTE: we could have pass in Some(Type::Null) instead of None here.
                //       The reason is because we want to provide better error message with context.
                //       So instead of saying expected type is null, we tell the user
                //       that the body of a `do` statement must be null
                None,
                &expression,
            )? {
                Type::Null => Ok(()),
                _ => Err(UnifyError {
                    position: get_expression_position(&expression),
                    kind: UnifyErrorKind::DoBodyMustHaveNullType,
                }),
            }
        }
        Statement::Let {
            left,
            right,
            type_annotation,
            type_variables,
        } => {
            // 0. Populate type variables
            environment.step_into_new_child_scope();
            type_variables
                .iter()
                .map(|type_variable| environment.insert_explicit_type_variable(type_variable))
                .collect::<Result<Vec<_>, UnifyError>>()?;

            // 1. Check if right matches type annotation
            let type_annotation_type =
                optional_type_annotation_to_type(environment, &type_annotation)?;

            let right_type = infer_expression_type(environment, type_annotation_type, &right)?;

            // 2. Rewrite explicit type variable as implicit type variables
            let explicit_type_variable_names = type_variables
                .iter()
                .map(|token| token.representation.clone())
                .collect();

            // println!(
            //     "right_type_1 = {:#?}",
            //     stringify_type(right_type.clone(), 0)
            // );
            let right_type = rewrite_explicit_type_variables_as_implicit(
                right_type,
                &explicit_type_variable_names,
            );
            // println!(
            //     "right_type_2 = {:#?}",
            //     stringify_type(right_type.clone(), 0)
            // );

            // 3. Generalize if possible
            let right_type_scheme = generalize_type(right_type);

            // 4. Add this variable into environment
            environment.step_out_to_parent_scope();
            environment.insert_value_symbol(
                &left,
                ValueSymbol {
                    declaration: Declaration::UserDefined {
                        source: environment.source.clone(),
                        token: left.clone(),
                        scope_name: environment.current_scope_name(),
                    },
                    type_scheme: right_type_scheme,
                    usage_references: Default::default(),
                },
            )?;

            Ok(())
        }
        Statement::Type {
            left,
            right,
            type_variables,
        } => {
            environment.step_into_new_child_scope();
            // 1. Populate type variables into current environment
            environment.step_into_new_child_scope();
            for type_variable in type_variables.clone() {
                environment.insert_type_symbol(
                    &type_variable,
                    TypeSymbol {
                        type_scheme: TypeScheme {
                            type_value: Type::ImplicitTypeVariable {
                                name: type_variable.clone().representation,
                            },
                            type_variables: vec![],
                        },
                        declaration: Declaration::UserDefined {
                            source: environment.source.clone(),
                            token: type_variable.clone(),
                            scope_name: environment.current_scope_name(),
                        },
                        usage_references: Default::default(),
                    },
                )?;
            }

            // 2. verify type declaration
            let type_value = type_annotation_to_type(environment, &right)?;

            // 3. Add this symbol into environment
            environment.step_out_to_parent_scope();
            environment.insert_type_symbol(
                &left,
                TypeSymbol {
                    declaration: Declaration::UserDefined {
                        source: environment.source.clone(),
                        token: left.clone(),
                        scope_name: environment.current_scope_name(),
                    },
                    type_scheme: TypeScheme {
                        type_variables: type_variables
                            .into_iter()
                            .map(|type_variable| type_variable.representation)
                            .collect(),
                        type_value,
                    },
                    usage_references: Default::default(),
                },
            )?;

            Ok(())
        }
        Statement::Enum {
            name,
            type_variables,
            tags,
        } => {
            // 1. Add this enum into environment first, to allow recursive definition
            let enum_name = name.representation.clone();
            let enum_type = Type::Named {
                name: enum_name.clone(),
                type_arguments: type_variables
                    .clone()
                    .into_iter()
                    .map(|type_variable| {
                        (
                            type_variable.representation.clone(),
                            Type::ImplicitTypeVariable {
                                name: type_variable.representation,
                            },
                        )
                    })
                    .collect(),
            };
            let type_variable_names = type_variables
                .clone()
                .into_iter()
                .map(|type_variable| type_variable.representation)
                .collect::<Vec<String>>();

            environment.insert_type_symbol(
                &name,
                TypeSymbol {
                    declaration: Declaration::UserDefined {
                        source: environment.source.clone(),
                        token: name.clone(),
                        scope_name: environment.current_scope_name(),
                    },
                    type_scheme: TypeScheme {
                        type_variables: type_variable_names.clone(),
                        type_value: enum_type,
                    },
                    usage_references: Default::default(),
                },
            )?;

            let source = environment.source.clone();

            // 2. Populate type variables into current environment
            environment.step_into_new_child_scope();
            for type_variable in type_variables.clone() {
                environment.insert_type_symbol(
                    &type_variable,
                    TypeSymbol {
                        type_scheme: TypeScheme {
                            type_value: Type::ImplicitTypeVariable {
                                name: type_variable.clone().representation,
                            },
                            type_variables: vec![],
                        },
                        declaration: Declaration::UserDefined {
                            source: source.clone(),
                            token: type_variable.clone(),
                            scope_name: environment.current_scope_name(),
                        },
                        usage_references: Default::default(),
                    },
                )?;
            }

            // 3. Add each tags into the value environment
            let constructor_symbols = tags
                .iter()
                .map(|tag| {
                    let declaration = Declaration::UserDefined {
                        source: source.clone(),
                        token: tag.tagname.clone(),
                        scope_name: environment.current_scope_name(),
                    };
                    Ok((
                        &tag.tagname,
                        ConstructorSymbol {
                            declaration,
                            usage_references: Default::default(),
                            enum_name: enum_name.clone(),
                            constructor_name: tag.tagname.representation.clone(),
                            type_variables: type_variable_names.clone(),
                            payload: match &tag.payload {
                                None => None,
                                Some(payload) => {
                                    // validate type annotation
                                    let payload_type_value = type_annotation_to_type(
                                        environment,
                                        payload.type_annotation.as_ref(),
                                    )?;
                                    Some(payload_type_value)
                                }
                            },
                        },
                    ))
                })
                .collect::<Result<Vec<(&Token, ConstructorSymbol)>, UnifyError>>()?;

            environment.step_out_to_parent_scope();

            constructor_symbols
                .into_iter()
                .map(|(token, constructor_symbol)| {
                    environment.insert_constructor_symbol(token, constructor_symbol)
                })
                .collect::<Result<Vec<()>, UnifyError>>()?;

            Ok(())
        }
    }?;

    Ok(())
}

pub fn rewrite_explicit_type_variables_as_implicit(
    type_value: Type,
    explicit_type_variable_names: &HashSet<String>,
) -> Type {
    match type_value {
        Type::ExplicitTypeVariable { name } => {
            if explicit_type_variable_names.get(&name).is_some() {
                Type::ImplicitTypeVariable { name }
            } else {
                Type::ExplicitTypeVariable { name }
            }
        }
        t @ Type::Underscore => t,
        t @ Type::Number => t,
        t @ Type::Null => t,
        t @ Type::Boolean => t,
        t @ Type::String => t,
        Type::ImplicitTypeVariable { name } => Type::ImplicitTypeVariable { name },
        Type::Tuple(types) => Type::Tuple(
            types
                .into_iter()
                .map(|type_value| {
                    rewrite_explicit_type_variables_as_implicit(
                        type_value,
                        explicit_type_variable_names,
                    )
                })
                .collect(),
        ),
        Type::Array(element_type) => {
            Type::Array(Box::new(rewrite_explicit_type_variables_as_implicit(
                *element_type,
                explicit_type_variable_names,
            )))
        }
        Type::Named {
            name,
            type_arguments,
        } => Type::Named {
            name,
            type_arguments: type_arguments
                .into_iter()
                .map(|(name, type_value)| {
                    (
                        name,
                        rewrite_explicit_type_variables_as_implicit(
                            type_value,
                            explicit_type_variable_names,
                        ),
                    )
                })
                .collect(),
        },
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_explicit_type_variables_as_implicit(
                            type_value,
                            explicit_type_variable_names,
                        ),
                    )
                })
                .collect(),
        },
        Type::Function(FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            first_argument_type: Box::new(rewrite_explicit_type_variables_as_implicit(
                *first_argument_type,
                explicit_type_variable_names,
            )),
            rest_arguments_types: rest_arguments_types
                .into_iter()
                .map(|type_value| {
                    rewrite_explicit_type_variables_as_implicit(
                        type_value,
                        explicit_type_variable_names,
                    )
                })
                .collect(),
            return_type: Box::new(rewrite_explicit_type_variables_as_implicit(
                *return_type,
                explicit_type_variable_names,
            )),
        }),
    }
}

pub fn generalize_type(type_value: Type) -> TypeScheme {
    let type_variables = get_free_type_variables_in_type(&type_value);
    TypeScheme {
        type_variables: type_variables.into_iter().collect(),
        type_value,
    }
}

pub fn get_type_annotation_position(type_annotation: &TypeAnnotation) -> Position {
    match type_annotation {
        TypeAnnotation::Underscore(token) => token.position,
        TypeAnnotation::Function {
            start_token,
            return_type,
            ..
        } => join_position(
            start_token.position,
            get_type_annotation_position(return_type.as_ref()),
        ),
        TypeAnnotation::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        TypeAnnotation::Named {
            name,
            type_arguments,
        } => match type_arguments {
            None => join_position(name.position, name.position),
            Some(TypeArguments {
                right_angular_bracket,
                ..
            }) => join_position(name.position, right_angular_bracket.position),
        },
    }
}

pub fn join_position(start_position: Position, end_position: Position) -> Position {
    Position {
        line_start: start_position.line_start,
        column_start: start_position.column_start,
        line_end: end_position.line_end,
        column_end: end_position.column_end,
        character_index_start: start_position.character_index_start,
        character_index_end: end_position.character_index_end,
    }
}

pub fn get_destructure_pattern_position(destructure_pattern: &DestructurePattern) -> Position {
    match destructure_pattern {
        DestructurePattern::Number(token)
        | DestructurePattern::String(token)
        | DestructurePattern::Identifier(token)
        | DestructurePattern::Underscore(token)
        | DestructurePattern::Boolean { token, .. }
        | DestructurePattern::Null(token) => token.position,
        DestructurePattern::Tag { tagname, payload } => match payload {
            Some(payload) => join_position(tagname.position, payload.right_parenthesis.position),
            None => tagname.position,
        },
        DestructurePattern::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        DestructurePattern::Array {
            left_square_bracket,
            right_square_bracket,
            ..
        } => join_position(left_square_bracket.position, right_square_bracket.position),
        DestructurePattern::Tuple {
            left_parenthesis,
            right_parenthesis,
            ..
        } => join_position(left_parenthesis.position, right_parenthesis.position),
    }
}

pub fn get_expression_position(expression_value: &Expression) -> Position {
    match expression_value {
        Expression::Array {
            left_square_bracket,
            right_square_bracket,
            ..
        } => join_position(left_square_bracket.position, right_square_bracket.position),
        Expression::String(token)
        | Expression::Number(token)
        | Expression::Variable(token)
        | Expression::Null(token)
        | Expression::Boolean { token, .. } => token.position,
        Expression::Tag { token, payload } => match payload {
            Some(payload) => join_position(token.position, payload.right_parenthesis.position),
            None => token.position,
        },
        Expression::RecordAccess {
            expression,
            property_name,
        } => join_position(get_expression_position(&expression), property_name.position),
        Expression::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        Expression::FunctionCall(function_call) => {
            let first_argument = function_call.first_argument.as_ref();
            let start_position = get_expression_position(&first_argument);
            let end_position = match &function_call.rest_arguments {
                Some(FunctionCallRestArguments {
                    right_parenthesis, ..
                }) => right_parenthesis.position,
                None => function_call.function_name.position,
            };
            join_position(start_position, end_position)
        }
        Expression::Function(function) => {
            let start_position = function.first_branch.start_token.position;
            let end_position = match function.branches.last() {
                Some(last_branch) => get_expression_position(&last_branch.body),
                None => get_expression_position(&function.first_branch.body),
            };
            join_position(start_position, end_position)
        }
        Expression::Let {
            let_keyword,
            true_branch,
            ..
        } => join_position(let_keyword.position, get_expression_position(&true_branch)),
    }
}

pub fn try_unify_type(
    environment: &mut Environment,
    expected: Option<Type>,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match expected {
        None => Ok(actual.clone()),
        Some(expected) => unify_type(environment, &expected, actual, position),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match unify_type_(environment, expected, actual, position) {
        Err(UnifyError {
            position,
            kind:
                UnifyErrorKind::TypeMismatch {
                    expected_type,
                    actual_type,
                },
        }) => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: environment.apply_subtitution_to_type(&expected_type),
                actual_type: environment.apply_subtitution_to_type(&actual_type),
            },
        }),
        other => other,
    }
}

pub fn unify_type_(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    // { expected, actual }.match(
    //     \{ expected = Number, actual = Number } => Number.Ok
    //     \{ expected = expected.Array, actual = actual.Array} =>
    //        environment.unifyType({expected, actual, position}).match(
    //          \elementType.Ok =>
    //              elementType.Array.Ok
    //          \{ kind = _.TypeMismatch, position = _ }.Error =>
    //              { expected = expected.Array, actual = actual.Array }.TypeMismatch.Error
    //          \error.Error =>
    //              error.Error
    //        )
    // )
    match (expected.clone(), actual.clone()) {
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Null, Type::Null) => Ok(Type::Null),
        (
            Type::ExplicitTypeVariable {
                name: expected_name,
            },
            Type::ExplicitTypeVariable { name: actual_name },
        ) => {
            if expected_name != actual_name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::ExplicitTypeVariable {
                            name: expected_name,
                        },
                        actual_type: Type::ExplicitTypeVariable { name: actual_name },
                    },
                })
            } else {
                Ok(Type::ExplicitTypeVariable {
                    name: expected_name,
                })
            }
        }
        (Type::Array(expected_element_type), Type::Array(actual_element_type)) => {
            match unify_type(
                environment,
                expected_element_type.as_ref(),
                actual_element_type.as_ref(),
                position,
            ) {
                Ok(element_type) => Ok(Type::Array(Box::new(element_type))),
                Err(UnifyError {
                    kind: UnifyErrorKind::TypeMismatch { .. },
                    ..
                }) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Array(expected_element_type),
                        actual_type: Type::Array(actual_element_type),
                    },
                }),
                Err(error) => Err(error),
            }
        }
        (
            Type::Named {
                name: expected_name,
                type_arguments: expected_arguments,
            },
            Type::Named {
                name: actual_name,
                type_arguments: actual_arguments,
            },
        ) => {
            if expected_name != actual_name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Named {
                            name: expected_name,
                            type_arguments: expected_arguments,
                        },
                        actual_type: Type::Named {
                            name: actual_name,
                            type_arguments: actual_arguments,
                        },
                    },
                })
            } else if expected_arguments.len() != actual_arguments.len() {
                panic!("Shoud not be possible, should be compiler bug")
            } else {
                expected_arguments
                    .clone()
                    .into_iter()
                    .zip(actual_arguments.into_iter())
                    .map(
                        |((expected_key, expected_type), (actual_key, actual_type))| {
                            assert_eq!(expected_key, actual_key);
                            unify_type(environment, &expected_type, &actual_type, position)
                        },
                    )
                    .collect::<Result<Vec<Type>, UnifyError>>()?;
                Ok(Type::Named {
                    name: expected_name,
                    type_arguments: expected_arguments,
                })
            }
        }
        (Type::Underscore, other) | (other, Type::Underscore) => Ok(other),
        (
            Type::ImplicitTypeVariable {
                name: expected_type_variable_name,
            },
            Type::ImplicitTypeVariable {
                name: actual_type_variable_name,
            },
        ) => {
            if expected_type_variable_name != actual_type_variable_name {
                let expected_type = environment
                    .get_type_variable_terminal_type(expected_type_variable_name.clone());
                let actual_type =
                    environment.get_type_variable_terminal_type(actual_type_variable_name.clone());

                match (expected_type, actual_type) {
                    (None, Some(actual_type)) => {
                        environment.update_substitution(
                            expected_type_variable_name,
                            actual_type.clone(),
                            position,
                        )?;
                        Ok(actual_type)
                    }
                    (Some(expected_type), None) => {
                        environment.update_substitution(
                            actual_type_variable_name,
                            expected_type.clone(),
                            position,
                        )?;
                        Ok(expected_type)
                    }
                    (Some(expected_type), Some(actual_type)) => {
                        unify_type(environment, &expected_type, &actual_type, position)
                    }
                    (None, None) => {
                        environment.update_substitution(
                            expected_type_variable_name.clone(),
                            Type::ImplicitTypeVariable {
                                name: actual_type_variable_name,
                            },
                            position,
                        )?;
                        Ok(Type::ImplicitTypeVariable {
                            name: expected_type_variable_name,
                        })
                    }
                }
            } else {
                Ok(actual.clone())
            }
        }
        (Type::ImplicitTypeVariable { name }, other_type)
        | (other_type, Type::ImplicitTypeVariable { name }) => {
            if type_variable_occurs_in_type(&name, &other_type) {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::InfiniteTypeDetected,
                })
            } else {
                // This is the magical part that makes type inference works
                environment.update_substitution(name, other_type.clone(), position)?;
                Ok(other_type)
            }
        }
        (Type::Function(expected_function), Type::Function(actual_function)) => {
            let function_type =
                unify_function_type(environment, &expected_function, &actual_function, position)?;
            Ok(Type::Function(function_type))
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
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                });
            }

            // 2. Find for extraneous keys
            let extraneous_keys: Vec<String> = actual_keys
                .filter(|actual_key| !expected_keys.any(|expected_key| expected_key == *actual_key))
                .collect();

            if !extraneous_keys.is_empty() {
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Record {
                            key_type_pairs: expected_key_type_pairs,
                        },
                        actual_type: Type::Record {
                            key_type_pairs: actual_key_type_pairs,
                        },
                    },
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

            let key_type_pairs = zipped
                .map(|((key, expected_type), (_, actual_type))| {
                    match unify_type(environment, &expected_type, &actual_type, position) {
                        Ok(_) => Ok((key, expected_type)),
                        Err(UnifyError {
                            position: _,
                            kind: UnifyErrorKind::TypeMismatch { .. },
                        }) => Err(UnifyError {
                            position,
                            kind: UnifyErrorKind::TypeMismatch {
                                expected_type: Type::Record {
                                    key_type_pairs: expected_key_type_pairs.clone(),
                                },
                                actual_type: Type::Record {
                                    key_type_pairs: actual_key_type_pairs.clone(),
                                },
                            },
                        }),
                        Err(other) => Err(other),
                    }
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        _ => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: expected.clone(),
                actual_type: actual.clone(),
            },
        }),
    }
}

pub fn rewrite_type_variables_in_type(
    from_type_variables: Vec<String>,
    to_types: Vec<Type>,
    in_type: Type,
) -> Type {
    assert!(from_type_variables.len() == to_types.len());
    from_type_variables.iter().zip(to_types.iter()).fold(
        in_type,
        |result_type, (from_type_variable, to_type)| {
            rewrite_type_variable_in_type(from_type_variable, to_type, result_type)
        },
    )
}

pub fn rewrite_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: Type,
) -> Type {
    match in_type {
        Type::Number => Type::Number,
        Type::Boolean => Type::Boolean,
        Type::String => Type::String,
        Type::Null => Type::Null,
        Type::ExplicitTypeVariable { name } => Type::ExplicitTypeVariable { name },
        Type::Array(type_value) => Type::Array(Box::new(rewrite_type_variable_in_type(
            from_type_variable,
            to_type,
            *type_value,
        ))),
        Type::ImplicitTypeVariable { name } => {
            if name == *from_type_variable {
                to_type.clone()
            } else {
                Type::ImplicitTypeVariable { name }
            }
        }
        Type::Named {
            name,
            type_arguments,
        } => Type::Named {
            name,
            type_arguments: type_arguments
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
        Type::Tuple(types) => Type::Tuple(
            types
                .into_iter()
                .map(|type_value| {
                    rewrite_type_variable_in_type(from_type_variable, to_type, type_value)
                })
                .collect(),
        ),
        Type::Underscore => Type::Underscore,
        Type::Function(FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            first_argument_type: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *first_argument_type,
            )),
            rest_arguments_types: rewrite_type_variable_in_types(
                from_type_variable,
                to_type,
                rest_arguments_types,
            ),
            return_type: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *return_type,
            )),
        }),
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
    }
}

pub fn rewrite_type_variable_in_types(
    from_type_variable: &str,
    to_type: &Type,
    in_types: Vec<Type>,
) -> Vec<Type> {
    in_types
        .into_iter()
        .map(|type_value| rewrite_type_variable_in_type(from_type_variable, to_type, type_value))
        .collect()
}

pub struct TagPayloadType {
    left_parenthesis: Token,
    type_value: Type,
    position: Position,
    right_parenthesis: Token,
}

pub fn infer_tag_type(
    environment: &mut Environment,
    tagname: &Token,
    payload: Option<TagPayloadType>,
) -> Result<Type, UnifyError> {
    // Look up constructor
    let constructor = match environment.get_consructor_symbol(SymbolName::Token(tagname.clone())) {
        Some(constructor_symbol) => constructor_symbol,
        None => {
            return Err(UnifyError {
                position: tagname.position,
                kind: UnifyErrorKind::UnknownConstructorSymbol,
            })
        }
    };

    let enum_type = environment
        .get_type_symbol(SymbolName::String(constructor.enum_name.clone()))
        .unwrap_or_else(|| {
            panic!(
                "Compiler error: Cannot find enum '{}' in type_symbols",
                constructor.enum_name
            )
        });

    // initiate type variables
    let instantiated_type_variables = enum_type
        .type_scheme
        .type_variables
        .iter()
        .map(|_| environment.introduce_implicit_type_variable(None))
        .collect::<Result<Vec<Type>, UnifyError>>()?;

    let return_type = rewrite_type_variables_in_type(
        enum_type.type_scheme.type_variables,
        instantiated_type_variables.clone(),
        enum_type.type_scheme.type_value,
    );

    match constructor.payload {
        None => match payload {
            Some(payload) => Err(UnifyError {
                position: join_position(
                    payload.left_parenthesis.position,
                    payload.right_parenthesis.position,
                ),
                kind: UnifyErrorKind::ThisTagDoesNotRequirePayload,
            }),
            None => Ok(return_type),
        },
        Some(expected_payload) => match payload {
            None => Err(UnifyError {
                position: tagname.position,
                kind: UnifyErrorKind::ThisTagRequiresPaylod {
                    payload_type: expected_payload,
                },
            }),
            Some(payload) => {
                let expected_payload = rewrite_type_variables_in_type(
                    constructor.type_variables,
                    instantiated_type_variables,
                    expected_payload,
                );
                unify_type(
                    environment,
                    &expected_payload,
                    &payload.type_value,
                    payload.position,
                )?;
                Ok(return_type)
            }
        },
    }
}
pub struct TypeVariableSubstitution {
    pub from_type_variable: String,
    pub to_type: Type,
}
type TypeVariableSubstitutions = Vec<TypeVariableSubstitution>;

pub fn infer_expression_type(
    environment: &mut Environment,
    expected_type: Option<Type>,
    expression: &Expression,
) -> Result<Type, UnifyError> {
    let result = match expression {
        Expression::Null(_) => try_unify_type(
            environment,
            expected_type,
            &Type::Null,
            get_expression_position(expression),
        ),
        Expression::String(_) => try_unify_type(
            environment,
            expected_type,
            &Type::String,
            get_expression_position(expression),
        ),
        Expression::Number(_) => try_unify_type(
            environment,
            expected_type,
            &Type::Number,
            get_expression_position(expression),
        ),
        Expression::Boolean { .. } => try_unify_type(
            environment,
            expected_type,
            &Type::Boolean,
            get_expression_position(&expression),
        ),
        Expression::Tag { token, payload } => {
            let payload = match payload {
                None => Ok(None),
                Some(payload) => {
                    let type_value = {
                        let expected_payload_type = match expected_type {
                            None => None,
                            _ => panic!("Not implemented yet"),
                        };
                        infer_expression_type(environment, expected_payload_type, &payload.value)?
                    };
                    Ok(Some(TagPayloadType {
                        left_parenthesis: payload.left_parenthesis.clone(),
                        type_value,
                        position: get_expression_position(&payload.value),
                        right_parenthesis: payload.right_parenthesis.clone(),
                    }))
                }
            }?;

            infer_tag_type(environment, token, payload)
        }
        Expression::Variable(variable) => {
            if let Some(symbol) = environment.get_value_symbol(SymbolName::Token(variable.clone()))
            {
                Ok(instantiate_type_scheme(environment, symbol.type_scheme))
            } else {
                Err(UnifyError {
                    position: variable.position,
                    kind: UnifyErrorKind::UnknownValueSymbol,
                })
            }
        }
        Expression::RecordAccess {
            expression,
            property_name,
        } => {
            let expression_type = infer_expression_type(environment, None, expression)?;
            match expression_type {
                Type::Record { key_type_pairs } => {
                    match key_type_pairs
                        .iter()
                        .find(|(key, _)| *key == property_name.representation)
                    {
                        None => Err(UnifyError {
                            position: property_name.position,
                            kind: UnifyErrorKind::NoSuchPropertyOnThisRecord {
                                expected_keys: key_type_pairs
                                    .iter()
                                    .map(|(key, _)| key.clone())
                                    .collect(),
                            },
                        }),
                        Some((_, type_value)) => Ok(type_value.clone()),
                    }
                }
                actual_type => Err(UnifyError {
                    position: property_name.position,
                    kind: UnifyErrorKind::CannotAccessPropertyOfNonRecord { actual_type },
                }),
            }
        }
        Expression::Let {
            left,
            right,
            false_branch,
            true_branch,
            type_annotation,
            let_keyword,
            ..
        } => {
            environment.step_into_new_child_scope();
            let type_annotation_type =
                optional_type_annotation_to_type(environment, type_annotation)?;
            let right_type =
                infer_expression_type(environment, type_annotation_type.clone(), right)?;
            let expression_type = match false_branch {
                None => {
                    let left_type =
                        infer_destructure_pattern(environment, type_annotation_type.clone(), left)?;
                    let left_type = match type_annotation_type {
                        None => Ok(left_type),
                        Some(expected_type) => unify_type(
                            environment,
                            &expected_type,
                            &left_type,
                            get_destructure_pattern_position(left),
                        ),
                    }?;
                    let true_branch_type = infer_expression_type(environment, None, true_branch)?;
                    match left_type {
                        Type::Null => Ok(true_branch_type),
                        _ => {
                            unify_type(
                                environment,
                                &left_type,
                                &true_branch_type,
                                get_expression_position(true_branch),
                            )?;
                            Ok(left_type)
                        }
                    }
                }
                Some(false_branch) => {
                    let function = Function {
                        first_branch: FunctionBranch {
                            start_token: let_keyword.clone(),
                            first_argument: Box::new(FunctionArgument {
                                destructure_pattern: *left.clone(),
                                type_annotation: type_annotation.clone(),
                            }),
                            body: true_branch.clone(),
                            rest_arguments: None,
                            return_type_annotation: None,
                        },
                        branches: vec![false_branch.first_branch.clone()]
                            .into_iter()
                            .chain(false_branch.branches.clone().into_iter())
                            .collect(),
                    };
                    let function_type =
                        infer_function_type(environment, None, &Box::new(function))?;

                    unify_type(
                        environment,
                        &right_type,
                        function_type.first_argument_type.as_ref(),
                        get_destructure_pattern_position(left),
                    )?;
                    Ok(*function_type.return_type)
                }
            }?;
            environment.step_out_to_parent_scope();
            Ok(expression_type)
        }
        Expression::Function(function) => match expected_type {
            None => Ok(Type::Function(infer_function_type(
                environment,
                None,
                function,
            )?)),
            Some(Type::Function(function_type)) => Ok(Type::Function(infer_function_type(
                environment,
                Some(function_type),
                function,
            )?)),
            Some(other_type) => Err(UnifyError {
                position: get_expression_position(&Expression::Function(function.clone())),
                kind: UnifyErrorKind::NotExpectingFunction {
                    expected_type: other_type.clone(),
                },
            }),
        },
        Expression::FunctionCall(function_call) => {
            // Check if expression being invoked is a function
            match environment
                .get_value_symbol(SymbolName::Token(function_call.function_name.clone()))
            {
                None => Err(UnifyError {
                    position: function_call.function_name.position,
                    kind: UnifyErrorKind::UnknownValueSymbol,
                }),
                Some(value_symbol) => {
                    // Substitute explicit type variables
                    // println!(
                    //     "value_symbol_name ={} ; value_symbol_type = {}",
                    //     function_call.function_name.representation,
                    //     stringify_type(value_symbol.type_scheme.type_value.clone(), 0)
                    // );
                    let expected_type = match &function_call.type_arguments {
                        None => instantiate_type_scheme(environment, value_symbol.type_scheme),
                        Some(TypeArguments { substitutions, .. }) => {
                            let remaining_type_variable_names =
                                value_symbol.type_scheme.type_variables;

                            // Substitute all the explicitly declared type variables
                            let (expected_type, remaining_type_variable_names) =
                                substitutions.iter().fold(
                                    Ok((
                                        value_symbol.type_scheme.type_value,
                                        remaining_type_variable_names,
                                    )),
                                    |result, (key, type_annotation)| match result {
                                        Err(error) => Err(error),
                                        Ok((expected_type, remaining_type_variable_names)) => {
                                            if !remaining_type_variable_names
                                                .iter()
                                                .any(|name| *name == key.representation)
                                            {
                                                return Err(UnifyError {
                                                    position: key.position,
                                                    kind:
                                                        UnifyErrorKind::UnknownTypeParameterName {
                                                            expected_names:
                                                                remaining_type_variable_names
                                                                    .to_vec(),
                                                        },
                                                });
                                            }
                                            let type_value = type_annotation_to_type(
                                                environment,
                                                type_annotation,
                                            )?;

                                            let expected_type = rewrite_type_variable_in_type(
                                                &key.representation,
                                                &type_value,
                                                expected_type,
                                            );

                                            Ok((
                                                expected_type,
                                                remaining_type_variable_names
                                                    .iter()
                                                    .filter_map(|name| {
                                                        if *name != key.representation {
                                                            Some(name.clone())
                                                        } else {
                                                            None
                                                        }
                                                    })
                                                    .collect(),
                                            ))
                                        }
                                    },
                                )?;

                            // Instantiate all the remaning type variables that are not substituted
                            remaining_type_variable_names.into_iter().fold(
                                expected_type,
                                |result, remaining_type_variable_name| {
                                    let type_variable = Type::ImplicitTypeVariable {
                                        name: environment.get_next_type_variable_name(),
                                    };
                                    rewrite_type_variable_in_type(
                                        &remaining_type_variable_name,
                                        &type_variable,
                                        result,
                                    )
                                },
                            )
                        }
                    };
                    match expected_type {
                        Type::Function(expected_function_type) => {
                            // println!(
                            //     "expected_function_type = {}",
                            //     stringify_type(Type::Function(expected_function_type.clone()), 0)
                            // );
                            // Tally arguments length
                            {
                                let expected_arguments_length =
                                    expected_function_type.rest_arguments_types.len() + 1;
                                let actual_arguments_length = match &function_call.rest_arguments {
                                    Some(rest_arguments) => rest_arguments.arguments.len() + 1,
                                    None => 1,
                                };
                                if actual_arguments_length != expected_arguments_length {
                                    return Err(UnifyError {
                                        position: get_expression_position(
                                            &Expression::FunctionCall(function_call.clone()),
                                        ),
                                        kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                                            actual_length: actual_arguments_length,
                                            expected_length: expected_arguments_length,
                                        },
                                    });
                                }
                            }

                            let expected_function_first_argument =
                                vec![*expected_function_type.first_argument_type];

                            let expected_function_arguments = expected_function_first_argument
                                .iter()
                                .chain(expected_function_type.rest_arguments_types.iter());

                            let actual_function_first_argument =
                                vec![*function_call.first_argument.clone()];
                            let actual_function_arguments = match &function_call.rest_arguments {
                                Some(rest_arguments) => rest_arguments.arguments.clone(),
                                None => Vec::new(),
                            };
                            let actual_function_arguments = actual_function_first_argument
                                .iter()
                                .chain(actual_function_arguments.iter());

                            // Unify the type of each argument
                            expected_function_arguments
                                .zip(actual_function_arguments)
                                .map(|(expected_argument_type, actual_argument)| {
                                    let expected_argument_type = environment
                                        .apply_subtitution_to_type(expected_argument_type);
                                    // println!(
                                    //     "expected_argument_type {}",
                                    //     stringify_type(expected_argument_type.clone(), 0)
                                    // );
                                    infer_expression_type(
                                        environment,
                                        Some(expected_argument_type.clone()),
                                        &actual_argument,
                                    )
                                })
                                .collect::<Result<Vec<Type>, UnifyError>>()?;

                            Ok(environment.apply_subtitution_to_type(
                                expected_function_type.return_type.as_ref(),
                            ))
                        }
                        Type::ImplicitTypeVariable { name } => {
                            let expected_function_type = Type::ImplicitTypeVariable {
                                name: environment.get_next_type_variable_name(),
                            };

                            let position = get_expression_position(&Expression::FunctionCall(
                                function_call.clone(),
                            ));

                            unify_type(
                                environment,
                                &Type::ImplicitTypeVariable { name },
                                &expected_function_type,
                                position,
                            )?;

                            let return_type = Type::ImplicitTypeVariable {
                                name: environment.get_next_type_variable_name(),
                            };
                            let actual_function_type = Type::Function(FunctionType {
                                first_argument_type: Box::new(infer_expression_type(
                                    environment,
                                    None,
                                    function_call.first_argument.as_ref(),
                                )?),
                                rest_arguments_types: match &function_call.rest_arguments {
                                    None => Vec::new(),
                                    Some(FunctionCallRestArguments { arguments, .. }) => arguments
                                        .clone()
                                        .into_iter()
                                        .map(|argument| {
                                            infer_expression_type(environment, None, &argument)
                                        })
                                        .collect::<Result<Vec<Type>, UnifyError>>()?,
                                },
                                return_type: Box::new(return_type.clone()),
                            });

                            unify_type(
                                environment,
                                &actual_function_type,
                                &expected_function_type,
                                position,
                            )?;

                            Ok(environment.apply_subtitution_to_type(&return_type))
                        }
                        other => Err(UnifyError {
                            position: function_call.function_name.position,
                            kind: UnifyErrorKind::CannotInvokeNonFunction { actual_type: other },
                        }),
                    }
                }
            }
        }
        Expression::Record {
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => {
            // Check for duplicated keys
            let mut key_map = HashSet::new();
            for key in key_value_pairs
                .iter()
                .map(|RecordKeyValue { key, .. }| key.clone())
            {
                if key_map.get(&key.representation).is_some() {
                    return Err(UnifyError {
                        position: key.position,
                        kind: UnifyErrorKind::DuplicatedRecordKey,
                    });
                } else {
                    key_map.insert(key.representation);
                }
            }

            let record_position =
                join_position(left_curly_bracket.position, right_curly_bracket.position);
            match expected_type {
                Some(Type::Record { key_type_pairs }) => infer_record_type(
                    environment,
                    Some(key_type_pairs),
                    key_value_pairs.clone(),
                    record_position,
                ),
                Some(other_type) => {
                    let actual_type = infer_record_type(
                        environment,
                        None,
                        key_value_pairs.clone(),
                        record_position,
                    )?;
                    // println!("other_type = {}", stringify_type(other_type.clone(), 0));
                    // println!("actual_type = {}", stringify_type(actual_type.clone(), 0));
                    unify_type(environment, &other_type, &actual_type, record_position)
                }
                None => {
                    infer_record_type(environment, None, key_value_pairs.clone(), record_position)
                }
            }
        }
        Expression::Array {
            elements,
            left_square_bracket,
            right_square_bracket,
        } => {
            let element_type = match expected_type {
                None => Ok(Type::ImplicitTypeVariable {
                    name: environment.get_next_type_variable_name(),
                }),
                Some(Type::Array(expected_element_type)) => Ok(*expected_element_type),
                Some(Type::ImplicitTypeVariable { name }) => {
                    let type_variable = Type::ImplicitTypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };
                    environment.update_substitution(
                        name,
                        Type::Array(Box::new(type_variable.clone())),
                        join_position(left_square_bracket.position, right_square_bracket.position),
                    )?;
                    Ok(type_variable)
                }
                Some(other_type) => Err(UnifyError {
                    position: join_position(
                        left_square_bracket.position,
                        right_square_bracket.position,
                    ),
                    kind: UnifyErrorKind::NotExpectingArray {
                        expected_type: other_type,
                    },
                }),
            }?;
            for element in elements {
                infer_expression_type(environment, Some(element_type.clone()), &element)?;
            }
            Ok(Type::Array(Box::new(
                environment.apply_subtitution_to_type(&element_type),
            )))
        }
    }?;
    Ok(environment.apply_subtitution_to_type(&result))
}

pub fn infer_record_type(
    environment: &mut Environment,
    expected_key_type_pairs: Option<Vec<(String, Type)>>,
    mut actual_key_value_pairs: Vec<RecordKeyValue>,
    record_position: Position,
) -> Result<Type, UnifyError> {
    match &expected_key_type_pairs {
        None => Ok(()),
        Some(expected_key_type_pairs) => {
            let expected_keys: Vec<String> = expected_key_type_pairs
                .iter()
                .map(|(key, _)| key.clone())
                .collect();
            let actual_keys: Vec<Token> = actual_key_value_pairs
                .iter()
                .map(|key_value_pair| key_value_pair.key.clone())
                .collect();

            // Find extranerous key
            let extraneous_key = actual_keys.iter().find(|actual_key| {
                !expected_keys
                    .iter()
                    .any(|expected_key| expected_key.eq(&actual_key.representation))
            });

            match extraneous_key {
                None => Ok(()),
                Some(extraneous_key) => Err(UnifyError {
                    position: extraneous_key.position,
                    kind: UnifyErrorKind::RecordExtraneousKey {
                        expected_keys: expected_keys.clone(),
                    },
                }),
            }?;

            // Find missing key
            let missing_keys = expected_keys
                .into_iter()
                .filter(|expected_key| {
                    !actual_keys
                        .iter()
                        .any(|actual_key| actual_key.representation.eq(expected_key))
                })
                .collect::<Vec<String>>();

            if missing_keys.is_empty() {
                Ok(())
            } else {
                Err(UnifyError {
                    position: record_position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                })
            }?;

            Ok(())
        }
    }?;

    // If we reach this stage, it means that the keys of expected record and actual record tallied
    // Therefore we can zipped them together, of course,
    // we need to sort the keys first before zipping
    let expected_key_type_pairs = match expected_key_type_pairs {
        None => iter::repeat(None)
            .take(actual_key_value_pairs.len())
            .collect::<Vec<_>>(),
        Some(mut expected_key_type_pairs) => {
            expected_key_type_pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
            expected_key_type_pairs.into_iter().map(Some).collect()
        }
    };

    actual_key_value_pairs.sort_by(|a, b| a.key.representation.cmp(&b.key.representation));

    let key_type_pairs = expected_key_type_pairs
        .iter()
        .zip(actual_key_value_pairs.iter())
        .map(
            |(
                expected_key_type_pair,
                RecordKeyValue {
                    key,
                    value,
                    type_annotation,
                },
            )| {
                let expected_type = get_expected_type(
                    environment,
                    expected_key_type_pair
                        .clone()
                        .map(|(_, type_value)| type_value),
                    type_annotation.clone(),
                )?;
                let value_type = infer_expression_type(environment, expected_type, &value)?;
                Ok((key.representation.clone(), value_type))
            },
        )
        .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;

    Ok(Type::Record { key_type_pairs })
}

pub fn infer_function_type(
    environment: &mut Environment,
    expected_function_type: Option<FunctionType>,
    function: &Function,
) -> Result<FunctionType, UnifyError> {
    let first_function_branch_type = infer_function_branch(
        environment,
        expected_function_type.clone(),
        &function.first_branch,
    )?;

    let function_type = FunctionType {
        first_argument_type: Box::new(environment.introduce_implicit_type_variable(None)?),
        rest_arguments_types: first_function_branch_type
            .rest_arguments_types
            .iter()
            .map(|_| Type::ImplicitTypeVariable {
                name: environment.get_next_type_variable_name(),
            })
            .collect(),
        return_type: Box::new(Type::ImplicitTypeVariable {
            name: environment.get_next_type_variable_name(),
        }),
    };

    unify_function_type(
        environment,
        &function_type,
        &first_function_branch_type,
        get_function_branch_position(&function.first_branch),
    )?;

    // Unify function branches
    for function_branch in &function.branches {
        let actual_function_type =
            infer_function_branch(environment, expected_function_type.clone(), function_branch)?;
        let position = get_function_branch_position(function_branch);
        unify_function_type(environment, &function_type, &actual_function_type, position)?;
    }

    let function_type = environment.apply_subtitution_to_function_type(&function_type);

    // Check for case exhasutiveness
    let (expected_type, actual_patterns) = {
        match function.first_branch.rest_arguments {
            None => {
                let expected_type = function_type.first_argument_type.clone();
                let actual_patterns = vec![function
                    .first_branch
                    .first_argument
                    .destructure_pattern
                    .clone()]
                .into_iter()
                .chain(
                    function
                        .branches
                        .iter()
                        .map(|branch| branch.first_argument.destructure_pattern.clone()),
                )
                .collect();
                (*expected_type, actual_patterns)
            }
            Some(_) => {
                let expected_type = Type::Tuple(
                    vec![*function_type.first_argument_type.clone()]
                        .into_iter()
                        .chain(function_type.rest_arguments_types.clone().into_iter())
                        .collect(),
                );
                let actual_patterns = vec![function.first_branch.clone()]
                    .iter()
                    .chain(function.branches.iter())
                    .map(function_branch_arguments_to_tuple)
                    .collect();

                (expected_type, actual_patterns)
            }
        }
    };
    check_exhasutiveness(
        environment,
        expected_type,
        actual_patterns,
        get_expression_position(&Expression::Function(Box::new(function.clone()))),
    )?;

    Ok(function_type)
}

pub fn function_branch_arguments_to_tuple(function_branch: &FunctionBranch) -> DestructurePattern {
    match &function_branch.rest_arguments {
        None => function_branch.first_argument.destructure_pattern.clone(),
        Some(FunctionBranchRestArguments {
            left_parenthesis,
            rest_arguments,
            right_parenthesis,
        }) => DestructurePattern::Tuple {
            left_parenthesis: left_parenthesis.clone(),
            right_parenthesis: right_parenthesis.clone(),
            values: vec![function_branch.first_argument.destructure_pattern.clone()]
                .into_iter()
                .chain(
                    rest_arguments
                        .clone()
                        .into_iter()
                        .map(|argument| argument.destructure_pattern),
                )
                .collect(),
        },
    }
}

pub fn check_exhasutiveness(
    environment: &Environment,
    expected_type: Type,
    actual_patterns: Vec<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    check_exhasutiveness_(
        environment,
        vec![TypedDestructurePattern::Any {
            type_value: expected_type,
        }],
        actual_patterns,
        position,
    )
}

pub fn check_exhasutiveness_(
    environment: &Environment,
    expected_patterns: Vec<TypedDestructurePattern>,
    actual_patterns: Vec<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    let remaining_expected_patterns = actual_patterns.into_iter().fold(
        Ok(expected_patterns),
        |expected_patterns, actual_pattern| match expected_patterns {
            Err(error) => Err(error),
            Ok(expected_patterns) => {
                if expected_patterns.is_empty() {
                    Err(UnifyError {
                        position: get_destructure_pattern_position(&actual_pattern),
                        kind: UnifyErrorKind::UnreachableCase,
                    })
                } else {
                    Ok(match_patterns(
                        environment,
                        &actual_pattern,
                        expected_patterns,
                    ))
                }
            }
        },
    )?;
    if remaining_expected_patterns.is_empty() {
        Ok(())
    } else {
        Err(UnifyError {
            position,
            kind: UnifyErrorKind::MissingCases(remaining_expected_patterns),
        })
    }
}

pub fn unify_function_type(
    environment: &mut Environment,
    expected_function_type: &FunctionType,
    actual_function_type: &FunctionType,
    position: Position,
) -> Result<FunctionType, UnifyError> {
    // compare arguments length
    if expected_function_type.rest_arguments_types.len()
        != actual_function_type.rest_arguments_types.len()
    {
        return Err(UnifyError {
            position,
            kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                expected_length: expected_function_type.rest_arguments_types.len() + 1,
                actual_length: actual_function_type.rest_arguments_types.len() + 1,
            },
        });
    }

    // unify every argument type
    let zipped = vec![*expected_function_type.first_argument_type.clone()]
        .into_iter()
        .chain(
            expected_function_type
                .clone()
                .rest_arguments_types
                .into_iter(),
        )
        .zip(
            vec![*actual_function_type.first_argument_type.clone()]
                .into_iter()
                .chain(
                    actual_function_type
                        .rest_arguments_types
                        .clone()
                        .into_iter(),
                ),
        );

    let unify_argument_type_result = zipped
        .enumerate()
        .map(|(index, (expected_type, actual_type))| {
            match unify_type(environment, &expected_type, &actual_type, position) {
                Ok(type_value) => Ok(type_value),
                Err(unify_error) => Err((index, unify_error)),
            }
        })
        .collect::<Result<Vec<Type>, (usize, UnifyError)>>();

    match unify_argument_type_result {
        Ok(rest_arguments_types) => {
            // unify return type
            match unify_type(
                environment,
                expected_function_type.return_type.as_ref(),
                actual_function_type.return_type.as_ref(),
                position,
            ) {
                Ok(return_type) => Ok(FunctionType {
                    first_argument_type: Box::new(environment.apply_subtitution_to_type(
                        expected_function_type.first_argument_type.as_ref(),
                    )),
                    rest_arguments_types,
                    return_type: Box::new(return_type),
                }),
                Err(_) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Function(
                            environment.apply_subtitution_to_function_type(&expected_function_type),
                        ),
                        actual_type: Type::Function(
                            environment.apply_subtitution_to_function_type(&actual_function_type),
                        ),
                    },
                }),
            }
        }
        Err(_) => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: Type::Function(expected_function_type.clone()),
                actual_type: Type::Function(actual_function_type.clone()),
            },
        }),
    }
}

pub fn get_function_branch_position(function_branch: &FunctionBranch) -> Position {
    let body_position = get_expression_position(&function_branch.body);
    join_position(function_branch.start_token.position, body_position)
}

pub fn infer_function_branch(
    environment: &mut Environment,
    expected_function_type: Option<FunctionType>,
    function_branch: &FunctionBranch,
) -> Result<FunctionType, UnifyError> {
    environment.step_into_new_child_scope();

    let first_argument_type = Box::new(infer_function_argument(
        environment,
        expected_function_type
            .clone()
            .map(|expected_function_type| *expected_function_type.first_argument_type.clone()),
        function_branch.first_argument.as_ref(),
    )?);

    let rest_arguments_types = {
        match &expected_function_type {
            None => function_branch
                .rest_arguments()
                .iter()
                .map(|argument| Ok(infer_function_argument(environment, None, &argument)?))
                .collect::<Result<Vec<Type>, UnifyError>>(),

            Some(expected_function_type) => {
                let actual_rest_arguments = function_branch.rest_arguments();
                if expected_function_type.rest_arguments_types.len() != actual_rest_arguments.len()
                {
                    Err(UnifyError {
                        position: get_function_branch_position(&function_branch),
                        kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                            expected_length: expected_function_type.rest_arguments_types.len() + 1,
                            actual_length: actual_rest_arguments.len() + 1,
                        },
                    })
                } else {
                    expected_function_type
                        .rest_arguments_types
                        .iter()
                        .zip(actual_rest_arguments.iter())
                        .map(|(expected_argument_type, actual_argument)| {
                            infer_function_argument(
                                environment,
                                Some(expected_argument_type.clone()),
                                actual_argument,
                            )
                        })
                        .collect::<Result<Vec<Type>, UnifyError>>()
                }
            }
        }
    }?;

    let return_type = {
        match &function_branch.return_type_annotation {
            None => expected_function_type
                .map(|expected_function_type| *expected_function_type.return_type.clone()),
            Some(type_annotation) => match expected_function_type {
                None => Some(type_annotation_to_type(environment, &type_annotation)?),
                Some(expected_function_type) => {
                    let actual_return_type =
                        type_annotation_to_type(environment, &type_annotation)?;
                    let return_type = unify_type(
                        environment,
                        expected_function_type.return_type.as_ref(),
                        &actual_return_type,
                        get_type_annotation_position(&type_annotation),
                    )?;
                    Some(return_type)
                }
            },
        }
    };

    let body_type = infer_expression_type(environment, return_type, &function_branch.body)?;

    let result = FunctionType {
        first_argument_type,
        rest_arguments_types,
        return_type: Box::new(environment.apply_subtitution_to_type(&body_type)),
    };
    // let result = match return_type {
    //     Some(return_type) => {
    //         let position = get_expression_position(&function_branch.body);
    //         unify_type(environment, &return_type, &body_type, position)?;
    //         Ok(FunctionType {
    //             first_argument_type,
    //             rest_arguments_types,
    //             return_type: Box::new(environment.apply_subtitution_to_type(&return_type)),
    //         })
    //     }
    //     None => Ok(FunctionType {
    //         first_argument_type,
    //         rest_arguments_types,
    //         return_type: Box::new(body_type),
    //     }),
    // }?;

    // Check for unused variables
    environment.check_for_unused_value_symbols(environment.current_scope_name())?;

    environment.step_out_to_parent_scope();

    // pass back the substitution to parent environment
    Ok(environment.apply_subtitution_to_function_type(&result))
}

pub fn get_expected_type(
    environment: &mut Environment,
    expected_type: Option<Type>,
    expected_type_annotation: Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    let expected_type = match (expected_type, expected_type_annotation) {
        (None, None) => None,
        (Some(expected_type), None) => Some(expected_type),
        (None, Some(type_annotation)) => {
            Some(type_annotation_to_type(environment, &type_annotation)?)
        }
        (Some(expected_type), Some(type_annotation)) => {
            let type_annotation_type = type_annotation_to_type(environment, &type_annotation)?;
            match unify_type(
                environment,
                &expected_type,
                &type_annotation_type,
                get_type_annotation_position(&type_annotation),
            ) {
                Ok(_) => Ok(()),
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch { expected_type, .. },
                }) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::WrongTypeAnnotation { expected_type },
                }),
                Err(other_error) => Err(other_error),
            }?;
            Some(expected_type)
        }
    };
    Ok(expected_type)
}

pub fn infer_function_argument(
    environment: &mut Environment,
    expected_argument_type: Option<Type>,
    function_argument: &FunctionArgument,
) -> Result<Type, UnifyError> {
    let expected_type = get_expected_type(
        environment,
        expected_argument_type,
        function_argument.type_annotation.clone(),
    )?;

    infer_destructure_pattern(
        environment,
        expected_type,
        &function_argument.destructure_pattern,
    )
    // match &function_argument.type_annotation {
    //     Some(type_annotation) => {
    //         let type_annotation_type = {
    //             let type_annotation_type = type_annotation_to_type(environment, type_annotation)?;
    //             match expected_argument_type {
    //                 None => type_annotation_type,
    //                 Some(expected_argument_type) => unify_type(
    //                     environment,
    //                     expected_argument_type,
    //                     &type_annotation_type,
    //                     get_type_annotation_position(type_annotation),
    //                 )?,
    //             }
    //         };

    //         let actual_type =
    //             infer_destructure_pattern(environment, &function_argument.destructure_pattern)?;

    //         unify_type(
    //             environment,
    //             &type_annotation_type,
    //             &actual_type,
    //             get_destructure_pattern_position(&function_argument.destructure_pattern),
    //         )
    //     }
    //     None => {
    //         let actual_argument_type =
    //             infer_destructure_pattern(environment, &function_argument.destructure_pattern)?;

    //         match expected_argument_type {
    //             None => Ok(actual_argument_type),
    //             Some(expected_argument_type) => unify_type(
    //                 environment,
    //                 expected_argument_type,
    //                 &actual_argument_type,
    //                 get_destructure_pattern_position(&function_argument.destructure_pattern),
    //             ),
    //         }
    //     }
    // }
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
    match &type_annotation {
        TypeAnnotation::Underscore(_) => Ok(Type::Underscore),
        TypeAnnotation::Named {
            name,
            type_arguments,
        } => {
            if let Some(symbol) = environment.get_type_symbol(SymbolName::Token(name.clone())) {
                let type_arguments = match type_arguments {
                    None => Ok(vec![]),
                    Some(TypeArguments {
                        substitutions,
                        left_angular_bracket,
                        right_angular_bracket,
                    }) => {
                        if symbol.type_scheme.type_variables.len() != substitutions.len() {
                            Err(UnifyError {
                                position: join_position(
                                    left_angular_bracket.position,
                                    right_angular_bracket.position,
                                ),
                                kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                                    actual_length: substitutions.len(),
                                    expected_type_parameter_names: symbol
                                        .type_scheme
                                        .type_variables
                                        .clone(),
                                },
                            })
                        } else {
                            substitutions
                                .iter()
                                .map(|(name, type_value)| {
                                    Ok((
                                        name.clone(),
                                        type_annotation_to_type(environment, type_value)?,
                                    ))
                                })
                                .collect::<Result<Vec<(Token, Type)>, UnifyError>>()
                        }
                    }
                }?;

                symbol
                        .type_scheme
                        .type_variables
                        .iter()
                        .zip(type_arguments.into_iter())
                        .fold(
                            Ok(symbol.type_scheme.type_value),
                            |result,
                             (
                                expected_type_variable_name,
                                (actual_type_variable_name, type_value),
                            )| {
                                match result {
                                    Err(error) => Err(error),
                                    Ok(result) => {
                                        if *expected_type_variable_name
                                            != actual_type_variable_name.representation
                                        {
                                            Err(UnifyError {
                                                position: actual_type_variable_name.position,
                                                kind: UnifyErrorKind::TypeArgumentNameMismatch {
                                                    expected_name: expected_type_variable_name
                                                        .clone(),
                                                },
                                            })
                                        } else {
                                            Ok(rewrite_type_variable_in_type(
                                                expected_type_variable_name,
                                                &type_value,
                                                result,
                                            ))
                                        }
                                    }
                                }
                            },
                        )
            } else {
                Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::UnknownTypeSymbol,
                })
            }
        }
        TypeAnnotation::Record {
            key_type_annotation_pairs,
            ..
        } => {
            let key_type_pairs = key_type_annotation_pairs
                .iter()
                .map(|(key, type_annotation)| {
                    let type_value = type_annotation_to_type(environment, &type_annotation)?;
                    Ok((key.representation.clone(), type_value))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        TypeAnnotation::Function {
            first_argument_type,
            rest_arguments_types,
            return_type,
            ..
        } => {
            let first_argument_type = Box::new(type_annotation_to_type(
                environment,
                first_argument_type.as_ref(),
            )?);
            let rest_arguments_types = rest_arguments_types
                .iter()
                .map(|argument_type| type_annotation_to_type(environment, argument_type))
                .collect::<Result<Vec<Type>, UnifyError>>()?;

            let return_type = type_annotation_to_type(environment, return_type)?;

            Ok(Type::Function(FunctionType {
                first_argument_type,
                rest_arguments_types,
                return_type: Box::new(return_type),
            }))
        }
    }
}

pub fn infer_destructure_pattern(
    environment: &mut Environment,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
) -> Result<Type, UnifyError> {
    match destructure_pattern {
        DestructurePattern::String(token) => {
            try_unify_type(environment, expected_type, &Type::String, token.position)
        }
        DestructurePattern::Number(token) => {
            try_unify_type(environment, expected_type, &Type::Number, token.position)
        }
        DestructurePattern::Null(token) => {
            try_unify_type(environment, expected_type, &Type::Null, token.position)
        }
        DestructurePattern::Boolean { token, .. } => {
            try_unify_type(environment, expected_type, &Type::Boolean, token.position)
        }
        DestructurePattern::Identifier(identifier) => match expected_type {
            None => environment.introduce_implicit_type_variable(Some(&identifier)),
            Some(expected_type) => {
                environment.insert_value_symbol(
                    identifier,
                    ValueSymbol {
                        declaration: Declaration::UserDefined {
                            source: environment.source.clone(),
                            scope_name: environment.current_scope_name(),
                            token: identifier.clone(),
                        },
                        type_scheme: TypeScheme {
                            type_variables: vec![],
                            type_value: expected_type.clone(),
                        },
                        usage_references: Default::default(),
                    },
                )?;
                Ok(expected_type)
            }
        },
        DestructurePattern::Tuple { values, .. } => Ok(Type::Tuple(
            values
                .iter()
                .map(|destructure_pattern| {
                    // TODO: pass in expected_type
                    infer_destructure_pattern(environment, None, destructure_pattern)
                })
                .collect::<Result<Vec<Type>, UnifyError>>()?,
        )),
        DestructurePattern::Tag { tagname, payload } => {
            let payload = match payload {
                None => Ok(None),
                Some(payload) => {
                    let payload_type =
                        infer_destructure_pattern(environment, None, &payload.destructure_pattern)?;

                    Ok(Some(TagPayloadType {
                        type_value: payload_type,
                        left_parenthesis: payload.left_parenthesis.clone(),
                        right_parenthesis: payload.right_parenthesis.clone(),
                        position: get_destructure_pattern_position(&payload.destructure_pattern),
                    }))
                }
            }?;
            infer_tag_type(environment, tagname, payload)
        }
        DestructurePattern::Array { spread: None, .. } => {
            Ok(Type::Array(Box::new(Type::ImplicitTypeVariable {
                name: environment.get_next_type_variable_name(),
            })))
        }
        DestructurePattern::Array {
            spread: Some(spread),
            left_square_bracket,
            right_square_bracket,
        } => {
            let expected_element_type = match expected_type {
                Some(Type::Array(expected_type)) => Ok(*expected_type.clone()),
                Some(Type::ImplicitTypeVariable { name }) => {
                    let element_type = Type::ImplicitTypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };
                    environment.update_substitution(
                        name,
                        Type::Array(Box::new(element_type.clone())),
                        join_position(left_square_bracket.position, right_square_bracket.position),
                    )?;
                    Ok(element_type)
                }
                Some(other_type) => Err(UnifyError {
                    position: join_position(
                        left_square_bracket.position,
                        right_square_bracket.position,
                    ),
                    kind: UnifyErrorKind::NotExpectingArray {
                        expected_type: other_type.clone(),
                    },
                }),
                None => Ok(Type::ImplicitTypeVariable {
                    name: environment.get_next_type_variable_name(),
                }),
            }?;

            infer_destructure_pattern(
                environment,
                Some(expected_element_type.clone()),
                &spread.left,
            )?;

            let expected_array_type = Type::Array(Box::new(expected_element_type.clone()));
            infer_destructure_pattern(
                environment,
                Some(expected_array_type.clone()),
                &spread.right,
            )?;

            Ok(environment.apply_subtitution_to_type(&expected_array_type))
        }
        DestructurePattern::Record {
            key_value_pairs, ..
        } => {
            let key_type_pairs: Vec<(String, Type)> = key_value_pairs
                .iter()
                .map(|DestructuredRecordKeyValue { key, as_value, .. }| {
                    // TODO: spread
                    // TODO: match against type annotation
                    let name = key.representation.clone();
                    match as_value {
                        Some(destructure_pattern) => {
                            let type_value =
                                // TODO: pass in expected_type
                                infer_destructure_pattern(environment, None, destructure_pattern)?;

                            Ok((name, type_value))
                        }
                        None => {
                            let type_variable =
                                environment.introduce_implicit_type_variable(Some(&key))?;
                            Ok((name, type_variable))
                        }
                    }
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        DestructurePattern::Underscore(_) => Ok(Type::Underscore),
    }
}

fn substitute_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: &Type,
) -> Type {
    match in_type {
        Type::String => Type::String,
        Type::Null => Type::Null,
        Type::Number => Type::Number,
        Type::Boolean => Type::Boolean,
        Type::ExplicitTypeVariable { name } => Type::ExplicitTypeVariable { name: name.clone() },
        Type::Array(type_value) => Type::Array(Box::new(substitute_type_variable_in_type(
            from_type_variable,
            to_type,
            type_value.as_ref(),
        ))),
        Type::ImplicitTypeVariable { name } => {
            if *name == *from_type_variable {
                to_type.clone()
            } else {
                in_type.clone()
            }
        }
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|type_value| {
                    substitute_type_variable_in_type(from_type_variable, to_type, type_value)
                })
                .collect(),
        ),
        Type::Function(FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            first_argument_type: Box::new(substitute_type_variable_in_type(
                from_type_variable,
                to_type,
                first_argument_type.as_ref(),
            )),
            rest_arguments_types: rest_arguments_types
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
        Type::Underscore => Type::Underscore,
        Type::Named {
            name,
            type_arguments: arguments,
        } => Type::Named {
            name: name.to_string(),
            type_arguments: arguments
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        substitute_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
    }
}

fn get_free_type_variables_in_type(type_value: &Type) -> HashSet<String> {
    match type_value {
        Type::Number
        | Type::String
        | Type::Null
        | Type::Boolean
        | Type::Underscore
        | Type::ExplicitTypeVariable { .. } => HashSet::new(),
        Type::Array(type_value) => get_free_type_variables_in_type(type_value.as_ref()),
        Type::Tuple(types) => types
            .iter()
            .flat_map(get_free_type_variables_in_type)
            .collect::<HashSet<String>>(),
        Type::ImplicitTypeVariable { name } => {
            let mut result: HashSet<String> = HashSet::new();
            result.insert(name.clone());
            result
        }
        Type::Function(FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type,
        }) => {
            let mut result: HashSet<String> = HashSet::new();

            result.extend(get_free_type_variables_in_type(
                first_argument_type.as_ref(),
            ));

            let type_variables: Vec<HashSet<String>> = rest_arguments_types
                .iter()
                .map(get_free_type_variables_in_type)
                .collect();
            type_variables
                .into_iter()
                .for_each(|type_variables| result.extend(type_variables));

            result.extend(get_free_type_variables_in_type(return_type.as_ref()));

            result
        }
        Type::Record { key_type_pairs } => key_type_pairs
            .iter()
            .map(|(_, type_value)| get_free_type_variables_in_type(&type_value))
            .fold(HashSet::new(), |result, type_variables| {
                result.into_iter().chain(type_variables).collect()
            }),
        Type::Named {
            type_arguments: arguments,
            ..
        } => arguments
            .iter()
            .map(|(_, type_value)| type_value)
            .flat_map(get_free_type_variables_in_type)
            .collect(),
    }
}

/// To check whether a type variable occur in a type.
/// This is to prevent absurd unification.
/// For example, the unification of A with (A -> B) should not
/// produce the subtituion of {A = A -> B}
fn type_variable_occurs_in_type(type_variable: &str, typ: &Type) -> bool {
    get_free_type_variables_in_type(typ).contains(type_variable)
}

/// For example, before using the type <A, B>(\A => B)
/// We need to instantiate A and B with a implicit type variable that can be substituted,
/// This is necessary to prevent name clashing during unification of two generic types.
///
/// In this case, we can (for example) substitute A as T1 and B as T2,
/// and the resulting type will be (\T1 => T2)
pub fn instantiate_type_scheme(environment: &mut Environment, type_scheme: TypeScheme) -> Type {
    let type_variable_substitutions: TypeVariableSubstitutions = type_scheme
        .type_variables
        .into_iter()
        .map(|from_type_variable| TypeVariableSubstitution {
            from_type_variable,
            to_type: Type::ImplicitTypeVariable {
                name: environment.get_next_type_variable_name(),
            },
        })
        .collect();

    type_variable_substitutions.into_iter().fold(
        type_scheme.type_value,
        |result,
         TypeVariableSubstitution {
             from_type_variable,
             to_type,
         }| { substitute_type_variable_in_type(&from_type_variable, &to_type, &result) },
    )
}
