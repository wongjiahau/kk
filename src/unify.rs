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
pub struct UnifyError {
    pub position: Position,
    pub kind: UnifyErrorKind,
}

#[derive(Debug)]
pub enum UnifyErrorKind {
    MissingCases(Vec<TypedDestructurePattern>),
    ThisTagDoesNotRequirePayload,
    ThisTagRequiresPaylod {
        payload_type: Type,
    },
    UnreachableCase,
    TypeArgumentsLengthMismatch {
        expected_length: usize,
        actual_length: usize,
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
    UnionTypeMismatch {
        expected_union_type: UnionType,
        missing_tags: Vec<TagType>,
        extraneous_tags: Vec<TagType>,
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
    RecordExtraneousKeys {
        extraneous_keys: Vec<String>,
        expected_type: Type,
    },
    RecordMissingKeys {
        missing_keys: Vec<String>,
        expected_type: Type,
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
    // println!("{:#?}", environment);
    match statement {
        Statement::Let {
            left,
            right,
            type_annotation,
        } => {
            let type_annotation_type =
                optional_type_annotation_to_type(environment, &type_annotation)?;
            let right_type = infer_expression_type(environment, &right)?;

            // println!("right_type {:#?}", right_type);

            // 1. Check if right matches type annotation
            let right_type = match type_annotation_type {
                Some(type_annotation_type) => {
                    let position = get_expression_position(&right);
                    unify_type(environment, &type_annotation_type, &right_type, position)?;
                    right_type
                }
                None => right_type,
            };

            // 2. Generalize if possible
            let right_type_scheme = generalize_type(right_type);

            // println!("{} = {:#?}", left.representation, right_type_scheme);

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
        Statement::Type {
            left,
            right,
            type_variables,
        } => {
            let mut current_environment = Environment::new(environment);

            // 1. Populate type variables into current environment
            for type_variable in type_variables.clone() {
                current_environment.insert_type_symbol(
                    &type_variable,
                    TypeSymbol {
                        type_scheme: TypeScheme {
                            type_value: Type::TypeVariable {
                                name: type_variable.clone().representation,
                            },
                            type_variables: vec![],
                        },
                        declaration: Declaration::UserDefined(
                            current_environment.source.clone(),
                            type_variable.clone(),
                        ),
                        usage_references: vec![],
                    },
                )?;
            }

            // 2. verify type declaration
            let type_value = type_annotation_to_type(&mut current_environment, &right)?;

            // 3. Add this symbol into environment
            environment.insert_type_symbol(
                &left,
                TypeSymbol {
                    declaration: Declaration::UserDefined(environment.source.clone(), left.clone()),
                    type_scheme: TypeScheme {
                        type_variables: type_variables
                            .into_iter()
                            .map(|type_variable| type_variable.representation)
                            .collect(),
                        type_value,
                    },
                    usage_references: vec![],
                },
            )
        }
        Statement::Enum {
            name,
            type_variables,
            tags,
        } => {
            // 1. Add this enum into environment first, to allow recursive definition
            let enum_type = Type::Named {
                name: name.representation.clone(),
                arguments: type_variables
                    .clone()
                    .into_iter()
                    .map(|type_variable| Type::TypeVariable {
                        name: type_variable.representation,
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
                    declaration: Declaration::UserDefined(environment.source.clone(), name.clone()),
                    type_scheme: TypeScheme {
                        type_variables: type_variable_names.clone(),
                        type_value: enum_type.clone(),
                    },
                    usage_references: vec![],
                },
            )?;

            let source = environment.source.clone();

            // 2. Populate type variables into current environment
            let mut current_environment = Environment::new(environment);
            for type_variable in type_variables.clone() {
                current_environment.insert_type_symbol(
                    &type_variable,
                    TypeSymbol {
                        type_scheme: TypeScheme {
                            type_value: Type::TypeVariable {
                                name: type_variable.clone().representation,
                            },
                            type_variables: vec![],
                        },
                        declaration: Declaration::UserDefined(
                            source.clone(),
                            type_variable.clone(),
                        ),
                        usage_references: vec![],
                    },
                )?;
            }

            // 3. Add each tags into the value environment
            let value_symbols = tags
                .iter()
                .map(|tag| {
                    let declaration = Declaration::UserDefined(source.clone(), tag.tagname.clone());
                    match &tag.payload {
                        Some(payload) => {
                            // validate type annotation
                            let paylod_type_value = type_annotation_to_type(
                                &mut current_environment,
                                payload.type_annotation.as_ref(),
                            )?;

                            Ok((
                                &tag.tagname,
                                ValueSymbol {
                                    declaration,
                                    usage_references: vec![],
                                    actual_type: TypeScheme {
                                        type_variables: type_variable_names.clone(),
                                        type_value: Type::Function(FunctionType {
                                            first_argument_type: Box::new(paylod_type_value),
                                            rest_arguments_types: vec![],
                                            return_type: Box::new(enum_type.clone()),
                                        }),
                                    },
                                },
                            ))
                        }
                        None => Ok((
                            &tag.tagname,
                            ValueSymbol {
                                declaration,
                                usage_references: vec![],
                                actual_type: TypeScheme {
                                    type_variables: type_variable_names.clone(),
                                    type_value: enum_type.clone(),
                                },
                            },
                        )),
                    }
                })
                .collect::<Result<Vec<(&Token, ValueSymbol)>, UnifyError>>()?;

            value_symbols
                .into_iter()
                .map(|(token, value_symbol)| environment.insert_value_symbol(token, value_symbol))
                .collect::<Result<Vec<()>, UnifyError>>()?;
            Ok(())
        }
    }?;

    // reset type variable index
    // environment.reset_type_variable_index();

    Ok(())
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
        TypeAnnotation::Union { type_annotations } => {
            let first = type_annotations.first();
            let last = type_annotations.last();
            match (first, last) {
                (Some(first), Some(last)) => join_position(
                    get_type_annotation_position(first),
                    get_type_annotation_position(last),
                ),
                (Some(first), None) => {
                    let position = get_type_annotation_position(first);
                    join_position(position, position)
                }
                (None, _) => panic!(
                    "First tag should exists, this is a compiler bug, let's stricten the type"
                ),
            }
        }
        TypeAnnotation::Named { name, arguments } => match arguments {
            None => join_position(name.position, name.position),
            Some(arguments) => {
                join_position(name.position, arguments.right_angular_bracket.position)
            }
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
        DestructurePattern::Tuple(_) => panic!("Compiler error, should not reach this branch"),
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
                None => get_expression_position(function_call.function.as_ref()),
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
        }) => {
            // println!("{:#?}", environment);
            Err(UnifyError {
                position,
                kind: UnifyErrorKind::TypeMismatch {
                    expected_type: environment.apply_subtitution_to_type(&expected_type),
                    actual_type: environment.apply_subtitution_to_type(&actual_type),
                },
            })
        }
        other => other,
    }
}

pub fn unify_type_(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match (expected.clone(), actual.clone()) {
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
        (
            Type::Named {
                name: expected_name,
                arguments: expected_arguments,
            },
            Type::Named {
                name: actual_name,
                arguments: actual_arguments,
            },
        ) => {
            if expected_name != actual_name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Named {
                            name: expected_name,
                            arguments: expected_arguments,
                        },
                        actual_type: Type::Named {
                            name: actual_name,
                            arguments: actual_arguments,
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
                    .map(|(expected_type, actual_type)| {
                        unify_type(environment, &expected_type, &actual_type, position)
                    })
                    .collect::<Result<Vec<Type>, UnifyError>>()?;
                Ok(Type::Named {
                    name: expected_name,
                    arguments: expected_arguments,
                })
            }
        }
        (Type::Underscore, other) | (other, Type::Underscore) => Ok(other),
        (Type::TypeVariable { name: name1 }, Type::TypeVariable { name: name2 }) => {
            if name1 != name2 {
                environment.update_substitution(
                    name1,
                    Type::TypeVariable { name: name2 },
                    position,
                )?;
            }
            Ok(actual.clone())
        }
        (Type::TypeVariable { name }, other_type) | (other_type, Type::TypeVariable { name }) => {
            if type_variable_occurs_in_type(&name, &other_type) {
                panic!("circular type substitution found, left={:#?}, actual_type={:#?}, expected_type={:#?} location={:#?}", 
                       name, other_type, expected, position)
            } else {
                environment.update_substitution(name, other_type.clone(), position)?;
                Ok(other_type)
            }
        }
        (Type::Function(expected_function), Type::Function(actual_function)) => {
            // Make all union types exact
            // This is necessary to catch extraneous or missing cases in pattern matching
            let expected_function =
                update_union_type_in_function_type(expected_function, &UnionTypeBound::Exact);

            let actual_function =
                update_union_type_in_function_type(actual_function, &UnionTypeBound::Exact);

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
                    kind: UnifyErrorKind::RecordMissingKeys {
                        missing_keys,
                        expected_type: Type::Record {
                            key_type_pairs: expected_key_type_pairs,
                        },
                    },
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
                // return Err(UnifyError {
                //     position,
                //     kind: UnifyErrorKind::RecordExtraneousKeys {
                //         extraneous_keys,
                //         expected_type: Type::Record {
                //             key_type_pairs: expected_key_type_pairs,
                //         },
                //     },
                // });
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
                    // TODO: Should pass in more specific location here
                    match unify_type(environment, &expected_type, &actual_type, position) {
                        Ok(_) => Ok((key, expected_type)),
                        Err(UnifyError {
                            position: _,
                            kind: UnifyErrorKind::TypeMismatch { .. },
                        }) => Err(UnifyError {
                            position,
                            kind: UnifyErrorKind::TypeMismatch {
                                expected_type: Type::Record {
                                    key_type_pairs: expected_key_type_pairs.clone()
                                },
                                actual_type: Type::Record{
                                    key_type_pairs: actual_key_type_pairs.clone()
                                }
                                // expected_key_type: expected_type,
                                // actual_key_type: actual_type,
                                // expected_record_type: expected_key_type_pairs.clone(),
                                // actual_record_type: actual_key_type_pairs.clone(),
                            },
                        }),
                        Err(other) => Err(other),
                    }
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        (
            Type::Union(UnionType {
                tags: expected_tags,
                bound: UnionTypeBound::AtLeast,
                catch_all: false,
            }),
            Type::Union(UnionType {
                tags: actual_tags,
                bound: _,
                catch_all: false,
            }),
        ) => Ok(Type::Union(UnionType {
            tags: join_union_tags(environment, expected_tags, actual_tags, position)?,
            bound: UnionTypeBound::AtMost,
            catch_all: false,
        })),
        (
            Type::Union(UnionType {
                tags: expected_tags,
                bound: UnionTypeBound::AtMost,
                catch_all: false,
            }),
            Type::Union(UnionType {
                tags: actual_tags,
                bound: actual_union_type_bound,
                catch_all,
            }),
        ) => {
            let CompareTagsResult {
                extraneous_tags, ..
            } = compare_tags(&expected_tags, &actual_tags);

            if extraneous_tags.is_empty() {
                Ok(Type::Union(UnionType {
                    tags: expected_tags,
                    bound: UnionTypeBound::AtMost,
                    catch_all: false,
                }))
            } else {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Union(UnionType {
                            tags: expected_tags,
                            bound: UnionTypeBound::AtMost,
                            catch_all: false,
                        }),
                        actual_type: Type::Union(UnionType {
                            tags: actual_tags,
                            bound: actual_union_type_bound,
                            catch_all,
                        }),
                    },
                })
            }
        }
        (
            Type::Union(UnionType {
                tags: expected_tags,
                bound: UnionTypeBound::Exact,
                ..
            }),
            Type::Union(UnionType {
                tags: actual_tags,
                bound: _,
                catch_all,
            }),
        ) => {
            let CompareTagsResult {
                missing_tags,
                extraneous_tags,
            } = compare_tags(&expected_tags, &actual_tags);

            if (!catch_all && !missing_tags.is_empty()) || !extraneous_tags.is_empty() {
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::UnionTypeMismatch {
                        expected_union_type: UnionType {
                            tags: expected_tags,
                            bound: UnionTypeBound::Exact,
                            catch_all: false,
                        },
                        missing_tags,
                        extraneous_tags,
                    },
                });
            }

            // Check for payload type
            let mut expected_tags = expected_tags;
            let mut actual_tags = actual_tags;
            expected_tags.sort_by(|a, b| a.tagname.cmp(&b.tagname));
            actual_tags.sort_by(|a, b| a.tagname.cmp(&b.tagname));

            let zipped = expected_tags
                .clone()
                .into_iter()
                .zip(actual_tags.into_iter());
            zipped
                .map(
                    |(expected_tag, actual_tag)| match (expected_tag, actual_tag) {
                        (
                            TagType {
                                payload: Some(expected_payload),
                                ..
                            },
                            TagType {
                                payload: Some(actual_payload),
                                ..
                            },
                        ) => {
                            unify_type(environment, &expected_payload, &actual_payload, position)?;
                            Ok(())
                        }
                        (TagType { payload: None, .. }, TagType { payload: None, .. }) => Ok(()),
                        (expected_tag_type, actual_tag_type) => Err(UnifyError {
                            position,
                            kind: UnifyErrorKind::TagTypeMismatch {
                                expected_tag_type,
                                actual_tag_type,
                            },
                        }),
                    },
                )
                .collect::<Result<Vec<()>, UnifyError>>()?;

            Ok(Type::Union(UnionType {
                tags: expected_tags,
                bound: UnionTypeBound::Exact,
                catch_all: false,
            }))
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

pub fn rewrite_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: Type,
) -> Type {
    match in_type {
        Type::Boolean => Type::Boolean,
        Type::TypeVariable { name } => {
            if name == *from_type_variable {
                to_type.clone()
            } else {
                Type::TypeVariable { name }
            }
        }
        Type::Named { name, arguments } => Type::Named {
            name,
            arguments: rewrite_type_variable_in_types(from_type_variable, to_type, arguments),
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
        Type::Union(UnionType {
            tags,
            bound,
            catch_all,
        }) => Type::Union(UnionType {
            bound,
            catch_all,
            tags: tags
                .into_iter()
                .map(|tag| rewrite_type_variable_in_tag_type(from_type_variable, to_type, tag))
                .collect(),
        }),
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

pub fn rewrite_type_variable_in_tag_type(
    from_type_variable: &str,
    to_type: &Type,
    TagType { tagname, payload }: TagType,
) -> TagType {
    TagType {
        tagname,
        payload: match payload {
            None => None,
            Some(payload) => Some(Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *payload,
            ))),
        },
    }
}

pub struct CompareTagsResult {
    pub missing_tags: Vec<TagType>,
    pub extraneous_tags: Vec<TagType>,
}

pub fn compare_tags(expected_tags: &Vec<TagType>, actual_tags: &Vec<TagType>) -> CompareTagsResult {
    let mut expected_tags_iter = expected_tags.clone().into_iter();
    let mut actual_tags_iter = actual_tags.clone().into_iter();
    let missing_tags = expected_tags
        .clone()
        .into_iter()
        .filter(|expected_tag| {
            !actual_tags_iter.any(|actual_tag| actual_tag.tagname == expected_tag.tagname)
        })
        .collect::<Vec<TagType>>();

    let extraneous_tags = actual_tags
        .clone()
        .into_iter()
        .filter(|actual_tag| {
            !expected_tags_iter.any(|expected_tag| expected_tag.tagname == actual_tag.tagname)
        })
        .collect::<Vec<TagType>>();

    CompareTagsResult {
        missing_tags,
        extraneous_tags,
    }
}

pub fn join_union_tags(
    environment: &mut Environment,
    left: Vec<TagType>,
    right: Vec<TagType>,
    position: Position,
) -> Result<Vec<TagType>, UnifyError> {
    // check for incompatible tags, i.e. tags with the same name but with non-unifiable payload

    let mut right_iter = right.iter();
    // println!("left = {:#?}", left);
    // println!("right = {:#?}", right);
    left.clone()
        .into_iter()
        .map(|left_tag_type| {
            let matching_tag =
                right_iter.find(|right_tag_type| left_tag_type.tagname == right_tag_type.tagname);

            // println!("left_tag_type = {:#?}", left_tag_type);
            // println!("match_tag = {:#?}", matching_tag);
            match matching_tag {
                None => Ok(()),
                Some(matching_tag) => match (&left_tag_type.payload, &matching_tag.payload) {
                    (Some(left_payload), Some(right_payload)) => {
                        unify_type(
                            environment,
                            left_payload.as_ref(),
                            right_payload.as_ref(),
                            position,
                        )?;
                        Ok(())
                    }
                    (None, None) => Ok(()),
                    (_, _) => Err(UnifyError {
                        position,
                        kind: UnifyErrorKind::TagTypeMismatch {
                            expected_tag_type: left_tag_type.clone(),
                            actual_tag_type: matching_tag.clone(),
                        },
                    }),
                },
            }
        })
        .collect::<Result<Vec<()>, UnifyError>>()?;

    Ok(left.into_iter().chain(right.into_iter()).collect())
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
    let constructor = match environment.get_value_symbol(&tagname.representation) {
        Some(value_symbol) => value_symbol,
        None => {
            return Err(UnifyError {
                position: tagname.position,
                kind: UnifyErrorKind::UnknownValueSymbol,
            })
        }
    };

    // initiate type variables
    let type_variables = constructor
        .actual_type
        .type_variables
        .iter()
        .map(|_| environment.get_next_type_variable_name())
        .collect::<Vec<String>>();

    let constructor_type = constructor
        .actual_type
        .type_variables
        .iter()
        .zip(type_variables.iter())
        .fold(
            constructor.actual_type.type_value.clone(),
            |result_type, (from_type_variable, to_type_variable)| {
                rewrite_type_variable_in_type(
                    from_type_variable,
                    &Type::TypeVariable {
                        name: to_type_variable.clone(),
                    },
                    result_type,
                )
            },
        );

    match constructor_type {
        Type::Named { .. } => match payload {
            Some(payload) => Err(UnifyError {
                position: join_position(
                    payload.left_parenthesis.position,
                    payload.right_parenthesis.position,
                ),
                kind: UnifyErrorKind::ThisTagDoesNotRequirePayload,
            }),
            None => Ok(constructor_type),
        },
        Type::Function(function_type) => match payload {
            None => Err(UnifyError {
                position: tagname.position,
                kind: UnifyErrorKind::ThisTagRequiresPaylod {
                    payload_type: *function_type.first_argument_type,
                },
            }),
            Some(payload) => {
                unify_type(
                    environment,
                    function_type.first_argument_type.as_ref(),
                    &payload.type_value,
                    payload.position,
                )?;
                Ok(*function_type.return_type)
            }
        },
        other => panic!("Compiler error, did not expect tag type to be {:#?}", other),
    }
}

pub fn infer_expression_type(
    environment: &mut Environment,
    expression: &Expression,
) -> Result<Type, UnifyError> {
    let result = match expression {
        Expression::Null(_) => Ok(null_type()),
        Expression::Boolean { .. } => Ok(Type::Boolean),
        Expression::String(_) => Ok(string_type()),
        Expression::Number(_) => Ok(number_type()),
        Expression::Tag { token, payload } => {
            let payload = match payload {
                None => Ok(None),
                Some(payload) => {
                    let type_value = infer_expression_type(environment, &payload.value)?;
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
                Err(UnifyError {
                    position: variable.position,
                    kind: UnifyErrorKind::UnknownValueSymbol,
                })
            }
        }
        Expression::Let {
            left,
            right,
            false_branch,
            true_branch,
            type_annotation,
            ..
        } => {
            let left_type = environment.introduce_type_variable(None)?;
            let inferred_left_type = infer_destructure_pattern(environment, left)?;
            let left_location = get_destructure_pattern_position(&left);
            unify_type(environment, &left_type, &inferred_left_type, left_location)?;
            let right_type = infer_expression_type(environment, &right)?;

            match type_annotation {
                None => Ok(()),
                Some(type_annotation) => {
                    let type_annotation = type_annotation_to_type(environment, type_annotation)?;
                    unify_type(
                        environment,
                        &type_annotation,
                        &left_type,
                        get_destructure_pattern_position(&left),
                    )?;
                    Ok(())
                }
            }?;

            let false_branch_type = match false_branch {
                None => {
                    unify_type(
                        environment,
                        &left_type,
                        &right_type,
                        get_expression_position(&right),
                    )?;
                    right_type.clone()
                }
                Some(false_branch) => match infer_expression_type(environment, &false_branch)? {
                    Type::TypeVariable { name } => Ok(Type::TypeVariable { name }),
                    Type::Function(function_type) => {
                        if function_type.rest_arguments_types.is_empty() {
                            unify_type(
                                environment,
                                &left_type,
                                function_type.first_argument_type.as_ref(),
                                get_destructure_pattern_position(&left),
                            )?;

                            unify_type(
                                environment,
                                &update_union_type_in_type(
                                    environment.apply_subtitution_to_type(&left_type),
                                    &UnionTypeBound::Exact,
                                ),
                                &update_union_type_in_type(
                                    right_type.clone(),
                                    &UnionTypeBound::Exact,
                                ),
                                get_expression_position(&right),
                            )?;

                            Ok(*function_type.return_type)
                        } else {
                            return Err(UnifyError {
                                position: get_expression_position(&false_branch),
                                kind: UnifyErrorKind::LetElseMustBeSingleArgumentFunction {
                                    actual_type: Type::Function(function_type),
                                },
                            });
                        }
                    }
                    other => Err(UnifyError {
                        position: get_expression_position(&false_branch),
                        kind: UnifyErrorKind::LetElseMustBeFunction { actual_type: other },
                    }),
                }?,
            };
            let true_branch_type = infer_expression_type(environment, &true_branch)?;

            // println!(
            //     "left_type = {:#?}",
            //     environment.apply_subtitution_to_type(&left_type)
            // );
            // println!(
            //     "right_type = {:#?}",
            //     environment.apply_subtitution_to_type(&right_type.clone())
            // );
            // println!(
            //     "true_branch_type = {:#?}",
            //     environment.apply_subtitution_to_type(&true_branch_type)
            // );
            //

            println!("right_type = {:#?}", right_type);
            println!("left_type = {:#?}", left_type);

            match **left {
                DestructurePattern::Identifier(_) | DestructurePattern::Underscore(_) => {
                    if let Some(false_branch) = false_branch {
                        return Err(UnifyError {
                            position: get_expression_position(false_branch.as_ref()),
                            kind: UnifyErrorKind::UnreachableCase,
                        });
                    }
                }
                _ => {
                    unify_type(
                        environment,
                        &false_branch_type,
                        &true_branch_type,
                        get_expression_position(&true_branch),
                    )?;
                }
            };

            Ok(environment.apply_subtitution_to_type(&false_branch_type))
        }
        Expression::Function(function) => {
            let first_function_branch_type =
                infer_function_branch(environment, &function.first_branch)?;

            let function_type = FunctionType {
                first_argument_type: Box::new(environment.introduce_type_variable(None)?),
                rest_arguments_types: first_function_branch_type
                    .rest_arguments_types
                    .iter()
                    .map(|_| Type::TypeVariable {
                        name: environment.get_next_type_variable_name(),
                    })
                    .collect(),
                return_type: Box::new(Type::TypeVariable {
                    name: environment.get_next_type_variable_name(),
                }),
            };

            // println!("function_type = {:#?}", function_type);

            unify_function_type(
                environment,
                &function_type,
                &first_function_branch_type,
                get_function_branch_position(&function.first_branch),
            )?;

            // Unify function branches
            for function_branch in &function.branches {
                let actual_function_type = infer_function_branch(environment, function_branch)?;
                let position = get_function_branch_position(function_branch);
                unify_function_type(environment, &function_type, &actual_function_type, position)?;
            }

            let function_type = environment.apply_subtitution_to_function_type(&function_type);

            // Check for case exhasutiveness
            let expected_type = Type::Tuple(
                vec![*function_type.clone().first_argument_type]
                    .into_iter()
                    .chain(function_type.rest_arguments_types.clone().into_iter())
                    .collect(),
            );
            let actual_patterns = vec![function_branch_arguments_to_tuple(&function.first_branch)]
                .into_iter()
                .chain(
                    function
                        .branches
                        .iter()
                        .map(function_branch_arguments_to_tuple),
                )
                .collect();
            check_exhasutiveness(
                expected_type,
                actual_patterns,
                get_expression_position(&Expression::Function(function.clone())),
            )?;

            // Close all unbounded union types
            Ok(update_union_type_in_type(
                Type::Function(function_type),
                &UnionTypeBound::AtMost,
            ))
        }
        Expression::FunctionCall(function_call) => {
            // Check if expression being invoked is a function
            match infer_expression_type(environment, &function_call.function)? {
                Type::Function(expected_function_type) => {
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
                                position: get_expression_position(&Expression::FunctionCall(
                                    function_call.clone(),
                                )),
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
                            let actual_argument_type =
                                infer_expression_type(environment, &actual_argument)?;
                            unify_type(
                                environment,
                                &expected_argument_type,
                                &actual_argument_type,
                                get_expression_position(&actual_argument),
                            )
                        })
                        .collect::<Result<Vec<Type>, UnifyError>>()?;

                    Ok(environment
                        .apply_subtitution_to_type(expected_function_type.return_type.as_ref()))
                }
                Type::TypeVariable { name } => {
                    let expected_function_type = Type::TypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };

                    let position =
                        get_expression_position(&Expression::FunctionCall(function_call.clone()));

                    unify_type(
                        environment,
                        &Type::TypeVariable { name },
                        &expected_function_type,
                        position,
                    )?;

                    let return_type = Type::TypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };
                    let actual_function_type = Type::Function(FunctionType {
                        first_argument_type: Box::new(infer_expression_type(
                            environment,
                            function_call.first_argument.as_ref(),
                        )?),
                        rest_arguments_types: match &function_call.rest_arguments {
                            None => Vec::new(),
                            Some(FunctionCallRestArguments { arguments, .. }) => arguments
                                .clone()
                                .into_iter()
                                .map(|argument| infer_expression_type(environment, &argument))
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
                    position: get_expression_position(&function_call.function),
                    kind: UnifyErrorKind::CannotInvokeNonFunction { actual_type: other },
                }),
            }
        }
        Expression::Record {
            key_value_pairs, ..
        } => {
            let key_type_pairs = key_value_pairs
                .iter()
                .map(
                    |RecordKeyValue {
                         key,
                         value,
                         type_annotation,
                     }| {
                        let value_type = infer_expression_type(environment, &value)?;
                        match type_annotation {
                            Some(type_annotation) => {
                                let type_annotation =
                                    type_annotation_to_type(environment, type_annotation)?;
                                unify_type(
                                    environment,
                                    &type_annotation,
                                    &value_type,
                                    get_expression_position(&value),
                                )?;
                                Ok(())
                            }
                            None => Ok(()),
                        }?;
                        Ok((key.representation.clone(), value_type))
                    },
                )
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;

            Ok(Type::Record { key_type_pairs })
        }
        Expression::Array { elements, .. } => {
            let element_type = Type::TypeVariable {
                name: environment.get_next_type_variable_name(),
            };
            for element in elements {
                let actual_element_type = &infer_expression_type(environment, &element)?;
                unify_type(
                    environment,
                    &element_type,
                    actual_element_type,
                    get_expression_position(&element),
                )?;
            }
            Ok(array_type(
                environment.apply_subtitution_to_type(&element_type),
            ))
        }
    }?;
    Ok(environment.apply_subtitution_to_type(&result))
}

pub fn function_branch_arguments_to_tuple(function_branch: &FunctionBranch) -> DestructurePattern {
    DestructurePattern::Tuple(
        vec![function_branch.first_argument.destructure_pattern.clone()]
            .into_iter()
            .chain(
                function_branch
                    .rest_arguments
                    .clone()
                    .into_iter()
                    .map(|argument| argument.destructure_pattern),
            )
            .collect(),
    )
}

#[derive(Debug, Clone)]
pub enum TypedDestructurePattern {
    Any {
        type_value: Type,
    },
    Enum {
        tagname: String,
        payload: Option<Box<TypedDestructurePattern>>,
    },
    Tuple(Vec<TypedDestructurePattern>),
    Boolean(bool), // Number
}

pub fn check_exhasutiveness(
    expected_type: Type,
    actual_patterns: Vec<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    check_exhasutiveness_(
        vec![TypedDestructurePattern::Any {
            type_value: expected_type,
        }],
        actual_patterns,
        position,
    )
}

pub fn check_exhasutiveness_(
    expected_patterns: Vec<TypedDestructurePattern>,
    actual_patterns: Vec<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    let mut expected_patterns = expected_patterns;
    for actual_pattern in actual_patterns {
        if expected_patterns.is_empty() {
            return Err(UnifyError {
                position: get_destructure_pattern_position(&actual_pattern),
                kind: UnifyErrorKind::UnreachableCase,
            });
        }
        expected_patterns = match_patterns(&actual_pattern, expected_patterns)
    }
    if expected_patterns.is_empty() {
        Ok(())
    } else {
        Err(UnifyError {
            position,
            kind: UnifyErrorKind::MissingCases(expected_patterns),
        })
        // Err(UnifyError {

        // })
    }
}

pub fn match_patterns(
    actual_pattern: &DestructurePattern,
    expected_patterns: Vec<TypedDestructurePattern>,
) -> Vec<TypedDestructurePattern> {
    expected_patterns
        .into_iter()
        .flat_map(
            |expected_pattern| match match_pattern(&actual_pattern, &expected_pattern) {
                MatchPatternResult::Matched => vec![],
                MatchPatternResult::NotMatched => vec![expected_pattern],
                MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                    match_patterns(actual_pattern, expanded_patterns)
                }
            },
        )
        .collect()
}

#[derive(Debug)]
pub enum MatchPatternResult {
    Matched,
    NotMatched,
    PartiallyMatched {
        expanded_patterns: Vec<TypedDestructurePattern>,
    },
}

pub fn match_pattern(
    actual_pattern: &DestructurePattern,
    expected_pattern: &TypedDestructurePattern,
) -> MatchPatternResult {
    match (actual_pattern, expected_pattern) {
        (DestructurePattern::Underscore(_), _) => MatchPatternResult::Matched,
        (DestructurePattern::Identifier(_), _) => MatchPatternResult::Matched,
        (
            DestructurePattern::Boolean { value: true, .. },
            TypedDestructurePattern::Boolean(true),
        ) => MatchPatternResult::Matched,
        (
            DestructurePattern::Boolean { value: false, .. },
            TypedDestructurePattern::Boolean(false),
        ) => MatchPatternResult::Matched,
        (
            DestructurePattern::Tag {
                tagname: actual_tagname,
                payload: None,
            },
            TypedDestructurePattern::Enum {
                tagname: expected_tagname,
                payload: None,
            },
        ) => {
            if actual_tagname.representation != *expected_tagname {
                MatchPatternResult::NotMatched
            } else {
                MatchPatternResult::Matched
            }
        }
        (
            DestructurePattern::Tag {
                tagname: actual_tagname,
                payload: Some(actual_payload),
            },
            TypedDestructurePattern::Enum {
                tagname: expected_tagname,
                payload: Some(expected_payload),
            },
        ) => {
            if actual_tagname.representation != *expected_tagname {
                MatchPatternResult::NotMatched
            } else {
                match match_pattern(&actual_payload.destructure_pattern, expected_payload) {
                    MatchPatternResult::Matched => MatchPatternResult::Matched,
                    MatchPatternResult::NotMatched => MatchPatternResult::NotMatched,
                    MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                        MatchPatternResult::PartiallyMatched {
                            expanded_patterns: expanded_patterns
                                .into_iter()
                                .map(|pattern| TypedDestructurePattern::Enum {
                                    tagname: expected_tagname.clone(),
                                    payload: Some(Box::new(pattern)),
                                })
                                .collect(),
                        }
                    }
                }
            }
        }
        (
            DestructurePattern::Tuple(actual_patterns),
            TypedDestructurePattern::Tuple(expected_patterns),
        ) => {
            if actual_patterns.len() != expected_patterns.len() {
                MatchPatternResult::NotMatched
            } else {
                let result = actual_patterns
                    .iter()
                    .zip(expected_patterns.iter())
                    .map(|(actual_pattern, expected_pattern)| {
                        (
                            expected_pattern,
                            match_pattern(actual_pattern, expected_pattern),
                        )
                    })
                    .collect::<Vec<(&TypedDestructurePattern, MatchPatternResult)>>();

                if result.iter().all(|(_, match_pattern_result)| {
                    matches!(match_pattern_result, MatchPatternResult::Matched)
                }) {
                    return MatchPatternResult::Matched;
                }

                if result.iter().any(|(_, match_pattern_result)| {
                    matches!(match_pattern_result, MatchPatternResult::NotMatched)
                }) {
                    return MatchPatternResult::NotMatched;
                }

                MatchPatternResult::PartiallyMatched {
                    expanded_patterns: expand_pattern_tuple(
                        result
                            .into_iter()
                            .map(|(expected_pattern, match_pattern_result)| {
                                match match_pattern_result {
                                    MatchPatternResult::Matched => vec![expected_pattern.clone()],
                                    MatchPatternResult::NotMatched => {
                                        panic!("Compiler error, should not reach this branch")
                                    }
                                    MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                                        expanded_patterns
                                    }
                                }
                            })
                            .collect(),
                    )
                    .into_iter()
                    .map(TypedDestructurePattern::Tuple)
                    .collect(),
                }
                // expand_pattern_tuple(result)
                // println!("result = {:#?}", result);
                // println!("actual_patterns = {:#?}", actual_patterns);
                // panic!("expected_patterns = {:#?}", expected_patterns)
            }
        }
        (_, TypedDestructurePattern::Any { type_value }) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: expand_pattern(type_value),
        },
        _ => MatchPatternResult::NotMatched,
    }
}

// pub fn match_tuple_pattern(patterns: Vec<(DestructurePattern, TypedDestructurePattern)>) -> Vec<TypedDestructurePattern> {
//     patterns.into_iter().flat_map()
// }

pub fn expand_pattern(type_value: &Type) -> Vec<TypedDestructurePattern> {
    match type_value {
        Type::Boolean => vec![
            TypedDestructurePattern::Boolean(true),
            TypedDestructurePattern::Boolean(false),
        ],
        Type::Tuple(types) => vec![TypedDestructurePattern::Tuple(
            types
                .iter()
                .map(|type_value| TypedDestructurePattern::Any {
                    type_value: type_value.clone(),
                })
                .collect(),
        )],
        _ => vec![],
    }
}

/// Example
/// Input = [[0, 1], [0, 1]]
/// Output = [[0, 0], [0, 1], [1, 0], [1, 1]]
pub fn expand_pattern_tuple(
    types: Vec<Vec<TypedDestructurePattern>>,
) -> Vec<Vec<TypedDestructurePattern>> {
    match types.split_first() {
        None => vec![],
        Some((head, tail)) => head
            .iter()
            .flat_map(|typed_destructure_pattern| {
                let tail = expand_pattern_tuple(tail.to_vec());
                if tail.is_empty() {
                    vec![vec![typed_destructure_pattern.clone()]]
                } else {
                    tail.into_iter()
                        .map(|patterns| {
                            vec![typed_destructure_pattern.clone()]
                                .into_iter()
                                .chain(patterns.into_iter())
                                .collect()
                        })
                        .collect::<Vec<Vec<TypedDestructurePattern>>>()
                }
            })
            .collect::<Vec<Vec<TypedDestructurePattern>>>(),
    }
}

pub fn update_union_type_in_function_type(
    FunctionType {
        first_argument_type,
        rest_arguments_types,
        return_type,
    }: FunctionType,
    bound: &UnionTypeBound,
) -> FunctionType {
    FunctionType {
        first_argument_type: Box::new(update_union_type_in_type(*first_argument_type, bound)),
        rest_arguments_types: rest_arguments_types
            .into_iter()
            .map(|argument_type| update_union_type_in_type(argument_type, bound))
            .collect(),
        return_type: Box::new(update_union_type_in_type(*return_type, bound)),
    }
}

pub fn update_union_type_in_type(type_value: Type, bound: &UnionTypeBound) -> Type {
    match type_value {
        Type::Underscore => Type::Underscore,
        Type::Union(UnionType { tags, .. }) => Type::Union(UnionType {
            tags,
            bound: bound.clone(),
            catch_all: false,
        }),
        Type::Function(function_type) => {
            Type::Function(update_union_type_in_function_type(function_type, bound))
        }
        Type::TypeVariable { name } => Type::TypeVariable { name },
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| (key, update_union_type_in_type(type_value, bound)))
                .collect(),
        },
        Type::Named { name, arguments } => Type::Named {
            name,
            arguments: arguments
                .into_iter()
                .map(|argument| update_union_type_in_type(argument, bound))
                .collect(),
        },
        _ => panic!(),
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
    parent_environment: &mut Environment,
    function_branch: &FunctionBranch,
) -> Result<FunctionType, UnifyError> {
    // Initialize new environment for this function branch
    let mut environment = Environment::new(parent_environment);

    let first_argument_type = Box::new(infer_function_argument(
        &mut environment,
        function_branch.first_argument.as_ref(),
    )?);

    let rest_arguments_types = function_branch
        .rest_arguments
        .iter()
        .map(|argument| Ok(infer_function_argument(&mut environment, &argument)?))
        .collect::<Result<Vec<Type>, UnifyError>>()?;

    let body_type = infer_expression_type(&mut environment, &function_branch.body)?;

    let return_type = optional_type_annotation_to_type(
        &mut environment,
        &function_branch.return_type_annotation,
    )?;

    let result = match return_type {
        Some(return_type) => {
            let position = get_expression_position(&function_branch.body);
            unify_type(&mut environment, &return_type, &body_type, position)?;
            Ok(FunctionType {
                first_argument_type,
                rest_arguments_types,
                return_type: Box::new(environment.apply_subtitution_to_type(&return_type)),
            })
        }
        None => Ok(FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type: Box::new(body_type),
        }),
    }?;

    // pass back the substitution to parent environment
    Ok(environment.apply_subtitution_to_function_type(&result))
}

pub fn infer_function_argument(
    environment: &mut Environment,
    function_argument: &FunctionArgument,
) -> Result<Type, UnifyError> {
    let type_annotation_type =
        optional_type_annotation_to_type(environment, &function_argument.type_annotation)?;

    match type_annotation_type {
        Some(type_annotation_type) => {
            let actual_type =
                infer_destructure_pattern(environment, &function_argument.destructure_pattern)?;

            Ok(unify_type(
                environment,
                &type_annotation_type,
                &actual_type,
                get_destructure_pattern_position(&function_argument.destructure_pattern),
            )?)
        }
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
    match &type_annotation {
        TypeAnnotation::Underscore(_) => Ok(Type::Underscore),
        TypeAnnotation::Named { name, arguments } => {
            if let Ok(symbol) = environment.get_type_symbol(&name) {
                let arguments = match arguments {
                    None => vec![],
                    Some(NamedTypeAnnotationArguments { arguments, .. }) => arguments
                        .iter()
                        .map(|argument| type_annotation_to_type(environment, argument))
                        .collect::<Result<Vec<Type>, UnifyError>>()?,
                };

                if symbol.type_scheme.type_variables.len() != arguments.len() {
                    Err(UnifyError {
                        position: get_type_annotation_position(type_annotation),
                        kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                            expected_length: symbol.type_scheme.type_variables.len(),
                            actual_length: arguments.len(),
                        },
                    })
                } else {
                    let result = symbol
                        .type_scheme
                        .type_variables
                        .iter()
                        .zip(arguments.into_iter())
                        .fold(
                            symbol.type_scheme.type_value,
                            |result, (type_variable_name, argument)| {
                                rewrite_type_variable_in_type(type_variable_name, &argument, result)
                            },
                        );
                    Ok(result)
                }
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
        TypeAnnotation::Union { type_annotations } => {
            // Make sure that only all the types are either a union or tag
            let tag_types = type_annotations.iter().fold(
                Ok(vec![]),
                |result, type_annotation| match result {
                    Ok(current_tags) => {
                        match type_annotation_to_type(environment, type_annotation)? {
                            Type::Union(UnionType { tags, .. }) => join_union_tags(
                                environment,
                                current_tags,
                                tags,
                                get_type_annotation_position(type_annotation),
                            ),
                            other => Err(UnifyError {
                                position: get_type_annotation_position(type_annotation),
                                kind: UnifyErrorKind::CannotUnionNonTagType {
                                    the_non_union_type: other,
                                },
                            }),
                        }
                    }
                    Err(unify_error) => Err(unify_error),
                },
            )?;

            // TODO: check for duplicated tags

            Ok(Type::Union(UnionType {
                tags: tag_types,
                bound: UnionTypeBound::AtMost,
                catch_all: false,
            }))
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
    destructure_pattern: &DestructurePattern,
) -> Result<Type, UnifyError> {
    match destructure_pattern {
        DestructurePattern::String(_) => Ok(string_type()),
        DestructurePattern::Number(_) => Ok(number_type()),
        DestructurePattern::Boolean { .. } => Ok(Type::Boolean),
        DestructurePattern::Null(_) => Ok(null_type()),
        DestructurePattern::Identifier(identifier) => {
            environment.introduce_type_variable(Some(&identifier))
        }
        DestructurePattern::Tuple(_) => panic!("Compiler error, should not reach here"),
        DestructurePattern::Tag { tagname, payload } => {
            let payload = match payload {
                None => Ok(None),
                Some(payload) => {
                    let payload_type =
                        infer_destructure_pattern(environment, &payload.destructure_pattern)?;

                    Ok(Some(TagPayloadType {
                        type_value: payload_type,
                        left_parenthesis: payload.left_parenthesis.clone(),
                        right_parenthesis: payload.right_parenthesis.clone(),
                        position: get_destructure_pattern_position(&payload.destructure_pattern),
                    }))
                }
            }?;
            infer_tag_type(environment, tagname, payload)

            // let tag_type = TagType {
            //     tagname: token.representation.clone(),
            //     payload: match payload {
            //         None => None,
            //         Some(payload) => Some(Box::new(infer_destructure_pattern(
            //             environment,
            //             &payload.destructure_pattern,
            //         )?)),
            //     },
            // };
            // Ok(Type::Union(UnionType {
            //     tags: vec![tag_type],
            //     bound: UnionTypeBound::AtLeast,
            //     catch_all: false,
            // }))
        }
        DestructurePattern::Array {
            initial_elements,
            tail_elements,
            spread,
            ..
        } => {
            let element_type = Type::TypeVariable {
                name: environment.get_next_type_variable_name(),
            };
            let all_elements = initial_elements
                .iter()
                .chain(tail_elements.iter())
                .collect::<Vec<&DestructurePattern>>();

            for destructure_pattern in all_elements {
                let inferred_element_type =
                    &infer_destructure_pattern(environment, &destructure_pattern)?;
                unify_type(
                    environment,
                    &element_type,
                    inferred_element_type,
                    get_destructure_pattern_position(destructure_pattern),
                )?;
            }
            let result_type = array_type(environment.apply_subtitution_to_type(&element_type));
            match spread {
                Some(DestructurePatternArraySpread {
                    binding: Some(binding),
                    ..
                }) => environment.insert_value_symbol(
                    binding,
                    ValueSymbol {
                        declaration: Declaration::UserDefined(
                            environment.source.clone(),
                            binding.clone(),
                        ),
                        actual_type: TypeScheme {
                            type_value: result_type.clone(),
                            type_variables: vec![],
                        },
                        usage_references: vec![],
                    },
                ),
                _ => Ok(()),
            }?;
            Ok(result_type)
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
        DestructurePattern::Underscore(_) => Ok(Type::Underscore),
    }
}

fn substitute_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: &Type,
) -> Type {
    match in_type {
        Type::Boolean => Type::Boolean,
        Type::TypeVariable { name } => {
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
        Type::Named { name, arguments } => Type::Named {
            name: name.to_string(),
            arguments: arguments
                .iter()
                .map(|argument| {
                    substitute_type_variable_in_type(from_type_variable, to_type, argument)
                })
                .collect(),
        },
        Type::Union(UnionType {
            bound,
            catch_all,
            tags,
        }) => Type::Union(UnionType {
            bound: bound.clone(),
            catch_all: *catch_all,
            tags: tags
                .iter()
                .map(|TagType { tagname, payload }| match payload {
                    None => TagType {
                        tagname: tagname.to_string(),
                        payload: None,
                    },
                    Some(payload) => TagType {
                        tagname: tagname.to_string(),
                        payload: Some(Box::new(substitute_type_variable_in_type(
                            from_type_variable,
                            to_type,
                            payload.as_ref(),
                        ))),
                    },
                })
                .collect(),
        }), // _ => in_type.clone(),
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
        Type::Boolean | Type::Underscore => HashSet::new(),
        Type::Tuple(types) => types
            .iter()
            .flat_map(get_free_type_variables_in_type)
            .collect::<HashSet<String>>(),
        Type::TypeVariable { name } => {
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
        Type::Union(UnionType { tags, .. }) => tags
            .iter()
            .flat_map(|tag_type| match &tag_type.payload {
                Some(payload) => get_free_type_variables_in_type(&payload),
                None => HashSet::new(),
            })
            .collect(),
        Type::Named { arguments, .. } => arguments
            .iter()
            .flat_map(get_free_type_variables_in_type)
            .collect(),
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
