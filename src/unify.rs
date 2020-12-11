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
    TypeArgumentsLengthMismatch {
        expected_length: usize,
        actual_length: usize,
        location: Location,
    },
    LetElseMustBeSingleArgumentFunction {
        location: Location,
        actual_type: Type,
    },
    LetElseMustBeFunction {
        location: Location,
        actual_type: Type,
    },
    FunctionTypeArgumentMismatch {
        argument_index: usize,
        unify_error: Box<UnifyError>,
        expected_function_type: FunctionType,
        actual_function_type: FunctionType,
        location: Location,
    },
    FunctionReturnTypeMismatch {
        unify_error: Box<UnifyError>,
        expected_function_type: FunctionType,
        actual_function_type: FunctionType,
        location: Location,
    },
    UnionTypeMismatch {
        expected_union_type: UnionType,
        missing_tags: Vec<TagType>,
        extraneous_tags: Vec<TagType>,
        location: Location,
    },
    CannotUnionNonTagType {
        the_non_union_type: Type,
        location: Location,
    },
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
    TagTypeMismatch {
        location: Location,
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
                    let location = get_expression_location(environment, &right);
                    unify_type(environment, &type_annotation_type, &right_type, location)?;
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
        Statement::TypeAlias {
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
                            environment.source.clone(),
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

pub fn get_type_annotation_location(
    environment: &Environment,
    type_annotation: &TypeAnnotation,
) -> Location {
    let position = get_type_annotation_position(type_annotation);
    Location {
        source: environment.source.clone(),
        position,
    }
}

pub fn get_type_annotation_position(type_annotation: &TypeAnnotation) -> Position {
    match type_annotation {
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
        TypeAnnotation::Tag { token, payload } => match payload {
            Some(payload) => join_position(token.position, payload.right_parenthesis.position),
            None => {
                let position = token.position;
                join_position(position, position)
            }
        },
        other => panic!("{:#?}", other),
    }
}

pub fn join_position(start_position: Position, end_position: Position) -> Position {
    Position {
        line_start: start_position.line_start,
        column_start: start_position.column_start,
        line_end: end_position.line_end,
        column_end: end_position.column_end,
    }
}

pub fn get_destructure_pattern_location(
    environment: &Environment,
    destructure_pattern: &DestructurePattern,
) -> Location {
    let position = get_destructure_pattern_position(destructure_pattern);
    Location {
        source: environment.source.clone(),
        position,
    }
}

pub fn get_destructure_pattern_position(destructure_pattern: &DestructurePattern) -> Position {
    match destructure_pattern {
        DestructurePattern::Number(token)
        | DestructurePattern::String(token)
        | DestructurePattern::Identifier(token)
        | DestructurePattern::Underscore(token) => token.position,
        DestructurePattern::Tag { token, payload } => match payload {
            Some(payload) => join_position(token.position, payload.right_parenthesis.position),
            None => token.position,
        },
        DestructurePattern::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
    }
}

pub fn get_expression_location(
    environment: &Environment,
    expression_value: &Expression,
) -> Location {
    let position = get_expression_position(expression_value);
    Location {
        source: environment.source.clone(),
        position,
    }
}

pub fn get_expression_position(expression_value: &Expression) -> Position {
    match expression_value {
        Expression::String(token) | Expression::Number(token) | Expression::Variable(token) => {
            token.position
        }
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
            let first_argument = function_call.arguments.first().unwrap();
            let last_argument = function_call.arguments.last().unwrap();
            let start_position = get_expression_position(&first_argument);
            let end_position = get_expression_position(&last_argument);
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
        other => panic!("{:#?}", other),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    location: Location,
) -> Result<Type, UnifyError> {
    match unify_type_(environment, expected, actual, location) {
        Err(UnifyError::TypeMismatch {
            location,
            expected_type,
            actual_type,
        }) => {
            // println!("{:#?}", environment);
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
) -> Result<Type, UnifyError> {
    match (expected.clone(), actual.clone()) {
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
                Err(UnifyError::TypeMismatch {
                    location,
                    expected_type: Type::Named {
                        name: expected_name,
                        arguments: expected_arguments,
                    },
                    actual_type: Type::Named {
                        name: actual_name,
                        arguments: actual_arguments,
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
                        unify_type(environment, &expected_type, &actual_type, location.clone())
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
                    location,
                )?;
            }
            Ok(actual.clone())
        }
        (Type::TypeVariable { name }, other_type) | (other_type, Type::TypeVariable { name }) => {
            if type_variable_occurs_in_type(&name, &other_type) {
                panic!("circular type substitution found, left={:#?}, actual_type={:#?}, expected_type={:#?} location={:#?}", 
                       name, other_type, expected, location)
            } else {
                environment.update_substitution(name, other_type.clone(), location)?;
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
                unify_function_type(environment, &expected_function, &actual_function, location)?;
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

            let key_type_pairs = zipped
                .map(|((key, expected_type), (_, actual_type))| {
                    // TODO: Should pass in more specific location here
                    match unify_type(environment, &expected_type, &actual_type, location.clone()) {
                        Ok(_) => Ok((key, expected_type)),
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
            tags: join_union_tags(environment, expected_tags, actual_tags, location)?,
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
                Err(UnifyError::TypeMismatch {
                    location,
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
                return Err(UnifyError::UnionTypeMismatch {
                    expected_union_type: UnionType {
                        tags: expected_tags,
                        bound: UnionTypeBound::Exact,
                        catch_all: false,
                    },
                    missing_tags,
                    extraneous_tags,
                    location,
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
            let unify_error = zipped
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
                            unify_type(
                                environment,
                                &expected_payload,
                                &actual_payload,
                                location.clone(),
                            )?;
                            Ok(())
                        }
                        (TagType { payload: None, .. }, TagType { payload: None, .. }) => Ok(()),
                        (_, _) => panic!("mismatch, one expected payload, the other dont have"),
                    },
                )
                .collect::<Result<Vec<()>, UnifyError>>();

            match unify_error {
                Ok(_) => Ok(Type::Union(UnionType {
                    tags: expected_tags,
                    bound: UnionTypeBound::Exact,
                    catch_all: false,
                })),
                Err(_) => panic!("Some tag payload type not matched"),
            }
        }
        _ => Err(UnifyError::TypeMismatch {
            location,
            expected_type: expected.clone(),
            actual_type: actual.clone(),
        }),
    }
}

pub fn rewrite_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: Type,
) -> Type {
    match in_type {
        Type::TypeVariable { name } => {
            if name == *from_type_variable {
                to_type.clone()
            } else {
                Type::TypeVariable { name }
            }
        }
        Type::Named { name, arguments } => Type::Named {
            name,
            arguments: arguments
                .into_iter()
                .map(|argument| {
                    rewrite_type_variable_in_type(from_type_variable, to_type, argument)
                })
                .collect(),
        },
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
        other => panic!("{:#?}", other),
    }
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
    location: Location,
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
                            location.clone(),
                        )?;
                        Ok(())
                    }
                    (_, _) => Err(UnifyError::TagTypeMismatch {
                        location: location.clone(),
                        expected_tag_type: left_tag_type.clone(),
                        actual_tag_type: matching_tag.clone(),
                    }),
                },
            }
        })
        .collect::<Result<Vec<()>, UnifyError>>()?;

    Ok(left.into_iter().chain(right.into_iter()).collect())
}

pub fn infer_expression_type(
    environment: &mut Environment,
    expression: &Expression,
) -> Result<Type, UnifyError> {
    let result = match expression {
        Expression::String(_) => Ok(string_type()),
        Expression::Number(_) => Ok(number_type()),
        Expression::Tag { token, payload } => {
            let tag_type = TagType {
                tagname: token.representation.clone(),
                payload: match payload {
                    Some(payload) => Some(Box::new(infer_expression_type(
                        environment,
                        &payload.value,
                    )?)),
                    None => None,
                },
            };
            Ok(Type::Union(UnionType {
                tags: vec![tag_type],
                bound: UnionTypeBound::AtLeast,
                catch_all: false,
            }))
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
                Err(UnifyError::UnknownValueSymbol {
                    location: Location {
                        source: environment.source.clone(),
                        position: variable.position,
                    },
                })
            }
        }
        Expression::Let {
            left,
            right,
            false_branch,
            true_branch,
            ..
        } => {
            let left_type = infer_destructure_pattern(environment, left)?;
            let right_type = infer_expression_type(environment, &right)?;

            unify_type(
                environment,
                &left_type,
                &right_type,
                get_destructure_pattern_location(environment, &left),
            )?;
            let false_branch_type = match false_branch {
                None => right_type,
                Some(false_branch) => match infer_expression_type(environment, &false_branch)? {
                    Type::TypeVariable { name } => Ok(Type::TypeVariable { name }),
                    Type::Function(function_type) => {
                        if function_type.arguments_types.get(0).is_some() {
                            if function_type.arguments_types.len() != 1 {
                                return Err(UnifyError::LetElseMustBeSingleArgumentFunction {
                                    actual_type: Type::Function(function_type),
                                    location: get_expression_location(environment, &false_branch),
                                });
                            } else {
                                let return_type = Type::TypeVariable {
                                    name: environment.get_next_type_variable_name(),
                                };
                                let expected_function_type = FunctionType {
                                    arguments_types: vec![right_type],
                                    return_type: Box::new(return_type.clone()),
                                };
                                unify_function_type(
                                    environment,
                                    &expected_function_type,
                                    &function_type,
                                    get_expression_location(environment, &false_branch),
                                )?;
                                Ok(environment.apply_subtitution_to_type(&return_type))
                            }
                        } else {
                            return Err(UnifyError::LetElseMustBeSingleArgumentFunction {
                                actual_type: Type::Function(function_type),
                                location: get_expression_location(environment, &false_branch),
                            });
                        }
                    }
                    other => Err(UnifyError::LetElseMustBeFunction {
                        actual_type: other,
                        location: get_expression_location(environment, &false_branch),
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
            unify_type(
                environment,
                &false_branch_type,
                &true_branch_type,
                get_expression_location(environment, &true_branch),
            )?;

            Ok(environment.apply_subtitution_to_type(&false_branch_type))
        }
        Expression::Function(function) => {
            let first_function_branch_type =
                infer_function_branch(environment, &function.first_branch)?;

            let function_type = FunctionType {
                arguments_types: first_function_branch_type
                    .arguments_types
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
                Location {
                    source: environment.source.clone(),
                    position: get_function_branch_position(&function.first_branch),
                },
            )?;

            // Unify function branches
            for function_branch in &function.branches {
                let actual_function_type = infer_function_branch(environment, function_branch)?;
                let position = get_function_branch_position(function_branch);
                unify_function_type(
                    environment,
                    &function_type,
                    &actual_function_type,
                    Location {
                        source: environment.source.clone(),
                        position,
                    },
                )?;
            }

            let function_type =
                Type::Function(environment.apply_subtitution_to_function_type(&function_type));

            // Close all unbounded union types
            Ok(update_union_type_in_type(
                function_type,
                &UnionTypeBound::AtMost,
            ))
        }
        Expression::FunctionCall(function_call) => {
            // Check if expression being invoked is a function
            match infer_expression_type(environment, &function_call.function)? {
                Type::Function(expected_function_type) => {
                    // Tally arguments length
                    if function_call.arguments.len() != expected_function_type.arguments_types.len()
                    {
                        return Err(UnifyError::InvalidFunctionArgumentLength {
                            location: get_expression_location(
                                environment,
                                &Expression::FunctionCall(function_call.clone()),
                            ),
                            actual_length: function_call.arguments.len(),
                            expected_length: expected_function_type.arguments_types.len(),
                        });
                    }

                    // Unify the type of each argument
                    expected_function_type
                        .arguments_types
                        .into_iter()
                        .zip(function_call.arguments.clone().into_iter())
                        .map(|(expected_argument_type, actual_argument)| {
                            let actual_argument_type =
                                infer_expression_type(environment, &actual_argument)?;
                            unify_type(
                                environment,
                                &expected_argument_type,
                                &actual_argument_type,
                                get_expression_location(environment, &actual_argument),
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

                    let location = get_expression_location(
                        environment,
                        &Expression::FunctionCall(function_call.clone()),
                    );

                    unify_type(
                        environment,
                        &Type::TypeVariable { name },
                        &expected_function_type,
                        location.clone(),
                    )?;

                    let return_type = Type::TypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };
                    let actual_function_type = Type::Function(FunctionType {
                        arguments_types: function_call
                            .arguments
                            .clone()
                            .into_iter()
                            .map(|argument| infer_expression_type(environment, &argument))
                            .collect::<Result<Vec<Type>, UnifyError>>()?,
                        return_type: Box::new(return_type.clone()),
                    });

                    unify_type(
                        environment,
                        &actual_function_type,
                        &expected_function_type,
                        location,
                    )?;

                    Ok(environment.apply_subtitution_to_type(&return_type))
                }
                other => Err(UnifyError::CannotInvokeNonFunction {
                    location: get_expression_location(environment, &function_call.function),
                    actual_type: other,
                }),
            }
        }
        Expression::Record {
            key_value_pairs, ..
        } => {
            let key_type_pairs = key_value_pairs
                .iter()
                .map(|RecordKeyValue { key, value, .. }| {
                    let value_type = infer_expression_type(environment, &value)?;
                    Ok((key.representation.clone(), value_type))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;

            Ok(Type::Record { key_type_pairs })
        }
        Expression::Array(elements) => {
            let element_type = Type::TypeVariable {
                name: environment.get_next_type_variable_name(),
            };
            for element in elements {
                let actual_element_type = &infer_expression_type(environment, &element)?;
                unify_type(
                    environment,
                    &element_type,
                    actual_element_type,
                    get_expression_location(environment, &element),
                )?;
            }
            Ok(environment.apply_subtitution_to_type(&element_type))
        }
    }?;
    Ok(environment.apply_subtitution_to_type(&result))
}

pub fn update_union_type_in_function_type(
    FunctionType {
        arguments_types,
        return_type,
    }: FunctionType,
    bound: &UnionTypeBound,
) -> FunctionType {
    FunctionType {
        arguments_types: arguments_types
            .into_iter()
            .map(|argument_type| update_union_type_in_type(argument_type, bound))
            .collect(),
        return_type: Box::new(update_union_type_in_type(*return_type, bound)),
    }
}

pub fn update_union_type_in_type(type_value: Type, bound: &UnionTypeBound) -> Type {
    match type_value {
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
        other => panic!("{:#?}", other),
    }
}

pub fn unify_function_type(
    environment: &mut Environment,
    expected_function_type: &FunctionType,
    actual_function_type: &FunctionType,
    location: Location,
) -> Result<FunctionType, UnifyError> {
    // compare arguments length
    if expected_function_type.arguments_types.len() != actual_function_type.arguments_types.len() {
        return Err(UnifyError::InvalidFunctionArgumentLength {
            location,
            expected_length: expected_function_type.arguments_types.len(),
            actual_length: actual_function_type.arguments_types.len(),
        });
    }

    // unify every argument type
    let zipped = expected_function_type
        .clone()
        .arguments_types
        .into_iter()
        .zip(actual_function_type.arguments_types.clone().into_iter());

    let unify_argument_type_result = zipped
        .enumerate()
        .map(|(index, (expected_type, actual_type))| {
            match unify_type(environment, &expected_type, &actual_type, location.clone()) {
                Ok(type_value) => Ok(type_value),
                Err(unify_error) => Err((index, unify_error)),
            }
        })
        .collect::<Result<Vec<Type>, (usize, UnifyError)>>();

    match unify_argument_type_result {
        Ok(arguments_types) => {
            // unify return type
            match unify_type(
                environment,
                expected_function_type.return_type.as_ref(),
                actual_function_type.return_type.as_ref(),
                location.clone(),
            ) {
                Ok(return_type) => Ok(FunctionType {
                    arguments_types,
                    return_type: Box::new(return_type),
                }),
                Err(unify_error) => Err(UnifyError::FunctionReturnTypeMismatch {
                    unify_error: Box::new(unify_error),
                    location,
                    expected_function_type: environment
                        .apply_subtitution_to_function_type(expected_function_type),
                    actual_function_type: environment
                        .apply_subtitution_to_function_type(actual_function_type),
                }),
            }
        }
        Err((argument_index, unify_error)) => Err(UnifyError::FunctionTypeArgumentMismatch {
            argument_index,
            unify_error: Box::new(unify_error),
            location,
            expected_function_type: environment
                .apply_subtitution_to_function_type(expected_function_type),
            actual_function_type: environment
                .apply_subtitution_to_function_type(actual_function_type),
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
    let mut environment = Environment::new(&parent_environment);

    let arguments_types = function_branch
        .arguments
        .iter()
        .map(|argument| Ok(unify_function_argument(&mut environment, &argument)?))
        .collect::<Result<Vec<Type>, UnifyError>>()?;

    let body_type = infer_expression_type(&mut environment, &function_branch.body)?;

    let return_type = optional_type_annotation_to_type(
        &mut environment,
        &function_branch.return_type_annotation,
    )?;

    let result = match return_type {
        Some(return_type) => {
            let location = get_expression_location(&environment, &function_branch.body);
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

    // pass back the substitution to parent environment

    Ok(environment.apply_subtitution_to_function_type(&result))
}

pub fn unify_function_argument(
    environment: &mut Environment,
    function_argument: &FunctionArgument,
) -> Result<Type, UnifyError> {
    let type_annotation_type =
        optional_type_annotation_to_type(environment, &function_argument.type_annotation)?;

    match type_annotation_type {
        Some(type_annotation_type) => {
            // let type_value = match type_annotation_type {
            //     Type::Compound {name, arguments} => {
            //         let name = environment.get_next_type_variable_name();
            //         Type::Compound {
            //             name,
            //             arguments: arguments.into_iter().map(|argument| {

            //             })
            //         }
            //     },
            //     other => other
            // };
            // instantiate type variable
            let actual_type =
                infer_destructure_pattern(environment, &function_argument.destructure_pattern)?;

            Ok(unify_type(
                environment,
                &type_annotation_type,
                &actual_type,
                get_destructure_pattern_location(
                    environment,
                    &function_argument.destructure_pattern,
                ),
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
        TypeAnnotation::Named { name, arguments } => {
            if let Ok(symbol) = environment.get_type_symbol(&name) {
                let arguments = arguments
                    .iter()
                    .map(|argument| type_annotation_to_type(environment, argument))
                    .collect::<Result<Vec<Type>, UnifyError>>()?;

                if symbol.type_scheme.type_variables.len() != arguments.len() {
                    Err(UnifyError::TypeArgumentsLengthMismatch {
                        location: get_type_annotation_location(environment, type_annotation),
                        expected_length: symbol.type_scheme.type_variables.len(),
                        actual_length: arguments.len(),
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
                Err(UnifyError::UnknownTypeSymbol {
                    location: Location {
                        source: environment.source.clone(),
                        position: name.position,
                    },
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
        TypeAnnotation::Tag { token, payload } => {
            let payload = match payload {
                Some(payload) => Some(Box::new(type_annotation_to_type(
                    environment,
                    payload.payload.as_ref(),
                )?)),
                None => None,
            };
            Ok(Type::Union(UnionType {
                tags: vec![TagType {
                    tagname: token.representation.clone(),
                    payload,
                }],
                bound: UnionTypeBound::AtMost,
                catch_all: false,
            }))
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
                                get_type_annotation_location(environment, type_annotation),
                            ),
                            other => Err(UnifyError::CannotUnionNonTagType {
                                the_non_union_type: other,
                                location: get_type_annotation_location(
                                    environment,
                                    type_annotation,
                                ),
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
            arguments_types,
            return_type,
            ..
        } => {
            let arguments_types = arguments_types
                .iter()
                .map(|argument_type| type_annotation_to_type(environment, argument_type))
                .collect::<Result<Vec<Type>, UnifyError>>()?;

            let return_type = type_annotation_to_type(environment, return_type)?;

            Ok(Type::Function(FunctionType {
                arguments_types,
                return_type: Box::new(return_type),
            }))
        }
        other => panic!("{:#?}", other),
    }
}

pub fn infer_destructure_pattern(
    environment: &mut Environment,
    destructure_pattern: &DestructurePattern,
) -> Result<Type, UnifyError> {
    match destructure_pattern {
        DestructurePattern::String(_) => Ok(string_type()),
        DestructurePattern::Number(_) => Ok(number_type()),
        DestructurePattern::Identifier(identifier) => {
            environment.introduce_type_variable(Some(&identifier))
        }
        DestructurePattern::Tag { token, payload } => {
            let tag_type = TagType {
                tagname: token.representation.clone(),
                payload: match payload {
                    None => None,
                    Some(payload) => Some(Box::new(infer_destructure_pattern(
                        environment,
                        &payload.destructure_pattern,
                    )?)),
                },
            };
            Ok(Type::Union(UnionType {
                tags: vec![tag_type],
                bound: UnionTypeBound::AtLeast,
                catch_all: false,
            }))
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
        Type::Underscore => Type::Underscore,
        Type::Named { name, arguments } => Type::Named {
            name:name.to_string(),
            arguments: arguments
                .iter()
                .map(|argument| {
                    substitute_type_variable_in_type(from_type_variable, to_type, argument)
                })
                .collect(),
        },
        Type::Union(_) => panic!(), // _ => in_type.clone(),
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
