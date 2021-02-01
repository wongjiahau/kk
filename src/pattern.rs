use crate::ast::*;
use crate::environment::*;
use crate::unify::rewrite_type_variables_in_type;
use std::collections::HashSet;

#[derive(Debug)]
pub enum MatchPatternResult {
    Matched,
    NotMatched,
    PartiallyMatched {
        expanded_patterns: Vec<TypedDestructurePattern>,
    },
}

enum PatternMatrix {
    Tuple {
        pattern_pairs: Vec<PatternPair>,
    },
    Record {
        key_pattern_pairs: Vec<(/*key*/ String, PatternPair)>,
    },
    Array {
        left: Box<PatternPair>,
        right: Box<PatternPair>,
    },
}

struct PatternPair {
    expected_pattern: TypedDestructurePattern,
    actual_pattern: DestructurePattern,
}

fn match_pattern_matrix(
    environment: &Environment,
    pattern_matrix: PatternMatrix,
) -> MatchPatternResult {
    enum WrapResultAs {
        Tuple,
        Record,
        Array,
    }

    struct RawPatternMatrix {
        key: String,
        actual_pattern: DestructurePattern,
        expected_pattern: TypedDestructurePattern,
    }

    fn match_raw_pattern_matrix(
        environment: &Environment,
        pattern_matrices: Vec<RawPatternMatrix>,
        wrap_result_as: WrapResultAs,
    ) -> MatchPatternResult {
        let pattern_matrices = pattern_matrices
            .iter()
            .map(
                |RawPatternMatrix {
                     key,
                     actual_pattern,
                     expected_pattern,
                 }| {
                    (
                        (key.clone(), expected_pattern.clone()),
                        match_pattern(environment, actual_pattern, expected_pattern),
                    )
                },
            )
            .collect::<Vec<((String, TypedDestructurePattern), MatchPatternResult)>>();
        if pattern_matrices.iter().all(|(_, match_pattern_result)| {
            matches!(match_pattern_result, MatchPatternResult::Matched)
        }) {
            return MatchPatternResult::Matched;
        }

        if pattern_matrices.iter().any(|(_, match_pattern_result)| {
            matches!(match_pattern_result, MatchPatternResult::NotMatched)
        }) {
            return MatchPatternResult::NotMatched;
        }

        MatchPatternResult::PartiallyMatched {
            expanded_patterns: expand_pattern_matrix(
                pattern_matrices
                    .into_iter()
                    .map(|((key, expected_pattern), match_pattern_result)| {
                        match match_pattern_result {
                            MatchPatternResult::Matched => {
                                vec![(key, expected_pattern)]
                            }
                            MatchPatternResult::NotMatched => {
                                panic!("Compiler error, should not reach this branch")
                            }
                            MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                                expanded_patterns
                                    .into_iter()
                                    .map(|expanded_pattern| (key.clone(), expanded_pattern))
                                    .collect()
                            }
                        }
                    })
                    .collect(),
            )
            .into_iter()
            .map(|key_pattern_pairs| match wrap_result_as {
                WrapResultAs::Record => TypedDestructurePattern::Record { key_pattern_pairs },
                WrapResultAs::Tuple => TypedDestructurePattern::Tuple(
                    key_pattern_pairs
                        .into_iter()
                        .map(|(_, pattern)| pattern)
                        .collect(),
                ),
                WrapResultAs::Array => TypedDestructurePattern::NonEmptyArray {
                    first_element: Box::new(
                        key_pattern_pairs.get(0).expect("Compiler error").1.clone(),
                    ),
                    rest_elements: Box::new(
                        key_pattern_pairs.get(1).expect("Compiler error").1.clone(),
                    ),
                },
            })
            .collect(),
        }
    }

    match pattern_matrix {
        PatternMatrix::Tuple { pattern_pairs } => {
            let pattern_matrices = pattern_pairs
                .into_iter()
                .map(
                    |PatternPair {
                         actual_pattern,
                         expected_pattern,
                     }| {
                        RawPatternMatrix {
                            key: "".to_string(),
                            actual_pattern,
                            expected_pattern,
                        }
                    },
                )
                .collect();
            match_raw_pattern_matrix(environment, pattern_matrices, WrapResultAs::Tuple)
        }
        PatternMatrix::Record { key_pattern_pairs } => {
            let pattern_matrices = key_pattern_pairs
                .into_iter()
                .map(
                    |(
                        key,
                        PatternPair {
                            actual_pattern,
                            expected_pattern,
                        },
                    )| {
                        RawPatternMatrix {
                            key,
                            actual_pattern,
                            expected_pattern,
                        }
                    },
                )
                .collect();
            match_raw_pattern_matrix(environment, pattern_matrices, WrapResultAs::Record)
        }
        PatternMatrix::Array { left, right } => {
            let pattern_matrices = vec![
                RawPatternMatrix {
                    key: "left".to_string(),
                    actual_pattern: left.actual_pattern,
                    expected_pattern: left.expected_pattern,
                },
                RawPatternMatrix {
                    key: "right".to_string(),
                    actual_pattern: right.actual_pattern,
                    expected_pattern: right.expected_pattern,
                },
            ];
            match_raw_pattern_matrix(environment, pattern_matrices, WrapResultAs::Array)
        }
    }
}

pub fn expand_pattern(
    environment: &Environment,
    type_value: &Type,
) -> Vec<TypedDestructurePattern> {
    match type_value {
        Type::Boolean => vec![
            TypedDestructurePattern::Boolean(true),
            TypedDestructurePattern::Boolean(false),
        ],
        Type::Integer | Type::Character | Type::String => vec![TypedDestructurePattern::Infinite {
            handled_cases: vec![],
        }],
        Type::Tuple(types) => vec![TypedDestructurePattern::Tuple(
            types
                .clone()
                .map(|type_value| TypedDestructurePattern::Any { type_value })
                .into_vector(),
        )],
        Type::Record { key_type_pairs } => vec![TypedDestructurePattern::Record {
            key_pattern_pairs: key_type_pairs
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        TypedDestructurePattern::Any {
                            type_value: type_value.clone(),
                        },
                    )
                })
                .collect(),
        }],
        Type::Named {
            name,
            type_arguments,
        } => environment
            .get_enum_constructors(name)
            .into_iter()
            .map(|constructor_symbol| {
                let constructor_name = constructor_symbol.constructor_name.clone();
                match &constructor_symbol.payload {
                    None => TypedDestructurePattern::EnumConstructor {
                        name: constructor_name,
                        payload: None,
                    },
                    Some(payload) => {
                        let payload =
                            rewrite_type_variables_in_type(type_arguments.clone(), payload.clone());
                        TypedDestructurePattern::EnumConstructor {
                            name: constructor_name,
                            payload: Some(Box::new(TypedDestructurePattern::Any {
                                type_value: payload,
                            })),
                        }
                    }
                }
            })
            .collect(),
        _ => vec![],
    }
}

/// Permutate possible patterns.
///
/// Example
/// ```
/// Input = [[0, 1], [A, B, C]]
/// Output = [
///     [0, A], [0, B], [0, C],
///     [1, A], [1, B], [1, C],
/// ]
/// ```
pub fn expand_pattern_matrix(
    types: Vec<
        Vec<(
            /*key*/ String,
            /*pattern*/ TypedDestructurePattern,
        )>,
    >,
) -> Vec<
    Vec<(
        /*key*/ String,
        /*pattern*/ TypedDestructurePattern,
    )>,
> {
    match types.split_first() {
        None => vec![],
        Some((head, tail)) => head
            .iter()
            .flat_map(|(key, typed_destructure_pattern)| {
                let tail = expand_pattern_matrix(tail.to_vec());
                if tail.is_empty() {
                    vec![vec![(key.clone(), typed_destructure_pattern.clone())]]
                } else {
                    tail.into_iter()
                        .map(|patterns| {
                            vec![(key.clone(), typed_destructure_pattern.clone())]
                                .into_iter()
                                .chain(patterns.into_iter())
                                .collect()
                        })
                        .collect::<Vec<Vec<(String, TypedDestructurePattern)>>>()
                }
            })
            .collect::<Vec<Vec<(String, TypedDestructurePattern)>>>(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedDestructurePattern {
    Any {
        type_value: Type,
    },
    EnumConstructor {
        name: String,
        payload: Option<Box<TypedDestructurePattern>>,
    },
    Tuple(Vec<TypedDestructurePattern>),
    Record {
        key_pattern_pairs: Vec<(String, TypedDestructurePattern)>,
    },
    Boolean(bool),
    EmptyArray,
    NonEmptyArray {
        first_element: Box<TypedDestructurePattern>,
        rest_elements: Box<TypedDestructurePattern>,
    },

    /// Used for representing patterns for String, Integer, Character etc.
    Infinite {
        /// This is for checking unreachable cases
        handled_cases: Vec<String>,
    },
}

pub fn match_patterns(
    environment: &Environment,
    actual_pattern: &DestructurePattern,
    expected_patterns: Vec<TypedDestructurePattern>,
) -> Vec<TypedDestructurePattern> {
    expected_patterns
        .into_iter()
        .flat_map(|expected_pattern| {
            match match_pattern(environment, &actual_pattern, &expected_pattern) {
                MatchPatternResult::Matched => vec![],
                MatchPatternResult::NotMatched => vec![expected_pattern],
                MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                    match_patterns(environment, actual_pattern, expanded_patterns)
                }
            }
        })
        .collect()
}

pub fn match_pattern(
    environment: &Environment,
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
            DestructurePattern::Infinite { token, .. },
            TypedDestructurePattern::Infinite { handled_cases },
        ) => {
            if handled_cases
                .iter()
                .any(|case| *case == token.representation)
            {
                MatchPatternResult::NotMatched
            } else {
                let mut handled_cases = handled_cases.clone();
                handled_cases.push(token.representation.clone());
                MatchPatternResult::PartiallyMatched {
                    expanded_patterns: vec![TypedDestructurePattern::Infinite { handled_cases }],
                }
            }
        }
        (
            DestructurePattern::EnumConstructor {
                scoped_name: actual_name,
                payload: None,
                ..
            },
            TypedDestructurePattern::EnumConstructor {
                name: expected_name,
                payload: None,
            },
        ) => {
            if actual_name.name.representation != *expected_name {
                MatchPatternResult::NotMatched
            } else {
                MatchPatternResult::Matched
            }
        }
        (
            DestructurePattern::EnumConstructor {
                scoped_name: actual_name,
                payload: Some(actual_payload),
                ..
            },
            TypedDestructurePattern::EnumConstructor {
                name: expected_name,
                payload: Some(expected_payload),
            },
        ) => {
            if actual_name.name.representation != *expected_name {
                MatchPatternResult::NotMatched
            } else {
                match match_pattern(environment, &actual_payload, expected_payload) {
                    MatchPatternResult::Matched => MatchPatternResult::Matched,
                    MatchPatternResult::NotMatched => MatchPatternResult::NotMatched,
                    MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                        MatchPatternResult::PartiallyMatched {
                            expanded_patterns: expanded_patterns
                                .into_iter()
                                .map(|pattern| TypedDestructurePattern::EnumConstructor {
                                    name: expected_name.clone(),
                                    payload: Some(Box::new(pattern)),
                                })
                                .collect(),
                        }
                    }
                }
            }
        }
        (DestructurePattern::Tuple(tuple), TypedDestructurePattern::Tuple(expected_patterns)) => {
            let actual_patterns = tuple.values.clone();
            if actual_patterns.len() != expected_patterns.len() {
                MatchPatternResult::NotMatched
            } else {
                let pattern_pairs = actual_patterns
                    .into_vector()
                    .iter()
                    .zip(expected_patterns.iter())
                    .map(|(actual_pattern, expected_pattern)| PatternPair {
                        actual_pattern: actual_pattern.clone(),
                        expected_pattern: expected_pattern.clone(),
                    })
                    .collect::<Vec<PatternPair>>();

                match_pattern_matrix(environment, PatternMatrix::Tuple { pattern_pairs })
            }
        }
        (
            DestructurePattern::Record {
                key_value_pairs: actual_key_pattern_pairs,
                ..
            },
            TypedDestructurePattern::Record {
                key_pattern_pairs: expected_key_pattern_pairs,
            },
        ) => {
            let actual_keys = actual_key_pattern_pairs
                .iter()
                .map(|key| key.key.representation.clone())
                .collect::<HashSet<String>>();

            let expected_keys = expected_key_pattern_pairs
                .iter()
                .map(|(key, _)| key.clone())
                .collect::<HashSet<String>>();

            if !actual_keys.eq(&expected_keys) {
                return MatchPatternResult::NotMatched;
            }
            let mut actual_key_pattern_pairs = actual_key_pattern_pairs.clone();
            let mut expected_key_pattern_pairs = expected_key_pattern_pairs.clone();
            actual_key_pattern_pairs
                .sort_by(|a, b| a.key.representation.cmp(&b.key.representation));
            expected_key_pattern_pairs.sort_by(|(a, _), (b, _)| a.cmp(&b));

            let key_pattern_pairs = actual_key_pattern_pairs
                .into_iter()
                .zip(expected_key_pattern_pairs.into_iter())
                .map(
                    |(DestructuredRecordKeyValue { key, as_value, .. }, (_, expected_pattern))| {
                        let actual_pattern = match as_value {
                            None => DestructurePattern::Identifier(key.clone()),
                            Some(destructure_pattern) => destructure_pattern,
                        };
                        (
                            key.representation,
                            PatternPair {
                                actual_pattern,
                                expected_pattern,
                            },
                        )
                    },
                )
                .collect::<Vec<(String, PatternPair)>>();

            match_pattern_matrix(environment, PatternMatrix::Record { key_pattern_pairs })
        }
        (
            DestructurePattern::Array { .. },
            TypedDestructurePattern::Any {
                type_value: Type::Array(element_type),
            },
        ) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: vec![
                TypedDestructurePattern::EmptyArray,
                TypedDestructurePattern::NonEmptyArray {
                    first_element: Box::new(TypedDestructurePattern::Any {
                        type_value: *element_type.clone(),
                    }),
                    rest_elements: Box::new(TypedDestructurePattern::Any {
                        type_value: Type::Array(element_type.clone()),
                    }),
                },
            ],
        },
        (DestructurePattern::Array { spread: None, .. }, TypedDestructurePattern::EmptyArray) => {
            MatchPatternResult::Matched
        }
        (
            DestructurePattern::Array {
                spread: Some(spread),
                ..
            },
            TypedDestructurePattern::NonEmptyArray {
                first_element,
                rest_elements,
            },
        ) => match_pattern_matrix(
            environment,
            PatternMatrix::Array {
                left: Box::new(PatternPair {
                    expected_pattern: *first_element.clone(),
                    actual_pattern: *spread.first_element.clone(),
                }),
                right: Box::new(PatternPair {
                    expected_pattern: *rest_elements.clone(),
                    actual_pattern: *spread.rest_elements.clone(),
                }),
            },
        ),
        (
            DestructurePattern::Null(_),
            TypedDestructurePattern::Any {
                type_value: Type::Null,
            },
        ) => {
            // Since Null has only one case, we can just return Matched
            // as long as there is one branch matching for null
            MatchPatternResult::Matched
        }
        // (
        //     DestructurePattern::Integer(_),
        //     TypedDestructurePattern::Any {
        //         type_value: Type::Integer,
        //     },
        // ) => {
        //     // always return NotMatched because there are infinite possible combination of numbers
        //     MatchPatternResult::NotMatched
        // }
        // (
        //     DestructurePattern::String(_),
        //     TypedDestructurePattern::Any {
        //         type_value: Type::String,
        //     },
        // ) => {
        //     // always return NotMatched because there are infinite possible combination of string
        //     MatchPatternResult::NotMatched
        // }
        // (
        //     DestructurePattern::Character(_),
        //     TypedDestructurePattern::Any {
        //         type_value: Type::Character,
        //     },
        // ) => {
        //     // always return NotMatched because there are infinite possible combination of string
        //     MatchPatternResult::NotMatched
        // }
        (_, TypedDestructurePattern::Any { type_value }) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: expand_pattern(environment, type_value),
        },
        _ => MatchPatternResult::NotMatched,
    }
}
