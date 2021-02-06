use crate::ast::*;
use crate::environment::*;
use crate::unify::rewrite_type_variables_in_type;
use std::collections::HashSet;

#[derive(Debug)]
pub enum MatchPatternResult {
    Matched,
    NotMatched,
    PartiallyMatched {
        expanded_patterns: Vec<ExpandablePattern>,
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
    expected_pattern: ExpandablePattern,
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
        expected_pattern: ExpandablePattern,
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
            .collect::<Vec<((String, ExpandablePattern), MatchPatternResult)>>();
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
                WrapResultAs::Record => ExpandablePattern::Record { key_pattern_pairs },
                WrapResultAs::Tuple => ExpandablePattern::Tuple(
                    key_pattern_pairs
                        .into_iter()
                        .map(|(_, pattern)| pattern)
                        .collect(),
                ),
                WrapResultAs::Array => ExpandablePattern::NonEmptyArray {
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

pub fn expand_pattern(environment: &Environment, type_value: &Type) -> Vec<ExpandablePattern> {
    match type_value {
        Type::Boolean => vec![
            ExpandablePattern::Boolean(true),
            ExpandablePattern::Boolean(false),
        ],
        Type::Integer => vec![ExpandablePattern::Infinite {
            handled_cases: vec![],
            kind: InfinitePatternKind::Integer,
        }],
        Type::String => vec![ExpandablePattern::Infinite {
            handled_cases: vec![],
            kind: InfinitePatternKind::String,
        }],
        Type::Character => vec![ExpandablePattern::Infinite {
            handled_cases: vec![],
            kind: InfinitePatternKind::Character,
        }],
        Type::Tuple(types) => vec![ExpandablePattern::Tuple(
            types
                .clone()
                .map(|type_value| ExpandablePattern::Any { type_value })
                .into_vector(),
        )],
        Type::Record { key_type_pairs } => vec![ExpandablePattern::Record {
            key_pattern_pairs: key_type_pairs
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        ExpandablePattern::Any {
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
                let enum_type = EnumType {
                    name: name.clone(),
                    type_arguments: type_arguments.clone(),
                };
                match &constructor_symbol.payload {
                    None => ExpandablePattern::EnumConstructor {
                        name: constructor_name,
                        payload: None,
                        enum_type,
                    },
                    Some(payload) => {
                        let payload =
                            rewrite_type_variables_in_type(type_arguments.clone(), payload.clone());
                        ExpandablePattern::EnumConstructor {
                            name: constructor_name,
                            enum_type,
                            payload: Some(Box::new(ExpandablePattern::Any {
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
    types: Vec<Vec<(/*key*/ String, /*pattern*/ ExpandablePattern)>>,
) -> Vec<Vec<(/*key*/ String, /*pattern*/ ExpandablePattern)>> {
    match types.split_first() {
        None => vec![],
        Some((head, tail)) => head
            .iter()
            .flat_map(|(key, expandable_pattern)| {
                let tail = expand_pattern_matrix(tail.to_vec());
                if tail.is_empty() {
                    vec![vec![(key.clone(), expandable_pattern.clone())]]
                } else {
                    tail.into_iter()
                        .map(|patterns| {
                            vec![(key.clone(), expandable_pattern.clone())]
                                .into_iter()
                                .chain(patterns.into_iter())
                                .collect()
                        })
                        .collect::<Vec<Vec<(String, ExpandablePattern)>>>()
                }
            })
            .collect::<Vec<Vec<(String, ExpandablePattern)>>>(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub name: String,
    pub type_arguments: Vec<(String, Type)>,
}

/// This is used for checking pattern exhaustiveness + unreachable cases
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpandablePattern {
    Any {
        type_value: Type,
    },
    EnumConstructor {
        name: String,
        payload: Option<Box<ExpandablePattern>>,
        enum_type: EnumType,
    },
    Tuple(Vec<ExpandablePattern>),
    Record {
        key_pattern_pairs: Vec<(String, ExpandablePattern)>,
    },
    Boolean(bool),
    EmptyArray,
    NonEmptyArray {
        first_element: Box<ExpandablePattern>,
        rest_elements: Box<ExpandablePattern>,
    },

    /// Used for representing patterns for String, Integer, Character etc.
    Infinite {
        /// This is for checking unreachable cases
        handled_cases: Vec<String>,
        kind: InfinitePatternKind,
    },
}

// impl ExpandablePattern {
//     pub fn to_type(&self, environment: &mut Environment) -> Type {
//         match &self {
//             ExpandablePattern::Any { type_value } => type_value.clone(),
//             ExpandablePattern::EnumConstructor { name, payload } => {
//                 let enum_type = get_enum_type(
//                     environment,
//                     None,
//                     ScopedName {
//                         name,
//                         namespaces: vec![],
//                     },
//                 )?;
//                 match paylod {
//                     None => Ok(enum_type.expected_enum_type),
//                 }
//             }
//             ExpandablePattern::Tuple(_) => {}
//             ExpandablePattern::Record { key_pattern_pairs } => {}
//             ExpandablePattern::Boolean(_) => {}
//             ExpandablePattern::EmptyArray => {}
//             ExpandablePattern::NonEmptyArray {
//                 first_element,
//                 rest_elements,
//             } => {}
//             ExpandablePattern::Infinite {
//                 handled_cases,
//                 kind,
//             } => {}
//         }
//     }
// }

pub fn match_patterns(
    environment: &Environment,
    actual_pattern: &DestructurePattern,
    expected_patterns: Vec<ExpandablePattern>,
) -> Vec<ExpandablePattern> {
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
    expected_pattern: &ExpandablePattern,
) -> MatchPatternResult {
    match (actual_pattern, expected_pattern) {
        (DestructurePattern::Underscore(_), _) => MatchPatternResult::Matched,
        (DestructurePattern::Identifier(_), _) => MatchPatternResult::Matched,
        (DestructurePattern::Boolean { value: true, .. }, ExpandablePattern::Boolean(true)) => {
            MatchPatternResult::Matched
        }
        (DestructurePattern::Boolean { value: false, .. }, ExpandablePattern::Boolean(false)) => {
            MatchPatternResult::Matched
        }
        (
            DestructurePattern::Infinite { token, .. },
            ExpandablePattern::Infinite {
                handled_cases,
                kind,
            },
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
                    expanded_patterns: vec![ExpandablePattern::Infinite {
                        handled_cases,
                        kind: kind.clone(),
                    }],
                }
            }
        }
        (
            DestructurePattern::EnumConstructor {
                name: actual_name,
                payload: None,
                ..
            },
            ExpandablePattern::EnumConstructor {
                name: expected_name,
                payload: None,
                ..
            },
        ) => {
            if actual_name.representation != *expected_name {
                MatchPatternResult::NotMatched
            } else {
                MatchPatternResult::Matched
            }
        }
        (
            DestructurePattern::EnumConstructor {
                name: actual_name,
                payload: Some(actual_payload),
                ..
            },
            ExpandablePattern::EnumConstructor {
                name: expected_name,
                payload: Some(expected_payload),
                enum_type,
            },
        ) => {
            if actual_name.representation != *expected_name {
                MatchPatternResult::NotMatched
            } else {
                match match_pattern(environment, &actual_payload, expected_payload) {
                    MatchPatternResult::Matched => MatchPatternResult::Matched,
                    MatchPatternResult::NotMatched => MatchPatternResult::NotMatched,
                    MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                        MatchPatternResult::PartiallyMatched {
                            expanded_patterns: expanded_patterns
                                .into_iter()
                                .map(|pattern| ExpandablePattern::EnumConstructor {
                                    name: expected_name.clone(),
                                    payload: Some(Box::new(pattern)),
                                    enum_type: enum_type.clone(),
                                })
                                .collect(),
                        }
                    }
                }
            }
        }
        (DestructurePattern::Tuple(tuple), ExpandablePattern::Tuple(expected_patterns)) => {
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
            ExpandablePattern::Record {
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
            ExpandablePattern::Any {
                type_value: Type::Array(element_type),
            },
        ) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: vec![
                ExpandablePattern::EmptyArray,
                ExpandablePattern::NonEmptyArray {
                    first_element: Box::new(ExpandablePattern::Any {
                        type_value: *element_type.clone(),
                    }),
                    rest_elements: Box::new(ExpandablePattern::Any {
                        type_value: Type::Array(element_type.clone()),
                    }),
                },
            ],
        },
        (DestructurePattern::Array { spread: None, .. }, ExpandablePattern::EmptyArray) => {
            MatchPatternResult::Matched
        }
        (
            DestructurePattern::Array {
                spread: Some(spread),
                ..
            },
            ExpandablePattern::NonEmptyArray {
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
            ExpandablePattern::Any {
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
        (_, ExpandablePattern::Any { type_value }) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: expand_pattern(environment, type_value),
        },
        _ => MatchPatternResult::NotMatched,
    }
}
