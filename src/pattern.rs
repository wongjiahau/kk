use crate::tokenize::{Token, TokenType};
use crate::{inferred_ast::Identifier, raw_ast::*, typ::*, unify::Positionable};
use crate::{inferred_ast::PropertyName, module::*};
use crate::{non_empty::NonEmpty, unify::rewrite_type_variables_in_type};
use std::collections::HashSet;

/// Why can't we just use the InferredDestructurePatternKind enum?
/// This is because when checking for case exhaustiveness, we want to assume that there is no OR patterns.
/// In other words, to check the case exhaustiveness of list of InferredDestructurePatternKind,
/// its OR patterns must be expanded first, for example:
///     
///     {x: A | B, y: C | D}
///
/// should be expanded into:
///
///     {x: A, y: C}
///     {x: A, y: D}
///     {x: B, y: C}
///     {x: B, y: D}
#[derive(Debug, Clone)]
pub struct CheckablePattern {
    /// If this is true, it means that this pattern is a result of expanding OR-patterns
    ///
    /// In other words, this pattern is not a direct pattern,
    /// but rather an expanded pattern.
    ///
    /// This property allow us to report errors differently between direct patterns and expanded patterns.
    ///
    /// Glossary:
    ///  * direct pattern = pattern that is written down by user verbatim
    ///  * expanded pattern = pattern that is not verbatimly written down by user, but rather a result of expanding OR-patterns
    ///
    /// For example, suppose user written down the following cases:
    /// ```
    ///     case A:
    ///     case B(C | D):
    /// ```
    /// After expanding the OR-patterns, we will get:
    /// ```
    ///     case A:
    ///     case B(C):
    ///     case B(D):
    /// ```
    /// In this example, `A` is a *direct* pattern, while `B(C)` and `B(D)` are both *expanded* patterns.
    pub is_result_of_expansion: bool,
    pub kind: CheckablePatternKind,
}

#[derive(Debug, Clone)]
pub enum CheckablePatternKind {
    Infinite {
        kind: InfinitePatternKind,
        token: Token,
    },
    Unit {
        left_parenthesis: Token,
        right_parenthesis: Token,
    },
    Underscore(Token),
    Identifier(Box<Identifier>),
    EnumConstructor {
        constructor_name: Token,
        payload: Option<Box<CheckablePattern>>,
    },
    Record {
        prefix: Token,
        key_pattern_pairs: Vec<(PropertyName, CheckablePattern)>,
    },
    Tuple {
        patterns: Box<NonEmpty<CheckablePatternKind>>,
    },
    Array {
        left_square_bracket: Token,
        spread: Option<CheckablePatternArraySpread>,
        right_square_bracket: Token,
    },
}

impl Positionable for CheckablePatternKind {
    fn position(&self) -> Position {
        match self {
            CheckablePatternKind::Infinite { token, .. }
            | CheckablePatternKind::Underscore(token) => token.position,
            CheckablePatternKind::Unit {
                left_parenthesis,
                right_parenthesis,
            } => left_parenthesis.position.join(right_parenthesis.position),
            CheckablePatternKind::Identifier(identifier) => identifier.token.position,
            CheckablePatternKind::EnumConstructor {
                constructor_name,
                payload,
            } => match payload {
                Some(payload) => constructor_name.position.join(payload.kind.position()),
                None => constructor_name.position,
            },
            CheckablePatternKind::Record {
                prefix,
                key_pattern_pairs,
                ..
            } => prefix.position.join_maybe(
                key_pattern_pairs
                    .last()
                    .map(|(_, pattern)| pattern.kind.position()),
            ),
            CheckablePatternKind::Tuple { .. } => {
                panic!("")
            }
            CheckablePatternKind::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckablePatternArraySpread {
    first_element: Box<CheckablePatternKind>,
    rest_elements: Box<CheckablePatternKind>,
}

#[derive(Debug, Clone)]
pub struct CheckablePatternEnumConstructorPayload {
    pub pattern: Box<CheckablePattern>,
}

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
    actual_pattern: CheckablePatternKind,
}

fn match_pattern_matrix(module: &Module, pattern_matrix: PatternMatrix) -> MatchPatternResult {
    enum WrapResultAs {
        Tuple,
        Record,
        Array,
    }

    struct RawPatternMatrix {
        key: String,
        actual_pattern: CheckablePatternKind,
        expected_pattern: ExpandablePattern,
    }

    fn match_raw_pattern_matrix(
        module: &Module,
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
                        match_pattern(module, actual_pattern, expected_pattern),
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
            expanded_patterns: cartesian_product(
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
            match_raw_pattern_matrix(module, pattern_matrices, WrapResultAs::Tuple)
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
            match_raw_pattern_matrix(module, pattern_matrices, WrapResultAs::Record)
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
            match_raw_pattern_matrix(module, pattern_matrices, WrapResultAs::Array)
        }
    }
}

pub fn expand_pattern(module: &Module, type_value: &Type) -> Vec<ExpandablePattern> {
    match type_value {
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
            symbol_uid,
            type_arguments,
            ..
        } => module
            .get_enum_constructors(symbol_uid.clone())
            .into_iter()
            .map(|constructor_symbol| {
                let constructor_name = constructor_symbol.constructor_name.clone();
                match &constructor_symbol.payload {
                    None => ExpandablePattern::EnumConstructor {
                        name: constructor_name,
                        payload: None,
                    },
                    Some(payload) => {
                        let payload =
                            rewrite_type_variables_in_type(type_arguments.clone(), payload.clone());
                        ExpandablePattern::EnumConstructor {
                            name: constructor_name,
                            payload: Some(Box::new(ExpandablePattern::Any {
                                type_value: payload,
                            })),
                        }
                    }
                }
            })
            .collect(),
        Type::ImplicitTypeVariable(type_variable) => {
            match module.get_type_variable_terminal_type(type_variable.name.clone()) {
                Some(type_value) => expand_pattern(module, &type_value),
                None => vec![],
            }
        }
        _ => vec![],
    }
}

/// Return the n-ary Cartesian Product of a given list of inputs.
///
/// Example
/// ```
/// Input = [[("a", 1), ("b", 2)], [("c", 3), ("d", 4)]]
/// Output =
///     [
///         [("a", 1), ("c", 3)],
///         [("a", 1), ("d", 4)],
///         [("b", 2), ("c", 3)],
///         [("b", 2), ("d", 4)]
///     ]
/// ```
/// The resulting dimensions depends on the input length.  
/// For example, if 3 groups are given, then the result will be an list of triples.
/// Refer https://en.wikipedia.org/wiki/Cartesian_product
pub fn cartesian_product<Key: Clone, Value: Clone>(
    items: Vec<Vec<(Key, Value)>>,
) -> Vec<Vec<(Key, Value)>> {
    match items.split_first() {
        None => vec![],
        Some((head, tail)) => head
            .iter()
            .flat_map(|(key, value)| {
                let tail = cartesian_product(tail.to_vec());
                if tail.is_empty() {
                    vec![vec![(key.clone(), value.clone())]]
                } else {
                    tail.into_iter()
                        .map(|patterns| {
                            vec![(key.clone(), value.clone())]
                                .into_iter()
                                .chain(patterns.into_iter())
                                .collect()
                        })
                        .collect::<Vec<Vec<(Key, Value)>>>()
                }
            })
            .collect::<Vec<Vec<(Key, Value)>>>(),
    }
}

#[test]
fn test_cartesian_product() {
    assert_eq!(
        cartesian_product(vec![vec![("a", 1), ("b", 2)], vec![("c", 3), ("d", 4)],]),
        vec![
            vec![("a", 1), ("c", 3)],
            vec![("a", 1), ("d", 4)],
            vec![("b", 2), ("c", 3)],
            vec![("b", 2), ("d", 4)]
        ]
    );

    // Resulting dimension depends on input length
    // If the input has 3 groups
    // Then the result will have the dimensions of N x 3, where N is the number of possible permutations
    assert_eq!(
        cartesian_product(vec![
            vec![("a", 1)],
            vec![("b", 2)],
            vec![("c", 3), ("d", 4)],
        ]),
        vec![
            vec![("a", 1), ("b", 2), ("c", 3)],
            vec![("a", 1), ("b", 2), ("d", 4)]
        ]
    )
}

#[derive(Debug, Clone)]
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
    },
    Tuple(Vec<ExpandablePattern>),
    Record {
        key_pattern_pairs: Vec<(String, ExpandablePattern)>,
    },
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

pub fn match_patterns(
    module: &Module,
    actual_pattern: &CheckablePatternKind,
    expected_patterns: Vec<ExpandablePattern>,
) -> Vec<ExpandablePattern> {
    expected_patterns
        .into_iter()
        .flat_map(|expected_pattern| {
            match match_pattern(module, &actual_pattern, &expected_pattern) {
                MatchPatternResult::Matched => vec![],
                MatchPatternResult::NotMatched => vec![expected_pattern],
                MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                    match_patterns(module, actual_pattern, expanded_patterns)
                }
            }
        })
        .collect()
}

pub fn match_pattern(
    module: &Module,
    actual_pattern: &CheckablePatternKind,
    expected_pattern: &ExpandablePattern,
) -> MatchPatternResult {
    match (actual_pattern, expected_pattern) {
        (CheckablePatternKind::Underscore(_), _) => MatchPatternResult::Matched,
        (CheckablePatternKind::Identifier(identifier), _) => {
            // check if identifier matches any enum constructor
            if module.matches_some_enum_constructor(&identifier.token.representation) {
                match_pattern(
                    module,
                    &CheckablePatternKind::EnumConstructor {
                        constructor_name: identifier.token.clone(),
                        payload: None,
                    },
                    expected_pattern,
                )
            } else {
                MatchPatternResult::Matched
            }
        }
        (
            CheckablePatternKind::Infinite { token, .. },
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
            CheckablePatternKind::EnumConstructor {
                constructor_name: actual_name,
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
            CheckablePatternKind::EnumConstructor {
                constructor_name: actual_name,
                payload: Some(actual_payload),
                ..
            },
            ExpandablePattern::EnumConstructor {
                name: expected_name,
                payload: Some(expected_payload),
            },
        ) => {
            if actual_name.representation != *expected_name {
                MatchPatternResult::NotMatched
            } else {
                match match_pattern(module, &actual_payload.kind, expected_payload) {
                    MatchPatternResult::Matched => MatchPatternResult::Matched,
                    MatchPatternResult::NotMatched => MatchPatternResult::NotMatched,
                    MatchPatternResult::PartiallyMatched { expanded_patterns } => {
                        MatchPatternResult::PartiallyMatched {
                            expanded_patterns: expanded_patterns
                                .into_iter()
                                .map(|pattern| ExpandablePattern::EnumConstructor {
                                    name: expected_name.clone(),
                                    payload: Some(Box::new(pattern)),
                                })
                                .collect(),
                        }
                    }
                }
            }
        }
        (
            CheckablePatternKind::Tuple {
                patterns: actual_patterns,
                ..
            },
            ExpandablePattern::Tuple(expected_patterns),
        ) => {
            let actual_patterns = actual_patterns.clone();
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

                match_pattern_matrix(module, PatternMatrix::Tuple { pattern_pairs })
            }
        }
        (
            CheckablePatternKind::Record {
                key_pattern_pairs: actual_key_pattern_pairs,
                ..
            },
            ExpandablePattern::Record {
                key_pattern_pairs: expected_key_pattern_pairs,
            },
        ) => {
            let actual_keys = actual_key_pattern_pairs
                .iter()
                .map(|(key, _)| key.0.representation.clone())
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
                .sort_by(|(a, _), (b, _)| a.0.representation.cmp(&b.0.representation));
            expected_key_pattern_pairs.sort_by(|(a, _), (b, _)| a.cmp(&b));

            let key_pattern_pairs = actual_key_pattern_pairs
                .into_iter()
                .zip(expected_key_pattern_pairs.into_iter())
                .map(|((key, as_value, ..), (_, expected_pattern))| {
                    (
                        key.0.representation,
                        PatternPair {
                            actual_pattern: as_value.kind,
                            expected_pattern,
                        },
                    )
                })
                .collect::<Vec<(String, PatternPair)>>();

            match_pattern_matrix(module, PatternMatrix::Record { key_pattern_pairs })
        }
        (
            CheckablePatternKind::Array { .. },
            ExpandablePattern::Any {
                type_value:
                    Type::BuiltInOneArgumentType {
                        kind: BuiltInOneArgumentTypeKind::Array,
                        type_argument: element_type,
                    },
            },
        ) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: vec![
                ExpandablePattern::EmptyArray,
                ExpandablePattern::NonEmptyArray {
                    first_element: Box::new(ExpandablePattern::Any {
                        type_value: *element_type.clone(),
                    }),
                    rest_elements: Box::new(ExpandablePattern::Any {
                        type_value: Type::BuiltInOneArgumentType {
                            kind: BuiltInOneArgumentTypeKind::Array,
                            type_argument: element_type.clone(),
                        },
                    }),
                },
            ],
        },
        (CheckablePatternKind::Array { spread: None, .. }, ExpandablePattern::EmptyArray) => {
            MatchPatternResult::Matched
        }
        (
            CheckablePatternKind::Array {
                spread: Some(spread),
                ..
            },
            ExpandablePattern::NonEmptyArray {
                first_element,
                rest_elements,
            },
        ) => match_pattern_matrix(
            module,
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
            CheckablePatternKind::Unit { .. },
            ExpandablePattern::Any {
                type_value: Type::Unit,
            },
        ) => {
            // Since Unit has only one case, we can just return Matched
            // as long as there is one branch matching for unit
            MatchPatternResult::Matched
        }
        (_, ExpandablePattern::Any { type_value }) => MatchPatternResult::PartiallyMatched {
            expanded_patterns: expand_pattern(module, type_value),
        },
        _ => MatchPatternResult::NotMatched,
    }
}
