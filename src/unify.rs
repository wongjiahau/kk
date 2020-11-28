use crate::ast::*;
use std::collections::HashMap;

pub struct Program {
    pub statements: Vec<Statement>,
    pub source: Source,
}

pub struct Environment {
    symbols: HashMap<String, Symbol>,
}

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub declaration: Option<Token>,
    pub annotated_type: Option<TypeAnnotation>,
    pub actual_type: Option<Type>,
    pub usage_references: Vec<UsageReference>,
}

#[derive(Clone)]
pub struct UsageReference {
    position: Position,
    source: Source,
}

pub fn unify_program(program: Program) -> Result<(), UnifyError> {
    // 1. TODO: Populate environment with imported symbols
    let mut environment: Environment = Environment {
        symbols: HashMap::new(),
    };

    // 2. Type check this program
    for statement in program.statements {
        let _ = unify_statement(&mut environment, program.source.clone(), statement)?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum UnifyError {
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
    source: Source,
    statement: Statement,
) -> Result<(), UnifyError> {
    match statement {
        Statement::Let {
            left,
            right,
            type_annotation,
        } => {
            let type_annotation_type = match type_annotation {
                Some(type_annotation) => {
                    Some(type_annotation_to_type(environment, type_annotation)?)
                }
                None => None,
            };
            let right_type = infer_expression_type(environment, &right.value)?;

            // 1. Check if right matches type annotation
            let right_type = match type_annotation_type {
                Some(type_annotation_type) => {
                    let (_, right_type) = unify_type(
                        type_annotation_type,
                        right_type,
                        get_expression_location(source, &right.value),
                    )?;
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

pub fn get_expression_location(source: Source, expression_value: &ExpressionValue) -> Location {
    match expression_value {
        ExpressionValue::String(token) | ExpressionValue::Number(token) => Location {
            position: token.position.clone(),
            source,
        },
        _ => panic!(),
    }
}

pub fn unify_type(
    expected: Type,
    actual: Type,
    location: Location,
) -> Result<(Type, Type), UnifyError> {
    match (expected.clone(), actual.clone()) {
        (Type::String, Type::String) => Ok((expected, actual)),
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
        _ => panic!(),
    }
}

pub fn type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: TypeAnnotation,
) -> Result<Type, UnifyError> {
    match type_annotation.representation {
        TypeRepresentation::Name(Token { representation, .. }) => match representation.as_str() {
            "string" => Ok(Type::String),
            _ => panic!(),
        },
        _ => panic!(),
    }
}

pub fn unify_destructure_pattern(
    environment: &mut Environment,
    destructure_pattern: &DestructurePattern,
    expression_type: &Type,
) -> Result<Type, UnifyError> {
    match (destructure_pattern, expression_type) {
        (DestructurePattern::Tag { payload: None, token }, Type::Tag {tagname, payload: None}) => {
            if token.representation == *tagname {
                Ok(Type::Tag { tagname: token.representation.clone(), payload: None, })
            }
            else {
                Err(UnifyError::CannotDestructure{
                    expression_type: expression_type.clone(),
                    destructure_pattern: destructure_pattern.clone()
                })
            }
        },
        (DestructurePattern::Identifier(token), _) => {
            environment.symbols.insert(
                token.representation.to_string(),
                Symbol {
                    name: token.representation.to_string(),
                            declaration: Some(token.clone()),
                    annotated_type: None,
                    actual_type: Some(expression_type.clone()),
                    usage_references: vec![],
                },
            );
            Ok(Type::NotInferred)
        }
        (_, _) => Err(UnifyError::CannotDestructure  {
                    expression_type: expression_type.clone(),
                    destructure_pattern: destructure_pattern.clone()
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
