use crate::ast::*;
use crate::pattern::TypedDestructurePattern;
use crate::unify::{UnifyError, UnifyErrorKind};
use colored::*;
use prettytable::{format::Alignment, Cell, Row, Table};
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub fn print_unify_error(source: Source, code: String, unify_error: UnifyError) {
    let origin = match source {
        Source::File { path } => path,
        Source::NonFile { env_name } => env_name,
    };
    let error = stringify_unify_error_kind(unify_error.kind);

    // println!("position = {:#?}", unify_error.position);
    // println!(
    //     "{:#?}",
    //     code.chars()
    //         .into_iter()
    //         .collect::<Vec<char>>()
    //         .get(unify_error.position.character_index_end)
    // );

    let range = {
        let start = unify_error.position.character_index_start;
        let end = unify_error.position.character_index_end;
        (start, end + 1)
    };

    let mut files = SimpleFiles::new();

    let file_id = files.add(origin, code);
    let diagnostic = Diagnostic::error()
        .with_labels(vec![Label::primary(
            file_id,
            Range {
                start: range.0,
                end: range.1,
            },
        )
        .with_message(error.summary)])
        .with_notes(vec![error.body]);

    // We now set up the writer and configuration, and then finally render the
    // diagnostic to standard error.

    let writer = StandardStream::stderr(ColorChoice::AlwaysAnsi);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("Unable to emit output");
    std::process::exit(1);
}

#[derive(Debug)]
pub struct StringifiedError {
    pub summary: String,
    pub body: String,
}

pub fn stringify_unify_error_kind(unify_error_kind: UnifyErrorKind) -> StringifiedError {
    match unify_error_kind {
        UnifyErrorKind::TypeMismatch {
            expected_type,
            actual_type,
            ..
        } => {
            let expected_type = stringify_type(expected_type, 0);
            let actual_type = stringify_type(actual_type, 0);
            let diffs = diff::lines(&expected_type, &actual_type).into_iter();

            let expected_type = diffs
                .clone()
                .filter_map(|diff| match diff {
                    diff::Result::Left(left) => Some(format!("- {}", left.green().to_string() )),
                    diff::Result::Both(left, _) => Some(format!("  {}", left.to_string() )),
                    diff::Result::Right(_) => None,
                })
                .collect::<Vec<String>>()
                .join("\n");

            let actual_type = diffs
                .filter_map(|diff| match diff {
                    diff::Result::Left(_) => None,
                    diff::Result::Both(_, right) => Some(format!("  {}", right)),
                    diff::Result::Right(right) => Some(format!("+ {}", right).red().to_string()),
                })
                .collect::<Vec<String>>()
                .join("\n");
            let mut table = Table::new();
            table.add_row(Row::new(vec![
                {
                    let mut cell = Cell::new("EXPECTED TYPE");
                    cell.align(Alignment::CENTER);
                    cell
                },
                {
                    let mut cell = Cell::new("ACTUAL TYPE");
                    cell.align(Alignment::CENTER);
                    cell
                },
            ]));
            table.add_row(Row::new(vec![
                Cell::new(&expected_type),
                Cell::new(&actual_type),
            ]));
            let table = table.to_string();
            StringifiedError {
                summary: "Type mismatch".to_string(),
                body: table,
            }
        }
        UnifyErrorKind::InvalidFunctionArgumentLength {
            expected_length,
            actual_length,
        } => StringifiedError {
            summary: "Function arguments length mismatch".to_string(),
            body: format!(
                "Expected {} {}, but this function has {} {}",
                expected_length,
                pluralize("argument".to_string(), expected_length),
                actual_length,
                pluralize("argument".to_string(), actual_length),
            ),
        },
        UnifyErrorKind::CannotInvokeNonFunction { .. } => StringifiedError {
            summary: "Cannot call a non-function".to_string(),
            body: "The type of this expression is not function, so you cannot call it.".to_string(),
        },
        UnifyErrorKind::UnknownValueSymbol => StringifiedError {
            summary: "Unknown value symbol".to_string(),
            body: "Cannot find this value symbol in the current scope".to_string(),
        },
        UnifyErrorKind::UnknownTypeSymbol => StringifiedError {
            summary: "Unknown type symbol".to_string(),
            body: "Cannot find this type symbol in the current scope".to_string(),
        },
        UnifyErrorKind::UnknownConstructorSymbol => StringifiedError {
            summary: "Unknown constructor symbol".to_string(),
            body: "Cannot find this constructor symbol in the current scope".to_string(),
        },
        UnifyErrorKind::MissingCases(missing_patterns) => StringifiedError {
            summary: "Non-exhaustive cases".to_string(),
            body: format!(
                "Missing case(s):\n\n{}",
                missing_patterns
                    .into_iter()
                    .map(stringify_typed_destrucutre_pattern)
                    .map(|result| format!("  \\{} => ...", result))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        },
        UnifyErrorKind::UnreachableCase => StringifiedError {
            summary: "Unreachable case".to_string(),
            body: "This case is unreachable because all possible cases are already handled by previous branches.".to_string()
        },
        UnifyErrorKind::UnusedVariale => StringifiedError {
            summary: "Unused variable".to_string(),
            body: "This variable created but not used anywhere, consider removing it.".to_string()
        },
        UnifyErrorKind::NoSuchPropertyOnThisRecord {
            mut expected_keys
        } => {
            expected_keys.sort();
            StringifiedError{
            summary: "No such property".to_string(),
            body: format!("Available properties:\n{}", indent_string(expected_keys.join("\n"), 2))
        } },
        UnifyErrorKind::CannotAccessPropertyOfNonRecord {actual_type} => StringifiedError {
            summary: "Cannot access property on a non-record type".to_string(),
            body: format!("The type of this expression is:\n{}", stringify_type(actual_type, 1))
        },
        UnifyErrorKind::InfiniteTypeDetected {type_variable_name, in_type} => StringifiedError {
            summary: "Infinite type".to_string(),
            body: format!(
                "Infinite type expansion will happen when substituting `{}` into:\n\n{}",
                type_variable_name,
                stringify_type(in_type, 1)
            )
        },
        UnifyErrorKind::DuplicatedRecordKey => StringifiedError {
            summary: "Duplicated record key".to_string(),
            body: "This key is already declared before in this record. Consider removing or renaming it.".to_string()
        },
        UnifyErrorKind::UnknownTypeParameterName {
            expected_names
        } => StringifiedError {
            summary: "Unknown type parameter name".to_string(),
            body: format!("The expected names are:\n{}", indent_string(expected_names.join("\n"), 2))
        },
        UnifyErrorKind::TypeArgumentsLengthMismatch {
            expected_type_parameter_names,
            actual_length
        } => StringifiedError {
            summary: "Type arguments length mismatch".to_string(),
            body:
                format!(
                "{} type arguments are given, but expected {}:\n{}",
                actual_length,
                expected_type_parameter_names.len(),
                indent_string(expected_type_parameter_names.join("\n"), 2 )
            )
        },
        UnifyErrorKind::TypeArgumentNameMismatch {expected_name} => StringifiedError {
            summary: "Type argument name mismatch".to_string(),
            body: format!("The expected name here is `{}`.", expected_name)
        },
        UnifyErrorKind::WrongTypeAnnotation {expected_type} => StringifiedError {
            summary: "Wrong type annotation".to_string(),
            body: format!("The expected type annotation is:\n\n{}", stringify_type(expected_type, 1))
        },
        UnifyErrorKind::RecordExtraneousKey {
            mut expected_keys
        } => StringifiedError {
            summary: "No such property".to_string(),
            body: {
                expected_keys.sort();
                format!("The expected properties are:\n{}", indent_string(expected_keys.join("\n"), 2))
            }
        },
        UnifyErrorKind::RecordMissingKeys {
            mut missing_keys
        } => StringifiedError {
            summary: "Missing properties".to_string(),
            body: {
                missing_keys.sort();
                format!("The missing properties are:\n{}", indent_string(missing_keys.join("\n"), 2))
            }
        },
        other => panic!("{:#?}", other),
    }
}

pub fn stringify_typed_destrucutre_pattern(
    typed_destructure_pattern: TypedDestructurePattern,
) -> String {
    match typed_destructure_pattern {
        TypedDestructurePattern::EmptyArray => "[]".to_string(),
        TypedDestructurePattern::NonEmptyArray { left, right } => {
            format!(
                "[{}, ...{}]",
                stringify_typed_destrucutre_pattern(*left),
                stringify_typed_destrucutre_pattern(*right)
            )
        }
        TypedDestructurePattern::Boolean(value) => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        TypedDestructurePattern::Record { key_pattern_pairs } => format!(
            "{{ {} }}",
            key_pattern_pairs
                .into_iter()
                .map(|(key, pattern)| format!(
                    "{} = {}",
                    key,
                    stringify_typed_destrucutre_pattern(pattern)
                ))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypedDestructurePattern::Tuple(patterns) => format!(
            "({})",
            patterns
                .into_iter()
                .map(stringify_typed_destrucutre_pattern)
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypedDestructurePattern::Any { .. } => "_".to_string(),
        TypedDestructurePattern::Enum { tagname, payload } => match payload {
            None => tagname,
            Some(payload) => format!(
                "{}({})",
                tagname,
                stringify_typed_destrucutre_pattern(*payload)
            ),
        },
    }
}

pub fn pluralize(input: String, count: usize) -> String {
    if count > 1 {
        format!("{}s", input)
    } else {
        input
    }
}

pub fn indent_string(string: String, number_of_spaces: usize) -> String {
    string
        .split('\n')
        .map(|string| format!("{}{}", " ".repeat(number_of_spaces), string))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn stringify_type(type_value: Type, indent_level: usize) -> String {
    match type_value {
        Type::Boolean => indent_string("boolean".to_string(), indent_level * 2),
        Type::Number => indent_string("number".to_string(), indent_level * 2),
        Type::Null => indent_string("null".to_string(), indent_level * 2),
        Type::String => indent_string("string".to_string(), indent_level * 2),
        Type::Array(element_type) => indent_string(
            format!(
                "Array<\n{}\n>",
                stringify_type(*element_type, indent_level + 1)
            ),
            indent_level * 2,
        ),
        Type::Named {
            name,
            type_arguments: arguments,
        } => {
            if arguments.is_empty() {
                name
            } else {
                let result = format!(
                    "{}<\n{}\n>",
                    name,
                    arguments
                        .into_iter()
                        .map(|(key, type_value)| format!(
                            "{}=\n{},",
                            indent_string(key, 2),
                            indent_string(stringify_type(type_value, 0), 4)
                        ),)
                        .collect::<Vec<String>>()
                        .join("\n")
                );
                indent_string(result, indent_level * 2)
            }
        }
        Type::Tuple(types) => format!(
            "(\n{}\n)",
            types
                .into_iter()
                .map(|type_value| stringify_type(type_value, indent_level + 1))
                .collect::<Vec<String>>()
                .join(",\n")
        ),
        Type::ImplicitTypeVariable { name } | Type::ExplicitTypeVariable { name } => {
            indent_string(name, indent_level * 2)
        }
        Type::Underscore => "_".to_string(),
        Type::Record { mut key_type_pairs } => {
            key_type_pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
            let result = format!(
                "{{\n{}\n}}",
                key_type_pairs
                    .into_iter()
                    .map(|(key, type_value)| {
                        format!(
                            "{}:\n{},",
                            indent_string(key, 2),
                            indent_string(stringify_type(type_value, 0), 4)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join("\n")
            );
            indent_string(result, indent_level * 2)
        }
        Type::Function(function_type) => {
            let result = format!(
                "\\(\n{}\n) =>\n{}",
                vec![*function_type.first_argument_type]
                    .into_iter()
                    .chain(function_type.rest_arguments_types.into_iter())
                    .map(|argument_type| { indent_string(stringify_type(argument_type, 0), 2) })
                    .collect::<Vec<String>>()
                    .join(",\n"),
                indent_string(stringify_type(*function_type.return_type, 0), 2)
            );
            indent_string(result, indent_level * 2)
        }
    }
}
