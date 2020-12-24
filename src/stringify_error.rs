use crate::ast::*;
use crate::unify::{TypedDestructurePattern, UnifyError, UnifyErrorKind};
use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use colored::*;
use prettytable::{format::Alignment, Cell, Row, Table};

pub fn stringify_unify_error(source: Source, code: String, unify_error: UnifyError) -> String {
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

    let snippet = Snippet {
        title: Some(Annotation {
            label: Some(&error.summary),
            id: None,
            annotation_type: AnnotationType::Error,
        }),
        footer: vec![],
        slices: vec![Slice {
            source: &code,
            line_start: 1,
            origin: Some(&origin),
            fold: true,
            annotations: vec![SourceAnnotation {
                label: "",
                annotation_type: AnnotationType::Error,
                range: {
                    let start = unify_error.position.character_index_start;
                    let end = unify_error.position.character_index_end;
                    (start, end + 1)
                },
            }],
        }],
        opt: FormatOptions {
            // color: true,
            // anonymized_line_numbers: false,
            // margin: None,
            ..Default::default()
        },
    };

    let code_snippet = DisplayList::from(snippet);

    format!("{}\n\n{}", code_snippet, &error.body)
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
                    diff::Result::Left(left) => Some(left.green().to_string()),
                    diff::Result::Both(left, _) => Some(left.to_string()),
                    diff::Result::Right(_) => None,
                })
                .collect::<Vec<String>>()
                .join("\n");

            let actual_type = diffs
                .filter_map(|diff| match diff {
                    diff::Result::Left(_) => None,
                    diff::Result::Both(_, right) => Some(format!("  {}", right)),
                    diff::Result::Right(right) => Some(format!("> {}", right).red().to_string()),
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
            body: "This case is already handled by one of the previous branches, please remove this branch.".to_string()
        },
        other => panic!("{:#?}", other),
    }
}

pub fn stringify_typed_destrucutre_pattern(
    typed_destructure_pattern: TypedDestructurePattern,
) -> String {
    match typed_destructure_pattern {
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
        Type::Array(_) => panic!(),
        Type::Named { name, arguments } => {
            let result = if arguments.is_empty() {
                name
            } else {
                format!(
                    "{}<{}>",
                    name,
                    arguments
                        .into_iter()
                        .map(|type_value| stringify_type(type_value, indent_level))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            };
            indent_string(result, indent_level * 2)
        }
        Type::Tuple(types) => format!(
            "(\n{}\n)",
            types
                .into_iter()
                .map(|type_value| stringify_type(type_value, indent_level + 1))
                .collect::<Vec<String>>()
                .join(",\n")
        ),
        Type::TypeVariable { name } => indent_string(name, indent_level * 2),
        Type::Underscore => "_".to_string(),
        Type::Union(UnionType { mut tags, .. }) => format!(
            "\n{}",
            {
                tags.sort_by(|a, b| a.tagname.cmp(&b.tagname));
                tags
            }
            .into_iter()
            .map(|tag| {
                let result = match tag.payload {
                    None => format!("| {}", tag.tagname),
                    Some(payload) => format!(
                        "| {}({})",
                        tag.tagname,
                        stringify_type(*payload, indent_level + 1)
                    ),
                };
                indent_string(result, indent_level * 2)
            })
            .collect::<Vec<String>>()
            .join("\n")
        ),
        Type::Record { mut key_type_pairs } => format!(
            "{{\n{}\n}}",
            {
                key_type_pairs.sort_by(|(a, _), (b, _)| a.cmp(&b));
                key_type_pairs
            }
            .into_iter()
            .map(|(key, type_value)| {
                let result = format!("{}: {}", key, stringify_type(type_value, indent_level * 2));
                indent_string(result, (indent_level + 1) * 2)
            })
            .collect::<Vec<String>>()
            .join("\n")
        ),
        Type::Function(function_type) => format!(
            "\\(\n{}\n) =>\n{}",
            vec![*function_type.first_argument_type]
                .into_iter()
                .chain(function_type.rest_arguments_types.into_iter())
                .map(|argument_type| { stringify_type(argument_type, indent_level + 1) })
                .collect::<Vec<String>>()
                .join(",\n"),
            stringify_type(*function_type.return_type, indent_level + 1)
        ),
    }
}
