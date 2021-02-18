use crate::tokenize::TokenizeError;
use crate::unify::{UnifyError, UnifyErrorKind};
use crate::{ast::*, compile::CompileError};
use crate::{
    compile::CompileErrorKind,
    parse::{ParseContext, ParseError, ParseErrorKind},
};
use crate::{pattern::ExpandablePattern, unify::Program};
use colored::*;
use core::panic;
use prettytable::{format::Alignment, Cell, Row, Table};
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub fn print_tokenize_error(program: Program, tokenize_error: TokenizeError) {
    match tokenize_error {
        TokenizeError::UnterminatedComment { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            print_error(program, range, StringifiedError {
                summary: "Syntax error: Unterminated comment".to_string(),
                body: "Comments must start with two slashes. Consider adding a slash (/) after here.".to_string()
            })
        }
        TokenizeError::InvalidToken { error, position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            print_error(
                program,
                range,
                StringifiedError {
                    summary: "Syntax error: Invalid token".to_string(),
                    body: error,
                },
            )
        }
        TokenizeError::UnknownCharacter { character } => {
            let range = ErrorRange {
                character_index_start: character.index,
                character_index_end: character.index,
            };
            print_error(
                program,
                range,
                StringifiedError {
                    summary: "Syntax error: Unknown character".to_string(),
                    body: "This character is not used anywhere in the syntax of KK.".to_string(),
                },
            )
        }
        TokenizeError::UnterminatedCharacterLiteral { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            print_error(
                program,
                range,
                StringifiedError {
                    summary: "Syntax error: unterminated character literal".to_string(),
                    body: "This character literal is not terminated, try adding single quote (') after here.".to_string(),
                },
            )
        }
        TokenizeError::UnterminatedStringLiteral { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            print_error(
                program,
                range,
                StringifiedError {
                    summary: "Syntax error: unterminated string literal".to_string(),
                    body: "This string is not terminated, try adding double quote (\") after here."
                        .to_string(),
                },
            )
        }
        TokenizeError::CharacterLiteralCannotBeEmpty { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };

            print_error(
                program,
                range,
                StringifiedError {
                    summary: "Syntax error: character literal cannot be empty".to_string(),
                    body: "Character literal must contain exactly one character. For example: 'x'"
                        .to_string(),
                },
            )
        }
    }
}

pub fn print_parse_error(program: Program, parse_error: ParseError) {
    let parse_context_description = get_parse_context_description(parse_error.context);
    let parse_context_description = format!(
        "We found this error when we are trying to parse a {}.\nExamples of {} are:\n\n{}",
        parse_context_description.name,
        parse_context_description.name,
        parse_context_description
            .examples
            .into_iter()
            .enumerate()
            .map(|(index, example)| format!(
                "Example #{}:\n\n{}",
                index + 1,
                indent_string(example.to_string(), 2)
            ))
            .collect::<Vec<String>>()
            .join("\n\n")
    );
    match parse_error.kind {
        ParseErrorKind::InvalidToken {
            actual_token,
            expected_token_type,
        } => {
            let range = ErrorRange {
                character_index_start: actual_token.position.character_index_start,
                character_index_end: actual_token.position.character_index_end,
            };
            let expected_token_message = match expected_token_type {
                None => "".to_string(),
                Some(expected_token_type) => {
                    format!(
                        "The expected token here is `{}`.\n\n",
                        stringify_token_type(expected_token_type)
                    )
                }
            };
            let actual_token_explanation = format!(
                "Note that `{}` is {}.\n\n",
                actual_token.representation,
                explain_token_type_usage(actual_token.token_type)
            );
            let error = StringifiedError {
                summary: format!(
                    "Syntax error: not expecting `{}` here",
                    actual_token.representation
                ),
                body: format!(
                    "{}{}{}",
                    expected_token_message, actual_token_explanation, parse_context_description
                ),
            };
            print_error(program, range, error)
        }
        ParseErrorKind::UnexpectedEOF {
            expected_token_type,
        } => {
            let range = ErrorRange {
                character_index_start: program.code.len() - 1,
                character_index_end: program.code.len() - 1,
            };
            let expected_token_message = match expected_token_type {
                None => "".to_string(),
                Some(expected_token_type) => {
                    format!(
                        "The expected token here is `{}`.\n\n",
                        stringify_token_type(expected_token_type)
                    )
                }
            };
            let error = StringifiedError {
                summary: "Syntax error: unexpected EOF (end of file)".to_string(),
                body: format!("{}{}", expected_token_message, parse_context_description),
            };
            print_error(program, range, error)
        }
    }
}

struct ParseContextDescription {
    name: &'static str,
    examples: Vec<&'static str>,
}

fn explain_token_type_usage(token_type: TokenType) -> &'static str {
    match token_type {
        TokenType::KeywordLet => "used for defining variables, for example `let x = 1`",
        TokenType::KeywordType => "used for defining type alias, for example `type People = { name: String }`",
        TokenType::KeywordEnum => "used for defining enum type (i.e. sum type or tagged union), for example `enum Color = Red() Blue()`",
        TokenType::KeywordDo => "used for defining expression with side effects, such as `do \"Hello world\".print()`",
        TokenType::KeywordElse => "only used in monadic let bindings, for example: `let Some(x) = y else | _ => \"Nope\"`",
        TokenType::KeywordNull => "only used to create a value with the null type (i.e. unit type)",
        TokenType::KeywordTrue | TokenType::KeywordFalse
            => "only used to create a boolean value",
        TokenType::KeywordImport => "only used for importing symbols from other files, for example: `import \"./foo.kk\" bar`",
        TokenType::KeywordExport => "only used for exporting symbols, for example: `export let foo = 1`",
        TokenType::Whitespace |TokenType::Newline => "meaningless in KK",
        TokenType::LeftCurlyBracket | TokenType::RightCurlyBracket => "used for declaring record type, for example `{ x: string }`, and constructing record value, for example `{ x = 'hello' }`",

        TokenType::LeftParenthesis | TokenType::RightParenthesis => "used for wrapping expressions and enum constructor only.",
        TokenType::LeftSquareBracket | TokenType::RightSquareBracket => "used for creating and destructuring array, for example `[1,2,3]`",
        TokenType::Colon => "only used for annotating types, for example `{ x: string }`",
        TokenType::DoubleColon => "only used for scope resolution, for example: `Color::Red()`",
        TokenType::LessThan | TokenType::MoreThan => "used for declaring type parameters, for example: `type Box<T> = { value: T }`",
        TokenType::Equals => "used for declaring variables, for example `let x = 1`",
        TokenType::Period => "used for calling a function, for example `1.add(2)`",
        TokenType::Spread => panic!("Subject to change"),
        TokenType::Comma => "used for record punning, for example `{x,}`",
        TokenType::Minus => "only used to represent negative numbers, for example `-123.4`",
        TokenType::FatArrowRight | TokenType::Pipe => "only used for creating function, for example `| x => x.add(1)`",
        TokenType::ThinArrowRight => "only used for annotating the return type of a function, for example `| x -> String => \"Hello\"`",
        TokenType::Underscore => "used in pattern matching to match values that are not used afterwards",
        TokenType::Identifier => "used to represent the name of a variable",
        TokenType::String => "only used to represent string values",
        TokenType::Float => "only used to represent floating point values",
        TokenType::Integer => "only used to represent integer values",
        TokenType::Character => "only used to represent character",
        TokenType::Comment => "only used for commenting",
        TokenType::Documentation => "only used for documentation"
    }
}

fn get_parse_context_description(parse_context: ParseContext) -> ParseContextDescription {
    match parse_context {
        ParseContext::Expression => ParseContextDescription {
            name: "Expression",
            examples: vec!["123", "\"hello world\"", "{ x 3 }", "[ 2 ]", "Some(true)"],
        },
        ParseContext::ExpressionLet => ParseContextDescription {
            name: "Let Expression",
            examples: vec![
                "let x = 1\nlet y = 2\nx.add(y)",
                "let Some(x) = a else | _ => Error(\"oops\")\nOk(x)",
            ],
        },
        ParseContext::ExpressionFunction => ParseContextDescription {
            name: "Function",
            examples: vec![
                "| x => x.add(1)",
                "| x:String  y:String -> String => x.concat(y)",
                "| true true => true\n| _ _ => false",
            ],
        },
        ParseContext::DotExpression => ParseContextDescription {
            name: "Dot Expression: Function Call, Property Access, or Record Update",
            examples: vec!["x.add(1)", "people.name", "x.{a 3}", "x.{a.square()}"],
        },
        ParseContext::ExpressionFunctionCall => ParseContextDescription {
            name: "Function call",
            examples: vec!["\"Hello world\".print()", "x.add(y)"],
        },
        ParseContext::ExpressionRecord => ParseContextDescription {
            name: "Record",
            examples: vec!["{ x 2 y 3 }", "{ x:Integer 2 }", "{ x, y, }"],
        },
        ParseContext::ExpressionRecordUpdate => ParseContextDescription {
            name: "Record Update",
            examples: vec!["a.{ x 2 }", "a.{ x.add(1) }", "a.{ x 2 y.square() }"],
        },
        ParseContext::ExpressionEnumConstructor => ParseContextDescription {
            name: "Enum Constructor",
            examples: vec!["None()", "Some(0)"],
        },
        ParseContext::Statement => ParseContextDescription {
            name: "Statement",
            examples: vec![
                "let x = 1",
                "type People = { name: String }",
                "do \"hello world\".print()",
                "enum Color = Red() Green()",
            ],
        },
        ParseContext::StatementImport => ParseContextDescription {
            name: "Import Statement",
            examples: vec![
                "import \"./foo.kk\" bar",
                "import \"./foo.kk\" bar = spam, baz",
            ],
        },
        ParseContext::StatementLet => ParseContextDescription {
            name: "Let Statement",
            examples: vec!["let x = 1", "let identity<T> = | x:T -> T => x"],
        },
        ParseContext::StatementType => ParseContextDescription {
            name: "Type Statement",
            examples: vec!["type ID = string", "type Box<T> = { value: T }"],
        },
        ParseContext::StatementEnum => ParseContextDescription {
            name: "Enum Statement",
            examples: vec![
                "enum Color = Red() Green()",
                "enum List<Element> = Nil() Cons({current: Element next: List<Element Element>})",
            ],
        },
        ParseContext::TypeAnnotationArray => ParseContextDescription {
            name: "Array Type Annotation",
            examples: vec!["[String]", "[[{x: Boolean}]]"],
        },
        ParseContext::TypeAnnotationRecord => ParseContextDescription {
            name: "Record Type Annotation",
            examples: vec!["{ x: String  y: { z: Integer } }"],
        },
        ParseContext::TypeAnnotationFunction => ParseContextDescription {
            name: "Function Type Annotation",
            examples: vec![
                "| Boolean -> Boolean",
                "| Integer Integer -> Integer",
                "| left:Integer right:Integer -> Integer",
            ],
        },
        ParseContext::Pattern => ParseContextDescription {
            name: "Pattern",
            examples: vec!["1", "\"hello world\"", "true", "Some(x)", "[]", "{ x, y, }"],
        },
        ParseContext::PatternArray => ParseContextDescription {
            name: "Array Pattern",
            examples: vec!["[]", "[head ...tail]"],
        },
        ParseContext::PatternEnum => ParseContextDescription {
            name: "Enum Pattern",
            examples: vec!["None()", "Some(x)"],
        },
        ParseContext::PatternRecord => ParseContextDescription {
            name: "Record Pattern",
            examples: vec!["{ x, y, }", "{ x true y Nil() }"],
        },
        ParseContext::TypeArguments => ParseContextDescription {
            name: "Type Arguments",
            examples: vec!["<Element = number>"],
        },
        ParseContext::EnumConstructorDefinition => ParseContextDescription {
            name: "Enum Constructor Definition",
            examples: vec!["Hello(number)", "Apple({ name: string })", "Bomb()"],
        },
        ParseContext::TypeVariablesDeclaration => ParseContextDescription {
            name: "Type Variables Declaration",
            examples: vec!["<T>", "<T U>"],
        },
        ParseContext::FunctionArguments => ParseContextDescription {
            name: "Function Arguments",
            examples: vec!["x", "x:Number"],
        },
    }
}

fn stringify_token_type(token_type: TokenType) -> &'static str {
    match token_type {
        TokenType::KeywordLet => "let",
        TokenType::KeywordType => "type",
        TokenType::KeywordEnum => "enum",
        TokenType::KeywordDo => "do",
        TokenType::KeywordElse => "else",
        TokenType::KeywordNull => "null",
        TokenType::KeywordTrue => "true",
        TokenType::KeywordFalse => "false",
        TokenType::KeywordImport => "import",
        TokenType::KeywordExport => "export",
        TokenType::Whitespace => " ",
        TokenType::LeftCurlyBracket => "{",
        TokenType::RightCurlyBracket => "}",
        TokenType::LeftParenthesis => "(",
        TokenType::RightParenthesis => ")",
        TokenType::LeftSquareBracket => "[",
        TokenType::RightSquareBracket => "]",
        TokenType::Newline => "\n",
        TokenType::Colon => ":",
        TokenType::DoubleColon => "::",
        TokenType::LessThan => "<",
        TokenType::MoreThan => ">",
        TokenType::Equals => "=",
        TokenType::Period => ".",
        TokenType::Spread => "...",
        TokenType::Comma => ",",
        TokenType::Minus => "-",
        TokenType::FatArrowRight => "=>",
        TokenType::ThinArrowRight => "->",
        TokenType::Pipe => "|",
        TokenType::Underscore => "_",
        TokenType::Identifier => "abc",
        TokenType::String => "\"abc\"",
        TokenType::Character => "'c'",
        TokenType::Float => "123.0",
        TokenType::Integer => "123",
        TokenType::Comment => "// this is a comment",
        TokenType::Documentation => "/// This is a documentation.",
    }
}

pub fn print_compile_error(CompileError { kind, program }: CompileError) {
    match kind {
        CompileErrorKind::TokenizeError(tokenize_error) => {
            print_tokenize_error(program, tokenize_error)
        }
        CompileErrorKind::ParseError(parse_error) => print_parse_error(program, *parse_error),
        CompileErrorKind::UnifyError(unify_error) => print_unify_error(program, *unify_error),
    }
}

pub fn print_unify_error(program: Program, unify_error: UnifyError) {
    let error = stringify_unify_error_kind(unify_error.kind);
    let range = ErrorRange {
        character_index_start: unify_error.position.character_index_start,
        character_index_end: unify_error.position.character_index_end,
    };

    print_error(program, range, error)
}

struct ErrorRange {
    character_index_start: usize,
    character_index_end: usize,
}

fn print_error(program: Program, range: ErrorRange, error: StringifiedError) {
    let origin = match program.source {
        Source::File { path } => path,
        Source::NonFile { env_name, .. } => env_name,
    };

    let range = {
        let start = range.character_index_start;
        let end = range.character_index_end;
        (start, end + 1)
    };

    let mut files = SimpleFiles::new();

    let file_id = files.add(origin, program.code);
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
        UnifyErrorKind::UnknownEnumConstructor => StringifiedError {
            summary: "Unknown enum constructor".to_string(),
            body: "Cannot find this enum constructor in the current scope".to_string(),
        },
        UnifyErrorKind::MissingCases(missing_patterns) => StringifiedError {
            summary: "Non-exhaustive cases".to_string(),
            body: format!(
                "Missing case(s):\n\n{}",
                missing_patterns
                    .into_iter()
                    .map(stringify_expandable_pattern)
                    .map(|result| format!("  | {} => ...", result))
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
            body: "This variable is declared but not used anywhere, consider removing it.".to_string()
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
        UnifyErrorKind::DuplicatedIdentifier {
            ..
        } => StringifiedError {
            summary: "Duplicated name".to_string(),
            body: "This variable is already declared before in this namespace.".to_string()
        },
        UnifyErrorKind::ConflictingFunctionDefinition {
            function_name,
            existing_first_parameter_type,
            new_first_parameter_type,
            ..
        } => StringifiedError {
            summary: "Conflicting function definition".to_string(),
            body: format!(
                "The first parameter type of this `{}`:\n\n{}\n\noverlaps with the first parameter type of another `{}` in this scope:\n\n{}\n\n", 
                function_name,
                stringify_type(new_first_parameter_type, 2),
                function_name,
                stringify_type(existing_first_parameter_type, 2),
            )
        },
        UnifyErrorKind::AmbiguousConstructorUsage {
            constructor_name,
            possible_enum_names
        } => StringifiedError {
            summary: "Ambiguous Constructor Usage".to_string(),
            body: format!(
                "The constructor `{}` belongs to more than one enums, which are:\n\n{}\n\n{}\n\n{}",
                constructor_name,
                indent_string(possible_enum_names.clone().into_vector().join("\n"), 2),
                "You can use type annotation to resolve this problem, for example:",
                indent_string(format!("let x: {} = {}()", possible_enum_names.first(), constructor_name), 2)
            )
        },
        UnifyErrorKind::AmbiguousFunction { available_function_signatures } => StringifiedError {
            summary: "Ambiguous Reference".to_string(),
            body: format!(
                "This function is overloaded with multiple signatures, but the first argument provided has unknown type.\n{}\n\n{}",
                "To disambiguate, annotate the type of the first argument of this function call with any of the following types:",
                indent_string(
                    available_function_signatures.into_iter().map(|signature| {
                        stringify_type(signature.function_type.parameters_types.first().clone(), 0)
                    })
                    .collect::<Vec<String>>()
                    .join("\n\n"),
                    2
                ),
            )
        },
        UnifyErrorKind::DoBodyMustHaveNullType { actual_type} => StringifiedError {
            summary: "Type Mismatch".to_string(),
            body: format!(
                "The body of do-expression must have the type of Null.\n{}\n\n{}",
                "But this expression has the type of:",
                stringify_type(actual_type, 2)
            )
        },
        UnifyErrorKind::ThisEnumConstructorDoesNotRequirePayload => StringifiedError {
            summary: "Unexpected Payload".to_string(),
            body: "This enum constructor does not require payload, consider removing this expression.".to_string()
        },
        UnifyErrorKind::ThisEnumConstructorRequiresPaylod { payload_type} => StringifiedError {
            summary: "Missing Payload".to_string(),
            body: format!(
                "{}\n\n{}", 
                "This enum constructor requires a payload with the type of:", 
                stringify_type(payload_type, 2)
            )
        },
        UnifyErrorKind::CannotPerformRecordUpdateOnNonRecord { actual_type } => StringifiedError {
            summary: "This is not a record.".to_string(),
            body: format!(
                "{}{}{}{}",
                "You can only perform record update on expression that has the type of Record, ",
                "for example: `{ name: String }`.\n",
                "But the type of this expression is:\n\n",
                stringify_type(actual_type, 2)
            )

        },
        UnifyErrorKind::NoMatchingFunction { actual_first_argument_type, expected_first_argument_types } => StringifiedError {
            summary: "No matching overload found.".to_string(),
            body: format!(
                "\n{}\n\n{}\n\n{}\n\n{}",
                "I cannot find a version of this function that takes the following type as first argument:",
                stringify_type(actual_first_argument_type, 2),
                "But I found other versions of this function that take any of the following type as first argument:",
                expected_first_argument_types.into_iter().map(|type_value| stringify_type(type_value, 2)).collect::<Vec<String>>().join("\n\n")
            )
        },
        UnifyErrorKind::ErrorneousImportPath { extra_information } => StringifiedError {
            summary: "Errorneous Import Path.".to_string(),
            body: extra_information
        },
        UnifyErrorKind::UnknownImportedName => StringifiedError {
            summary: "Unknown Name".to_string(),
            body: "This name cannot be found in the imported file.".to_string()
        },
        UnifyErrorKind::CannotImportPrivateSymbol => StringifiedError {
            summary: "Cannot import private symbol.".to_string(),
            body: format!(
                "{}{}",
                "Consider exporting this symbol if you want to import it in this file.\nFor example:\n\n",
                "    export let x = 2"
            )
        },
        UnifyErrorKind::CyclicDependency {
            import_relations
        } => StringifiedError {
            summary: "Cyclic dependencies detected.".to_string(),
            body: format!(
                "Explanation:\n\n{}",
                indent_string(
                    import_relations.into_iter().map(|relation| {
                        format!("{} imports {}", relation.importer_path, relation.importee_path)
                    }).collect::<Vec<String>>().join("\n"), 
                    2
                )
            )
        }

    }
}

pub fn stringify_expandable_pattern(expandable_pattern: ExpandablePattern) -> String {
    match expandable_pattern {
        ExpandablePattern::EmptyArray => "[]".to_string(),
        ExpandablePattern::NonEmptyArray {
            first_element,
            rest_elements,
        } => {
            format!(
                "[{} ...{}]",
                stringify_expandable_pattern(*first_element),
                stringify_expandable_pattern(*rest_elements)
            )
        }
        ExpandablePattern::Boolean(value) => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        ExpandablePattern::Record { key_pattern_pairs } => format!(
            "{{ {} }}",
            key_pattern_pairs
                .into_iter()
                .map(|(key, pattern)| format!("{} {}", key, stringify_expandable_pattern(pattern)))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        ExpandablePattern::Tuple(patterns) => patterns
            .into_iter()
            .map(stringify_expandable_pattern)
            .collect::<Vec<String>>()
            .join(" "),
        ExpandablePattern::Any { .. } | ExpandablePattern::Infinite { .. } => "_".to_string(),
        ExpandablePattern::EnumConstructor { name, payload, .. } => format!(
            "{}({})",
            name,
            match payload {
                Some(payload) => stringify_expandable_pattern(*payload),
                None => "".to_string(),
            }
        ),
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
        Type::Boolean => indent_string("Boolean".to_string(), indent_level * 2),
        Type::Float => indent_string("Float".to_string(), indent_level * 2),
        Type::Integer => indent_string("Integer".to_string(), indent_level * 2),
        Type::Null => indent_string("Null".to_string(), indent_level * 2),
        Type::String => indent_string("String".to_string(), indent_level * 2),
        Type::Character => indent_string("Character".to_string(), indent_level * 2),
        Type::Array(element_type) => indent_string(
            format!("[\n{}\n]", stringify_type(*element_type, indent_level + 1)),
            indent_level * 2,
        ),
        Type::Named {
            name,
            type_arguments: arguments,
            ..
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
                            "{}\n{},",
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
                .map(|type_value| stringify_type(type_value, indent_level + 1))
                .into_vector()
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
                "| (\n{}\n) =>\n{}",
                function_type
                    .parameters_types
                    .map(|argument_type| { indent_string(stringify_type(argument_type, 0), 2) })
                    .into_vector()
                    .join(",\n"),
                indent_string(stringify_type(*function_type.return_type, 0), 2)
            );
            indent_string(result, indent_level * 2)
        }
    }
}
