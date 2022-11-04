use crate::{compile::CompileError, raw_ast::*, typ::*};
use crate::{
    compile::CompileErrorKind,
    parse::{ParseContext, ParseError, ParseErrorKind},
};
use crate::tokenize::TokenizeError;
use crate::{module::ModuleMeta, pattern::ExpandablePattern};
use crate::{
    pattern::CheckablePatternKind,
    unify::{UnifyError, UnifyErrorKind},
};
use colored::*;
use prettytable::{format::Alignment, Cell, Row, Table};
use std::fs;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

fn get_tokenize_error(tokenize_error: TokenizeError) -> (ErrorRange, StringifiedError) {
    match tokenize_error {
        TokenizeError::UnterminatedMultilineComment { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            (
                range,
                StringifiedError {
                    summary: "Syntax error: Unterminated multiline comment".to_string(),
                    body: "Multiline comment must start and end with three hashes. Consider adding triple hash (###) at the end.".to_string()
                }
            )
        }
        TokenizeError::InvalidToken { error, position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            (
                range,
                StringifiedError {
                    summary: "Syntax error: Invalid token".to_string(),
                    body: error,
                },
            )
        }
        TokenizeError::UnterminatedCharacterLiteral { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            (
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
            (
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
            (
                range,
                StringifiedError {
                    summary: "Syntax error: character literal cannot be empty".to_string(),
                    body: "Character literal must contain exactly one character. For example: 'x'"
                        .to_string(),
                },
            )
        }
        TokenizeError::UnexpectedCharacter {
            position,
            expected_character_value,
        } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            (
                range,
                StringifiedError {
                    summary: "Syntax error: unexpected character".to_string(),
                    body: format!("The expected character is: {}", expected_character_value),
                },
            )
        }
        TokenizeError::UnexpectedEof {
            expected_character_value,
        } => {
            let range = ErrorRange {
                character_index_end: 0,
                character_index_start: 0,
            };
            (
                range,
                StringifiedError {
                    summary: "Syntax error: unexpected EOF (end of file)".to_string(),
                    body: format!("The expected character is: {}", expected_character_value),
                },
            )
        }
    }
}

pub fn print_parse_error(filename: String,code: String, parse_error: ParseError) {
    let parse_context_description = {
        match parse_error.context {
            Some(context) => {
                let parse_context_description = get_parse_context_description(context);
                Some(format!(
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
                ))
            }
            None => None,
        }
    }.unwrap_or_else(||"".to_string());
    let (range, error) = match parse_error.kind {
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
            (range, error)
        }
        ParseErrorKind::UnexpectedEof {
            expected_token_type,
        } => {
            let range = ErrorRange {
                character_index_start: code.len() - 1,
                character_index_end: code.len() - 1,
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
            (range, error)
        }
        ParseErrorKind::RecordWilcardCanOnlyAppearOnce { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            let error = StringifiedError {
                summary: "Extraneous wildcard operator".to_string(),
                body: "Wildcard operator (..) can only appear once in a record expression."
                    .to_string(),
            };
            (range, error)
        }
        ParseErrorKind::ExpectedPattern { position } => {
            let range = ErrorRange {
                character_index_start: position.character_index_start,
                character_index_end: position.character_index_end,
            };
            let error = StringifiedError {
                summary: "Expected pattern here".to_string(),
                body: "Unable to convert this expression into a destructure pattern.".to_string(),
            };
            (range, error)
        }
        ParseErrorKind::TokenizeError(tokenize_error) => get_tokenize_error(tokenize_error),
    };
    print_error(filename, code, range, error)
}

struct ParseContextDescription {
    name: &'static str,
    examples: Vec<&'static str>,
}

fn explain_token_type_usage(token_type: TokenType) -> &'static str {
    match token_type {
        TokenType::KeywordLet => "used for defining variables, for example:\n\n\tlet x = 1",
        TokenType::KeywordType => "used for defining type alias, for example:\n\n\ttype People = { name: String }",
        TokenType::KeywordImport => "only used for importing symbols from other files, for example:\n\n\timport \"./foo.kk\" { bar spam hello: hello2}",
        TokenType::KeywordPublic => "only used for declaring public symbols, for example:\n\n\tpublic let foo = 1",
        TokenType::KeywordPrivate => "only used for declaring private symbols, for example:\n\n\tpublic let foo = 1",
        TokenType::Whitespace |TokenType::Newline => "meaningless in KK",
        TokenType::LeftCurlyBracket | TokenType::RightCurlyBracket => "used for declaring record type, for example:\n\n\t{ x: String }\n\nand constructing record value, for example: \n\n\t{ x: \"hello\" }",

        TokenType::LeftParenthesis | TokenType::RightParenthesis => "used for wrapping expressions and enum constructor only",
        TokenType::LeftSquareBracket | TokenType::RightSquareBracket => "used for creating and destructuring array, for example:\n\n\t[1 2 3]",
        TokenType::Colon => "only used for annotating types, for example:\n\n\t{ x: string }",
        TokenType::DoubleColon => "only used for scope resolution, for example:\n\n\tColor::Red()",
        TokenType::LessThan | TokenType::MoreThan => "used for declaring type parameters, for example:\n\n\ttype Box<T> = { value: T }",
        TokenType::Equals => "used for declaring variables locally, for example:\n\n\tlet x = 1",
        TokenType::Period => "used for calling a function, for example:\n\n\t1.add(2)",
        TokenType::DoublePeriod => "used for record wildcard, for example:\n\n\tlet f : |{x: Integer y: Integer} => Integer = |{..} => x.plus(y)",
        TokenType::Comma => "used for separation",
        TokenType::ArrowRight  => "only used for creating function, for example:\n\n\t| x => x.add(1)",
        TokenType::Pipe => "used in tagged union and or-pattern.",
        TokenType::Underscore => "used in pattern matching to match values that are not used afterwards",
        TokenType::Identifier => "used to represent the name of a variable",
        TokenType::InterpolatedString{..} | TokenType::String{..} => "only used to represent string values",
        TokenType::Float => "only used to represent floating point values",
        TokenType::Integer => "only used to represent integer values",
        TokenType::Character => "only used to represent character",
        TokenType::Comment => "only used for commenting",
        TokenType::MultilineComment  => "only used for documentation",
        TokenType::Bang => "used for annotating Promise type, for example: \n\n\t!Integer",
        TokenType::Other(_) => "not used anywhere in the syntax of KK",
        TokenType::Tag => "used as constructor of enum",
        TokenType::Operator => "used for defining symbolic functions",
        TokenType::KeywordEntry => "used for defining the entry point of a file",
        TokenType::Semicolon => "used for separating statements",
        TokenType::Tilde => "used for declaring the start of a CPS Sugar",
        TokenType::KeywordExists => "used for declaring constraints",
        TokenType::Backslash => "used for declaring lambda",
        TokenType::KeywordAs => todo!(),
        TokenType::HashLeftCurlyBracket => "used for declaring record",
        TokenType::KeywordCase => "used for declaring variants, or in pattern matching",

    }
}

fn get_parse_context_description(parse_context: ParseContext) -> ParseContextDescription {
    match parse_context {
        ParseContext::Expression => ParseContextDescription {
            name: "Expression",
            examples: vec!["123", "\"hello world\"", "{ x: 3 }", "[ 2 ]", "Some(true)"],
        },
        ParseContext::ExpressionFunction => ParseContextDescription {
            name: "Function",
            examples: vec!["x => x.add(1)", "(x: Boolean, y: Integer): Boolean => true"],
        },
        ParseContext::DotExpression => ParseContextDescription {
            name: "Dot Expression: Function Call, Property Access, or Record Update",
            examples: vec!["x.add(1)", "people.name", "x.{a: 3}", "x.{a.square}"],
        },
        ParseContext::ExpressionFunctionCall => ParseContextDescription {
            name: "Function call",
            examples: vec!["\"Hello world\".print()", "x.add(y)"],
        },
        ParseContext::ExpressionRecord => ParseContextDescription {
            name: "Record",
            examples: vec!["{ x: 2, y: 3 }"],
        },
        ParseContext::ExpressionRecordUpdate => ParseContextDescription {
            name: "Record Update",
            examples: vec!["a.{ x: 2 }", "a.{ x.add(1) }", "a.{ x: 2, y.square() }"],
        },
        ParseContext::ExpressionEnumConstructor => ParseContextDescription {
            name: "Enum Constructor",
            examples: vec!["None", "Some(0)"],
        },
        ParseContext::Statement => ParseContextDescription {
            name: "Statement",
            examples: vec![
                "let x = 1",
                "type People = { name: String }",
                "do \"hello world\".print()",
                "enum Color { Red, Green }",
            ],
        },
        ParseContext::StatementImport => ParseContextDescription {
            name: "Import Statement",
            examples: vec![
                "import { bar } from \"./foo.kk\"",
                "import { bar as spam, baz } from \"./foo.kk\"",
            ],
        },
        ParseContext::StatementLet => ParseContextDescription {
            name: "Let Statement",
            examples: vec!["let x : Integer = 1", "let identity = <T>(t: T): T => t"],
        },
        ParseContext::StatementType => ParseContextDescription {
            name: "Type Statement",
            examples: vec!["type ID = String", "type Box<T> = { value: T }"],
        },
        ParseContext::StatementEnum => ParseContextDescription {
            name: "Enum Statement",
            examples: vec![
                "enum Color { Red, Green }",
                "enum List<A> { Nil, Cons({head: A, tail: List<A>}) }",
            ],
        },
        ParseContext::TypeAnnotationArray => ParseContextDescription {
            name: "Array Type Annotation",
            examples: vec!["[String]", "[[{x: Boolean}]]"],
        },
        ParseContext::TypeAnnotationRecord => ParseContextDescription {
            name: "Record Type Annotation",
            examples: vec!["{ x: String,  y: { z: Integer } }"],
        },
        ParseContext::TypeAnnotationFunction => ParseContextDescription {
            name: "Function Type Annotation",
            examples: vec![
                "{ Int -> Boolean }",
                "{ Int Int -> Boolean }",
                "{ [T] Int -> Boolean }",
                "{ [T] Int -> Boolean where (==): { Int Int -> Boolean } }",
            ],
        },
        ParseContext::Pattern => ParseContextDescription {
            name: "Pattern",
            examples: vec!["1", "\"hello world\"", "true", "Some(x)", "[]", "{ x, y }"],
        },
        ParseContext::PatternArray => ParseContextDescription {
            name: "Array Pattern",
            examples: vec!["[]", "[head ...tail]"],
        },
        ParseContext::PatternEnum => ParseContextDescription {
            name: "Enum Pattern",
            examples: vec!["None", "Some(x)"],
        },
        ParseContext::PatternRecord => ParseContextDescription {
            name: "Record Pattern",
            examples: vec!["{ x, y }", "{ x: true, y: Nil }"],
        },
        ParseContext::TypeArguments => ParseContextDescription {
            name: "Type Arguments",
            examples: vec!["<Element>"],
        },
        ParseContext::EnumConstructorDefinition => ParseContextDescription {
            name: "Enum Constructor Definition",
            examples: vec!["Hello(number)", "Apple({ name: string })", "Bomb"],
        },
        ParseContext::TypeVariablesDeclaration => ParseContextDescription {
            name: "Type Variables Declaration",
            examples: vec!["<T>", "<T, U>"],
        },
        ParseContext::DocumentationCodeSnippet => ParseContextDescription {
            name: "Documentation Code Snippet",
            examples: vec!["```\"Hello world\".print()```"],
        },
        ParseContext::FunctionParameters => ParseContextDescription {
            name: "Function Parameters",
            examples: vec!["(a: string, b: number)"],
        },
        ParseContext::TypeVariableConstraint => ParseContextDescription {
            name: "Type Variable Constraint",
            examples: vec!["Equatable<A>"],
        },
        ParseContext::Lambda => ParseContextDescription {
            name: "Lambda",
            examples: vec![
                "{ 'Hello world' print }",
                "{ x -> x + 2 }",
                "{ x y -> x + y }",
                "{ | true -> false | false -> true }",
            ],
        },
        ParseContext::EntryStatement => ParseContextDescription {
            name: "Entry Statement",
            examples: vec!["entry 'Hello world' print"],
        },
        ParseContext::BlockLevelStatement => ParseContextDescription {
            name: "Block Level Statement",
            examples: vec!["x = 123", "'Hello world' print"],
        },
    }
}

fn stringify_token_type(token_type: TokenType) -> &'static str {
    match token_type {
        TokenType::KeywordLet => "let",
        TokenType::KeywordType => "type",
        TokenType::KeywordImport => "import",
        TokenType::KeywordPublic => "public",
        TokenType::KeywordPrivate => "private",
        TokenType::KeywordEntry => "entry",
        TokenType::KeywordExists => "exists",
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
        TokenType::DoublePeriod => "..",
        TokenType::Comma => ",",
        TokenType::ArrowRight => "->",
        TokenType::Pipe => "|",
        TokenType::Underscore => "_",
        TokenType::Identifier => "abc",
        TokenType::Character => "'c'",
        TokenType::Float => "123.0",
        TokenType::Integer => "123",
        TokenType::Comment => "// this is a comment",
        TokenType::MultilineComment { .. } => "/* This is multiline comment */", 
        TokenType::Bang => "!",
        TokenType::String{..} => "\"Hello \"",
        TokenType::InterpolatedString { .. } => "\"Hello #{my_name}\"",
        TokenType::Other(_) => "unknown character",
        TokenType::Tag => "tag",
        TokenType::Operator => todo!(),
        TokenType::Semicolon => ";",
        TokenType::Tilde => "~",
        TokenType::Backslash => "\\",
        TokenType::KeywordAs => "as",
        TokenType::HashLeftCurlyBracket => "#{",
        TokenType::KeywordCase => "case",
    }
}

pub fn print_compile_error(CompileError { kind, path }: CompileError) {
    let filename = path.to_str().unwrap().to_string();
    let code = fs::read_to_string(path).unwrap();
    match kind {
        CompileErrorKind::ParseError(parse_error) => print_parse_error(filename, code, *parse_error),
        CompileErrorKind::UnifyError(unify_error) => print_unify_error(filename, code, *unify_error),
    }
}

pub fn print_unify_error(filename: String, code: String, unify_error: UnifyError) {
    let error = stringify_unify_error_kind(unify_error.kind);
    let range = ErrorRange {
        character_index_start: unify_error.position.character_index_start,
        character_index_end: unify_error.position.character_index_end,
    };

    print_error(filename, code, range, error)
}

struct ErrorRange {
    character_index_start: usize,
    character_index_end: usize,
}

fn print_error(filename: String, code: String, range: ErrorRange, error: StringifiedError) {
    let range = {
        let start = range.character_index_start;
        let end = range.character_index_end;
        (start, end + 1)
    };

    let mut files = SimpleFiles::new();

    let file_id = files.add(filename, code);
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
        UnifyErrorKind::InvalidArgumentLength {
            expected_length,
            actual_length,
        } => StringifiedError {
            summary: "Arguments length mismatch".to_string(),
            body: format!(
                "Expected {} {}{}, but {}{} {} {} provided.",
                expected_length,
                pluralize("argument".to_string(), expected_length),
                if expected_length < actual_length {" only"} else {""},
                if actual_length < expected_length {"only "} else {""},
                actual_length,
                pluralize("argument".to_string(), actual_length),
                if actual_length > 1 {"are"} else {"is"}
            ),
        },
        UnifyErrorKind::CannotInvokeNonFunction { .. } => StringifiedError {
            summary: "Cannot call a non-function".to_string(),
            body: "The type of this expression is not function, so you cannot call it.".to_string(),
        },
        UnifyErrorKind::UnknownValueSymbol {symbol_name} => StringifiedError {
            summary: format!("Unknown variable `{}`", symbol_name),
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
                    .map(|result| format!("  {}", result))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        },
        UnifyErrorKind::PartiallyUnreachableCase {redundant_expanded_pattern: redundant_pattern} => StringifiedError {
            summary: "Partially unreachable case".to_string(),
            body: format!(
                "This case is partially unreachable because it contains a redundant pattern that is already handled by previous cases.\nNamely:\n\n{}",
                indent_string(
                    redundant_pattern.to_string(),
                    4
                )
            )
        },
        UnifyErrorKind::UnreachableCase => StringifiedError {
            summary: "Unreachable case".to_string(),
            body: "This case is unreachable as it is already handled by previous cases.".to_string()
        },
        UnifyErrorKind::NoSuchProperty {
            mut expected_keys
        } => {
            expected_keys.sort();
            StringifiedError{
            summary: "No such property".to_string(),
            body: format!("Available properties:\n{}", indent_string(expected_keys.join("\n"), 2))
        } },
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
            missing_keys
        } => StringifiedError {
            summary: "Missing properties".to_string(),
            body: {
                let mut missing_keys = missing_keys.into_vector();
                missing_keys.sort();
                format!("The missing properties are:\n{}", indent_string(missing_keys.join("\n"), 2))
            }
        },
        UnifyErrorKind::DuplicatedIdentifier {
            name,
            ..
        } => StringifiedError {
            summary: "Duplicated name".to_string(),
            body: format!("This variable `{}` is already declared before in this module.", name)
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
                        todo!()
                    })
                    .collect::<Vec<String>>()
                    .join("\n\n"),
                    2
                ),
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
        UnifyErrorKind::NoMatchingValueSymbol {possible_value_symbols} => StringifiedError {
            summary: "No matching overload found.".to_string(),
            body: format!(
                "todo"
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
        },
        UnifyErrorKind::WithExpressionBindFunctionConditionNotMet { actual_type } => StringifiedError {
            summary: "Unsatifactory bind function.".to_string(),
            body: format!("The bind function must satisfies all of the following conditions:\n{}\n\n{}\n\n{}", 
                vec![
                    "  (1) Must be a two-parameter function",
                    "  (2) The expected second parameter type must be type of function",
                ].join("\n"), 
                "But the given bind function has the following type:", 
                stringify_type(actual_type, 2)
            )
        },
        UnifyErrorKind::LetBindingRefutablePattern { missing_patterns } => StringifiedError {
            summary: "Let Binding Refutable Pattern".to_string(),
            body: format!(
                "{}\n{}\n{}",
                "Refutable pattern (non-exhaustive) pattern is not allowed for let binding.",
                "The cases that are not matched are:\n",
                missing_patterns
                    .into_iter()
                    .map(|pattern| indent_string(stringify_expandable_pattern(pattern), 2))
                    .collect::<Vec<String>>()
                    .join("\n")
            )
        },
        UnifyErrorKind::MissingTypeAnnotationForRecordWildcard => StringifiedError {
            summary: "Missing Type Annotation for Record Wildcard".to_string(),
            body: "Without type annotation, the compiler cannot populate the properties automatically.\nConsider adding type annotation on the outer level to fix this problem.".to_string(),
        },
        UnifyErrorKind::MissingParameterTypeAnnotationForTopLevelFunction => StringifiedError {
            summary: "Missing Parameter Type Annotation".to_string(),
            body: "Parameter type annotation is required for top-level function to enable single dispatch disambiguation.".to_string()
        },
        UnifyErrorKind::MissingReturnTypeAnnotationForTopLevelFunction => StringifiedError {
            summary: "Missing Return Type Annotation".to_string(),
            body: "Return type annotation is required for top-level function.".to_string()
        },
        UnifyErrorKind::MissingParameterTypeAnnotationForFunctionTypeAnnotation => StringifiedError {
            summary: "Missing Parameter Type Annotation".to_string(),
            body: "Parameter type annotation is required for function type annotation.".to_string()
        },
        UnifyErrorKind::NotExpectingFunction { expected_type } => StringifiedError{
            summary: "Not Expecting Function".to_string(),
            body: format!("The expected type is:\n\n{}", stringify_type(expected_type, 2))
        },
        UnifyErrorKind::FunctionCallAtTopLevelIsNotAllowed => StringifiedError {
            summary: "Top level function call is not allowed".to_string(),
            body: "This is to ensure that importing a module does not has side effects".to_string()
        },
        UnifyErrorKind::CannotAccessPropertyOfNonRecord => StringifiedError {
            summary: "Cannot access property of a non-record".to_string(),
            body: "".to_string()
        },
        UnifyErrorKind::NotExpectingTypeVariable => StringifiedError {
            summary: "Not expecting type variable here".to_string(),
            body: "".to_string()
        },
        UnifyErrorKind::MissingBindings { missing_expected_bindings } => StringifiedError {
            summary:"Missing bindings".to_string(),
            body: format!(
                "{}\n\n{}", 
                "Every pattern in an OR pattern must have the same set of bindings, however this pattern is missing the following binding(s) which are found in preceding pattern(s):",
                    missing_expected_bindings.map(|binding| indent_string(binding.identifier.token.representation, 4))
                    .into_vector().join("\n")
            )
        },
        UnifyErrorKind::ExtraneousBinding {extraneous_binding ,expected_bindings} => StringifiedError {
            summary: "Extraneous Binding".to_string(),
            body: format!(
                "All preceding patterns does not have this binding, namely `{}`.\n{}\n{}\n\n{}",
                extraneous_binding.identifier.token.representation,
                "Note that every patterns in an OR pattern must have the same set of bindings.",
                "In this case:",
                expected_bindings.into_iter().map(|binding|
                    indent_string(binding.identifier.token.representation, 4)
                )
                .collect::<Vec<String>>()
                .join("\n")
            )
        },
        UnifyErrorKind::UnsatisfiedConstraint { missing_constraint } => StringifiedError {
            summary: "Unsatisfied Constraint".to_string(),
            body: format!(
                "The following variable is required but not found:\n\n{}: {}",
                missing_constraint.name,
                stringify_type(missing_constraint.type_value, 0)
                
            )
        },
        UnifyErrorKind::ConstraintUnsatisfied { interface_name, for_types } => StringifiedError {
            summary: "Constraint unsatisfied".to_string(),
            body: format!(
                "The following implementation is needed:\n\n{}<{}>",
                indent_string(interface_name, 4),
                for_types
                    .into_vector()
                    .into_iter()
                    .map(|for_type| stringify_type(for_type, 0))
                    .collect::<Vec<String>>().join(",")
            )
        },
        UnifyErrorKind::UnknownTypeVariable { unknown_type_variable_name } => StringifiedError {
            summary: "Unknown type variable".to_string(),
            body: format!("No type variable has the name of {} in this scope.", unknown_type_variable_name)
        },
        UnifyErrorKind::TopLevelLetStatementCannotBeDestructured => todo!(),
        UnifyErrorKind::MissingTypeAnnotationForTopLevelBinding => todo!(),
        UnifyErrorKind::CannotBeOverloaded {name} => StringifiedError { 
            summary: format!("`{}` cannot be overloaded", name), 
            body: "".to_string() 
        },
        UnifyErrorKind::AmbiguousSymbol { matching_value_symbols } => StringifiedError { 
            summary: "Ambiguous Symbol".to_string(), 
            body: format!(
                "Matching types:\n\n{}\n\nPut type annotation for disambiguation.", 
                matching_value_symbols
                  .into_iter()
                  .map(|(_, symbol)| stringify_type(symbol.type_value, 1))
                  .collect::<Vec<String>>().join("\n\n")
            )
        },
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
        ExpandablePattern::Record { key_pattern_pairs } => format!(
            "({})",
            key_pattern_pairs
                .into_iter()
                .map(|(key, pattern)| format!("{} = {}", key, stringify_expandable_pattern(pattern)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        ExpandablePattern::Tuple(patterns) => patterns
            .into_iter()
            .map(stringify_expandable_pattern)
            .collect::<Vec<String>>()
            .join(" "),
        ExpandablePattern::Any { .. } | ExpandablePattern::Infinite { .. } => "_".to_string(),
        ExpandablePattern::EnumConstructor { name, payload, .. } => match payload {
            Some(payload) => format!("{}({})", name, stringify_expandable_pattern(*payload),),
            None => name,
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
        Type::Float => indent_string("Float".to_string(), indent_level * 2),
        Type::Integer => indent_string("Int".to_string(), indent_level * 2),
        Type::Unit => indent_string("()".to_string(), indent_level * 2),
        Type::String => indent_string("String".to_string(), indent_level * 2),
        Type::Character => indent_string("Character".to_string(), indent_level * 2),
        Type::BuiltInOneArgumentType {
            kind,
            type_argument,
        } => match kind {
            BuiltInOneArgumentTypeKind::Array => indent_string(
                format!("[\n{}\n]", stringify_type(*type_argument, indent_level + 1)),
                indent_level * 2,
            ),
            BuiltInOneArgumentTypeKind::Quoted => {
                format!("`{}`", stringify_type(*type_argument, indent_level))
            }
        },
        Type::Named {
            name,
            type_arguments: arguments,
            ..
        } => {
            if arguments.is_empty() {
                name
            } else {
                let result = format!(
                    "{}<{}>",
                    name,
                    arguments
                        .into_iter()
                        .map(|(_, type_value)| stringify_type(type_value, 0))
                        .collect::<Vec<String>>()
                        .join(", ")
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
        Type::ImplicitTypeVariable(type_variable) => {
            indent_string(type_variable.name, indent_level * 2)
        }
        Type::ExplicitTypeVariable(type_variable) => {
            indent_string(type_variable.name, indent_level * 2)
        }
        Type::Underscore => "_".to_string(),
        Type::Record { mut key_type_pairs } => {
            key_type_pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
            let result = format!(
                "#{{{}}}",
                key_type_pairs
                    .into_iter()
                    .map(|(key, type_value)| {
                        format!(
                            "{}: {}",
                            key,
                            stringify_type(type_value, 0)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            indent_string(result, indent_level * 2)
        }
        Type::Function(function_type) => {
            let result = format!(
                "{} -> {}",
                stringify_type(*function_type.parameter_type, 0),
                stringify_type(*function_type.return_type, 0)
            );
            indent_string(result, indent_level * 2)
        }
        Type::TypeScheme(type_scheme) => {
            format!(
                "<{}>{}",
                type_scheme
                    .type_variables
                    .into_vector()
                    .into_iter()
                    .map(|type_variable| type_variable.name)
                    .collect::<Vec<String>>()
                    .join(", "),
                stringify_type(type_scheme.type_value, indent_level)
            )
        }
    }
}

impl CheckablePatternKind {
    pub fn to_string(&self) -> String {
        match self {
            CheckablePatternKind::Infinite { kind, token } => match kind {
                InfinitePatternKind::String => format!("\"{}\"", token.representation),
                InfinitePatternKind::Character => format!("'{}'", token.representation),
                InfinitePatternKind::Integer => token.representation.clone(),
            },
            CheckablePatternKind::Boolean { token, .. } => token.representation.clone(),
            CheckablePatternKind::Unit{..} => "()".to_string(),
            CheckablePatternKind::Underscore(_) => "_".to_string(),
            CheckablePatternKind::Identifier(identifier) => identifier.token.representation.clone(),
            CheckablePatternKind::EnumConstructor {
                constructor_name,
                payload,
            } => match payload {
                Some(payload) => {
                    format!(
                        "{}({})",
                        constructor_name.representation,
                        payload.kind.to_string()
                    )
                }
                None => constructor_name.representation.clone(),
            },
            CheckablePatternKind::Record {
                key_pattern_pairs, ..
            } => format!(
                "({})",
                key_pattern_pairs
                    .iter()
                    .map(|(property_name, pattern)| format!(
                        "{} = {}",
                        property_name.0.representation,
                        pattern.kind.to_string()
                    ))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CheckablePatternKind::Array { .. } => {
                panic!()
            }
            CheckablePatternKind::Tuple { .. } => {
                panic!()
            }
        }
    }
}
