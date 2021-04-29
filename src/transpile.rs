//! This file is temporarily disabled in favour of transpile_cps.rs
//! We are just keeping this file in case we don't want CPS anymore in the future.
use crate::{
    typechecked_ast::*,
    unify::{TypecheckedModule, UnifyProgramResult},
};

const KK_MODULE: &str = "KK_MODULE";

pub fn transpile_program(unify_project_result: UnifyProgramResult) -> String {
    // TODO: move this to a file
    let built_in_library = "
        const print_0 = (x) => console.log(x)
        "
    .to_string();
    let imported_modules = unify_project_result
        .imported_modules
        .into_iter()
        .map(|(_, module)| transpile_module(module))
        .collect::<Vec<String>>()
        .join(";");

    let entry_module = transpile_module(unify_project_result.entrypoint);
    format!(
        "{};\nvar KK_MODULE={{}};\n{};\n{}",
        built_in_library, imported_modules, entry_module
    )
}

pub fn transpile_module(module: TypecheckedModule) -> String {
    let module_uid = format!("\"{}\"", module.module.meta.uid.string_value());
    let statements = transpile_statements(module.statements.clone());
    let exported_symbols = module
        .statements
        .iter()
        .flat_map(|statement| match statement {
            TypecheckedStatement::Let { exported, left, .. } if *exported => {
                get_destructure_pattern_bindings(left.clone())
            }
            _ => vec![],
        })
        .map(transpile_identifier)
        .collect::<Vec<String>>();

    format!(
        "{}[{}] = (() => {{{};return {{{}}}}})()",
        KK_MODULE,
        module_uid,
        statements,
        exported_symbols.join(",")
    )
}

pub fn get_destructure_pattern_bindings(
    destructure_pattern: TypecheckedDestructurePattern,
) -> Vec<Identifier> {
    match destructure_pattern {
        TypecheckedDestructurePattern::Infinite { .. }
        | TypecheckedDestructurePattern::Boolean { .. }
        | TypecheckedDestructurePattern::Null(_)
        | TypecheckedDestructurePattern::Underscore(_) => vec![],
        TypecheckedDestructurePattern::Identifier(name) => vec![*name],
        TypecheckedDestructurePattern::EnumConstructor { payload, .. } => match payload {
            None => vec![],
            Some(payload) => get_destructure_pattern_bindings(*payload.pattern),
        },
        TypecheckedDestructurePattern::Record {
            key_pattern_pairs, ..
        } => key_pattern_pairs
            .into_iter()
            .flat_map(|(_, pattern)| get_destructure_pattern_bindings(pattern))
            .collect(),
        TypecheckedDestructurePattern::Array { .. } => {
            panic!("")
        }
        TypecheckedDestructurePattern::Tuple { values } => values
            .into_vector()
            .into_iter()
            .flat_map(get_destructure_pattern_bindings)
            .collect(),
    }
}

pub fn transpile_statements(statements: Vec<TypecheckedStatement>) -> String {
    statements
        .into_iter()
        .map(transpile_statement)
        .collect::<Vec<String>>()
        .join(";")
}

pub fn transpile_statement(statement: TypecheckedStatement) -> String {
    match statement {
        TypecheckedStatement::Expression(expression) => {
            format!("({});", transpile_expression(expression))
        }
        TypecheckedStatement::Let { left, right, .. } => {
            let TranspiledDestructurePattern {
                bindings,
                conditions: _, // Note: conditions can be ignored as we assume that the type checker already checked for case exhaustiveness
            } = transpile_destructure_pattern(left, transpile_expression(right));
            bindings.join(";")
        }
        TypecheckedStatement::ImportStatement(TypecheckedImportStatement {
            module_uid,
            imported_name,
            imported_as,
        }) => {
            format!(
                "let {} = {}[\"{}\"].{}",
                transpile_identifier(imported_as),
                KK_MODULE,
                module_uid.string_value(),
                transpile_identifier(imported_name),
            )
        }
    }
}

fn transpile_identifier(identifier: Identifier) -> String {
    // Note that when we transpile identifier, we omit the module_uid and only use the index
    format!(
        "{}_{}",
        identifier.token.representation, identifier.uid.index
    )
}

pub fn transpile_expression(expression: TypecheckedExpression) -> String {
    match expression {
        TypecheckedExpression::Null => "null".to_string(),
        TypecheckedExpression::Boolean(value) => {
            if value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        TypecheckedExpression::String { representation }
        | TypecheckedExpression::Character { representation }
        | TypecheckedExpression::Float { representation }
        | TypecheckedExpression::Integer { representation } => representation,
        TypecheckedExpression::InterpolatedString { sections } => {
            format!(
                "({})",
                sections
                    .into_iter()
                    .map(|section| {
                        match section {
                            TypecheckedInterpolatedStringSection::String(string) => {
                                format!("\"{}\"", string)
                            }
                            TypecheckedInterpolatedStringSection::Expression(expression) => {
                                format!("({})", transpile_expression(*expression))
                            }
                        }
                    })
                    .collect::<Vec<String>>()
                    .join("+")
            )
        }
        TypecheckedExpression::Variable(variable) => transpile_identifier(variable),
        TypecheckedExpression::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => format!(
            "{{$:'{}',_:{}}}",
            constructor_name,
            match payload {
                Some(payload) => transpile_expression(*payload),
                None => "null".to_string(),
            }
        ),
        TypecheckedExpression::RecordAccess {
            expression,
            property_name,
        } => format!(
            "({}).{}",
            transpile_expression(*expression),
            transpile_property_name(property_name)
        ),
        TypecheckedExpression::Record { key_value_pairs } => format!(
            "{{{}}}",
            key_value_pairs
                .into_iter()
                .map(|(key, value)| {
                    format!(
                        "{}: {}",
                        transpile_property_name(key),
                        transpile_expression(value)
                    )
                })
                .collect::<Vec<String>>()
                .join(",")
        ),
        TypecheckedExpression::BranchedFunction(function) => {
            let TypecheckedBranchedFunction { branches } = *function;
            let number_of_args = branches.first().parameters.len();
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = branches
                .map(transpile_function_branch)
                .into_vector()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
        TypecheckedExpression::FunctionCall(function) => {
            let TypecheckedFunctionCall {
                function,
                first_argument,
                rest_arguments,
            } = *function;
            let function = transpile_expression(*function);
            let arguments = vec![*first_argument]
                .into_iter()
                .chain(rest_arguments.into_iter())
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",");
            format!("({})({})", function, arguments)
        }
        TypecheckedExpression::Array { elements, .. } => format!(
            "[{}]",
            elements
                .into_iter()
                .map(transpile_expression)
                .collect::<Vec<String>>()
                .join(",")
        ),
        TypecheckedExpression::RecordUpdate {
            expression,
            updates,
        } => {
            let updates = updates
                .into_iter()
                .map(|update| match update {
                    TypecheckedRecordUpdate::ValueUpdate {
                        property_name,
                        new_value,
                    } => {
                        format!(
                            "{}: {}",
                            transpile_property_name(property_name),
                            transpile_expression(new_value)
                        )
                    }
                    TypecheckedRecordUpdate::FunctionalUpdate {
                        property_name,
                        function,
                    } => {
                        let property_name = transpile_property_name(property_name);
                        format!(
                            "{}: ({})($.{})",
                            property_name,
                            transpile_expression(function),
                            property_name,
                        )
                    }
                })
                .collect::<Vec<String>>()
                .join(",");
            format!(
                "($=>({{...$,{}}}))({})",
                updates,
                transpile_expression(*expression)
            )
        }
        TypecheckedExpression::Javascript { code } => code,
        TypecheckedExpression::If {
            condition,
            if_true,
            if_false,
        } => {
            format!(
                "(({}) ? ({}) : ({}))",
                transpile_expression(*condition),
                transpile_expression(*if_true),
                transpile_expression(*if_false),
            )
        }
        TypecheckedExpression::Block {
            statements,
            return_value,
        } => {
            format!(
                "(()=>{{{};return {}}})()",
                transpile_statements(statements),
                transpile_expression(*return_value)
            )
        }
    }
}

fn transpile_property_name(property_name: PropertyName) -> String {
    format!("${}", property_name.0.representation)
}

pub fn transpile_function_branch(function_branch: TypecheckedFunctionBranch) -> String {
    let transpiled_destructure_pattern = function_branch
        .parameters
        .into_vector()
        .into_iter()
        .enumerate()
        .map(|(index, pattern)| transpile_destructure_pattern(pattern, format!("_{}", index)))
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

    // let return_now = match *function_branch.body {
    //     TypecheckedExpression::Let { .. } => "",
    //     _ => "return ",
    // };
    let body = transpile_expression(*function_branch.body);
    let conditions = {
        if transpiled_destructure_pattern.conditions.is_empty() {
            "".to_string()
        } else {
            format!(
                "if({})",
                transpiled_destructure_pattern.conditions.join(" && "),
            )
        }
    };
    format!(
        "{}{{{};return {}}}",
        conditions,
        transpiled_destructure_pattern.bindings.join(";"),
        // return_now,
        body
    )
}

#[derive(Debug)]
pub struct TranspiledDestructurePattern {
    conditions: Vec<String>,
    bindings: Vec<String>,
}

pub fn transpile_destructure_pattern(
    destructure_pattern: TypecheckedDestructurePattern,
    from_expression: String,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        TypecheckedDestructurePattern::Infinite { token, .. } => TranspiledDestructurePattern {
            conditions: vec![format!("{} === {}", from_expression, token.representation)],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Boolean { value, .. } => TranspiledDestructurePattern {
            conditions: vec![format!(
                "{} === {}",
                from_expression,
                if value { "true" } else { "false" }
            )],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Null { .. } => TranspiledDestructurePattern {
            conditions: vec![format!("{} === null", from_expression)],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Underscore { .. } => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        TypecheckedDestructurePattern::Array { spread, .. } => match spread {
            None => TranspiledDestructurePattern {
                conditions: vec![format!("{}.length === 0", from_expression)],
                bindings: vec![],
            },
            Some(spread) => join_transpiled_destructure_patterns(vec![
                TranspiledDestructurePattern {
                    conditions: vec![format!("{}.length > 0", from_expression)],
                    bindings: vec![],
                },
                transpile_destructure_pattern(
                    *spread.first_element,
                    format!("{}[0]", from_expression),
                ),
                transpile_destructure_pattern(
                    *spread.rest_elements,
                    format!("{}.slice(1)", from_expression),
                ),
            ]),
        },
        TypecheckedDestructurePattern::Identifier(variable) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                transpile_identifier(*variable),
                from_expression
            )],
        },
        TypecheckedDestructurePattern::EnumConstructor {
            constructor_name,
            payload,
            ..
        } => {
            let first = TranspiledDestructurePattern {
                conditions: vec![format!(
                    "{}.$==='{}'",
                    from_expression, constructor_name.representation
                )],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(payload) => Some(transpile_destructure_pattern(
                    *payload.pattern,
                    format!("{}._", from_expression),
                )),
            };
            match rest {
                None => first,
                Some(rest) => join_transpiled_destructure_pattern(first, rest),
            }
        }
        TypecheckedDestructurePattern::Record {
            key_pattern_pairs, ..
        } => key_pattern_pairs.into_iter().fold(
            TranspiledDestructurePattern {
                bindings: vec![],
                conditions: vec![],
            },
            |result, (key, destructure_pattern)| {
                join_transpiled_destructure_pattern(
                    result,
                    transpile_destructure_pattern(
                        destructure_pattern,
                        format!("{}.{}", from_expression, transpile_property_name(key)),
                    ),
                )
            },
        ),
        TypecheckedDestructurePattern::Tuple { .. } => {
            panic!("Compiler error, should not reach here")
        }
    }
}

fn join_transpiled_destructure_patterns(
    xs: Vec<TranspiledDestructurePattern>,
) -> TranspiledDestructurePattern {
    xs.into_iter().fold(
        TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        join_transpiled_destructure_pattern,
    )
}

fn join_transpiled_destructure_pattern(
    a: TranspiledDestructurePattern,
    b: TranspiledDestructurePattern,
) -> TranspiledDestructurePattern {
    TranspiledDestructurePattern {
        conditions: a
            .conditions
            .into_iter()
            .chain(b.conditions.into_iter())
            .collect(),
        bindings: a
            .bindings
            .into_iter()
            .chain(b.bindings.into_iter())
            .collect(),
    }
}
