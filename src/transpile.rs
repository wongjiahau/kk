use crate::ast::*;

pub fn transpile_statements(statements: Vec<Statement>) -> String {
    statements.into_iter().map(transpile_statement).collect()
}

pub fn transpile_statement(statement: Statement) -> String {
    match statement {
        Statement::Let { left, right, .. } => format!(
            "const {} = {}",
            transpile_destructure_pattern(left),
            transpile_expression(right)
        ),
    }
}

pub fn transpile_destructure_pattern(destructure_pattern: DestructurePattern) -> String {
    match destructure_pattern {
        DestructurePattern::Identifier(i) => i.representation,
        _ => panic!(),
    }
}

fn transpile_tag(tag: String) -> String {
    tag.as_str()[1..].to_string()
}

pub fn transpile_expression(expression: Expression) -> String {
    match expression.value {
        ExpressionValue::String(s) => s.representation,
        ExpressionValue::Variable(v) => v.representation,
        ExpressionValue::Tag(t) => format!("{{$:'{}'}}", transpile_tag(t.representation)),
        ExpressionValue::Function(Function {
            first_branch,
            branches,
        }) => {
            let number_of_args = first_branch.arguments.len();
            let arguments: Vec<String> = (0..number_of_args).map(|x| format!("_{}", x)).collect();
            let branches = vec![first_branch]
                .into_iter()
                .chain(branches.into_iter())
                .into_iter()
                .map(transpile_function_branch)
                .collect::<Vec<String>>()
                .join("\n");
            format!("({})=>{{{}}}", arguments.join(","), branches)
        }
    }
}

pub fn transpile_function_branch(function_branch: FunctionBranch) -> String {
    let transpiled_destructure_pattern = function_branch
        .arguments
        .into_iter()
        .enumerate()
        .map(|(index, argument)| {
            transpile_function_destructure_pattern(
                argument.destructure_pattern,
                format!("_{}", index),
            )
        })
        .fold(
            TranspiledDestructurePattern {
                conditions: vec![],
                bindings: vec![],
            },
            join_transpiled_destructure_pattern,
        );

    let body = transpile_expression(*function_branch.body);
    let conditions = {
        if transpiled_destructure_pattern.conditions.len() > 0 {
            format!(
                "if({})",
                transpiled_destructure_pattern.conditions.join(" && "),
            )
        } else {
            "".to_string()
        }
    };
    format!(
        "{}{{{};return {}}}",
        conditions,
        transpiled_destructure_pattern.bindings.join(";"),
        body
    )
}

#[derive(Debug)]
pub struct TranspiledDestructurePattern {
    conditions: Vec<String>,
    bindings: Vec<String>,
}

pub fn transpile_function_destructure_pattern(
    destructure_pattern: DestructurePattern,
    from_expression: String,
) -> TranspiledDestructurePattern {
    match destructure_pattern {
        DestructurePattern::Underscore(_) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![],
        },
        DestructurePattern::Identifier(identifier) => TranspiledDestructurePattern {
            conditions: vec![],
            bindings: vec![format!(
                "var {} = {}",
                identifier.representation, from_expression
            )],
        },
        DestructurePattern::Tag { token, payload } => {
            let first = TranspiledDestructurePattern {
                conditions: vec![format!(
                    "{}.$==='{}'",
                    from_expression,
                    transpile_tag(token.representation)
                )],
                bindings: vec![],
            };
            let rest = match payload {
                None => None,
                Some(destructure_pattern) => Some(transpile_function_destructure_pattern(
                    *destructure_pattern,
                    format!("{}._", from_expression),
                )),
            };
            match rest {
                None => first,
                Some(rest) => join_transpiled_destructure_pattern(first, rest),
            }
        }
    }
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