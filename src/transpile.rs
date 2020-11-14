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
    }
}

pub fn transpile_expression(expression: Expression) -> String {
    match expression.value {
        ExpressionValue::String(s) => s.representation,
        ExpressionValue::Variable(v) => v.representation,
        other => panic!("{:#?}", other), // ExpressionValue::Function { branches } => {
                                          //     if branches.len() == 1 {
                                          //         let first_branch = branches[0];
                                          //     }
                                          // }
    }
}
