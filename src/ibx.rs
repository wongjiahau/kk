use itertools::Itertools;

use crate::raw_ast::Token;

/// Infix prefix expression
#[derive(Debug, Clone)]
pub enum Ibx {
    String(Token),
    Integer(Token),
    Float(Token),
    Identifier(Token),
    Infix {
        left: Box<Ibx>,
        center: Box<Ibx>,
        right: Box<Ibx>,
    },
    Array {
        left_square_bracket: Token,
        right_square_bracket: Token,
        elements: Vec<Ibx>,
    },
}

impl Ibx {
    pub fn to_string(&self) -> String {
        match self {
            Ibx::String(token) => format!("\"{}\"", token.representation),
            Ibx::Float(token) | Ibx::Identifier(token) | Ibx::Integer(token) => {
                format!("{}", token.representation)
            }
            Ibx::Array {
                left_square_bracket,
                elements,
                right_square_bracket,
            } => format!(
                "[ {} ]",
                elements
                    .iter()
                    .map(|element| { element.to_string() })
                    .join(", ")
            ),
            Ibx::Infix {
                left,
                center,
                right,
            } => format!(
                "({} {} {})",
                left.to_string(),
                center.to_string(),
                right.to_string()
            ),
            // Ibx::Parenthesized {
            //     left_parenthesis,
            //     ibx,
            //     right_parenthesis,
            // } => format!("({})", ibx.to_string()),
            // Ibx::Prefix { left, right } => format!("({} {})", left.to_string(), right.to_string()),
        }
    }
}
