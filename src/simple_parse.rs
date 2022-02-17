use crate::parse::{ParseContext, ParseError, ParseErrorKind};
use crate::raw_ast::{Token, TokenType};
use crate::simple_ast::*;
use crate::tokenize::Tokenizer;

pub struct Parser<'a> {
    tokenizer: &'a mut Tokenizer,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: &mut Tokenizer) -> Parser {
        Parser { tokenizer }
    }
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Expression, ParseError> {
        let mut parser = Parser { tokenizer };
        parser.parse_expression()
    }

    /// Return the next meaningful token.  
    /// In other words, meaningless comments such as Whitespace and Comments will be skipped.
    fn next_meaningful_token(&mut self) -> Result<Option<Token>, ParseError> {
        loop {
            match self.tokenizer.next()? {
                None => return Ok(None),
                Some(token) => {
                    if is_token_meaningless(&token) {
                        continue;
                    } else {
                        return Ok(Some(token));
                    }
                }
            }
        }
    }

    /// Peek the next meaningful token without advancing the iterator.  
    /// In other words, meaningless comments such as Whitespace, Comments and Documentation will be skipped.
    fn peek_next_meaningful_token(&mut self) -> Result<Option<Token>, ParseError> {
        loop {
            match self.tokenizer.peek()? {
                None => return Ok(None),
                Some(token) => {
                    if is_token_meaningless(&token) {
                        let _ = self.tokenizer.next();
                        continue;
                    } else {
                        return Ok(Some(token));
                    }
                }
            }
        }
    }

    fn invalid_token(actual_token: Token, context: Option<ParseContext>) -> ParseError {
        ParseError {
            context,
            kind: ParseErrorKind::InvalidToken {
                actual_token,
                expected_token_type: None,
            },
        }
    }

    fn unexpected_eof(context: Option<ParseContext>) -> ParseError {
        ParseError {
            context,
            kind: ParseErrorKind::UnexpectedEof {
                expected_token_type: None,
            },
        }
    }

    pub fn eat_token(
        &mut self,
        token_type: TokenType,
        context: Option<ParseContext>,
    ) -> Result<Token, ParseError> {
        let token = self.next_meaningful_token()?;
        Parser::validate_token(token_type, token, context)
    }

    fn validate_token(
        expected_token_type: TokenType,
        actual_token: Option<Token>,
        context: Option<ParseContext>,
    ) -> Result<Token, ParseError> {
        if let Some(token) = actual_token {
            if variant_eq(&token.token_type, &expected_token_type) {
                Ok(token)
            } else {
                Err(ParseError {
                    context,
                    kind: ParseErrorKind::InvalidToken {
                        actual_token: token,
                        expected_token_type: Some(expected_token_type),
                    },
                })
            }
        } else {
            Err(ParseError {
                context,
                kind: ParseErrorKind::UnexpectedEof {
                    expected_token_type: Some(expected_token_type),
                },
            })
        }
    }

    fn try_eat_token(&mut self, token_type: TokenType) -> Result<Option<Token>, ParseError> {
        if let Some(token) = self.peek_next_meaningful_token()? {
            Ok(if variant_eq(&token.token_type, &token_type) {
                match self.next_meaningful_token()? {
                    Some(token) => Some(token),
                    None => None,
                }
            } else {
                None
            })
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_low_precedence_expression()
    }

    fn is_terminating(token_type: &TokenType) -> bool {
        match token_type {
            TokenType::RightCurlyBracket
            | TokenType::RightParenthesis
            | TokenType::RightSquareBracket
            | TokenType::Comma
            | TokenType::Colon => true,
            _ => false,
        }
    }

    /// High precedence expression
    /// * function (right associative colon)
    fn parse_low_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let parameter = self.parse_mid_precedence_expression()?;
        self.try_parse_function(parameter)
    }

    fn try_parse_function(&mut self, parameter: Expression) -> Result<Expression, ParseError> {
        match self.try_eat_token(TokenType::Colon)? {
            Some(_) => {
                let body = self.parse_low_precedence_expression()?;
                Ok(Expression::Function(Function {
                    parameter: Box::new(parameter),
                    body: Box::new(body),
                }))
            }
            _ => Ok(parameter),
        }
    }

    /// Mid precedence expression =
    /// * function call
    /// * binary variant construction
    /// * match expression
    fn parse_mid_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let left_argument = self.parse_high_precedence_expression()?;
        self.try_parse_function_call(left_argument)
    }

    fn try_parse_function_call(
        &mut self,
        left_argument: Expression,
    ) -> Result<Expression, ParseError> {
        match self.peek_next_meaningful_token()? {
            None => Ok(left_argument),
            Some(token) if Parser::is_terminating(&token.token_type) => Ok(left_argument),
            Some(_) => {
                let function = self.parse_high_precedence_expression()?;
                let right_argument = self.parse_high_precedence_expression()?;
                let left_argument = match function {
                    // Unary function call
                    Expression::Identifier(i) if i == "|" => {
                        Expression::FunctionCall(FunctionCall {
                            argument: Box::new(left_argument),
                            function: Box::new(right_argument),
                        })
                    }

                    // Match expression
                    Expression::Identifier(i) if i == "match" => Expression::Match(Match {
                        value: Box::new(left_argument),
                        cases: match right_argument {
                            Expression::Object(object) => Ok(object
                                .pairs
                                .into_iter()
                                .map(|pair| match pair.pattern {
                                    None => panic!(),
                                    Some(pattern) => MatchCase {
                                        pattern,
                                        body: pair.value,
                                    },
                                })
                                .collect()),
                            other => Err(ParseError {
                                context: todo!(),
                                kind: todo!(),
                            }),
                        }?,
                    }),

                    // Conditional expression
                    Expression::Identifier(i) if i == "else" => {
                        Expression::Conditional(Conditional {
                            default: Box::new(right_argument),
                            branches: match left_argument {
                                Expression::Object(object) => Ok(object
                                    .pairs
                                    .into_iter()
                                    .map(|pair| match pair.pattern {
                                        None => panic!(),
                                        Some(condition) => Branch {
                                            condition,
                                            body: pair.value,
                                        },
                                    })
                                    .collect()),
                                other => Err(ParseError {
                                    context: todo!(),
                                    kind: todo!(),
                                }),
                            }?,
                        })
                    }

                    // Binary variant construction
                    Expression::TagOnlyVariant(tag) => Expression::Variant(Variant {
                        tag,
                        left: Box::new(left_argument),
                        right: Box::new(right_argument),
                    }),

                    // Binary function call
                    _ => {
                        // Note: (x f y) === (y | (x | f))
                        // Or in maths, (x f y) === (f(x))(y)
                        // Expression::FunctionCall()
                        //
                        // Why?
                        // This is so that the syntactical sequence is
                        // the same as when the function is declared, i.e.
                        // f: (x: (y: body))
                        //
                        // Where x is on the left; y is on the right

                        Expression::FunctionCall(FunctionCall {
                            argument: Box::new(right_argument),
                            function: Box::new(Expression::FunctionCall(FunctionCall {
                                function: Box::new(function),
                                argument: Box::new(left_argument),
                            })),
                        })
                    }
                };
                self.try_parse_function_call(left_argument)
            }
        }
    }

    /// High precedenc expression =
    /// * object access (e.g. "a.b.c")
    fn parse_high_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let object = self.parse_highest_precedence_expression()?;
        self.try_parse_high_precedence_expression(object)
    }

    fn try_parse_high_precedence_expression(
        &mut self,
        object: Expression,
    ) -> Result<Expression, ParseError> {
        match self.peek_next_meaningful_token()? {
            Some(token) if Parser::is_terminating(&token.token_type) => Ok(object),
            Some(Token {
                token_type: TokenType::Period,
                ..
            }) => {
                self.eat_token(TokenType::Period, None)?;
                let property = self.parse_highest_precedence_expression()?;
                self.try_parse_high_precedence_expression(Expression::ObjectAccess(ObjectAccess {
                    object: Box::new(object),
                    property: Box::new(property),
                }))
            }
            _ => Ok(object),
        }
    }

    /// Highest predence expressions =
    /// * string
    /// * number
    /// * array
    /// * tuple/unit
    /// * parenthesized expression
    /// * object
    fn parse_highest_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let first = match self.next_meaningful_token()? {
            None => Err(Parser::unexpected_eof(None)),
            Some(token) => Ok(token),
        }?;

        match first.token_type {
            TokenType::String => Ok(Expression::String(first.representation)),
            TokenType::Integer => Ok(Expression::Number(Number::Int64(
                first.representation.parse().unwrap(),
            ))),
            TokenType::Float => Ok(Expression::Number(Number::Float64(
                first.representation.parse().unwrap(),
            ))),
            TokenType::Identifier => {
                if first.representation.starts_with("#") {
                    Ok(Expression::TagOnlyVariant(first.representation))
                } else {
                    Ok(Expression::Identifier(first.representation))
                }
            }
            TokenType::LeftCurlyBracket => Ok(Expression::Object(self.parse_object(first)?)),
            TokenType::LeftParenthesis => Ok(self.parse_tuple_or_parenthesized_expression(first)?),

            other => {
                panic!("{:#?}", other)
            }
        }
    }

    fn parse_tuple_or_parenthesized_expression(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<Expression, ParseError> {
        let mut values = vec![];
        let right_parenthesis = loop {
            if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                break right_parenthesis;
            }
            values.push(self.parse_low_precedence_expression()?);
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                continue;
            }
            let right_parenthesis = self.eat_token(TokenType::RightParenthesis, None)?;
            match values.split_first() {
                Some((head, [])) => return Ok(head.clone()),
                _ => break right_parenthesis,
            }
        };
        Ok(Expression::Tuple(Tuple {
            left_parenthesis,
            values,
            right_parenthesis,
        }))
    }

    fn parse_object(&mut self, left_curly_bracket: Token) -> Result<Object, ParseError> {
        let mut pairs = vec![];
        let right_curly_bracket = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break right_curly_bracket;
            }
            let pattern = self.parse_mid_precedence_expression()?;
            if self.try_eat_token(TokenType::Colon)?.is_some() {
                let value = self.parse_low_precedence_expression()?;
                pairs.push(ObjectPair {
                    pattern: Some(pattern),
                    value,
                });
            } else {
                match &pattern {
                    Expression::Identifier(i) => {
                        // object punning / shorthand
                        pairs.push(ObjectPair {
                            pattern: Some(Pattern::Identifier(i.clone())),
                            value: pattern,
                        });
                    }
                    pattern => {
                        pairs.push(ObjectPair {
                            pattern: None,
                            value: pattern.clone(),
                        });
                    }
                };
            }
            self.eat_token(TokenType::Comma, None)?;
        };
        Ok(Object {
            left_curly_bracket,
            pairs,
            right_curly_bracket,
        })
    }
}

fn is_token_meaningless(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Whitespace | TokenType::Newline | TokenType::Comment
    )
}

/// Compare the variants of two enum, while ignoring the payload
/// Refer https://stackoverflow.com/a/32554326/6587634
fn variant_eq(a: &TokenType, b: &TokenType) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
