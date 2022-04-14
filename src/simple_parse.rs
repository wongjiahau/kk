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
        let result = parser.parse_expression()?;
        // println!("result={:#?}", result);
        Ok(result)
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

    fn next_meaningful_token_is_terminating(&mut self) -> Result<bool, ParseError> {
        match self.peek_next_meaningful_token()? {
            Some(token) if Parser::is_terminating(&token.token_type) => Ok(true),
            None => Ok(true),
            _ => Ok(false),
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

    /// Low precedence expression
    /// (none at the moment)
    fn parse_low_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_mid_precedence_expression()
    }

    /// Mid precedence expression =
    /// * function call
    /// * binary variant construction
    /// * match expression
    /// * lambda
    fn parse_mid_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let left_argument = self.parse_high_precedence_expression()?;
        self.try_parse_function_call(left_argument)
    }

    fn try_parse_function_call(&mut self, left: Expression) -> Result<Expression, ParseError> {
        match self.peek_next_meaningful_token()? {
            None => Ok(left),
            Some(token) if Parser::is_terminating(&token.token_type) => Ok(left),
            Some(_) => {
                let middle = self.parse_high_precedence_expression()?;

                let right = self.parse_high_precedence_expression()?;
                let result = match middle {
                    // Effect perform
                    Expression::Identifier(i) if i.representation == "!" => {
                        Expression::Perform(Box::new(Perform {
                            name: match right {
                                Expression::Identifier(id) => id,
                                _ => panic!("Expecting identifier for perform"),
                            },
                            body: left,
                        }))
                    }

                    // Effect handler
                    Expression::Identifier(i) if i.representation == "handle" => {
                        Expression::EffectHandlerNode(EffectHandlerNode {
                            handler: match right {
                                Expression::Object(object) => {
                                    match object.pairs.split_first()  {
                                        Some((head, [])) => {
                                            match head.pattern.clone() {
                                                Some(Expression::Identifier(effect_name)) => {
                                                    match head.value.clone() {
                                                        Expression::Function(function) => {
                                                            Handler {
                                                                name: effect_name,
                                                                function,
                                                            }
                                                        }
                                                        _ => panic!("Body of effect should be a function")
                                                    }
                                                }
                                                _ => panic!("Name of effect should be identifier")
                                            }
                                        },
                                        _ => panic!("The object of `handle` should have one and only one property which should be an identifier"),
                                    }

                                }
                                _ => panic!("The right side of `handle` should be an object with one property")
                            },
                            handled: Box::new(left),
                        })
                    }

                    // Postfix unary function call (function comes AFTER argument)
                    Expression::Identifier(i) if i.representation == "|" => {
                        Expression::FunctionCall(FunctionCall {
                            function: Box::new(right),
                            argument: Box::new(left),
                        })
                    }

                    // Function
                    Expression::Identifier(i) if i.representation == "<-" => {
                        Expression::Function(Function {
                            body: Box::new(left),
                            parameter: Box::new(right),
                        })
                    }

                    // Assignment
                    Expression::Identifier(i) if i.representation == "as" => {
                        Expression::Assignment(Assignment {
                            value: Box::new(left),
                            pattern: Box::new(right),
                        })
                    }

                    // Match expression
                    Expression::Identifier(i) if i.representation == "match" => {
                        Expression::Match(Match {
                            value: Box::new(left),
                            cases: match right {
                                Expression::Object(object) => Ok(object
                                    .pairs
                                    .into_iter()
                                    .map(|pair| MatchCase {
                                        pattern: pair
                                            .pattern
                                            .expect("match case needs to have left side"),
                                        body: pair.value,
                                    })
                                    .collect()),
                                other => Err(ParseError {
                                    context: todo!(),
                                    kind: todo!(),
                                }),
                            }?,
                        })
                    }
                    // Conditional expression
                    Expression::Identifier(i) if i.representation == "else" => {
                        Expression::Conditional(Conditional {
                            default: Box::new(left),
                            branches: match right {
                                Expression::Array(array) => Ok(array
                                    .values
                                    .into_iter()
                                    .map(|value| match value {
                                        Expression::Branch(branch) => *branch,
                                        _ => panic!("Match should use `if`"),
                                    })
                                    .collect()),
                                other => Err(ParseError {
                                    context: todo!(),
                                    kind: todo!(),
                                }),
                            }?,
                        })
                    }

                    Expression::Identifier(i) if i.representation == "if" => {
                        Expression::Branch(Box::new(Branch {
                            condition: right,
                            body: left,
                        }))
                    }

                    // If branch

                    // Binary variant construction
                    Expression::TagOnlyVariant(tag) => Expression::Variant(Variant {
                        tag,
                        left: Box::new(left),
                        right: Box::new(right),
                    }),

                    // Binary operation
                    _ => {
                        // Note: (x f y) === (x | (y | f))
                        // Or in maths, (x f y) === (f(y))(x)
                        //
                        // Why?
                        // This is so that the syntactical sequence is
                        // the same as when the function is declared, i.e.
                        // f: body given x given y
                        //
                        // Where x is on the left; y is on the right

                        Expression::FunctionCall(FunctionCall {
                            argument: Box::new(left),
                            function: Box::new(Expression::FunctionCall(FunctionCall {
                                function: Box::new(middle),
                                argument: Box::new(right),
                            })),
                        })
                    }
                };
                self.try_parse_function_call(result)
            }
        }
    }

    /// High precedence expression =
    /// * object access (e.g. "a.b.c")
    /// * dot syntax (e.g. "x.(+ | 2).(print)")
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
                self.try_parse_high_precedence_expression(match property {
                    Expression::Parenthesized(parenthesized) => {
                        Expression::FunctionCall(FunctionCall {
                            function: Box::new(Expression::Parenthesized(parenthesized)),
                            argument: Box::new(object),
                        })
                    }
                    Expression::Identifier(token) => Expression::ObjectAccess(ObjectAccess {
                        object: Box::new(object),
                        property: token,
                    }),
                    other => panic!("{:#?}", other),
                })
            }
            _ => Ok(object),
        }
    }

    /// Highest predence expressions =
    /// * string
    /// * number
    /// * array
    /// * tag only variant
    /// * parenthesized expression
    /// * object
    fn parse_highest_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let first = match self.next_meaningful_token()? {
            None => Err(Parser::unexpected_eof(None)),
            Some(token) => Ok(token),
        }?;

        match &first.token_type {
            TokenType::String => Ok(Expression::String(first)),
            TokenType::Integer => Ok(Expression::Number(Number::Int64(
                first.representation.parse().unwrap(),
            ))),
            TokenType::Float => Ok(Expression::Number(Number::Float64(
                first.representation.parse().unwrap(),
            ))),
            TokenType::Identifier => Ok(Expression::Identifier(first)),
            TokenType::LeftCurlyBracket => Ok(Expression::Object(self.parse_object(first)?)),
            TokenType::LeftParenthesis => Ok(self.parse_parenthesized_expression(first)?),

            TokenType::Tag => Ok(Expression::TagOnlyVariant(first)),
            TokenType::LeftSquareBracket => {
                todo!()
            }
            other => {
                panic!("first={:#?} other={:#?}", first, other)
            }
        }
    }

    fn parse_parenthesized_expression(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<Expression, ParseError> {
        Ok(Expression::Parenthesized(Parenthesized {
            left_parenthesis,
            expression: Box::new(self.parse_low_precedence_expression()?),
            right_parenthesis: self.eat_token(TokenType::RightParenthesis, None)?,
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
