use crate::module::Access;
use crate::parse::{ParseContext, ParseError, ParseErrorKind};
use crate::raw_ast::{Position, StringLiteral, Token, TokenType};
use crate::{non_empty::NonEmpty, tokenize::Tokenizer, unify::Positionable};
use crate::{simple_ast::*, tokenize::TokenizeError};

pub struct Parser<'a> {
    temporary_variable_index: usize,
    tokenizer: &'a mut Tokenizer,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: &mut Tokenizer) -> Parser {
        Parser {
            tokenizer,
            temporary_variable_index: 0,
        }
    }
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Array, ParseError> {
        let mut parser = Parser {
            tokenizer,
            temporary_variable_index: 0,
        };
        parser.parse_top_level_array()
    }

    fn parse_top_level_array(&mut self) -> Result<Array, ParseError> {
        self.parse_array(Bracket::None)
    }

    fn parse_array(&mut self, bracket: Bracket) -> Result<Array, ParseError> {
        let mut elements = vec![];
        if self.try_eat_array_closing_bracket(&bracket)?.is_some() {
            return Ok(Array { elements, bracket });
        }
        let elements = loop {
            elements.push(self.parse_low_precedence_expression()?);
            if self.try_eat_token(TokenType::Comma)?.is_none() {
                self.eat_array_closing_bracket(&bracket)?;
                break elements;
            }

            if self.try_eat_array_closing_bracket(&bracket)?.is_some() {
                break elements;
            }
        };
        Ok(Array { elements, bracket })
    }

    fn eat_array_closing_bracket(&mut self, bracket: &Bracket) -> Result<(), ParseError> {
        let context = None;
        match bracket {
            Bracket::Round => {
                self.eat_token(TokenType::RightParenthesis, context)?;
            }
            Bracket::Square => {
                self.eat_token(TokenType::RightSquareBracket, context)?;
            }
            Bracket::Curly => {
                self.eat_token(TokenType::RightCurlyBracket, context)?;
            }
            Bracket::None => {
                // do nothing
            }
        };
        Ok(())
    }

    fn try_eat_array_closing_bracket(
        &mut self,
        bracket: &Bracket,
    ) -> Result<Option<Token>, ParseError> {
        match bracket {
            Bracket::Round => self.try_eat_token(TokenType::RightParenthesis),
            Bracket::Square => self.try_eat_token(TokenType::RightSquareBracket),
            Bracket::Curly => self.try_eat_token(TokenType::RightCurlyBracket),
            Bracket::None => {
                // do nothing
                Ok(None)
            }
        }
    }

    fn parse_low_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let expression = self.parse_mid_precedence_expression()?;

        self.try_parse_operator_call(expression)
    }

    fn try_parse_operator_call(
        &mut self,
        expression: Expression,
    ) -> Result<Expression, ParseError> {
        if let Some(operator) = self.try_eat_token(TokenType::Operator)? {
            let right = self.parse_low_precedence_expression()?;
            Ok(Expression::OperatorCall(OperatorCall {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            }))
        } else {
            Ok(expression)
        }
    }

    fn parse_mid_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let expression = self.parse_high_precedence_expression()?;
        self.try_parse_infix_function_call(expression)
    }

    fn parse_prefix_function_call(&mut self) -> Result<Expression, ParseError> {
        let previous = self.parse_high_precedence_expression()?;
        self.try_parse_prefix_function_call(previous)
    }

    fn try_parse_prefix_function_call(
        &mut self,
        previous: Expression,
    ) -> Result<Expression, ParseError> {
        if self.next_token_is_terminating_for_prefix_function_call()? {
            return Ok(previous);
        }
        // This is a prefix function call
        let mut arguments = vec![];
        let arguments = loop {
            if self.next_token_is_terminating_for_prefix_function_call()? {
                break arguments;
            } else {
                arguments.push(self.parse_high_precedence_expression()?)
            }
        };
        Ok(Expression::PrefixFunctionCall(PrefixFunctionCall {
            function: Box::new(previous),
            arguments,
        }))
    }

    fn try_parse_infix_function_call(
        &mut self,
        previous: Expression,
    ) -> Result<Expression, ParseError> {
        if self.next_token_is_terminating_for_infix_function_call()? {
            return Ok(previous);
        }

        let head = self.try_parse_prefix_function_call(previous)?;

        let mut function_calls = vec![];
        let function_calls = loop {
            if self.try_eat_token(TokenType::Period)?.is_some() {
                function_calls.push(self.parse_prefix_function_call()?)
            } else {
                break function_calls;
            }
        };
        Ok(Expression::InfixFunctionCall(InfixFunctionCall {
            head: Box::new(head),
            tail: function_calls,
        }))
    }

    fn parse_high_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Expression);
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String(string_literal) => Ok(Expression::String(string_literal)),
                TokenType::Character => Ok(Expression::Character(token.clone())),
                TokenType::HashLeftSquareBracket => {
                    Ok(Expression::Array(self.parse_array(Bracket::Square)?))
                }
                TokenType::LeftSquareBracket => {
                    Ok(Expression::Array(self.parse_array(Bracket::Square)?))
                }
                TokenType::LeftCurlyBracket => {
                    Ok(Expression::Array(self.parse_array(Bracket::Curly)?))
                }
                TokenType::LeftParenthesis => {
                    Ok(Expression::Array(self.parse_array(Bracket::Round)?))
                }
                TokenType::Float => Ok(Expression::Float(token.clone())),
                TokenType::Integer => Ok(Expression::Integer(token.clone())),
                TokenType::Identifier => Ok(Expression::Identifier(token.clone())),
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
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

    pub fn eat_token(
        &mut self,
        token_type: TokenType,
        context: Option<ParseContext>,
    ) -> Result<Token, ParseError> {
        let token = self.next_meaningful_token()?;
        Parser::validate_token(token_type, token, context)
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

    fn next_token_is_terminating_for_operator_call(&mut self) -> Result<bool, ParseError> {
        match self.peek_next_meaningful_token()? {
            None => Ok(true),
            Some(token) => Ok(matches!(
                token.token_type,
                TokenType::Comma
                    | TokenType::RightParenthesis
                    | TokenType::RightCurlyBracket
                    | TokenType::RightSquareBracket
            )),
        }
    }

    fn next_token_is_terminating_for_infix_function_call(&mut self) -> Result<bool, ParseError> {
        Ok(self.next_token_is_terminating_for_operator_call()?
            || match self.peek_next_meaningful_token()? {
                None => true,
                Some(token) => matches!(token.token_type, TokenType::Operator),
            })
    }

    fn next_token_is_terminating_for_prefix_function_call(&mut self) -> Result<bool, ParseError> {
        Ok(self.next_token_is_terminating_for_infix_function_call()?
            || match self.peek_next_meaningful_token()? {
                None => true,
                Some(token) => matches!(token.token_type, TokenType::Period),
            })
    }

    fn try_eat_string_literal(&mut self) -> Result<Option<StringLiteral>, ParseError> {
        Ok(match self.peek_next_meaningful_token()? {
            None => None,
            Some(token) => match token.token_type {
                TokenType::String(string_literal) => {
                    self.next_meaningful_token()?;
                    Some(string_literal)
                }
                _ => None,
            },
        })
    }

    fn eat_string_literal(&mut self) -> Result<StringLiteral, ParseError> {
        match self.next_meaningful_token()? {
            None => Err(Parser::unexpected_eof(None)),
            Some(token) => match token.token_type {
                TokenType::String(string_literal) => Ok(string_literal),
                _ => Err(Parser::invalid_token(token, None)),
            },
        }
    }

    fn get_next_temporary_variable_index(&mut self) -> usize {
        let result = self.temporary_variable_index;
        self.temporary_variable_index += 1;
        result
    }
}

fn is_token_meaningless(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Whitespace
            | TokenType::Newline
            | TokenType::Comment
            | TokenType::MultilineComment
    )
}

/// Compare the variants of two enum, while ignoring the payload
/// Refer https://stackoverflow.com/a/32554326/6587634
fn variant_eq(a: &TokenType, b: &TokenType) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
