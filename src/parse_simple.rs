use crate::parse::{ParseContext, ParseError, ParseErrorKind};
use crate::raw_ast::{Token, TokenType};
use crate::simple_ast::*;
use crate::{non_empty::NonEmpty, tokenize::Tokenizer};

pub struct Parser<'a> {
    temporary_variable_index: usize,
    tokenizer: &'a mut Tokenizer,
}

struct ParseArrayArgument {
    opening_bracket: Token,
    kind: BracketKind,
}

impl<'a> Parser<'a> {
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<TopLevelArray, ParseError> {
        let mut parser = Parser {
            tokenizer,
            temporary_variable_index: 0,
        };
        parser.parse_top_level_array()
    }

    fn parse_top_level_array(&mut self) -> Result<TopLevelArray, ParseError> {
        let head = self.parse_low_precedence_expression()?;
        let mut tail = vec![];
        loop {
            if self.peek_next_meaningful_token()?.is_none() {
                break Ok(TopLevelArray {
                    nodes: NonEmpty { head, tail },
                });
            } else {
                self.eat_token(TokenType::Semicolon, None)?;
                tail.push(self.parse_low_precedence_expression()?)
            }
        }
    }

    fn parse_array(&mut self, argument: ParseArrayArgument) -> Result<Array, ParseError> {
        let mut nodes = vec![];
        if let Some(bracket) = self.try_eat_array_closing_bracket(&argument)? {
            return Ok(Array {
                nodes,
                bracket,
                has_trailing_comma: false,
            });
        }
        let (elements, bracket, has_trailing_comma) = loop {
            nodes.push(self.parse_lowest_precedence_expression()?);
            if self.try_eat_token(TokenType::Comma)?.is_none() {
                let bracket = self.eat_array_closing_bracket(argument)?;
                break (nodes, bracket, false);
            }

            if let Some(bracket) = self.try_eat_array_closing_bracket(&argument)? {
                break (nodes, bracket, true);
            }
        };
        Ok(Array {
            nodes: elements,
            bracket,
            has_trailing_comma,
        })
    }

    fn eat_array_closing_bracket(
        &mut self,
        argument: ParseArrayArgument,
    ) -> Result<Bracket, ParseError> {
        let context = None;
        let closing = match argument.kind {
            BracketKind::Round => self.eat_token(TokenType::RightParenthesis, context),
            BracketKind::Square => self.eat_token(TokenType::RightSquareBracket, context),
            BracketKind::Curly => self.eat_token(TokenType::RightCurlyBracket, context),
        }?;
        Ok(Bracket {
            kind: argument.kind,
            opening: argument.opening_bracket,
            closing,
        })
    }

    fn try_eat_array_closing_bracket(
        &mut self,
        argument: &ParseArrayArgument,
    ) -> Result<Option<Bracket>, ParseError> {
        let closing = match argument.kind {
            BracketKind::Round => self.try_eat_token(TokenType::RightParenthesis),
            BracketKind::Square => self.try_eat_token(TokenType::RightSquareBracket),
            BracketKind::Curly => self.try_eat_token(TokenType::RightCurlyBracket),
        }?;
        match closing {
            None => Ok(None),

            Some(closing) => Ok(Some(Bracket {
                kind: argument.kind.clone(),
                opening: argument.opening_bracket.clone(),
                closing,
            })),
        }
    }

    /// Semicolon array
    fn parse_lowest_precedence_expression(&mut self) -> Result<Node, ParseError> {
        let head = self.parse_low_precedence_expression()?;
        let mut tail = vec![];
        let tail = loop {
            if self.next_token_is_terminating_for_semicolon_array()? {
                break tail;
            } else {
                self.eat_token(TokenType::Semicolon, None)?;
                tail.push(self.parse_low_precedence_expression()?)
            }
        };
        if tail.is_empty() {
            Ok(head)
        } else {
            Ok(Node::SemicolonArray(SemicolonArray {
                nodes: Box::new(NonEmpty { head, tail }),
            }))
        }
    }

    /// Operator call
    fn parse_low_precedence_expression(&mut self) -> Result<Node, ParseError> {
        let expression = self.parse_mid_precedence_expression()?;

        self.try_parse_operator_call(expression)
    }

    fn try_parse_operator_call(&mut self, expression: Node) -> Result<Node, ParseError> {
        if let Some(operator) = self.try_eat_token(TokenType::Operator)? {
            let right = self.parse_low_precedence_expression()?;
            Ok(Node::OperatorCall(OperatorCall {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            }))
        } else {
            Ok(expression)
        }
    }

    /// Infix function call
    fn parse_mid_precedence_expression(&mut self) -> Result<Node, ParseError> {
        let expression = self.parse_high_precedence_expression(false)?;
        self.try_parse_infix_function_call(expression)
    }

    fn parse_prefix_function_call(&mut self) -> Result<Node, ParseError> {
        let previous = self.parse_high_precedence_expression(false)?;
        self.try_parse_prefix_function_call(previous)
    }

    fn try_parse_prefix_function_call(&mut self, previous: Node) -> Result<Node, ParseError> {
        if self.next_token_is_terminating_for_prefix_function_call()? {
            return Ok(previous);
        }
        // This is a prefix function call
        let mut arguments = vec![];
        let arguments = loop {
            if self.next_token_is_terminating_for_prefix_function_call()? {
                break arguments;
            } else {
                arguments.push(self.parse_high_precedence_expression(true)?)
            }
        };
        Ok(Node::PrefixFunctionCall(PrefixFunctionCall {
            function: Box::new(previous),
            arguments,
        }))
    }

    fn try_parse_infix_function_call(&mut self, previous: Node) -> Result<Node, ParseError> {
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
        if function_calls.is_empty() {
            Ok(head)
        } else {
            Ok(Node::InfixFunctionCall(InfixFunctionCall {
                head: Box::new(head),
                tail: function_calls,
            }))
        }
    }

    fn parse_high_precedence_expression(
        &mut self,
        treat_identifier_as_keyword: bool,
    ) -> Result<Node, ParseError> {
        let context = Some(ParseContext::Expression);
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String(string_literal) => {
                    Ok(Node::Literal(Literal::String(string_literal)))
                }
                TokenType::Character => Ok(Node::Literal(Literal::Character(token.clone()))),
                TokenType::HashLeftSquareBracket | TokenType::LeftSquareBracket => {
                    Ok(Node::Array(self.parse_array(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Square,
                    })?))
                }
                TokenType::LeftCurlyBracket => {
                    Ok(Node::Array(self.parse_array(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Curly,
                    })?))
                }
                TokenType::LeftParenthesis => {
                    Ok(Node::Array(self.parse_array(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Round,
                    })?))
                }
                TokenType::Float => Ok(Node::Literal(Literal::Float(token.clone()))),
                TokenType::Integer => Ok(Node::Literal(Literal::Integer(token.clone()))),
                TokenType::Identifier => Ok(Node::Literal(if treat_identifier_as_keyword {
                    Literal::Keyword(token.clone())
                } else {
                    Literal::Identifier(token.clone())
                })),
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

    fn next_token_is_terminating_for_semicolon_array(&mut self) -> Result<bool, ParseError> {
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

    fn next_token_is_terminating_for_operator_call(&mut self) -> Result<bool, ParseError> {
        Ok(self.next_token_is_terminating_for_semicolon_array()?
            || match self.peek_next_meaningful_token()? {
                None => true,
                Some(token) => matches!(
                    token.token_type,
                    TokenType::Comma
                        | TokenType::Semicolon
                        | TokenType::RightParenthesis
                        | TokenType::RightCurlyBracket
                        | TokenType::RightSquareBracket
                ),
            })
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
