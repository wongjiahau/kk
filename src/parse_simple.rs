use crate::parse::{ParseContext, ParseError, ParseErrorKind};
use crate::simple_ast::*;
use crate::tokenize::{Token, TokenType};
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
    pub fn new(tokenizer: &mut Tokenizer) -> Parser {
        Parser {
            tokenizer,
            temporary_variable_index: 0,
        }
    }
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<TopLevelArray, ParseError> {
        let mut parser = Parser {
            tokenizer,
            temporary_variable_index: 0,
        };
        parser.parse_top_level_array()
    }

    fn parse_top_level_array(&mut self) -> Result<TopLevelArray, ParseError> {
        let head = self.parse_high_precedence_node()?;
        let mut tail = vec![];
        loop {
            if self.peek_next_meaningful_token()?.is_none() {
                break Ok(TopLevelArray {
                    nodes: NonEmpty { head, tail },
                });
            } else {
                tail.push(self.parse_high_precedence_node()?)
            }
        }
    }

    fn parse_list(&mut self, argument: ParseArrayArgument) -> Result<List, ParseError> {
        let mut nodes = vec![];
        if let Some(bracket) = self.try_eat_array_closing_bracket(&argument)? {
            return Ok(List { bracket, nodes });
        }
        let (nodes, bracket) = loop {
            nodes.push(self.parse_high_precedence_node()?);

            if let Some(bracket) = self.try_eat_array_closing_bracket(&argument)? {
                break (nodes, bracket);
            }
        };
        Ok(List { bracket, nodes })
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

    pub fn parse_high_precedence_node(&mut self) -> Result<Node, ParseError> {
        let context = Some(ParseContext::Expression);
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String(string_literal) => {
                    Ok(Node::Literal(Literal::String(string_literal)))
                }
                TokenType::Character => Ok(Node::Literal(Literal::Character(token.clone()))),
                TokenType::LeftSquareBracket => {
                    Ok(Node::List(self.parse_list(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Square,
                    })?))
                }
                TokenType::LeftCurlyBracket => {
                    Ok(Node::List(self.parse_list(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Curly,
                    })?))
                }
                TokenType::LeftParenthesis => {
                    Ok(Node::List(self.parse_list(ParseArrayArgument {
                        opening_bracket: token,
                        kind: BracketKind::Round,
                    })?))
                }
                TokenType::Float => Ok(Node::Literal(Literal::Float(token.clone()))),
                TokenType::Integer => Ok(Node::Literal(Literal::Integer(token.clone()))),
                TokenType::Identifier => {
                    Ok(Node::Literal(if token.representation.starts_with(":") {
                        Literal::Keyword(token.clone())
                    } else {
                        Literal::Identifier(token.clone())
                    }))
                }
                TokenType::InterpolatedString(interpolated_string) => Ok(Node::Literal(
                    Literal::InterpolatedString(interpolated_string),
                )),
                TokenType::MultilineComment => Ok(Node::Comment(token)),
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
    matches!(token.token_type, TokenType::Whitespace | TokenType::Newline)
}

/// Compare the variants of two enum, while ignoring the payload
/// Refer https://stackoverflow.com/a/32554326/6587634
fn variant_eq(a: &TokenType, b: &TokenType) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
