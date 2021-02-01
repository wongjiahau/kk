use crate::ast::*;
use crate::non_empty::NonEmpty;
use core::slice::Iter;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub context: ParseContext,
    pub kind: ParseErrorKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
    InvalidToken {
        actual_token: Token,
        expected_token_type: Option<TokenType>,
    },
    UnexpectedEOF {
        expected_token_type: Option<TokenType>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseContext {
    // Expression
    Expression,
    ExpressionLet,
    ExpressionFunction,
    ExpressionFunctionCallOrPropertyAccess,
    ExpressionFunctionCall,
    ExpressionRecord,
    ExpressionArray,
    ExpressionEnumConstructor,

    // Statement
    Statement,
    StatementLet,
    StatementType,
    StatementEnum,

    // Type annotation
    TypeAnnotationRecord,
    TypeAnnotationArray,
    TypeAnnotationFunction,

    // Destructure pattern
    Pattern,
    PatternArray,
    PatternEnum,
    PatternRecord,

    // Others
    TypeArguments,
    EnumConstructorDefinition,
    TypeVariablesDeclaration,
    FunctionArguments,
    ScopeResolution,
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
        let tokens = tokens
            .into_iter()
            .filter(|token| {
                !matches!(
                    token.token_type,
                    TokenType::Whitespace
                        | TokenType::Newline
                        | TokenType::Comment
                        | TokenType::Documentation
                )
            })
            .collect::<Vec<Token>>();
        let mut parser = Parser {
            tokens: tokens.iter().peekable(),
        };
        parser.parse_statements()
    }

    fn invalid_token(actual_token: Token, context: ParseContext) -> ParseError {
        ParseError {
            context,
            kind: ParseErrorKind::InvalidToken {
                actual_token,
                expected_token_type: None,
            },
        }
    }

    fn unexpected_eof(context: ParseContext) -> ParseError {
        ParseError {
            context,
            kind: ParseErrorKind::UnexpectedEOF {
                expected_token_type: None,
            },
        }
    }

    fn parse_statements(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::<Statement>::new();
        while self.tokens.peek().is_some() {
            statements.push(self.parse_statement()?)
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let context = ParseContext::Statement;
        match self.tokens.next() {
            Some(token) => match token.token_type {
                TokenType::KeywordLet => self.parse_let_statement(token.clone()),
                TokenType::KeywordType => self.parse_type_statement(token.clone()),
                TokenType::KeywordEnum => self.parse_enum_statement(token.clone()),
                TokenType::KeywordDo => self.parse_do_statement(token.clone()),
                _ => Err(Parser::invalid_token(token.clone(), context)),
            },
            None => Err(Parser::unexpected_eof(context)),
        }
    }

    fn parse_let_statement(&mut self, keyword_let: Token) -> Result<Statement, ParseError> {
        let context = ParseContext::StatementLet;
        let left = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        let type_annotation = self.try_parse_colon_type_annotation(context)?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_expression()?;
        Ok(Statement::Let {
            keyword_let,
            left,
            right,
            type_variables,
            type_annotation,
        })
    }

    fn parse_type_statement(&mut self, keyword_type: Token) -> Result<Statement, ParseError> {
        let context = ParseContext::StatementType;
        let left = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_type_annotation(context)?;
        Ok(Statement::Type {
            keyword_type,
            left,
            type_variables,
            right,
        })
    }

    fn parse_enum_statement(&mut self, keyword_enum: Token) -> Result<Statement, ParseError> {
        let context = ParseContext::StatementEnum;
        let name = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;

        let mut constructors = vec![self.parse_enum_constructor_definition()?];

        while let Some(Token {
            token_type: TokenType::Identifier,
            ..
        }) = self.tokens.peek()
        {
            constructors.push(self.parse_enum_constructor_definition()?)
        }
        Ok(Statement::Enum {
            keyword_enum,
            name,
            type_variables,
            constructors,
        })
    }

    fn parse_do_statement(&mut self, keyword_do: Token) -> Result<Statement, ParseError> {
        let expression = self.parse_expression()?;
        Ok(Statement::Do {
            keyword_do,
            expression,
        })
    }

    fn eat_token(
        &mut self,
        token_type: TokenType,
        context: ParseContext,
    ) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.token_type == token_type {
                Ok(token.clone())
            } else {
                Err(ParseError {
                    context,
                    kind: ParseErrorKind::InvalidToken {
                        actual_token: token.clone(),
                        expected_token_type: Some(token_type),
                    },
                })
            }
        } else {
            Err(ParseError {
                context,
                kind: ParseErrorKind::UnexpectedEOF {
                    expected_token_type: Some(token_type),
                },
            })
        }
    }

    fn try_eat_token(&mut self, token_type: TokenType) -> Option<Token> {
        if let Some(token) = self.tokens.peek() {
            if token.token_type == token_type {
                match self.tokens.next() {
                    Some(token) => Some(token.clone()),
                    None => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn try_parse_type_arguments(&mut self) -> Result<Option<TypeArguments>, ParseError> {
        let context = ParseContext::TypeArguments;
        if let Some(left_angular_bracket) = self.try_eat_token(TokenType::LessThan) {
            let mut substitutions = Vec::new();
            loop {
                let parameter_name = self.eat_token(TokenType::Identifier, context)?;
                self.eat_token(TokenType::Equals, context)?;
                let type_annotation = self.parse_type_annotation(context)?;
                substitutions.push((parameter_name, type_annotation));
                if self.try_eat_token(TokenType::Comma).is_none() {
                    let right_angular_bracket = self.eat_token(TokenType::MoreThan, context)?;
                    return Ok(Some(TypeArguments {
                        left_angular_bracket,
                        substitutions,
                        right_angular_bracket,
                    }));
                }
            }
        } else {
            Ok(None)
        }
    }

    fn parse_enum_constructor_definition(&mut self) -> Result<EnumConstructor, ParseError> {
        let context = ParseContext::EnumConstructorDefinition;
        let name = self.eat_token(TokenType::Identifier, context)?;
        let left_parenthesis = self.eat_token(TokenType::LeftParenthesis, context)?;
        if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis) {
            Ok(EnumConstructor {
                name,
                left_parenthesis,
                payload: None,
                right_parenthesis,
            })
        } else {
            let type_annotation = self.parse_type_annotation(context)?;
            let right_parenthesis = self.eat_token(TokenType::RightParenthesis, context)?;
            Ok(EnumConstructor {
                name,
                left_parenthesis,
                right_parenthesis,
                payload: Some(Box::new(type_annotation)),
            })
        }
    }

    fn try_parse_type_variables_declaration(&mut self) -> Result<Vec<Token>, ParseError> {
        let context = ParseContext::TypeVariablesDeclaration;
        if self.try_eat_token(TokenType::LessThan).is_some() {
            let mut type_variables = vec![];
            let type_variables = loop {
                type_variables.push(self.eat_token(TokenType::Identifier, context)?);
                if self.try_eat_token(TokenType::Comma).is_none() {
                    break type_variables;
                }
            };
            self.eat_token(TokenType::MoreThan, context)?;
            Ok(type_variables)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_function(&mut self, backslash_token: Token) -> Result<Function, ParseError> {
        let mut branches = Vec::<FunctionBranch>::new();
        let first_branch = self.parse_function_branch(backslash_token)?;
        while let Some(Token {
            token_type: TokenType::Backslash,
            ..
        }) = self.tokens.peek()
        {
            let backslash_token = self.tokens.next().unwrap().clone();
            let branch = self.parse_function_branch(backslash_token)?;
            branches.push(branch);
        }
        Ok(Function {
            branches: NonEmpty {
                head: first_branch,
                tail: branches,
            },
        })
    }

    fn parse_function_branch(
        &mut self,
        backslash_token: Token,
    ) -> Result<FunctionBranch, ParseError> {
        let context = ParseContext::ExpressionFunction;
        let parameters = self.parse_function_parameters()?;
        let return_type_annotation = self.try_parse_function_return_type_annotation(context)?;
        self.eat_token(TokenType::FatArrowRight, context)?;
        let body = self.parse_expression()?;
        Ok(FunctionBranch {
            start_token: backslash_token,
            parameters,
            body: Box::new(body),
            return_type_annotation,
        })
    }
    fn parse_function_parameters(&mut self) -> Result<FunctionParameters, ParseError> {
        let context = ParseContext::FunctionArguments;
        if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis) {
            let first_parameter = self.parse_function_parameter(context)?;
            let mut rest_parameters = Vec::<FunctionParameter>::new();
            let right_parenthesis = loop {
                if self.try_eat_token(TokenType::Comma).is_none() {
                    break self.eat_token(TokenType::RightParenthesis, context)?;
                }
                let argument = self.parse_function_parameter(context)?;
                rest_parameters.push(argument);
                if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis) {
                    break right_parenthesis;
                } else {
                    self.eat_token(TokenType::Comma, context)?;
                    if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)
                    {
                        break right_parenthesis;
                    }
                }
            };
            Ok(FunctionParameters::WithParenthesis {
                left_parenthesis,
                right_parenthesis,
                parameters: NonEmpty {
                    head: first_parameter,
                    tail: rest_parameters,
                },
            })
        } else {
            Ok(FunctionParameters::NoParenthesis {
                parameter: self.parse_function_parameter(context)?,
            })
        }
    }
    fn parse_function_parameter(
        &mut self,
        context: ParseContext,
    ) -> Result<FunctionParameter, ParseError> {
        let destructure_pattern = self.parse_destructure_pattern()?;
        let type_annotation = self.try_parse_colon_type_annotation(context)?;
        Ok(FunctionParameter {
            destructure_pattern,
            type_annotation,
        })
    }

    fn try_parse_function_return_type_annotation(
        &mut self,
        context: ParseContext,
    ) -> Result<Option<TypeAnnotation>, ParseError> {
        if self.try_eat_token(TokenType::ThinArrowRight).is_some() {
            match self.parse_type_annotation(context) {
                Ok(type_annotation) => Ok(Some(type_annotation)),
                Err(error) => Err(error),
            }
        } else {
            Ok(None)
        }
    }

    fn try_parse_colon_type_annotation(
        &mut self,
        context: ParseContext,
    ) -> Result<Option<TypeAnnotation>, ParseError> {
        if self.try_eat_token(TokenType::Colon).is_some() {
            match self.parse_type_annotation(context) {
                Ok(type_annotation) => Ok(Some(type_annotation)),
                Err(error) => Err(error),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_type_annotation(
        &mut self,
        context: ParseContext,
    ) -> Result<TypeAnnotation, ParseError> {
        if let Some(token) = self.tokens.next() {
            match token.token_type.clone() {
                TokenType::Identifier | TokenType::KeywordNull => {
                    let name = token.clone();
                    Ok(TypeAnnotation::Named {
                        name,
                        type_arguments: self.try_parse_type_arguments()?,
                    })
                }
                TokenType::LeftSquareBracket => {
                    let context = ParseContext::TypeAnnotationArray;
                    let left_square_bracket = token.clone();
                    let element_type = self.parse_type_annotation(context)?;
                    let right_square_bracket =
                        self.eat_token(TokenType::RightSquareBracket, context)?;
                    Ok(TypeAnnotation::Array {
                        left_square_bracket,
                        element_type: Box::new(element_type),
                        right_square_bracket,
                    })
                }
                TokenType::LeftCurlyBracket => {
                    let left_curly_bracket = token.clone();
                    let context = ParseContext::TypeAnnotationRecord;
                    let mut key_type_annotation_pairs: Vec<(Token, TypeAnnotation)> = Vec::new();
                    loop {
                        let key = self.eat_token(TokenType::Identifier, context)?;
                        self.eat_token(TokenType::Colon, context)?;
                        let type_annotation = self.parse_type_annotation(context)?;
                        key_type_annotation_pairs.push((key, type_annotation));
                        if self.try_eat_token(TokenType::Comma).is_none() {
                            break;
                        }
                    }
                    let right_curly_bracket =
                        self.eat_token(TokenType::RightCurlyBracket, context)?;
                    Ok(TypeAnnotation::Record {
                        left_curly_bracket,
                        key_type_annotation_pairs,
                        right_curly_bracket,
                    })
                }
                TokenType::Backslash => {
                    let start_token = token.clone();
                    let context = ParseContext::TypeAnnotationFunction;
                    let (first_argument_type, rest_arguments_types) =
                        if self.try_eat_token(TokenType::LeftParenthesis).is_some() {
                            let first_argument = self.parse_type_annotation(context)?;
                            let mut arguments_types = vec![];
                            if self.try_eat_token(TokenType::Comma).is_some() {
                                arguments_types.push(self.parse_type_annotation(context)?)
                            }
                            self.eat_token(TokenType::RightParenthesis, context)?;
                            (first_argument, arguments_types)
                        } else {
                            (self.parse_type_annotation(context)?, vec![])
                        };

                    self.eat_token(TokenType::ThinArrowRight, context)?;
                    let return_type = self.parse_type_annotation(context)?;
                    Ok(TypeAnnotation::Function {
                        start_token,
                        parameters_types: Box::new(NonEmpty {
                            head: first_argument_type,
                            tail: rest_arguments_types,
                        }),
                        return_type: Box::new(return_type),
                    })
                }
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn parse_function_call_or_property_access(
        &mut self,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        if self.try_eat_token(TokenType::Period).is_none() {
            return Ok(first_argument);
        }
        let context = ParseContext::ExpressionFunctionCallOrPropertyAccess;
        let first_argument = {
            let name = self.eat_token(TokenType::Identifier, context)?;
            match self.tokens.peek() {
                Some(Token {
                    token_type: TokenType::LeftParenthesis,
                    ..
                })
                | Some(Token {
                    token_type: TokenType::LessThan,
                    ..
                }) => {
                    // Then this is a function call
                    let type_arguments = self.try_parse_type_arguments()?;
                    let function_call_arguments = self.parse_function_call_rest_arguments()?;
                    Expression::FunctionCall(Box::new(FunctionCall {
                        first_argument: Box::new(first_argument),
                        rest_arguments: function_call_arguments,
                        type_arguments,
                        function_name: name,
                    }))
                }
                _ => {
                    // Then this is a property access
                    Expression::RecordAccess {
                        expression: Box::new(first_argument),
                        property_name: name,
                    }
                }
            }
        };
        self.parse_function_call_or_property_access(first_argument)
    }

    fn parse_function_call_rest_arguments(
        &mut self,
    ) -> Result<Option<FunctionCallRestArguments>, ParseError> {
        let context = ParseContext::ExpressionFunctionCall;
        let left_parenthesis = self.eat_token(TokenType::LeftParenthesis, context)?;
        let arguments = {
            let mut arguments: Vec<Expression> = vec![];
            match self.tokens.peek() {
                Some(Token {
                    token_type: TokenType::RightParenthesis,
                    ..
                }) => arguments,
                _ => loop {
                    arguments.push(self.parse_expression()?);
                    if self.try_eat_token(TokenType::Comma).is_none() {
                        break arguments;
                    }
                },
            }
        };
        let right_parenthesis = self.eat_token(TokenType::RightParenthesis, context)?;
        Ok(Some(FunctionCallRestArguments {
            left_parenthesis,
            arguments,
            right_parenthesis,
        }))
    }

    fn try_parse_function_call(
        &mut self,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        match self.tokens.peek() {
            Some(Token {
                token_type: TokenType::Period,
                ..
            }) => {
                let function_call = self.parse_function_call_or_property_access(first_argument)?;
                Ok(function_call)
            }
            _ => Ok(first_argument),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.tokens.peek() {
            match &token.token_type {
                TokenType::Backslash => {
                    let backslash_token = self.tokens.next().unwrap().clone();
                    let function = self.parse_function(backslash_token)?;
                    Ok(Expression::Function(Box::new(function)))
                }
                TokenType::KeywordLet => {
                    let let_token = self.tokens.next().unwrap().clone();
                    self.parse_let_expression(let_token)
                }
                _ => {
                    let simple_expression = self.parse_simple_expression()?;
                    self.try_parse_function_call(simple_expression)
                }
            }
        } else {
            Err(Parser::unexpected_eof(ParseContext::Expression))
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParseError> {
        let context = ParseContext::Expression;
        if let Some(token) = self.tokens.next() {
            match &token.token_type {
                TokenType::String => Ok(Expression::String(token.clone())),
                TokenType::Character => Ok(Expression::Character(token.clone())),
                TokenType::LeftCurlyBracket => self.parse_record(token.clone()),
                TokenType::LeftSquareBracket => self.parse_array(token.clone()),
                TokenType::Float => Ok(Expression::Float(token.clone())),
                TokenType::Integer => Ok(Expression::Integer(token.clone())),
                TokenType::KeywordTrue => Ok(Expression::Boolean {
                    token: token.clone(),
                    value: true,
                }),
                TokenType::KeywordFalse => Ok(Expression::Boolean {
                    token: token.clone(),
                    value: false,
                }),
                TokenType::KeywordNull => Ok(Expression::Null(token.clone())),
                TokenType::Identifier => match self.tokens.peek() {
                    Some(Token {
                        token_type: TokenType::ScopeResolution,
                        ..
                    })
                    | Some(Token {
                        token_type: TokenType::LeftParenthesis,
                        ..
                    }) => {
                        let context = ParseContext::ExpressionEnumConstructor;
                        let scoped_name = self.parse_scoped_name(token.clone())?;
                        let left_parenthesis =
                            self.eat_token(TokenType::LeftParenthesis, context)?;
                        if let Some(right_parenthesis) =
                            self.try_eat_token(TokenType::RightParenthesis)
                        {
                            Ok(Expression::EnumConstructor {
                                scoped_name,
                                left_parenthesis,
                                payload: None,
                                right_parenthesis,
                            })
                        } else {
                            let payload = self.parse_expression()?;
                            let right_parenthesis =
                                self.eat_token(TokenType::RightParenthesis, context)?;

                            Ok(Expression::EnumConstructor {
                                scoped_name,
                                left_parenthesis,
                                payload: Some(Box::new(payload)),
                                right_parenthesis,
                            })
                        }
                    }
                    _ => Ok(Expression::Variable(token.clone())),
                },
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn parse_array(&mut self, left_square_bracket: Token) -> Result<Expression, ParseError> {
        let context = ParseContext::ExpressionArray;
        let mut elements: Vec<Expression> = Vec::new();
        loop {
            if let Some(Token {
                token_type: TokenType::RightSquareBracket,
                ..
            }) = self.tokens.peek()
            {
                break;
            };
            elements.push(self.parse_expression()?);
            if self.try_eat_token(TokenType::Comma).is_none() {
                break;
            }
        }
        let right_square_bracket = self.eat_token(TokenType::RightSquareBracket, context)?;
        Ok(Expression::Array {
            left_square_bracket,
            elements,
            right_square_bracket,
        })
    }

    fn parse_record(&mut self, left_curly_bracket: Token) -> Result<Expression, ParseError> {
        let context = ParseContext::ExpressionRecord;
        let spread = if self.try_eat_token(TokenType::Spread).is_some() {
            let expression = self.parse_expression()?;
            match self.tokens.peek() {
                Some(Token {
                    token_type: TokenType::RightCurlyBracket,
                    ..
                }) => {
                    self.try_eat_token(TokenType::Comma);
                }
                _ => {
                    if let Err(error) = self.eat_token(TokenType::Comma, context) {
                        return Err(error);
                    }
                }
            };
            Some(Box::new(expression))
        } else {
            None
        };
        let mut key_value_pairs: Vec<RecordKeyValue> = Vec::new();
        loop {
            match self.tokens.peek() {
                Some(Token {
                    token_type: TokenType::RightCurlyBracket,
                    ..
                }) => break,
                _ => {
                    let key = self.eat_token(TokenType::Identifier, context)?;
                    let type_annotation = self.try_parse_colon_type_annotation(context)?;
                    let value = if self.try_eat_token(TokenType::Equals).is_some() {
                        self.parse_expression()?
                    } else {
                        Expression::Variable(key.clone())
                    };

                    key_value_pairs.push(RecordKeyValue {
                        key: key.clone(),
                        type_annotation,
                        value,
                    })
                }
            }
            if self.try_eat_token(TokenType::Comma).is_none() {
                break;
            }
        }
        let right_curly_bracket = self.eat_token(TokenType::RightCurlyBracket, context)?;
        Ok(Expression::Record {
            spread,
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
        })
    }

    fn parse_let_expression(&mut self, keyword_let: Token) -> Result<Expression, ParseError> {
        let context = ParseContext::ExpressionLet;
        let left = self.parse_destructure_pattern()?;
        let type_annotation = if self.try_eat_token(TokenType::Colon).is_some() {
            Some(self.parse_type_annotation(context)?)
        } else {
            None
        };
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_expression()?;
        let else_return = if self.try_eat_token(TokenType::KeywordElse).is_some() {
            let backslash_token = self.eat_token(TokenType::Backslash, context)?;
            Some(Box::new(self.parse_function(backslash_token)?))
        } else {
            None
        };
        let return_value = self.parse_expression()?;
        Ok(Expression::Let {
            keyword_let,
            left: Box::new(left),
            type_annotation,
            right: Box::new(right),
            false_branch: else_return,
            true_branch: Box::new(return_value),
        })
    }

    fn parse_scoped_name(&mut self, first_token: Token) -> Result<ScopedName, ParseError> {
        let mut namespaces = vec![];
        let namespaces = loop {
            match self.tokens.peek() {
                Some(Token {
                    token_type: TokenType::ScopeResolution,
                    ..
                }) => {
                    self.tokens.next();
                    let namespace =
                        self.eat_token(TokenType::Identifier, ParseContext::ScopeResolution)?;
                    namespaces.push(namespace)
                }
                _ => break namespaces,
            }
        };
        match namespaces.split_last() {
            None => Ok(ScopedName {
                name: first_token,
                namespaces,
            }),
            Some((last, namespaces)) => {
                let mut namespaces = namespaces.to_vec();
                namespaces.push(first_token);
                Ok(ScopedName {
                    name: last.clone(),
                    namespaces,
                })
            }
        }
    }

    fn parse_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        if let Some(token) = self.tokens.next() {
            match &token.token_type {
                TokenType::Identifier => match self.tokens.peek() {
                    Some(Token {
                        token_type: TokenType::ScopeResolution,
                        ..
                    })
                    | Some(Token {
                        token_type: TokenType::LeftParenthesis,
                        ..
                    }) => {
                        let scoped_name = self.parse_scoped_name(token.clone())?;
                        let context = ParseContext::PatternEnum;
                        let left_parenthesis =
                            self.eat_token(TokenType::LeftParenthesis, context)?;
                        if let Some(right_parenthesis) =
                            self.try_eat_token(TokenType::RightParenthesis)
                        {
                            Ok(DestructurePattern::EnumConstructor {
                                scoped_name,
                                left_parenthesis,
                                payload: None,
                                right_parenthesis,
                            })
                        } else {
                            let destructure_pattern = self.parse_destructure_pattern()?;
                            let right_parenthesis =
                                self.eat_token(TokenType::RightParenthesis, context)?;
                            Ok(DestructurePattern::EnumConstructor {
                                scoped_name,
                                left_parenthesis,
                                payload: Some(Box::new(destructure_pattern)),
                                right_parenthesis,
                            })
                        }
                    }
                    _ => Ok(DestructurePattern::Identifier(token.clone())),
                },
                TokenType::Underscore => Ok(DestructurePattern::Underscore(token.clone())),
                TokenType::LeftCurlyBracket => {
                    let context = ParseContext::PatternRecord;
                    let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = Vec::new();
                    loop {
                        if self.try_eat_token(TokenType::RightCurlyBracket).is_some() {
                            break;
                        }
                        let key = self.eat_token(TokenType::Identifier, context)?;
                        let type_annotation = self.try_parse_colon_type_annotation(context)?;
                        let as_value = if self.try_eat_token(TokenType::Equals).is_some() {
                            Some(self.parse_destructure_pattern()?)
                        } else {
                            None
                        };
                        key_value_pairs.push(DestructuredRecordKeyValue {
                            key,
                            type_annotation,
                            as_value,
                        });
                        if self.try_eat_token(TokenType::Comma).is_none() {
                            break;
                        }
                    }
                    let right_curly_bracket =
                        self.eat_token(TokenType::RightCurlyBracket, context)?;
                    Ok(DestructurePattern::Record {
                        left_curly_bracket: token.clone(),
                        key_value_pairs,
                        right_curly_bracket,
                    })
                }
                TokenType::Integer => Ok(DestructurePattern::Infinite {
                    token: token.clone(),
                    kind: InfinitePatternKind::Integer,
                }),
                TokenType::String => Ok(DestructurePattern::Infinite {
                    token: token.clone(),
                    kind: InfinitePatternKind::String,
                }),
                TokenType::Character => Ok(DestructurePattern::Infinite {
                    token: token.clone(),
                    kind: InfinitePatternKind::Character,
                }),
                TokenType::KeywordTrue => Ok(DestructurePattern::Boolean {
                    token: token.clone(),
                    value: true,
                }),
                TokenType::KeywordFalse => Ok(DestructurePattern::Boolean {
                    token: token.clone(),
                    value: false,
                }),
                TokenType::KeywordNull => Ok(DestructurePattern::Null(token.clone())),
                TokenType::LeftSquareBracket => {
                    let context = ParseContext::PatternArray;
                    let left_square_bracket = token.clone();
                    if let Some(right_square_bracket) =
                        self.try_eat_token(TokenType::RightSquareBracket)
                    {
                        Ok(DestructurePattern::Array {
                            left_square_bracket,
                            right_square_bracket,
                            spread: None,
                        })
                    } else {
                        let first_element = Box::new(self.parse_destructure_pattern()?);
                        self.eat_token(TokenType::Comma, context)?;
                        let spread_token = self.eat_token(TokenType::Spread, context)?;
                        let rest_elements = Box::new(self.parse_destructure_pattern()?);
                        let right_square_bracket =
                            self.eat_token(TokenType::RightSquareBracket, context)?;
                        Ok(DestructurePattern::Array {
                            left_square_bracket,
                            right_square_bracket,
                            spread: Some(DestructurePatternArraySpread {
                                first_element,
                                spread_token,
                                rest_elements,
                            }),
                        })
                    }
                }
                _ => Err(Parser::invalid_token(token.clone(), ParseContext::Pattern)),
            }
        } else {
            Err(Parser::unexpected_eof(ParseContext::Pattern))
        }
    }
}
