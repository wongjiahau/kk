use crate::{ast::*, tokenize::TokenizeError, unify::join_position};
use crate::{non_empty::NonEmpty, tokenize::Tokenizer};

#[derive(Debug)]
pub struct ParseError {
    pub context: Option<ParseContext>,
    pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    TokenizeError(TokenizeError),
    UnnecessaryParenthesisForSingleArgumentFunctionCall {
        position: Position,
    },
    InvalidToken {
        actual_token: Token,
        expected_token_type: Option<TokenType>,
    },
    UnexpectedEof {
        expected_token_type: Option<TokenType>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseContext {
    // Expression
    Expression,
    ExpressionLet,
    ExpressionFunction,
    DotExpression,
    ExpressionFunctionCall,
    ExpressionRecord,
    ExpressionRecordUpdate,
    ExpressionEnumConstructor,
    ExpressionQuoted,
    ExpressionMonadicLet,

    // Statement
    Statement,
    StatementLet,
    StatementType,
    StatementEnum,
    StatementImport,

    // Type annotation
    TypeAnnotationRecord,
    TypeAnnotationArray,
    TypeAnnotationFunction,
    TypeAnnotationQuoted,
    TypeAnnotationPromise,

    // Destructure pattern
    Pattern,
    PatternArray,
    PatternEnum,
    PatternRecord,

    // Others
    TypeArguments,
    EnumConstructorDefinition,
    TypeVariablesDeclaration,
    DocumentationCodeSnippet,
}

pub struct Parser<'a> {
    tokenizer: &'a mut Tokenizer,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: &mut Tokenizer) -> Parser {
        Parser { tokenizer }
    }
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Vec<Statement>, ParseError> {
        let mut parser = Parser { tokenizer };
        parser.parse_statements()
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

    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::<Statement>::new();
        while let Some(token) = self.peek_next_meaningful_token()? {
            match token.token_type {
                TokenType::TripleBacktick => return Ok(statements),
                _ => {
                    statements.extend(self.parse_statement()?);
                }
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Vec<Statement>, ParseError> {
        let context = Some(ParseContext::Statement);
        fn vectorized(statement: Statement) -> Vec<Statement> {
            vec![statement]
        }
        match self.next_meaningful_token()? {
            Some(token) => match token.token_type {
                TokenType::KeywordLet => self
                    .parse_let_statement(None, token.clone())
                    .map(vectorized),
                TokenType::KeywordType => self
                    .parse_type_statement(None, token.clone())
                    .map(vectorized),
                TokenType::KeywordEnum => self
                    .parse_enum_statement(None, token.clone())
                    .map(vectorized),
                TokenType::KeywordDo => self.parse_do_statement(token.clone()).map(vectorized),
                TokenType::KeywordImport => {
                    self.parse_import_statement(token.clone()).map(vectorized)
                }
                TokenType::KeywordExport => {
                    let keyword_export = Some(token.clone());
                    match self.next_meaningful_token()? {
                        Some(token) => match token.token_type {
                            TokenType::KeywordLet => {
                                self.parse_let_statement(keyword_export, token)
                            }
                            TokenType::KeywordType => {
                                self.parse_type_statement(keyword_export, token)
                            }
                            TokenType::KeywordEnum => {
                                self.parse_enum_statement(keyword_export, token)
                            }
                            _ => Err(Parser::invalid_token(token, context)),
                        },
                        None => Err(Parser::unexpected_eof(context)),
                    }
                    .map(vectorized)
                }
                TokenType::MultilineComment { characters } => {
                    let mut tokenizer = Tokenizer::from_characters(characters);
                    let mut statements = vec![];
                    while let Some(token) = tokenizer.next()? {
                        if let TokenType::TripleBacktick = token.token_type {
                            let mut parser = Parser::new(&mut tokenizer);
                            statements.extend(parser.parse_statements()?);
                            parser.eat_token(
                                TokenType::TripleBacktick,
                                Some(ParseContext::DocumentationCodeSnippet),
                            )?;
                        }
                    }

                    Ok(statements)
                }
                _ => Err(Parser::invalid_token(token.clone(), context)),
            },
            None => Err(Parser::unexpected_eof(context)),
        }
    }

    fn parse_let_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_let: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementLet);
        let left = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Colon, context)?;
        let type_annotation = self.parse_type_annotation(context)?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_expression()?;
        Ok(Statement::Let(LetStatement {
            keyword_export,
            keyword_let,
            left,
            right,
            type_variables,
            type_annotation,
        }))
    }

    fn parse_type_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_type: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementType);
        let left = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_type_annotation(context)?;
        Ok(Statement::Type(TypeAliasStatement {
            keyword_export,
            keyword_type,
            left,
            type_variables,
            right,
        }))
    }

    fn parse_enum_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_enum: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementEnum);
        let name = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;

        let mut constructors = vec![self.parse_enum_constructor_definition()?];

        while let Some(Token {
            token_type: TokenType::Identifier,
            ..
        }) = self.peek_next_meaningful_token()?
        {
            constructors.push(self.parse_enum_constructor_definition()?)
        }
        Ok(Statement::Enum(EnumStatement {
            keyword_export,
            keyword_enum,
            name,
            type_variables,
            constructors,
        }))
    }

    fn parse_do_statement(&mut self, keyword_do: Token) -> Result<Statement, ParseError> {
        let expression = self.parse_expression()?;
        Ok(Statement::Do(DoStatement {
            keyword_do,
            expression,
        }))
    }

    fn parse_import_statement(&mut self, keyword_import: Token) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementImport);
        let url = self.eat_token(TokenType::String, context)?;
        self.eat_token(TokenType::LeftCurlyBracket, context)?;
        let first_name = {
            let name = self.eat_token(TokenType::Identifier, context)?;
            if self.try_eat_token(TokenType::Colon)?.is_some() {
                let alias_as = self.eat_token(TokenType::Identifier, context)?;
                ImportedName {
                    name,
                    alias_as: Some(alias_as),
                }
            } else {
                ImportedName {
                    name,
                    alias_as: None,
                }
            }
        };
        let other_names = {
            let mut other_names = Vec::new();
            loop {
                if let Some(name) = self.try_eat_token(TokenType::Identifier)? {
                    if self.try_eat_token(TokenType::Colon)?.is_some() {
                        let alias_as = self.eat_token(TokenType::Identifier, context)?;
                        other_names.push(ImportedName {
                            name,
                            alias_as: Some(alias_as),
                        })
                    } else {
                        other_names.push(ImportedName {
                            name,
                            alias_as: None,
                        })
                    }
                } else {
                    break other_names;
                }
            }
        };
        self.eat_token(TokenType::RightCurlyBracket, context)?;
        Ok(Statement::Import(ImportStatement {
            keyword_import,
            url,
            imported_names: NonEmpty {
                head: first_name,
                tail: other_names,
            },
        }))
    }

    pub fn eat_token(
        &mut self,
        token_type: TokenType,
        context: Option<ParseContext>,
    ) -> Result<Token, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            if variant_eq(&token.token_type, &token_type) {
                Ok(token)
            } else {
                Err(ParseError {
                    context,
                    kind: ParseErrorKind::InvalidToken {
                        actual_token: token,
                        expected_token_type: Some(token_type),
                    },
                })
            }
        } else {
            Err(ParseError {
                context,
                kind: ParseErrorKind::UnexpectedEof {
                    expected_token_type: Some(token_type),
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

    fn try_parse_type_arguments(&mut self) -> Result<Option<TypeArguments>, ParseError> {
        let context = Some(ParseContext::TypeArguments);
        if let Some(left_angular_bracket) = self.try_eat_token(TokenType::LessThan)? {
            let mut arguments = Vec::new();
            loop {
                arguments.push(self.parse_type_annotation(context)?);
                if let Some(right_angular_bracket) = self.try_eat_token(TokenType::MoreThan)? {
                    return Ok(Some(TypeArguments {
                        left_angular_bracket,
                        arguments,
                        right_angular_bracket,
                    }));
                }
            }
        } else {
            Ok(None)
        }
    }

    fn parse_enum_constructor_definition(
        &mut self,
    ) -> Result<EnumConstructorDefinition, ParseError> {
        let context = Some(ParseContext::EnumConstructorDefinition);
        let name = self.eat_token(TokenType::Identifier, context)?;
        if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
            let type_annotation = self.parse_type_annotation(context)?;
            let right_parenthesis = self.eat_token(TokenType::RightParenthesis, context)?;
            Ok(EnumConstructorDefinition {
                name,
                payload: Some(Box::new(EnumConstructorDefinitionPayload {
                    left_parenthesis,
                    type_annotation,
                    right_parenthesis,
                })),
            })
        } else {
            Ok(EnumConstructorDefinition {
                name,
                payload: None,
            })
        }
    }

    fn try_parse_type_variables_declaration(&mut self) -> Result<Vec<Token>, ParseError> {
        let context = Some(ParseContext::TypeVariablesDeclaration);
        if self.try_eat_token(TokenType::LessThan)?.is_some() {
            let mut type_variables = vec![];
            loop {
                type_variables.push(self.eat_token(TokenType::Identifier, context)?);
                if self.try_eat_token(TokenType::MoreThan)?.is_some() {
                    return Ok(type_variables);
                }
            }
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_function(&mut self, pipe_token: Token) -> Result<Function, ParseError> {
        if let Some(period) = self.try_eat_token(TokenType::Period)? {
            // This is a function shorthand expression
            let phantom_variable = Token {
                token_type: TokenType::Identifier,
                representation: "$".to_string(),
                position: period.position,
            };
            let function_body =
                self.parse_dot_expression(period, Expression::Variable(phantom_variable.clone()))?;
            Ok(Function {
                branches: NonEmpty {
                    head: FunctionBranch {
                        start_token: phantom_variable.clone(),
                        parameters: NonEmpty {
                            head: DestructurePattern::Identifier(phantom_variable),
                            tail: vec![],
                        },
                        body: Box::new(function_body),
                    },
                    tail: vec![],
                },
            })
        } else {
            // This is a normal function literal
            let mut branches = Vec::<FunctionBranch>::new();
            let first_branch = self.parse_function_branch(pipe_token)?;
            while let Some(Token {
                token_type: TokenType::Pipe,
                ..
            }) = self.peek_next_meaningful_token()?
            {
                let pipe_token = self.next_meaningful_token()?.unwrap().clone();
                let branch = self.parse_function_branch(pipe_token)?;
                branches.push(branch);
            }
            Ok(Function {
                branches: NonEmpty {
                    head: first_branch,
                    tail: branches,
                },
            })
        }
    }

    fn parse_function_branch(&mut self, pipe_token: Token) -> Result<FunctionBranch, ParseError> {
        let context = Some(ParseContext::ExpressionFunction);
        let parameters = self.parse_function_parameters()?;
        self.eat_token(TokenType::FatArrowRight, context)?;
        let body = self.parse_expression()?;
        Ok(FunctionBranch {
            start_token: pipe_token,
            parameters,
            body: Box::new(body),
        })
    }
    fn parse_function_parameters(&mut self) -> Result<NonEmpty<DestructurePattern>, ParseError> {
        let first_parameter = self.parse_destructure_pattern()?;
        let mut rest_parameters = Vec::<DestructurePattern>::new();
        loop {
            match self.peek_next_meaningful_token()? {
                Some(Token {
                    token_type: TokenType::FatArrowRight,
                    ..
                }) => break,
                _ => {
                    rest_parameters.push(self.parse_destructure_pattern()?);
                }
            };
        }
        Ok(NonEmpty {
            head: first_parameter,
            tail: rest_parameters,
        })
    }

    fn parse_type_annotation(
        &mut self,
        context: Option<ParseContext>,
    ) -> Result<TypeAnnotation, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::Identifier | TokenType::KeywordNull => {
                    let name = token;
                    Ok(TypeAnnotation::Named {
                        name,
                        type_arguments: self.try_parse_type_arguments()?,
                    })
                }
                TokenType::Backtick => {
                    let context = Some(ParseContext::TypeAnnotationQuoted);
                    let opening_backtick = token;
                    let type_annotation = self.parse_type_annotation(context)?;
                    let closing_backtick = self.eat_token(TokenType::Backtick, context)?;
                    Ok(TypeAnnotation::Quoted {
                        opening_backtick,
                        type_annotation: Box::new(type_annotation),
                        closing_backtick,
                    })
                }
                TokenType::LeftSquareBracket => {
                    let context = Some(ParseContext::TypeAnnotationArray);
                    let left_square_bracket = token;
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
                    let left_curly_bracket = token;
                    let context = Some(ParseContext::TypeAnnotationRecord);
                    let mut key_type_annotation_pairs: Vec<(Token, TypeAnnotation)> = Vec::new();
                    let right_curly_bracket = loop {
                        if let Some(right_curly_bracket) =
                            self.try_eat_token(TokenType::RightCurlyBracket)?
                        {
                            break right_curly_bracket;
                        }
                        let key = self.eat_token(TokenType::Identifier, context)?;
                        self.eat_token(TokenType::Colon, context)?;
                        let type_annotation = self.parse_type_annotation(context)?;
                        key_type_annotation_pairs.push((key, type_annotation));
                    };
                    Ok(TypeAnnotation::Record {
                        left_curly_bracket,
                        key_type_annotation_pairs,
                        right_curly_bracket,
                    })
                }
                TokenType::Pipe => {
                    let start_token = token;
                    let context = Some(ParseContext::TypeAnnotationFunction);
                    let (first_argument_type, rest_arguments_types) = {
                        let first_argument = self.parse_type_annotation(context)?;
                        let mut arguments_types = vec![];
                        loop {
                            if self.try_eat_token(TokenType::FatArrowRight)?.is_some() {
                                break;
                            }
                            arguments_types.push(self.parse_type_annotation(context)?)
                        }
                        (first_argument, arguments_types)
                    };
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
                TokenType::Bang => {
                    let bang_token = token;
                    let context = Some(ParseContext::TypeAnnotationPromise);
                    let type_annotation = self.parse_type_annotation(context)?;
                    Ok(TypeAnnotation::Promise {
                        bang_token,
                        type_annotation: Box::new(type_annotation),
                    })
                }
                _ => Err(Parser::invalid_token(token, context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    /// Dot expressions means either of the following:
    /// - function call, e.g. `x.f()`  
    /// - property access, e.g. `x.f`  
    /// - record update, e.g. `x.{ a 3 }` or `x.{ a.square() }`
    fn parse_dot_expression(
        &mut self,
        _period: Token,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::DotExpression);

        let first_argument = {
            if let Some(left_curly_bracket) = self.try_eat_token(TokenType::LeftCurlyBracket)? {
                // Then this is a record update
                self.parse_record_update(first_argument, left_curly_bracket)?
            } else if self.try_eat_token(TokenType::LeftParenthesis)?.is_some() {
                // Then this is a immediately invoked function
                let function = self.parse_expression()?;
                self.eat_token(
                    TokenType::RightParenthesis,
                    Some(ParseContext::ExpressionFunctionCall),
                )?;
                let type_arguments = self.try_parse_type_arguments()?;
                let function_call_arguments = self.parse_function_call_rest_arguments()?;
                Expression::FunctionCall(Box::new(FunctionCall {
                    first_argument: Box::new(first_argument),
                    rest_arguments: function_call_arguments,
                    type_arguments,
                    function: Box::new(function),
                }))
            } else {
                let name = self.eat_token(TokenType::Identifier, context)?;
                match self.peek_next_meaningful_token()? {
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
                            function: Box::new(Expression::Variable(name)),
                        }))
                    }
                    _ => {
                        // Then this is a property access or single-argument function call
                        Expression::RecordAccessOrFunctionCall {
                            expression: Box::new(first_argument),
                            property_name: name,
                        }
                    }
                }
            }
        };
        self.try_parse_dot_expression(first_argument)
    }

    fn parse_record_update(
        &mut self,
        expression: Expression,
        left_curly_bracket: Token,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecordUpdate);
        let mut record_updates: Vec<RecordUpdate> = Vec::new();
        let (record_updates, right_curly_bracket) = loop {
            let property_name = self.eat_token(TokenType::Identifier, context)?;
            if let Some(period) = self.try_eat_token(TokenType::Period)? {
                // Then this is functional update
                let phantom_variable = Expression::Variable(property_name.clone());
                let function_body = self.parse_dot_expression(period.clone(), phantom_variable)?;
                record_updates.push(RecordUpdate::FunctionalUpdate {
                    function: Expression::Function(Box::new(Function {
                        branches: NonEmpty {
                            head: FunctionBranch {
                                start_token: period.clone(),
                                parameters: NonEmpty {
                                    head: DestructurePattern::Identifier(property_name.clone()),
                                    tail: vec![],
                                },
                                body: Box::new(function_body),
                            },
                            tail: vec![],
                        },
                    })),
                    property_name,
                })
            } else if self.try_eat_token(TokenType::Colon)?.is_some() {
                // this is a value update
                let new_value = self.parse_expression()?;
                record_updates.push(RecordUpdate::ValueUpdate {
                    property_name,
                    new_value,
                })
            } else {
                // this is a value update with punning
                record_updates.push(RecordUpdate::ValueUpdate {
                    property_name: property_name.clone(),
                    new_value: Expression::Variable(property_name),
                })
            }
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break (record_updates, right_curly_bracket);
            }
        };

        Ok(Expression::RecordUpdate {
            left_curly_bracket,
            expression: Box::new(expression),
            updates: record_updates,
            right_curly_bracket,
        })
    }

    fn parse_function_call_rest_arguments(
        &mut self,
    ) -> Result<Option<FunctionCallRestArguments>, ParseError> {
        let context = Some(ParseContext::ExpressionFunctionCall);
        if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
            let arguments = {
                let mut arguments: Vec<Expression> = vec![];
                match self.peek_next_meaningful_token()? {
                    Some(Token {
                        token_type: TokenType::RightParenthesis,
                        ..
                    }) => arguments,
                    _ => loop {
                        arguments.push(self.parse_expression()?);
                        if self.try_eat_token(TokenType::Comma)?.is_none() {
                            break arguments;
                        }
                    },
                }
            };
            let right_parenthesis = self.eat_token(TokenType::RightParenthesis, context)?;
            if arguments.is_empty() {
                Err(ParseError {
                    context,
                    kind: ParseErrorKind::UnnecessaryParenthesisForSingleArgumentFunctionCall {
                        position: join_position(
                            left_parenthesis.position,
                            right_parenthesis.position,
                        ),
                    },
                })
            } else {
                Ok(Some(FunctionCallRestArguments {
                    left_parenthesis,
                    arguments,
                    right_parenthesis,
                }))
            }
        } else {
            Ok(None)
        }
    }

    fn try_parse_dot_expression(
        &mut self,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        match self.peek_next_meaningful_token()? {
            Some(Token {
                token_type: TokenType::Period,
                ..
            }) => {
                let period =
                    self.eat_token(TokenType::Period, Some(ParseContext::DotExpression))?;
                self.parse_dot_expression(period, first_argument)
            }
            _ => Ok(first_argument),
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.peek_next_meaningful_token()? {
            match &token.token_type {
                TokenType::Pipe => {
                    let pipe_token = self.next_meaningful_token()?.unwrap();
                    let function = self.parse_function(pipe_token)?;
                    Ok(Expression::Function(Box::new(function)))
                }
                TokenType::KeywordIf => {
                    let keyword_if = self.next_meaningful_token()?.unwrap();
                    self.parse_if_expression(keyword_if)
                }
                TokenType::KeywordLet => {
                    let let_token = self.next_meaningful_token()?.unwrap();
                    self.parse_let_expression(let_token)
                }
                TokenType::JavascriptCode => Ok(Expression::UnsafeJavascript {
                    code: self.next_meaningful_token()?.unwrap(),
                }),
                TokenType::Bang => Ok(Expression::Promise {
                    bang: self.next_meaningful_token()?.unwrap(),
                    expression: Box::new(self.parse_expression()?),
                }),
                _ => {
                    let simple_expression = self.parse_simple_expression()?;
                    self.try_parse_dot_expression(simple_expression)
                }
            }
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Expression)))
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Expression);
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String => Ok(Expression::String(token.clone())),
                TokenType::InterpolatedString {
                    start_quote,
                    sections,
                    end_quote,
                } => Ok(Expression::InterpolatedString {
                    start_quote,
                    sections,
                    end_quote,
                }),
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
                TokenType::Identifier => {
                    if let Some(left_parenthesis) =
                        self.try_eat_token(TokenType::LeftParenthesis)?
                    {
                        let context = Some(ParseContext::ExpressionEnumConstructor);
                        let name = token.clone();
                        let expression = self.parse_expression()?;
                        let right_parenthesis =
                            self.eat_token(TokenType::RightParenthesis, context)?;

                        Ok(Expression::EnumConstructor {
                            name,
                            payload: Some(Box::new(ExpressionEnumConstructorPayload {
                                left_parenthesis,
                                expression,
                                right_parenthesis,
                            })),
                        })
                    } else {
                        Ok(Expression::Variable(token.clone()))
                    }
                }
                TokenType::Backtick => {
                    let context = Some(ParseContext::ExpressionQuoted);
                    let opening_backtick = token.clone();
                    let expression = self.parse_expression()?;
                    let closing_backtick = self.eat_token(TokenType::Backtick, context)?;
                    Ok(Expression::Quoted {
                        opening_backtick,
                        expression: Box::new(expression),
                        closing_backtick,
                    })
                }
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn parse_array(&mut self, left_square_bracket: Token) -> Result<Expression, ParseError> {
        let mut elements: Vec<Expression> = Vec::new();
        let right_square_bracket = loop {
            if let Some(right_square_bracket) = self.try_eat_token(TokenType::RightSquareBracket)? {
                break right_square_bracket;
            };
            elements.push(self.parse_expression()?);
        };
        Ok(Expression::Array {
            left_square_bracket,
            elements,
            right_square_bracket,
        })
    }

    fn parse_record(&mut self, left_curly_bracket: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecord);
        let mut key_value_pairs: Vec<RecordKeyValue> = Vec::new();
        loop {
            match self.peek_next_meaningful_token()? {
                Some(Token {
                    token_type: TokenType::RightCurlyBracket,
                    ..
                }) => break,
                _ => {
                    let key = self.eat_token(TokenType::Identifier, context)?;
                    let value = if self.try_eat_token(TokenType::Colon)?.is_some() {
                        self.parse_expression()?
                    } else {
                        Expression::Variable(key.clone())
                    };

                    key_value_pairs.push(RecordKeyValue {
                        key: key.clone(),
                        value,
                    })
                }
            }
        }
        let right_curly_bracket = self.eat_token(TokenType::RightCurlyBracket, context)?;
        Ok(Expression::Record {
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
        })
    }

    fn parse_if_expression(&mut self, keyword_if: Token) -> Result<Expression, ParseError> {
        Ok(Expression::If {
            keyword_if,
            condition: Box::new(self.parse_expression()?),
            if_true: Box::new(self.parse_expression()?),
            if_false: Box::new(self.parse_expression()?),
        })
    }

    fn parse_let_expression(&mut self, keyword_let: Token) -> Result<Expression, ParseError> {
        if self.try_eat_token(TokenType::Slash)?.is_some() {
            let context = Some(ParseContext::ExpressionMonadicLet);
            let binary_function_name = self.eat_token(TokenType::Identifier, context)?;
            let left_patterns = {
                let first_left_pattern = self.parse_destructure_pattern()?;
                let mut tail_left_patterns = vec![];
                loop {
                    match self.peek_next_meaningful_token()? {
                        Some(Token {
                            token_type: TokenType::Equals,
                            ..
                        }) => {
                            break NonEmpty {
                                head: first_left_pattern,
                                tail: tail_left_patterns,
                            }
                        }
                        _ => tail_left_patterns.push(self.parse_destructure_pattern()?),
                    }
                }
            };
            self.eat_token(TokenType::Equals, context)?;
            return Ok(Expression::ApplicativeLet(ApplicativeLet {
                keyword_let,
                left_patterns,
                binary_function_name,
                right: Box::new(self.parse_expression()?),
                body: Box::new(self.parse_expression()?),
            }));
        }
        let context = Some(ParseContext::ExpressionLet);
        let left = self.parse_destructure_pattern()?;
        let type_annotation = if self.try_eat_token(TokenType::Colon)?.is_some() {
            Some(self.parse_type_annotation(context)?)
        } else {
            None
        };
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_expression()?;
        let return_value = self.parse_expression()?;
        Ok(Expression::Let {
            keyword_let,
            left: Box::new(left),
            type_annotation,
            right: Box::new(right),
            body: Box::new(return_value),
        })
    }

    fn parse_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            match &token.token_type {
                TokenType::Identifier => match self.peek_next_meaningful_token()? {
                    Some(Token {
                        token_type: TokenType::LeftParenthesis,
                        ..
                    }) => {
                        let context = Some(ParseContext::PatternEnum);
                        let name = token.clone();
                        let left_parenthesis =
                            self.eat_token(TokenType::LeftParenthesis, context)?;
                        let pattern = self.parse_destructure_pattern()?;
                        let right_parenthesis =
                            self.eat_token(TokenType::RightParenthesis, context)?;
                        Ok(DestructurePattern::EnumConstructor {
                            name,
                            payload: Some(Box::new(DestructurePatternEnumConstructorPayload {
                                left_parenthesis,
                                pattern,
                                right_parenthesis,
                            })),
                        })
                    }
                    _ => Ok(DestructurePattern::Identifier(token.clone())),
                },
                TokenType::Underscore => Ok(DestructurePattern::Underscore(token.clone())),
                TokenType::LeftCurlyBracket => {
                    let context = Some(ParseContext::PatternRecord);
                    let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = Vec::new();
                    let right_curly_bracket = loop {
                        if let Some(right_curly_bracket) =
                            self.try_eat_token(TokenType::RightCurlyBracket)?
                        {
                            break right_curly_bracket;
                        }
                        let key = self.eat_token(TokenType::Identifier, context)?;
                        let as_value = if self.try_eat_token(TokenType::Colon)?.is_some() {
                            Some(self.parse_destructure_pattern()?)
                        } else {
                            None
                        };
                        key_value_pairs.push(DestructuredRecordKeyValue { key, as_value });
                    };
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
                    let context = Some(ParseContext::PatternArray);
                    let left_square_bracket = token.clone();
                    if let Some(right_square_bracket) =
                        self.try_eat_token(TokenType::RightSquareBracket)?
                    {
                        Ok(DestructurePattern::Array {
                            left_square_bracket,
                            right_square_bracket,
                            spread: None,
                        })
                    } else {
                        let first_element = Box::new(self.parse_destructure_pattern()?);
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
                _ => Err(Parser::invalid_token(
                    token.clone(),
                    Some(ParseContext::Pattern),
                )),
            }
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Pattern)))
        }
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
