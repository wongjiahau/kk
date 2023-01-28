use crate::formatter::ToDoc;
use crate::module::Access;
use crate::simple_ast::{self, MacroExpandedList, MacroExpandedNode};
use crate::stringify_error::stringify_type;
use crate::tokenize::{StringLiteral, Token, TokenType};
use crate::{non_empty::NonEmpty, tokenize::Tokenizer, unify::Positionable};
use crate::{raw_ast::*, tokenize::TokenizeError};

#[derive(Debug)]
pub struct ParseError {
    pub context: Option<ParseContext>,
    pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    TokenizeError(TokenizeError),
    InvalidToken {
        actual_token: Token,
        expected_token_type: Option<TokenType>,
    },
    UnexpectedEof {
        expected_token_type: Option<TokenType>,
    },
    RecordWilcardCanOnlyAppearOnce {
        position: Position,
    },
    ExpectedPattern {
        position: Position,
    },
    ExpectedNone {
        position: Position,
    },
    ExpectedNode {
        previous_position: Position,
    },
    UnexpectedNode {
        position: Position,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseContext {
    // Expression
    Expression,
    ExpressionFunction,
    DotExpression,
    ExpressionFunctionCall,
    ExpressionRecord,
    ExpressionRecordUpdate,
    ExpressionEnumConstructor,

    // Statement
    Statement,
    StatementLet,
    StatementType,
    StatementEnum,
    StatementImport,

    // Type annotation
    TypeAnnotationRecord,
    TypeAnnotationArray,

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
    FunctionParameters,
    FunctionParameter,
    TypeVariableConstraint,
    TypeAnnotationFunction,
    Lambda,
    EntryStatement,
    BlockLevelStatement,
    NodeIdentifier,
    NodeArray,
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
        while self.peek_next_meaningful_token()?.is_some() {
            statements.extend(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_access(&mut self) -> Result<Access, ParseError> {
        Ok(
            if let Some(keyword_public) = self.try_eat_token(TokenType::KeywordPublic)? {
                Access::Public { keyword_public }
            } else if let Some(keyword_export) = self.try_eat_token(TokenType::KeywordExport)? {
                Access::Exported { keyword_export }
            } else {
                Access::Protected
            },
        )
    }
    fn parse_statement(&mut self) -> Result<Vec<Statement>, ParseError> {
        let context = Some(ParseContext::Statement);
        fn vectorized(statement: Statement) -> Vec<Statement> {
            vec![statement]
        }
        if let Some(keyword_entry) = self.try_eat_token(TokenType::KeywordEntry)? {
            return Ok(vec![self.parse_entry_statement(keyword_entry)?]);
        }
        let access = self.parse_access()?;
        match self.peek_next_meaningful_token()? {
            Some(token) => match token.token_type {
                TokenType::KeywordLet => {
                    let token = self.next_meaningful_token()?.unwrap();
                    Ok(vec![Statement::Let(
                        self.parse_top_level_let_statement(access, token)?,
                    )])
                }
                TokenType::KeywordType => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_type_alias_statement(access, token)
                        .map(vectorized)
                }
                TokenType::KeywordClass => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_enum_statement(access, token).map(vectorized)
                }
                TokenType::KeywordImport => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_import_statement(access, token).map(vectorized)
                }
                _ => Err(Parser::invalid_token(token, None)),
            },
            None => Err(Parser::unexpected_eof(context)),
        }
    }

    fn parse_entry_statement(&mut self, keyword_entry: Token) -> Result<Statement, ParseError> {
        Ok(Statement::Entry(EntryStatement {
            keyword_entry,
            expression: self.parse_low_precedence_expression()?,
        }))
    }

    fn parse_top_level_let_statement(
        &mut self,
        access: Access,
        keyword_let: Token,
    ) -> Result<LetStatement, ParseError> {
        let context = Some(ParseContext::StatementLet);

        let type_variables_declaration = self.try_parse_type_variables_declaration()?;

        let mut parameters = {
            if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
                vec![self.parse_parameter(left_parenthesis)?]
            } else {
                vec![]
            }
        };

        let name = {
            if !parameters.is_empty() {
                if self.try_eat_token(TokenType::Period)?.is_some() {
                    self.eat_token(TokenType::Identifier, context)?
                } else {
                    self.eat_token(TokenType::Operator, context)?
                }
            } else {
                self.eat_token(TokenType::Identifier, context)?
            }
        };

        parameters.extend(self.try_parse_parameters()?);

        self.eat_token(TokenType::Colon, context)?;
        let type_annotation = self.parse_type_annotation(context)?;

        let type_constraints = self.try_parse_type_constraints()?;

        let doc_string = self.try_eat_string_literal()?;

        self.eat_token(TokenType::Equals, context)?;
        let expression = self.parse_low_precedence_expression()?;

        let type_annotation = {
            let type_annotation = match parameters.split_last() {
                // If no parameter is found
                None => type_annotation,

                // If some parameters are found
                Some((last, init)) => {
                    TypeAnnotation::Function(Self::convert_to_function_type_annotation(
                        init.to_vec(),
                        FunctionTypeAnnotation {
                            parameter: Box::new(last.type_annotation.clone()),
                            return_type: Box::new(type_annotation),
                            // We place the type constraints at the most inner function type
                            //
                            // For example, if the function type is A -> B -> C, and we have a
                            // constraints X,
                            // X will be placed at (B -> C), not (A -> (B -> C))
                            //
                            // Therefore, we will get A -> ((B -> C) | X)
                            // instead of (A -> (B -> C)) | X
                            type_constraints,
                        },
                    ))
                }
            };

            match type_variables_declaration {
                Some(type_variables) => TypeAnnotation::Scheme {
                    type_variables,
                    type_annotation: Box::new(type_annotation),
                },
                None => type_annotation,
            }
        };
        Ok(LetStatement {
            access,
            keyword_let,
            name,
            doc_string,
            expression: Self::convert_to_function(parameters, expression),
            type_annotation,
        })
    }

    fn convert_to_function(parameters: Vec<Parameter>, return_value: Expression) -> Expression {
        match parameters.split_first() {
            Some((head, tail)) => Expression::Function(Box::new(Function {
                branches: NonEmpty {
                    head: FunctionBranch {
                        parameter: Box::new(head.pattern.clone()),
                        body: Box::new(Self::convert_to_function(tail.to_vec(), return_value)),
                    },
                    tail: vec![],
                },
            })),
            None => return_value,
        }
    }

    fn convert_to_function_type_annotation(
        parameters: Vec<Parameter>,
        return_type: FunctionTypeAnnotation,
    ) -> FunctionTypeAnnotation {
        match parameters.split_first() {
            Some((head, tail)) => FunctionTypeAnnotation {
                parameter: Box::new(head.type_annotation.clone()),
                return_type: Box::new(TypeAnnotation::Function(
                    Self::convert_to_function_type_annotation(tail.to_vec(), return_type),
                )),
                type_constraints: None,
            },
            None => return_type,
        }
    }

    fn try_parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = vec![];
        loop {
            if let Some(identifier) = self.try_eat_token(TokenType::Identifier)? {
                parameters.push(Parameter {
                    pattern: DestructurePattern::Identifier(identifier.clone()),
                    type_annotation: TypeAnnotation::Keyword { identifier },
                })
            } else if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
                parameters.push(self.parse_parameter(left_parenthesis)?)
            } else {
                return Ok(parameters);
            }
        }
    }

    fn parse_parameter(&mut self, left_parenthesis: Token) -> Result<Parameter, ParseError> {
        if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
            Ok(Parameter {
                pattern: DestructurePattern::Unit {
                    left_parenthesis: left_parenthesis.clone(),
                    right_parenthesis: right_parenthesis.clone(),
                },
                type_annotation: TypeAnnotation::Unit {
                    left_parenthesis,
                    right_parenthesis,
                },
            })
        } else {
            let pattern = self.parse_destructure_pattern()?;
            self.eat_token(TokenType::Colon, None)?;
            let type_annotation = self.parse_type_annotation(None)?;
            self.eat_token(TokenType::RightParenthesis, None)?;
            Ok(Parameter {
                pattern,
                type_annotation,
            })
        }
    }

    fn parse_type_alias_statement(
        &mut self,
        access: Access,
        keyword_type: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementType);
        let name = self.eat_token(TokenType::Identifier, context)?;
        let type_variables_declaration = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_type_annotation(context)?;
        Ok(Statement::Type(TypeAliasStatement {
            access,
            keyword_type,
            left: name,
            type_variables_declaration,
            right,
        }))
    }

    fn parse_enum_statement(
        &mut self,
        access: Access,
        keyword_class: Token,
    ) -> Result<Statement, ParseError> {
        let context = None;
        let name = self.eat_token(TokenType::Identifier, context)?;
        let type_variables_declaration = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;
        let mut constructors = vec![self.parse_enum_constructor_definition()?];

        let constructors = loop {
            if self.try_eat_token(TokenType::Pipe)?.is_some() {
                constructors.push(self.parse_enum_constructor_definition()?);
            } else {
                break constructors;
            }
        };
        Ok(Statement::Enum(EnumStatement {
            access,
            keyword_enum: keyword_class,
            name,
            type_variables_declaration,
            constructors,
        }))
    }

    fn parse_import_statement(
        &mut self,
        access: Access,
        keyword_import: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementImport);

        let url = self.eat_string_literal()?;
        let right = self.try_parse_import_specification(context)?;

        Ok(Statement::Import(ImportStatement {
            access,
            keyword_import,
            url,
            specification: right,
        }))
    }

    fn try_parse_import_specification(
        &mut self,
        context: Option<ParseContext>,
    ) -> Result<Option<ImportStatementSpecification>, ParseError> {
        if let Some(left_curly_bracket) = self.try_eat_token(TokenType::LeftCurlyBracket)? {
            let mut aliases = vec![];
            loop {
                aliases.push(ImportSpecificationAlias {
                    name: self.eat_token(TokenType::Identifier, context)?,
                    alias: {
                        if self.try_eat_token(TokenType::Equals)?.is_some() {
                            Some(self.eat_token(TokenType::Identifier, context)?)
                        } else {
                            None
                        }
                    },
                });
                if let Some(right_curly_bracket) =
                    self.try_eat_token(TokenType::RightCurlyBracket)?
                {
                    return Ok(Some(ImportStatementSpecification {
                        left_curly_bracket,
                        aliases,
                        right_curly_bracket,
                    }));
                } else {
                    self.eat_token(TokenType::Comma, context)?;
                }
            }
        } else {
            Ok(None)
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

    fn try_parse_type_arguments(&mut self) -> Result<Option<TypeArguments>, ParseError> {
        let context = Some(ParseContext::TypeArguments);
        if let Some(left_angular_bracket) = self.try_eat_token(TokenType::LessThan)? {
            Ok(Some(
                self.parse_type_arguments(left_angular_bracket, context)?,
            ))
        } else {
            Ok(None)
        }
    }

    fn parse_type_arguments(
        &mut self,
        left_angular_bracket: Token,
        context: Option<ParseContext>,
    ) -> Result<TypeArguments, ParseError> {
        let head = self.parse_type_annotation(context)?;
        let mut tail = Vec::new();
        let right_angular_bracket = loop {
            if let Some(right_angular_bracket) = self.try_eat_token(TokenType::MoreThan)? {
                break right_angular_bracket;
            } else if self.try_eat_token(TokenType::Comma)?.is_some() {
                tail.push(self.parse_type_annotation(context)?);
            } else {
                break self.eat_token(TokenType::MoreThan, context)?;
            }
        };
        Ok(TypeArguments {
            type_annotations: Box::new(NonEmpty { head, tail }),
        })
    }

    fn parse_enum_constructor_definition(
        &mut self,
    ) -> Result<EnumConstructorDefinition, ParseError> {
        let context = Some(ParseContext::EnumConstructorDefinition);
        let name = self.eat_token(TokenType::Identifier, context)?;
        Ok(EnumConstructorDefinition {
            name,
            payload: {
                if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
                    Some(Box::new(EnumConstructorDefinitionPayload {
                        type_annotation: self
                            .parse_parenthesized_type_annotation(left_parenthesis)?,
                    }))
                } else if let Some(left_curly_bracket) =
                    self.try_eat_token(TokenType::LeftCurlyBracket)?
                {
                    Some(Box::new(EnumConstructorDefinitionPayload {
                        type_annotation: self.parse_record_type_annotation(left_curly_bracket)?,
                    }))
                } else {
                    None
                }
            },
        })
    }

    fn try_parse_type_variables_declaration(
        &mut self,
    ) -> Result<Option<TypeVariablesDeclaration>, ParseError> {
        if self.try_eat_token(TokenType::LessThan)?.is_some() {
            Ok(Some(self.parse_type_variables_declaration()?))
        } else {
            Ok(None)
        }
    }

    fn parse_type_variables_declaration(&mut self) -> Result<TypeVariablesDeclaration, ParseError> {
        let context = Some(ParseContext::TypeVariablesDeclaration);
        let head = self.eat_token(TokenType::Identifier, context)?;
        let mut tail = vec![];
        loop {
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                tail.push(self.eat_token(TokenType::Identifier, context)?);
            } else {
                self.eat_token(TokenType::MoreThan, context)?;
                break;
            }
        }

        Ok(TypeVariablesDeclaration {
            // left_angular_bracket,
            type_variables: NonEmpty { head, tail },
            // right_angular_bracket,
        })
    }

    fn try_parse_type_annotation(&mut self) -> Result<Option<TypeAnnotation>, ParseError> {
        if self.try_eat_token(TokenType::Colon)?.is_some() {
            Ok(Some(self.parse_type_annotation(None)?))
        } else {
            Ok(None)
        }
    }

    fn parse_type_annotation(
        &mut self,
        context: Option<ParseContext>,
    ) -> Result<TypeAnnotation, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            let potential_function_parameter_type = match token.token_type {
                TokenType::Identifier => {
                    let name = token;
                    Ok(TypeAnnotation::Named {
                        name,
                        type_arguments: self.try_parse_type_arguments()?,
                    })
                }
                TokenType::HashLeftSquareBracket => {
                    let context = Some(ParseContext::TypeAnnotationArray);
                    let hash_left_square_bracket = token;
                    let element_type = self.parse_type_annotation(context)?;
                    let right_square_bracket =
                        self.eat_token(TokenType::RightSquareBracket, context)?;
                    Ok(TypeAnnotation::Array {
                        hash_left_square_bracket,
                        element_type: Box::new(element_type),
                        right_square_bracket,
                    })
                }
                TokenType::LessThan => Ok(TypeAnnotation::Scheme {
                    type_variables: self.parse_type_variables_declaration()?,
                    type_annotation: Box::new(self.parse_type_annotation(None)?),
                }),
                TokenType::LeftCurlyBracket => {
                    let left_curly_bracket = token;
                    self.parse_record_type_annotation(left_curly_bracket)
                }
                TokenType::LeftParenthesis => {
                    let left_parenthesis = token;
                    if let Some(right_parenthesis) =
                        self.try_eat_token(TokenType::RightParenthesis)?
                    {
                        Ok(TypeAnnotation::Unit {
                            left_parenthesis,
                            right_parenthesis,
                        })
                    } else {
                        self.parse_parenthesized_type_annotation(left_parenthesis)
                    }
                }
                _ => Err(Parser::invalid_token(token, context)),
            }?;
            self.try_parse_function_type_annotation(potential_function_parameter_type)
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn parse_record_type_annotation(
        &mut self,
        left_curly_bracket: Token,
    ) -> Result<TypeAnnotation, ParseError> {
        let context = Some(ParseContext::TypeAnnotationRecord);
        let mut key_type_annotation_pairs: Vec<(Token, TypeAnnotation)> = vec![];
        let right_curly_bracket = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break right_curly_bracket;
            }
            let key = self.eat_token(TokenType::Identifier, context)?;
            self.eat_token(TokenType::Colon, context)?;
            let type_annotation = self.parse_type_annotation(context)?;
            key_type_annotation_pairs.push((key, type_annotation));

            if self.try_eat_token(TokenType::Comma)?.is_none() {
                break self.eat_token(TokenType::RightCurlyBracket, context)?;
            };
        };
        Ok(TypeAnnotation::Record {
            left_curly_bracket,
            key_type_annotation_pairs,
            right_curly_bracket,
        })
    }

    fn try_parse_function_type_annotation(
        &mut self,
        potential_function_parameter_type: TypeAnnotation,
    ) -> Result<TypeAnnotation, ParseError> {
        if self.try_eat_token(TokenType::ArrowRight)?.is_some() {
            Ok(TypeAnnotation::Function(FunctionTypeAnnotation {
                parameter: Box::new(potential_function_parameter_type),
                return_type: Box::new(self.parse_type_annotation(None)?),
                type_constraints: self.try_parse_type_constraints()?,
            }))
        } else {
            Ok(potential_function_parameter_type)
        }
    }

    pub fn parse_low_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let expression = self.parse_mid_precedence_expression()?;
        self.try_parse_statement_expression(expression)
    }

    fn try_parse_statement_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Expression, ParseError> {
        if self.try_eat_token(TokenType::Semicolon)?.is_some() {
            let next = self.parse_mid_precedence_expression()?;
            self.try_parse_statement_expression(Expression::Statements {
                current: Box::new(expression),
                next: Box::new(next),
            })
        } else {
            Ok(expression)
        }
    }

    pub fn parse_mid_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.try_eat_token(TokenType::Underscore)? {
            Ok(Expression::Identifier(token))
        } else if let Some(operator) = self.try_eat_token(TokenType::Operator)? {
            // Operator shorthand
            // e.g. `+ x` means `-> temp [ temp + x ]`
            let argument = self.parse_high_precedence_expression()?;
            Ok(Expression::Function(Box::new(Function {
                branches: NonEmpty {
                    head: {
                        let phantom_parameter = Token::dummy_identifier(format!("temp{}", todo!()));
                        FunctionBranch {
                            parameter: Box::new(DestructurePattern::Identifier(
                                phantom_parameter.clone(),
                            )),
                            body: Box::new(Expression::FunctionCall(Box::new(FunctionCall {
                                function: Box::new(Expression::FunctionCall(Box::new(
                                    FunctionCall {
                                        function: Box::new(Expression::Identifier(operator)),
                                        argument: Box::new(Expression::Identifier(
                                            phantom_parameter,
                                        )),
                                        type_arguments: None,
                                    },
                                ))),
                                argument: Box::new(argument),
                                type_arguments: None,
                            }))),
                        }
                    },
                    tail: vec![],
                },
            })))
        } else if let Some(keyword_let) = self.try_eat_token(TokenType::KeywordLet)? {
            Ok(Expression::Let {
                keyword_let,
                left: self.parse_destructure_pattern()?,
                type_annotation: self.try_parse_type_annotation()?,
                right: {
                    self.eat_token(TokenType::Equals, None)?;
                    Box::new(self.parse_mid_precedence_expression()?)
                },
                body: {
                    self.eat_token(TokenType::Semicolon, None)?;
                    Box::new(self.parse_low_precedence_expression()?)
                },
            })
        } else {
            let expression = self.parse_high_precedence_expression()?;
            self.try_parse_function_call(expression)
        }
    }

    fn parse_high_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Expression);
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String(string_literal) => Ok(Expression::String(string_literal)),
                TokenType::InterpolatedString(interpolated_string) => {
                    interpolated_string.to_expression(todo!())
                }
                TokenType::Character => Ok(Expression::Character(token.clone())),
                TokenType::HashLeftSquareBracket => self.parse_array(token.clone()),
                TokenType::LeftSquareBracket => Ok(Expression::Function(Box::new(Function {
                    branches: NonEmpty {
                        head: self.parse_default_function_branch(token)?,
                        tail: vec![],
                    },
                }))),
                TokenType::LeftCurlyBracket => self.parse_record(token.clone()),
                TokenType::ArrowRight => self.parse_function(),
                TokenType::LeftParenthesis => {
                    let left_parenthesis = token;
                    self.parse_parenthesized_or_unit_or_tuple(left_parenthesis)
                }
                TokenType::Float => Ok(Expression::Float(token.clone())),
                TokenType::Integer => Ok(Expression::Integer(token.clone())),
                TokenType::Identifier => Ok(Expression::Identifier(token.clone())),
                TokenType::Tilde => {
                    let bind_function =
                        Expression::Identifier(self.eat_token(TokenType::Identifier, None)?);
                    let expression = Box::new(self.parse_high_precedence_expression()?);

                    // Collect CpsBangs
                    let (bangs, expression) = expression
                        .clone()
                        .collect_cps_bangs(todo!(), &bind_function);

                    let expression = bangs.into_iter().fold(expression, |expression, bang| {
                        Expression::FunctionCall(Box::new(FunctionCall {
                            function: Box::new(Expression::FunctionCall(Box::new(FunctionCall {
                                function: Box::new(bang.function),
                                argument: Box::new(bang.argument),
                                type_arguments: None,
                            }))),
                            argument: Box::new(Expression::Function(Box::new(Function {
                                branches: NonEmpty {
                                    head: FunctionBranch {
                                        parameter: Box::new(bang.temporary_variable),
                                        body: Box::new(expression),
                                    },
                                    tail: vec![],
                                },
                            }))),
                            type_arguments: None,
                        }))
                    });
                    Ok(Expression::TildeClosure(TildeClosure {
                        tilde: token,
                        bind_function: Box::new(bind_function),
                        expression: Box::new(expression),
                    }))
                }
                TokenType::KeywordInnate => {
                    let context = None;
                    let function_name = self.eat_token(TokenType::Identifier, context)?;
                    Ok(Expression::IntrinsicFunctionCall(IntrinsicFunctionCall {
                        function_name,
                        argument: Box::new(self.parse_high_precedence_expression()?),
                    }))
                }
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn parse_array(&mut self, hash_left_square_bracket: Token) -> Result<Expression, ParseError> {
        let mut elements: Vec<Expression> = Vec::new();
        let right_square_bracket = loop {
            if let Some(right_square_bracket) = self.try_eat_token(TokenType::RightSquareBracket)? {
                break right_square_bracket;
            };
            if !elements.is_empty() {
                self.eat_token(TokenType::Comma, None)?;
            }
            elements.push(self.parse_low_precedence_expression()?);
        };
        Ok(Expression::Array {
            hash_left_square_bracket,
            elements,
            right_square_bracket,
        })
    }

    fn parse_record(&mut self, left_curly_bracket: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecord);
        let wildcard = self.try_eat_token(TokenType::DoublePeriod)?; // TODO: parse wildcard
        let mut elements: Vec<RecordKeyValue> = vec![];
        let right_curly_bracket = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break right_curly_bracket;
            }

            elements.push({
                let key =
                    self.eat_token(TokenType::Identifier, Some(ParseContext::ExpressionRecord))?;
                let value = if self.try_eat_token(TokenType::Equals)?.is_some() {
                    self.parse_low_precedence_expression()?
                } else {
                    Expression::Identifier(key.clone())
                };

                RecordKeyValue {
                    key: key.clone(),
                    value,
                }
            });

            if self.try_eat_token(TokenType::Comma)?.is_none() {
                break self.eat_token(TokenType::RightCurlyBracket, context)?;
            }
        };
        Ok(Expression::Record {
            wildcard,
            key_value_pairs: elements,
            left_curly_bracket,
            right_curly_bracket,
        })
    }

    fn parse_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        let or_pattern = self.parse_or_destructure_pattern()?;

        if or_pattern.tail.is_empty() {
            Ok(or_pattern.head)
        } else {
            Ok(DestructurePattern::Or(or_pattern))
        }
    }

    fn parse_or_destructure_pattern(&mut self) -> Result<OrDestructurePattern, ParseError> {
        let simple_pattern = self.parse_simple_destructure_pattern()?;
        let mut tail_patterns = vec![];
        let tail_patterns = loop {
            if self.try_eat_token(TokenType::Pipe)?.is_some() {
                tail_patterns.push(self.parse_simple_destructure_pattern()?)
            } else {
                break tail_patterns;
            }
        };

        Ok(Box::new(NonEmpty {
            head: simple_pattern,
            tail: tail_patterns,
        }))
    }

    fn parse_record_pattern(
        &mut self,
        left_curly_bracket: Token,
    ) -> Result<DestructurePattern, ParseError> {
        let context = Some(ParseContext::PatternRecord);
        let mut wildcard = None;
        let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = vec![];
        let right_curly_bracket = loop {
            match self.next_meaningful_token()? {
                Some(
                    right_curly_bracket @ Token {
                        token_type: TokenType::RightCurlyBracket,
                        ..
                    },
                ) => {
                    break right_curly_bracket;
                }
                Some(
                    double_period @ Token {
                        token_type: TokenType::DoublePeriod,
                        ..
                    },
                ) => wildcard = Some(double_period),
                other => {
                    let key = Parser::validate_token(TokenType::Identifier, other, context)?;
                    let type_annotation = self.try_parse_type_annotation()?;
                    let as_value = if self.try_eat_token(TokenType::Equals)?.is_some() {
                        Some(self.parse_destructure_pattern()?)
                    } else {
                        None
                    };
                    key_value_pairs.push(DestructuredRecordKeyValue {
                        key,
                        type_annotation,
                        as_value,
                    });
                    if self.try_eat_token(TokenType::Comma)?.is_none() {
                        break self.eat_token(TokenType::RightCurlyBracket, context)?;
                    }
                }
            }
        };
        Ok(DestructurePattern::Record {
            wildcard,
            left_curly_bracket,
            key_value_pairs,
            right_curly_bracket,
        })
    }

    /// Simple destructure pattern means all kinds of destructure pattern except for OR patterns
    /// OR patterns means `P1 | P2 | P3`
    fn parse_simple_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::Identifier => {
                    if let Some(left_parenthesis) =
                        self.try_eat_token(TokenType::LeftParenthesis)?
                    {
                        Ok(DestructurePattern::EnumConstructor {
                            name: token,
                            payload: Some(Box::new(
                                self.parse_parenthesized_pattern(left_parenthesis)?,
                            )),
                        })
                    } else if let Some(left_curly_bracket) =
                        self.try_eat_token(TokenType::LeftCurlyBracket)?
                    {
                        Ok(DestructurePattern::EnumConstructor {
                            name: token,
                            payload: Some(Box::new(self.parse_record_pattern(left_curly_bracket)?)),
                        })
                    } else {
                        Ok(DestructurePattern::Identifier(token))
                    }
                }
                TokenType::Underscore => Ok(DestructurePattern::Underscore(token)),
                TokenType::LeftParenthesis => self.parse_parenthesized_pattern(token),
                TokenType::LeftCurlyBracket => self.parse_record_pattern(token),
                TokenType::Integer => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::Integer,
                }),
                TokenType::String { .. } => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::String,
                }),
                TokenType::Character => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::Character,
                }),
                TokenType::LeftSquareBracket => {
                    let context = Some(ParseContext::PatternArray);
                    let left_square_bracket = token;
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
                        let spread_token = self.eat_token(TokenType::DoublePeriod, context)?;
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
                _ => Err(Parser::invalid_token(token, Some(ParseContext::Pattern))),
            }
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Pattern)))
        }
    }

    fn parse_function_tail_branches(
        &mut self,
        head: FunctionBranch,
    ) -> Result<NonEmpty<FunctionBranch>, ParseError> {
        let mut tail = vec![];
        let tail = loop {
            if self.try_eat_token(TokenType::ArrowRight)?.is_some() {
                tail.push(self.parse_function_branch()?)
            } else if let Some(left_square_bracket) =
                self.try_eat_token(TokenType::LeftSquareBracket)?
            {
                tail.push(self.parse_default_function_branch(left_square_bracket)?);

                // Break the loop, as any following branches are unreachable
                break tail;
            } else {
                break tail;
            }
        };
        Ok(NonEmpty { head, tail })
    }

    fn parse_function(&mut self) -> Result<Expression, ParseError> {
        let head = self.parse_function_branch()?;
        let branches = self.parse_function_tail_branches(head)?;
        Ok(Expression::Function(Box::new(Function { branches })))
    }

    fn parse_function_branch(&mut self) -> Result<FunctionBranch, ParseError> {
        let context = Some(ParseContext::ExpressionFunction);
        let parameter = self.parse_destructure_pattern()?;
        self.eat_token(TokenType::LeftSquareBracket, context)?;
        let body = self.parse_low_precedence_expression()?;
        self.eat_token(TokenType::RightSquareBracket, context)?;
        Ok(FunctionBranch {
            parameter: Box::new(parameter),
            body: Box::new(body),
        })
    }

    fn try_parse_function_call(&mut self, previous: Expression) -> Result<Expression, ParseError> {
        if self.next_token_is_terminating()? {
            return Ok(previous);
        }

        if let Some(bang) = self.try_eat_token(TokenType::Bang)? {
            return self.try_parse_function_call(Expression::CpsBang {
                argument: Box::new(previous),
                bang,
            });
        }

        let (function, argument) = {
            if self.try_eat_token(TokenType::Period)?.is_some() {
                let next = self.parse_high_precedence_expression()?;
                // This is a postfix function application
                (next, previous)
            } else if let Some(token) = self.try_eat_token(TokenType::Operator)? {
                // This is a postfix function application
                (Expression::Identifier(token), previous)
            } else {
                let next = self.parse_high_precedence_expression()?;
                // This is a prefix function application
                // And if the argument is an identifier without parenthesis
                // The identifier will be treated as a keyword
                match next {
                    Expression::Identifier(identifier) => {
                        (previous, Expression::Keyword(identifier))
                    }
                    _ => (previous, next),
                }
            }
        };

        if let Expression::Record {
            left_curly_bracket,
            key_value_pairs,
            right_curly_bracket,
            ..
        } = function
        {
            // This is a record update
            return self.try_parse_function_call(Expression::RecordUpdate {
                expression: Box::new(argument),
                left_curly_bracket,
                updates: key_value_pairs
                    .into_iter()
                    .map(|key_value_pair| RecordUpdate::ValueUpdate {
                        property_name: key_value_pair.key,
                        new_value: key_value_pair.value,
                    })
                    .collect(),
                right_curly_bracket,
            });
        }

        let previous = match (function, argument) {
            // If function is EnumConstructor, transform the function call into enum constructor with payload
            (
                Expression::EnumConstructor {
                    name,
                    payload: None,
                },
                argument,
            ) => Expression::EnumConstructor {
                name,
                payload: Some(Box::new(argument)),
            },

            // otherwise
            (function, argument) => Expression::FunctionCall(Box::new(FunctionCall {
                function: Box::new(function),
                argument: Box::new(argument),
                type_arguments: None,
            })),
        };
        self.try_parse_function_call(previous)
    }

    fn next_token_is_terminating(&mut self) -> Result<bool, ParseError> {
        match self.peek_next_meaningful_token()? {
            None => Ok(true),
            Some(token) => Ok(matches!(
                token.token_type,
                TokenType::Comma
                    | TokenType::Equals
                    | TokenType::KeywordLet
                    | TokenType::RightParenthesis
                    | TokenType::RightCurlyBracket
                    | TokenType::RightSquareBracket
                    | TokenType::KeywordType
                    | TokenType::KeywordPublic
                    | TokenType::KeywordExport
                    | TokenType::KeywordEntry
                    | TokenType::Semicolon
                    | TokenType::Colon
                    | TokenType::Pipe
            )),
        }
    }

    fn parse_parenthesized_type_annotation(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<TypeAnnotation, ParseError> {
        let context = None;
        Ok(TypeAnnotation::Parenthesized {
            left_parenthesis,
            type_annotation: Box::new(self.parse_type_annotation(context)?),
            right_parenthesis: self.eat_token(TokenType::RightParenthesis, context)?,
        })
    }

    fn try_parse_type_constraints(
        &mut self,
    ) -> Result<Option<TypeConstraintsAnnotation>, ParseError> {
        let context = None;
        if self.try_eat_token(TokenType::KeywordGiven)?.is_some() {
            Ok(Some(TypeConstraintsAnnotation {
                left_curly_bracket: self.eat_token(TokenType::LeftCurlyBracket, context)?,
                type_constraints: {
                    let mut type_constraints = vec![];
                    loop {
                        if let Some(identifier) = self.try_eat_token(TokenType::Identifier)? {
                            type_constraints.push(TypeConstraintAnnotation {
                                identifier,
                                type_annotation: {
                                    self.eat_token(TokenType::Colon, context)?;
                                    self.parse_type_annotation(context)?
                                },
                            })
                        } else {
                            break type_constraints;
                        }
                    }
                },
                right_curly_bracket: self.eat_token(TokenType::RightCurlyBracket, context)?,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_parenthesized_or_unit_or_tuple(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<Expression, ParseError> {
        if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
            return Ok(Expression::Unit {
                left_parenthesis,
                right_parenthesis,
            });
        }

        let head = self.parse_low_precedence_expression()?;

        if !self.try_eat_token(TokenType::Comma)?.is_some() {
            Ok(Expression::Parenthesized {
                left_parenthesis,
                value: Box::new(head),
                right_parenthesis: self.eat_token(TokenType::RightParenthesis, None)?,
            })
        } else {
            // This is a tuple
            let mut tail = vec![];
            let (tail, right_parenthesis) = loop {
                if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                    break (tail, right_parenthesis);
                } else {
                    tail.push(self.parse_low_precedence_expression()?);
                    if self.try_eat_token(TokenType::Comma)?.is_none() {
                        let right_parenthesis =
                            self.eat_token(TokenType::RightParenthesis, None)?;
                        break (tail, right_parenthesis);
                    }
                }
            };

            Ok(Expression::Tuple {
                left_parenthesis,
                elements: Box::new(NonEmpty { head, tail }),
                right_parenthesis,
            })
        }
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

    fn parse_parenthesized_pattern(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<DestructurePattern, ParseError> {
        let context = None;
        if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
            return Ok(DestructurePattern::Unit {
                left_parenthesis,
                right_parenthesis,
            });
        }

        let head = self.parse_destructure_pattern()?;
        if self.try_eat_token(TokenType::Comma)?.is_some() {
            let mut tail = vec![];
            let (right_parenthesis, tail) = loop {
                tail.push(self.parse_destructure_pattern()?);

                if self.try_eat_token(TokenType::Comma)?.is_none() {
                    break (self.eat_token(TokenType::RightParenthesis, context)?, tail);
                }
            };
            Ok(DestructurePattern::Tuple(DestructurePatternTuple {
                parentheses: Some((left_parenthesis, right_parenthesis)),
                values: Box::new(NonEmpty { head, tail }),
            }))
        } else {
            Ok(DestructurePattern::Parenthesized {
                left_parenthesis,
                pattern: Box::new(head),
                type_annotation: self.try_parse_type_annotation()?,
                right_parenthesis: self.eat_token(TokenType::RightParenthesis, context)?,
            })
        }
    }

    /// This is a sugar for default case,
    /// where
    ///
    ///   [ expr ]
    ///
    /// means
    ///
    ///   -> _ [ expr ]
    ///
    fn parse_default_function_branch(
        &mut self,
        left_square_bracket: Token,
    ) -> Result<FunctionBranch, ParseError> {
        let expression = self.parse_low_precedence_expression()?;
        self.eat_token(TokenType::RightSquareBracket, None)?;
        Ok(FunctionBranch {
            parameter: Box::new(DestructurePattern::Underscore(left_square_bracket)),
            body: Box::new(expression),
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

impl StringLiteral {
    pub fn extract_codes(&self) -> Result<Vec<Expression>, ParseError> {
        let mut parsing_code_block = false;

        pulldown_cmark::Parser::new(self.content.as_str())
            .into_offset_iter()
            .filter_map(|(event, range)| match event {
                pulldown_cmark::Event::Text(text) => {
                    if parsing_code_block {
                        Some((text.to_string(), range))
                    } else {
                        None
                    }
                }
                pulldown_cmark::Event::Code(code) => Some((code.to_string(), range)),
                pulldown_cmark::Event::Start(pulldown_cmark::Tag::CodeBlock(_)) => {
                    parsing_code_block = true;
                    None
                }
                pulldown_cmark::Event::End(pulldown_cmark::Tag::CodeBlock(_)) => {
                    parsing_code_block = false;
                    None
                }
                _ => None,
            })
            .map(|(raw_code, range)| {
                Parser::new(&mut Tokenizer::with_offset(
                    raw_code,
                    self.start_quotes.last().index + range.start + 1,
                ))
                .parse_low_precedence_expression()
            })
            .collect::<Result<Vec<_>, _>>()
    }
}

struct ParserState {
    temporary_variable_index: usize,
}

impl ParserState {
    fn get_next_temporary_variable_index(&mut self) -> usize {
        let result = self.temporary_variable_index;
        self.temporary_variable_index += 1;
        result
    }
}

impl Expression {
    /// Collect CPS bangs, and transformed those bangs into temporary variables
    fn collect_cps_bangs(
        self,
        state: &mut ParserState,
        bind_function: &Expression,
    ) -> (Vec<CollectCpsBangResult>, Expression) {
        match self {
            // The main logic is here
            Expression::CpsBang { argument, bang } => {
                let temporary_variable = Token {
                    position: argument.position().join(bang.position),
                    token_type: TokenType::Identifier,
                    representation: format!("temp{}", state.get_next_temporary_variable_index()),
                };
                (
                    vec![CollectCpsBangResult {
                        temporary_variable: DestructurePattern::Identifier(
                            temporary_variable.clone(),
                        ),
                        function: bind_function.clone(),
                        argument: argument.as_ref().clone(),
                    }],
                    Expression::Identifier(temporary_variable),
                )
            }

            Expression::TildeClosure(tilde_closure) => {
                // Do nothing, since it is already desugared
                (vec![], Expression::TildeClosure(tilde_closure))
            }

            // Whatever down here is boilerplate code
            // Which can be solved using the Scrap Your Boilerplate technique
            // Reference: https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf
            Expression::Unit { .. }
            | Expression::Float(_)
            | Expression::Integer(_)
            | Expression::String(_)
            | Expression::Character(_)
            | Expression::Identifier(_)
            | Expression::Keyword(_) => (vec![], self),

            Expression::Statements { current, next } => {
                let (bangs1, current) = current.collect_cps_bangs(state, bind_function);
                let (bangs2, next) = next.collect_cps_bangs(state, bind_function);
                (
                    bangs1.into_iter().chain(bangs2.into_iter()).collect(),
                    Expression::Statements {
                        current: Box::new(current),
                        next: Box::new(next),
                    },
                )
            }
            Expression::Parenthesized {
                left_parenthesis,
                right_parenthesis,
                value,
            } => {
                let (bangs, value) = value.collect_cps_bangs(state, bind_function);
                (
                    bangs,
                    Expression::Parenthesized {
                        left_parenthesis,
                        right_parenthesis,
                        value: Box::new(value),
                    },
                )
            }
            Expression::InterpolatedString {
                start_quotes: start_quote,
                sections,
                end_quotes: end_quote,
            } => {
                let NonEmpty { head, tail } = sections.map(|section| match section {
                    InterpolatedStringSection::String(string) => {
                        (vec![], InterpolatedStringSection::String(string))
                    }
                    InterpolatedStringSection::Expression(expression) => {
                        let (bangs, expression) =
                            expression.collect_cps_bangs(state, bind_function);
                        (
                            bangs,
                            InterpolatedStringSection::Expression(Box::new(expression)),
                        )
                    }
                });
                let (bangs, sections): (
                    Vec<Vec<CollectCpsBangResult>>,
                    Vec<InterpolatedStringSection>,
                ) = tail.into_iter().unzip();
                let bangs = head
                    .0
                    .into_iter()
                    .chain(bangs.into_iter().flatten())
                    .collect();
                (
                    bangs,
                    Expression::InterpolatedString {
                        start_quotes: start_quote,
                        sections: NonEmpty {
                            head: head.1,
                            tail: sections,
                        },
                        end_quotes: end_quote,
                    },
                )
            }
            Expression::EnumConstructor { name, payload } => match payload {
                Some(payload) => {
                    let (bangs, payload) = payload.collect_cps_bangs(state, bind_function);
                    (
                        bangs,
                        Expression::EnumConstructor {
                            name,
                            payload: Some(Box::new(payload)),
                        },
                    )
                }
                None => (
                    vec![],
                    Expression::EnumConstructor {
                        name,
                        payload: None,
                    },
                ),
            },
            Expression::Function(function) => {
                // Don't collect bang for function, because bang should not escape the body of a
                // function
                //
                // This is because the body of a function might be evaluated more than once,
                // for example, the function passed to Array::map
                (vec![], Expression::Function(function))
            }
            Expression::FunctionCall(function_call) => {
                let (bangs1, function) = function_call
                    .function
                    .collect_cps_bangs(state, bind_function);
                let (bangs2, argument) = function_call
                    .argument
                    .collect_cps_bangs(state, bind_function);
                (
                    bangs1.into_iter().chain(bangs2.into_iter()).collect(),
                    Expression::FunctionCall(Box::new(FunctionCall {
                        function: Box::new(function),
                        argument: Box::new(argument),
                        type_arguments: function_call.type_arguments,
                    })),
                )
            }
            Expression::Record {
                wildcard,
                left_curly_bracket: left_square_bracket,
                key_value_pairs,
                right_curly_bracket: right_square_bracket,
            } => {
                let (bangs, key_value_pairs): (
                    Vec<Vec<CollectCpsBangResult>>,
                    Vec<RecordKeyValue>,
                ) = key_value_pairs
                    .into_iter()
                    .map(|key_value_pair| {
                        let (bangs, value) =
                            key_value_pair.value.collect_cps_bangs(state, bind_function);
                        (
                            bangs,
                            RecordKeyValue {
                                key: key_value_pair.key,
                                value,
                            },
                        )
                    })
                    .unzip();
                (
                    bangs.into_iter().flatten().collect(),
                    Expression::Record {
                        wildcard,
                        left_curly_bracket: left_square_bracket,
                        key_value_pairs,
                        right_curly_bracket: right_square_bracket,
                    },
                )
            }
            Expression::RecordAccess {
                expression,
                property_name,
            } => {
                let (bangs, expression) = expression.collect_cps_bangs(state, bind_function);
                (
                    bangs,
                    Expression::RecordAccess {
                        expression: Box::new(expression),
                        property_name,
                    },
                )
            }
            x => todo!("{:#?}", x),
        }
    }
}

struct CollectCpsBangResult {
    temporary_variable: DestructurePattern,
    function: Expression,
    argument: Expression,
}

impl simple_ast::TopLevelArray {
    pub fn into_statements(self) -> Result<Vec<Statement>, ParseError> {
        let mut state = ParserState {
            temporary_variable_index: 0,
        };
        self.expand_macro()
            .into_iter()
            .map(|node| {
                // println!("{}", node.to_pretty());
                let result = node.into_statement(&mut state)?;
                // println!("{:#?}", result);
                Ok(result)
            })
            .collect::<Result<Vec<_>, _>>()
    }

    fn expand_macro(self) -> Vec<MacroExpandedNode> {
        self.nodes
            .into_vector()
            .into_iter()
            .filter_map(|node| node.expand_macro())
            .collect()
    }
}

impl simple_ast::Literal {
    fn expand_macro(self) -> Self {
        use simple_ast::*;
        match self {
            Literal::InterpolatedString(interpolated_string) => todo!(),
            _ => self,
        }
    }
}

impl MacroExpandedList {
    fn skip(&self, count: usize) -> Result<Self, ParseError> {
        let nodes = self
            .nodes
            .to_vector()
            .into_iter()
            .skip(count)
            .map(|node| node.clone())
            .collect::<Vec<_>>();

        let nodes = match nodes.split_first() {
            Some((head, tail)) => Ok(NonEmpty {
                head: head.clone(),
                tail: tail.to_vec(),
            }),
            None => Err(ParseError {
                context: None,
                kind: ParseErrorKind::UnexpectedNode {
                    position: self.nodes.first().position(),
                },
            }),
        }?;
        Ok(Self {
            nodes: Box::new(nodes),
        })
    }
    fn to_statement(
        &self,
        state: &mut ParserState,
        access: Access,
    ) -> Result<Statement, ParseError> {
        use simple_ast::*;
        let first = self.nodes.first().to_identifier(None)?;
        match first.representation.as_str() {
            "enum" => {
                let name = self.get(1)?.to_identifier(None)?;
                let constructors = self
                    .get(2)?
                    .into_list()?
                    .into_enum_constructor_definitions(state)?;
                self.nodes.get(3).should_be_none()?;
                Ok(Statement::Enum(EnumStatement {
                    access: Access::Protected,
                    keyword_enum: first,
                    name,
                    type_variables_declaration: None,
                    constructors,
                }))
            }
            "defn" => {
                let name = self.get(1)?.to_identifier(None)?;
                let signature = self.get(2)?.into_list()?.to_function_signature()?;
                let body = self.get(3)?.to_expression(state)?;
                self.nodes.get(4).should_be_none()?;
                Ok(Statement::Let(LetStatement {
                    access,
                    keyword_let: first,
                    name,
                    doc_string: None,
                    type_annotation: signature.to_function_type_annotation(),
                    expression: Expression::Function(Box::new(signature.to_function(body))),
                }))
            }
            "def" => {
                let name = self.get(1)?.to_identifier(None)?;
                let type_annotation = self.get(2)?.to_type_annotation()?;
                let expression = self.get(3)?.to_expression(state)?;
                Ok(Statement::Let(LetStatement {
                    access,
                    keyword_let: first,
                    name,
                    doc_string: None,
                    type_annotation,
                    expression,
                }))
            }
            "export" => self.skip(1)?.to_statement(
                state,
                Access::Exported {
                    keyword_export: first,
                },
            ),
            _ => Ok(Statement::Entry(EntryStatement {
                keyword_entry: Token::dummy(),
                expression: self.to_expression(state)?,
            })),
        }
    }

    fn get(&self, index: usize) -> Result<MacroExpandedNode, ParseError> {
        let position = if index == 0 {
            self.nodes.first().position()
        } else {
            self.get(index - 1)?.position()
        };
        self.nodes.get(index).into_node(position)
    }
}

impl simple_ast::Node {
    fn expand_macro(&self) -> Option<MacroExpandedNode> {
        use simple_ast::*;
        match self {
            Node::List(list) => Some(list.expand_macro()),
            Node::Literal(literal) => {
                Some(MacroExpandedNode::Literal(literal.clone().expand_macro()))
            }
            Node::Comment(_) => None,
        }
    }
}

impl MacroExpandedNode {
    fn into_statement(&self, state: &mut ParserState) -> Result<Statement, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(list) => list.to_statement(state, Access::Protected),
            _ => Ok(Statement::Entry(EntryStatement {
                keyword_entry: Token::dummy(),
                expression: {
                    let expression = self.to_expression(state)?;
                    expression
                },
            })),
        }
    }

    fn to_record_key_value(&self, state: &mut ParserState) -> Result<RecordKeyValue, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::Literal(Literal::Identifier(token)) => Ok(RecordKeyValue {
                key: token.clone(),
                value: Expression::Identifier(token.clone()),
            }),
            MacroExpandedNode::List(list) => {
                let key = list.get(0)?.to_identifier(None)?;
                list.get(1)?.to_identifier(Some("="))?;
                let value = list.get(2)?.to_expression(state)?;
                list.nodes.get(3).should_be_none()?;
                Ok(RecordKeyValue { key, value })
            }
            _ => Err(ParseError {
                context: None,
                kind: ParseErrorKind::UnexpectedNode {
                    position: self.position(),
                },
            }),
        }
    }

    fn to_function_branches(
        self,
        state: &mut ParserState,
    ) -> Result<NonEmpty<FunctionBranch>, ParseError> {
        let position = self.position();
        let list = self.into_list()?;
        Ok(NonEmpty {
            head: list.nodes.head.to_function_branch(state)?,
            tail: list
                .nodes
                .tail()
                .into_iter()
                .map(|node| node.to_function_branch(state))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn to_function_branch(&self, state: &mut ParserState) -> Result<FunctionBranch, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(MacroExpandedList { nodes }) => {
                let first = nodes.head.to_identifier(Some("case"))?;
                let pattern = nodes.tail.get(0).into_node(first.position)?.to_pattern()?;
                let third = nodes
                    .tail
                    .get(1)
                    .into_node(pattern.position())?
                    .to_identifier(Some("->"))?;
                let expression = nodes
                    .tail
                    .get(2)
                    .into_node(third.position)?
                    .to_expression(state)?;
                nodes.tail.get(3).should_be_none()?;
                Ok(FunctionBranch {
                    parameter: Box::new(pattern),
                    body: Box::new(expression),
                })
            }
            _ => {
                // default branch
                Ok(FunctionBranch {
                    parameter: Box::new(DestructurePattern::Underscore(Token {
                        token_type: TokenType::Underscore,
                        representation: "_".to_string(),
                        position: self.position(),
                    })),
                    body: Box::new(self.to_expression(state)?),
                })
            }
        }
    }

    fn to_enum_constructor_definition(&self) -> Result<EnumConstructorDefinition, ParseError> {
        match &self {
            simple_ast::MacroExpandedNode::List(_) => todo!(),
            simple_ast::MacroExpandedNode::Literal(literal) => match literal {
                simple_ast::Literal::Identifier(token) => Ok(EnumConstructorDefinition {
                    name: token.clone(),
                    payload: None,
                }),
                _ => Err(ParseError {
                    context: None,
                    kind: ParseErrorKind::UnexpectedNode {
                        position: self.position(),
                    },
                }),
            },
            _ => todo!(),
        }
    }

    fn into_list(self) -> Result<MacroExpandedList, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(list) => Ok(list),
            _ => Err(ParseError {
                context: None,
                kind: ParseErrorKind::UnexpectedNode {
                    position: self.position(),
                },
            }),
        }
    }

    fn to_identifier<'a>(&self, name: Option<&'a str>) -> Result<Token, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::Literal(Literal::Identifier(token) | Literal::Keyword(token)) => {
                if match name {
                    Some(name) => token.representation.eq(name),
                    None => true,
                } {
                    Ok(token.clone())
                } else {
                    Err(ParseError {
                        context: None,
                        kind: ParseErrorKind::UnexpectedNode {
                            position: self.position(),
                        },
                    })
                }
            }
            _ => Err(ParseError {
                context: Some(ParseContext::NodeIdentifier),
                kind: ParseErrorKind::UnexpectedNode {
                    position: self.position(),
                },
            }),
        }
    }

    fn into_array(self) -> Result<MacroExpandedList, ParseError> {
        match self {
            MacroExpandedNode::List(array) => Ok(array),
            _ => Err(ParseError {
                context: Some(ParseContext::NodeArray),
                kind: ParseErrorKind::UnexpectedNode {
                    position: self.position(),
                },
            }),
        }
    }

    fn to_pattern(&self) -> Result<DestructurePattern, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(array) => array.to_pattern(),
            MacroExpandedNode::Literal(literal) => match literal {
                Literal::Identifier(token) | Literal::Keyword(token) => {
                    Ok(DestructurePattern::Identifier(token.clone()))
                }
                other => todo!("{}", other.to_pretty()),
            },
            MacroExpandedNode::Unit { bracket } => Ok(DestructurePattern::Unit {
                left_parenthesis: bracket.opening.clone(),
                right_parenthesis: bracket.closing.clone(),
            }),
        }
    }

    fn to_expression(&self, state: &mut ParserState) -> Result<Expression, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(array) => array.to_expression(state),
            MacroExpandedNode::Literal(literal) => match literal {
                Literal::String(string_literal) => Ok(Expression::String(string_literal.clone())),
                Literal::Integer(token) => Ok(Expression::Integer(token.clone())),
                Literal::Float(token) => Ok(Expression::Float(token.clone())),
                Literal::Character(token) => Ok(Expression::Character(token.clone())),
                Literal::Identifier(token) => Ok(Expression::Identifier(token.clone())),
                Literal::Keyword(token) => Ok(Expression::Keyword(token.clone())),
                Literal::InterpolatedString(interpolated_string) => {
                    interpolated_string.to_expression(state)
                }
                _ => panic!(),
            },
            MacroExpandedNode::Unit { bracket } => Ok(Expression::Unit {
                left_parenthesis: bracket.opening.clone(),
                right_parenthesis: bracket.closing.clone(),
            }),
        }
    }

    fn into_type_variables_declaration(self) -> Result<TypeVariablesDeclaration, ParseError> {
        self.into_list()?.to_type_variables_declaration()
    }

    fn to_type_annotation(&self) -> Result<TypeAnnotation, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::Unit { bracket } => Ok(TypeAnnotation::Unit {
                left_parenthesis: bracket.opening.clone(),
                right_parenthesis: bracket.closing.clone(),
            }),
            MacroExpandedNode::List(list) => match list.head() {
                MacroExpandedNode::Literal(literal) => match literal {
                    Literal::String(_) => todo!(),
                    Literal::InterpolatedString(_) => todo!(),
                    Literal::Integer(_) => todo!(),
                    Literal::Float(_) => todo!(),
                    Literal::Character(_) => todo!(),

                    Literal::Keyword(_) => todo!(),
                    Literal::Identifier(identifier) => match identifier.representation.as_str() {
                        "forall" => {
                            let type_variables = list.get(1)?.into_type_variables_declaration()?;
                            let type_annotation = list.get(2)?.to_type_annotation()?;
                            list.nodes.get(3).should_be_none()?;
                            Ok(TypeAnnotation::Scheme {
                                type_variables,
                                type_annotation: Box::new(type_annotation),
                            })
                        }
                        _ => Ok(TypeAnnotation::Named {
                            name: identifier.clone(),
                            type_arguments: {
                                match list.tail().split_first() {
                                    None => None,
                                    Some((head, tail)) => Some(TypeArguments {
                                        type_annotations: Box::new(NonEmpty {
                                            head: head.to_type_annotation()?,
                                            tail: tail
                                                .into_iter()
                                                .map(|node| node.to_type_annotation())
                                                .collect::<Result<Vec<_>, _>>()?,
                                        }),
                                    }),
                                }
                            },
                        }),
                    },
                },
                _ => todo!(),
            },
            MacroExpandedNode::Literal(literal) => match literal {
                Literal::Identifier(token) => Ok(TypeAnnotation::Named {
                    name: token.clone(),
                    type_arguments: None,
                }),
                Literal::Keyword(token) => Ok(TypeAnnotation::Keyword {
                    identifier: token.clone(),
                }),
                Literal::String(_) => todo!(),
                Literal::Integer(_) => todo!(),
                Literal::Float(_) => todo!(),
                Literal::Character(_) => todo!(),
                Literal::InterpolatedString(_) => todo!(),
            },
            MacroExpandedNode::Unit { bracket } => Ok(TypeAnnotation::Unit {
                left_parenthesis: bracket.opening.clone(),
                right_parenthesis: bracket.closing.clone(),
            }),
        }
    }

    fn to_function_parameter(&self) -> Result<Parameter, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::List(list) => list.to_function_parameter(),
            MacroExpandedNode::Literal(literal) => match literal {
                Literal::Keyword(identifier) => Ok(Parameter {
                    pattern: DestructurePattern::Identifier(identifier.clone()),
                    type_annotation: TypeAnnotation::Keyword {
                        identifier: identifier.clone(),
                    },
                }),
                Literal::String(_) => todo!(),
                Literal::Integer(_) => todo!(),
                Literal::Float(_) => todo!(),
                Literal::Character(_) => todo!(),
                Literal::Identifier(token) => todo!("{}", token.to_pretty()),
                Literal::InterpolatedString(_) => todo!(),
            },
            MacroExpandedNode::Unit { bracket } => Ok(Parameter {
                pattern: DestructurePattern::Unit {
                    left_parenthesis: bracket.opening.clone(),
                    right_parenthesis: bracket.closing.clone(),
                },
                type_annotation: TypeAnnotation::Unit {
                    left_parenthesis: bracket.opening.clone(),
                    right_parenthesis: bracket.closing.clone(),
                },
            }),
        }
    }

    fn to_key_type_annotation_pair(&self) -> Result<(Token, TypeAnnotation), ParseError> {
        match self {
            _ => todo!(),
        }
    }

    fn to_desctuctured_record_key_value(&self) -> Result<DestructuredRecordKeyValue, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::Literal(Literal::Identifier(identifier)) => {
                Ok(DestructuredRecordKeyValue {
                    key: identifier.clone(),
                    type_annotation: None,
                    as_value: None,
                })
            }
            _ => todo!(),
        }
    }

    fn to_record_update(&self, state: &mut ParserState) -> Result<RecordUpdate, ParseError> {
        use simple_ast::*;
        match self {
            MacroExpandedNode::Literal(Literal::Identifier(token)) => {
                Ok(RecordUpdate::ValueUpdate {
                    property_name: token.clone(),
                    new_value: Expression::Identifier(token.clone()),
                })
            }
            _ => todo!(),
        }
    }
}

struct ValueDeclaration {
    type_variables_declaration: Option<TypeVariablesDeclaration>,
    type_constraints: Option<TypeConstraintsAnnotation>,
    name: Token,
    parameters: Vec<Parameter>,
    type_annotation: TypeAnnotation,
}
impl ValueDeclaration {
    fn into_let_statement(
        self,
        keyword_let: Token,
        expression: Expression,
        access: Access,
    ) -> Result<Statement, ParseError> {
        let type_annotation = {
            let type_annotation = match self.parameters.split_last() {
                // If no parameter is found
                None => self.type_annotation,

                // If some parameters are found
                Some((last, init)) => {
                    TypeAnnotation::Function(Parser::convert_to_function_type_annotation(
                        init.to_vec(),
                        FunctionTypeAnnotation {
                            parameter: Box::new(last.type_annotation.clone()),
                            return_type: Box::new(self.type_annotation),
                            // We place the type constraints at the most inner function type
                            //
                            // For example, if the function type is A -> B -> C, and we have a
                            // constraints X,
                            // X will be placed at (B -> C), not (A -> (B -> C))
                            //
                            // Therefore, we will get A -> ((B -> C) | X)
                            // instead of (A -> (B -> C)) | X
                            type_constraints: self.type_constraints,
                        },
                    ))
                }
            };

            match self.type_variables_declaration {
                Some(type_variables) => TypeAnnotation::Scheme {
                    type_variables,
                    type_annotation: Box::new(type_annotation),
                },
                None => type_annotation,
            }
        };
        Ok(Statement::Let(LetStatement {
            access,
            keyword_let,
            name: self.name,
            doc_string: None,
            type_annotation,
            expression: Parser::convert_to_function(self.parameters, expression),
        }))
    }
}

struct TypeDeclaration {
    name: Token,
    type_variables_declaration: Option<TypeVariablesDeclaration>,
}
impl Positionable for TypeDeclaration {
    fn position(&self) -> Position {
        self.name.position.join_maybe(
            self.type_variables_declaration
                .as_ref()
                .map(|t| t.position()),
        )
    }
}

impl MacroExpandedList {
    fn to_function_signature(&self) -> Result<FunctionSignature, ParseError> {
        use simple_ast::*;
        let last = self.last();
        let init = self.init();
        let return_type = last.to_type_annotation()?;
        let list = init;
        let first = self.first();
        match list.get(0).cloned().into_node(first.position())? {
            MacroExpandedNode::Literal(Literal::Identifier(id))
                if id.representation == "forall" =>
            {
                let type_variables_declaration = list
                    .get(1)
                    .cloned()
                    .into_node(id.position)?
                    .into_type_variables_declaration()?;
                let position = type_variables_declaration.position();
                Ok(FunctionSignature {
                    type_variables_declaration: Some(type_variables_declaration),
                    parameters: NonEmpty {
                        head: list
                            .get(2)
                            .cloned()
                            .into_node(position)?
                            .to_function_parameter()?,
                        tail: list
                            .into_iter()
                            .skip(3)
                            .map(|node| node.to_function_parameter())
                            .collect::<Result<_, _>>()?,
                    },
                    return_type,
                })
            }
            _ => Ok(FunctionSignature {
                type_variables_declaration: None,
                parameters: NonEmpty {
                    head: list
                        .get(0)
                        .cloned()
                        .into_node(first.position())?
                        .to_function_parameter()?,
                    tail: list
                        .iter()
                        .skip(1)
                        .map(|node| node.to_function_parameter())
                        .collect::<Result<_, _>>()?,
                },
                return_type,
            }),
        }
    }
    fn to_expression(&self, state: &mut ParserState) -> Result<Expression, ParseError> {
        use simple_ast::*;
        let tail = self.tail();
        match self.nodes.first() {
            // Match expression
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "match" => {
                Ok(Expression::FunctionCall(Box::new(FunctionCall {
                    type_arguments: None,
                    argument: Box::new(tail.get(0).into_node(id.position)?.to_expression(state)?),
                    function: Box::new(Expression::Function(Box::new(Function {
                        branches: NonEmpty {
                            head: tail
                                .get(1)
                                .into_node(id.position)?
                                .to_function_branch(state)?,
                            tail: tail
                                .into_iter()
                                .skip(2)
                                .map(|node| node.to_function_branch(state))
                                .collect::<Result<Vec<_>, _>>()?,
                        },
                    }))),
                })))
            }

            // Function
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "->" => {
                let pattern = tail.get(0).into_node(id.position)?.to_pattern()?;
                let body = tail
                    .get(1)
                    .into_node(pattern.position())?
                    .to_expression(state)?;
                tail.get(2).should_be_none()?;
                Ok(Expression::Function(Box::new(Function {
                    branches: NonEmpty {
                        head: FunctionBranch {
                            parameter: Box::new(pattern),
                            body: Box::new(body),
                        },
                        tail: vec![],
                    },
                })))
            }

            // Record
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "#" => {
                Ok(Expression::Record {
                    left_curly_bracket: Token::dummy(),
                    right_curly_bracket: Token::dummy(),
                    wildcard: None,
                    key_value_pairs: self
                        .tail()
                        .into_iter()
                        .map(|node| node.to_record_key_value(state))
                        .collect::<Result<_, _>>()?,
                })
            }

            // Do
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "do" => {
                fn make_let_statement(
                    state: &mut ParserState,
                    current: &MacroExpandedNode,
                    next: &[&MacroExpandedNode],
                ) -> Result<Expression, ParseError> {
                    if let Some((head, tail)) = next.split_first() {
                        let (left, right) = match current {
                            MacroExpandedNode::List(list)
                                if list.first().to_identifier(Some("let")).is_ok() =>
                            {
                                let left = list.get(1)?.to_pattern()?;
                                list.get(2)?.to_identifier(Some("="))?;
                                let right = list.get(3)?.to_expression(state)?;
                                list.nodes.get(4).should_be_none()?;
                                (left, right)
                            }
                            _ => {
                                let left = DestructurePattern::Underscore(Token::dummy());
                                let right = current.to_expression(state)?;
                                (left, right)
                            }
                        };
                        Ok(Expression::Let {
                            keyword_let: Token::dummy(),
                            type_annotation: None,
                            left,
                            right: Box::new(right),
                            body: Box::new(make_let_statement(state, *head, tail)?),
                        })
                    } else {
                        current.to_expression(state)
                    }
                }
                make_let_statement(
                    state,
                    &self.nodes.tail().get(0).into_node(id.position)?,
                    self.nodes
                        .tail()
                        .into_iter()
                        .skip(1)
                        .collect::<Vec<_>>()
                        .as_slice(),
                )
            }

            // Intrinsic functions
            MacroExpandedNode::Literal(Literal::Identifier(id))
                if id.representation == "intrinsic" =>
            {
                let function_name = tail.get(0).into_node(id.position)?.to_identifier(None)?;

                tail.get(2).should_be_none()?;
                let argument = Box::new(
                    tail.get(1)
                        .into_node(function_name.position)?
                        .to_expression(state)?,
                );
                Ok(Expression::IntrinsicFunctionCall(IntrinsicFunctionCall {
                    function_name,
                    argument,
                }))
            }

            // Function call
            //
            // For example: (a b c) means ((a b) c)
            _ => {
                if self.nodes.len() == 1 {
                    self.nodes.first().to_expression(state)
                } else {
                    fn make_function(
                        last_node: &MacroExpandedNode,
                        initial_nodes: &[&MacroExpandedNode],
                        state: &mut ParserState,
                    ) -> Result<Expression, ParseError> {
                        if let Some((second_last, init)) = initial_nodes.split_last() {
                            Ok(Expression::FunctionCall(Box::new(FunctionCall {
                                type_arguments: None,
                                function: Box::new(make_function(second_last, init, state)?),
                                argument: Box::new(last_node.to_expression(state)?),
                            })))
                        } else {
                            last_node.to_expression(state)
                        }
                    }

                    make_function(self.nodes.last(), self.nodes.init().as_slice(), state)
                }
            }
        }
    }

    fn to_function_parameter(&self) -> Result<Parameter, ParseError> {
        let pattern = self.first().to_pattern()?;
        let type_annotation = self.get(1)?.to_type_annotation()?;
        self.nodes.get(2).should_be_none()?;
        Ok(Parameter {
            pattern,
            type_annotation,
        })
    }

    fn to_pattern(&self) -> Result<DestructurePattern, ParseError> {
        match self.tail().split_first() {
            None => self.head().to_pattern(),
            Some((second, [])) => todo!(),
            Some((second, tail)) => todo!(),
        }
    }

    fn to_type_variables_declaration(&self) -> Result<TypeVariablesDeclaration, ParseError> {
        let tokens = self
            .nodes
            .to_vector()
            .iter()
            .map(|element| element.to_identifier(None))
            .collect::<Result<Vec<_>, _>>()?;
        match tokens.split_first() {
            None => Err(ParseError {
                context: None,
                kind: todo!(),
            }),
            Some((head, tail)) => Ok(TypeVariablesDeclaration {
                type_variables: NonEmpty {
                    head: head.clone(),
                    tail: tail.to_vec(),
                },
            }),
        }
    }

    fn into_enum_constructor_definitions(
        self,
        state: &mut ParserState,
    ) -> Result<Vec<EnumConstructorDefinition>, ParseError> {
        self.nodes
            .to_vector()
            .into_iter()
            .map(|element| element.to_enum_constructor_definition())
            .collect::<Result<Vec<_>, _>>()
    }

    fn to_type_annotation(&self) -> Result<TypeAnnotation, ParseError> {
        match self.tail().split_first() {
            None => self.head().to_type_annotation(),
            Some((second, tail)) => Ok(TypeAnnotation::Named {
                name: self.head().to_identifier(None)?,
                type_arguments: Some(TypeArguments {
                    type_annotations: Box::new(NonEmpty {
                        head: second.to_type_annotation()?,
                        tail: tail
                            .to_vec()
                            .into_iter()
                            .map(|node| node.to_type_annotation())
                            .collect::<Result<_, _>>()?,
                    }),
                }),
            }),
        }
    }
}

impl simple_ast::List {
    fn expand_macro(&self) -> MacroExpandedNode {
        use simple_ast::*;
        let nodes = self
            .nodes
            .iter()
            .filter_map(Node::expand_macro)
            .collect::<Vec<_>>();
        match self.bracket.kind {
            BracketKind::Round => match nodes.split_first() {
                // Dot chaining
                // For example: (. a (b c) d) is the same as:
                // Infix: a.b(c).d
                // Prefix: (d (b a c))
                //
                // Example 2: (. a b) same as (b a)
                // Example 3: (. a (b c)) same as (b a c)
                // Example 4: (. a b c) same as (c (b a))
                Some((MacroExpandedNode::Literal(Literal::Identifier(id)), [head, tail @ ..]))
                    if id.representation == "." =>
                {
                    // Say `left` is `a` and `tail` is `[b c]`.
                    //
                    fn chain(
                        left: MacroExpandedNode,
                        tail: &[MacroExpandedNode],
                    ) -> MacroExpandedNode {
                        match tail.split_first() {
                            Some((head, tail)) => match head {
                                MacroExpandedNode::List(list) => {
                                    let left = match list.nodes.tail.split_first() {
                                        Some((second, rest)) => {
                                            let first = list.nodes.first();
                                            MacroExpandedNode::List(MacroExpandedList {
                                                nodes: Box::new(NonEmpty {
                                                    head: first.clone(),
                                                    tail: vec![left, second.clone()]
                                                        .into_iter()
                                                        .chain(
                                                            rest.into_iter()
                                                                .map(|node| node.clone()),
                                                        )
                                                        .collect(),
                                                }),
                                            })
                                        }
                                        _ => MacroExpandedNode::List(MacroExpandedList {
                                            nodes: Box::new(NonEmpty {
                                                head: head.clone(),
                                                tail: vec![left],
                                            }),
                                        }),
                                    };
                                    chain(
                                        MacroExpandedNode::List(MacroExpandedList {
                                            nodes: Box::new(NonEmpty {
                                                head: left,
                                                tail: vec![],
                                            }),
                                        }),
                                        tail,
                                    )
                                }

                                // Literal or Unit
                                _ => MacroExpandedNode::List(MacroExpandedList {
                                    nodes: Box::new(NonEmpty {
                                        head: head.clone(),
                                        tail: vec![left],
                                    }),
                                }),
                            },
                            None => MacroExpandedNode::List(MacroExpandedList {
                                nodes: Box::new(NonEmpty {
                                    head: left,
                                    tail: vec![],
                                }),
                            }),
                        }
                    }
                    chain(head.clone(), tail)
                }

                // others
                Some((head, tail)) => MacroExpandedNode::List(MacroExpandedList {
                    nodes: Box::new(NonEmpty {
                        head: head.clone(),
                        tail: tail.to_vec(),
                    }),
                }),

                None => MacroExpandedNode::Unit {
                    bracket: self.bracket.clone(),
                },
            },

            // Right-associative macro
            // Example: {-> a b c} means:
            // Infix: (a -> (b -> c))
            // Prefix: (-> (a (-> b c)))
            BracketKind::Curly => match nodes.split_first() {
                Some((operator, [first_argument, second_argument, tail @ ..])) => {
                    fn right_associativitify(
                        operator: MacroExpandedNode,
                        bracket: Bracket,
                        first_argument: MacroExpandedNode,
                        second_argument: MacroExpandedNode,
                        tail: &[MacroExpandedNode],
                    ) -> MacroExpandedNode {
                        match tail.split_first() {
                            None => {
                                // TODO: we should use another bracket?
                                MacroExpandedNode::List(MacroExpandedList {
                                    nodes: Box::new(NonEmpty {
                                        head: operator,
                                        tail: vec![first_argument, second_argument],
                                    }),
                                })
                            }
                            Some((third_argument, tail)) => {
                                MacroExpandedNode::List(MacroExpandedList {
                                    nodes: Box::new(NonEmpty {
                                        head: operator.clone(),
                                        tail: vec![
                                            first_argument,
                                            right_associativitify(
                                                operator,
                                                bracket,
                                                second_argument,
                                                third_argument.clone(),
                                                tail,
                                            ),
                                        ],
                                    }),
                                })
                            }
                        }
                    }
                    right_associativitify(
                        operator.clone(),
                        self.bracket.clone(),
                        first_argument.clone(),
                        second_argument.clone(),
                        tail,
                    )
                }
                Some((head, tail)) => MacroExpandedNode::List(MacroExpandedList {
                    nodes: Box::new(NonEmpty {
                        head: head.clone(),
                        tail: tail.to_vec(),
                    }),
                }),
                None => MacroExpandedNode::Unit {
                    bracket: self.bracket.clone(),
                },
            },

            // Left-associative macro
            // Example: [+ a b c] means:
            // Infix: ((a + b) + c)
            // Prefix: (+ (+ a b) c)
            BracketKind::Square => {
                todo!()
            }
        }
    }
}

trait Parsable {
    fn into_node(self, previous_position: Position) -> Result<MacroExpandedNode, ParseError>;

    fn should_be_none(self) -> Result<(), ParseError>;
}

impl Parsable for Option<&MacroExpandedNode> {
    fn into_node(self, previous_position: Position) -> Result<MacroExpandedNode, ParseError> {
        match self {
            None => Err(ParseError {
                context: None,
                kind: ParseErrorKind::ExpectedNode { previous_position },
            }),
            Some(node) => Ok(node.clone()),
        }
    }

    fn should_be_none(self) -> Result<(), ParseError> {
        match self {
            None => Ok(()),
            Some(node) => Err(ParseError {
                context: None,
                kind: ParseErrorKind::ExpectedNone {
                    position: node.position(),
                },
            }),
        }
    }
}

fn curry_function_call(function: Expression, arguments: Vec<Expression>) -> Expression {
    match arguments.split_first() {
        Some((head, tail)) => curry_function_call(
            Expression::FunctionCall(Box::new(FunctionCall {
                function: Box::new(function),
                argument: Box::new(head.clone()),
                type_arguments: None,
            })),
            tail.to_vec(),
        ),
        None => function,
    }
}

impl simple_ast::InterpolatedStringSection {
    fn to_interpolated_string_section(
        &self,
        state: &mut ParserState,
    ) -> Result<InterpolatedStringSection, ParseError> {
        match self {
            simple_ast::InterpolatedStringSection::String(string) => {
                Ok(InterpolatedStringSection::String(string.to_string()))
            }
            simple_ast::InterpolatedStringSection::Expression(node) => match node.expand_macro() {
                None => Err(ParseError {
                    context: None,
                    kind: ParseErrorKind::UnexpectedNode {
                        position: node.position(),
                    },
                }),
                Some(node) => Ok(InterpolatedStringSection::Expression(Box::new(
                    node.to_expression(state)?,
                ))),
            },
        }
    }
}

impl simple_ast::InterpolatedString {
    fn to_expression(&self, state: &mut ParserState) -> Result<Expression, ParseError> {
        Ok(Expression::InterpolatedString {
            start_quotes: self.start_quotes.clone(),
            sections: NonEmpty {
                head: self.sections.head.to_interpolated_string_section(state)?,
                tail: self
                    .sections
                    .tail
                    .iter()
                    .map(|section| section.to_interpolated_string_section(state))
                    .collect::<Result<_, _>>()?,
            },
            end_quotes: self.end_quotes.clone(),
        })
    }
}

impl FunctionSignature {
    fn to_function(&self, body: Expression) -> Function {
        fn to_function(parameters: Vec<DestructurePattern>, body: Function) -> Function {
            match parameters.split_last() {
                None => body,
                Some((last, init)) => to_function(
                    init.to_vec(),
                    Function {
                        branches: NonEmpty {
                            head: FunctionBranch {
                                parameter: Box::new(last.clone()),
                                body: Box::new(Expression::Function(Box::new(body))),
                            },
                            tail: vec![],
                        },
                    },
                ),
            }
        }
        to_function(
            self.parameters
                .init()
                .iter()
                .map(|parameter| parameter.pattern.clone())
                .collect(),
            Function {
                branches: NonEmpty {
                    head: FunctionBranch {
                        parameter: Box::new(self.parameters.last().pattern.clone()),
                        body: Box::new(body),
                    },
                    tail: vec![],
                },
            },
        )
    }
    fn to_function_type_annotation(&self) -> TypeAnnotation {
        fn to_function_type_annotation(
            parameter_types: Vec<TypeAnnotation>,
            return_type: FunctionTypeAnnotation,
        ) -> FunctionTypeAnnotation {
            match parameter_types.split_last() {
                Some((last, init)) => to_function_type_annotation(
                    init.to_vec(),
                    FunctionTypeAnnotation {
                        parameter: Box::new(last.clone()),
                        return_type: Box::new(TypeAnnotation::Function(return_type)),
                        type_constraints: None,
                    },
                ),
                None => return_type,
            }
        }
        let function_type_annotation = to_function_type_annotation(
            self.parameters
                .init()
                .into_iter()
                .map(|parameter| parameter.type_annotation.clone())
                .collect(),
            FunctionTypeAnnotation {
                parameter: Box::new(self.parameters.last().type_annotation.clone()),
                return_type: Box::new(self.return_type.clone()),
                type_constraints: None,
            },
        );
        match &self.type_variables_declaration {
            None => TypeAnnotation::Function(function_type_annotation),
            Some(type_variables) => TypeAnnotation::Scheme {
                type_variables: type_variables.clone(),
                type_annotation: Box::new(TypeAnnotation::Function(function_type_annotation)),
            },
        }
    }
}
