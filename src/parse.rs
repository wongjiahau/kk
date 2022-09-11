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
    StatementModule,

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
    TypeVariableConstraint,
    TypeAnnotationFunction,
    Lambda,
    EntryStatement,
    BlockLevelStatement,
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
                // This is for parsing code snippet within documentation string (i.e. multiline comment)
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
        let keyword_export = self.try_eat_token(TokenType::KeywordExport)?;
        match self.peek_next_meaningful_token()? {
            Some(token) => match token.token_type {
                TokenType::KeywordLet => {
                    let token = self.next_meaningful_token()?.unwrap();
                    Ok(vec![Statement::Let(
                        self.parse_top_level_let_statement(keyword_export, token)?,
                    )])
                }
                TokenType::KeywordType => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_type_alias_or_enum_statement(keyword_export, token)
                        .map(vectorized)
                }
                TokenType::KeywordEntry => {
                    let token = self.next_meaningful_token()?.unwrap();
                    Ok(vec![self.parse_entry_statement(keyword_export, token)?])
                }
                TokenType::KeywordModule => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_module_statement(keyword_export, token)
                        .map(vectorized)
                }
                TokenType::MultilineComment { characters } => {
                    // Remove the asterisk at the first column
                    // This is because multiline comments looks like the following
                    // /**
                    //   * Hello
                    //   * ```
                    //   * Code here
                    //   * ```
                    //   */
                    let characters = characters
                        .into_iter()
                        .filter(|character| {
                            !(character.value == '*' && character.column_number == 1)
                        })
                        .collect::<Vec<_>>();
                    self.next_meaningful_token()?.unwrap();
                    let mut statements = vec![];
                    let mut iter = characters.into_iter();
                    loop {
                        let mut backtick_count = 0;
                        loop {
                            if let Some(character) = iter.next() {
                                if character.value == '`' {
                                    backtick_count += 1;
                                } else {
                                    backtick_count = 0;
                                }
                                if backtick_count == 3 {
                                    break;
                                }
                            } else {
                                return Ok(statements);
                            }
                        }

                        // Extract statements from code snippets whenever possible
                        let mut tokenizer = Tokenizer::from_character_iter(iter);
                        let mut parser = Parser::new(&mut tokenizer);
                        statements.extend(parser.parse_statements()?);
                        parser.eat_token(
                            TokenType::TripleBacktick,
                            Some(ParseContext::DocumentationCodeSnippet),
                        )?;
                        iter = tokenizer.remaining_characters().into_iter();
                    }
                }
                _ => Err(Parser::invalid_token(token, None)),
            },
            None => Err(Parser::unexpected_eof(context)),
        }
    }

    fn parse_entry_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_entry: Token,
    ) -> Result<Statement, ParseError> {
        if keyword_export.is_some() {
            panic!("Cannot export entry")
        }
        Ok(Statement::Entry(EntryStatement {
            keyword_entry,
            expression: self.parse_low_precedence_expression()?,
        }))
    }

    fn parse_top_level_let_statement(
        &mut self,
        keyword_export: Option<Token>,
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

        let name = self.eat_token(TokenType::Identifier, context)?;

        let doc_string = self.try_eat_doc_string()?;

        parameters.extend(self.try_parse_parameters()?);

        self.eat_token(TokenType::Colon, context)?;
        let type_annotation = self.parse_type_annotation(context)?;

        let type_constraints = self.try_parse_type_constraints()?;

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
            keyword_export,
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
                left_curly_bracket: Token::dummy(),
                branches: NonEmpty {
                    head: FunctionBranch {
                        parameter: Box::new(head.pattern.clone()),
                        body: Box::new(Self::convert_to_function(tail.to_vec(), return_value)),
                    },
                    tail: vec![],
                },
                right_curly_bracket: Token::dummy(),
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
            if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
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

    fn parse_type_alias_or_enum_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_type: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementType);
        let name = self.eat_token(TokenType::Identifier, context)?;
        let type_variables = self.try_parse_type_variables_declaration()?;
        self.eat_token(TokenType::Equals, context)?;
        if let Some(Token {
            token_type: TokenType::Tag,
            ..
        }) = self.peek_next_meaningful_token()?
        {
            self.parse_enum_statement(keyword_export, keyword_type, name, type_variables)
        } else {
            let right = self.parse_type_annotation(context)?;
            Ok(Statement::Type(TypeAliasStatement {
                keyword_export,
                keyword_type,
                left: name,
                type_variables_declaration: type_variables,
                right,
            }))
        }
    }

    fn parse_enum_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_type: Token,
        name: Token,
        type_variables: Option<TypeVariablesDeclaration>,
    ) -> Result<Statement, ParseError> {
        let mut constructors = vec![self.parse_enum_constructor_definition()?];

        let constructors = loop {
            if self.try_eat_token(TokenType::Pipe)?.is_some() {
                constructors.push(self.parse_enum_constructor_definition()?);
            } else {
                break constructors;
            }
        };
        Ok(Statement::Enum(EnumStatement {
            keyword_export,
            keyword_enum: keyword_type,
            name,
            type_variables_declaration: type_variables,
            constructors,
        }))
    }

    fn parse_module_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_module: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementModule);

        let left = self.parse_module_destructure_pattern()?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_module_value()?;

        Ok(Statement::Module(ModuleStatement {
            keyword_export,
            keyword_module,
            left,
            right,
        }))
    }

    fn parse_module_value(&mut self) -> Result<ModuleValue, ParseError> {
        let context = None;
        let token = self.next_meaningful_token()?;
        match token {
            None => Err(Parser::unexpected_eof(context)),
            Some(token) => match token.token_type {
                TokenType::KeywordImport => {
                    let url = self.eat_token(TokenType::String, context)?;
                    Ok(ModuleValue::Import {
                        keyword_import: token,
                        url,
                    })
                }
                _ => Err(Parser::invalid_token(token, context)),
            },
        }
    }

    fn parse_module_destructure_pattern(&mut self) -> Result<ModuleDestructurePattern, ParseError> {
        let context = None;
        let token = self.next_meaningful_token()?;
        match token {
            None => Err(Parser::unexpected_eof(context)),
            Some(token) => match token.token_type {
                TokenType::Identifier => Ok(ModuleDestructurePattern::Identifier(token)),
                TokenType::LeftParenthesis => {
                    let mut pairs = vec![];
                    let (pairs, right_parenthesis) = loop {
                        if let Some(right_parenthesis) =
                            self.try_eat_token(TokenType::RightParenthesis)?
                        {
                            break (pairs, right_parenthesis);
                        }
                        pairs.push(ModuleDestructurePatternPair {
                            name: self.eat_token(TokenType::Identifier, context)?,
                            pattern: {
                                if self.next_token_is_terminating()? {
                                    None
                                } else {
                                    Some(self.parse_module_destructure_pattern()?)
                                }
                            },
                        })
                    };
                    Ok(ModuleDestructurePattern::Record {
                        left_parenthesis: token,
                        spread: None,
                        pairs,
                        right_parenthesis,
                    })
                }
                _ => {
                    todo!()
                }
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
            left_angular_bracket,
            type_annotations: Box::new(NonEmpty { head, tail }),
            right_angular_bracket,
        })
    }

    fn parse_enum_constructor_definition(
        &mut self,
    ) -> Result<EnumConstructorDefinition, ParseError> {
        let context = Some(ParseContext::EnumConstructorDefinition);
        let name = self.eat_token(TokenType::Tag, context)?;
        if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
            Ok(EnumConstructorDefinition {
                name,
                payload: Some(Box::new(EnumConstructorDefinitionPayload {
                    type_annotation: self
                        .parse_parenthesized_or_record_type_annotation(left_parenthesis)?,
                })),
            })
        } else {
            Ok(EnumConstructorDefinition {
                name,
                payload: None,
            })
        }
    }

    fn try_parse_type_variables_declaration(
        &mut self,
    ) -> Result<Option<TypeVariablesDeclaration>, ParseError> {
        if let Some(less_than) = self.try_eat_token(TokenType::LessThan)? {
            Ok(Some(self.parse_type_variables_declaration(less_than)?))
        } else {
            Ok(None)
        }
    }

    fn parse_type_variables_declaration(
        &mut self,
        left_angular_bracket: Token,
    ) -> Result<TypeVariablesDeclaration, ParseError> {
        let context = Some(ParseContext::TypeVariablesDeclaration);
        let head = self.eat_token(TokenType::Identifier, context)?;
        let mut tail = vec![];
        let mut constraints = vec![];
        let right_angular_bracket = loop {
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                tail.push(self.eat_token(TokenType::Identifier, context)?);
            } else {
                break self.eat_token(TokenType::MoreThan, context)?;
            }
        };

        Ok(TypeVariablesDeclaration {
            left_angular_bracket,
            type_variables: NonEmpty { head, tail },
            right_angular_bracket,
            constraints,
        })
    }

    fn parse_type_variable_constraint(
        &mut self,
        interface_name: Token,
    ) -> Result<TypeVariableConstraint, ParseError> {
        let context = Some(ParseContext::TypeVariableConstraint);
        let type_variables = {
            self.eat_token(TokenType::LessThan, context)?;
            let first_type_variable = self.eat_token(TokenType::Identifier, context)?;
            let mut tail_type_variables = vec![];
            loop {
                if self.try_eat_token(TokenType::MoreThan)?.is_some() {
                    break NonEmpty {
                        head: first_type_variable,
                        tail: tail_type_variables,
                    };
                } else {
                    self.eat_token(TokenType::Comma, context)?;
                    tail_type_variables.push(self.eat_token(TokenType::Identifier, context)?)
                }
            }
        };
        Ok(TypeVariableConstraint {
            interface_name,
            type_variables,
        })
    }

    fn parse_shorthand_dot_expression(&mut self, period: Token) -> Result<Function, ParseError> {
        // This is a function shorthand expression
        let phantom_variable = Token {
            token_type: TokenType::Identifier,
            representation: "$".to_string(),
            position: period.position,
        };
        let function_body =
            self.parse_dot_expression(period, Expression::Identifier(phantom_variable.clone()))?;
        todo!();
        // Ok(Lambda {
        //     branches: NonEmpty {
        //         head: FunctionBranch {
        //             start_token: phantom_variable.clone(),
        //             parameters: NonEmpty {
        //                 head: DestructurePattern::Identifier(phantom_variable),
        //                 tail: vec![],
        //             },
        //             body: Box::new(function_body),
        //         },
        //         tail: vec![],
        //     },
        // })
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
                TokenType::LessThan => {
                    let left_angular_bracket = token;
                    Ok(TypeAnnotation::Scheme {
                        type_variables: self
                            .parse_type_variables_declaration(left_angular_bracket)?,
                        type_annotation: Box::new(self.parse_type_annotation(None)?),
                    })
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
                        self.parse_parenthesized_or_record_type_annotation(left_parenthesis)
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
        left_parenthesis: Token,
        first_key_type_pair: (Token, TypeAnnotation),
    ) -> Result<TypeAnnotation, ParseError> {
        let context = Some(ParseContext::TypeAnnotationRecord);
        let mut key_type_annotation_pairs: Vec<(Token, TypeAnnotation)> = vec![first_key_type_pair];
        let right_parenthesis = loop {
            if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                break right_parenthesis;
            }
            self.eat_token(TokenType::Comma, context)?;
            let key = self.eat_token(TokenType::Identifier, context)?;
            self.eat_token(TokenType::Colon, context)?;
            let type_annotation = self.parse_type_annotation(context)?;
            key_type_annotation_pairs.push((key, type_annotation));
            if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                break right_parenthesis;
            } else {
                self.eat_token(TokenType::Comma, context)?;
            }
        };
        if key_type_annotation_pairs.is_empty() {
            Ok(TypeAnnotation::Unit {
                left_parenthesis,
                right_parenthesis,
            })
        } else {
            Ok(TypeAnnotation::Record {
                left_parenthesis,
                key_type_annotation_pairs,
                right_parenthesis,
            })
        }
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

    /// Dot expressions means either of the following:
    /// - property access, e.g. `x.f`  
    fn parse_dot_expression(
        &mut self,
        _period: Token,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::DotExpression);

        let first_argument =
            if let Some(left_parenthesis) = self.try_eat_token(TokenType::LeftParenthesis)? {
                self.parse_record_update(left_parenthesis, first_argument)?
            } else {
                let name = self.eat_token(TokenType::Identifier, context)?;
                Expression::RecordAccess {
                    expression: Box::new(first_argument),
                    property_name: name,
                }
            };

        self.try_parse_dot_expression(first_argument)
    }

    fn parse_record_update(
        &mut self,
        left_parenthesis: Token,
        expression: Expression,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecordUpdate);
        let mut record_updates: Vec<RecordUpdate> = Vec::new();
        let (record_updates, right_parenthesis) = loop {
            let property_name = self.eat_token(TokenType::Identifier, context)?;
            if self.try_eat_token(TokenType::Equals)?.is_some() {
                // this is a value update
                let new_value = self.parse_mid_precedence_expression()?;
                record_updates.push(RecordUpdate::ValueUpdate {
                    property_name,
                    new_value,
                })
            } else {
                // this is a value update with punning
                record_updates.push(RecordUpdate::ValueUpdate {
                    property_name: property_name.clone(),
                    new_value: Expression::Identifier(property_name),
                })
            }
            if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                break (record_updates, right_parenthesis);
            } else {
                self.eat_token(TokenType::Comma, context)?;
            }
        };

        Ok(Expression::RecordUpdate {
            left_parenthesis,
            expression: Box::new(expression),
            updates: record_updates,
            right_parenthesis,
        })
    }
    fn try_parse_dot_expression(
        &mut self,
        first_argument: Expression,
    ) -> Result<Expression, ParseError> {
        if let Some(period) = self.try_eat_token(TokenType::Period)? {
            self.parse_dot_expression(period, first_argument)
        } else {
            Ok(first_argument)
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
        if let Some(token) = self.peek_next_meaningful_token()? {
            let expression = match &token.token_type {
                TokenType::Period => {
                    let period = self.next_meaningful_token()?.unwrap();
                    Ok(Expression::Function(Box::new(
                        self.parse_shorthand_dot_expression(period)?,
                    )))
                }
                TokenType::Underscore => Ok(Expression::Identifier(
                    self.next_meaningful_token()?.unwrap(),
                )),
                TokenType::JavascriptCode => Ok(Expression::UnsafeJavascript {
                    code: self.next_meaningful_token()?.unwrap(),
                }),

                TokenType::KeywordLet => {
                    let keyword_let = self.next_meaningful_token()?.unwrap();
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
                }
                _ => self.parse_high_precedence_expression(),
            }?;
            self.try_parse_function_call(expression)
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Expression)))
        }
    }

    fn parse_high_precedence_expression(&mut self) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Expression);
        let expression = if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::String => Ok(Expression::String(token.clone())),
                TokenType::InterpolatedString(InterpolatedString {
                    start_quote,
                    sections,
                    end_quote,
                }) => Ok(Expression::InterpolatedString {
                    start_quote,
                    sections,
                    end_quote,
                }),
                TokenType::Character => Ok(Expression::Character(token.clone())),
                TokenType::LeftSquareBracket => self.parse_array(token.clone()),
                TokenType::LeftCurlyBracket => self.parse_function(token.clone()),
                TokenType::LeftParenthesis => {
                    let left_parenthesis = token;
                    self.parse_parenthesized_or_record(left_parenthesis)
                }
                TokenType::Float => Ok(Expression::Float(token.clone())),
                TokenType::Integer => Ok(Expression::Integer(token.clone())),
                TokenType::Identifier => Ok(Expression::Identifier(token.clone())),
                TokenType::Tag => Ok(Expression::EnumConstructor {
                    name: token.clone(),
                    payload: None,
                }),
                TokenType::Tilde => Ok(Expression::CpsClosure {
                    tilde: token.clone(),
                    expression: Box::new(self.parse_high_precedence_expression()?),
                }),
                _ => Err(Parser::invalid_token(token.clone(), context)),
            }
        } else {
            Err(Parser::unexpected_eof(context))
        }?;
        self.try_parse_dot_expression(expression)
    }

    fn parse_array(&mut self, left_square_bracket: Token) -> Result<Expression, ParseError> {
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
            left_square_bracket,
            elements,
            right_square_bracket,
        })
    }

    fn parse_record(
        &mut self,
        left_parenthesis: Token,
        first_element: RecordKeyValue,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecord);
        let mut wildcard = None;
        let mut elements: Vec<RecordKeyValue> = vec![first_element];
        let right_parenthesis = loop {
            if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
                break right_parenthesis;
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
                break self.eat_token(TokenType::RightParenthesis, context)?;
            }
        };
        Ok(Expression::Record {
            wildcard,
            key_value_pairs: elements,
            left_parenthesis,
            right_parenthesis,
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
        left_parenthesis: Token,
        first_key_pattern_pair: DestructuredRecordKeyValue,
    ) -> Result<DestructurePattern, ParseError> {
        let context = Some(ParseContext::PatternRecord);
        let mut wildcard = None;
        let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = vec![first_key_pattern_pair];
        let right_parenthesis = loop {
            match self.next_meaningful_token()? {
                Some(
                    right_parenthesis @ Token {
                        token_type: TokenType::RightParenthesis,
                        ..
                    },
                ) => {
                    break right_parenthesis;
                }
                Some(
                    double_period @ Token {
                        token_type: TokenType::DoublePeriod,
                        ..
                    },
                ) => wildcard = Some(double_period),
                other => {
                    let key = Parser::validate_token(TokenType::Identifier, other, context)?;
                    let as_value = if self.try_eat_token(TokenType::Equals)?.is_some() {
                        Some(self.parse_destructure_pattern()?)
                    } else {
                        None
                    };
                    key_value_pairs.push(DestructuredRecordKeyValue { key, as_value });
                    if self.try_eat_token(TokenType::Comma)?.is_none() {
                        break self.eat_token(TokenType::RightParenthesis, context)?;
                    }
                }
            }
        };
        Ok(DestructurePattern::Record {
            wildcard,
            left_parenthesis,
            key_value_pairs,
            right_parenthesis,
        })
    }

    /// Simple destructure pattern means all kinds of destructure pattern except for OR patterns
    /// OR patterns means `P1 | P2 | P3`
    fn parse_simple_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            let pattern = match token.token_type {
                TokenType::Tag => Ok(DestructurePattern::EnumConstructor {
                    name: token,
                    payload: None,
                }),
                TokenType::Identifier => Ok(DestructurePattern::Identifier(token)),
                TokenType::Underscore => Ok(DestructurePattern::Underscore(token)),
                TokenType::LeftParenthesis => {
                    let left_parenthesis = token;
                    if let Some(right_parenthesis) =
                        self.try_eat_token(TokenType::RightParenthesis)?
                    {
                        Ok(DestructurePattern::Unit {
                            left_parenthesis,
                            right_parenthesis,
                        })
                    } else {
                        let pattern = self.parse_destructure_pattern()?;
                        if self.try_eat_token(TokenType::Comma)?.is_some() {
                            let key = match pattern {
                                DestructurePattern::Identifier(key) => key,
                                other => panic!(),
                            };
                            return self.parse_record_pattern(
                                left_parenthesis,
                                DestructuredRecordKeyValue {
                                    key,
                                    as_value: None,
                                },
                            );
                        }
                        if self.try_eat_token(TokenType::Equals)?.is_some() {
                            let key = match pattern {
                                DestructurePattern::Identifier(key) => key,
                                other => panic!("other ={:#?}", other),
                            };
                            let as_value = Some(self.parse_destructure_pattern()?);
                            if self.try_eat_token(TokenType::Comma)?.is_some() {
                                return self.parse_record_pattern(
                                    left_parenthesis,
                                    DestructuredRecordKeyValue { key, as_value },
                                );
                            } else {
                                return Ok(DestructurePattern::Record {
                                    wildcard: None,
                                    left_parenthesis,
                                    key_value_pairs: vec![DestructuredRecordKeyValue {
                                        key,
                                        as_value,
                                    }],
                                    right_parenthesis: self.eat_token(
                                        TokenType::RightParenthesis,
                                        Some(ParseContext::PatternRecord),
                                    )?,
                                });
                            }
                        }
                        self.eat_token(TokenType::RightParenthesis, None)?;
                        Ok(pattern)
                    }
                }
                TokenType::Integer => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::Integer,
                }),
                TokenType::String => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::String,
                }),
                TokenType::Character => Ok(DestructurePattern::Infinite {
                    token,
                    kind: InfinitePatternKind::Character,
                }),
                TokenType::LeftCurlyBracket => {
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
            }?;
            self.try_parse_enum_constructor_with_payload_pattern(pattern)
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Pattern)))
        }
    }

    fn try_parse_enum_constructor_with_payload_pattern(
        &mut self,
        previous_pattern: DestructurePattern,
    ) -> Result<DestructurePattern, ParseError> {
        if self.next_token_is_terminating()? {
            return Ok(previous_pattern);
        }
        let next_pattern = self.parse_simple_destructure_pattern()?;
        let pattern = match next_pattern {
            DestructurePattern::EnumConstructor {
                name,
                payload: None,
            } => DestructurePattern::EnumConstructor {
                name,
                payload: Some(Box::new(previous_pattern)),
            },
            _ => match previous_pattern {
                DestructurePattern::EnumConstructor {
                    name,
                    payload: None,
                } => DestructurePattern::EnumConstructor {
                    name,
                    payload: Some(Box::new(next_pattern)),
                },
                _ => panic!("Syntax error"),
            },
        };
        self.try_parse_enum_constructor_with_payload_pattern(pattern)
    }

    fn parse_function_tail_branches(
        &mut self,
        head: FunctionBranch,
    ) -> Result<NonEmpty<FunctionBranch>, ParseError> {
        let mut tail = vec![];
        let tail = loop {
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                tail.push(self.parse_function_branch()?)
            } else {
                break tail;
            }
        };
        Ok(NonEmpty { head, tail })
    }

    fn parse_function(&mut self, left_curly_bracket: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Lambda);
        let (branches, right_curly_bracket) = {
            // Try parse as block first
            let block = self._try(|parser| {
                let expression = parser.parse_low_precedence_expression()?;
                Ok((
                    NonEmpty {
                        head: FunctionBranch {
                            parameter: Box::new(DestructurePattern::Unit {
                                left_parenthesis: left_curly_bracket.clone(),
                                right_parenthesis: left_curly_bracket.clone(),
                            }),
                            body: Box::new(expression),
                        },
                        tail: vec![],
                    },
                    parser.eat_token(TokenType::RightCurlyBracket, context)?,
                ))
            });
            match block {
                Some(block) => block,
                None => {
                    // Otherwise, parse a lambda
                    let head = FunctionBranch {
                        parameter: Box::new(self.parse_destructure_pattern()?),
                        body: {
                            self.eat_token(TokenType::ArrowRight, context)?;
                            Box::new(self.parse_low_precedence_expression()?)
                        },
                    };
                    (
                        self.parse_function_tail_branches(head)?,
                        self.eat_token(TokenType::RightCurlyBracket, context)?,
                    )
                }
            }
        };
        Ok(Expression::Function(Box::new(Function {
            left_curly_bracket,
            branches,
            right_curly_bracket,
        })))
    }

    fn parse_function_branch(&mut self) -> Result<FunctionBranch, ParseError> {
        let parameter = self.parse_destructure_pattern()?;
        self.eat_token(TokenType::ArrowRight, None)?;
        let body = self.parse_low_precedence_expression()?;
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
            let function = Box::new(self.parse_low_precedence_expression()?);
            return self.try_parse_function_call(Expression::CpsBang {
                argument: Box::new(previous),
                bang,
                function,
            });
        }

        let next = self.parse_high_precedence_expression()?;
        let (function, argument) = match &next {
            // TODO: handle operator

            // Postfix
            Expression::Identifier(_) | Expression::EnumConstructor { .. } => (next, previous),

            // Prefix function call
            _ => (previous, next),
        };

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
                    | TokenType::Pipe
                    | TokenType::KeywordLet
                    | TokenType::RightParenthesis
                    | TokenType::RightCurlyBracket
                    | TokenType::RightSquareBracket
                    | TokenType::KeywordType
                    | TokenType::KeywordEntry
                    | TokenType::Semicolon
                    | TokenType::Colon
                    | TokenType::ArrowRight
            )),
        }
    }

    fn parse_parenthesized_or_record_type_annotation(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<TypeAnnotation, ParseError> {
        let context = None;
        let type_annotation = self.parse_type_annotation(context)?;
        match &type_annotation {
            TypeAnnotation::Named {
                name,
                type_arguments: None,
            } => {
                if self.try_eat_token(TokenType::Colon)?.is_some() {
                    let type_annotation = self.parse_type_annotation(context)?;
                    self.parse_record_type_annotation(
                        left_parenthesis,
                        (name.clone(), type_annotation),
                    )
                } else {
                    Ok(TypeAnnotation::Parenthesized {
                        left_parenthesis,
                        type_annotation: Box::new(type_annotation),
                        right_parenthesis: self.eat_token(TokenType::RightParenthesis, context)?,
                    })
                }
            }
            _ => Ok(TypeAnnotation::Parenthesized {
                left_parenthesis,
                type_annotation: Box::new(type_annotation),
                right_parenthesis: self.eat_token(TokenType::RightParenthesis, context)?,
            }),
        }
    }

    fn try_parse_type_constraints(
        &mut self,
    ) -> Result<Option<TypeConstraintsAnnotation>, ParseError> {
        let context = None;
        if self.try_eat_token(TokenType::KeywordExists)?.is_some() {
            Ok(Some(TypeConstraintsAnnotation {
                left_parenthesis: self.eat_token(TokenType::LeftParenthesis, context)?,
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
                right_parenthesis: self.eat_token(TokenType::RightParenthesis, context)?,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_parenthesized_or_record(
        &mut self,
        left_parenthesis: Token,
    ) -> Result<Expression, ParseError> {
        if let Some(right_parenthesis) = self.try_eat_token(TokenType::RightParenthesis)? {
            return Ok(Expression::Unit {
                left_parenthesis,
                right_parenthesis,
            });
        }
        let expression = self.parse_low_precedence_expression()?;
        if self.try_eat_token(TokenType::Comma)?.is_some() {
            let first_key_value_pair = match &expression {
                Expression::Identifier(key) => RecordKeyValue {
                    key: key.clone(),
                    value: expression,
                },
                other => RecordKeyValue {
                    key: Token {
                        token_type: TokenType::Identifier,
                        representation: "0".to_string(),
                        position: other.position(),
                    },
                    value: other.clone(),
                },
            };
            return self.parse_record(left_parenthesis, first_key_value_pair);
        }
        if self.try_eat_token(TokenType::Equals)?.is_some() {
            let key = match expression {
                Expression::Identifier(token) => Ok(token),
                _ => panic!("Expected identifier"),
            }?;
            let value = self.parse_low_precedence_expression()?;
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                return self.parse_record(left_parenthesis, RecordKeyValue { key, value });
            } else {
                return Ok(Expression::Record {
                    wildcard: None,
                    left_parenthesis,
                    key_value_pairs: vec![RecordKeyValue { key, value }],
                    right_parenthesis: self.eat_token(
                        TokenType::RightParenthesis,
                        Some(ParseContext::ExpressionRecord),
                    )?,
                });
            }
        }

        Ok(Expression::Parenthesized {
            left_parenthesis,
            right_parenthesis: self.eat_token(TokenType::RightParenthesis, None)?,
            value: Box::new(expression),
        })
    }

    fn try_eat_doc_string(&mut self) -> Result<Option<DocString>, ParseError> {
        match self.peek_next_meaningful_token()? {
            None => Ok(None),
            Some(token) => match token.token_type {
                TokenType::InterpolatedString(interpolated_string) => {
                    self.next_meaningful_token()?;
                    Ok(Some(DocString::InterpolatedString(interpolated_string)))
                }
                TokenType::String => {
                    self.next_meaningful_token()?;
                    Ok(Some(DocString::String(token)))
                }
                _ => Ok(None),
            },
        }
    }

    /// Try parsing using `f`,
    /// if `f` fails, the iterator will be reverted back to its previous state
    /// (i.e. before calling `f`).
    ///
    /// Note: this method should not be used recursively.
    fn _try<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        self.tokenizer.create_checkpoint();
        match f(self) {
            Ok(result) => {
                self.tokenizer.delete_checkpoint();
                Some(result)
            }
            Err(_) => {
                self.tokenizer.restore_checkpoint();
                None
            }
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

/// This function is for converting an expression into a pattern.
/// This is possible because a pattern should be a subset of an expression.  
/// In other words, every valid pattern is also a valid expression, but not vice versa.
fn convert_expression_to_pattern(
    expression: Expression,
    context: Option<ParseContext>,
) -> Result<DestructurePattern, ParseError> {
    match expression {
        Expression::Identifier(token) => match token.representation.as_str() {
            "_" => Ok(DestructurePattern::Underscore(token)),
            _ => Ok(DestructurePattern::Identifier(token)),
        },
        Expression::Unit {
            left_parenthesis,
            right_parenthesis,
        } => Ok(DestructurePattern::Unit {
            left_parenthesis,
            right_parenthesis,
        }),
        Expression::Record {
            wildcard,
            left_parenthesis,
            right_parenthesis,
            key_value_pairs,
        } => {
            let key_value_pairs = key_value_pairs
                .to_vec()
                .into_iter()
                .map(|RecordKeyValue { key, value }| {
                    Ok(DestructuredRecordKeyValue {
                        key,
                        as_value: Some(convert_expression_to_pattern(
                            value,
                            Some(ParseContext::PatternRecord),
                        )?),
                    })
                })
                .collect::<Result<Vec<DestructuredRecordKeyValue>, ParseError>>()?;
            Ok(DestructurePattern::Record {
                left_parenthesis,
                wildcard,
                key_value_pairs,
                right_parenthesis,
            })
        }
        Expression::EnumConstructor { name, payload } => Ok(DestructurePattern::EnumConstructor {
            name,
            payload: match payload {
                None => None,
                Some(payload) => Some(Box::new(convert_expression_to_pattern(
                    *payload,
                    Some(ParseContext::ExpressionFunction),
                )?)),
            },
        }),
        Expression::Integer(token) => Ok(DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::Integer,
        }),
        Expression::String(token) => Ok(DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::String,
        }),
        Expression::Character(token) => Ok(DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::Character,
        }),
        Expression::Float(_)
        | Expression::InterpolatedString { .. }
        | Expression::Function(_)
        | Expression::FunctionCall(_)
        | Expression::RecordAccess { .. }
        | Expression::RecordUpdate { .. }
        | Expression::Let { .. }
        | Expression::Statements { .. }
        | Expression::CpsBang { .. }
        | Expression::CpsClosure { .. }
        | Expression::UnsafeJavascript { .. } => Err(ParseError {
            context,
            kind: ParseErrorKind::ExpectedPattern {
                position: expression.position(),
            },
        }),
        Expression::Array { .. } => {
            panic!("Not sure how to handle yet")
        }
        Expression::Parenthesized { value, .. } => convert_expression_to_pattern(*value, context),
    }
}
