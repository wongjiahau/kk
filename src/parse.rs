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
    ExpressionQuoted,
    ExpressionSwitch,
    ExpressionIf,

    // Statement
    Statement,
    StatementLet,
    StatementType,
    StatementEnum,
    StatementImport,
    StatementWith,
    StatementInterface,
    StatementImplements,

    // Type annotation
    TypeAnnotationRecord,
    TypeAnnotationArray,
    TypeAnnotationQuoted,

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
                    self.parse_let_statement(keyword_export, token)
                        .map(vectorized)
                }
                TokenType::KeywordWith => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_with_statement(token).map(vectorized)
                }
                TokenType::KeywordType => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_type_statement(keyword_export, token)
                        .map(vectorized)
                }
                TokenType::KeywordEntry => {
                    let token = self.next_meaningful_token()?.unwrap();
                    Ok(vec![self.parse_entry_statement(keyword_export, token)?])
                }
                TokenType::KeywordEnum => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_enum_statement(keyword_export, token)
                        .map(vectorized)
                }
                TokenType::KeywordImport => {
                    let token = self.next_meaningful_token()?.unwrap();
                    self.parse_import_statement(token).map(vectorized)
                }
                TokenType::KeywordInterface => {
                    let keyword_interface = self.next_meaningful_token()?.unwrap();
                    self.parse_interface_statement(keyword_export, keyword_interface)
                        .map(vectorized)
                }
                TokenType::KeywordImplements => {
                    let keyword_implements = self.next_meaningful_token()?.unwrap();
                    self.parse_implements_statement(keyword_export, keyword_implements)
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
                _ => Ok(vec![Statement::Expression(self.parse_expression()?)]),
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
        let context = Some(ParseContext::EntryStatement);
        let left_curly_bracket = self.eat_token(TokenType::LeftCurlyBracket, context)?;
        Ok(Statement::Entry(EntryStatement {
            keyword_entry,
            block: self.parse_block(left_curly_bracket)?,
        }))
    }

    fn parse_implements_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_implements: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementImplements);
        let type_variables_declaration = self.try_parse_type_variables_declaration()?;
        let interface_name = self.eat_token(TokenType::Identifier, context)?;
        let less_than = self.eat_token(TokenType::LessThan, context)?;
        let for_types = self.parse_type_arguments(less_than, context)?;
        let left_curly_bracket = self.eat_token(TokenType::LeftCurlyBracket, context)?;
        let mut definitions = vec![];
        let (definitions, right_curly_bracket) = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break (definitions, right_curly_bracket);
            } else {
                let keyword_let = self.eat_token(TokenType::KeywordLet, context)?;
                let name = self.eat_token(TokenType::Identifier, context)?;
                self.eat_token(TokenType::Equals, context)?;
                let expression = self.parse_expression()?;
                definitions.push(ImplementDefinition {
                    keyword_let,
                    name,
                    expression,
                })
            }
        };
        Ok(Statement::Implement(ImplementStatement {
            keyword_export,
            keyword_implements,
            type_variables_declaration,
            interface_name,
            for_types,
            left_curly_bracket,
            definitions,
            right_curly_bracket,
        }))
    }

    fn parse_interface_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_interface: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementInterface);
        let name = self.eat_token(TokenType::Identifier, context)?;
        let less_than = self.eat_token(TokenType::LessThan, context)?;
        let type_variables = self.parse_type_variables_declaration(less_than)?;
        let left_curly_bracket = self.eat_token(TokenType::LeftCurlyBracket, context)?;

        let mut definitions = vec![];
        let (definitions, right_curly_bracket) = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break (definitions, right_curly_bracket);
            } else {
                let keyword_let = self.eat_token(TokenType::KeywordLet, context)?;
                let name = self.eat_token(TokenType::Identifier, context)?;
                self.eat_token(TokenType::Colon, context)?;
                let type_annotation = self.parse_type_annotation(context)?;
                definitions.push(InterfaceDefinition {
                    keyword_let,
                    name,
                    type_annotation,
                })
            }
        };
        Ok(Statement::Interface(InterfaceStatement {
            keyword_export,
            keyword_interface,
            name,
            type_variables_declartion: type_variables,
            left_curly_bracket,
            definitions,
            right_curly_bracket,
        }))
    }

    fn parse_with_statement(&mut self, keyword_with: Token) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementWith);
        let pattern = self.parse_destructure_pattern()?;
        self.eat_token(TokenType::Equals, context)?;
        let (binary_function_name, right) = {
            match self.parse_expression()? {
                Expression::RecordAccess {
                    expression,
                    property_name,
                } => Ok((property_name, expression)),
                other => panic!(
                    "Error, expected record access for the right of with statement but got {:#?}",
                    other
                ),
            }
        }?;
        let mut statements = vec![];
        loop {
            match self.peek_next_meaningful_token()? {
                Some(Token {
                    token_type: TokenType::RightCurlyBracket,
                    ..
                }) => break,
                _ => {
                    statements.extend(self.parse_statement()?);
                }
            }
        }
        let statements = match statements.split_first() {
            None => panic!("With statement need to have at least one trailing statement"),
            Some((head, tail)) => Ok(NonEmpty {
                head: head.clone(),
                tail: tail.into(),
            }),
        }?;
        let with_expression = Expression::With(WithExpression {
            keyword_with,
            left_patterns: Box::new(NonEmpty {
                head: pattern,
                tail: vec![],
            }),
            binary_function_name,
            right,
            body: Box::new(Expression::Block(Block::WithoutBrackets {
                statements: Box::new(statements),
            })),
        });
        Ok(Statement::Expression(with_expression))
    }

    fn parse_let_statement(
        &mut self,
        keyword_export: Option<Token>,
        keyword_let: Token,
    ) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementLet);
        let left = self.parse_destructure_pattern()?;
        let type_annotation = self.try_parse_type_annotation()?;
        self.eat_token(TokenType::Equals, context)?;
        let right = self.parse_expression()?;
        Ok(Statement::Let(LetStatement {
            keyword_export,
            keyword_let,
            left,
            right,
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
            type_variables_declaration: type_variables,
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

        let constructors = loop {
            if self.try_eat_token(TokenType::Pipe)?.is_some() {
                constructors.push(self.parse_enum_constructor_definition()?);
            } else {
                break constructors;
            }
        };
        Ok(Statement::Enum(EnumStatement {
            keyword_export,
            keyword_enum,
            name,
            type_variables_declaration: type_variables,
            constructors,
        }))
    }

    fn parse_imported_name(
        &mut self,
        context: Option<ParseContext>,
    ) -> Result<ImportedName, ParseError> {
        let name = self.eat_token(TokenType::Identifier, context)?;
        Ok(if self.try_eat_token(TokenType::KeywordAs)?.is_some() {
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
        })
    }

    fn parse_import_statement(&mut self, keyword_import: Token) -> Result<Statement, ParseError> {
        let context = Some(ParseContext::StatementImport);

        let import_type = if let Some(asterisk) = self.try_eat_token(TokenType::Asterisk)? {
            ImportType::All { asterisk }
        } else {
            self.eat_token(TokenType::LeftCurlyBracket, context)?;

            let first_name = self.parse_imported_name(context)?;
            let other_names = {
                let mut other_names = Vec::new();
                loop {
                    if self.try_eat_token(TokenType::RightCurlyBracket)?.is_some() {
                        break other_names;
                    } else if self.try_eat_token(TokenType::Comma)?.is_some() {
                        other_names.push(self.parse_imported_name(context)?)
                    }
                }
            };
            ImportType::Selected {
                imported_names: NonEmpty {
                    head: first_name,
                    tail: other_names,
                },
            }
        };
        self.eat_token(TokenType::KeywordFrom, context)?;
        let url = self.eat_token(TokenType::String, context)?;
        Ok(Statement::Import(ImportStatement {
            keyword_import,
            url,
            import_type,
        }))
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
            } else if self.try_eat_token(TokenType::KeywordWhere)?.is_some() {
                loop {
                    if let Some(interface_name) = self.try_eat_token(TokenType::Identifier)? {
                        constraints.push(self.parse_type_variable_constraint(interface_name)?);
                        if self.try_eat_token(TokenType::Comma)?.is_none() {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                break self.eat_token(TokenType::MoreThan, context)?;
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

    fn parse_shorthand_dot_expression(&mut self, period: Token) -> Result<Lambda, ParseError> {
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

    fn parse_function_parameters(&mut self) -> Result<FunctionParameters, ParseError> {
        let context = Some(ParseContext::FunctionParameters);
        let left_parenthesis = self.eat_token(TokenType::LeftParenthesis, context)?;
        if self.try_eat_token(TokenType::RightParenthesis)?.is_some() {
            panic!("Still pending design decision")
        } else {
            let first_parameter = FunctionParameter {
                pattern: self.parse_destructure_pattern()?,
                type_annotation: Box::new(self.try_parse_type_annotation()?),
            };
            self.parse_function_parameters_tail(left_parenthesis, first_parameter, false)
        }
    }

    /// This function assumes that left parenthesis and the first parameter is already parsed
    fn parse_function_parameters_tail(
        &mut self,
        left_parenthesis: Token,
        first_parameter: FunctionParameter,
        comma_is_eaten: bool,
    ) -> Result<FunctionParameters, ParseError> {
        let context = Some(ParseContext::FunctionParameters);
        let mut tail_parameters = vec![];
        let right_parenthesis = if comma_is_eaten || self.try_eat_token(TokenType::Comma)?.is_some()
        {
            loop {
                match self.peek_next_meaningful_token()? {
                    Some(
                        right_parenthesis @ Token {
                            token_type: TokenType::RightParenthesis,
                            ..
                        },
                    ) => break right_parenthesis,
                    _ => {
                        let pattern = self.parse_destructure_pattern()?;
                        let type_annotation = self.try_parse_type_annotation()?;
                        tail_parameters.push(FunctionParameter {
                            pattern,
                            type_annotation: Box::new(type_annotation),
                        });
                        if self.try_eat_token(TokenType::Comma)?.is_none() {
                            break self.eat_token(TokenType::RightParenthesis, context)?;
                        }
                    }
                };
            }
        } else {
            self.eat_token(TokenType::RightParenthesis, context)?
        };
        Ok(FunctionParameters {
            parameters: Box::new(NonEmpty {
                head: first_parameter,
                tail: tail_parameters,
            }),
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
                TokenType::HashLeftCurlyBracket => {
                    let hash_left_curly_bracket = token;
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
                        if let Some(right_curly_bracket) =
                            self.try_eat_token(TokenType::RightCurlyBracket)?
                        {
                            break right_curly_bracket;
                        } else {
                            self.eat_token(TokenType::Comma, context)?;
                        }
                    };
                    Ok(TypeAnnotation::Record {
                        left_curly_bracket: hash_left_curly_bracket,
                        key_type_annotation_pairs,
                        right_curly_bracket,
                    })
                }
                TokenType::LeftParenthesis => {
                    let left_parenthesis = token;
                    let type_annotation = self.parse_type_annotation(context)?;
                    match &type_annotation {
                        TypeAnnotation::Named {
                            name,
                            type_arguments: None,
                        } => match self.peek_next_meaningful_token()? {
                            _ => {
                                self.eat_token(TokenType::RightParenthesis, context)?;
                                Ok(type_annotation)
                            }
                        },
                        _ => {
                            self.eat_token(TokenType::RightParenthesis, context)?;
                            Ok(type_annotation)
                        }
                    }
                }
                _ => Err(Parser::invalid_token(token, context)),
            }?;
            self.try_parse_function_type_annotation(potential_function_parameter_type)
        } else {
            Err(Parser::unexpected_eof(context))
        }
    }

    fn try_parse_function_type_annotation(
        &mut self,
        potential_function_parameter_type: TypeAnnotation,
    ) -> Result<TypeAnnotation, ParseError> {
        if self.try_eat_token(TokenType::ArrowRight)?.is_some() {
            Ok(TypeAnnotation::Function {
                parameters: Box::new(NonEmpty {
                    head: potential_function_parameter_type,
                    tail: vec![],
                }),
                return_type: Box::new(self.parse_type_annotation(None)?),
            })
        } else {
            Ok(potential_function_parameter_type)
        }
    }

    /// Dot expressions means either of the following:
    /// - function call, e.g. `x.f()`  
    /// - property access, e.g. `x.f`  
    /// - record update, e.g. `x.{ a: 3 }` or `x.{ a.square() }`
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
                            function: Box::new(Expression::Identifier(name)),
                        }))
                    }
                    _ => {
                        // Then this is a property access or single-argument function call
                        Expression::RecordAccess {
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
                let phantom_variable = Expression::Identifier(property_name.clone());
                let function_body = self.parse_dot_expression(period.clone(), phantom_variable)?;
                record_updates.push(RecordUpdate::FunctionalUpdate {
                    function: Expression::Lambda(Box::new(Lambda {
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
                        left_curly_bracket,
                        right_curly_bracket: todo!(),
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
                    new_value: Expression::Identifier(property_name),
                })
            }
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break (record_updates, right_curly_bracket);
            } else {
                self.eat_token(TokenType::Comma, context)?;
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
        if self.next_token_is_terminating()? {
            return Ok(None);
        }
        let mut arguments = vec![];
        loop {
            if let Some(token) = self.peek_next_meaningful_token()? {
                match token.token_type {
                    TokenType::Identifier | TokenType::Operator => {
                        return Ok(Some(FunctionCallRestArguments { arguments }))
                    }
                    _ => arguments.push(self.parse_simple_expression()?),
                }
            }
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
                TokenType::Period => {
                    let period = self.next_meaningful_token()?.unwrap();
                    Ok(Expression::Lambda(Box::new(
                        self.parse_shorthand_dot_expression(period)?,
                    )))
                }
                TokenType::Underscore => Ok(Expression::Identifier(
                    self.next_meaningful_token()?.unwrap(),
                )),
                TokenType::KeywordIf => {
                    let keyword_if = self.next_meaningful_token()?.unwrap();
                    self.parse_if_expression(keyword_if)
                }
                TokenType::KeywordSwitch => {
                    let keyword_switch = self.next_meaningful_token()?.unwrap();
                    self.parse_switch_expression(keyword_switch)
                }
                TokenType::JavascriptCode => Ok(Expression::UnsafeJavascript {
                    code: self.next_meaningful_token()?.unwrap(),
                }),
                TokenType::LessThan => {
                    let left_angular_bracket = self.next_meaningful_token()?.unwrap();
                    let type_variables =
                        self.parse_type_variables_declaration(left_angular_bracket)?;
                    let function_parameters = self.parse_function_parameters()?;

                    Ok(Expression::ArrowFunction(Box::new(
                        self.parse_arrow_function(Some(type_variables), function_parameters)?,
                    )))
                }

                TokenType::LeftParenthesis => {
                    let left_parenthesis = self.next_meaningful_token()?.unwrap();
                    let expression = self.parse_expression()?;
                    match self.next_meaningful_token()? {
                        None => Err(ParseError {
                            kind: ParseErrorKind::UnexpectedEof {
                                expected_token_type: None,
                            },
                            context: None, // TODO: add context
                        }),
                        Some(token) => {
                            match token.token_type {
                                TokenType::RightParenthesis => {
                                    // This is not a lambda expression
                                    // Just an expression grouped by parentheses
                                    Ok(Expression::Parenthesized {
                                        left_parenthesis,
                                        right_parenthesis: token,
                                        value: Box::new(expression),
                                    })
                                }
                                TokenType::Colon => {
                                    let context = Some(ParseContext::ExpressionFunction);
                                    let type_annotation = self.parse_type_annotation(context)?;
                                    let parameters = self.parse_function_parameters_tail(
                                        left_parenthesis,
                                        FunctionParameter {
                                            pattern: convert_expression_to_pattern(
                                                expression,
                                                Some(ParseContext::ExpressionFunction),
                                            )?,
                                            type_annotation: Box::new(Some(type_annotation)),
                                        },
                                        false,
                                    )?;
                                    Ok(Expression::ArrowFunction(Box::new(
                                        self.parse_arrow_function(None, parameters)?,
                                    )))
                                }
                                TokenType::Comma => {
                                    let parameters = self.parse_function_parameters_tail(
                                        left_parenthesis,
                                        FunctionParameter {
                                            pattern: convert_expression_to_pattern(
                                                expression,
                                                Some(ParseContext::ExpressionFunction),
                                            )?,
                                            type_annotation: Box::new(None),
                                        },
                                        true,
                                    )?;
                                    Ok(Expression::ArrowFunction(Box::new(
                                        self.parse_arrow_function(None, parameters)?,
                                    )))
                                }
                                other => {
                                    panic!("Cannot handle {:?}", other)
                                }
                            }
                        }
                    }
                }
                _ => {
                    let simple_expression = self.parse_simple_expression()?;
                    self.try_parse_function_call(simple_expression)
                }
            }
        } else {
            Err(Parser::unexpected_eof(Some(ParseContext::Expression)))
        }
    }

    fn parse_arrow_function(
        &mut self,
        type_variables: Option<TypeVariablesDeclaration>,
        parameters: FunctionParameters,
    ) -> Result<ArrowFunction, ParseError> {
        let context = Some(ParseContext::ExpressionFunction);
        let return_type_annotation = self.try_parse_type_annotation()?;
        self.eat_token(TokenType::ArrowRight, context)?;
        let body = self.parse_expression()?;
        Ok(ArrowFunction {
            type_variables_declaration: type_variables,
            return_type_annotation,
            parameters: ArrowFunctionParameters::WithParenthesis(Box::new(parameters)),
            body: Box::new(body),
        })
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
                TokenType::HashLeftCurlyBracket => self.parse_record_or_block(token.clone()),
                TokenType::LeftCurlyBracket => self.parse_lambda(token.clone()),
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
                TokenType::Identifier => Ok(Expression::Identifier(token.clone())),
                TokenType::Tag => {
                    let name = token.clone();

                    Ok(Expression::EnumConstructor {
                        name,
                        payload: {
                            if let Some(left_parenthesis) =
                                self.try_eat_token(TokenType::LeftParenthesis)?
                            {
                                let context = Some(ParseContext::ExpressionEnumConstructor);
                                let expression = self.parse_expression()?;
                                let right_parenthesis =
                                    self.eat_token(TokenType::RightParenthesis, context)?;
                                Some(Box::new(ExpressionEnumConstructorPayload {
                                    left_parenthesis,
                                    expression,
                                    right_parenthesis,
                                }))
                            } else {
                                None
                            }
                        },
                    })
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
            if !elements.is_empty() {
                self.eat_token(TokenType::Comma, None)?;
            }
            elements.push(self.parse_expression()?);
        };
        Ok(Expression::Array {
            left_square_bracket,
            elements,
            right_square_bracket,
        })
    }

    fn parse_block(&mut self, left_curly_bracket: Token) -> Result<Block, ParseError> {
        let statements = self.parse_statement()?;
        let mut statements = statements;
        let right_curly_bracket = loop {
            if self.try_eat_token(TokenType::Comma)?.is_some() {
                statements.extend(self.parse_statement()?);
            } else {
                break self.eat_token(TokenType::RightCurlyBracket, None)?;
            }
        };
        Ok(Block::WithBrackets {
            left_curly_bracket,
            statements,
            right_curly_bracket,
        })
    }

    fn parse_record_or_block(
        &mut self,
        left_curly_bracket: Token,
    ) -> Result<Expression, ParseError> {
        if let Some(wildcard) = self.try_eat_token(TokenType::TriplePeriod)? {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                return Ok(Expression::Record {
                    left_curly_bracket,
                    wildcard: Some(wildcard),
                    key_value_pairs: vec![],
                    right_curly_bracket,
                });
            } else {
                let context = Some(ParseContext::ExpressionRecord);
                self.eat_token(TokenType::Comma, context)?;
                let key = self.eat_token(TokenType::Identifier, context)?;
                if let Some(right_curly_bracket) =
                    self.try_eat_token(TokenType::RightCurlyBracket)?
                {
                    return Ok(Expression::Record {
                        left_curly_bracket,
                        wildcard: Some(wildcard),
                        key_value_pairs: vec![RecordKeyValue {
                            key: key.clone(),
                            value: Expression::Identifier(key),
                        }],
                        right_curly_bracket,
                    });
                } else if self.try_eat_token(TokenType::Comma)?.is_some() {
                    return self.parse_record(
                        left_curly_bracket,
                        RecordKeyValue {
                            key: key.clone(),
                            value: Expression::Identifier(key),
                        },
                    );
                } else {
                    self.eat_token(TokenType::Colon, context)?;
                    let value = self.parse_expression()?;
                    return self.parse_record(left_curly_bracket, RecordKeyValue { key, value });
                }
            }
        };
        let statements = self.parse_statement()?;
        match (self.peek_next_meaningful_token()?, statements.first()) {
            (
                Some(Token {
                    token_type: TokenType::Colon,
                    ..
                }),
                Some(Statement::Expression(Expression::Identifier(identifier))),
            ) => {
                let context = Some(ParseContext::ExpressionRecord);
                self.eat_token(TokenType::Colon, context)?;
                let first_key_value = RecordKeyValue {
                    key: identifier.clone(),
                    value: self.parse_expression()?,
                };
                self.parse_record(left_curly_bracket, first_key_value)
            }
            (
                Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }),
                Some(Statement::Expression(Expression::Identifier(identifier))),
            ) => {
                let first_key_value = RecordKeyValue {
                    key: identifier.clone(),
                    value: Expression::Identifier(identifier.clone()),
                };
                self.parse_record(left_curly_bracket, first_key_value)
            }
            (
                Some(Token {
                    token_type: TokenType::RightCurlyBracket,
                    ..
                }),
                Some(Statement::Expression(Expression::Identifier(identifier))),
            ) => {
                let context = Some(ParseContext::ExpressionRecord);
                Ok(Expression::Record {
                    wildcard: None,
                    left_curly_bracket,
                    right_curly_bracket: self.eat_token(TokenType::RightCurlyBracket, context)?,
                    key_value_pairs: vec![RecordKeyValue {
                        key: identifier.clone(),
                        value: Expression::Identifier(identifier.clone()),
                    }],
                })
            }
            _ => {
                let mut statements = statements;
                let right_curly_bracket = loop {
                    if let Some(right_curly_bracket) =
                        self.try_eat_token(TokenType::RightCurlyBracket)?
                    {
                        break right_curly_bracket;
                    } else {
                        statements.extend(self.parse_statement()?);
                    }
                };
                Ok(Expression::Block(Block::WithBrackets {
                    left_curly_bracket,
                    statements,
                    right_curly_bracket,
                }))
            }
        }
    }

    fn parse_record(
        &mut self,
        left_curly_bracket: Token,
        first_key_value: RecordKeyValue,
    ) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionRecord);
        let mut wildcard = None;
        let mut key_value_pairs: Vec<RecordKeyValue> = vec![first_key_value];
        let right_curly_bracket = loop {
            if self.try_eat_token(TokenType::Comma)?.is_none() {
                break self.eat_token(TokenType::RightCurlyBracket, context)?;
            }
            match self.next_meaningful_token()? {
                Some(
                    double_period @ Token {
                        token_type: TokenType::TriplePeriod,
                        ..
                    },
                ) => {
                    if wildcard.is_none() {
                        wildcard = Some(double_period)
                    } else {
                        return Err(ParseError {
                            context,
                            kind: ParseErrorKind::RecordWilcardCanOnlyAppearOnce {
                                position: double_period.position,
                            },
                        });
                    }
                }
                other => {
                    let key = Parser::validate_token(TokenType::Identifier, other, context)?;
                    let value = if self.try_eat_token(TokenType::Colon)?.is_some() {
                        self.parse_expression()?
                    } else {
                        Expression::Identifier(key.clone())
                    };

                    key_value_pairs.push(RecordKeyValue {
                        key: key.clone(),
                        value,
                    });
                }
            }
        };
        Ok(Expression::Record {
            wildcard,
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
        })
    }

    fn parse_switch_expression(&mut self, keyword_switch: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionSwitch);
        let expression = self.parse_expression()?;
        let left_curly_bracket = self.eat_token(TokenType::LeftCurlyBracket, context)?;
        let head = self.parse_switch_case(context)?;
        let mut tail = vec![];
        let (tail, right_curly_bracket) = loop {
            if let Some(right_curly_bracket) = self.try_eat_token(TokenType::RightCurlyBracket)? {
                break (tail, right_curly_bracket);
            } else {
                tail.push(self.parse_switch_case(context)?)
            }
        };
        Ok(Expression::Switch {
            keyword_switch,
            expression: Box::new(expression),
            left_curly_bracket,
            cases: Box::new(NonEmpty { head, tail }),
            right_curly_bracket,
        })
    }

    fn parse_switch_case(
        &mut self,
        context: Option<ParseContext>,
    ) -> Result<SwitchCase, ParseError> {
        let keyword_case = self.eat_token(TokenType::KeywordCase, context)?;
        let pattern = self.parse_destructure_pattern()?;
        self.eat_token(TokenType::Colon, context)?;
        let body = self.parse_expression()?;
        Ok(SwitchCase {
            keyword_case,
            pattern,
            body: Box::new(body),
        })
    }

    fn parse_if_expression(&mut self, keyword_if: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::ExpressionIf);
        Ok(Expression::If {
            keyword_if,
            condition: Box::new(self.parse_expression()?),
            if_true: Box::new(self.parse_expression()?),
            keyword_else: self.eat_token(TokenType::KeywordElse, context)?,
            if_false: Box::new(self.parse_expression()?),
        })
    }

    fn parse_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        let simple_pattern = self.parse_simple_destructure_pattern()?;
        let mut tail_patterns = vec![];
        let tail_patterns = loop {
            if self.try_eat_token(TokenType::Pipe)?.is_some() {
                tail_patterns.push(self.parse_simple_destructure_pattern()?)
            } else {
                break tail_patterns;
            }
        };

        if tail_patterns.is_empty() {
            Ok(simple_pattern)
        } else {
            Ok(DestructurePattern::Or(Box::new(NonEmpty {
                head: simple_pattern,
                tail: tail_patterns,
            })))
        }
    }

    /// Simple destructure pattern means all kinds of destructure pattern except for OR patterns
    /// OR patterns means `P1 | P2 | P3`
    fn parse_simple_destructure_pattern(&mut self) -> Result<DestructurePattern, ParseError> {
        if let Some(token) = self.next_meaningful_token()? {
            match token.token_type {
                TokenType::Tag => Ok(DestructurePattern::EnumConstructor {
                    name: token,
                    payload: {
                        match self.peek_next_meaningful_token()? {
                            Some(Token {
                                token_type: TokenType::LeftParenthesis,
                                ..
                            }) => {
                                let context = Some(ParseContext::PatternEnum);
                                let left_parenthesis =
                                    self.eat_token(TokenType::LeftParenthesis, context)?;
                                let pattern = self.parse_destructure_pattern()?;
                                let right_parenthesis =
                                    self.eat_token(TokenType::RightParenthesis, context)?;
                                Some(Box::new(DestructurePatternEnumConstructorPayload {
                                    left_parenthesis,
                                    pattern,
                                    right_parenthesis,
                                }))
                            }
                            _ => None,
                        }
                    },
                }),
                TokenType::Identifier => Ok(DestructurePattern::Identifier(token)),
                TokenType::Underscore => Ok(DestructurePattern::Underscore(token)),
                TokenType::LeftCurlyBracket => {
                    let context = Some(ParseContext::PatternRecord);
                    let mut wildcard = None;
                    let mut key_value_pairs: Vec<DestructuredRecordKeyValue> = Vec::new();
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
                                    token_type: TokenType::TriplePeriod,
                                    ..
                                },
                            ) => wildcard = Some(double_period),
                            other => {
                                let key =
                                    Parser::validate_token(TokenType::Identifier, other, context)?;
                                let as_value = if self.try_eat_token(TokenType::Colon)?.is_some() {
                                    Some(self.parse_destructure_pattern()?)
                                } else {
                                    None
                                };
                                key_value_pairs.push(DestructuredRecordKeyValue { key, as_value });
                                if self.try_eat_token(TokenType::Comma)?.is_none() {
                                    break self.eat_token(TokenType::RightCurlyBracket, context)?;
                                }
                            }
                        }
                    };
                    Ok(DestructurePattern::Record {
                        wildcard,
                        left_curly_bracket: token,
                        key_value_pairs,
                        right_curly_bracket,
                    })
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
                TokenType::KeywordTrue => Ok(DestructurePattern::Boolean { token, value: true }),
                TokenType::KeywordFalse => Ok(DestructurePattern::Boolean {
                    token,
                    value: false,
                }),
                TokenType::KeywordNull => Ok(DestructurePattern::Null(token)),
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
                        let spread_token = self.eat_token(TokenType::TriplePeriod, context)?;
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

    fn parse_lambda(&mut self, left_curly_bracket: Token) -> Result<Expression, ParseError> {
        let context = Some(ParseContext::Lambda);
        let (branches, right_curly_bracket) =
            if let Some(pipe) = self.try_eat_token(TokenType::Pipe)? {
                let head_branch = self.parse_function_branch(pipe)?;
                let mut tail_branches = vec![];
                loop {
                    if let Some(right_curly_bracket) =
                        self.try_eat_token(TokenType::RightCurlyBracket)?
                    {
                        break (
                            NonEmpty {
                                head: head_branch,
                                tail: tail_branches,
                            },
                            right_curly_bracket,
                        );
                    } else {
                        let pipe = self.eat_token(TokenType::Pipe, context)?;
                        tail_branches.push(self.parse_function_branch(pipe)?)
                    }
                }
            } else {
                (
                    NonEmpty {
                        head: self.parse_function_branch(left_curly_bracket.clone())?,
                        tail: vec![],
                    },
                    self.eat_token(TokenType::RightCurlyBracket, context)?,
                )
            };
        Ok(Expression::Lambda(Box::new(Lambda {
            left_curly_bracket,
            branches,
            right_curly_bracket,
        })))
    }

    fn parse_function_branch(&mut self, start_token: Token) -> Result<FunctionBranch, ParseError> {
        let head_parameter = self.parse_destructure_pattern()?;
        let tail_parameters = loop {
            let mut tail_parameters = vec![];
            if self.try_eat_token(TokenType::ArrowRight)?.is_some() {
                break tail_parameters;
            } else {
                tail_parameters.push(self.parse_destructure_pattern()?)
            }
        };
        let body = self.parse_expression()?;
        Ok(FunctionBranch {
            start_token,
            parameters: NonEmpty {
                head: head_parameter,
                tail: tail_parameters,
            },
            body: Box::new(body),
        })
    }

    fn try_parse_function_call(&mut self, previous: Expression) -> Result<Expression, ParseError> {
        if self.next_token_is_terminating()? {
            return Ok(previous);
        }

        let next = self.parse_simple_expression()?;
        let previous = match &next {
            // TODO: handle operator

            // Postfix or infix function call
            Expression::Identifier(_) => Expression::FunctionCall(Box::new(FunctionCall {
                function: Box::new(next),
                first_argument: Box::new(previous),
                rest_arguments: self.parse_function_call_rest_arguments()?,
                type_arguments: None,
            })),

            // Prefix function call
            _ => Expression::FunctionCall(Box::new(FunctionCall {
                function: Box::new(previous),
                first_argument: Box::new(next),
                rest_arguments: self.parse_function_call_rest_arguments()?,
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
                    | TokenType::KeywordEnum
                    | TokenType::KeywordEntry
            )),
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
        Expression::Null(token) => Ok(DestructurePattern::Null(token)),
        Expression::Record {
            wildcard,
            left_curly_bracket,
            right_curly_bracket,
            key_value_pairs,
        } => Ok(DestructurePattern::Record {
            wildcard,
            left_curly_bracket,
            right_curly_bracket,
            key_value_pairs: key_value_pairs
                .into_iter()
                .map(|pair| {
                    Ok(DestructuredRecordKeyValue {
                        key: pair.key,
                        as_value: Some(convert_expression_to_pattern(
                            pair.value,
                            Some(ParseContext::ExpressionFunction),
                        )?),
                    })
                })
                .collect::<Result<Vec<DestructuredRecordKeyValue>, ParseError>>()?,
        }),
        Expression::EnumConstructor { name, payload } => Ok(DestructurePattern::EnumConstructor {
            name,
            payload: match payload {
                None => None,
                Some(payload) => Some(Box::new(DestructurePatternEnumConstructorPayload {
                    left_parenthesis: payload.left_parenthesis,
                    pattern: convert_expression_to_pattern(
                        payload.expression,
                        Some(ParseContext::ExpressionFunction),
                    )?,
                    right_parenthesis: payload.right_parenthesis,
                })),
            },
        }),
        Expression::Boolean { token, value } => Ok(DestructurePattern::Boolean { token, value }),
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
        | Expression::Quoted { .. }
        | Expression::Lambda(_)
        | Expression::ArrowFunction(_)
        | Expression::Lambda(_)
        | Expression::FunctionCall(_)
        | Expression::RecordAccess { .. }
        | Expression::RecordUpdate { .. }
        | Expression::With(_)
        | Expression::If { .. }
        | Expression::Switch { .. }
        | Expression::Block(_)
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
