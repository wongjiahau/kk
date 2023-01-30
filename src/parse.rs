use crate::formatter::ToDoc;
use crate::module::Access;
use crate::parse_simple;
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

        Ok(pulldown_cmark::Parser::new(self.content.as_str())
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
                let node = parse_simple::Parser::new(&mut Tokenizer::with_offset(
                    raw_code,
                    self.start_quotes.last().index + range.start + 1,
                ))
                .parse_high_precedence_node()?
                .expand_macro();
                match node {
                    Some(node) => Ok(Some(node.to_expression(&mut ParserState::new())?)),
                    None => Ok(None),
                }
            })
            .collect::<Result<Vec<_>, ParseError>>()
            .into_iter()
            .flatten()
            .filter_map(|node| node)
            .collect())
    }
}

struct ParserState {
    temporary_variable_index: usize,
}

impl ParserState {
    fn new() -> Self {
        ParserState {
            temporary_variable_index: 0,
        }
    }
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
                key_value_pairs,
                prefix,
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
                        prefix,
                        wildcard,
                        key_value_pairs,
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

impl MacroExpandedList {
    fn to_desctuctured_record_key_value(&self) -> Result<DestructuredRecordKeyValue, ParseError> {
        let key = self.head().to_identifier(None)?;
        let pattern = self.tail().get(0).into_node(key.position)?.to_pattern()?;
        self.tail().get(1).should_be_none()?;
        Ok(DestructuredRecordKeyValue {
            key,
            type_annotation: None,
            as_value: Some(pattern),
        })
    }
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
                let constructors = self.skip(2)?.into_enum_constructor_definitions(state)?;
                Ok(Statement::Enum(EnumStatement {
                    access: Access::Protected,
                    keyword_enum: first,
                    name,
                    type_variables_declaration: None,
                    constructors,
                }))
            }
            "defn" => {
                let signature = self.get(1)?.into_list()?.to_function_signature(None)?;
                let body = self.get(2)?.to_expression(state)?;
                self.nodes.get(3).should_be_none()?;
                Ok(Statement::Let(LetStatement {
                    access,
                    keyword_let: first,
                    name: signature.name.clone(),
                    doc_string: None,
                    type_annotation: signature.to_function_type_annotation(),
                    expression: signature.to_function(body),
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
            Node::Literal(literal) => Some(MacroExpandedNode::Literal(literal.clone())),
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
                // list.get(1)?.to_identifier(Some("="))?;
                let value = list.get(1)?.to_expression(state)?;
                list.nodes.get(2).should_be_none()?;
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
                let pattern = nodes.head.to_pattern()?;
                let expression = nodes
                    .tail
                    .get(0)
                    .into_node(pattern.position())?
                    .to_expression(state)?;
                nodes.tail.get(1).should_be_none()?;
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
            simple_ast::MacroExpandedNode::List(list) => list.to_enum_constructor_definition(),
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
                        "%" => Ok(TypeAnnotation::Record {
                            prefix: identifier.clone(),
                            key_type_annotation_pairs: list
                                .tail()
                                .into_iter()
                                .map(|node| {
                                    let list = node.into_list()?;
                                    let key = list.head().to_identifier(None)?;
                                    let type_annotation = list
                                        .tail()
                                        .get(0)
                                        .into_node(key.position)?
                                        .to_type_annotation()?;
                                    Ok((key, type_annotation))
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                        }),
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
            MacroExpandedNode::List(list) => list.to_desctuctured_record_key_value(),
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
    fn to_enum_constructor_definition(&self) -> Result<EnumConstructorDefinition, ParseError> {
        let name = self.head().to_identifier(None)?;
        Ok(EnumConstructorDefinition {
            name,
            payload: match self.nodes.get(1) {
                None => None,
                // TODO: auto-wrap payload as tuple type annotations
                // So that we can have multi-argument constructor
                Some(node) => Some(Box::new(EnumConstructorDefinitionPayload {
                    type_annotation: node.to_type_annotation()?,
                })),
            },
        })
    }
    fn to_function_signature(
        &self,
        type_variables_declaration: Option<TypeVariablesDeclaration>,
    ) -> Result<FunctionSignature, ParseError> {
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
                self.skip(2)?
                    .to_function_signature(Some(type_variables_declaration))
            }

            // Infix-style function signature
            MacroExpandedNode::List(another_list) => {
                let first_parameter = another_list.to_function_parameter()?;
                let name = list
                    .get(1)
                    .cloned()
                    .into_node(first_parameter.position())?
                    .to_identifier(None)?;

                let parameters_tail = list
                    .into_iter()
                    .skip(2)
                    .map(|node| node.to_function_parameter())
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(FunctionSignature {
                    name,
                    type_variables_declaration,
                    parameters: NonEmpty {
                        head: first_parameter,
                        tail: parameters_tail,
                    },
                    return_type,
                })
            }

            // Prefix-style function signature
            _ => Ok(FunctionSignature {
                type_variables_declaration,
                name: list
                    .get(0)
                    .cloned()
                    .into_node(first.position())?
                    .to_identifier(None)?,
                parameters: NonEmpty {
                    head: list
                        .get(1)
                        .cloned()
                        .into_node(first.position())?
                        .to_function_parameter()?,
                    tail: list
                        .iter()
                        .skip(2)
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

            // Dot chaining
            // 2 cases:
            //
            //   (. a b) = (b a)
            //   (. a (b c)) = (b a c)
            //
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "." => {
                let first = tail.get(0).into_node(self.head().position())?;
                let second = tail.get(1).into_node(self.head().position())?;
                tail.get(2).should_be_none()?;
                let node = match second {
                    MacroExpandedNode::List(list) => MacroExpandedNode::List(MacroExpandedList {
                        nodes: Box::new(NonEmpty {
                            head: list.head().clone(),
                            tail: vec![first]
                                .into_iter()
                                .chain(list.tail().into_iter())
                                .collect(),
                        }),
                    }),
                    _ => MacroExpandedNode::List(MacroExpandedList {
                        nodes: Box::new(NonEmpty {
                            head: second,
                            tail: vec![first],
                        }),
                    }),
                };
                node.to_expression(state)
            }

            // Record
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "%" => {
                Ok(Expression::Record {
                    prefix: id.clone(),
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
                                // list.get(2)?.to_identifier(Some("="))?;
                                let right = list.get(2)?.to_expression(state)?;
                                list.nodes.get(3).should_be_none()?;
                                (left, right)
                            }
                            _ => {
                                let left = DestructurePattern::Underscore(Token {
                                    position: current.position(),
                                    ..Token::dummy()
                                });
                                let right = current.to_expression(state)?;
                                (left, right)
                            }
                        };
                        Ok(Expression::Let {
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
        use simple_ast::Literal;
        match self.head() {
            // Or pattern
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "|" => Ok(
                DestructurePattern::Or(Box::new(match self.tail().split_first() {
                    None => todo!("parse error"),
                    Some((head, tail)) => NonEmpty {
                        head: head.to_pattern()?,
                        tail: tail
                            .into_iter()
                            .map(|node| node.to_pattern())
                            .collect::<Result<Vec<_>, _>>()?,
                    },
                })),
            ),
            // Record
            MacroExpandedNode::Literal(Literal::Identifier(id)) if id.representation == "%" => {
                Ok(DestructurePattern::Record {
                    prefix: id.clone(),
                    wildcard: None,
                    key_value_pairs: self
                        .tail()
                        .into_iter()
                        .map(|node| node.to_desctuctured_record_key_value())
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }
            _ => match self.tail().split_first() {
                None => self.head().to_pattern(),
                Some((second, [])) => Ok(DestructurePattern::EnumConstructor {
                    name: self.head().to_identifier(None)?,
                    payload: Some(Box::new(second.to_pattern()?)),
                }),
                Some((second, tail)) => todo!("{}", self.to_pretty()),
            },
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
                        first_argument: MacroExpandedNode,
                        second_argument: MacroExpandedNode,
                        tail: &[MacroExpandedNode],
                    ) -> MacroExpandedNode {
                        match tail.split_first() {
                            None => MacroExpandedNode::List(MacroExpandedList {
                                nodes: Box::new(NonEmpty {
                                    head: operator,
                                    tail: vec![first_argument, second_argument],
                                }),
                            }),
                            Some((third_argument, tail)) => {
                                MacroExpandedNode::List(MacroExpandedList {
                                    nodes: Box::new(NonEmpty {
                                        head: operator.clone(),
                                        tail: vec![
                                            first_argument,
                                            right_associativitify(
                                                operator,
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
            BracketKind::Square => match nodes.split_first() {
                Some((operator, [init @ .., second_last_argument, last_argument])) => {
                    fn left_associativitify(
                        operator: MacroExpandedNode,
                        last_argument: MacroExpandedNode,
                        second_last_argument: MacroExpandedNode,
                        init: &[MacroExpandedNode],
                    ) -> MacroExpandedNode {
                        match init.split_last() {
                            None => MacroExpandedNode::List(MacroExpandedList {
                                nodes: Box::new(NonEmpty {
                                    head: operator.clone(),
                                    tail: vec![second_last_argument, last_argument],
                                }),
                            }),
                            Some((third_last_argument, init)) => {
                                MacroExpandedNode::List(MacroExpandedList {
                                    nodes: Box::new(NonEmpty {
                                        head: operator.clone(),
                                        tail: vec![
                                            left_associativitify(
                                                operator,
                                                second_last_argument,
                                                third_last_argument.clone(),
                                                init,
                                            ),
                                            last_argument,
                                        ],
                                    }),
                                })
                            }
                        }
                    }
                    left_associativitify(
                        operator.clone(),
                        last_argument.clone(),
                        second_last_argument.clone(),
                        init,
                    )
                }
                Some((operator, [first_argument])) => MacroExpandedNode::List(MacroExpandedList {
                    nodes: Box::new(NonEmpty {
                        head: operator.clone(),
                        tail: vec![first_argument.clone()],
                    }),
                }),
                Some((operator, [])) => operator.clone(),
                None => MacroExpandedNode::Unit {
                    bracket: self.bracket.clone(),
                },
            },
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
    fn to_function(&self, body: Expression) -> Expression {
        fn to_function(parameters: Vec<DestructurePattern>, body: Expression) -> Expression {
            match parameters.split_last() {
                None => body,
                Some((last, init)) => to_function(
                    init.to_vec(),
                    Expression::Function(Box::new(Function {
                        branches: NonEmpty {
                            head: FunctionBranch {
                                parameter: Box::new(last.clone()),
                                body: Box::new(body),
                            },
                            tail: vec![],
                        },
                    })),
                ),
            }
        }
        to_function(
            self.parameters
                .to_vector()
                .into_iter()
                .map(|parameter| parameter.pattern.clone())
                .collect(),
            body,
        )
    }
    fn to_function_type_annotation(&self) -> TypeAnnotation {
        fn to_function_type_annotation(
            parameter_types: Vec<TypeAnnotation>,
            return_type: TypeAnnotation,
        ) -> TypeAnnotation {
            match parameter_types.split_last() {
                Some((last, init)) => to_function_type_annotation(
                    init.to_vec(),
                    TypeAnnotation::Function(FunctionTypeAnnotation {
                        parameter: Box::new(last.clone()),
                        return_type: Box::new(return_type),
                        type_constraints: None,
                    }),
                ),
                None => return_type,
            }
        }
        let function_type_annotation = to_function_type_annotation(
            self.parameters
                .to_vector()
                .into_iter()
                .map(|parameter| parameter.type_annotation.clone())
                .collect(),
            self.return_type.clone(),
        );
        match &self.type_variables_declaration {
            None => function_type_annotation,
            Some(type_variables) => TypeAnnotation::Scheme {
                type_variables: type_variables.clone(),
                type_annotation: Box::new(function_type_annotation),
            },
        }
    }
}
