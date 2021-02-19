use crate::{
    compile::{CompileError, CompileErrorKind},
    non_empty::NonEmpty,
    parse::Parser,
    tokenize::tokenize,
};

use crate::ast::*;
use crate::environment::*;
use crate::pattern::*;
use crate::typechecked_ast::*;
use relative_path::RelativePath;
use std::fs;
use std::iter;
use std::{collections::HashSet, path::Path};

#[derive(Debug, Clone)]
pub struct Program {
    pub source: Source,
    pub code: String,

    /// This is used for checking circular references
    pub import_relations: Vec<ImportRelation>,
}

#[derive(Debug, Clone)]
pub struct ImportRelation {
    pub importer_path: String,
    pub importee_path: String,
}

pub struct UnifyProgramResult {
    pub statements: Vec<TypecheckedStatement>,
    pub environment: Environment,
}

/// `starting_symbol_uid` is needed to make sure each symbol has a UID that is unique across different modules
pub fn unify_statements(
    program: Program,
    statements: Vec<Statement>,
    starting_symbol_uid: usize,
) -> Result<UnifyProgramResult, CompileError> {
    // 1. TODO: Populate environment with imported symbols
    let mut environment: Environment = Environment::new(program, starting_symbol_uid);

    // 2. Type check this program
    let statements = statements
        .into_iter()
        .map(|statement| infer_statement(&mut environment, statement))
        .collect::<Result<Vec<Vec<TypecheckedStatement>>, CompileError>>()?
        .into_iter()
        .flatten()
        .collect();
    Ok(UnifyProgramResult {
        statements,
        environment,
    })
}

#[derive(Debug)]
pub struct UnifyError {
    pub position: Position,
    pub kind: UnifyErrorKind,
}

impl UnifyError {
    pub fn into_compile_error(self, program: Program) -> CompileError {
        CompileError {
            program,
            kind: CompileErrorKind::UnifyError(Box::new(self)),
        }
    }
}

#[derive(Debug)]
pub enum UnifyErrorKind {
    CyclicDependency {
        import_relations: Vec<ImportRelation>,
    },
    CannotImportPrivateSymbol,
    UnknownImportedName,
    ErrorneousImportPath {
        extra_information: String,
    },
    ConflictingFunctionDefinition {
        function_name: String,

        /// First parameter type of existing function
        existing_first_parameter_type: Type,

        /// First parameter type of the new function to be created
        new_first_parameter_type: Type,
        first_declared_at: Position,
    },
    AmbiguousFunction {
        available_function_signatures: Vec<FunctionSignature>,
    },
    NoMatchingFunction {
        actual_first_argument_type: Type,
        expected_first_argument_types: Vec<Type>,
    },
    AmbiguousConstructorUsage {
        constructor_name: String,
        possible_enum_names: NonEmpty<String>,
    },
    UnnecessaryTypeAnnotation {
        expected_type: Type,
    },
    DuplicatedRecordKey,
    InfiniteTypeDetected {
        type_variable_name: String,
        in_type: Type,
    },
    DoBodyMustHaveNullType {
        actual_type: Type,
    },
    NoSuchPropertyOnThisRecord {
        expected_keys: Vec<String>,
    },
    CannotAccessPropertyOfNonRecord {
        actual_type: Type,
    },
    CannotPerformRecordUpdateOnNonRecord {
        actual_type: Type,
    },
    MissingCases(Vec<ExpandablePattern>),
    ThisEnumConstructorDoesNotRequirePayload,
    ThisEnumConstructorRequiresPaylod {
        payload_type: Type,
    },
    UnreachableCase,
    TypeArgumentsLengthMismatch {
        actual_length: usize,
        expected_type_parameter_names: Vec<String>,
    },
    RecordExtraneousKey {
        expected_keys: Vec<String>,
    },
    RecordMissingKeys {
        missing_keys: Vec<String>,
    },
    CannotInvokeNonFunction {
        actual_type: Type,
    },
    DuplicatedIdentifier {
        name: String,
        first_declared_at: Position,
        then_declared_at: Position,
    },
    InvalidFunctionArgumentLength {
        expected_length: usize,
        actual_length: usize,
    },
    UnknownTypeSymbol,
    UnknownValueSymbol,
    UnknownEnumConstructor,
    TypeMismatch {
        expected_type: Type,
        actual_type: Type,
    },
}

pub fn infer_statement(
    environment: &mut Environment,
    statement: Statement,
) -> Result<Vec<TypecheckedStatement>, CompileError> {
    match statement {
        Statement::Do(do_statement) => match infer_do_statement(environment, do_statement) {
            Ok(statement) => Ok(vec![statement]),
            Err(unify_error) => Err(unify_error.into_compile_error(environment.program.clone())),
        },
        Statement::Let(let_statement) => match infer_let_statement(environment, let_statement) {
            Ok(statement) => Ok(vec![statement]),
            Err(unify_error) => Err(unify_error.into_compile_error(environment.program.clone())),
        },
        Statement::Type(type_statement) => {
            match infer_type_statement(environment, type_statement) {
                Ok(()) => Ok(vec![]),
                Err(unify_error) => {
                    Err(unify_error.into_compile_error(environment.program.clone()))
                }
            }
        }
        Statement::Enum(enum_statement) => {
            match infer_enum_statement(environment, enum_statement) {
                Ok(()) => Ok(vec![]),
                Err(unify_error) => {
                    Err(unify_error.into_compile_error(environment.program.clone()))
                }
            }
        }
        Statement::Import(import_statement) => {
            infer_import_statement(environment, import_statement)
        }
    }
}

pub fn infer_import_statement(
    environment: &mut Environment,
    ImportStatement {
        url,
        imported_names,
        ..
    }: ImportStatement,
) -> Result<Vec<TypecheckedStatement>, CompileError> {
    let current_path = environment.source().path();
    let import_path = url.representation.trim_matches('"');
    let path = RelativePath::new(&current_path)
        .parent()
        .expect("Should always have parent")
        .join(RelativePath::new(import_path))
        .normalize();
    let path_string = path.to_string();
    if environment
        .program
        .import_relations
        .iter()
        .any(|relation| path_string == relation.importer_path)
    {
        return Err(UnifyError {
            position: url.position,
            kind: UnifyErrorKind::CyclicDependency {
                import_relations: {
                    let mut relations = environment.program.import_relations.clone();
                    relations.push(ImportRelation {
                        importer_path: environment.source().path(),
                        importee_path: path_string,
                    });
                    relations
                },
            },
        }
        .into_compile_error(environment.program.clone()));
    }
    match fs::read_to_string(path.to_path(Path::new("."))) {
        Ok(code) => {
            // Compile the imported file
            let source = Source::File {
                path: path_string.clone(),
            };
            let program = Program {
                source,
                code: code.clone(),
                import_relations: {
                    let mut importer_paths = environment.program.import_relations.clone();
                    importer_paths.push(ImportRelation {
                        importer_path: environment.program.source.path(),
                        importee_path: path_string,
                    });
                    importer_paths
                },
            };
            let tokens = match tokenize(code) {
                Ok(tokens) => Ok(tokens),
                Err(tokenize_error) => Err(CompileError {
                    program: program.clone(),
                    kind: CompileErrorKind::TokenizeError(tokenize_error),
                }),
            }?;
            let statements = match Parser::parse(tokens) {
                Ok(statements) => Ok(statements),
                Err(parse_error) => Err(CompileError {
                    program: program.clone(),
                    kind: CompileErrorKind::ParseError(Box::new(parse_error)),
                }),
            }?;

            // Check whether each imported name exists and is exported in this program
            let imported =
                unify_statements(program, statements, environment.get_next_symbol_uid())?;
            let statements = imported_names
                .into_vector()
                .into_iter()
                .map(|imported_name| {
                    let matching_symbol_entries = imported
                        .environment
                        .get_all_matching_symbols(&imported_name.name);

                    if matching_symbol_entries.is_empty() {
                        Err(UnifyError {
                            position: imported_name.name.position,
                            kind: UnifyErrorKind::UnknownImportedName,
                        }
                        .into_compile_error(environment.program.clone()))
                    } else {
                        let name = match &imported_name.alias_as {
                            Some(name) => name,
                            None => &imported_name.name,
                        };

                        // Insert the matching value symbol into the current environment
                        let statements = matching_symbol_entries
                            .iter()
                            .map(|entry| {
                                if !entry.symbol.meta.exported {
                                    return Err(UnifyError {
                                        position: imported_name.name.position,
                                        kind: UnifyErrorKind::CannotImportPrivateSymbol,
                                    }
                                    .into_compile_error(environment.program.clone()));
                                }
                                match environment.insert_symbol(
                                    Some(entry.uid),
                                    Symbol {
                                        meta: SymbolMeta {
                                            name: name.clone(),
                                            ..entry.symbol.meta
                                        },
                                        kind: entry.symbol.kind.clone(),
                                    },
                                ) {
                                    Ok(uid) => {
                                        // only insert new typechecked statement if import alias is defined
                                        match imported_name.clone().alias_as {
                                            Some(alias_as) => match entry.symbol.kind {
                                                SymbolKind::Value(_) => {
                                                    Ok(vec![TypecheckedStatement::Let {
                                                        left: Variable {
                                                            uid,
                                                            representation: alias_as.representation,
                                                        },
                                                        right: TypecheckedExpression::Variable(
                                                            Variable {
                                                                uid: entry.uid,
                                                                representation: entry
                                                                    .symbol
                                                                    .meta
                                                                    .name
                                                                    .representation
                                                                    .clone(),
                                                            },
                                                        ),
                                                    }])
                                                }
                                                _ => Ok(vec![]),
                                            },
                                            None => Ok(vec![]),
                                        }
                                    }
                                    Err(unify_error) => {
                                        Err(unify_error
                                            .into_compile_error(environment.program.clone()))
                                    }
                                }
                            })
                            .collect::<Result<Vec<Vec<TypecheckedStatement>>, CompileError>>()?;

                        Ok(statements)
                    }
                })
                .collect::<Result<Vec<Vec<Vec<TypecheckedStatement>>>, CompileError>>()?;

            // After importing, we need to increment the `current_uid` of the currrent environment
            // to the biggest `current_uid` of the imported environment
            // so that UID uniqueness can be maintained across different modules
            // this uniquenss is important for transpilation, so that the generated code
            // will not contain both variables with the same name
            environment.set_current_uid(imported.environment.get_current_uid());

            Ok(imported
                .statements
                .into_iter()
                .chain(statements.into_iter().flatten().flatten())
                .collect())
        }
        Err(error) => Err(UnifyError {
            position: url.position,
            kind: UnifyErrorKind::ErrorneousImportPath {
                extra_information: format!("{}", error),
            },
        }
        .into_compile_error(environment.program.clone())),
    }
}

pub fn infer_enum_statement(
    environment: &mut Environment,
    EnumStatement {
        name,
        constructors,
        type_variables,
        keyword_export,
        ..
    }: EnumStatement,
) -> Result<(), UnifyError> {
    // 1. Add this enum into environment first, to allow recursive definition
    let enum_uid = environment.get_next_symbol_uid();
    let enum_name = name.representation.clone();
    let enum_type = Type::Named {
        symbol_uid: enum_uid,
        name: enum_name.clone(),
        type_arguments: type_variables
            .clone()
            .into_iter()
            .map(|type_variable| {
                (
                    type_variable.representation.clone(),
                    Type::ImplicitTypeVariable {
                        name: type_variable.representation,
                    },
                )
            })
            .collect(),
    };
    let type_variable_names = type_variables
        .clone()
        .into_iter()
        .map(|type_variable| type_variable.representation)
        .collect::<Vec<String>>();

    environment.insert_symbol(
        Some(enum_uid),
        Symbol {
            meta: SymbolMeta {
                name,
                exported: keyword_export.is_some(),
            },
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: type_variable_names.clone(),
                    type_value: enum_type,
                },
            }),
        },
    )?;

    // 2. Populate type variables into current environment
    environment.step_into_new_child_scope();
    for type_variable in type_variables.clone() {
        environment.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: type_variable.clone(),
                    exported: false,
                },
                kind: SymbolKind::Type(TypeSymbol {
                    type_scheme: TypeScheme {
                        type_value: Type::ImplicitTypeVariable {
                            name: type_variable.clone().representation,
                        },
                        type_variables: vec![],
                    },
                }),
            },
        )?;
    }

    // 3. Add each tags into the enum namespace
    let constructor_symbols = constructors
        .iter()
        .map(|constructor| {
            Ok(Symbol {
                meta: SymbolMeta {
                    name: constructor.name.clone(),
                    exported: keyword_export.is_some(),
                },
                kind: SymbolKind::EnumConstructor(EnumConstructorSymbol {
                    enum_uid,
                    enum_name: enum_name.clone(),
                    constructor_name: constructor.name.representation.clone(),
                    type_variables: type_variable_names.clone(),
                    payload: match &constructor.payload {
                        None => None,
                        Some(payload) => {
                            // validate type annotation
                            let payload_type_value =
                                type_annotation_to_type(environment, &payload.type_annotation)?;
                            Some(payload_type_value)
                        }
                    },
                }),
            })
        })
        .collect::<Result<Vec<Symbol>, UnifyError>>()?;

    environment.step_out_to_parent_scope();

    constructor_symbols
        .into_iter()
        .map(|constructor_symbol| environment.insert_symbol(None, constructor_symbol))
        .collect::<Result<Vec<_>, UnifyError>>()?;

    Ok(())
}

pub fn infer_type_statement(
    environment: &mut Environment,
    TypeStatement {
        keyword_export,
        left,
        right,
        type_variables,
        ..
    }: TypeStatement,
) -> Result<(), UnifyError> {
    environment.step_into_new_child_scope();
    // 1. Populate type variables into current environment
    environment.step_into_new_child_scope();
    for type_variable in type_variables.clone() {
        environment.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: type_variable.clone(),
                    exported: false,
                },
                kind: SymbolKind::Type(TypeSymbol {
                    type_scheme: TypeScheme {
                        type_value: Type::ImplicitTypeVariable {
                            name: type_variable.clone().representation,
                        },
                        type_variables: vec![],
                    },
                }),
            },
        )?;
    }

    // 2. verify type declaration
    let type_value = type_annotation_to_type(environment, &right)?;

    // 3. Add this type symbol into this environment
    environment.step_out_to_parent_scope();
    environment.insert_symbol(
        None,
        Symbol {
            meta: SymbolMeta {
                name: left,
                exported: keyword_export.is_some(),
            },
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: type_variables
                        .into_iter()
                        .map(|type_variable| type_variable.representation)
                        .collect(),
                    type_value,
                },
            }),
        },
    )?;

    Ok(())
}

pub fn infer_let_statement(
    environment: &mut Environment,
    LetStatement {
        keyword_export,
        left,
        type_variables,
        right,
        type_annotation,
        ..
    }: LetStatement,
) -> Result<TypecheckedStatement, UnifyError> {
    // 0. Populate type variables
    environment.step_into_new_child_scope();
    type_variables
        .iter()
        .map(|type_variable| environment.insert_explicit_type_variable(type_variable))
        .collect::<Result<Vec<_>, UnifyError>>()?;

    // 1. Add itself into current scope to enable recursive definition if type annotation
    //    is present, and is type of function
    let type_annotation_type = optional_type_annotation_to_type(environment, &type_annotation)?;

    let uid = match type_annotation_type.clone() {
        Some(Type::Function(function_type)) => {
            let (uid, _) = environment
                .insert_value_symbol_with_type(&left, Some(Type::Function(function_type)))?;
            Ok(Some(uid))
        }
        _ => Ok(None),
    }?;

    // 2. Check if right matches type annotation
    let right = infer_expression_type(environment, type_annotation_type, &right)?;

    // 3. Rewrite explicit type variable as implicit type variables
    let explicit_type_variable_names = type_variables
        .iter()
        .map(|token| token.representation.clone())
        .collect();

    let right_type = rewrite_explicit_type_variables_as_implicit(
        right.type_value,
        &explicit_type_variable_names,
    );

    // 4. Generalize if possible
    let right_type_scheme = generalize_type(right_type);

    // 5. Add this variable into environment
    //    Note that we have to use back the same uid if this is a recursive function
    environment.step_out_to_parent_scope();
    let uid = environment.insert_symbol(
        uid,
        Symbol {
            meta: SymbolMeta {
                name: left.clone(),
                exported: keyword_export.is_some(),
            },
            kind: SymbolKind::Value(ValueSymbol {
                type_scheme: right_type_scheme,
            }),
        },
    )?;

    Ok(TypecheckedStatement::Let {
        left: Variable {
            uid,
            representation: left.representation,
        },
        right: right.expression,
    })
}

pub fn infer_do_statement(
    environment: &mut Environment,
    do_statement: DoStatement,
) -> Result<TypecheckedStatement, UnifyError> {
    match infer_expression_type(
        environment,
        // NOTE: we could have pass in Some(Type::Null) instead of None here.
        //       The reason is because we want to provide better error message with context.
        //       So instead of saying expected type is null, we tell the user
        //       that the body of a `do` statement must be null
        None,
        &do_statement.expression,
    )? {
        InferExpressionResult {
            expression,
            type_value: Type::Null,
        } => Ok(TypecheckedStatement::Do { expression }),
        InferExpressionResult { type_value, .. } => Err(UnifyError {
            position: get_expression_position(&do_statement.expression),
            kind: UnifyErrorKind::DoBodyMustHaveNullType {
                actual_type: type_value,
            },
        }),
    }
}

pub fn rewrite_explicit_type_variables_as_implicit(
    type_value: Type,
    explicit_type_variable_names: &HashSet<String>,
) -> Type {
    match type_value {
        Type::ExplicitTypeVariable { name } => {
            if explicit_type_variable_names.get(&name).is_some() {
                Type::ImplicitTypeVariable { name }
            } else {
                Type::ExplicitTypeVariable { name }
            }
        }
        t @ Type::Underscore => t,
        t @ Type::Float => t,
        t @ Type::Integer => t,
        t @ Type::Null => t,
        t @ Type::Boolean => t,
        t @ Type::String => t,
        t @ Type::Character => t,
        Type::ImplicitTypeVariable { name } => Type::ImplicitTypeVariable { name },
        Type::Tuple(types) => Type::Tuple(Box::new(types.map(|type_value| {
            rewrite_explicit_type_variables_as_implicit(type_value, explicit_type_variable_names)
        }))),
        Type::Array(element_type) => {
            Type::Array(Box::new(rewrite_explicit_type_variables_as_implicit(
                *element_type,
                explicit_type_variable_names,
            )))
        }
        Type::Named {
            name,
            symbol_uid,
            type_arguments,
        } => Type::Named {
            name,
            symbol_uid,
            type_arguments: type_arguments
                .into_iter()
                .map(|(name, type_value)| {
                    (
                        name,
                        rewrite_explicit_type_variables_as_implicit(
                            type_value,
                            explicit_type_variable_names,
                        ),
                    )
                })
                .collect(),
        },
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_explicit_type_variables_as_implicit(
                            type_value,
                            explicit_type_variable_names,
                        ),
                    )
                })
                .collect(),
        },
        Type::Function(FunctionType {
            parameters_types: arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            parameters_types: Box::new(arguments_types.map(|type_value| {
                rewrite_explicit_type_variables_as_implicit(
                    type_value,
                    explicit_type_variable_names,
                )
            })),
            return_type: Box::new(rewrite_explicit_type_variables_as_implicit(
                *return_type,
                explicit_type_variable_names,
            )),
        }),
    }
}

pub fn generalize_type(type_value: Type) -> TypeScheme {
    let type_variables = get_free_type_variables_in_type(&type_value);
    TypeScheme {
        type_variables: type_variables.into_iter().collect(),
        type_value,
    }
}

pub fn get_type_annotation_position(type_annotation: &TypeAnnotation) -> Position {
    match type_annotation {
        TypeAnnotation::Underscore(token) => token.position,
        TypeAnnotation::Function {
            start_token,
            return_type,
            ..
        } => join_position(
            start_token.position,
            get_type_annotation_position(return_type.as_ref()),
        ),
        TypeAnnotation::Array {
            left_square_bracket,
            right_square_bracket,
            ..
        } => join_position(left_square_bracket.position, right_square_bracket.position),
        TypeAnnotation::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        TypeAnnotation::Named {
            name,
            type_arguments,
        } => match type_arguments {
            None => join_position(name.position, name.position),
            Some(TypeArguments {
                right_angular_bracket,
                ..
            }) => join_position(name.position, right_angular_bracket.position),
        },
    }
}

pub fn join_position(start_position: Position, end_position: Position) -> Position {
    Position {
        line_start: start_position.line_start,
        column_start: start_position.column_start,
        line_end: end_position.line_end,
        column_end: end_position.column_end,
        character_index_start: start_position.character_index_start,
        character_index_end: end_position.character_index_end,
    }
}

pub fn get_destructure_pattern_position(destructure_pattern: &DestructurePattern) -> Position {
    match destructure_pattern {
        DestructurePattern::Infinite { token, .. }
        | DestructurePattern::Identifier(token)
        | DestructurePattern::Underscore(token)
        | DestructurePattern::Boolean { token, .. }
        | DestructurePattern::Null(token) => token.position,
        DestructurePattern::EnumConstructor { name, payload, .. } => match payload {
            None => name.position,
            Some(payload) => join_position(name.position, payload.right_parenthesis.position),
        },
        DestructurePattern::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        DestructurePattern::Array {
            left_square_bracket,
            right_square_bracket,
            ..
        } => join_position(left_square_bracket.position, right_square_bracket.position),
        DestructurePattern::Tuple(tuple) => match &tuple.parentheses {
            Some((left, right)) => join_position(left.position, right.position),
            None => join_position(
                get_destructure_pattern_position(tuple.values.first()),
                get_destructure_pattern_position(tuple.values.last()),
            ),
        },
    }
}

pub fn get_expression_position(expression_value: &Expression) -> Position {
    match expression_value {
        Expression::Array {
            left_square_bracket,
            right_square_bracket,
            ..
        } => join_position(left_square_bracket.position, right_square_bracket.position),
        Expression::String(token)
        | Expression::Character(token)
        | Expression::Float(token)
        | Expression::Integer(token)
        | Expression::Variable(token)
        | Expression::Null(token)
        | Expression::Boolean { token, .. } => token.position,
        Expression::EnumConstructor { name, payload, .. } => match payload {
            None => name.position,
            Some(payload) => join_position(name.position, payload.right_parenthesis.position),
        },
        Expression::RecordAccess {
            expression,
            property_name,
        } => join_position(get_expression_position(&expression), property_name.position),
        Expression::Record {
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => join_position(left_curly_bracket.position, right_curly_bracket.position),
        Expression::RecordUpdate {
            expression,
            right_curly_bracket,
            ..
        } => join_position(
            get_expression_position(expression.as_ref()),
            right_curly_bracket.position,
        ),
        Expression::FunctionCall(function_call) => {
            let first_argument = function_call.first_argument.as_ref();
            let start_position = get_expression_position(&first_argument);
            let end_position = match &function_call.rest_arguments {
                Some(FunctionCallRestArguments {
                    right_parenthesis, ..
                }) => right_parenthesis.position,
                None => function_call.function_name.position,
            };
            join_position(start_position, end_position)
        }
        Expression::Function(function) => {
            let start_position = function.branches.first().start_token.position;
            let end_position = get_expression_position(&function.branches.last().body);
            join_position(start_position, end_position)
        }
        Expression::Let {
            keyword_let,
            true_branch,
            ..
        } => join_position(keyword_let.position, get_expression_position(&true_branch)),
    }
}

pub fn try_unify_type(
    environment: &mut Environment,
    expected: Option<Type>,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match expected {
        None => Ok(actual.clone()),
        Some(expected) => unify_type(environment, &expected, actual, position),
    }
}

pub fn unify_type(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match unify_type_(environment, expected, actual, position) {
        Err(UnifyError {
            position,
            kind:
                UnifyErrorKind::TypeMismatch {
                    expected_type,
                    actual_type,
                },
        }) => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: environment.apply_subtitution_to_type(&expected_type),
                actual_type: environment.apply_subtitution_to_type(&actual_type),
            },
        }),
        other => other,
    }
}

pub fn unify_type_(
    environment: &mut Environment,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match (expected.clone(), actual.clone()) {
        (Type::Float, Type::Float) => Ok(Type::Float),
        (Type::Integer, Type::Integer) => Ok(Type::Integer),
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Character, Type::Character) => Ok(Type::Character),
        (Type::Null, Type::Null) => Ok(Type::Null),
        (
            Type::ExplicitTypeVariable {
                name: expected_name,
            },
            Type::ExplicitTypeVariable { name: actual_name },
        ) => {
            if expected_name != actual_name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::ExplicitTypeVariable {
                            name: expected_name,
                        },
                        actual_type: Type::ExplicitTypeVariable { name: actual_name },
                    },
                })
            } else {
                Ok(Type::ExplicitTypeVariable {
                    name: expected_name,
                })
            }
        }
        (Type::Array(expected_element_type), Type::Array(actual_element_type)) => {
            match unify_type(
                environment,
                expected_element_type.as_ref(),
                actual_element_type.as_ref(),
                position,
            ) {
                Ok(element_type) => Ok(Type::Array(Box::new(element_type))),
                Err(UnifyError {
                    kind: UnifyErrorKind::TypeMismatch { .. },
                    ..
                }) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Array(expected_element_type),
                        actual_type: Type::Array(actual_element_type),
                    },
                }),
                Err(error) => Err(error),
            }
        }
        (
            Type::Named {
                symbol_uid: expected_uid,
                name: expected_name,
                type_arguments: expected_arguments,
            },
            Type::Named {
                symbol_uid: actual_uid,
                name: actual_name,
                type_arguments: actual_arguments,
            },
        ) => {
            if expected_name != actual_name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Named {
                            name: expected_name,
                            symbol_uid: expected_uid,
                            type_arguments: expected_arguments,
                        },
                        actual_type: Type::Named {
                            name: actual_name,
                            symbol_uid: actual_uid,
                            type_arguments: actual_arguments,
                        },
                    },
                })
            } else if expected_arguments.len() != actual_arguments.len() {
                panic!("Shoud not be possible, should be compiler bug")
            } else {
                let unify_type_arguments_result = expected_arguments
                    .clone()
                    .into_iter()
                    .zip(actual_arguments.clone().into_iter())
                    .map(
                        |((expected_key, expected_type), (actual_key, actual_type))| {
                            assert_eq!(expected_key, actual_key);
                            unify_type(environment, &expected_type, &actual_type, position)
                        },
                    )
                    .collect::<Result<Vec<Type>, UnifyError>>();

                match unify_type_arguments_result {
                    Ok(_) => Ok(Type::Named {
                        name: expected_name,
                        symbol_uid: expected_uid,
                        type_arguments: expected_arguments,
                    }),
                    Err(UnifyError {
                        kind: UnifyErrorKind::TypeMismatch { .. },
                        ..
                    }) => Err(UnifyError {
                        position,
                        kind: UnifyErrorKind::TypeMismatch {
                            expected_type: Type::Named {
                                name: expected_name,
                                symbol_uid: expected_uid,
                                type_arguments: expected_arguments,
                            },
                            actual_type: Type::Named {
                                name: actual_name,
                                symbol_uid: actual_uid,
                                type_arguments: actual_arguments,
                            },
                        },
                    }),
                    Err(other) => Err(other),
                }
            }
        }
        (Type::Underscore, other) | (other, Type::Underscore) => Ok(other),
        (
            Type::ImplicitTypeVariable {
                name: expected_type_variable_name,
            },
            Type::ImplicitTypeVariable {
                name: actual_type_variable_name,
            },
        ) => {
            if expected_type_variable_name != actual_type_variable_name {
                let expected_type = environment
                    .get_type_variable_terminal_type(expected_type_variable_name.clone());
                let actual_type =
                    environment.get_type_variable_terminal_type(actual_type_variable_name.clone());

                match (expected_type, actual_type) {
                    (None, Some(actual_type)) => {
                        environment.update_substitution(
                            expected_type_variable_name,
                            actual_type.clone(),
                            position,
                        )?;
                        Ok(actual_type)
                    }
                    (Some(expected_type), None) => {
                        environment.update_substitution(
                            actual_type_variable_name,
                            expected_type.clone(),
                            position,
                        )?;
                        Ok(expected_type)
                    }
                    (Some(expected_type), Some(actual_type)) => {
                        unify_type(environment, &expected_type, &actual_type, position)
                    }
                    (None, None) => {
                        environment.update_substitution(
                            expected_type_variable_name.clone(),
                            Type::ImplicitTypeVariable {
                                name: actual_type_variable_name,
                            },
                            position,
                        )?;
                        Ok(Type::ImplicitTypeVariable {
                            name: expected_type_variable_name,
                        })
                    }
                }
            } else {
                Ok(actual.clone())
            }
        }
        (Type::ImplicitTypeVariable { name }, other_type)
        | (other_type, Type::ImplicitTypeVariable { name }) => {
            if type_variable_occurs_in_type(&name, &other_type) {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::InfiniteTypeDetected {
                        type_variable_name: name,
                        in_type: other_type,
                    },
                })
            } else {
                // This is the magical part that makes type inference works
                environment.update_substitution(name, other_type.clone(), position)?;
                Ok(other_type)
            }
        }
        (Type::Function(expected_function), Type::Function(actual_function)) => {
            let function_type =
                unify_function_type(environment, &expected_function, &actual_function, position)?;
            Ok(Type::Function(function_type))
        }
        (
            Type::Record {
                key_type_pairs: mut expected_key_type_pairs,
            },
            Type::Record {
                key_type_pairs: mut actual_key_type_pairs,
            },
        ) => {
            let mut expected_keys = expected_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key);

            let mut actual_keys = actual_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key);

            // 1. Find for missing keys
            let missing_keys: Vec<String> = expected_keys
                .clone()
                .filter(|expected_key| !actual_keys.any(|actual_key| *expected_key == actual_key))
                .collect();

            if !missing_keys.is_empty() {
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                });
            }

            // 2. Find for extraneous keys
            let extraneous_keys: Vec<String> = actual_keys
                .filter(|actual_key| !expected_keys.any(|expected_key| expected_key == *actual_key))
                .collect();

            if !extraneous_keys.is_empty() {
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Record {
                            key_type_pairs: expected_key_type_pairs,
                        },
                        actual_type: Type::Record {
                            key_type_pairs: actual_key_type_pairs,
                        },
                    },
                });
            }

            // 3. If no missing keys and no extraneous keys, that means all keys are present
            //    Therefore we can happily zip them and expect them to align properly after sorting
            expected_key_type_pairs.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

            actual_key_type_pairs.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

            let zipped = expected_key_type_pairs
                .clone()
                .into_iter()
                .zip(actual_key_type_pairs.clone().into_iter());

            let key_type_pairs = zipped
                .map(|((key, expected_type), (_, actual_type))| {
                    match unify_type(environment, &expected_type, &actual_type, position) {
                        Ok(_) => Ok((key, expected_type)),
                        Err(UnifyError {
                            position: _,
                            kind: UnifyErrorKind::TypeMismatch { .. },
                        }) => Err(UnifyError {
                            position,
                            kind: UnifyErrorKind::TypeMismatch {
                                expected_type: Type::Record {
                                    key_type_pairs: expected_key_type_pairs.clone(),
                                },
                                actual_type: Type::Record {
                                    key_type_pairs: actual_key_type_pairs.clone(),
                                },
                            },
                        }),
                        Err(other) => Err(other),
                    }
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        _ => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: expected.clone(),
                actual_type: actual.clone(),
            },
        }),
    }
}

pub fn rewrite_type_variables_in_type(
    from_to_mappings: Vec<(String, Type)>,
    in_type: Type,
) -> Type {
    from_to_mappings
        .iter()
        .fold(in_type, |result_type, (from_type_variable, to_type)| {
            rewrite_type_variable_in_type(from_type_variable, to_type, result_type)
        })
}

pub fn rewrite_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: Type,
) -> Type {
    match in_type {
        Type::Float => Type::Float,
        Type::Integer => Type::Integer,
        Type::Boolean => Type::Boolean,
        Type::String => Type::String,
        Type::Character => Type::Character,
        Type::Null => Type::Null,
        Type::ExplicitTypeVariable { name } => Type::ExplicitTypeVariable { name },
        Type::Array(type_value) => Type::Array(Box::new(rewrite_type_variable_in_type(
            from_type_variable,
            to_type,
            *type_value,
        ))),
        Type::ImplicitTypeVariable { name } => {
            if name == *from_type_variable {
                to_type.clone()
            } else {
                Type::ImplicitTypeVariable { name }
            }
        }
        Type::Named {
            name,
            symbol_uid,
            type_arguments,
        } => Type::Named {
            name,
            symbol_uid,
            type_arguments: type_arguments
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
        Type::Tuple(types) => Type::Tuple(Box::new(types.map(|type_value| {
            rewrite_type_variable_in_type(from_type_variable, to_type, type_value)
        }))),
        Type::Underscore => Type::Underscore,
        Type::Function(FunctionType {
            parameters_types: arguments_types,
            return_type,
        }) => Type::Function(FunctionType {
            parameters_types: Box::new(arguments_types.map(|argument_type| {
                rewrite_type_variable_in_type(from_type_variable, to_type, argument_type)
            })),
            return_type: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *return_type,
            )),
        }),
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .into_iter()
                .map(|(key, type_value)| {
                    (
                        key,
                        rewrite_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
    }
}

pub struct GetEnumTypeResult {
    pub expected_enum_type: Type,
    pub expected_payload_type: Option<Type>,
}

pub fn get_enum_type(
    environment: &mut Environment,
    expected_type: Option<Type>,
    token: &Token,
) -> Result<GetEnumTypeResult, UnifyError> {
    let expected_enum_uid = match &expected_type {
        Some(Type::Named { symbol_uid, .. }) => Some(*symbol_uid),
        _ => None,
    };
    // Look up constructor
    let constructor = environment.get_constructor_symbol(expected_enum_uid, &token)?;

    let enum_type = environment
        .get_type_symbol_by_uid(constructor.enum_uid)
        .unwrap_or_else(|| panic!("Compiler error, cannot find enum type of a constructor"));

    // initiate type variables
    let instantiated_type_variables = {
        match expected_type {
            Some(Type::Named {
                type_arguments,
                symbol_uid,
                ..
            }) if symbol_uid == constructor.enum_uid => type_arguments,
            _ => enum_type
                .type_scheme
                .type_variables
                .iter()
                .map(|type_variable_name| {
                    (
                        type_variable_name.clone(),
                        Type::ImplicitTypeVariable {
                            name: environment.get_next_type_variable_name(),
                        },
                    )
                })
                .collect::<Vec<(String, Type)>>(),
        }
    };

    let expected_enum_type = rewrite_type_variables_in_type(
        instantiated_type_variables.clone(),
        enum_type.type_scheme.type_value,
    );

    let expected_payload_type = match &constructor.payload {
        None => None,
        Some(expected_payload) => Some(rewrite_type_variables_in_type(
            instantiated_type_variables,
            expected_payload.clone(),
        )),
    };

    Ok(GetEnumTypeResult {
        expected_enum_type,
        expected_payload_type,
    })
}

pub struct TypeVariableSubstitution {
    pub from_type_variable: String,
    pub to_type: Type,
}
type TypeVariableSubstitutions = Vec<TypeVariableSubstitution>;

struct InferExpressionResult {
    expression: TypecheckedExpression,
    type_value: Type,
}

fn infer_expression_type(
    environment: &mut Environment,
    expected_type: Option<Type>,
    expression: &Expression,
) -> Result<InferExpressionResult, UnifyError> {
    // NOTE: this part might be a little inefficient,
    // because we might unify the same expression more than once
    // We do this so that the programming part for this algorithm won't be repititive and tedious
    let result = infer_expression_type_(environment, expected_type.clone(), expression)?;

    let type_value = try_unify_type(
        environment,
        expected_type,
        &result.type_value,
        get_expression_position(expression),
    )?;

    Ok(InferExpressionResult {
        expression: result.expression,
        type_value,
    })
}

fn infer_expression_type_(
    environment: &mut Environment,
    expected_type: Option<Type>,
    expression: &Expression,
) -> Result<InferExpressionResult, UnifyError> {
    let result: InferExpressionResult = match expression {
        Expression::Null(_) => Ok(InferExpressionResult {
            expression: TypecheckedExpression::Null,
            type_value: Type::Null,
        }),
        Expression::String(token) => Ok(InferExpressionResult {
            type_value: Type::String,
            expression: TypecheckedExpression::String {
                representation: token.representation.clone(),
            },
        }),
        Expression::Character(token) => Ok(InferExpressionResult {
            type_value: Type::Character,
            expression: TypecheckedExpression::Character {
                representation: token.representation.clone(),
            },
        }),
        Expression::Float(token) => Ok(InferExpressionResult {
            type_value: Type::Float,
            expression: TypecheckedExpression::Float {
                representation: token.representation.clone(),
            },
        }),
        Expression::Integer(token) => Ok(InferExpressionResult {
            type_value: Type::Integer,
            expression: TypecheckedExpression::Integer {
                representation: token.representation.clone(),
            },
        }),
        Expression::Boolean { value, .. } => Ok(InferExpressionResult {
            type_value: Type::Boolean,
            expression: TypecheckedExpression::Boolean(*value),
        }),
        Expression::EnumConstructor { name, payload, .. } => {
            let result = get_enum_type(environment, expected_type, name)?;
            match (result.expected_payload_type, payload.clone()) {
                (None, None) => Ok(InferExpressionResult {
                    type_value: result.expected_enum_type,
                    expression: TypecheckedExpression::EnumConstructor {
                        constructor_name: name.representation.clone(),
                        payload: None,
                    },
                }),
                (None, Some(payload)) => Err(UnifyError {
                    position: get_expression_position(&payload.expression),
                    kind: UnifyErrorKind::ThisEnumConstructorDoesNotRequirePayload,
                }),
                (Some(expected_payload_type), None) => Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::ThisEnumConstructorRequiresPaylod {
                        payload_type: expected_payload_type,
                    },
                }),
                (Some(expected_payload_type), Some(payload)) => {
                    let typechecked_payload = infer_expression_type(
                        environment,
                        Some(expected_payload_type),
                        &payload.expression,
                    )?;
                    Ok(InferExpressionResult {
                        type_value: result.expected_enum_type,
                        expression: TypecheckedExpression::EnumConstructor {
                            constructor_name: name.representation.clone(),
                            payload: Some(Box::new(typechecked_payload.expression)),
                        },
                    })
                }
            }
        }
        Expression::Variable(variable) => {
            // check if the variable name matches any constructor
            if environment.matches_some_enum_constructor(&variable.representation) {
                infer_expression_type_(
                    environment,
                    expected_type,
                    &Expression::EnumConstructor {
                        name: variable.clone(),
                        payload: None,
                    },
                )
            } else {
                let result = environment.get_value_symbol(
                    &variable,
                    &expected_type,
                    environment.current_scope_name(),
                )?;
                Ok(InferExpressionResult {
                    type_value: instantiate_type_scheme(environment, result.type_scheme),
                    expression: TypecheckedExpression::Variable(Variable {
                        uid: result.symbol_uid,
                        representation: variable.representation.clone(),
                    }),
                })
            }
        }
        Expression::RecordAccess {
            expression,
            property_name,
        } => {
            let expression = infer_expression_type(environment, None, expression)?;
            match expression.type_value {
                Type::Record { key_type_pairs } => {
                    match key_type_pairs
                        .iter()
                        .find(|(key, _)| *key == property_name.representation)
                    {
                        None => Err(UnifyError {
                            position: property_name.position,
                            kind: UnifyErrorKind::NoSuchPropertyOnThisRecord {
                                expected_keys: key_type_pairs
                                    .iter()
                                    .map(|(key, _)| key.clone())
                                    .collect(),
                            },
                        }),
                        Some((_, type_value)) => Ok(InferExpressionResult {
                            type_value: type_value.clone(),
                            expression: TypecheckedExpression::RecordAccess {
                                expression: Box::new(expression.expression),
                                property_name: property_name.representation.clone(),
                            },
                        }),
                    }
                }
                actual_type => Err(UnifyError {
                    position: property_name.position,
                    kind: UnifyErrorKind::CannotAccessPropertyOfNonRecord { actual_type },
                }),
            }
        }

        Expression::RecordUpdate {
            expression,
            updates,
            ..
        } => {
            let typechecked_expression =
                infer_expression_type(environment, expected_type, expression)?;

            match &typechecked_expression.type_value {
                Type::Record { key_type_pairs } => {
                    let typechecked_updates = updates
                        .iter()
                        .map(|update| {
                            let actual_key = match &update {
                                RecordUpdate::FunctionalUpdate { property_name, .. } => {
                                    property_name
                                }
                                RecordUpdate::ValueUpdate { property_name, .. } => property_name,
                            };
                            let matching_key_type_pair = key_type_pairs
                                .iter()
                                .find(|(key, _)| *key == actual_key.representation);

                            match matching_key_type_pair {
                                None => Err(UnifyError {
                                    position: actual_key.position,
                                    kind: UnifyErrorKind::NoSuchPropertyOnThisRecord {
                                        expected_keys: key_type_pairs
                                            .iter()
                                            .map(|(key, _)| key.clone())
                                            .collect(),
                                    },
                                }),
                                Some((_, expected_type)) => match update {
                                    RecordUpdate::ValueUpdate {
                                        new_value,
                                        property_name,
                                        ..
                                    } => {
                                        let typechecked_value = infer_expression_type(
                                            environment,
                                            Some(expected_type.clone()),
                                            new_value,
                                        )?;
                                        Ok(TypecheckedRecordUpdate::ValueUpdate {
                                            property_name: property_name.representation.clone(),
                                            new_value: typechecked_value.expression,
                                        })
                                    }
                                    RecordUpdate::FunctionalUpdate {
                                        function,
                                        property_name,
                                        ..
                                    } => {
                                        let expected_type = Type::Function(FunctionType {
                                            parameters_types: Box::new(NonEmpty {
                                                head: expected_type.clone(),
                                                tail: vec![],
                                            }),
                                            return_type: Box::new(expected_type.clone()),
                                        });
                                        let typechecked_function = infer_expression_type(
                                            environment,
                                            Some(expected_type),
                                            function,
                                        )?;
                                        Ok(TypecheckedRecordUpdate::FunctionalUpdate {
                                            property_name: property_name.representation.clone(),
                                            function: typechecked_function.expression,
                                        })
                                    }
                                },
                            }
                        })
                        .collect::<Result<Vec<TypecheckedRecordUpdate>, UnifyError>>()?;
                    Ok(InferExpressionResult {
                        type_value: typechecked_expression.type_value,
                        expression: TypecheckedExpression::RecordUpdate {
                            expression: Box::new(typechecked_expression.expression),
                            updates: typechecked_updates,
                        },
                    })
                }
                other_type => Err(UnifyError {
                    position: get_expression_position(expression.as_ref()),
                    kind: UnifyErrorKind::CannotPerformRecordUpdateOnNonRecord {
                        actual_type: other_type.clone(),
                    },
                }),
            }
        }

        // NOTE: Let expression is desugared into immediately invoked function call
        // For example, `let x = 1 x.plus(1)` means `(x => x.plus(1))(1)
        Expression::Let {
            left,
            right,
            false_branch,
            true_branch,
            type_annotation,
            keyword_let,
            ..
        } => {
            environment.step_into_new_child_scope();
            let type_annotation_type =
                optional_type_annotation_to_type(environment, type_annotation)?;
            let typechecked_right =
                infer_expression_type(environment, type_annotation_type, right)?;

            let result = match false_branch {
                None => {
                    let expected_left_type = get_expected_type(
                        environment,
                        Some(typechecked_right.type_value),
                        type_annotation.clone(),
                    )?;

                    let typechecked_left =
                        infer_destructure_pattern(environment, expected_left_type, left)?;

                    let typechecked_true_branch =
                        infer_expression_type(environment, expected_type, true_branch)?;

                    let result_type = match check_exhaustiveness(
                        environment,
                        typechecked_left.type_value.clone(),
                        NonEmpty {
                            head: *left.clone(),
                            tail: vec![],
                        },
                        get_destructure_pattern_position(left),
                    ) {
                        Ok(_) => {
                            // If the pattern of left is already exhaustive,
                            // the type of this let expression should be true_branch_type
                            Ok(typechecked_true_branch.type_value)
                        }
                        Err(UnifyError {
                            kind: UnifyErrorKind::MissingCases(remaining_patterns),
                            ..
                        }) => {
                            // If the pattern of the `left` is NOT exhaustive
                            // the type of this let expression should be type of `cases` unified together
                            // This unification is for handling cases as such:
                            //
                            //      let foo = \null =>
                            //          let Ok(x) = Ok(1)
                            //          Ok({x})
                            //
                            // In this case, the implicit return type should be Result<T, U>, not Result<Integer, U>
                            // This is required such that the ending type can be Result<X, U>, where X can be not Integer
                            let expected_type = {
                                let type_value = Type::ImplicitTypeVariable {
                                    name: environment.get_next_type_variable_name(),
                                };
                                let types = remaining_patterns
                                    .into_iter()
                                    .map(|expandable_pattern| {
                                        expandable_pattern.to_type(environment)
                                    })
                                    .collect::<Vec<Type>>();
                                types
                                    .into_iter()
                                    .fold(Ok(type_value), |result, type_value| match result {
                                        Err(error) => Err(error),
                                        Ok(expected_type) => unify_type(
                                            environment,
                                            &expected_type,
                                            &type_value,
                                            Position::dummy(),
                                        ),
                                    })?
                            };

                            let type_value = unify_type(
                                environment,
                                &expected_type,
                                &typechecked_true_branch.type_value,
                                get_expression_position(true_branch),
                            )?;
                            Ok(type_value)
                        }
                        Err(other_error) => Err(other_error),
                    }?;

                    Ok(InferExpressionResult {
                        type_value: result_type,
                        expression: TypecheckedExpression::FunctionCall(Box::new(
                            TypecheckedFunctionCall {
                                function: Box::new(TypecheckedExpression::Function(Box::new(
                                    TypecheckedFunction {
                                        branches: Box::new(NonEmpty {
                                            head: TypecheckedFunctionBranch {
                                                parameters: Box::new(NonEmpty {
                                                    head: typechecked_left.destructure_pattern,
                                                    tail: vec![],
                                                }),
                                                body: Box::new(typechecked_true_branch.expression),
                                            },
                                            tail: vec![TypecheckedFunctionBranch {
                                                parameters: Box::new(NonEmpty {
                                                    head: TypecheckedDestructurePattern::Underscore,
                                                    tail: vec![],
                                                }),
                                                body: Box::new(
                                                    typechecked_right.expression.clone(),
                                                ),
                                            }],
                                        }),
                                    },
                                ))),
                                first_argument: Box::new(typechecked_right.expression),
                                rest_arguments: vec![],
                            },
                        )),
                    })
                }
                Some(false_branch) => {
                    // Check the case exhaustiveness of the union between left and false_branch
                    let function = Function {
                        branches: {
                            let mut branches = false_branch.branches.clone();
                            branches.insert(
                                0,
                                FunctionBranch {
                                    start_token: keyword_let.clone(),
                                    parameters: NonEmpty {
                                        head: FunctionParameter {
                                            destructure_pattern: *left.clone(),
                                            type_annotation: type_annotation.clone(),
                                        },
                                        tail: vec![],
                                    },
                                    body: true_branch.clone(),
                                    return_type_annotation: None,
                                },
                            );
                            branches
                        },
                    };
                    let expected_function_type = FunctionType {
                        parameters_types: Box::new(NonEmpty {
                            head: typechecked_right.type_value,
                            tail: vec![],
                        }),
                        return_type: Box::new(environment.introduce_implicit_type_variable(None)?),
                    };
                    let typechecked_function = infer_function(
                        environment,
                        Some(expected_function_type),
                        &Box::new(function),
                    )?;

                    Ok(InferExpressionResult {
                        type_value: *typechecked_function.function_type.return_type,
                        expression: TypecheckedExpression::FunctionCall(Box::new(
                            TypecheckedFunctionCall {
                                first_argument: Box::new(typechecked_right.expression),
                                rest_arguments: vec![],
                                function: Box::new(TypecheckedExpression::Function(Box::new(
                                    typechecked_function.function,
                                ))),
                            },
                        )),
                    })
                }
            }?;
            environment.step_out_to_parent_scope();
            Ok(result)
        }
        Expression::Function(function) => {
            let expected_function_type = match expected_type {
                Some(Type::Function(function_type)) => Some(function_type),
                _ => None,
            };
            let typechecked_function =
                infer_function(environment, expected_function_type, function)?;
            Ok(InferExpressionResult {
                type_value: Type::Function(typechecked_function.function_type),
                expression: TypecheckedExpression::Function(Box::new(
                    typechecked_function.function,
                )),
            })
        }
        Expression::FunctionCall(function_call) => {
            // Get expected function type
            let expected_function_type = {
                // Note that we only infer the type of the first argument
                // The rest arguments is just instantiated with type variables
                let typechecked_first_argument = infer_expression_type(
                    environment,
                    None,
                    function_call.first_argument.as_ref(),
                )?;
                let expected_rest_arguments_types = match &function_call.rest_arguments {
                    None => Ok(vec![]),
                    Some(rest_arguments) => rest_arguments
                        .arguments
                        .iter()
                        .map(|_| environment.introduce_implicit_type_variable(None))
                        .collect::<Result<Vec<Type>, UnifyError>>(),
                }?;
                let expected_return_type = environment.introduce_implicit_type_variable(None)?;
                Type::Function(FunctionType {
                    parameters_types: Box::new(NonEmpty {
                        head: typechecked_first_argument.type_value,
                        tail: expected_rest_arguments_types,
                    }),
                    return_type: Box::new(expected_return_type),
                })
            };

            // Get the actual function type
            // note that we use infer_expression_type_ instead of infer_expression_type
            // This is so that we can throw the error of `cannot invoke non-function`
            let typechecked_function = infer_expression_type_(
                environment,
                Some(expected_function_type),
                &Expression::Variable(function_call.function_name.clone()),
            )?;

            // Check if expression being invoked is a function
            match typechecked_function.type_value {
                Type::Function(expected_function_type) => {
                    // Tally arguments length
                    {
                        let expected_arguments_length =
                            expected_function_type.parameters_types.len();
                        let actual_arguments_length = match &function_call.rest_arguments {
                            Some(rest_arguments) => rest_arguments.arguments.len() + 1,
                            None => 1,
                        };
                        if actual_arguments_length != expected_arguments_length {
                            return Err(UnifyError {
                                position: get_expression_position(&Expression::FunctionCall(
                                    function_call.clone(),
                                )),
                                kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                                    actual_length: actual_arguments_length,
                                    expected_length: expected_arguments_length,
                                },
                            });
                        }
                    }

                    // Unify first argument
                    let typechecked_first_argument = infer_expression_type(
                        environment,
                        Some(expected_function_type.parameters_types.first().clone()),
                        function_call.first_argument.as_ref(),
                    )?;

                    // Unify the type of each rest argument
                    let actual_rest_arguments = match function_call.rest_arguments.clone() {
                        Some(rest_arguments) => rest_arguments.arguments,
                        None => Vec::new(),
                    };
                    let typechecked_rest_arguments = expected_function_type
                        .parameters_types
                        .tail()
                        .iter()
                        .zip(actual_rest_arguments.iter())
                        .map(|(expected_argument_type, actual_argument)| {
                            let expected_argument_type =
                                environment.apply_subtitution_to_type(expected_argument_type);
                            infer_expression_type(
                                environment,
                                Some(expected_argument_type),
                                &actual_argument,
                            )
                        })
                        .collect::<Result<Vec<InferExpressionResult>, UnifyError>>()?;

                    Ok(InferExpressionResult {
                        type_value: environment
                            .apply_subtitution_to_type(expected_function_type.return_type.as_ref()),
                        expression: TypecheckedExpression::FunctionCall(Box::new(
                            TypecheckedFunctionCall {
                                function: Box::new(typechecked_function.expression),
                                first_argument: Box::new(typechecked_first_argument.expression),
                                rest_arguments: typechecked_rest_arguments
                                    .iter()
                                    .map(|argument| argument.expression.clone())
                                    .collect(),
                            },
                        )),
                    })
                }
                Type::ImplicitTypeVariable { name } => {
                    let expected_function_type = Type::ImplicitTypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };

                    let position =
                        get_expression_position(&Expression::FunctionCall(function_call.clone()));

                    unify_type(
                        environment,
                        &Type::ImplicitTypeVariable { name },
                        &expected_function_type,
                        position,
                    )?;

                    let return_type = Type::ImplicitTypeVariable {
                        name: environment.get_next_type_variable_name(),
                    };

                    let typechecked_first_argument = infer_expression_type(
                        environment,
                        None,
                        function_call.first_argument.as_ref(),
                    )?;

                    let typechecked_rest_arguments = match &function_call.rest_arguments {
                        None => Vec::new(),
                        Some(FunctionCallRestArguments { arguments, .. }) => arguments
                            .clone()
                            .into_iter()
                            .map(|argument| infer_expression_type(environment, None, &argument))
                            .collect::<Result<Vec<InferExpressionResult>, UnifyError>>()?,
                    };
                    let actual_function_type = Type::Function(FunctionType {
                        parameters_types: Box::new(NonEmpty {
                            head: typechecked_first_argument.type_value,
                            tail: typechecked_rest_arguments
                                .iter()
                                .map(|argument| argument.type_value.clone())
                                .collect(),
                        }),
                        return_type: Box::new(return_type.clone()),
                    });

                    unify_type(
                        environment,
                        &actual_function_type,
                        &expected_function_type,
                        position,
                    )?;

                    Ok(InferExpressionResult {
                        type_value: environment.apply_subtitution_to_type(&return_type),
                        expression: TypecheckedExpression::FunctionCall(Box::new(
                            TypecheckedFunctionCall {
                                function: Box::new(typechecked_function.expression),
                                first_argument: Box::new(typechecked_first_argument.expression),
                                rest_arguments: typechecked_rest_arguments
                                    .iter()
                                    .map(|argument| argument.expression.clone())
                                    .collect(),
                            },
                        )),
                    })
                }
                other => Err(UnifyError {
                    position: function_call.function_name.position,
                    kind: UnifyErrorKind::CannotInvokeNonFunction { actual_type: other },
                }),
            }
        }
        Expression::Record {
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => {
            // Check for duplicated keys
            let mut key_map = HashSet::new();
            for key in key_value_pairs
                .iter()
                .map(|RecordKeyValue { key, .. }| key.clone())
            {
                if key_map.get(&key.representation).is_some() {
                    return Err(UnifyError {
                        position: key.position,
                        kind: UnifyErrorKind::DuplicatedRecordKey,
                    });
                } else {
                    key_map.insert(key.representation);
                }
            }

            let record_position =
                join_position(left_curly_bracket.position, right_curly_bracket.position);
            match expected_type {
                Some(Type::Record { key_type_pairs }) => infer_record_type(
                    environment,
                    Some(key_type_pairs),
                    key_value_pairs.clone(),
                    record_position,
                ),
                _ => infer_record_type(environment, None, key_value_pairs.clone(), record_position),
            }
        }
        Expression::Array { elements, .. } => {
            let element_type = match expected_type {
                Some(Type::Array(expected_element_type)) => Ok(*expected_element_type),
                _ => Ok(Type::ImplicitTypeVariable {
                    name: environment.get_next_type_variable_name(),
                }),
            }?;
            let typechecked_elements = elements
                .iter()
                .map(|element| {
                    infer_expression_type(environment, Some(element_type.clone()), element)
                })
                .collect::<Result<Vec<InferExpressionResult>, UnifyError>>()?;
            Ok(InferExpressionResult {
                type_value: Type::Array(Box::new(
                    environment.apply_subtitution_to_type(&element_type),
                )),
                expression: TypecheckedExpression::Array {
                    elements: typechecked_elements
                        .iter()
                        .map(|element| element.expression.clone())
                        .collect(),
                },
            })
        }
    }?;
    Ok(InferExpressionResult {
        type_value: environment.apply_subtitution_to_type(&result.type_value),
        expression: result.expression,
    })
}

fn infer_record_type(
    environment: &mut Environment,
    expected_key_type_pairs: Option<Vec<(String, Type)>>,
    mut actual_key_value_pairs: Vec<RecordKeyValue>,
    record_position: Position,
) -> Result<InferExpressionResult, UnifyError> {
    match &expected_key_type_pairs {
        None => Ok(()),
        Some(expected_key_type_pairs) => {
            let expected_keys: Vec<String> = expected_key_type_pairs
                .iter()
                .map(|(key, _)| key.clone())
                .collect();
            let actual_keys: Vec<Token> = actual_key_value_pairs
                .iter()
                .map(|key_value_pair| key_value_pair.key.clone())
                .collect();

            // Find extranerous key
            let extraneous_key = actual_keys.iter().find(|actual_key| {
                !expected_keys
                    .iter()
                    .any(|expected_key| expected_key.eq(&actual_key.representation))
            });

            match extraneous_key {
                None => Ok(()),
                Some(extraneous_key) => Err(UnifyError {
                    position: extraneous_key.position,
                    kind: UnifyErrorKind::RecordExtraneousKey {
                        expected_keys: expected_keys.clone(),
                    },
                }),
            }?;

            // Find missing key
            let missing_keys = expected_keys
                .into_iter()
                .filter(|expected_key| {
                    !actual_keys
                        .iter()
                        .any(|actual_key| actual_key.representation.eq(expected_key))
                })
                .collect::<Vec<String>>();

            if missing_keys.is_empty() {
                Ok(())
            } else {
                Err(UnifyError {
                    position: record_position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                })
            }?;

            Ok(())
        }
    }?;

    // If we reach this stage, it means that the keys of expected record and actual record tallied
    // Therefore we can zipped them together, of course,
    // we need to sort the keys first before zipping
    let expected_key_type_pairs = match expected_key_type_pairs {
        None => iter::repeat(None)
            .take(actual_key_value_pairs.len())
            .collect::<Vec<_>>(),
        Some(mut expected_key_type_pairs) => {
            expected_key_type_pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
            expected_key_type_pairs.into_iter().map(Some).collect()
        }
    };

    actual_key_value_pairs.sort_by(|a, b| a.key.representation.cmp(&b.key.representation));

    let typechecked_key_value_pairs = expected_key_type_pairs
        .iter()
        .zip(actual_key_value_pairs.iter())
        .map(
            |(
                expected_key_type_pair,
                RecordKeyValue {
                    key,
                    value,
                    type_annotation,
                },
            )| {
                let expected_type = get_expected_type(
                    environment,
                    expected_key_type_pair
                        .clone()
                        .map(|(_, type_value)| type_value),
                    type_annotation.clone(),
                )?;
                let value_type = infer_expression_type(environment, expected_type, &value)?;
                Ok((key.representation.clone(), value_type))
            },
        )
        .collect::<Result<Vec<(String, InferExpressionResult)>, UnifyError>>()?;

    Ok(InferExpressionResult {
        type_value: Type::Record {
            key_type_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (key.clone(), result.type_value.clone()))
                .collect(),
        },
        expression: TypecheckedExpression::Record {
            key_value_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (key.clone(), result.expression.clone()))
                .collect(),
        },
    })
}

struct InferFunctionResult {
    function_type: FunctionType,
    function: TypecheckedFunction,
}
fn infer_function(
    environment: &mut Environment,
    expected_function_type: Option<FunctionType>,
    function: &Function,
) -> Result<InferFunctionResult, UnifyError> {
    let typechecked_first_branch = infer_function_branch(
        environment,
        expected_function_type,
        &function.branches.first(),
    )?;

    let typechecked_rest_branches = function
        .branches
        .tail()
        .iter()
        .map(|function_branch| {
            infer_function_branch(
                environment,
                Some(typechecked_first_branch.function_type.clone()),
                &function_branch,
            )
        })
        .collect::<Result<Vec<InferFunctionBranchResult>, UnifyError>>()?;

    let function_type =
        environment.apply_subtitution_to_function_type(&typechecked_first_branch.function_type);

    // Check for case exhaustiveness
    let expected_type = Type::Tuple(function_type.clone().parameters_types);
    let actual_patterns = function
        .clone()
        .branches
        .map(|branch| DestructurePattern::Tuple(function_branch_parameters_to_tuple(&branch)));

    check_exhaustiveness(
        environment,
        expected_type,
        actual_patterns,
        get_expression_position(&Expression::Function(Box::new(function.clone()))),
    )?;

    Ok(InferFunctionResult {
        function_type,
        function: TypecheckedFunction {
            branches: Box::new(NonEmpty {
                head: typechecked_first_branch.function_branch,
                tail: typechecked_rest_branches
                    .iter()
                    .map(|branch| branch.function_branch.clone())
                    .collect(),
            }),
        },
    })
}

pub fn function_branch_parameters_to_tuple(
    function_branch: &FunctionBranch,
) -> DestructurePatternTuple {
    DestructurePatternTuple {
        parentheses: None,
        values: Box::new(
            function_branch
                .parameters
                .clone()
                .map(|argument| argument.destructure_pattern),
        ),
    }
}

pub fn check_exhaustiveness(
    environment: &Environment,
    expected_type: Type,
    actual_patterns: NonEmpty<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    check_exhaustiveness_(
        environment,
        vec![ExpandablePattern::Any {
            type_value: expected_type,
        }],
        actual_patterns,
        position,
    )
}

pub fn check_exhaustiveness_(
    environment: &Environment,
    expected_patterns: Vec<ExpandablePattern>,
    actual_patterns: NonEmpty<DestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    // println!("expected_patterns = {:#?}", expected_patterns);
    // println!("actual_patterns = {:#?}", actual_patterns);
    let remaining_expected_patterns = actual_patterns.into_vector().into_iter().fold(
        Ok(expected_patterns),
        |expected_patterns, actual_pattern| match expected_patterns {
            Err(error) => Err(error),
            Ok(expected_patterns) => {
                // println!("actual_pattern = {:?}", actual_pattern);
                // println!("expected_patterns = {:?}", expected_patterns);
                if expected_patterns.is_empty() {
                    // If there are still remaning actual_patterns but expected_patterns already exhausted
                    // Then this actual_pattern is an unreachable case
                    Err(UnifyError {
                        position: get_destructure_pattern_position(&actual_pattern),
                        kind: UnifyErrorKind::UnreachableCase,
                    })
                } else {
                    let new_patterns =
                        match_patterns(environment, &actual_pattern, expected_patterns.clone());

                    if new_patterns.eq(&expected_patterns) {
                        // If new_patterns equals to the current expected_patterns
                        // Then this actual_pattern is also an unreachable case
                        //  as it does not remove any patterns from expected_patterns
                        Err(UnifyError {
                            position: get_destructure_pattern_position(&actual_pattern),
                            kind: UnifyErrorKind::UnreachableCase,
                        })
                    } else {
                        Ok(new_patterns)
                    }
                }
            }
        },
    )?;
    if remaining_expected_patterns.is_empty() {
        Ok(())
    } else {
        // println!("reamning_pattern={:?}", remaining_expected_patterns);
        Err(UnifyError {
            position,
            kind: UnifyErrorKind::MissingCases(remaining_expected_patterns),
        })
    }
}

pub fn unify_function_type(
    environment: &mut Environment,
    expected_function_type: &FunctionType,
    actual_function_type: &FunctionType,
    position: Position,
) -> Result<FunctionType, UnifyError> {
    // compare parameters length
    if expected_function_type.parameters_types.len() != actual_function_type.parameters_types.len()
    {
        return Err(UnifyError {
            position,
            kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                expected_length: expected_function_type.parameters_types.len(),
                actual_length: actual_function_type.parameters_types.len(),
            },
        });
    }

    // unify parameters types
    let zipped = expected_function_type
        .clone()
        .parameters_types
        .zip(*actual_function_type.parameters_types.clone());

    let unify_rest_argument_type_result = zipped.fold_result(|(expected_type, actual_type)| {
        unify_type(environment, &expected_type, &actual_type, position)
    });

    match unify_rest_argument_type_result {
        Ok(parameters_types) => {
            // unify return type
            match unify_type(
                environment,
                expected_function_type.return_type.as_ref(),
                actual_function_type.return_type.as_ref(),
                position,
            ) {
                Ok(return_type) => Ok(FunctionType {
                    parameters_types: Box::new(parameters_types),
                    return_type: Box::new(return_type),
                }),
                Err(_) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::Function(
                            environment.apply_subtitution_to_function_type(&expected_function_type),
                        ),
                        actual_type: Type::Function(
                            environment.apply_subtitution_to_function_type(&actual_function_type),
                        ),
                    },
                }),
            }
        }
        Err(_) => Err(UnifyError {
            position,
            kind: UnifyErrorKind::TypeMismatch {
                expected_type: Type::Function(expected_function_type.clone()),
                actual_type: Type::Function(actual_function_type.clone()),
            },
        }),
    }
}

pub fn get_function_branch_position(function_branch: &FunctionBranch) -> Position {
    let body_position = get_expression_position(&function_branch.body);
    join_position(function_branch.start_token.position, body_position)
}

struct InferFunctionBranchResult {
    function_branch: TypecheckedFunctionBranch,
    function_type: FunctionType,
}
fn infer_function_branch(
    environment: &mut Environment,
    expected_function_type: Option<FunctionType>,
    function_branch: &FunctionBranch,
) -> Result<InferFunctionBranchResult, UnifyError> {
    environment.step_into_new_child_scope();

    let typechecked_parameters = {
        match &expected_function_type {
            None => function_branch.parameters().fold_result(|parameter| {
                Ok(infer_function_parameter(environment, None, &parameter)?)
            }),

            Some(expected_function_type) => {
                let actual_parameters = function_branch.parameters();
                if expected_function_type.parameters_types.len() != actual_parameters.len() {
                    Err(UnifyError {
                        position: get_function_branch_position(&function_branch),
                        kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                            expected_length: expected_function_type.parameters_types.len(),
                            actual_length: actual_parameters.len(),
                        },
                    })
                } else {
                    expected_function_type
                        .clone()
                        .parameters_types
                        .zip(actual_parameters)
                        .fold_result(|(expected_parameter_type, actual_parameter)| {
                            infer_function_parameter(
                                environment,
                                Some(expected_parameter_type),
                                &actual_parameter,
                            )
                        })
                }
            }
        }
    }?;

    let expected_return_type = get_expected_type(
        environment,
        expected_function_type.map(|expected_function_type| *expected_function_type.return_type),
        function_branch.return_type_annotation.clone(),
    )?;

    let typechecked_body =
        infer_expression_type(environment, expected_return_type, &function_branch.body)?;

    let result_type = FunctionType {
        parameters_types: Box::new(
            typechecked_parameters
                .clone()
                .map(|argument| argument.type_value),
        ),
        return_type: Box::new(environment.apply_subtitution_to_type(&typechecked_body.type_value)),
    };

    // Check for unused variables
    environment.check_for_unused_symbols(environment.current_scope_name())?;

    environment.step_out_to_parent_scope();

    Ok(InferFunctionBranchResult {
        function_type: environment.apply_subtitution_to_function_type(&result_type),
        function_branch: TypecheckedFunctionBranch {
            parameters: Box::new(
                typechecked_parameters.map(|argument| argument.destructure_pattern),
            ),
            body: Box::new(typechecked_body.expression),
        },
    })
}

pub fn get_expected_type(
    environment: &mut Environment,
    expected_type: Option<Type>,
    expected_type_annotation: Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match (expected_type, expected_type_annotation) {
        (None, None) => Ok(None),
        (Some(expected_type), None) => Ok(Some(expected_type)),
        (None, Some(type_annotation)) => Ok(Some(type_annotation_to_type(
            environment,
            &type_annotation,
        )?)),
        (Some(expected_type), Some(type_annotation)) => Err(UnifyError {
            position: get_type_annotation_position(&type_annotation),
            kind: UnifyErrorKind::UnnecessaryTypeAnnotation { expected_type },
        }),
    }
}

fn infer_function_parameter(
    environment: &mut Environment,
    expected_argument_type: Option<Type>,
    function_argument: &FunctionParameter,
) -> Result<InferDestructurePatternResult, UnifyError> {
    let expected_type = get_expected_type(
        environment,
        expected_argument_type,
        function_argument.type_annotation.clone(),
    )?;

    infer_destructure_pattern(
        environment,
        expected_type,
        &function_argument.destructure_pattern,
    )
}

pub fn optional_type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: &Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match type_annotation {
        Some(type_annotation) => Ok(Some(type_annotation_to_type(environment, type_annotation)?)),
        None => Ok(None),
    }
}

pub fn type_annotation_to_type(
    environment: &mut Environment,
    type_annotation: &TypeAnnotation,
) -> Result<Type, UnifyError> {
    match &type_annotation {
        TypeAnnotation::Underscore(_) => Ok(Type::Underscore),
        TypeAnnotation::Named {
            name,
            type_arguments,
        } => {
            if let Some(type_symbol) = environment.get_type_symbol_by_name(&name) {
                let type_arguments = match type_arguments {
                    None => Ok(vec![]),
                    Some(TypeArguments {
                        arguments,
                        left_angular_bracket,
                        right_angular_bracket,
                    }) => {
                        if type_symbol.type_scheme.type_variables.len() != arguments.len() {
                            Err(UnifyError {
                                position: join_position(
                                    left_angular_bracket.position,
                                    right_angular_bracket.position,
                                ),
                                kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                                    actual_length: arguments.len(),
                                    expected_type_parameter_names: type_symbol
                                        .type_scheme
                                        .type_variables
                                        .clone(),
                                },
                            })
                        } else {
                            arguments
                                .iter()
                                .map(|type_value| type_annotation_to_type(environment, type_value))
                                .collect::<Result<Vec<Type>, UnifyError>>()
                        }
                    }
                }?;

                type_symbol
                    .type_scheme
                    .type_variables
                    .iter()
                    .zip(type_arguments.into_iter())
                    .fold(
                        Ok(type_symbol.type_scheme.type_value),
                        |result, (expected_type_variable_name, type_value)| match result {
                            Err(error) => Err(error),
                            Ok(result) => Ok(rewrite_type_variable_in_type(
                                expected_type_variable_name,
                                &type_value,
                                result,
                            )),
                        },
                    )
            } else {
                Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::UnknownTypeSymbol,
                })
            }
        }
        TypeAnnotation::Record {
            key_type_annotation_pairs,
            ..
        } => {
            let key_type_pairs = key_type_annotation_pairs
                .iter()
                .map(|(key, type_annotation)| {
                    let type_value = type_annotation_to_type(environment, &type_annotation)?;
                    Ok((key.representation.clone(), type_value))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        TypeAnnotation::Array { element_type, .. } => Ok(Type::Array(Box::new(
            type_annotation_to_type(environment, element_type)?,
        ))),
        TypeAnnotation::Function {
            parameters_types,
            return_type,
            ..
        } => Ok(Type::Function(FunctionType {
            parameters_types: Box::new(parameters_types.clone().fold_result(|parameter_type| {
                type_annotation_to_type(environment, &parameter_type)
            })?),
            return_type: Box::new(type_annotation_to_type(environment, return_type)?),
        })),
    }
}

#[derive(Clone)]
struct InferDestructurePatternResult {
    destructure_pattern: TypecheckedDestructurePattern,
    type_value: Type,
}
fn infer_destructure_pattern(
    environment: &mut Environment,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
) -> Result<InferDestructurePatternResult, UnifyError> {
    // Same story as infer_expression_type
    let result =
        infer_destructure_pattern_(environment, expected_type.clone(), destructure_pattern)?;

    let type_value = try_unify_type(
        environment,
        expected_type,
        &result.type_value,
        get_destructure_pattern_position(destructure_pattern),
    )?;

    Ok(InferDestructurePatternResult {
        destructure_pattern: result.destructure_pattern,
        type_value,
    })
}

fn infer_destructure_pattern_(
    environment: &mut Environment,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
) -> Result<InferDestructurePatternResult, UnifyError> {
    match destructure_pattern {
        DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::String,
        } => Ok(InferDestructurePatternResult {
            type_value: Type::String,
            destructure_pattern: TypecheckedDestructurePattern::String {
                representation: token.representation.clone(),
            },
        }),
        DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::Character,
        } => Ok(InferDestructurePatternResult {
            type_value: Type::Character,
            destructure_pattern: TypecheckedDestructurePattern::Character {
                representation: token.representation.clone(),
            },
        }),
        DestructurePattern::Infinite {
            token,
            kind: InfinitePatternKind::Integer,
        } => Ok(InferDestructurePatternResult {
            type_value: Type::Integer,
            destructure_pattern: TypecheckedDestructurePattern::Integer {
                representation: token.representation.clone(),
            },
        }),
        DestructurePattern::Null(_) => Ok(InferDestructurePatternResult {
            type_value: Type::Null,
            destructure_pattern: TypecheckedDestructurePattern::Null,
        }),
        DestructurePattern::Boolean { value, .. } => Ok(InferDestructurePatternResult {
            type_value: Type::Boolean,
            destructure_pattern: TypecheckedDestructurePattern::Boolean(*value),
        }),
        DestructurePattern::Underscore(_) => Ok(InferDestructurePatternResult {
            type_value: environment.introduce_implicit_type_variable(None)?,
            destructure_pattern: TypecheckedDestructurePattern::Underscore,
        }),
        DestructurePattern::Identifier(identifier) => {
            // If this identifier matches any enum constructor,
            // then treat it as an enum constructor
            if environment.matches_some_enum_constructor(&identifier.representation) {
                infer_destructure_pattern(
                    environment,
                    expected_type,
                    &DestructurePattern::EnumConstructor {
                        name: identifier.clone(),
                        payload: None,
                    },
                )
            }
            // If this identifier does not match any enum constructor
            // then treat it as a new variable
            else {
                let (uid, type_value) =
                    environment.insert_value_symbol_with_type(identifier, expected_type)?;
                Ok(InferDestructurePatternResult {
                    type_value,
                    destructure_pattern: TypecheckedDestructurePattern::Variable(Variable {
                        uid,
                        representation: identifier.representation.clone(),
                    }),
                })
            }
        }
        DestructurePattern::Tuple(tuple) => {
            let typechecked_values = tuple.values.clone().fold_result(|destructure_pattern| {
                // TODO: pass in expected_type
                infer_destructure_pattern(environment, None, &destructure_pattern)
            })?;
            Ok(InferDestructurePatternResult {
                type_value: Type::Tuple(Box::new(
                    typechecked_values.clone().map(|value| value.type_value),
                )),
                destructure_pattern: TypecheckedDestructurePattern::Tuple {
                    values: Box::new(typechecked_values.map(|value| value.destructure_pattern)),
                },
            })
        }
        DestructurePattern::EnumConstructor { name, payload, .. } => {
            let result = get_enum_type(environment, expected_type, name)?;
            match (result.expected_payload_type, payload.clone()) {
                (None, None) => Ok(InferDestructurePatternResult {
                    type_value: result.expected_enum_type,
                    destructure_pattern: TypecheckedDestructurePattern::EnumConstructor {
                        constructor_name: name.representation.clone(),
                        payload: None,
                    },
                }),
                (None, Some(payload)) => Err(UnifyError {
                    position: get_destructure_pattern_position(&payload.pattern),
                    kind: UnifyErrorKind::ThisEnumConstructorDoesNotRequirePayload,
                }),
                (Some(expected_payload_type), None) => Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::ThisEnumConstructorRequiresPaylod {
                        payload_type: expected_payload_type,
                    },
                }),
                (Some(expected_payload_type), Some(payload)) => {
                    let typechecked_payload = infer_destructure_pattern(
                        environment,
                        Some(expected_payload_type),
                        &payload.pattern,
                    )?;
                    Ok(InferDestructurePatternResult {
                        type_value: result.expected_enum_type,
                        destructure_pattern: TypecheckedDestructurePattern::EnumConstructor {
                            constructor_name: name.representation.clone(),
                            payload: Some(Box::new(typechecked_payload.destructure_pattern)),
                        },
                    })
                }
            }
        }
        DestructurePattern::Array { spread: None, .. } => Ok(InferDestructurePatternResult {
            type_value: Type::Array(Box::new(Type::ImplicitTypeVariable {
                name: environment.get_next_type_variable_name(),
            })),
            destructure_pattern: TypecheckedDestructurePattern::Array { spread: None },
        }),
        DestructurePattern::Array {
            spread: Some(spread),
            ..
        } => {
            let expected_element_type = match expected_type {
                Some(Type::Array(expected_type)) => *expected_type,
                _ => Type::ImplicitTypeVariable {
                    name: environment.get_next_type_variable_name(),
                },
            };

            let typechecked_first_element = infer_destructure_pattern(
                environment,
                Some(expected_element_type.clone()),
                &spread.first_element,
            )?;

            let expected_array_type = Type::Array(Box::new(expected_element_type));
            let typechecked_rest_elements = infer_destructure_pattern(
                environment,
                Some(expected_array_type.clone()),
                &spread.rest_elements,
            )?;

            Ok(InferDestructurePatternResult {
                type_value: environment.apply_subtitution_to_type(&expected_array_type),
                destructure_pattern: TypecheckedDestructurePattern::Array {
                    spread: Some(TypecheckedDesturcturePatternArraySpread {
                        first_element: Box::new(typechecked_first_element.destructure_pattern),
                        rest_elements: Box::new(typechecked_rest_elements.destructure_pattern),
                    }),
                },
            })
        }
        DestructurePattern::Record {
            key_value_pairs, ..
        } => {
            let expected_key_type_pairs = match expected_type {
                Some(Type::Record { key_type_pairs }) => Some(key_type_pairs),
                _ => None,
            };
            let typechecked_key_pattern_pairs = key_value_pairs
                .iter()
                .map(
                    |DestructuredRecordKeyValue {
                         key,
                         as_value,
                         type_annotation,
                     }| {
                        // Try to find the expected type for this key
                        // TODO: optimise this section, as the current algorithm is very slow
                        let actual_key = key.representation.clone();
                        let expected_type = match &expected_key_type_pairs {
                            None => optional_type_annotation_to_type(environment, type_annotation),
                            Some(expected_key_type_pairs) => {
                                let matching_key_type_pair = expected_key_type_pairs
                                    .iter()
                                    .find(|(expected_key, _)| actual_key.eq(expected_key));
                                match matching_key_type_pair {
                                    None => Err(UnifyError {
                                        position: key.position,
                                        kind: UnifyErrorKind::NoSuchPropertyOnThisRecord {
                                            expected_keys: expected_key_type_pairs
                                                .iter()
                                                .map(|(key, _)| key.clone())
                                                .collect(),
                                        },
                                    }),
                                    Some((_, expected_type)) => get_expected_type(
                                        environment,
                                        Some(expected_type.clone()),
                                        type_annotation.clone(),
                                    ),
                                }
                            }
                        }?;

                        match as_value {
                            Some(destructure_pattern) => {
                                let typechecked_destructure_pattern = infer_destructure_pattern(
                                    environment,
                                    expected_type,
                                    destructure_pattern,
                                )?;

                                Ok((actual_key, typechecked_destructure_pattern))
                            }
                            None => {
                                let (uid, type_value) = environment
                                    .insert_value_symbol_with_type(key, expected_type)?;
                                Ok((
                                    actual_key.clone(),
                                    InferDestructurePatternResult {
                                        type_value,
                                        destructure_pattern:
                                            TypecheckedDestructurePattern::Variable(Variable {
                                                uid,
                                                representation: actual_key,
                                            }),
                                    },
                                ))
                            }
                        }
                    },
                )
                .collect::<Result<Vec<(String, InferDestructurePatternResult)>, UnifyError>>()?;

            Ok(InferDestructurePatternResult {
                type_value: Type::Record {
                    key_type_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| (key.clone(), pattern.type_value.clone()))
                        .collect(),
                },
                destructure_pattern: TypecheckedDestructurePattern::Record {
                    key_pattern_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| (key.clone(), pattern.destructure_pattern.clone()))
                        .collect(),
                },
            })
        }
    }
}

fn substitute_type_variable_in_type(
    from_type_variable: &str,
    to_type: &Type,
    in_type: &Type,
) -> Type {
    match in_type {
        Type::String => Type::String,
        Type::Character => Type::Character,
        Type::Null => Type::Null,
        Type::Float => Type::Float,
        Type::Integer => Type::Integer,
        Type::Boolean => Type::Boolean,
        Type::ExplicitTypeVariable { name } => Type::ExplicitTypeVariable { name: name.clone() },
        Type::Array(type_value) => Type::Array(Box::new(substitute_type_variable_in_type(
            from_type_variable,
            to_type,
            type_value.as_ref(),
        ))),
        Type::ImplicitTypeVariable { name } => {
            if *name == *from_type_variable {
                to_type.clone()
            } else {
                in_type.clone()
            }
        }
        Type::Tuple(types) => Type::Tuple(Box::new(types.clone().map(|type_value| {
            substitute_type_variable_in_type(from_type_variable, to_type, &type_value)
        }))),
        Type::Function(FunctionType {
            parameters_types,
            return_type,
        }) => Type::Function(FunctionType {
            parameters_types: Box::new(parameters_types.clone().map(|parameter_type| {
                substitute_type_variable_in_type(from_type_variable, to_type, &parameter_type)
            })),
            return_type: Box::new(substitute_type_variable_in_type(
                from_type_variable,
                to_type,
                return_type.as_ref(),
            )),
        }),
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        substitute_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
        Type::Underscore => Type::Underscore,
        Type::Named {
            name,
            symbol_uid,
            type_arguments: arguments,
        } => Type::Named {
            name: name.to_string(),
            symbol_uid: *symbol_uid,
            type_arguments: arguments
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        substitute_type_variable_in_type(from_type_variable, to_type, type_value),
                    )
                })
                .collect(),
        },
    }
}

fn get_free_type_variables_in_type(type_value: &Type) -> HashSet<String> {
    match type_value {
        Type::Float
        | Type::Integer
        | Type::String
        | Type::Character
        | Type::Null
        | Type::Boolean
        | Type::Underscore
        | Type::ExplicitTypeVariable { .. } => HashSet::new(),
        Type::Array(type_value) => get_free_type_variables_in_type(type_value.as_ref()),
        Type::Tuple(types) => types
            .clone()
            .into_vector()
            .iter()
            .flat_map(get_free_type_variables_in_type)
            .collect::<HashSet<String>>(),
        Type::ImplicitTypeVariable { name } => {
            let mut result: HashSet<String> = HashSet::new();
            result.insert(name.clone());
            result
        }
        Type::Function(FunctionType {
            parameters_types,
            return_type,
        }) => {
            let mut result: HashSet<String> = HashSet::new();

            let type_variables: Vec<HashSet<String>> = parameters_types
                .clone()
                .into_vector()
                .iter()
                .map(get_free_type_variables_in_type)
                .collect();
            type_variables
                .into_iter()
                .for_each(|type_variables| result.extend(type_variables));

            result.extend(get_free_type_variables_in_type(return_type.as_ref()));

            result
        }
        Type::Record { key_type_pairs } => key_type_pairs
            .iter()
            .map(|(_, type_value)| get_free_type_variables_in_type(&type_value))
            .fold(HashSet::new(), |result, type_variables| {
                result.into_iter().chain(type_variables).collect()
            }),
        Type::Named {
            type_arguments: arguments,
            ..
        } => arguments
            .iter()
            .map(|(_, type_value)| type_value)
            .flat_map(get_free_type_variables_in_type)
            .collect(),
    }
}

/// To check whether a type variable occur in a type.
/// This is to prevent absurd unification.
/// For example, the unification of A with (A -> B) should not
/// produce the subtituion of {A = A -> B}
fn type_variable_occurs_in_type(type_variable: &str, typ: &Type) -> bool {
    get_free_type_variables_in_type(typ).contains(type_variable)
}

/// For example, before using the type <A, B>(\A => B)
/// We need to instantiate A and B with a implicit type variable that can be substituted,
/// This is necessary to prevent name clashing during unification of two generic types.
///
/// In this case, we can (for example) substitute A as T1 and B as T2,
/// and the resulting type will be (\T1 => T2)
pub fn instantiate_type_scheme(environment: &mut Environment, type_scheme: TypeScheme) -> Type {
    let type_variable_substitutions: TypeVariableSubstitutions = type_scheme
        .type_variables
        .into_iter()
        .map(|from_type_variable| TypeVariableSubstitution {
            from_type_variable,
            to_type: Type::ImplicitTypeVariable {
                name: environment.get_next_type_variable_name(),
            },
        })
        .collect();

    type_variable_substitutions.into_iter().fold(
        type_scheme.type_value,
        |result,
         TypeVariableSubstitution {
             from_type_variable,
             to_type,
         }| { substitute_type_variable_in_type(&from_type_variable, &to_type, &result) },
    )
}
