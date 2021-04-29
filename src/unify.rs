use crate::{
    compile::{CompileError, CompileErrorKind},
    non_empty::NonEmpty,
    parse::Parser,
    tokenize::Tokenizer,
};

use crate::ast::*;
use crate::module::*;
use crate::pattern::*;
use crate::typechecked_ast::*;
use indexmap::IndexMap;
use relative_path::RelativePath;
use std::fs;
use std::iter;
use std::{collections::HashSet, path::Path};

pub struct UnifyProgramResult {
    /// The entrypoint module
    pub entrypoint: TypecheckedModule,

    // This is for memoization.
    // By doing this we can prevent duplicated efforts to typecheck the same module again.
    // For example (suppose the entry point is D):
    //
    //      D imports B
    //      D imports C
    //      B imports A
    //      C imports A
    //
    // From above, we can see that A is being imported twice,
    //  without memoization we will need to compile A twice, which is bad for performance.
    pub imported_modules: ImportedModules,
}

pub fn unify_statements(
    module_meta: ModuleMeta,
    statements: Vec<Statement>,
    imported_modules: &ImportedModules,
    is_entry_point: bool,
) -> Result<UnifyProgramResult, CompileError> {
    // 1. Partition statements based on their types
    let (
        import_statements,
        type_statements,
        enum_statements,
        let_statements,
        top_level_expressions,
        function_statements,
    ) = {
        let mut import_statements = Vec::new();
        let mut type_statements = Vec::new();
        let mut enum_statements = Vec::new();
        let mut let_statements = Vec::new();
        let mut top_level_expressions = Vec::new();
        let mut function_statements = Vec::new();
        for statement in statements {
            match statement {
                Statement::Import(import_statement) => {
                    import_statements.push(import_statement);
                }
                Statement::Type(type_statement) => {
                    type_statements.push(type_statement);
                }
                Statement::Enum(enum_statement) => {
                    enum_statements.push(enum_statement);
                }
                Statement::Let(let_statement) => {
                    match (&let_statement.left, &let_statement.right) {
                        (
                            DestructurePattern::Identifier(name),
                            Expression::ArrowFunction(arrow_function),
                        ) => function_statements.push(FunctionStatement {
                            keyword_export: let_statement.keyword_export,
                            name: name.clone(),
                            arrow_function: *arrow_function.clone(),
                        }),
                        _ => {
                            let_statements.push(let_statement);
                        }
                    }
                }
                Statement::Expression(expression) => {
                    // Top level expressions will only be compiled on entrypoint file
                    if is_entry_point {
                        top_level_expressions.push(expression);
                    }
                }
            }
        }
        (
            import_statements,
            type_statements,
            enum_statements,
            let_statements,
            top_level_expressions,
            function_statements,
        )
    };

    // 2. Infer import statements (to include every imported symbols into the current module)

    let mut module: Module = Module::new(module_meta);

    let init: (Vec<TypecheckedStatement>, _) = (vec![], imported_modules.clone());

    let (typechecked_import_statements, imported_modules) =
        import_statements
            .into_iter()
            .fold(Ok(init), |result, import_statement| match result {
                Err(error) => Err(error),
                Ok((mut statements, mut imported_modules)) => {
                    let current =
                        infer_import_statement(&mut module, &imported_modules, import_statement)?;

                    statements.extend(
                        current
                            .import_statements
                            .into_iter()
                            .map(|import_statement| {
                                TypecheckedStatement::ImportStatement(import_statement)
                            })
                            .collect::<Vec<TypecheckedStatement>>(),
                    );

                    imported_modules.extend(current.imported_modules);

                    Ok((statements, imported_modules))
                }
            })?;

    // 3. Insert type symbols (including enums) into the current module.
    //    Note that we will first insert them into the current module first before checking their definition,
    //    this is so that mutually recursive type can be checked.
    let enum_statements = enum_statements
        .into_iter()
        .map(|enum_statement| {
            let enum_uid = insert_enum_symbol(&mut module, enum_statement.clone())?;
            Ok((enum_uid, enum_statement))
        })
        .collect::<Result<Vec<(SymbolUid, EnumStatement)>, UnifyError>>()
        .map_err(|unify_error| unify_error.into_compile_error(module.meta.clone()))?;

    type_statements
        .into_iter()
        .map(|type_statement| infer_type_alias_statement(&mut module, type_statement))
        .collect::<Result<Vec<()>, UnifyError>>()
        .map_err(|unify_error| unify_error.into_compile_error(module.meta.clone()))?;

    enum_statements
        .into_iter()
        .map(|(enum_uid, enum_statement)| {
            infer_enum_statement(&mut module, &enum_uid, enum_statement)
        })
        .collect::<Result<Vec<()>, UnifyError>>()
        .map_err(|unify_error| unify_error.into_compile_error(module.meta.clone()))?;

    // 4. Insert top-level constant expression (i.e. expressions that does not have function call)
    let typechecked_let_statements = let_statements
        .into_iter()
        .map(|let_statement| {
            if let Some(position) = has_direct_function_call(&let_statement.right) {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::FunctionCallAtTopLevelIsNotAllowed,
                })
            } else {
                let expected_type =
                    get_expected_type(&mut module, None, let_statement.type_annotation)?;
                let right =
                    infer_expression_type(&mut module, expected_type, &let_statement.right)?;
                let left = infer_destructure_pattern(
                    &mut module,
                    Some(right.type_value),
                    &let_statement.left,
                    let_statement.keyword_export.is_some(),
                )?;
                Ok(TypecheckedStatement::Let {
                    exported: let_statement.keyword_export.is_some(),
                    left: left.destructure_pattern,
                    right: right.expression,
                })
            }
        })
        .collect::<Result<Vec<TypecheckedStatement>, UnifyError>>()
        .map_err(|e| e.into_compile_error(module.meta.clone()))?;

    // 5. Insert top-level functions into the current module.
    //    Similar as type symbols, we will insert them into the module first before checking their definition,
    //    this is to that mutually recursive functions can be checked.
    let function_statements = function_statements.into_iter().map(|function_statement| {
        let type_value = {
            module.run_in_new_child_scope(|module| {
                function_statement
                    .arrow_function
                    .type_variables
                    .iter()
                    .map(|type_variable_name| module.insert_explicit_type_variable(type_variable_name))
                    .collect::<Result<Vec<SymbolUid>, UnifyError>>()?;

                let type_value = {
                        let parameters_types = function_statement
                            .arrow_function
                            .clone()
                            .parameters
                            .parameters()
                            .into_vector()
                            .into_iter()
                            .map(|parameter| {
                                match *parameter.type_annotation {
                                    None => {
                                        Err(UnifyError {
                                            position: get_destructure_pattern_position(&parameter.pattern),
                                            kind: UnifyErrorKind::MissingParameterTypeAnnotationForTopLevelFunction
                                        })
                                    }
                                    Some(type_annotation) => {
                                        type_annotation_to_type(module, &type_annotation)
                                    }
                                }
                            })
                            .collect::<Result<Vec<Type>, UnifyError>>()?;

                        let return_type = match &function_statement.arrow_function.return_type_annotation {
                            Some(type_annotation) => type_annotation_to_type(module, &type_annotation),
                            None => Err(UnifyError {
                                position: match &function_statement.arrow_function.parameters {
                                    ArrowFunctionParameters::WithoutParenthesis(pattern) => {
                                        get_destructure_pattern_position(pattern)
                                    }
                                    ArrowFunctionParameters::WithParenthesis(parameters) => {
                                        parameters.right_parenthesis.position
                                    }
                                },
                                kind: UnifyErrorKind::MissingReturnTypeAnnotationForTopLevelFunction
                            })
                        }?;
                        Type::Function(FunctionType {
                            parameters_types: Box::new(match parameters_types.split_first() {
                                // TODO: decide the following question
                                None => panic!("should function has at least one parameter?"),
                                Some((head, tail)) => {
                                    Ok( NonEmpty { head: head.clone(), tail: (*tail).to_vec() })
                                }
                            }?),
                            return_type: Box::new(return_type),
                        })
                    };

                let type_variables = function_statement
                        .arrow_function
                        .type_variables
                        .iter()
                        .map(|type_variable| type_variable.representation.clone())
                        .collect::<Vec<String>>();
                Ok(try_lift_as_type_scheme(type_value, type_variables))
            })
        }?;

        let uid = module.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: function_statement.name.clone(),
                    exported: function_statement.keyword_export.is_some(),
                },
                kind: SymbolKind::Value(ValueSymbol {
                    type_value: type_value.clone(),
                }),
            },
        )?;
        Ok((
            function_statement.keyword_export.is_some(),
            uid,
            function_statement.name,
            function_statement.arrow_function,
            type_value,
        ))
    })
        .collect::<Result<Vec<_>, UnifyError>>()
        .map_err(|error| error.into_compile_error(module.meta.clone()))?;

    let typechecked_function_statements = function_statements
        .into_iter()
        .map(|(exported, uid, name, arrow_function, expected_type)| {
            module.run_in_new_child_scope(|module| {
                // Populate type variables
                arrow_function
                    .type_variables
                    .iter()
                    .map(|type_variable_name| {
                        module.insert_explicit_type_variable(type_variable_name)
                    })
                    .collect::<Result<Vec<SymbolUid>, UnifyError>>()?;

                let result = infer_arrow_function(
                    module,
                    Some(expected_type.clone()),
                    arrow_function.clone(),
                )?;
                // Return the typechecked statement, which is needed for transpilation
                Ok(TypecheckedStatement::Let {
                    exported,
                    left: TypecheckedDestructurePattern::Identifier(Box::new(Identifier {
                        uid: uid.clone(),
                        token: name.clone(),
                    })),
                    right: result.expression,
                })
            })
        })
        .collect::<Result<Vec<TypecheckedStatement>, UnifyError>>()
        .map_err(|unify_error| unify_error.into_compile_error(module.meta.clone()))?;

    // 6. Lastly we will infer do statements
    let typechecked_top_level_expressions_statements = top_level_expressions
        .into_iter()
        .map(
            |expression| match infer_expression_type(&mut module, Some(Type::Null), &expression) {
                Ok(result) => Ok(TypecheckedStatement::Expression(result.expression)),
                Err(unify_error) => Err(unify_error.into_compile_error(module.meta.clone())),
            },
        )
        .collect::<Result<Vec<TypecheckedStatement>, CompileError>>()?;
    Ok(UnifyProgramResult {
        entrypoint: TypecheckedModule {
            module,
            statements: {
                let mut statements = Vec::new();
                statements.extend(typechecked_import_statements);
                statements.extend(typechecked_let_statements);
                statements.extend(typechecked_function_statements);
                statements.extend(typechecked_top_level_expressions_statements);
                statements
            },
        },
        imported_modules,
    })
}

/// Check that an expression does not have direct function call
/// If found, return the position of the function call
fn has_direct_function_call(expression: &Expression) -> Option<Position> {
    match expression {
        Expression::Null(_)
        | Expression::Boolean { .. }
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::Character(_)
        | Expression::Identifier(_)
        | Expression::String(_)
        | Expression::BranchedFunction(_)
        | Expression::ArrowFunction(_) => None,
        Expression::FunctionCall(function_call) => {
            Some(get_expression_position(&function_call.function))
        }
        Expression::With(with_expression) => Some(with_expression.binary_function_name.position),
        Expression::UnsafeJavascript { code } => Some(code.position),
        Expression::InterpolatedString { sections, .. } => {
            sections.find_map(|section| match section {
                InterpolatedStringSection::Expression(expression) => {
                    has_direct_function_call(expression.as_ref())
                }
                _ => None,
            })
        }
        Expression::Quoted { expression, .. } => has_direct_function_call(expression),
        Expression::EnumConstructor { payload, .. } => match payload {
            None => None,
            Some(expression) => has_direct_function_call(&expression.expression),
        },
        Expression::Record {
            key_value_pairs, ..
        } => key_value_pairs
            .iter()
            .find_map(|pair| has_direct_function_call(&pair.value)),
        Expression::RecordAccess { expression, .. } => has_direct_function_call(&expression),
        Expression::RecordUpdate {
            expression,
            updates,
            ..
        } => has_direct_function_call(&expression).or_else(|| {
            updates.iter().find_map(|update| match update {
                RecordUpdate::ValueUpdate { new_value, .. } => has_direct_function_call(new_value),
                RecordUpdate::FunctionalUpdate { function, .. } => {
                    Some(get_expression_position(function))
                }
            })
        }),
        Expression::Array { elements, .. } => elements.iter().find_map(has_direct_function_call),
        Expression::If {
            condition,
            if_true,
            if_false,
            ..
        } => has_direct_function_call(condition)
            .or_else(|| has_direct_function_call(if_true))
            .or_else(|| has_direct_function_call(if_false)),
        Expression::Switch {
            expression, cases, ..
        } => has_direct_function_call(&expression)
            .or_else(|| cases.find_map(|case| has_direct_function_call(&case.body))),
        Expression::Block(block) => {
            block
                .clone()
                .statements()
                .iter()
                .find_map(|statement| match statement {
                    Statement::Let(let_statement) => has_direct_function_call(&let_statement.right),
                    Statement::Expression(expression) => has_direct_function_call(expression),
                    Statement::Type(_) => None,
                    Statement::Enum(_) => None,
                    Statement::Import(_) => None,
                })
        }
    }
}

#[derive(Debug)]
pub struct UnifyError {
    pub position: Position,
    pub kind: UnifyErrorKind,
}

impl UnifyError {
    pub fn into_compile_error(self, module_meta: ModuleMeta) -> CompileError {
        CompileError {
            module_meta,
            kind: CompileErrorKind::UnifyError(Box::new(self)),
        }
    }
}

#[derive(Debug)]
pub enum UnifyErrorKind {
    NotExpectingTypeVariable,
    FunctionCallAtTopLevelIsNotAllowed,
    NotExpectingFunction {
        expected_type: Type,
    },
    MissingParameterTypeAnnotationForFunctionTypeAnnotation,
    MissingParameterTypeAnnotationForTopLevelFunction,
    MissingReturnTypeAnnotationForTopLevelFunction,
    MissingTypeAnnotationForRecordWildcard,
    LetBindingRefutablePattern {
        missing_patterns: Vec<ExpandablePattern>,
    },
    WithExpressionBindFunctionConditionNotMet {
        actual_type: Type,
    },
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
    DuplicatedRecordKey,
    InfiniteTypeDetected {
        type_variable_name: String,
        in_type: Type,
    },
    CannotAccessPropertyOfNonRecord,
    NoSuchProperty {
        expected_keys: Vec<String>,
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
    UnknownValueSymbol {
        symbol_name: String,
    },
    UnknownEnumConstructor,
    TypeMismatch {
        expected_type: Type,
        actual_type: Type,
    },
}

/// We use IndexMap instead of HashMap for storing imported modules because we need to preserve the insertion order,
/// in order for the transpiled code to work (i.e. to prevent referencing variable that is not yet declared).
/// Of course, this only work if the dependency graph contains no cycle,
/// fortunately, this language do not allow cyclic imports.
type ImportedModules = IndexMap<ModuleUid, TypecheckedModule>;

#[derive(Debug, Clone)]
pub struct TypecheckedModule {
    pub module: Module,
    pub statements: Vec<TypecheckedStatement>,
}
pub struct InferImportStatementResult {
    pub import_statements: Vec<TypecheckedImportStatement>,
    pub imported_modules: ImportedModules,
}

pub fn infer_import_statement(
    module: &mut Module,
    imported_modules: &ImportedModules,
    ImportStatement {
        url, import_type, ..
    }: ImportStatement,
) -> Result<InferImportStatementResult, CompileError> {
    let importer_path = module.uid().string_value();
    let import_path = url.representation.trim_matches('"');
    let path = RelativePath::new(&importer_path)
        .parent()
        .expect("Should always have parent")
        .join(RelativePath::new(import_path))
        .normalize();
    let path_string = path.to_string();
    if module
        .meta
        .import_relations
        .iter()
        .any(|relation| path_string == relation.importer_path)
    {
        return Err(UnifyError {
            position: url.position,
            kind: UnifyErrorKind::CyclicDependency {
                import_relations: {
                    let mut relations = module.meta.import_relations.clone();
                    relations.push(ImportRelation {
                        importer_path,
                        importee_path: path_string,
                    });
                    relations
                },
            },
        }
        .into_compile_error(module.meta.clone()));
    }
    let relative_path = path.to_path(Path::new("."));
    let uid = ModuleUid::Local {
        relative_path: path.to_string(),
    };

    // Look up for memoize imported modules
    let imported = match imported_modules.get(&uid) {
        Some(module) => {
            println!("Compile cache hit: {}", uid.string_value());
            UnifyProgramResult {
                entrypoint: module.clone(),
                imported_modules: IndexMap::new(),
            }
        }
        None => {
            match fs::read_to_string(relative_path) {
                Ok(code) => {
                    let module_meta = ModuleMeta {
                        uid: uid.clone(),
                        code: code.clone(),
                        import_relations: {
                            let mut importer_paths = module.meta.import_relations.clone();
                            importer_paths.push(ImportRelation {
                                importer_path: module.meta.uid.string_value(),
                                importee_path: path_string,
                            });
                            importer_paths
                        },
                    };

                    // Parse the imported module
                    let statements = match Parser::parse(&mut Tokenizer::new(code)) {
                        Ok(statements) => Ok(statements),
                        Err(parse_error) => Err(CompileError {
                            module_meta: module_meta.clone(),
                            kind: CompileErrorKind::ParseError(Box::new(parse_error)),
                        }),
                    }?;

                    // Typecheck the imported module
                    unify_statements(module_meta, statements, imported_modules, false)?
                }
                Err(error) => {
                    return Err(UnifyError {
                        position: url.position,
                        kind: UnifyErrorKind::ErrorneousImportPath {
                            extra_information: format!("{}", error),
                        },
                    }
                    .into_compile_error(module.meta.clone()))
                }
            }
        }
    };

    // Check whether each imported name exists and is exported in this program
    let matching_symbol_entries: Vec<(SymbolEntry, Option<Token>)> = {
        match import_type {
            ImportType::All { asterisk } => imported
                .entrypoint
                .module
                .get_all_exported_symbols()
                .into_iter()
                .map(|entry| {
                    let name = entry.symbol.meta.name.clone();
                    (
                        entry,
                        Some(Token {
                            position: asterisk.position,
                            ..name
                        }),
                    )
                })
                .collect(),
            ImportType::Selected { imported_names } => imported_names
                .into_vector()
                .into_iter()
                .map(|imported_name| {
                    let matching_symbol_entries = imported
                        .entrypoint
                        .module
                        .get_all_matching_symbols(&imported_name.name);

                    if matching_symbol_entries.is_empty() {
                        Err(UnifyError {
                            position: imported_name.name.position,
                            kind: UnifyErrorKind::UnknownImportedName,
                        }
                        .into_compile_error(module.meta.clone()))
                    } else {
                        Ok(matching_symbol_entries
                            .into_iter()
                            .map(|entry| {
                                if !entry.symbol.meta.exported {
                                    Err(UnifyError {
                                        position: imported_name.name.position,
                                        kind: UnifyErrorKind::CannotImportPrivateSymbol,
                                    }
                                    .into_compile_error(module.meta.clone()))
                                } else {
                                    Ok((entry, imported_name.alias_as.clone()))
                                }
                            })
                            .collect::<Result<Vec<(SymbolEntry, Option<Token>)>, CompileError>>()?)
                    }
                })
                .collect::<Result<Vec<Vec<(SymbolEntry, Option<Token>)>>, CompileError>>()?
                .into_iter()
                .flatten()
                .collect(),
        }
    };

    let import_statements = matching_symbol_entries
        .into_iter()
        .map(|(entry, alias_as)| {
            let alias_as = alias_as.unwrap_or_else(|| entry.symbol.meta.name.clone());
            match module.insert_symbol(
                Some(entry.uid.clone()),
                Symbol {
                    meta: SymbolMeta {
                        name: alias_as.clone(),
                        exported: false,
                    },
                    kind: entry.symbol.kind.clone(),
                },
            ) {
                Ok(uid) => {
                    // only insert import statement if the imported symbol is a value (i.e. not type)
                    match entry.symbol.kind {
                        SymbolKind::Value(_) => Ok(vec![TypecheckedImportStatement {
                            module_uid: imported.entrypoint.module.uid(),
                            imported_name: Identifier {
                                uid: entry.uid.clone(),
                                token: entry.symbol.meta.name,
                            },
                            imported_as: Identifier {
                                uid,
                                token: alias_as,
                            },
                        }]),
                        _ => Ok(vec![]),
                    }
                }
                Err(unify_error) => Err(unify_error.into_compile_error(module.meta.clone())),
            }
        })
        .collect::<Result<Vec<Vec<TypecheckedImportStatement>>, CompileError>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(InferImportStatementResult {
        import_statements,
        imported_modules: {
            let mut imported_modules = imported.imported_modules;
            let uid = imported.entrypoint.module.uid();
            imported_modules.insert(uid, imported.entrypoint);
            imported_modules
        },
    })
}

/// Insert the symbol of an enum into the current module without checking it's body.  
/// Returns the UID of this enum.
pub fn insert_enum_symbol(
    module: &mut Module,
    EnumStatement {
        name,
        type_variables,
        keyword_export,
        ..
    }: EnumStatement,
) -> Result<SymbolUid, UnifyError> {
    let enum_uid = module.get_next_symbol_uid();
    let enum_name = name.representation.clone();
    let enum_type = Type::Named {
        symbol_uid: enum_uid.clone(),
        name: enum_name,
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
        .into_iter()
        .map(|type_variable| type_variable.representation)
        .collect::<Vec<String>>();

    let type_value = match type_variable_names.split_first() {
        None => enum_type,
        Some((head, tail)) => Type::TypeScheme(Box::new(TypeScheme {
            type_variables: NonEmpty {
                head: head.clone(),
                tail: tail.to_vec(),
            },
            type_value: enum_type,
        })),
    };

    module.insert_symbol(
        Some(enum_uid),
        Symbol {
            meta: SymbolMeta {
                name,
                exported: keyword_export.is_some(),
            },
            kind: SymbolKind::Type(TypeSymbol { type_value }),
        },
    )
}

/// Validate the body of an enum statement and insert the constructors into the current module
pub fn infer_enum_statement(
    module: &mut Module,
    enum_uid: &SymbolUid,
    EnumStatement {
        name,
        constructors,
        type_variables,
        keyword_export,
        ..
    }: EnumStatement,
) -> Result<(), UnifyError> {
    // 1. Populate type variables into current module
    let constructor_symbols = module.run_in_new_child_scope(|module| {
        for type_variable in type_variables.clone() {
            module.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: type_variable.clone(),
                        exported: false,
                    },
                    kind: SymbolKind::Type(TypeSymbol {
                        type_value: Type::ImplicitTypeVariable {
                            name: type_variable.clone().representation,
                        },
                    }),
                },
            )?;
        }
        // 2. Add each tags into the enum namespace
        let constructor_symbols = constructors
            .iter()
            .map(|constructor| {
                Ok(Symbol {
                    meta: SymbolMeta {
                        name: constructor.name.clone(),
                        exported: keyword_export.is_some(),
                    },
                    kind: SymbolKind::EnumConstructor(EnumConstructorSymbol {
                        enum_uid: enum_uid.clone(),
                        enum_name: name.representation.clone(),
                        constructor_name: constructor.name.representation.clone(),
                        type_variables: type_variables
                            .clone()
                            .into_iter()
                            .map(|type_variable| type_variable.representation)
                            .collect(),
                        payload: match &constructor.payload {
                            None => None,
                            Some(payload) => {
                                // validate type annotation
                                let payload_type_value =
                                    type_annotation_to_type(module, &payload.type_annotation)?;
                                Some(payload_type_value)
                            }
                        },
                    }),
                })
            })
            .collect::<Result<Vec<Symbol>, UnifyError>>()?;
        Ok(constructor_symbols)
    })?;

    constructor_symbols
        .into_iter()
        .map(|constructor_symbol| module.insert_symbol(None, constructor_symbol))
        .collect::<Result<Vec<_>, UnifyError>>()?;

    Ok(())
}

/// Validate the body of a type alias statement
pub fn infer_type_alias_statement(
    module: &mut Module,
    TypeAliasStatement {
        keyword_export,
        left,
        right,
        type_variables,
        ..
    }: TypeAliasStatement,
) -> Result<(), UnifyError> {
    // 1. Populate type variables into current module
    let type_value = module.run_in_new_child_scope(|module| {
        for type_variable in type_variables.clone() {
            module.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: type_variable.clone(),
                        exported: false,
                    },
                    kind: SymbolKind::Type(TypeSymbol {
                        type_value: Type::ImplicitTypeVariable {
                            name: type_variable.clone().representation,
                        },
                    }),
                },
            )?;
        }

        // 2. verify type declaration
        type_annotation_to_type(module, &right)
    })?;
    // 3. Add this type symbol into this module
    module.insert_symbol(
        None,
        Symbol {
            meta: SymbolMeta {
                name: left,
                exported: keyword_export.is_some(),
            },
            kind: SymbolKind::Type(TypeSymbol {
                type_value: try_lift_as_type_scheme(
                    type_value,
                    type_variables
                        .iter()
                        .map(|type_variable| type_variable.representation.clone())
                        .collect(),
                ),
            }),
        },
    )?;

    Ok(())
}

pub fn try_lift_as_type_scheme(type_value: Type, type_variables: Vec<String>) -> Type {
    match type_variables.split_first() {
        None => type_value,
        Some((head, tail)) => Type::TypeScheme(Box::new(TypeScheme {
            type_value,
            type_variables: NonEmpty {
                head: head.clone(),
                tail: tail.to_vec(),
            },
        })),
    }
}

/// @deprecated
/// This function is left here: for reference purpose
pub fn generalize_type(type_value: Type) -> TypeScheme {
    let type_variables = get_free_type_variables_in_type(&type_value);
    TypeScheme {
        type_variables: panic!(), // type_variables.into_iter().collect(),
        type_value,
    }
}

pub fn get_type_annotation_position(type_annotation: &TypeAnnotation) -> Position {
    match type_annotation {
        TypeAnnotation::Underscore(token) => token.position,
        TypeAnnotation::Function {
            parameters,
            return_type,
            ..
        } => join_position(
            parameters.left_parenthesis.position,
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
        TypeAnnotation::Quoted {
            opening_backtick,
            closing_backtick,
            ..
        } => join_position(opening_backtick.position, closing_backtick.position),
        TypeAnnotation::Promise {
            bang_token,
            type_annotation,
        } => join_position(
            bang_token.position,
            get_type_annotation_position(type_annotation),
        ),
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
        | Expression::Identifier(token)
        | Expression::Null(token)
        | Expression::Boolean { token, .. } => token.position,
        Expression::InterpolatedString {
            start_quote,
            end_quote,
            ..
        } => join_position(start_quote.position(), end_quote.position()),
        Expression::Quoted {
            opening_backtick,
            closing_backtick,
            ..
        } => join_position(opening_backtick.position, closing_backtick.position),
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
                None => get_expression_position(&function_call.function),
            };
            join_position(start_position, end_position)
        }
        Expression::BranchedFunction(function) => {
            let start_position = function.branches.first().start_token.position;
            let end_position = get_expression_position(&function.branches.last().body);
            join_position(start_position, end_position)
        }
        Expression::UnsafeJavascript { code } => code.position,
        Expression::With(with_expression) => join_position(
            with_expression.keyword_with.position,
            get_expression_position(&with_expression.body),
        ),
        Expression::If {
            keyword_if,
            if_false,
            ..
        } => join_position(keyword_if.position, get_expression_position(if_false)),
        Expression::Switch {
            keyword_switch,
            right_curly_bracket,
            ..
        } => join_position(keyword_switch.position, right_curly_bracket.position),
        Expression::ArrowFunction(new_function) => {
            let start_position = match &new_function.parameters {
                ArrowFunctionParameters::WithoutParenthesis(pattern) => {
                    get_destructure_pattern_position(&pattern)
                }
                ArrowFunctionParameters::WithParenthesis(parameters) => {
                    parameters.left_parenthesis.position
                }
            };
            join_position(
                start_position,
                get_expression_position(new_function.body.as_ref()),
            )
        }
        Expression::Block(block) => match block {
            Block::WithBrackets {
                left_curly_bracket,
                right_curly_bracket,
                ..
            } => join_position(left_curly_bracket.position, right_curly_bracket.position),
            Block::WithoutBrackets { statements } => join_position(
                get_statement_position(statements.first()),
                get_statement_position(statements.last()),
            ),
        },
    }
}

fn get_statement_position(statement: &Statement) -> Position {
    match statement {
        Statement::Let(let_statement) => join_position(
            let_statement.keyword_let.position,
            get_expression_position(&let_statement.right),
        ),
        Statement::Expression(expression) => get_expression_position(expression),
        Statement::Type(type_statement) => join_position(
            type_statement.keyword_type.position,
            get_type_annotation_position(&type_statement.right),
        ),
        Statement::Enum(enum_statement) => join_position(
            enum_statement.keyword_enum.position,
            enum_statement.right_curly_bracket.position,
        ),
        Statement::Import(import_statement) => join_position(
            import_statement.keyword_import.position,
            import_statement.url.position,
        ),
    }
}

pub fn try_unify_type(
    module: &mut Module,
    expected: Option<Type>,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match expected {
        None => Ok(actual.clone()),
        Some(expected) => unify_type(module, &expected, actual, position),
    }
}

pub fn unify_type(
    module: &mut Module,
    expected: &Type,
    actual: &Type,
    position: Position,
) -> Result<Type, UnifyError> {
    match unify_type_(module, expected, actual, position) {
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
                expected_type: module.apply_subtitution_to_type(&expected_type),
                actual_type: module.apply_subtitution_to_type(&actual_type),
            },
        }),
        other => other,
    }
}

pub fn unify_type_(
    module: &mut Module,
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
        (
            Type::BuiltInOneArgumentType {
                kind: expected_kind,
                type_argument: expected_type_argument,
            },
            Type::BuiltInOneArgumentType {
                kind: actual_kind,
                type_argument: actual_type_argument,
            },
        ) => {
            let unify_error = UnifyError {
                position,
                kind: UnifyErrorKind::TypeMismatch {
                    expected_type: Type::BuiltInOneArgumentType {
                        kind: expected_kind.clone(),
                        type_argument: expected_type_argument.clone(),
                    },
                    actual_type: Type::BuiltInOneArgumentType {
                        kind: actual_kind.clone(),
                        type_argument: actual_type_argument.clone(),
                    },
                },
            };
            if expected_kind != actual_kind {
                Err(unify_error)
            } else {
                match unify_type(
                    module,
                    expected_type_argument.as_ref(),
                    actual_type_argument.as_ref(),
                    position,
                ) {
                    Ok(type_value) => Ok(Type::BuiltInOneArgumentType {
                        kind: expected_kind,
                        type_argument: Box::new(type_value),
                    }),
                    Err(UnifyError {
                        kind: UnifyErrorKind::TypeMismatch { .. },
                        ..
                    }) => Err(unify_error),
                    Err(error) => Err(error),
                }
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
                            unify_type(module, &expected_type, &actual_type, position)
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
                let expected_type =
                    module.get_type_variable_terminal_type(expected_type_variable_name.clone());
                let actual_type =
                    module.get_type_variable_terminal_type(actual_type_variable_name.clone());

                match (expected_type, actual_type) {
                    (None, Some(actual_type)) => {
                        module.update_substitution(
                            expected_type_variable_name,
                            actual_type.clone(),
                            position,
                        )?;
                        Ok(actual_type)
                    }
                    (Some(expected_type), None) => {
                        module.update_substitution(
                            actual_type_variable_name,
                            expected_type.clone(),
                            position,
                        )?;
                        Ok(expected_type)
                    }
                    (Some(expected_type), Some(actual_type)) => {
                        unify_type(module, &expected_type, &actual_type, position)
                    }
                    (None, None) => {
                        module.update_substitution(
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
                module.update_substitution(name, other_type.clone(), position)?;
                Ok(other_type)
            }
        }
        (Type::Function(expected_function), Type::Function(actual_function)) => {
            let function_type =
                unify_function_type(module, &expected_function, &actual_function, position)?;
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
            let expected_keys = expected_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key)
                .collect::<Vec<String>>();

            let actual_keys = actual_key_type_pairs
                .clone()
                .into_iter()
                .map(|(key, _)| key);

            // 1. Find for missing keys
            let missing_keys: Vec<String> = expected_keys
                .clone()
                .into_iter()
                .filter(|expected_key| {
                    !actual_keys
                        .clone()
                        .any(|actual_key| **expected_key == *actual_key)
                })
                .collect();

            if !missing_keys.is_empty() {
                return Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                });
            }

            // 2. Find for extraneous keys
            let extraneous_keys: Vec<String> = actual_keys
                .into_iter()
                .filter(|actual_key| {
                    !expected_keys
                        .iter()
                        .any(|expected_key| *expected_key == *actual_key)
                })
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
                    match unify_type(module, &expected_type, &actual_type, position) {
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
        (Type::TypeScheme(expected_type_scheme), Type::TypeScheme(actual_type_scheme)) => {
            // The current implementation does not consider alpha conversion
            // TODO: compare properly using Alpha Equivalence
            //       refer https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence
            if expected_type_scheme
                .type_variables
                .ne(&actual_type_scheme.type_variables)
            {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::TypeScheme(expected_type_scheme),
                        actual_type: Type::TypeScheme(actual_type_scheme),
                    },
                })
            } else {
                unify_type(
                    module,
                    &expected_type_scheme.type_value,
                    &actual_type_scheme.type_value,
                    position,
                )
            }
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
        Type::ImplicitTypeVariable { name } => {
            if name == *from_type_variable {
                to_type.clone()
            } else {
                Type::ImplicitTypeVariable { name }
            }
        }
        Type::BuiltInOneArgumentType {
            kind,
            type_argument,
        } => Type::BuiltInOneArgumentType {
            kind,
            type_argument: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *type_argument,
            )),
        },
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
        Type::TypeScheme(type_scheme) => Type::TypeScheme(Box::new(TypeScheme {
            type_variables: type_scheme.type_variables,
            type_value: rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                type_scheme.type_value,
            ),
        })),
    }
}

pub struct GetEnumTypeResult {
    pub expected_enum_type: Type,
    pub expected_payload_type: Option<Type>,
}

pub fn get_enum_type(
    module: &mut Module,
    expected_type: Option<Type>,
    token: &Token,
) -> Result<GetEnumTypeResult, UnifyError> {
    let expected_enum_uid = match &expected_type {
        Some(Type::Named { symbol_uid, .. }) => Some(symbol_uid.clone()),
        _ => None,
    };
    // Look up constructor
    let constructor = module.get_constructor_symbol(expected_enum_uid, &token)?;

    let enum_type = module
        .get_type_symbol_by_uid(&constructor.enum_uid)
        .unwrap_or_else(|| panic!("Compiler error, cannot find enum type of a constructor"));

    match enum_type.type_value {
        Type::TypeScheme(enum_type) => {
            // initiate type variables
            let instantiated_type_variables = {
                match expected_type {
                    Some(Type::Named {
                        type_arguments,
                        symbol_uid,
                        ..
                    }) if symbol_uid == constructor.enum_uid => type_arguments,
                    _ => enum_type
                        .type_variables
                        .clone()
                        .into_vector()
                        .iter()
                        .map(|type_variable_name| {
                            (
                                type_variable_name.clone(),
                                Type::ImplicitTypeVariable {
                                    name: module.get_next_type_variable_name(),
                                },
                            )
                        })
                        .collect::<Vec<(String, Type)>>(),
                }
            };

            let expected_enum_type = rewrite_type_variables_in_type(
                instantiated_type_variables.clone(),
                enum_type.type_value,
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
        _ => Ok(GetEnumTypeResult {
            expected_enum_type: enum_type.type_value,
            expected_payload_type: constructor.payload,
        }),
    }
}

pub struct TypeVariableSubstitution {
    pub from_type_variable: String,
    pub to_type: Type,
}
type TypeVariableSubstitutions = Vec<TypeVariableSubstitution>;

#[derive(Debug)]
struct InferExpressionResult {
    expression: TypecheckedExpression,
    type_value: Type,
}

fn infer_expression_type(
    module: &mut Module,
    expected_type: Option<Type>,
    expression: &Expression,
) -> Result<InferExpressionResult, UnifyError> {
    // NOTE: this part might be a little inefficient,
    // because we might unify the same expression more than once
    // We do this so that the programming part for this algorithm won't be repititive and tedious
    let result = infer_expression_type_(module, expected_type.clone(), expression)?;

    let type_value = try_unify_type(
        module,
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
    module: &mut Module,
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
        Expression::InterpolatedString { sections, .. } => Ok(InferExpressionResult {
            type_value: Type::String,
            expression: TypecheckedExpression::InterpolatedString {
                sections: sections
                    .clone()
                    .into_vector()
                    .into_iter()
                    .map(|section| match section {
                        InterpolatedStringSection::String(string) => {
                            Ok(TypecheckedInterpolatedStringSection::String(string))
                        }
                        InterpolatedStringSection::Expression(expression) => {
                            let expression = infer_expression_type(
                                module,
                                Some(Type::String),
                                expression.as_ref(),
                            )?;
                            Ok(TypecheckedInterpolatedStringSection::Expression(Box::new(
                                expression.expression,
                            )))
                        }
                    })
                    .collect::<Result<Vec<TypecheckedInterpolatedStringSection>, UnifyError>>()?,
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
        Expression::Quoted {
            expression,
            opening_backtick,
            closing_backtick,
        } => {
            let position = join_position(opening_backtick.position, closing_backtick.position);
            let expected_type = match expected_type {
                Some(Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Quoted,
                    type_argument,
                }) => Some(*type_argument),
                _ => None,
            };
            let result = infer_expression_type(module, expected_type, expression)?;
            Ok(InferExpressionResult {
                type_value: Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Quoted,
                    type_argument: Box::new(result.type_value),
                },
                expression: TypecheckedExpression::Record {
                    key_value_pairs: vec![
                        (
                            PropertyName(Token::dummy_identifier("value".to_string())),
                            result.expression,
                        ),
                        (
                            PropertyName(Token::dummy_identifier("meta".to_string())),
                            TypecheckedExpression::Record {
                                key_value_pairs: vec![
                                    (
                                        PropertyName(Token::dummy_identifier(
                                            "filename".to_string(),
                                        )),
                                        TypecheckedExpression::String {
                                            representation: format!(
                                                "\"{}\"",
                                                module.meta.uid.string_value(),
                                            ),
                                        },
                                    ),
                                    (
                                        PropertyName(Token::dummy_identifier(
                                            "line_start".to_string(),
                                        )),
                                        TypecheckedExpression::Integer {
                                            representation: position.line_start.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName(Token::dummy_identifier(
                                            "line_end".to_string(),
                                        )),
                                        TypecheckedExpression::Integer {
                                            representation: position.line_end.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName(Token::dummy_identifier(
                                            "column_start".to_string(),
                                        )),
                                        TypecheckedExpression::Integer {
                                            representation: position.column_start.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName(Token::dummy_identifier(
                                            "column_end".to_string(),
                                        )),
                                        TypecheckedExpression::Integer {
                                            representation: position.column_end.to_string(),
                                        },
                                    ),
                                ],
                            },
                        ),
                    ],
                },
            })
        }
        Expression::EnumConstructor { name, payload, .. } => {
            let result = get_enum_type(module, expected_type, name)?;
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
                        module,
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
        Expression::Identifier(variable) => {
            // check if the variable name matches any constructor
            if module.matches_some_enum_constructor(&variable.representation) {
                infer_expression_type_(
                    module,
                    expected_type,
                    &Expression::EnumConstructor {
                        name: variable.clone(),
                        payload: None,
                    },
                )
            } else {
                let result = module.get_value_symbol(
                    &variable,
                    &expected_type,
                    module.current_scope_name(),
                )?;
                Ok(InferExpressionResult {
                    type_value: match result.type_value {
                        Type::TypeScheme(type_scheme) => {
                            instantiate_type_scheme(module, *type_scheme)
                        }
                        _ => result.type_value,
                    },
                    expression: TypecheckedExpression::Variable(Identifier {
                        uid: result.symbol_uid,
                        token: variable.clone(),
                    }),
                })
            }
        }
        Expression::RecordAccess {
            expression,
            property_name,
        } => {
            let result = infer_expression_type(module, None, expression)?;
            let key_type_pairs = match result.type_value {
                // Re-pack quoted type as record
                Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Quoted,
                    type_argument,
                } => {
                    vec![
                        ("value".to_string(), *type_argument),
                        (
                            "meta".to_string(),
                            Type::Record {
                                key_type_pairs: vec![
                                    ("filename".to_string(), Type::String),
                                    ("line_start".to_string(), Type::Integer),
                                    ("line_end".to_string(), Type::Integer),
                                    ("column_start".to_string(), Type::Integer),
                                    ("column_end".to_string(), Type::Integer),
                                ],
                            },
                        ),
                    ]
                }
                Type::Record { key_type_pairs } => key_type_pairs,
                _ => {
                    return Err(UnifyError {
                        position: property_name.position,
                        kind: UnifyErrorKind::CannotAccessPropertyOfNonRecord,
                    })
                }
            };

            match key_type_pairs
                .iter()
                .find(|(key, _)| *key == property_name.representation)
            {
                None => {
                    // Try to infer as function call
                    match infer_expression_type(
                        module,
                        expected_type,
                        &Expression::FunctionCall(Box::new(FunctionCall {
                            function: Box::new(Expression::Identifier(property_name.clone())),
                            first_argument: expression.clone(),
                            rest_arguments: None,
                            type_arguments: None,
                        })),
                    ) {
                        Ok(result) => Ok(result),
                        Err(UnifyError {
                            kind: UnifyErrorKind::UnknownValueSymbol { .. },
                            ..
                        }) => Err(UnifyError {
                            position: property_name.position,
                            kind: UnifyErrorKind::NoSuchProperty {
                                expected_keys: key_type_pairs
                                    .iter()
                                    .map(|(key, _)| key.clone())
                                    .collect(),
                            },
                        }),

                        Err(other_error) => Err(other_error),
                    }
                }
                Some((_, type_value)) => Ok(InferExpressionResult {
                    type_value: type_value.clone(),
                    expression: TypecheckedExpression::RecordAccess {
                        expression: Box::new(result.expression),
                        property_name: PropertyName(property_name.clone()),
                    },
                }),
            }
        }

        Expression::RecordUpdate {
            expression,
            updates,
            ..
        } => {
            let typechecked_expression = infer_expression_type(module, expected_type, expression)?;

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
                                    kind: UnifyErrorKind::NoSuchProperty {
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
                                            module,
                                            Some(expected_type.clone()),
                                            new_value,
                                        )?;
                                        Ok(TypecheckedRecordUpdate::ValueUpdate {
                                            property_name: PropertyName(property_name.clone()),
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
                                            module,
                                            Some(expected_type),
                                            function,
                                        )?;
                                        Ok(TypecheckedRecordUpdate::FunctionalUpdate {
                                            property_name: PropertyName(property_name.clone()),
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
        Expression::BranchedFunction(function) => {
            let expected_function_type = match expected_type {
                Some(Type::Function(function_type)) => Some(function_type),
                _ => None,
            };
            let typechecked_function = infer_function(module, expected_function_type, function)?;
            Ok(InferExpressionResult {
                type_value: Type::Function(typechecked_function.function_type),
                expression: TypecheckedExpression::BranchedFunction(Box::new(
                    typechecked_function.function,
                )),
            })
        }
        Expression::FunctionCall(function_call) => {
            // Get the type of the first argument, this is necessary for performing single-dispatch
            let typechecked_first_argument =
                infer_expression_type(module, None, function_call.first_argument.as_ref())?;

            // Get expected function type
            let expected_function_type = {
                // Note that we only infer the type of the first argument
                // The rest arguments is just instantiated with type variables
                let expected_rest_arguments_types = match &function_call.rest_arguments {
                    None => Ok(vec![]),
                    Some(rest_arguments) => rest_arguments
                        .arguments
                        .iter()
                        .map(|_| module.introduce_implicit_type_variable(None))
                        .collect::<Result<Vec<Type>, UnifyError>>(),
                }?;
                let expected_return_type = module.introduce_implicit_type_variable(None)?;
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
                module,
                Some(expected_function_type),
                function_call.function.as_ref(),
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
                                position: get_expression_position(function_call.function.as_ref()),
                                kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                                    actual_length: actual_arguments_length,
                                    expected_length: expected_arguments_length,
                                },
                            });
                        }
                    }

                    // Unify first argument
                    let typechecked_first_argument = infer_expression_type(
                        module,
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
                                module.apply_subtitution_to_type(expected_argument_type);
                            infer_expression_type(
                                module,
                                Some(expected_argument_type),
                                &actual_argument,
                            )
                        })
                        .collect::<Result<Vec<InferExpressionResult>, UnifyError>>()?;

                    Ok(InferExpressionResult {
                        type_value: module
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
                        name: module.get_next_type_variable_name(),
                    };

                    let position =
                        get_expression_position(&Expression::FunctionCall(function_call.clone()));

                    unify_type(
                        module,
                        &Type::ImplicitTypeVariable { name },
                        &expected_function_type,
                        position,
                    )?;

                    let return_type = Type::ImplicitTypeVariable {
                        name: module.get_next_type_variable_name(),
                    };

                    let typechecked_first_argument =
                        infer_expression_type(module, None, function_call.first_argument.as_ref())?;

                    let typechecked_rest_arguments = match &function_call.rest_arguments {
                        None => Vec::new(),
                        Some(FunctionCallRestArguments { arguments, .. }) => arguments
                            .clone()
                            .into_iter()
                            .map(|argument| infer_expression_type(module, None, &argument))
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
                        module,
                        &actual_function_type,
                        &expected_function_type,
                        position,
                    )?;

                    Ok(InferExpressionResult {
                        type_value: module.apply_subtitution_to_type(&return_type),
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
                    position: get_expression_position(&function_call.function),
                    kind: UnifyErrorKind::CannotInvokeNonFunction { actual_type: other },
                }),
            }
        }
        Expression::With(with_expression) => infer_with_expression_type(module, with_expression),
        Expression::Record {
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
            wildcard,
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

            let key_value_pairs = match wildcard {
                None => Ok(key_value_pairs.clone()),
                Some(wildcard) => match &expected_type {
                    None => Err(UnifyError {
                        position: wildcard.position,
                        kind: UnifyErrorKind::MissingTypeAnnotationForRecordWildcard,
                    }),
                    Some(Type::Record {
                        key_type_pairs: expected_key_type_pairs,
                    }) => Ok(key_value_pairs
                        .clone()
                        .into_iter()
                        .chain(expected_key_type_pairs.clone().into_iter().filter_map(
                            |(expected_key, _)| {
                                if key_value_pairs.iter().any(|actual_key_value| {
                                    actual_key_value.key.representation == *expected_key
                                }) {
                                    None
                                } else {
                                    let key = Token {
                                        position: wildcard.position,
                                        token_type: TokenType::Identifier,
                                        representation: expected_key.clone(),
                                    };
                                    Some(RecordKeyValue {
                                        key,
                                        value: Expression::Identifier(Token {
                                            position: wildcard.position,
                                            token_type: TokenType::Identifier,
                                            representation: expected_key,
                                        }),
                                    })
                                }
                            },
                        ))
                        .collect()),
                    _ => Ok(key_value_pairs.clone()),
                },
            }?;

            match expected_type {
                Some(Type::Record { key_type_pairs }) => infer_record_type(
                    module,
                    Some(key_type_pairs),
                    key_value_pairs,
                    record_position,
                ),
                _ => infer_record_type(module, None, key_value_pairs, record_position),
            }
        }
        Expression::Array { elements, .. } => {
            let element_type = match expected_type {
                Some(Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Array,
                    type_argument: expected_element_type,
                }) => Ok(*expected_element_type),
                _ => Ok(Type::ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                }),
            }?;
            let typechecked_elements = elements
                .iter()
                .map(|element| infer_expression_type(module, Some(element_type.clone()), element))
                .collect::<Result<Vec<InferExpressionResult>, UnifyError>>()?;
            Ok(InferExpressionResult {
                type_value: Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Array,
                    type_argument: Box::new(module.apply_subtitution_to_type(&element_type)),
                },
                expression: TypecheckedExpression::Array {
                    elements: typechecked_elements
                        .iter()
                        .map(|element| element.expression.clone())
                        .collect(),
                },
            })
        }
        Expression::UnsafeJavascript { code } => Ok(InferExpressionResult {
            type_value: module.introduce_implicit_type_variable(None)?,
            expression: TypecheckedExpression::Javascript {
                code: code.representation.clone(),
            },
        }),
        Expression::If {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let condition = infer_expression_type(module, Some(Type::Boolean), condition)?;
            let return_type = module.introduce_implicit_type_variable(None)?;
            let if_true = infer_expression_type(module, Some(return_type.clone()), if_true)?;
            let if_false = infer_expression_type(module, Some(return_type.clone()), if_false)?;

            Ok(InferExpressionResult {
                type_value: module.apply_subtitution_to_type(&return_type),
                expression: TypecheckedExpression::If {
                    condition: Box::new(condition.expression),
                    if_true: Box::new(if_true.expression),
                    if_false: Box::new(if_false.expression),
                },
            })
        }
        Expression::Switch {
            keyword_switch,
            expression,
            cases,
            ..
        } => {
            let typechecked_expression = infer_expression_type(module, None, expression)?;
            let typechecked_first_case = module.run_in_new_child_scope(|module| {
                Ok((
                    infer_destructure_pattern(
                        module,
                        Some(typechecked_expression.type_value.clone()),
                        &cases.first().pattern,
                        false,
                    )?,
                    infer_expression_type(
                        module,
                        expected_type.clone(),
                        cases.first().body.as_ref(),
                    )?,
                ))
            })?;
            let typechecked_rest_cases = cases
                .tail()
                .iter()
                .map(|case| {
                    module.run_in_new_child_scope(|module| {
                        let pattern = infer_destructure_pattern(
                            module,
                            Some(typechecked_expression.type_value.clone()),
                            &case.pattern,
                            false,
                        )?;
                        let body = infer_expression_type(
                            module,
                            Some(typechecked_first_case.1.type_value.clone()),
                            case.body.as_ref(),
                        )?;
                        Ok((pattern, body))
                    })
                })
                .collect::<Result<Vec<(InferDestructurePatternResult, InferExpressionResult)>, _>>(
                )?;
            check_exhaustiveness(
                module,
                typechecked_first_case.0.type_value.clone(),
                NonEmpty {
                    head: typechecked_first_case.0.destructure_pattern.clone(),
                    tail: typechecked_rest_cases
                        .iter()
                        .map(|(result, _)| result.destructure_pattern.clone())
                        .collect(),
                },
                keyword_switch.position,
            )?;
            Ok(InferExpressionResult {
                type_value: typechecked_first_case.1.type_value,
                expression: TypecheckedExpression::FunctionCall(Box::new(
                    TypecheckedFunctionCall {
                        function: Box::new(TypecheckedExpression::BranchedFunction(Box::new(
                            TypecheckedBranchedFunction {
                                branches: Box::new(NonEmpty {
                                    head: TypecheckedFunctionBranch {
                                        parameters: Box::new(NonEmpty {
                                            head: typechecked_first_case.0.destructure_pattern,
                                            tail: vec![],
                                        }),
                                        body: Box::new(typechecked_first_case.1.expression),
                                    },
                                    tail: typechecked_rest_cases
                                        .iter()
                                        .map(|case| TypecheckedFunctionBranch {
                                            parameters: Box::new(NonEmpty {
                                                head: case.0.destructure_pattern.clone(),
                                                tail: vec![],
                                            }),
                                            body: Box::new(case.1.expression.clone()),
                                        })
                                        .collect(),
                                }),
                            },
                        ))),
                        first_argument: Box::new(typechecked_expression.expression),
                        rest_arguments: vec![],
                    },
                )),
            })
        }
        Expression::ArrowFunction(function) => {
            infer_arrow_function(module, expected_type, *function.clone())
        }
        Expression::Block(block) => {
            let typechecked_statements =
                infer_block_level_statements(module, expected_type, block.clone().statements())?;

            let (typechecked_statements, return_value, type_value) =
                match typechecked_statements.split_last() {
                    Some((last, initial)) => match last {
                        (TypecheckedStatement::Expression(expression), type_value) => {
                            (initial.to_vec(), expression.clone(), type_value.clone())
                        }
                        _ => (initial.to_vec(), TypecheckedExpression::Null, Type::Null),
                    },
                    None => (vec![], TypecheckedExpression::Null, Type::Null),
                };

            Ok(InferExpressionResult {
                type_value,
                expression: TypecheckedExpression::Block {
                    statements: typechecked_statements
                        .into_iter()
                        .map(|(statement, _)| statement)
                        .collect(),
                    return_value: Box::new(return_value),
                },
            })
        }
    }?;
    Ok(InferExpressionResult {
        type_value: module.apply_subtitution_to_type(&result.type_value),
        expression: result.expression,
    })
}

fn infer_arrow_function(
    module: &mut Module,
    expected_type: Option<Type>,
    function: ArrowFunction,
) -> Result<InferExpressionResult, UnifyError> {
    let position = get_expression_position(&Expression::ArrowFunction(Box::new(function.clone())));
    let expected_function_type = match expected_type {
        Some(Type::Function(function_type)) => match function.type_variables.split_first() {
            None => Ok(function_type),
            Some((head, tail)) => Err(UnifyError {
                position: join_position(head.position, tail.last().unwrap_or(head).position),
                kind: UnifyErrorKind::NotExpectingTypeVariable,
            }),
        },
        Some(Type::TypeScheme(type_scheme)) => {
            match type_scheme.type_value {
                Type::Function(function_type) => {
                    let type_variables = function
                        .type_variables
                        .iter()
                        .map(|type_variable| type_variable.representation.clone())
                        .collect::<Vec<String>>();

                    // TODO: perform alpha conversion before checking equivalence
                    if type_scheme.type_variables.into_vector().ne(&type_variables) {
                        Err(UnifyError {
                            position,
                            kind: panic!()
                            // UnifyErrorKind::TypeVariableMismatch {
                            //     expected_type_variables: type_scheme.type_variables,
                            // },
                        })
                    } else {
                        Ok(function_type)
                    }
                }
                other_type => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::NotExpectingFunction {
                        expected_type: other_type,
                    },
                }),
            }
        }
        Some(other_type) => Err(UnifyError {
            position,
            kind: UnifyErrorKind::NotExpectingFunction {
                expected_type: other_type,
            },
        }),
        None => {
            let parameters_types = function
                .parameters
                .clone()
                .parameters()
                .fold_result(|_| module.introduce_implicit_type_variable(None))?;
            let return_type = module.introduce_implicit_type_variable(None)?;
            Ok(FunctionType {
                parameters_types: Box::new(parameters_types),
                return_type: Box::new(return_type),
            })
        }
    }?;
    if expected_function_type.parameters_types.len() != function.parameters.len() {
        return Err(UnifyError {
            position: match &function.parameters {
                ArrowFunctionParameters::WithoutParenthesis(pattern) => {
                    get_destructure_pattern_position(&pattern)
                }
                ArrowFunctionParameters::WithParenthesis(function_parameters) => join_position(
                    function_parameters.left_parenthesis.position,
                    function_parameters.right_parenthesis.position,
                ),
            },
            kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                expected_length: expected_function_type.parameters_types.len(),
                actual_length: function.parameters.len(),
            },
        });
    };
    module.run_in_new_child_scope(|module| {
        // Insert explicit type variables
        function
            .type_variables
            .iter()
            .map(|type_variable_name| module.insert_explicit_type_variable(type_variable_name))
            .collect::<Result<Vec<_>, UnifyError>>()?;

        let function_parameters = function.parameters.clone().parameters();
        let head_parameter = {
            let expected_type = get_expected_type(
                module,
                Some(expected_function_type.parameters_types.head.clone()),
                *function_parameters.head.type_annotation.clone(),
            )?;
            infer_destructure_pattern(
                module,
                expected_type,
                &function_parameters.head.pattern,
                false,
            )
        }?;
        let tail_parameters = expected_function_type
            .parameters_types
            .tail
            .clone()
            .into_iter()
            .zip(function_parameters.tail().iter())
            .map(|(expected_type, parameter)| {
                let expected_type = get_expected_type(
                    module,
                    Some(expected_type),
                    *parameter.type_annotation.clone(),
                )?;
                infer_destructure_pattern(module, expected_type, &parameter.pattern, false)
            })
            .collect::<Result<Vec<InferDestructurePatternResult>, UnifyError>>()?;

        let expected_return_type = get_expected_type(
            module,
            Some(*expected_function_type.return_type.clone()),
            function.return_type_annotation.clone(),
        )?;
        let body = infer_expression_type(module, expected_return_type, &function.body)?;
        Ok(InferExpressionResult {
            type_value: try_lift_as_type_scheme(
                Type::Function(expected_function_type.clone()),
                function
                    .type_variables
                    .clone()
                    .into_iter()
                    .map(|type_variable| type_variable.representation)
                    .collect(),
            ),
            expression: TypecheckedExpression::BranchedFunction(Box::new(
                TypecheckedBranchedFunction {
                    branches: Box::new(NonEmpty {
                        head: TypecheckedFunctionBranch {
                            parameters: Box::new(NonEmpty {
                                head: head_parameter.destructure_pattern,
                                tail: tail_parameters
                                    .into_iter()
                                    .map(|parameter| parameter.destructure_pattern)
                                    .collect(),
                            }),
                            body: Box::new(body.expression),
                        },
                        tail: vec![],
                    }),
                },
            )),
        })
    })
}

fn infer_block_level_statements(
    module: &mut Module,
    expected_final_type: Option<Type>,
    statements: Vec<Statement>,
) -> Result<Vec<(TypecheckedStatement, Type)>, UnifyError> {
    match statements.split_first() {
        None => Ok(vec![]),
        Some((head, tail)) => module.run_in_new_child_scope(|module| {
            // We only pass in the expected type for the last statement
            // As the last statement will be returned as expression
            let expected_type = if tail.is_empty() {
                expected_final_type.clone()
            } else {
                Some(Type::Null)
            };
            let mut result = vec![infer_block_level_statement(module, expected_type, head)?];
            let tail =
                infer_block_level_statements(module, expected_final_type.clone(), tail.to_vec())?;
            result.extend(tail);
            Ok(result)
        }),
    }
}

/// Returns the typechecked statement, the type value of the statement
fn infer_block_level_statement(
    module: &mut Module,
    expected_type: Option<Type>,
    statement: &Statement,
) -> Result<(TypecheckedStatement, Type), UnifyError> {
    let result = match statement {
        Statement::Let(LetStatement {
            type_annotation,
            left,
            right,
            ..
        }) => {
            let typechecked_right = {
                let type_annotation_type =
                    optional_type_annotation_to_type(module, type_annotation)?;
                infer_expression_type(module, type_annotation_type, right)?
            };

            let expected_left_type = get_expected_type(
                module,
                Some(typechecked_right.type_value),
                type_annotation.clone(),
            )?;

            let typechecked_left =
                infer_destructure_pattern(module, expected_left_type, left, false)?;

            match check_exhaustiveness(
                module,
                typechecked_left.type_value.clone(),
                NonEmpty {
                    head: typechecked_left.destructure_pattern.clone(),
                    tail: vec![],
                },
                get_destructure_pattern_position(&left),
            ) {
                Ok(_) => Ok((
                    TypecheckedStatement::Let {
                        exported: false,
                        left: typechecked_left.destructure_pattern,
                        right: typechecked_right.expression,
                    },
                    Type::Null,
                )),
                Err(UnifyError {
                    kind: UnifyErrorKind::MissingCases(remaining_patterns),
                    position,
                }) => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::LetBindingRefutablePattern {
                        missing_patterns: remaining_patterns,
                    },
                }),
                Err(other_error) => Err(other_error),
            }
        }
        Statement::Expression(expression) => {
            let typechecked_expression = infer_expression_type(module, expected_type, &expression)?;
            Ok((
                TypecheckedStatement::Expression(typechecked_expression.expression),
                typechecked_expression.type_value,
            ))
        }
        _ => {
            // TODO: separate Statement enum as TopLevelStatement and BlockLevelStatement
            panic!()
        }
    }?;
    Ok(result)
}

fn infer_with_expression_type(
    module: &mut Module,
    WithExpression {
        left_patterns,
        binary_function_name,
        right,
        body,
        ..
    }: &WithExpression,
) -> Result<InferExpressionResult, UnifyError> {
    // Get the type of the first argument, this is necessary for performing single-dispatch
    let typechecked_right = infer_expression_type(module, None, right.as_ref())?;

    // Get expected function type
    let expected_function_type = {
        // Note that we only infer the type of the first argument
        // The rest arguments is just instantiated with type variables
        let expected_rest_arguments_types = vec![module.introduce_implicit_type_variable(None)?];
        let expected_return_type = module.introduce_implicit_type_variable(None)?;
        Type::Function(FunctionType {
            parameters_types: Box::new(NonEmpty {
                head: typechecked_right.type_value.clone(),
                tail: expected_rest_arguments_types,
            }),
            return_type: Box::new(expected_return_type),
        })
    };

    // Get the actual function type
    // note that we use infer_expression_type_ instead of infer_expression_type
    // This is so that we can throw the error of `cannot invoke non-function`
    let bind_function = infer_expression_type_(
        module,
        Some(expected_function_type),
        &Expression::Identifier(binary_function_name.clone()),
    )?;

    match bind_function.type_value.clone() {
        Type::Function(bind_function_type) => {
            unify_type(
                module,
                bind_function_type.parameters_types.first(),
                &typechecked_right.type_value,
                get_expression_position(right),
            )?;
            let rest_parameters_types = bind_function_type.parameters_types.tail();
            match rest_parameters_types.split_first() {
                Some((Type::Function(function_type), [])) => {
                    let function_arguments_position = join_position(
                        get_destructure_pattern_position(left_patterns.first()),
                        get_destructure_pattern_position(left_patterns.last()),
                    );
                    if function_type.parameters_types.len() != left_patterns.len() {
                        return Err(UnifyError {
                            position: function_arguments_position,
                            kind: UnifyErrorKind::InvalidFunctionArgumentLength {
                                expected_length: function_type.parameters_types.len(),
                                actual_length: left_patterns.len(),
                            },
                        });
                    }
                    module.run_in_new_child_scope(|module| {
                        let typechecked_left_first_pattern = infer_destructure_pattern(
                            module,
                            Some(function_type.parameters_types.first().clone()),
                            left_patterns.first(),
                            false,
                        )?;
                        let typechecked_left_tail_patterns = function_type
                            .parameters_types
                            .tail()
                            .iter()
                            .zip(left_patterns.tail().iter())
                            .map(|(expected_type, pattern)| {
                                infer_destructure_pattern(
                                    module,
                                    Some(expected_type.clone()),
                                    &pattern,
                                    false,
                                )
                            })
                            .collect::<Result<Vec<InferDestructurePatternResult>, UnifyError>>()?;

                        // Check for pattern refutability
                        check_exhaustiveness(
                            module,
                            Type::Tuple(function_type.parameters_types.clone()),
                            NonEmpty {
                                head: TypecheckedDestructurePattern::Tuple {
                                    values: Box::new(NonEmpty {
                                        head: typechecked_left_first_pattern
                                            .destructure_pattern
                                            .clone(),
                                        tail: typechecked_left_tail_patterns
                                            .iter()
                                            .map(|pattern| pattern.destructure_pattern.clone())
                                            .collect(),
                                    }),
                                },
                                tail: vec![],
                            },
                            function_arguments_position,
                        )?;

                        let body = infer_expression_type(
                            module,
                            Some(*function_type.return_type.clone()),
                            body,
                        )?;

                        Ok(InferExpressionResult {
                            expression: TypecheckedExpression::FunctionCall(Box::new(
                                TypecheckedFunctionCall {
                                    function: Box::new(bind_function.expression.clone()),
                                    first_argument: Box::new(typechecked_right.expression.clone()),
                                    rest_arguments: vec![TypecheckedExpression::BranchedFunction(
                                        Box::new(TypecheckedBranchedFunction {
                                            branches: Box::new(NonEmpty {
                                                head: TypecheckedFunctionBranch {
                                                    parameters: Box::new(NonEmpty {
                                                        head: typechecked_left_first_pattern
                                                            .destructure_pattern,
                                                        tail: typechecked_left_tail_patterns
                                                            .into_iter()
                                                            .map(|result| {
                                                                result.destructure_pattern
                                                            })
                                                            .collect(),
                                                    }),
                                                    body: Box::new(body.expression),
                                                },
                                                tail: vec![],
                                            }),
                                        }),
                                    )],
                                },
                            )),
                            type_value: *bind_function_type.return_type.clone(),
                        })
                    })
                }
                _ => Err(UnifyError {
                    position: binary_function_name.position,
                    kind: UnifyErrorKind::WithExpressionBindFunctionConditionNotMet {
                        actual_type: bind_function.type_value,
                    },
                }),
            }
        }
        other_type => Err(UnifyError {
            position: binary_function_name.position,
            kind: UnifyErrorKind::WithExpressionBindFunctionConditionNotMet {
                actual_type: other_type,
            },
        }),
    }
}

fn infer_record_type(
    module: &mut Module,
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
        .map(|(expected_key_type_pair, RecordKeyValue { key, value })| {
            let expected_type = expected_key_type_pair
                .clone()
                .map(|(_, type_value)| type_value);
            let value_type = infer_expression_type(module, expected_type, &value)?;
            Ok((key.clone(), value_type))
        })
        .collect::<Result<Vec<(Token, InferExpressionResult)>, UnifyError>>()?;

    Ok(InferExpressionResult {
        type_value: Type::Record {
            key_type_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (key.representation.clone(), result.type_value.clone()))
                .collect(),
        },
        expression: TypecheckedExpression::Record {
            key_value_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (PropertyName(key.clone()), result.expression.clone()))
                .collect(),
        },
    })
}

struct InferFunctionResult {
    function_type: FunctionType,
    function: TypecheckedBranchedFunction,
}
fn infer_function(
    module: &mut Module,
    expected_function_type: Option<FunctionType>,
    function: &BranchedFunction,
) -> Result<InferFunctionResult, UnifyError> {
    let typechecked_first_branch =
        infer_function_branch(module, expected_function_type, &function.branches.first())?;

    let typechecked_rest_branches = function
        .branches
        .tail()
        .iter()
        .map(|function_branch| {
            infer_function_branch(
                module,
                Some(typechecked_first_branch.function_type.clone()),
                &function_branch,
            )
        })
        .collect::<Result<Vec<InferFunctionBranchResult>, UnifyError>>()?;

    let function_type =
        module.apply_subtitution_to_function_type(&typechecked_first_branch.function_type);

    // Check for case exhaustiveness
    let expected_type = Type::Tuple(function_type.clone().parameters_types);
    let actual_patterns = NonEmpty {
        head: function_branch_parameters_to_tuple(&typechecked_first_branch.function_branch),
        tail: typechecked_rest_branches
            .iter()
            .map(|branch| function_branch_parameters_to_tuple(&branch.function_branch))
            .collect(),
    };

    check_exhaustiveness(
        module,
        expected_type,
        actual_patterns,
        get_expression_position(&Expression::BranchedFunction(Box::new(function.clone()))),
    )?;

    Ok(InferFunctionResult {
        function_type,
        function: TypecheckedBranchedFunction {
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
    function_branch: &TypecheckedFunctionBranch,
) -> TypecheckedDestructurePattern {
    TypecheckedDestructurePattern::Tuple {
        values: function_branch.parameters.clone(),
    }
}

pub fn check_exhaustiveness(
    module: &Module,
    expected_type: Type,
    actual_patterns: NonEmpty<TypecheckedDestructurePattern>,
    position: Position,
) -> Result<(), UnifyError> {
    check_exhaustiveness_(
        module,
        vec![ExpandablePattern::Any {
            type_value: expected_type,
        }],
        actual_patterns,
        position,
    )
}

pub fn check_exhaustiveness_(
    module: &Module,
    expected_patterns: Vec<ExpandablePattern>,
    actual_patterns: NonEmpty<TypecheckedDestructurePattern>,
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
                        position: actual_pattern.position(),
                        kind: UnifyErrorKind::UnreachableCase,
                    })
                } else {
                    let new_patterns =
                        match_patterns(module, &actual_pattern, expected_patterns.clone());

                    if new_patterns.eq(&expected_patterns) {
                        // If new_patterns equals to the current expected_patterns
                        // Then this actual_pattern is also an unreachable case
                        //  as it does not remove any patterns from expected_patterns
                        Err(UnifyError {
                            position: actual_pattern.position(),
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
    module: &mut Module,
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
        unify_type(module, &expected_type, &actual_type, position)
    });

    match unify_rest_argument_type_result {
        Ok(parameters_types) => {
            // unify return type
            match unify_type(
                module,
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
                            module.apply_subtitution_to_function_type(&expected_function_type),
                        ),
                        actual_type: Type::Function(
                            module.apply_subtitution_to_function_type(&actual_function_type),
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

struct InferFunctionBranchResult {
    function_branch: TypecheckedFunctionBranch,
    function_type: FunctionType,
}
fn infer_function_branch(
    module: &mut Module,
    expected_function_type: Option<FunctionType>,
    function_branch: &FunctionBranch,
) -> Result<InferFunctionBranchResult, UnifyError> {
    module.run_in_new_child_scope(|module| {
        let typechecked_parameters = {
            match &expected_function_type {
                None => function_branch
                    .parameters
                    .clone()
                    .fold_result(|destructure_pattern| {
                        infer_destructure_pattern(module, None, &destructure_pattern, false)
                    }),

                Some(expected_function_type) => {
                    let actual_parameters = function_branch.parameters.clone();
                    if expected_function_type.parameters_types.len() != actual_parameters.len() {
                        Err(UnifyError {
                            position: join_position(
                                get_destructure_pattern_position(
                                    function_branch.parameters.first(),
                                ),
                                get_destructure_pattern_position(function_branch.parameters.last()),
                            ),
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
                                infer_destructure_pattern(
                                    module,
                                    Some(expected_parameter_type),
                                    &actual_parameter,
                                    false,
                                )
                            })
                    }
                }
            }
        }?;

        let expected_return_type = expected_function_type
            .clone()
            .map(|expected_function_type| *expected_function_type.return_type);

        let typechecked_body =
            infer_expression_type(module, expected_return_type, &function_branch.body)?;

        let result_type = FunctionType {
            parameters_types: Box::new(
                typechecked_parameters
                    .clone()
                    .map(|argument| argument.type_value),
            ),
            return_type: Box::new(module.apply_subtitution_to_type(&typechecked_body.type_value)),
        };

        // Check for unused variables
        module.check_for_unused_symbols(module.current_scope_name())?;

        Ok(InferFunctionBranchResult {
            function_type: module.apply_subtitution_to_function_type(&result_type),
            function_branch: TypecheckedFunctionBranch {
                parameters: Box::new(
                    typechecked_parameters.map(|argument| argument.destructure_pattern),
                ),
                body: Box::new(typechecked_body.expression),
            },
        })
    })
}

pub fn get_expected_type(
    module: &mut Module,
    expected_type: Option<Type>,
    expected_type_annotation: Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match (expected_type, expected_type_annotation) {
        (None, None) => Ok(None),
        (Some(expected_type), None) => Ok(Some(expected_type)),
        (None, Some(type_annotation)) => {
            Ok(Some(type_annotation_to_type(module, &type_annotation)?))
        }
        (Some(expected_type), Some(type_annotation)) => {
            let actual_type = type_annotation_to_type(module, &type_annotation)?;
            unify_type(
                module,
                &expected_type,
                &actual_type,
                get_type_annotation_position(&type_annotation),
            )?;
            Ok(Some(actual_type))
        }
    }
}

pub fn optional_type_annotation_to_type(
    module: &mut Module,
    type_annotation: &Option<TypeAnnotation>,
) -> Result<Option<Type>, UnifyError> {
    match type_annotation {
        Some(type_annotation) => Ok(Some(type_annotation_to_type(module, type_annotation)?)),
        None => Ok(None),
    }
}

pub fn type_annotation_to_type(
    module: &mut Module,
    type_annotation: &TypeAnnotation,
) -> Result<Type, UnifyError> {
    match &type_annotation {
        TypeAnnotation::Underscore(_) => Ok(Type::Underscore),
        TypeAnnotation::Named {
            name,
            type_arguments: actual_type_arguments,
        } => {
            if let Some(expected_type_symbol) = module.get_type_symbol_by_name(&name) {
                match (
                    expected_type_symbol.type_value.clone(),
                    actual_type_arguments,
                ) {
                    (Type::TypeScheme(expected_type_scheme), Some(actual_type_arguments)) => {
                        if expected_type_scheme.type_variables.len()
                            != actual_type_arguments.arguments.len()
                        {
                            Err(UnifyError {
                                position: join_position(
                                    actual_type_arguments.left_angular_bracket.position,
                                    actual_type_arguments.right_angular_bracket.position,
                                ),
                                kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                                    actual_length: actual_type_arguments.arguments.len(),
                                    expected_type_parameter_names: expected_type_scheme
                                        .type_variables
                                        .clone()
                                        .into_vector(),
                                },
                            })
                        } else {
                            let type_arguments = actual_type_arguments
                                .arguments
                                .iter()
                                .map(|type_value| type_annotation_to_type(module, type_value))
                                .collect::<Result<Vec<Type>, UnifyError>>()?;

                            expected_type_scheme
                                .type_variables
                                .into_vector()
                                .iter()
                                .zip(type_arguments.into_iter())
                                .fold(
                                    Ok(expected_type_scheme.type_value),
                                    |result, (expected_type_variable_name, type_value)| match result
                                    {
                                        Err(error) => Err(error),
                                        Ok(result) => Ok(rewrite_type_variable_in_type(
                                            expected_type_variable_name,
                                            &type_value,
                                            result,
                                        )),
                                    },
                                )
                        }
                    }
                    (Type::TypeScheme(expected_type_scheme), _) => Err(UnifyError {
                        position: name.position,
                        kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                            actual_length: 0,
                            expected_type_parameter_names: expected_type_scheme
                                .type_variables
                                .clone()
                                .into_vector(),
                        },
                    }),
                    (_, Some(actual_type_arguments)) => Err(UnifyError {
                        position: join_position(
                            actual_type_arguments.left_angular_bracket.position,
                            actual_type_arguments.right_angular_bracket.position,
                        ),
                        kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                            actual_length: actual_type_arguments.arguments.len(),
                            expected_type_parameter_names: vec![],
                        },
                    }),
                    (_, None) => Ok(expected_type_symbol.type_value),
                }
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
                    let type_value = type_annotation_to_type(module, &type_annotation)?;
                    Ok((key.representation.clone(), type_value))
                })
                .collect::<Result<Vec<(String, Type)>, UnifyError>>()?;
            Ok(Type::Record { key_type_pairs })
        }
        TypeAnnotation::Array { element_type, .. } => Ok(Type::BuiltInOneArgumentType {
            kind: BuiltInOneArgumentTypeKind::Array,
            type_argument: Box::new(type_annotation_to_type(module, element_type)?),
        }),
        TypeAnnotation::Quoted {
            type_annotation, ..
        } => Ok(Type::BuiltInOneArgumentType {
            kind: BuiltInOneArgumentTypeKind::Quoted,
            type_argument: Box::new(type_annotation_to_type(module, type_annotation.as_ref())?),
        }),
        TypeAnnotation::Promise {
            type_annotation, ..
        } => Ok(Type::BuiltInOneArgumentType {
            kind: BuiltInOneArgumentTypeKind::Promise,
            type_argument: Box::new(type_annotation_to_type(module, type_annotation.as_ref())?),
        }),
        TypeAnnotation::Function {
            parameters,
            return_type,
            ..
        } => Ok(Type::Function(FunctionType {
            parameters_types: Box::new(parameters.parameters.clone().fold_result(|parameter| {
                match *parameter.type_annotation {
                    Some(parameter_type) => type_annotation_to_type(module, &parameter_type),
                    None => Err(UnifyError {
                        position: get_destructure_pattern_position(&parameter.pattern),
                        kind:
                            UnifyErrorKind::MissingParameterTypeAnnotationForFunctionTypeAnnotation,
                    }),
                }
            })?),
            return_type: Box::new(type_annotation_to_type(module, return_type)?),
        })),
    }
}

#[derive(Clone)]
struct InferDestructurePatternResult {
    destructure_pattern: TypecheckedDestructurePattern,
    type_value: Type,
}
fn infer_destructure_pattern(
    module: &mut Module,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
    exported: bool,
) -> Result<InferDestructurePatternResult, UnifyError> {
    // Same story as infer_expression_type
    let result =
        infer_destructure_pattern_(module, expected_type.clone(), destructure_pattern, exported)?;

    let type_value = try_unify_type(
        module,
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
    module: &mut Module,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
    exported: bool,
) -> Result<InferDestructurePatternResult, UnifyError> {
    match destructure_pattern {
        DestructurePattern::Infinite { token, kind } => Ok(InferDestructurePatternResult {
            type_value: match &kind {
                InfinitePatternKind::Character => Type::Character,
                InfinitePatternKind::String => Type::String,
                InfinitePatternKind::Integer => Type::Integer,
            },
            destructure_pattern: TypecheckedDestructurePattern::Infinite {
                kind: kind.clone(),
                token: token.clone(),
            },
        }),
        DestructurePattern::Null(token) => Ok(InferDestructurePatternResult {
            type_value: Type::Null,
            destructure_pattern: TypecheckedDestructurePattern::Null(token.clone()),
        }),
        DestructurePattern::Boolean { value, token } => Ok(InferDestructurePatternResult {
            type_value: Type::Boolean,
            destructure_pattern: TypecheckedDestructurePattern::Boolean {
                value: *value,
                token: token.clone(),
            },
        }),
        DestructurePattern::Underscore(token) => Ok(InferDestructurePatternResult {
            type_value: module.introduce_implicit_type_variable(None)?,
            destructure_pattern: TypecheckedDestructurePattern::Underscore(token.clone()),
        }),
        DestructurePattern::Identifier(identifier) => {
            // If this identifier matches any enum constructor,
            // then treat it as an enum constructor
            if module.matches_some_enum_constructor(&identifier.representation) {
                infer_destructure_pattern(
                    module,
                    expected_type,
                    &DestructurePattern::EnumConstructor {
                        name: identifier.clone(),
                        payload: None,
                    },
                    false,
                )
            }
            // If this identifier does not match any enum constructor
            // then treat it as a new variable
            else {
                let (uid, type_value) =
                    module.insert_value_symbol_with_type(identifier, expected_type, exported)?;
                Ok(InferDestructurePatternResult {
                    type_value,
                    destructure_pattern: TypecheckedDestructurePattern::Identifier(Box::new(
                        Identifier {
                            uid,
                            token: identifier.clone(),
                        },
                    )),
                })
            }
        }
        DestructurePattern::Tuple(tuple) => {
            let typechecked_values = tuple.values.clone().fold_result(|destructure_pattern| {
                // TODO: pass in expected_type
                infer_destructure_pattern(module, None, &destructure_pattern, false)
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
            let result = get_enum_type(module, expected_type, name)?;
            match (result.expected_payload_type, payload.clone()) {
                (None, None) => Ok(InferDestructurePatternResult {
                    type_value: result.expected_enum_type,
                    destructure_pattern: TypecheckedDestructurePattern::EnumConstructor {
                        constructor_name: name.clone(),
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
                        module,
                        Some(expected_payload_type),
                        &payload.pattern,
                        false,
                    )?;
                    Ok(InferDestructurePatternResult {
                        type_value: result.expected_enum_type,
                        destructure_pattern: TypecheckedDestructurePattern::EnumConstructor {
                            constructor_name: name.clone(),
                            payload: Some(TypecheckedDestructurePatternEnumConstructorPayload {
                                right_parenthesis: payload.right_parenthesis,
                                left_parenthesis: payload.left_parenthesis,
                                pattern: Box::new(typechecked_payload.destructure_pattern),
                            }),
                        },
                    })
                }
            }
        }
        DestructurePattern::Array {
            spread: None,
            left_square_bracket,
            right_square_bracket,
        } => Ok(InferDestructurePatternResult {
            type_value: Type::BuiltInOneArgumentType {
                kind: BuiltInOneArgumentTypeKind::Array,
                type_argument: Box::new(Type::ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                }),
            },
            destructure_pattern: TypecheckedDestructurePattern::Array {
                spread: None,
                left_square_bracket: left_square_bracket.clone(),
                right_square_bracket: right_square_bracket.clone(),
            },
        }),
        DestructurePattern::Array {
            left_square_bracket,
            right_square_bracket,
            spread: Some(spread),
            ..
        } => {
            let expected_element_type = match expected_type {
                Some(Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Array,
                    type_argument: expected_type,
                }) => *expected_type,
                _ => Type::ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                },
            };

            let typechecked_first_element = infer_destructure_pattern(
                module,
                Some(expected_element_type.clone()),
                &spread.first_element,
                false,
            )?;

            let expected_array_type = Type::BuiltInOneArgumentType {
                kind: BuiltInOneArgumentTypeKind::Array,
                type_argument: Box::new(expected_element_type),
            };
            let typechecked_rest_elements = infer_destructure_pattern(
                module,
                Some(expected_array_type.clone()),
                &spread.rest_elements,
                false,
            )?;

            Ok(InferDestructurePatternResult {
                type_value: module.apply_subtitution_to_type(&expected_array_type),
                destructure_pattern: TypecheckedDestructurePattern::Array {
                    left_square_bracket: left_square_bracket.clone(),
                    right_square_bracket: right_square_bracket.clone(),
                    spread: Some(TypecheckedDesturcturePatternArraySpread {
                        first_element: Box::new(typechecked_first_element.destructure_pattern),
                        rest_elements: Box::new(typechecked_rest_elements.destructure_pattern),
                    }),
                },
            })
        }
        DestructurePattern::Record {
            wildcard,
            key_value_pairs,
            left_curly_bracket,
            right_curly_bracket,
            ..
        } => {
            let expected_key_type_pairs = match expected_type {
                Some(Type::Record { key_type_pairs }) => Some(key_type_pairs),
                _ => None,
            };
            let key_value_pairs =
                match wildcard {
                    None => Ok(key_value_pairs.clone()),
                    Some(wildcard) => match &expected_key_type_pairs {
                        None => Err(UnifyError {
                            position: wildcard.position,
                            kind: UnifyErrorKind::MissingTypeAnnotationForRecordWildcard,
                        }),
                        Some(expected_key_type_pairs) => Ok(key_value_pairs
                            .clone()
                            .into_iter()
                            .chain(expected_key_type_pairs.iter().filter_map(
                                |(expected_key, _)| {
                                    if key_value_pairs.iter().any(|actual_key_value| {
                                        actual_key_value.key.representation == *expected_key
                                    }) {
                                        None
                                    } else {
                                        let key = Token {
                                            position: wildcard.position,
                                            token_type: TokenType::Identifier,
                                            representation: expected_key.clone(),
                                        };
                                        Some(DestructuredRecordKeyValue {
                                            key,
                                            as_value: None,
                                        })
                                    }
                                },
                            ))
                            .collect()),
                    },
                }?;
            let typechecked_key_pattern_pairs = key_value_pairs
                .iter()
                .map(|DestructuredRecordKeyValue { key, as_value }| {
                    // Try to find the expected type for this key
                    // TODO: optimise this section, as the current algorithm is very slow
                    let actual_key = key.clone();
                    let expected_type = match &expected_key_type_pairs {
                        None => Ok(None),
                        Some(expected_key_type_pairs) => {
                            let matching_key_type_pair =
                                expected_key_type_pairs.iter().find(|(expected_key, _)| {
                                    actual_key.representation.eq(expected_key)
                                });
                            match matching_key_type_pair {
                                None => Err(UnifyError {
                                    position: key.position,
                                    kind: UnifyErrorKind::NoSuchProperty {
                                        expected_keys: expected_key_type_pairs
                                            .iter()
                                            .map(|(key, _)| key.clone())
                                            .collect(),
                                    },
                                }),
                                Some((_, expected_type)) => Ok(Some(expected_type.clone())),
                            }
                        }
                    }?;

                    match as_value {
                        Some(destructure_pattern) => {
                            let typechecked_destructure_pattern = infer_destructure_pattern(
                                module,
                                expected_type,
                                destructure_pattern,
                                false,
                            )?;

                            Ok((actual_key, typechecked_destructure_pattern))
                        }
                        None => {
                            let (uid, type_value) =
                                module.insert_value_symbol_with_type(key, expected_type, false)?;
                            Ok((
                                actual_key.clone(),
                                InferDestructurePatternResult {
                                    type_value,
                                    destructure_pattern: TypecheckedDestructurePattern::Identifier(
                                        Box::new(Identifier {
                                            uid,
                                            token: actual_key,
                                        }),
                                    ),
                                },
                            ))
                        }
                    }
                })
                .collect::<Result<Vec<(Token, InferDestructurePatternResult)>, UnifyError>>()?;

            Ok(InferDestructurePatternResult {
                type_value: Type::Record {
                    key_type_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| {
                            (key.representation.clone(), pattern.type_value.clone())
                        })
                        .collect(),
                },
                destructure_pattern: TypecheckedDestructurePattern::Record {
                    left_curly_bracket: left_curly_bracket.clone(),
                    right_curly_bracket: right_curly_bracket.clone(),
                    key_pattern_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| {
                            (
                                PropertyName(key.clone()),
                                pattern.destructure_pattern.clone(),
                            )
                        })
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
        Type::BuiltInOneArgumentType {
            kind,
            type_argument,
        } => Type::BuiltInOneArgumentType {
            kind: kind.clone(),
            type_argument: Box::new(substitute_type_variable_in_type(
                from_type_variable,
                to_type,
                type_argument.as_ref(),
            )),
        },

        // TODO: this might have problem since we allow type variable shadowing,
        //  For example: let f = <T>(g: <T>(t: T) => T): Integer = 123
        // Rewriting the T of f should not rewrite the T of g, but the current implementation will.
        Type::ExplicitTypeVariable { name } | Type::ImplicitTypeVariable { name } => {
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
            symbol_uid: symbol_uid.clone(),
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
        Type::TypeScheme(type_scheme) => Type::TypeScheme(Box::new(TypeScheme {
            type_variables: type_scheme.type_variables.clone(),
            type_value: substitute_type_variable_in_type(
                from_type_variable,
                to_type,
                &type_scheme.type_value,
            ),
        })),
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
        Type::BuiltInOneArgumentType { type_argument, .. } => {
            get_free_type_variables_in_type(type_argument.as_ref())
        }
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
        Type::TypeScheme(type_scheme) => get_free_type_variables_in_type(&type_scheme.type_value),
    }
}

/// To check whether a type variable occur in a type.
/// This is to prevent absurd unification.
/// For example, the unification of A with (A -> B) should not
/// produce the subtitution of {A = A -> B}
fn type_variable_occurs_in_type(type_variable: &str, typ: &Type) -> bool {
    get_free_type_variables_in_type(typ).contains(type_variable)
}

/// For example, before using the type <A, B>(A => B)
/// We need to instantiate A and B with a implicit type variable that can be substituted,
/// This is necessary to prevent name clashing during unification of two generic types.
///
/// In this case, we can (for example) substitute A as T1 and B as T2,
/// and the resulting type will be (T1 => T2)
pub fn instantiate_type_scheme(module: &mut Module, type_scheme: TypeScheme) -> Type {
    let type_variable_substitutions: TypeVariableSubstitutions = type_scheme
        .type_variables
        .map(|from_type_variable| TypeVariableSubstitution {
            from_type_variable,
            to_type: Type::ImplicitTypeVariable {
                name: module.get_next_type_variable_name(),
            },
        })
        .into_vector();

    type_variable_substitutions.into_iter().fold(
        type_scheme.type_value,
        |result,
         TypeVariableSubstitution {
             from_type_variable,
             to_type,
         }| { substitute_type_variable_in_type(&from_type_variable, &to_type, &result) },
    )
}
