use crate::{
    compile::{CompileError, CompileErrorKind},
    non_empty::NonEmpty,
    parse::Parser,
    tokenize::tokenize,
};

use crate::ast::*;
use crate::module::*;
use crate::pattern::*;
use crate::typechecked_ast::*;
use relative_path::RelativePath;
use std::iter;
use std::{collections::HashMap, fs};
use std::{collections::HashSet, path::Path};

pub struct UnifyProgramResult {
    pub statements: Vec<TypecheckedStatement>,
    pub module: Module,

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

/// `starting_symbol_uid` is needed to make sure each symbol has a UID that is unique across different modules
pub fn unify_statements(
    module_meta: ModuleMeta,
    statements: Vec<Statement>,
    starting_symbol_uid: usize,
    imported_modules: &ImportedModules,
    is_entry_point: bool,
) -> Result<UnifyProgramResult, CompileError> {
    // 1. Partition statements based on their types
    let (import_statements, type_statements, enum_statements, let_statements, do_statements) = {
        let mut import_statements = Vec::new();
        let mut type_statements = Vec::new();
        let mut enum_statements = Vec::new();
        let mut let_statements = Vec::new();
        let mut do_statements = Vec::new();
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
                    let_statements.push(let_statement);
                }
                Statement::Do(do_statement) => {
                    // Do statements will only be compiled on entrypoint file
                    if is_entry_point {
                        do_statements.push(do_statement);
                    }
                }
            }
        }
        (
            import_statements,
            type_statements,
            enum_statements,
            let_statements,
            do_statements,
        )
    };

    // 2. Infer import statements (to include every imported symbols into the current module)

    let mut module: Module = Module::new(module_meta, starting_symbol_uid);

    let init: (Vec<TypecheckedStatement>, _) = (vec![], imported_modules.clone());

    let (typechecked_import_statements, imported_modules) =
        import_statements
            .into_iter()
            .fold(Ok(init), |result, import_statement| match result {
                Err(error) => Err(error),
                Ok((mut statements, mut imported_modules)) => {
                    let current =
                        infer_import_statement(&mut module, &imported_modules, import_statement)?;

                    statements.extend(current.statements.into_iter());

                    imported_modules.extend(current.imported_modules);

                    Ok((statements, imported_modules))
                }
            })?;

    // 3. Insert type symbols (including enums) into the current module.
    //    Note that we will first insert them into the current module first before checking their definition,
    //    this is so that mutually recursive type can be checked.
    type_statements
        .into_iter()
        .map(
            |type_statement| match infer_type_statement(&mut module, type_statement) {
                Ok(()) => Ok(()),
                Err(unify_error) => Err(unify_error.into_compile_error(module.meta.clone())),
            },
        )
        .collect::<Result<Vec<()>, CompileError>>()?;

    enum_statements
        .into_iter()
        .map(
            |enum_statement| match infer_enum_statement(&mut module, enum_statement) {
                Ok(()) => Ok(()),
                Err(unify_error) => Err(unify_error.into_compile_error(module.meta.clone())),
            },
        )
        .collect::<Result<Vec<()>, CompileError>>()?;

    // 4. Insert let symbols into the current module.
    //    Similar as type symbols, we will insert them into the module first before checking their definition,
    //    this is to that mutually recursive functions can be checked.
    let let_statements = let_statements
        .into_iter()
        .map(|let_statement| {
            let type_scheme = {
                module.step_into_new_child_scope();
                let_statement
                    .type_variables
                    .iter()
                    .map(|type_variable_name| {
                        module.insert_explicit_type_variable(type_variable_name)
                    })
                    .collect::<Result<Vec<usize>, UnifyError>>()?;

                let type_scheme = TypeScheme {
                    type_variables: let_statement
                        .type_variables
                        .iter()
                        .map(|type_variable| type_variable.representation.clone())
                        .collect(),
                    type_value: type_annotation_to_type(
                        &mut module,
                        &let_statement.type_annotation,
                    )?,
                };
                module.step_out_to_parent_scope();
                Ok(type_scheme)
            }?;

            // Check for clashing between function name and record property name
            if let Type::Function(FunctionType {
                parameters_types, ..
            }) = &type_scheme.type_value
            {
                let first_parameter_type = parameters_types.first();
                if let Type::Record { key_type_pairs } = first_parameter_type {
                    if key_type_pairs
                        .iter()
                        .any(|(key, _)| *key == let_statement.left.representation)
                    {
                        return Err(UnifyError {
                            position: let_statement.left.position,
                            kind: UnifyErrorKind::PropertyNameClashWithFunctionName {
                                name: let_statement.left.representation,
                            },
                        });
                    }
                }
            };

            let uid = module.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: let_statement.left.clone(),
                        exported: let_statement.keyword_export.is_some(),
                    },
                    kind: SymbolKind::Value(ValueSymbol {
                        type_scheme: TypeScheme {
                            type_variables: type_scheme.type_variables,
                            type_value: rewrite_explicit_type_variables_as_implicit(
                                type_scheme.type_value.clone(),
                                &{
                                    let mut hashset = HashSet::new();
                                    for type_variable in &let_statement.type_variables {
                                        hashset.insert(type_variable.representation.clone());
                                    }
                                    hashset
                                },
                            ),
                        },
                    }),
                },
            )?;
            Ok((
                uid,
                let_statement.left.representation,
                let_statement.type_variables,
                type_scheme.type_value,
                let_statement.right,
            ))
        })
        .collect::<Result<Vec<_>, UnifyError>>()
        .map_err(|error| error.into_compile_error(module.meta.clone()))?;

    let typechecked_let_statements = let_statements
        .into_iter()
        .map(|(uid, name, type_variables, expected_type, expression)| {
            module.step_into_new_child_scope();

            // Populate type variables
            type_variables
                .iter()
                .map(|type_variable_name| module.insert_explicit_type_variable(type_variable_name))
                .collect::<Result<Vec<usize>, UnifyError>>()?;

            // Typecheck the expression
            let result = infer_expression_type(&mut module, Some(expected_type), &expression)?;

            module.step_out_to_parent_scope();

            // Return the typechecked statement, which is needed for transpilation
            Ok(TypecheckedStatement::Let {
                left: Variable {
                    uid,
                    representation: name,
                },
                right: result.expression,
            })
        })
        .collect::<Result<Vec<TypecheckedStatement>, UnifyError>>()
        .map_err(|unify_error| unify_error.into_compile_error(module.meta.clone()))?;

    // 5. Lastly we will infer do statements
    let typechecked_do_statements = do_statements
        .into_iter()
        .map(
            |do_statement| match infer_do_statement(&mut module, do_statement) {
                Ok(statement) => Ok(statement),
                Err(unify_error) => Err(unify_error.into_compile_error(module.meta.clone())),
            },
        )
        .collect::<Result<Vec<TypecheckedStatement>, CompileError>>()?;
    Ok(UnifyProgramResult {
        statements: {
            let mut statements = Vec::new();
            statements.extend(typechecked_import_statements);
            statements.extend(typechecked_let_statements);
            statements.extend(typechecked_do_statements);
            statements
        },
        module,
        imported_modules,
    })
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
    LetBindingRefutablePattern {
        missing_patterns: Vec<ExpandablePattern>,
    },
    ApplicativeLetExpressionBindFunctionConditionNotMet {
        actual_type: Type,
    },
    PropertyNameClashWithFunctionName {
        name: String,
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
    NoSuchPropertyOrFunction {
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
    UnknownValueSymbol,
    UnknownEnumConstructor,
    TypeMismatch {
        expected_type: Type,
        actual_type: Type,
    },
}

type ImportedModules = HashMap<ModuleUid, Module>;
pub struct InferStatementResult {
    pub statements: Vec<TypecheckedStatement>,
    pub imported_modules: ImportedModules,
}

pub fn infer_import_statement(
    module: &mut Module,
    imported_modules: &ImportedModules,
    ImportStatement {
        url,
        imported_names,
        ..
    }: ImportStatement,
) -> Result<InferStatementResult, CompileError> {
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
                module: module.clone(),
                statements: vec![],
                imported_modules: HashMap::new(),
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

                    // Tokenize the imported module
                    let tokens = match tokenize(code) {
                        Ok(tokens) => Ok(tokens),
                        Err(tokenize_error) => Err(CompileError {
                            module_meta: module_meta.clone(),
                            kind: CompileErrorKind::TokenizeError(tokenize_error),
                        }),
                    }?;

                    // Parse the imported module
                    let statements = match Parser::parse(tokens) {
                        Ok(statements) => Ok(statements),
                        Err(parse_error) => Err(CompileError {
                            module_meta: module_meta.clone(),
                            kind: CompileErrorKind::ParseError(Box::new(parse_error)),
                        }),
                    }?;

                    // Typecheck the imported module
                    unify_statements(
                        module_meta,
                        statements,
                        module.get_next_symbol_uid(),
                        imported_modules,
                        false,
                    )?
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
    let statements = imported_names
        .into_vector()
        .into_iter()
        .map(|imported_name| {
            let matching_symbol_entries = imported
                .module
                .get_all_matching_symbols(&imported_name.name);

            if matching_symbol_entries.is_empty() {
                Err(UnifyError {
                    position: imported_name.name.position,
                    kind: UnifyErrorKind::UnknownImportedName,
                }
                .into_compile_error(module.meta.clone()))
            } else {
                let name = match &imported_name.alias_as {
                    Some(name) => name,
                    None => &imported_name.name,
                };

                // Insert the matching value symbol into the current module
                let statements = matching_symbol_entries
                    .iter()
                    .map(|entry| {
                        if !entry.symbol.meta.exported {
                            return Err(UnifyError {
                                position: imported_name.name.position,
                                kind: UnifyErrorKind::CannotImportPrivateSymbol,
                            }
                            .into_compile_error(module.meta.clone()));
                        }
                        match module.insert_symbol(
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
                                                right: TypecheckedExpression::Variable(Variable {
                                                    uid: entry.uid,
                                                    representation: entry
                                                        .symbol
                                                        .meta
                                                        .name
                                                        .representation
                                                        .clone(),
                                                }),
                                            }])
                                        }
                                        _ => Ok(vec![]),
                                    },
                                    None => Ok(vec![]),
                                }
                            }
                            Err(unify_error) => {
                                Err(unify_error.into_compile_error(module.meta.clone()))
                            }
                        }
                    })
                    .collect::<Result<Vec<Vec<TypecheckedStatement>>, CompileError>>()?;

                Ok(statements)
            }
        })
        .collect::<Result<Vec<Vec<Vec<TypecheckedStatement>>>, CompileError>>()?;

    // After importing, we need to increment the `current_uid` of the currrent module
    // to the biggest `current_uid` of the imported module
    // so that UID uniqueness can be maintained across different modules
    // this uniquenss is important for transpilation, so that the generated code
    // will not contain both variables with the same name
    module.set_current_uid(imported.module.get_current_uid());

    Ok(InferStatementResult {
        statements: imported
            .statements
            .into_iter()
            .chain(statements.into_iter().flatten().flatten())
            .collect(),
        imported_modules: {
            let mut imported_modules = imported.imported_modules;
            let uid = imported.module.uid();
            imported_modules.insert(uid, imported.module);
            imported_modules
        },
    })
}

pub fn infer_enum_statement(
    module: &mut Module,
    EnumStatement {
        name,
        constructors,
        type_variables,
        keyword_export,
        ..
    }: EnumStatement,
) -> Result<(), UnifyError> {
    // 1. Add this enum into module first, to allow recursive definition
    let enum_uid = module.get_next_symbol_uid();
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

    module.insert_symbol(
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

    // 2. Populate type variables into current module
    module.step_into_new_child_scope();
    for type_variable in type_variables.clone() {
        module.insert_symbol(
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
                                type_annotation_to_type(module, &payload.type_annotation)?;
                            Some(payload_type_value)
                        }
                    },
                }),
            })
        })
        .collect::<Result<Vec<Symbol>, UnifyError>>()?;

    module.step_out_to_parent_scope();

    constructor_symbols
        .into_iter()
        .map(|constructor_symbol| module.insert_symbol(None, constructor_symbol))
        .collect::<Result<Vec<_>, UnifyError>>()?;

    Ok(())
}

pub fn infer_type_statement(
    module: &mut Module,
    TypeStatement {
        keyword_export,
        left,
        right,
        type_variables,
        ..
    }: TypeStatement,
) -> Result<(), UnifyError> {
    module.step_into_new_child_scope();
    // 1. Populate type variables into current module
    module.step_into_new_child_scope();
    for type_variable in type_variables.clone() {
        module.insert_symbol(
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
    let type_value = type_annotation_to_type(module, &right)?;

    // 3. Add this type symbol into this module
    module.step_out_to_parent_scope();
    module.insert_symbol(
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

pub fn infer_do_statement(
    module: &mut Module,
    do_statement: DoStatement,
) -> Result<TypecheckedStatement, UnifyError> {
    match infer_expression_type(
        module,
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
        Type::BuiltInOneArgumentType {
            kind,
            type_argument: argument,
        } => Type::BuiltInOneArgumentType {
            kind,
            type_argument: Box::new(rewrite_explicit_type_variables_as_implicit(
                *argument,
                explicit_type_variable_names,
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

/// @deprecated
/// This function is left here: for reference purpose
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
        | Expression::Variable(token)
        | Expression::Null(token)
        | Expression::Boolean { token, .. } => token.position,
        Expression::Promise { bang, expression } => {
            join_position(bang.position, get_expression_position(expression))
        }
        Expression::Quoted {
            opening_backtick,
            closing_backtick,
            ..
        } => join_position(opening_backtick.position, closing_backtick.position),
        Expression::EnumConstructor { name, payload, .. } => match payload {
            None => name.position,
            Some(payload) => join_position(name.position, payload.right_parenthesis.position),
        },
        Expression::RecordAccessOrFunctionCall {
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
        Expression::Function(function) => {
            let start_position = function.branches.first().start_token.position;
            let end_position = get_expression_position(&function.branches.last().body);
            join_position(start_position, end_position)
        }
        Expression::Let {
            keyword_let,
            body: true_branch,
            ..
        } => join_position(keyword_let.position, get_expression_position(&true_branch)),
        Expression::UnsafeJavascript { code } => code.position,
        Expression::ApplicativeLet(applicative_let) => join_position(
            applicative_let.keyword_let.position,
            get_expression_position(&applicative_let.body),
        ),
        Expression::If {
            keyword_if,
            if_false,
            ..
        } => join_position(keyword_if.position, get_expression_position(if_false)),
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
        Some(Type::Named { symbol_uid, .. }) => Some(*symbol_uid),
        _ => None,
    };
    // Look up constructor
    let constructor = module.get_constructor_symbol(expected_enum_uid, &token)?;

    let enum_type = module
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
                            name: module.get_next_type_variable_name(),
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
                        (PropertyName("value".to_string()), result.expression),
                        (
                            PropertyName("meta".to_string()),
                            TypecheckedExpression::Record {
                                key_value_pairs: vec![
                                    (
                                        PropertyName("filename".to_string()),
                                        TypecheckedExpression::String {
                                            representation: format!(
                                                "\"{}\"",
                                                module.meta.uid.string_value(),
                                            ),
                                        },
                                    ),
                                    (
                                        PropertyName("line_start".to_string()),
                                        TypecheckedExpression::Integer {
                                            representation: position.line_start.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName("line_end".to_string()),
                                        TypecheckedExpression::Integer {
                                            representation: position.line_end.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName("column_start".to_string()),
                                        TypecheckedExpression::Integer {
                                            representation: position.column_start.to_string(),
                                        },
                                    ),
                                    (
                                        PropertyName("column_end".to_string()),
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
        Expression::Variable(variable) => {
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
                    type_value: instantiate_type_scheme(module, result.type_scheme),
                    expression: TypecheckedExpression::Variable(Variable {
                        uid: result.symbol_uid,
                        representation: variable.representation.clone(),
                    }),
                })
            }
        }
        Expression::RecordAccessOrFunctionCall {
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
                    // Infer as function call
                    return infer_expression_type(
                        module,
                        expected_type,
                        &Expression::FunctionCall(Box::new(FunctionCall {
                            function: Box::new(Expression::Variable(property_name.clone())),
                            first_argument: expression.clone(),
                            rest_arguments: None,
                            type_arguments: None,
                        })),
                    );
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
                            function: Box::new(Expression::Variable(property_name.clone())),
                            first_argument: expression.clone(),
                            rest_arguments: None,
                            type_arguments: None,
                        })),
                    ) {
                        Ok(result) => Ok(result),
                        Err(UnifyError {
                            kind: UnifyErrorKind::UnknownValueSymbol,
                            ..
                        }) => Err(UnifyError {
                            position: property_name.position,
                            kind: UnifyErrorKind::NoSuchPropertyOrFunction {
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
                        property_name: PropertyName(property_name.representation.clone()),
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
                                    kind: UnifyErrorKind::NoSuchPropertyOrFunction {
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
                                            property_name: PropertyName(
                                                property_name.representation.clone(),
                                            ),
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
                                            property_name: PropertyName(
                                                property_name.representation.clone(),
                                            ),
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
            body: true_branch,
            type_annotation,
            ..
        } => {
            module.step_into_new_child_scope();
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

            let typechecked_left = infer_destructure_pattern(module, expected_left_type, left)?;

            let typechecked_body = infer_expression_type(module, expected_type, true_branch)?;

            let result_type = match check_exhaustiveness(
                module,
                typechecked_left.type_value.clone(),
                NonEmpty {
                    head: *left.clone(),
                    tail: vec![],
                },
                get_destructure_pattern_position(left),
            ) {
                Ok(_) => Ok(typechecked_body.type_value),
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
            }?;

            let result = InferExpressionResult {
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
                                        body: Box::new(typechecked_body.expression),
                                    },
                                    tail: vec![],
                                }),
                            },
                        ))),
                        first_argument: Box::new(typechecked_right.expression),
                        rest_arguments: vec![],
                    },
                )),
            };

            module.step_out_to_parent_scope();
            Ok(result)
        }
        Expression::Function(function) => {
            let expected_function_type = match expected_type {
                Some(Type::Function(function_type)) => Some(function_type),
                _ => None,
            };
            let typechecked_function = infer_function(module, expected_function_type, function)?;
            Ok(InferExpressionResult {
                type_value: Type::Function(typechecked_function.function_type),
                expression: TypecheckedExpression::Function(Box::new(
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
        Expression::ApplicativeLet(applicative_let) => {
            infer_applicative_let_type(module, applicative_let)
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
                    module,
                    Some(key_type_pairs),
                    key_value_pairs.clone(),
                    record_position,
                ),
                _ => infer_record_type(module, None, key_value_pairs.clone(), record_position),
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
        Expression::Promise { expression, .. } => {
            let expected_type = match expected_type {
                Some(Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Promise,
                    type_argument: type_value,
                }) => Some(*type_value),
                other => other,
            };
            let result = infer_expression_type(module, expected_type, expression)?;
            Ok(InferExpressionResult {
                type_value: Type::BuiltInOneArgumentType {
                    kind: BuiltInOneArgumentTypeKind::Promise,
                    type_argument: Box::new(result.type_value),
                },
                expression: TypecheckedExpression::Promise(Box::new(result.expression)),
            })
        }
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
    }?;
    Ok(InferExpressionResult {
        type_value: module.apply_subtitution_to_type(&result.type_value),
        expression: result.expression,
    })
}

fn infer_applicative_let_type(
    module: &mut Module,
    ApplicativeLet {
        left_patterns,
        binary_function_name,
        right,
        body,
        ..
    }: &ApplicativeLet,
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
        &Expression::Variable(binary_function_name.clone()),
    )?;

    match &bind_function.type_value {
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
                    module.step_into_new_child_scope();

                    let typechecked_left_first_pattern = infer_destructure_pattern(
                        module,
                        Some(function_type.parameters_types.first().clone()),
                        left_patterns.first(),
                    )?;
                    let typechecked_left_tail_patterns = function_type
                        .parameters_types
                        .tail()
                        .iter()
                        .zip(left_patterns.tail().iter())
                        .map(|(expected_type, pattern)| {
                            infer_destructure_pattern(module, Some(expected_type.clone()), &pattern)
                        })
                        .collect::<Result<Vec<InferDestructurePatternResult>, UnifyError>>()?;

                    // Check for pattern refutability
                    check_exhaustiveness(
                        module,
                        Type::Tuple(function_type.parameters_types.clone()),
                        NonEmpty {
                            head: DestructurePattern::Tuple(DestructurePatternTuple {
                                parentheses: None,
                                values: Box::new(left_patterns.clone()),
                            }),
                            tail: vec![],
                        },
                        function_arguments_position,
                    )?;

                    let body = infer_expression_type(
                        module,
                        Some(*function_type.return_type.clone()),
                        body,
                    )?;

                    module.step_out_to_parent_scope();

                    Ok(InferExpressionResult {
                        expression: TypecheckedExpression::FunctionCall(Box::new(
                            TypecheckedFunctionCall {
                                function: Box::new(bind_function.expression),
                                first_argument: Box::new(typechecked_right.expression),
                                rest_arguments: vec![TypecheckedExpression::Function(Box::new(
                                    TypecheckedFunction {
                                        branches: Box::new(NonEmpty {
                                            head: TypecheckedFunctionBranch {
                                                parameters: Box::new(NonEmpty {
                                                    head: typechecked_left_first_pattern
                                                        .destructure_pattern,
                                                    tail: typechecked_left_tail_patterns
                                                        .into_iter()
                                                        .map(|result| result.destructure_pattern)
                                                        .collect(),
                                                }),
                                                body: Box::new(body.expression),
                                            },
                                            tail: vec![],
                                        }),
                                    },
                                ))],
                            },
                        )),
                        type_value: *bind_function_type.return_type.clone(),
                    })
                }
                _ => Err(UnifyError {
                    position: binary_function_name.position,
                    kind: UnifyErrorKind::ApplicativeLetExpressionBindFunctionConditionNotMet {
                        actual_type: bind_function.type_value,
                    },
                }),
            }
        }
        other_type => Err(UnifyError {
            position: binary_function_name.position,
            kind: UnifyErrorKind::ApplicativeLetExpressionBindFunctionConditionNotMet {
                actual_type: other_type.clone(),
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
            Ok((key.representation.clone(), value_type))
        })
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
                .map(|(key, result)| (PropertyName(key.clone()), result.expression.clone()))
                .collect(),
        },
    })
}

struct InferFunctionResult {
    function_type: FunctionType,
    function: TypecheckedFunction,
}
fn infer_function(
    module: &mut Module,
    expected_function_type: Option<FunctionType>,
    function: &Function,
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
    let actual_patterns = function
        .clone()
        .branches
        .map(|branch| DestructurePattern::Tuple(function_branch_parameters_to_tuple(&branch)));

    check_exhaustiveness(
        module,
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
        values: Box::new(function_branch.parameters.clone()),
    }
}

pub fn check_exhaustiveness(
    module: &Module,
    expected_type: Type,
    actual_patterns: NonEmpty<DestructurePattern>,
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
                        match_patterns(module, &actual_pattern, expected_patterns.clone());

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

pub fn get_function_branch_position(function_branch: &FunctionBranch) -> Position {
    let body_position = get_expression_position(&function_branch.body);
    join_position(function_branch.start_token.position, body_position)
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
    module.step_into_new_child_scope();

    let typechecked_parameters = {
        match &expected_function_type {
            None => function_branch
                .parameters
                .clone()
                .fold_result(|destructure_pattern| {
                    Ok(infer_destructure_pattern(
                        module,
                        None,
                        &destructure_pattern,
                    )?)
                }),

            Some(expected_function_type) => {
                let actual_parameters = function_branch.parameters.clone();
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
                            infer_destructure_pattern(
                                module,
                                Some(expected_parameter_type),
                                &actual_parameter,
                            )
                        })
                }
            }
        }
    }?;

    let expected_return_type =
        expected_function_type.map(|expected_function_type| *expected_function_type.return_type);

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

    module.step_out_to_parent_scope();

    Ok(InferFunctionBranchResult {
        function_type: module.apply_subtitution_to_function_type(&result_type),
        function_branch: TypecheckedFunctionBranch {
            parameters: Box::new(
                typechecked_parameters.map(|argument| argument.destructure_pattern),
            ),
            body: Box::new(typechecked_body.expression),
        },
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
        (Some(expected_type), Some(type_annotation)) => Err(UnifyError {
            position: get_type_annotation_position(&type_annotation),
            kind: UnifyErrorKind::UnnecessaryTypeAnnotation { expected_type },
        }),
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
            type_arguments,
        } => {
            if let Some(type_symbol) = module.get_type_symbol_by_name(&name) {
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
                                .map(|type_value| type_annotation_to_type(module, type_value))
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
            parameters_types,
            return_type,
            ..
        } => {
            Ok(Type::Function(FunctionType {
                parameters_types: Box::new(parameters_types.clone().fold_result(
                    |parameter_type| type_annotation_to_type(module, &parameter_type),
                )?),
                return_type: Box::new(type_annotation_to_type(module, return_type)?),
            }))
        }
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
) -> Result<InferDestructurePatternResult, UnifyError> {
    // Same story as infer_expression_type
    let result = infer_destructure_pattern_(module, expected_type.clone(), destructure_pattern)?;

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
            type_value: module.introduce_implicit_type_variable(None)?,
            destructure_pattern: TypecheckedDestructurePattern::Underscore,
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
                )
            }
            // If this identifier does not match any enum constructor
            // then treat it as a new variable
            else {
                let (uid, type_value) =
                    module.insert_value_symbol_with_type(identifier, expected_type, false)?;
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
                infer_destructure_pattern(module, None, &destructure_pattern)
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
                        module,
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
            type_value: Type::BuiltInOneArgumentType {
                kind: BuiltInOneArgumentTypeKind::Array,
                type_argument: Box::new(Type::ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                }),
            },
            destructure_pattern: TypecheckedDestructurePattern::Array { spread: None },
        }),
        DestructurePattern::Array {
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
            )?;

            let expected_array_type = Type::BuiltInOneArgumentType {
                kind: BuiltInOneArgumentTypeKind::Array,
                type_argument: Box::new(expected_element_type),
            };
            let typechecked_rest_elements = infer_destructure_pattern(
                module,
                Some(expected_array_type.clone()),
                &spread.rest_elements,
            )?;

            Ok(InferDestructurePatternResult {
                type_value: module.apply_subtitution_to_type(&expected_array_type),
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
                .map(|DestructuredRecordKeyValue { key, as_value }| {
                    // Try to find the expected type for this key
                    // TODO: optimise this section, as the current algorithm is very slow
                    let actual_key = key.representation.clone();
                    let expected_type = match &expected_key_type_pairs {
                        None => Ok(None),
                        Some(expected_key_type_pairs) => {
                            let matching_key_type_pair = expected_key_type_pairs
                                .iter()
                                .find(|(expected_key, _)| actual_key.eq(expected_key));
                            match matching_key_type_pair {
                                None => Err(UnifyError {
                                    position: key.position,
                                    kind: UnifyErrorKind::NoSuchPropertyOrFunction {
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
                                    destructure_pattern: TypecheckedDestructurePattern::Variable(
                                        Variable {
                                            uid,
                                            representation: actual_key,
                                        },
                                    ),
                                },
                            ))
                        }
                    }
                })
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
        Type::ExplicitTypeVariable { name } => Type::ExplicitTypeVariable { name: name.clone() },
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
pub fn instantiate_type_scheme(module: &mut Module, type_scheme: TypeScheme) -> Type {
    let type_variable_substitutions: TypeVariableSubstitutions = type_scheme
        .type_variables
        .into_iter()
        .map(|from_type_variable| TypeVariableSubstitution {
            from_type_variable,
            to_type: Type::ImplicitTypeVariable {
                name: module.get_next_type_variable_name(),
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
