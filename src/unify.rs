use crate::{
    compile::{CompileError, CompileErrorKind},
    non_empty::NonEmpty,
    parse::{ParseError, Parser},
    stringify_error::stringify_type,
    tokenize::Tokenizer,
    utils::to_relative_path,
};

use crate::inferred_ast::*;
use crate::module::*;
use crate::pattern::*;
use crate::raw_ast::*;
use crate::typ::*;
use indexmap::IndexMap;
use itertools::Itertools;
use relative_path::RelativePath;
use std::{any::type_name, ffi::OsStr, fs};
use std::{collections::HashSet, path::Path};
use std::{iter, path::PathBuf};

pub struct UnifyProgramResult {
    /// The entrypoint module
    pub entrypoint: InferredModule,

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

pub enum ReadModuleFolderName {
    FromCli {
        folder_name: String,
        entry_point_filename: String,
    },
    FromCode {
        folder_name: StringLiteral,
        filename: PathBuf,
    },
}

impl ReadModuleFolderName {
    fn to_string(&self) -> String {
        match self {
            ReadModuleFolderName::FromCli { folder_name, .. } => folder_name.clone(),
            ReadModuleFolderName::FromCode { folder_name, .. } => folder_name.content.clone(),
        }
    }
}

/// `position` is optional if the `filename` is passed from CLI
pub fn read_module(
    module_meta: &ModuleMeta,
    imported_modules: &ImportedModules,
    path: PathBuf,
    dir: fs::ReadDir,
    entry_point_filename: Option<&PathBuf>,
) -> Result<UnifyProgramResult, CompileError> {
    let path_string = path.to_str().unwrap().to_string();
    let module_meta = ModuleMeta {
        uid: ModuleUid::Local {
            folder_relative_path: path_string.clone(),
        },
        import_relations: {
            match entry_point_filename {
                // If this is the entry point, no need to add import_relations, otherwise
                // this relation will be a file pointing to itself
                Some(_) => module_meta.import_relations.clone(),
                None => {
                    let mut importer_paths = module_meta.import_relations.clone();
                    importer_paths.push(ImportRelation {
                        importer_path: module_meta.uid.string_value(),
                        importee_path: path_string,
                    });
                    importer_paths
                }
            }
        },
    };
    let files = dir
        .into_iter()
        .filter_map(|dir_entry| {
            let dir_entry = dir_entry.unwrap();

            if fs::metadata(dir_entry.path()).unwrap().is_file()
                && dir_entry
                    .path()
                    .extension()
                    .map(|str| str.eq("kk"))
                    .unwrap_or(false)
            {
                Some(dir_entry)
            } else {
                None
            }
        })
        .map(|dir_entry| {
            let filename = dir_entry.file_name().to_str().unwrap().to_string();
            Ok(File {
                name: filename.clone(),
                path: dir_entry.path(),
                statements: Parser::parse(&mut Tokenizer::new(
                    fs::read_to_string(dir_entry.path()).expect(
                        format!("Unable to read file: {}", dir_entry.path().display()).as_str(),
                    ),
                ))
                .map_err(|error| CompileError {
                    path: dir_entry.path(),
                    kind: CompileErrorKind::ParseError(Box::new(error)),
                })?,
                is_entry_point: false,
            })
        })
        .collect::<Result<Vec<_>, CompileError>>()?;

    // Typecheck the imported module
    unify_statements(module_meta, files, imported_modules, entry_point_filename)
}

pub struct File {
    name: String,
    path: PathBuf,
    statements: Vec<Statement>,
    is_entry_point: bool,
}

pub fn unify_statements(
    module_meta: ModuleMeta,
    files: Vec<File>,
    imported_modules: &ImportedModules,
    entry_point_filename: Option<&PathBuf>,
) -> Result<UnifyProgramResult, CompileError> {
    // 1. Partition statements based on their types
    let (
        import_statements,
        type_statements,
        enum_statements,
        let_statements,
        top_level_expressions,
        doc_string_codes,
    ) = {
        let mut import_statements = Vec::new();
        let mut type_statements = Vec::new();
        let mut enum_statements = Vec::new();
        let mut let_statements = Vec::new();
        let mut top_level_expressions = Vec::new();
        let mut doc_string_codes = Vec::new();

        for file in files {
            let path = file.path;
            for statement in file.statements {
                match statement {
                    Statement::Type(type_statement) => {
                        type_statements.push((path.clone(), type_statement));
                    }
                    Statement::Enum(enum_statement) => {
                        enum_statements.push((path.clone(), enum_statement));
                    }
                    Statement::Let(let_statement) => {
                        match &let_statement.doc_string {
                            Some(doc_string) => doc_string_codes.extend(
                                doc_string
                                    .extract_codes()
                                    .map_err(|parse_error| CompileError {
                                        kind: CompileErrorKind::ParseError(Box::new(parse_error)),
                                        path: path.clone(),
                                    })?
                                    .into_iter()
                                    .map(|code| (path.clone(), code))
                                    .collect::<Vec<(PathBuf, Expression)>>(),
                            ),
                            None => {}
                        }
                        let_statements.push((path.clone(), let_statement));
                    }
                    Statement::Entry(entry_statement) => {
                        match entry_point_filename {
                            Some(entry_point_filename) if path.eq(entry_point_filename) => {
                                top_level_expressions
                                    .push((path.clone(), entry_statement.expression))
                            }
                            _ => {
                                // do nothing
                            }
                        }
                    }
                    Statement::Import(import_statement) => {
                        import_statements.push((path.clone(), import_statement))
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
            doc_string_codes,
        )
    };

    // 2. Infer import statements (to include every imported symbols into the current module)

    let mut module: Module = Module::new(module_meta);

    let init: (Vec<InferredStatement>, _) = (vec![], imported_modules.clone());

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
                                InferredStatement::ImportStatement(import_statement)
                            })
                            .collect::<Vec<InferredStatement>>(),
                    );

                    imported_modules.extend(current.imported_modules);

                    Ok((statements, imported_modules))
                }
            })?;

    // 3. Insert type symbols (including enums) into the current module.
    //    Note that we will first insert them into the current module first before checking their definition,
    //    this is so that mutually recursive type can be checked.

    // Insert enum types
    let enum_statements = enum_statements
        .into_iter()
        .map(|(path, enum_statement)| {
            let (enum_uid, enum_type) = insert_enum_symbol(&mut module, enum_statement.clone())
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
            Ok((path, enum_uid, enum_type, enum_statement))
        })
        .collect::<Result<Vec<_>, CompileError>>()?;

    // Check the definition of type alias statement
    type_statements
        .into_iter()
        .map(|(path, type_statement)| {
            infer_type_alias_statement(&mut module, type_statement)
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))
        })
        .collect::<Result<Vec<()>, CompileError>>()?;

    // Check the definition of enum statement
    let constructors_definitions = enum_statements
        .into_iter()
        .map(|(path, enum_uid, enum_type, enum_statement)| {
            infer_enum_statement(&mut module, &enum_uid, enum_type, enum_statement)
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))
        })
        .collect::<Result<Vec<_>, CompileError>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    // 4. Insert the type of top-level let statement (for implementing mutually recursive functions)
    let unchecked_let_statements = let_statements
        .into_iter()
        .map(|(path, let_statement)| {
            let type_value = type_annotation_to_type(&mut module, &let_statement.type_annotation)
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
            let name = let_statement.name.clone();
            let uid = module
                .insert_symbol(
                    None,
                    Symbol {
                        meta: SymbolMeta {
                            name: name.clone(),
                            access: let_statement.access.clone(),
                        },
                        kind: SymbolKind::Value(ValueSymbol { type_value }),
                    },
                )
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
            Ok((path, uid, name, let_statement))
        })
        .collect::<Result<Vec<_>, CompileError>>()?;

    // 5. Insert top-level constant expression (i.e. expressions that does not have direct function call)
    let typechecked_let_statements = unchecked_let_statements
        .into_iter()
        .map(|(path, uid, name, let_statement)| {
            if let Some(position) = has_direct_function_call(&let_statement.expression) {
                Err(CompileError {
                    path: path.clone(),
                    kind: CompileErrorKind::UnifyError(Box::new(UnifyError {
                        position,
                        kind: UnifyErrorKind::FunctionCallAtTopLevelIsNotAllowed,
                    })),
                })
            } else {
                let expected_type =
                    type_annotation_to_type(&mut module, &let_statement.type_annotation)
                        .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
                let right = infer_expression_type(
                    &mut module,
                    Some(expected_type.clone()),
                    &let_statement.expression,
                )
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
                Ok(InferredStatement::Let {
                    access: let_statement.access,
                    left: InferredDestructurePattern {
                        type_value: right.type_value,
                        kind: InferredDestructurePatternKind::Identifier(Box::new(Identifier {
                            uid,
                            token: name,
                        })),
                    },
                    right: right.expression,
                })
            }
        })
        .collect::<Result<Vec<InferredStatement>, CompileError>>()?;

    // 6. Then we will infer expression statements
    let typechecked_top_level_expressions_statements = top_level_expressions
        .into_iter()
        .map(|(path, expression)| {
            let expression = infer_expression_type(&mut module, Some(Type::Unit), &expression)
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))?;
            Ok(InferredStatement::Expression(expression.expression))
        })
        .collect::<Result<Vec<InferredStatement>, CompileError>>()?;

    // 7. Lastly, we typecheck the docstring codes
    doc_string_codes
        .into_iter()
        .map(|(path, expression)| {
            infer_expression_type(&mut module, None, &expression)
                .map_err(|unify_error| unify_error.into_compile_error(path.clone()))
        })
        .collect::<Result<Vec<_>, CompileError>>()?;
    Ok(UnifyProgramResult {
        entrypoint: InferredModule {
            module,
            statements: {
                let mut statements = Vec::new();
                statements.extend(typechecked_import_statements);
                statements.extend(typechecked_let_statements);
                statements.extend(constructors_definitions);
                statements.extend(typechecked_top_level_expressions_statements);
                statements
            },
        },
        imported_modules,
    })
}

#[derive(Debug, Clone)]
pub struct PopulatedTypeVariables {
    pub declared_type_variables: NonEmpty<ExplicitTypeVariable>,
}

/// Populate explicit type variables into the current environment.   
/// Also validate the required constraints.   
/// A list of SymbolUid will be returned.
/// Each SymbolUid correspond to each implementation-dictionary.
///
/// For example, if the given `type_variables_declaration` contains
/// 3 constraints, then 3 SymbolUid will be returned.  
///
/// The caller of this function should parameterise the corresponding expression with the returned list of SymbolUid.
fn populate_explicit_type_variables(
    module: &mut Module,
    type_variables_declaration: &Option<TypeVariablesDeclaration>,
) -> Result<Option<PopulatedTypeVariables>, UnifyError> {
    match type_variables_declaration {
        None => Ok(None),
        Some(declaration) => {
            declaration
                .type_variables
                .clone()
                .fold_result(|type_variable| {
                    module.insert_explicit_type_variable(&type_variable)
                })?;

            let declared_type_variables =
                declaration
                    .type_variables
                    .clone()
                    .map(|type_variable| ExplicitTypeVariable {
                        name: type_variable.representation,
                    });

            Ok(Some(PopulatedTypeVariables {
                declared_type_variables,
            }))
        }
    }
}

/// Check that an expression does not have direct function call
/// If found, return the position of the function call
fn has_direct_function_call(expression: &Expression) -> Option<Position> {
    match expression {
        Expression::Unit { .. }
        | Expression::Float(_)
        | Expression::Integer(_)
        | Expression::Character(_)
        | Expression::Identifier(_)
        | Expression::String(_)
        | Expression::CpsBang { .. }
        | Expression::Keyword(_)
        | Expression::Function(_) => None,
        Expression::FunctionCall(function_call) => Some(function_call.function.position()),
        Expression::InterpolatedString { sections, .. } => {
            sections.find_map(|section| match section {
                InterpolatedStringSection::Expression(expression) => {
                    has_direct_function_call(expression.as_ref())
                }
                _ => None,
            })
        }
        Expression::EnumConstructor { payload, .. } => match payload {
            None => None,
            Some(expression) => has_direct_function_call(&expression),
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
            })
        }),
        Expression::RecordAccess { expression, .. } => has_direct_function_call(&expression),
        Expression::Array { elements, .. } => elements.iter().find_map(has_direct_function_call),
        Expression::Parenthesized { value, .. } => has_direct_function_call(value),
        Expression::Let { body, .. } => has_direct_function_call(&body),
        Expression::Statements { current, next } => {
            has_direct_function_call(current).or(has_direct_function_call(next))
        }
        Expression::CpsClosure { expression, .. } => has_direct_function_call(expression),
    }
}

#[derive(Debug)]
pub struct UnifyError {
    pub position: Position,
    pub kind: UnifyErrorKind,
}

impl UnifyError {
    pub fn into_compile_error(self, path: PathBuf) -> CompileError {
        CompileError {
            path,
            kind: CompileErrorKind::UnifyError(Box::new(self)),
        }
    }
}

#[derive(Debug)]
pub enum UnifyErrorKind {
    AmbiguousSymbol {
        matching_value_symbols: Vec<(SymbolUid, ValueSymbol)>,
    },
    ConstraintUnsatisfied {
        interface_name: String,
        for_types: NonEmpty<Type>,
    },
    UnsatisfiedConstraint {
        missing_constraint: TypeConstraint,
    },
    UnknownTypeVariable {
        unknown_type_variable_name: String,
    },
    ExtraneousBinding {
        extraneous_binding: Binding,
        expected_bindings: Vec<Binding>,
    },
    MissingBindings {
        missing_expected_bindings: NonEmpty<Binding>,
    },
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
    CannotImportProtectedSymbol,
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
    NoMatchingValueSymbol {
        possible_value_symbols: Vec<(SymbolUid, ValueSymbol)>,
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
    PartiallyUnreachableCase {
        redundant_expanded_pattern: CheckablePatternKind,
    },
    TypeArgumentsLengthMismatch {
        actual_length: usize,
        expected_type_parameter_names: Vec<String>,
    },
    RecordExtraneousKey {
        expected_keys: Vec<String>,
    },
    RecordMissingKeys {
        missing_keys: NonEmpty<String>,
    },
    CannotInvokeNonFunction {
        actual_type: Type,
    },
    DuplicatedIdentifier {
        name: String,
        first_declared_at: Position,
        then_declared_at: Position,
    },
    InvalidArgumentLength {
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
    TopLevelLetStatementCannotBeDestructured,
    MissingTypeAnnotationForTopLevelBinding,
    CannotBeOverloaded {
        name: String,
    },
}

/// We use IndexMap instead of HashMap for storing imported modules because we need to preserve the insertion order,
/// in order for the transpiled code to work (i.e. to prevent referencing variable that is not yet declared).
/// Of course, this only work if the dependency graph contains no cycle,
/// fortunately, this language do not allow cyclic imports.
type ImportedModules = IndexMap<ModuleUid, InferredModule>;

#[derive(Debug, Clone)]
pub struct InferredModule {
    pub module: Module,
    pub statements: Vec<InferredStatement>,
}
pub struct InferImportStatementResult {
    pub import_statements: Vec<InferredImportStatement>,
    pub imported_modules: ImportedModules,
}

pub fn infer_import_statement(
    module: &mut Module,
    imported_modules: &ImportedModules,
    (path, import_statement): (PathBuf, ImportStatement),
) -> Result<InferImportStatementResult, CompileError> {
    // Look up for memoize imported modules
    let inferred_right = {
        let importer_path = module.uid().string_value();

        let import_path = import_statement.url.content.trim_matches('"');
        let import_path = to_relative_path(
            PathBuf::from(format!("{}/{}", importer_path, import_path))
                .canonicalize()
                .map_err(|error| CompileError {
                    path: path.clone(),
                    kind: CompileErrorKind::UnifyError(Box::new(UnifyError {
                        position: import_statement.url.position(),
                        kind: UnifyErrorKind::ErrorneousImportPath {
                            extra_information: error.to_string(),
                        },
                    })),
                })?,
        )
        .unwrap();
        let path_string = import_path.to_str().unwrap().to_string();
        if module
            .meta
            .import_relations
            .iter()
            .any(|relation| path_string == relation.importer_path)
        {
            return Err(UnifyError {
                position: import_statement.url.position(),
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
            .into_compile_error(path.clone()));
        }
        let uid = ModuleUid::Local {
            folder_relative_path: import_path.to_str().unwrap().to_string(),
        };

        // Look up for memoize imported modules
        match imported_modules.get(&uid) {
            Some(module) => {
                println!("Compile cache hit: {}", uid.string_value());
                Ok(UnifyProgramResult {
                    entrypoint: module.clone(),
                    imported_modules: IndexMap::new(),
                })
            }
            None => match fs::read_dir(import_path.clone()) {
                Err(error) => {
                    return Err(UnifyError {
                        position: import_statement.url.position(),
                        kind: UnifyErrorKind::ErrorneousImportPath {
                            extra_information: format!("{}", error),
                        },
                    }
                    .into_compile_error(import_path.clone()))
                }
                Ok(dir) => read_module(&module.meta, imported_modules, import_path, dir, None),
            },
        }
    }?;

    // Check whether each imported name exists and is exported in this program
    let matching_symbol_entries: Vec<(SymbolEntry, Option<Token>)> =
        match import_statement.specification {
            // Import all symbols
            None => inferred_right
                .entrypoint
                .module
                .get_all_exported_symbols()
                .into_iter()
                .map(|entry| {
                    let name = entry.symbol.meta.name.clone();
                    (
                        entry,
                        Some(Token {
                            position: import_statement.keyword_import.position,
                            ..name
                        }),
                    )
                })
                .collect(),

            // Not importing all symbols
            Some(specification) => specification
                .aliases
                .into_iter()
                .map(|alias| {
                    let matching_symbol_entries = inferred_right
                        .entrypoint
                        .module
                        .get_all_matching_symbols(&alias.name);

                    if matching_symbol_entries.is_empty() {
                        Err(UnifyError {
                            position: alias.name.position,
                            kind: UnifyErrorKind::UnknownImportedName,
                        }
                        .into_compile_error(path.clone()))
                    } else {
                        Ok(matching_symbol_entries
                            .into_iter()
                            .map(|entry| match entry.symbol.meta.access {
                                Access::Protected { .. } => Err(UnifyError {
                                    position: alias.name.position.clone(),
                                    kind: UnifyErrorKind::CannotImportProtectedSymbol,
                                }
                                .into_compile_error(path.clone())),
                                _ => Ok((
                                    entry,
                                    Some(match &alias.alias {
                                        Some(alias) => alias.clone(),
                                        None => alias.name.clone(),
                                    }),
                                )),
                            })
                            .collect::<Result<Vec<(SymbolEntry, Option<Token>)>, CompileError>>()?)
                    }
                })
                .collect::<Result<Vec<Vec<(SymbolEntry, Option<Token>)>>, CompileError>>()?
                .into_iter()
                .flatten()
                .collect(),
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
                        access: Access::Protected,
                    },
                    kind: entry.symbol.kind.clone(),
                },
            ) {
                Ok(uid) => {
                    // only insert import statement if the imported symbol is a value (i.e. not type)
                    match entry.symbol.kind {
                        SymbolKind::Value(_) => Ok(vec![InferredImportStatement {
                            module_uid: inferred_right.entrypoint.module.uid(),
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
                Err(unify_error) => Err(unify_error.into_compile_error(path.clone())),
            }
        })
        .collect::<Result<Vec<Vec<InferredImportStatement>>, CompileError>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(InferImportStatementResult {
        import_statements,
        imported_modules: {
            let mut imported_modules = inferred_right.imported_modules;
            let uid = inferred_right.entrypoint.module.uid();
            imported_modules.insert(uid, inferred_right.entrypoint);
            imported_modules
        },
    })
}

/// Insert the symbol of an enum into the current module without checking it's body.  
/// Returns the UID of this enum.
pub fn insert_enum_symbol(
    module: &mut Module,
    enum_statement: EnumStatement,
) -> Result<(SymbolUid, Type), UnifyError> {
    let enum_uid = module.get_next_symbol_uid();
    let enum_name = enum_statement.name.representation.clone();
    let enum_type = Type::Named {
        symbol_uid: enum_uid.clone(),
        name: enum_name,
        type_arguments: enum_statement
            .type_variables()
            .into_iter()
            .map(|type_variable| {
                (
                    type_variable.representation.clone(),
                    Type::ExplicitTypeVariable(ExplicitTypeVariable {
                        name: type_variable.representation,
                    }),
                )
            })
            .collect(),
    };
    let type_variable_names = enum_statement
        .type_variables()
        .into_iter()
        .map(|type_variable| type_variable.representation)
        .collect::<Vec<String>>();

    let type_value = match type_variable_names.split_first() {
        None => enum_type,
        Some((head, tail)) => Type::TypeScheme(Box::new(TypeScheme {
            type_variables: NonEmpty {
                head: ExplicitTypeVariable { name: head.clone() },
                tail: tail
                    .to_vec()
                    .into_iter()
                    .map(|name| ExplicitTypeVariable { name })
                    .collect(),
            },
            type_value: enum_type,
        })),
    };

    let uid = module.insert_symbol(
        Some(enum_uid),
        Symbol {
            meta: SymbolMeta {
                name: enum_statement.name,
                access: enum_statement.access,
            },
            kind: SymbolKind::Type(TypeSymbol {
                type_value: type_value.clone(),
            }),
        },
    )?;
    Ok((uid, type_value))
}

/// Validate the body of an enum statement
/// and insert the constructors into the current module
/// and insert the constructors as functions
pub fn infer_enum_statement(
    module: &mut Module,
    enum_uid: &SymbolUid,
    enum_type: Type,
    enum_statement: EnumStatement,
) -> Result<Vec<InferredStatement>, UnifyError> {
    let constructor_symbols = module.run_in_new_child_scope(|module| {
        // 1. Populate type variables into current module
        populate_explicit_type_variables(module, &enum_statement.type_variables_declaration)?;

        // 2. Validate the definition of each constructor
        enum_statement
            .constructors
            .iter()
            .map(|constructor| {
                Ok((
                    constructor,
                    EnumConstructorSymbol {
                        enum_uid: enum_uid.clone(),
                        enum_name: enum_statement.name.representation.clone(),
                        constructor_name: constructor.name.representation.clone(),
                        type_variables: enum_statement
                            .type_variables()
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
                    },
                ))
            })
            .collect::<Result<Vec<_>, UnifyError>>()
    })?;

    constructor_symbols
        .into_iter()
        .map(|(constructor, constructor_symbol)| {
            // 3. Add each constructor into the symbol tables
            module.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: constructor.name.clone(),
                        access: constructor.access.clone(),
                    },
                    kind: SymbolKind::EnumConstructor(constructor_symbol.clone()),
                },
            )?;

            // 4. Create a function for each constructor, so that constructors does not require special
            //    treatment

            let type_value = {
                match (&constructor_symbol.payload, &enum_type) {
                    // If the enum_type is a type scheme, we need to inject the payload type into
                    // the type scheme
                    (Some(payload), Type::TypeScheme(type_scheme)) => {
                        Type::TypeScheme(Box::new(TypeScheme {
                            type_variables: type_scheme.type_variables.clone(),
                            type_value: Type::Function(FunctionType {
                                parameter_type: Box::new(payload.clone()),
                                return_type: Box::new(type_scheme.type_value.clone()),
                                type_constraints: vec![],
                            }),
                        }))
                    }

                    // If the enum_type is not a type scheme, we can simply construct a function
                    // type, using the enum_type as the return type
                    (Some(payload), _) => Type::Function(FunctionType {
                        parameter_type: Box::new(payload.clone()),
                        return_type: Box::new(enum_type.clone()),
                        type_constraints: vec![],
                    }),

                    // If there's no payload, the type of this constructor will be just the
                    // enum_tpye
                    _ => enum_type.clone(),
                }
            };

            let value_symbol = Symbol {
                meta: SymbolMeta {
                    name: constructor.name.clone(),
                    access: enum_statement.access.clone(),
                },
                kind: SymbolKind::Value(ValueSymbol {
                    type_value: type_value.clone(),
                }),
            };

            let uid = module.insert_symbol(None, value_symbol)?;
            Ok(InferredStatement::Let {
                access: constructor.access.clone(),
                left: InferredDestructurePattern {
                    type_value: type_value.clone(),
                    kind: InferredDestructurePatternKind::Identifier(Box::new(Identifier {
                        uid,
                        token: constructor.name.clone(),
                    })),
                },
                right: match &constructor_symbol.payload {
                    None => InferredExpression::EnumConstructor {
                        constructor_name: constructor.name.representation.clone(),
                        payload: None,
                    },
                    Some(payload_type) => {
                        InferredExpression::BranchedFunction(Box::new(InferredBranchedFunction {
                            branches: Box::new(NonEmpty {
                                head: {
                                    let dummy_identifier = Identifier {
                                        uid: module.get_next_symbol_uid(),
                                        token: Token::dummy_identifier(
                                            "$$constructor_payload_parameter$$".to_string(),
                                        ),
                                    };
                                    InferredFunctionBranch {
                                        parameter: Box::new(InferredDestructurePattern {
                                            type_value: payload_type.clone(),
                                            kind: InferredDestructurePatternKind::Identifier(
                                                Box::new(dummy_identifier.clone()),
                                            ),
                                        }),
                                        body: Box::new(InferExpressionResult {
                                            expression: InferredExpression::EnumConstructor {
                                                constructor_name: constructor
                                                    .name
                                                    .representation
                                                    .clone(),
                                                payload: Some(Box::new(
                                                    InferredExpression::Variable(dummy_identifier),
                                                )),
                                            },
                                            type_value,
                                        }),
                                    }
                                },
                                tail: vec![],
                            }),
                        }))
                    }
                },
            })
        })
        .collect::<Result<Vec<_>, UnifyError>>()
}

/// Validate the body of a type alias statement
pub fn infer_type_alias_statement(
    module: &mut Module,
    TypeAliasStatement {
        access,
        left,
        right,
        type_variables_declaration,
        ..
    }: TypeAliasStatement,
) -> Result<(), UnifyError> {
    let type_value = module.run_in_new_child_scope(|module| {
        // 1. Populate type variables into current module
        populate_explicit_type_variables(module, &type_variables_declaration)?;

        // 2. verify type declaration
        type_annotation_to_type(module, &right)
    })?;
    // 3. Add this type symbol into this module
    module.insert_symbol(
        None,
        Symbol {
            meta: SymbolMeta { name: left, access },
            kind: SymbolKind::Type(TypeSymbol {
                type_value: try_lift_as_type_scheme(
                    type_value,
                    type_variables_declaration
                        .map(|declaration| {
                            declaration
                                .type_variables
                                .into_vector()
                                .iter()
                                .map(|type_variable| ExplicitTypeVariable {
                                    name: type_variable.representation.clone(),
                                })
                                .collect()
                        })
                        .unwrap_or_else(Vec::new),
                ),
            }),
        },
    )?;

    Ok(())
}

pub fn try_lift_as_type_scheme(
    type_value: Type,
    type_variables: Vec<ExplicitTypeVariable>,
) -> Type {
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

impl Positionable for TypeAnnotation {
    fn position(&self) -> Position {
        match self {
            TypeAnnotation::Underscore(token) => token.position,
            TypeAnnotation::Function(FunctionTypeAnnotation {
                parameter,
                return_type,
                type_constraints,
            }) => parameter
                .position()
                .join(return_type.as_ref().position())
                .join_maybe(match type_constraints {
                    Some(type_constraints) => Some(type_constraints.position()),
                    None => None,
                }),
            TypeAnnotation::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
            TypeAnnotation::Record {
                left_curly_bracket,
                right_curly_bracket,
                ..
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            TypeAnnotation::Named {
                name,
                type_arguments,
            } => match type_arguments {
                None => name.position.join(name.position),
                Some(TypeArguments {
                    right_angular_bracket,
                    ..
                }) => name.position.join(right_angular_bracket.position),
            },
            TypeAnnotation::Quoted {
                opening_backtick,
                closing_backtick,
                ..
            } => opening_backtick.position.join(closing_backtick.position),
            TypeAnnotation::Scheme {
                type_variables,
                type_annotation,
            } => type_variables.position().join(type_annotation.position()),
            TypeAnnotation::Parenthesized {
                left_parenthesis,
                right_parenthesis,
                ..
            }
            | TypeAnnotation::Unit {
                left_parenthesis,
                right_parenthesis,
            } => left_parenthesis.position.join(right_parenthesis.position),
            TypeAnnotation::Keyword { identifier } => identifier.position,
        }
    }
}

impl Positionable for DestructurePattern {
    fn position(&self) -> Position {
        match self {
            DestructurePattern::Infinite { token, .. }
            | DestructurePattern::Identifier(token)
            | DestructurePattern::Underscore(token) => token.position,
            DestructurePattern::EnumConstructor { name, payload, .. } => match payload {
                None => name.position,
                Some(payload) => name.position.join(payload.position()),
            },

            DestructurePattern::Unit {
                left_parenthesis,
                right_parenthesis,
            } => left_parenthesis.position.join(right_parenthesis.position),
            DestructurePattern::Record {
                hash_left_curly_bracket: left_curly_bracket,
                right_curly_bracket,
                ..
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            DestructurePattern::Array {
                left_square_bracket,
                right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
            DestructurePattern::Tuple(tuple) => match &tuple.parentheses {
                Some((left, right)) => left.position.join(right.position),
                None => tuple.values.clone().position(),
            },
            DestructurePattern::Or(patterns) => patterns.clone().position(),
            DestructurePattern::Parenthesized {
                left_parenthesis,
                right_parenthesis,
                ..
            } => left_parenthesis.position.join(right_parenthesis.position),
        }
    }
}

impl Position {
    pub fn join(self, other: Position) -> Position {
        let start_position = self.min(other);
        let end_position = self.max(other);
        Position {
            line_start: start_position.line_start,
            column_start: start_position.column_start,
            line_end: end_position.line_end,
            column_end: end_position.column_end,
            character_index_start: start_position.character_index_start,
            character_index_end: end_position.character_index_end,
        }
    }
    pub fn join_maybe(self, other: Option<Position>) -> Position {
        match other {
            Some(other) => self.join(other),
            None => self,
        }
    }
}

impl Positionable for Expression {
    fn position(&self) -> Position {
        match self {
            Expression::Array {
                hash_left_parenthesis: left_square_bracket,
                right_parenthesis: right_square_bracket,
                ..
            } => left_square_bracket
                .position
                .join(right_square_bracket.position),
            Expression::Character(token)
            | Expression::Float(token)
            | Expression::Integer(token)
            | Expression::Identifier(token) => token.position,

            Expression::Unit {
                left_parenthesis,
                right_parenthesis,
            } => left_parenthesis.position.join(right_parenthesis.position),

            Expression::String(string_literal) => string_literal.position(),
            Expression::InterpolatedString {
                start_quotes,
                end_quotes,
                ..
            } => start_quotes
                .first()
                .position()
                .join(end_quotes.last().position()),
            Expression::EnumConstructor { name, payload, .. } => match payload {
                None => name.position,
                Some(payload) => name.position.join(payload.position()),
            },
            Expression::RecordAccess {
                expression,
                property_name,
            } => expression.position().join(property_name.position),
            Expression::Record {
                hash_left_curly_bracket: left_curly_bracket,
                right_curly_bracket,
                ..
            } => left_curly_bracket
                .position
                .join(right_curly_bracket.position),
            Expression::RecordUpdate {
                expression,
                right_curly_bracket,
                ..
            } => expression
                .as_ref()
                .position()
                .join(right_curly_bracket.position),
            Expression::FunctionCall(function_call) => {
                let argument_position = function_call.argument.as_ref().position();
                let function_position = function_call.function.as_ref().position();

                argument_position.join(function_position)
            }
            Expression::Function(function) => function
                .branches
                .first()
                .parameter
                .position()
                .join(function.branches.last().body.position()),
            Expression::Parenthesized {
                left_parenthesis,
                right_parenthesis,
                ..
            } => left_parenthesis.position.join(right_parenthesis.position),
            Expression::Let {
                keyword_let, body, ..
            } => keyword_let.position.join(body.position()),
            Expression::Statements { current, next } => current.position().join(next.position()),
            Expression::CpsClosure { tilde, expression } => {
                tilde.position.join(expression.position())
            }
            Expression::CpsBang {
                argument, function, ..
            } => argument.position().join(function.position()),
            Expression::Keyword(identifier) => identifier.position,
        }
    }
}

pub trait Positionable {
    fn position(&self) -> Position;
}

impl<T: Positionable> NonEmpty<T> {
    pub fn position(self) -> Position {
        let init = self.first().position();
        self.into_vector()
            .into_iter()
            .fold(init, |result, current| result.join(current.position()))
    }
}

impl Positionable for Statement {
    fn position(&self) -> Position {
        match self {
            Statement::Let(let_statement) => let_statement
                .keyword_let
                .position
                .join(let_statement.expression.position()),

            Statement::Type(type_statement) => type_statement
                .keyword_type
                .position
                .join(type_statement.right.position()),
            Statement::Enum(enum_statement) => enum_statement.keyword_class.position.join_maybe(
                enum_statement
                    .constructors
                    .last()
                    .map(|constructor| constructor.position()),
            ),
            Statement::Entry(entry_statement) => entry_statement
                .keyword_entry
                .position
                .join(entry_statement.expression.position()),
            Statement::Import(import_statement) => import_statement
                .keyword_import
                .position
                .join_maybe(match &import_statement.specification {
                    None => None,
                    Some(specification) => Some(specification.position()),
                }),
        }
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
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Character, Type::Character) => Ok(Type::Character),
        (Type::Unit, Type::Unit) => Ok(Type::Unit),
        (Type::Keyword(a), Type::Keyword(b)) if a == b => Ok(Type::Keyword(a)),
        (
            Type::ExplicitTypeVariable(expected_type_variable),
            Type::ExplicitTypeVariable(actual_type_variable),
        ) => {
            if expected_type_variable.name != actual_type_variable.name {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::TypeMismatch {
                        expected_type: Type::ExplicitTypeVariable(expected_type_variable),
                        actual_type: Type::ExplicitTypeVariable(actual_type_variable),
                    },
                })
            } else {
                Ok(Type::ExplicitTypeVariable(expected_type_variable))
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
            Type::ImplicitTypeVariable(expected_type_variable),
            Type::ImplicitTypeVariable(actual_type_variable),
        ) => {
            // This part is where the Hindley-Milner type inference magic happens
            // This part is also known as the Type Variable Unification
            // Refer https://www.cs.tau.ac.il/~msagiv/courses/apl12/types.pdf

            if expected_type_variable.name != actual_type_variable.name {
                let expected_type =
                    module.get_type_variable_terminal_type(expected_type_variable.name.clone());
                let actual_type =
                    module.get_type_variable_terminal_type(actual_type_variable.name.clone());

                match (expected_type, actual_type) {
                    (None, Some(actual_type)) => {
                        module.update_substitution(
                            expected_type_variable,
                            actual_type.clone(),
                            position,
                        )?;
                        Ok(actual_type)
                    }
                    (Some(expected_type), None) => {
                        module.update_substitution(
                            actual_type_variable,
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
                            expected_type_variable.clone(),
                            Type::ImplicitTypeVariable(actual_type_variable),
                            position,
                        )?;
                        Ok(Type::ImplicitTypeVariable(expected_type_variable))
                    }
                }
            } else {
                Ok(actual.clone())
            }
        }
        (Type::ImplicitTypeVariable(type_variable), other_type)
        | (other_type, Type::ImplicitTypeVariable(type_variable)) => {
            // This check is necessar to prevent infinite loop
            if type_variable_occurs_in_type(&type_variable.name, &other_type) {
                Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::InfiniteTypeDetected {
                        type_variable_name: type_variable.name,
                        in_type: other_type,
                    },
                })
            } else {
                // This is the magical part that makes type inference works
                module.update_substitution(type_variable, other_type.clone(), position)?;
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
                key_type_pairs: expected_key_type_pairs,
            },
            Type::Record {
                key_type_pairs: actual_key_type_pairs,
            },
        ) => {
            let zipped = match_key_values_pairs(
                &expected_key_type_pairs,
                &actual_key_type_pairs,
                |(key, _)| key,
                |(key, _)| key,
                PartialEq::eq,
            )
            .map_err(|error| UnifyError {
                position,
                kind: match error {
                    MatchKeyValueError::MissingKeys(missing_keys) => {
                        UnifyErrorKind::RecordMissingKeys { missing_keys }
                    }
                    MatchKeyValueError::ExtraneousKey(_) => UnifyErrorKind::RecordExtraneousKey {
                        expected_keys: expected_key_type_pairs
                            .clone()
                            .into_iter()
                            .map(|(key, _)| key)
                            .collect::<Vec<String>>(),
                    },
                },
            })?;

            let key_type_pairs = zipped
                .into_iter()
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

pub enum MatchKeyValueError<ExpectedKey, ActualKey> {
    MissingKeys(NonEmpty<ExpectedKey>),
    ExtraneousKey(NonEmpty<ActualKey>),
}

/// Match the actual pairs against the expected pairs.  
/// Pairs means key-value pairs.
pub fn match_key_values_pairs<A: Clone, B: Clone, ExpectedKey: Clone, ActualKey: Clone, F, G, H>(
    expected_pairs: &Vec<A>,
    actual_pairs: &Vec<B>,
    get_expected_pair_key: F,
    get_actual_pair_key: G,
    compare_key: H,
) -> Result<Vec<(A, B)>, MatchKeyValueError<ExpectedKey, ActualKey>>
where
    F: Fn(&A) -> &ExpectedKey,
    G: Fn(&B) -> &ActualKey,
    H: Fn(&ExpectedKey, &ActualKey) -> bool,
{
    use itertools::Either;

    let (zipped, missing_expected_keys): (Vec<(A, B)>, Vec<ExpectedKey>) =
        expected_pairs.iter().partition_map(|expected_pair| {
            let expected_pair_key = get_expected_pair_key(expected_pair);
            let matching_actual_pair = actual_pairs.iter().find(|actual_pair| {
                compare_key(expected_pair_key, get_actual_pair_key(actual_pair))
            });
            match matching_actual_pair {
                Some(matching_actual_pair) => {
                    Either::Left((expected_pair.clone(), matching_actual_pair.clone()))
                }
                None => Either::Right(expected_pair_key.clone()),
            }
        });

    match missing_expected_keys.split_first() {
        Some((head, tail)) => Err(MatchKeyValueError::MissingKeys(NonEmpty {
            head: head.clone(),
            tail: tail.to_vec(),
        })),
        None => Ok(()),
    }?;

    // Find extraneous key from actual pairs
    let extraneous_pair = actual_pairs
        .iter()
        .filter(|actual_pair| {
            !expected_pairs.iter().any(|expected_pair| {
                compare_key(
                    get_expected_pair_key(expected_pair),
                    get_actual_pair_key(actual_pair),
                )
            })
        })
        .collect::<Vec<&B>>();

    match extraneous_pair.split_first() {
        Some((head, tail)) => Err(MatchKeyValueError::ExtraneousKey(NonEmpty {
            head: get_actual_pair_key(head).clone(),
            tail: tail
                .iter()
                .map(|pair| get_actual_pair_key(pair).clone())
                .collect(),
        })),
        None => Ok(zipped),
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
        Type::String => Type::String,
        Type::Character => Type::Character,
        Type::Unit => Type::Unit,
        Type::Keyword(identifier) => Type::Keyword(identifier),

        Type::ExplicitTypeVariable(type_variable) => {
            if type_variable.name == *from_type_variable {
                to_type.clone()
            } else {
                Type::ExplicitTypeVariable(type_variable)
            }
        }
        // => Type::ExplicitTypeVariable { name },
        Type::ImplicitTypeVariable(type_variable) => {
            if type_variable.name == *from_type_variable {
                to_type.clone()
            } else {
                Type::ImplicitTypeVariable(type_variable)
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
            parameter_type,
            return_type,
            type_constraints,
        }) => Type::Function(FunctionType {
            parameter_type: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *parameter_type,
            )),
            return_type: Box::new(rewrite_type_variable_in_type(
                from_type_variable,
                to_type,
                *return_type,
            )),
            type_constraints: type_constraints
                .into_iter()
                .map(|type_constraint| TypeConstraint {
                    name: type_constraint.name,
                    type_value: rewrite_type_variable_in_type(
                        from_type_variable,
                        to_type,
                        type_constraint.type_value,
                    ),
                })
                .collect(),
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
    constructor_name: &Token,
) -> Result<GetEnumTypeResult, UnifyError> {
    let expected_enum_uid = match &expected_type {
        Some(Type::Named { symbol_uid, .. }) => Some(symbol_uid.clone()),
        _ => None,
    };
    // Look up constructor
    let constructor = module.get_constructor_symbol(expected_enum_uid, &constructor_name)?;

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
                        .map(|type_variable| {
                            (
                                type_variable.name.clone(),
                                Type::ImplicitTypeVariable(ImplicitTypeVariable {
                                    name: module.get_next_type_variable_name(),
                                }),
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

#[derive(Debug, Clone)]
pub struct InferExpressionResult {
    pub expression: InferredExpression,
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
        expression.position(),
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
        Expression::Unit { .. } => Ok(InferExpressionResult {
            expression: InferredExpression::Unit,
            type_value: Type::Unit,
        }),
        Expression::String(string_literal) => Ok(InferExpressionResult {
            type_value: Type::String,
            expression: InferredExpression::String(string_literal.clone()),
        }),
        Expression::Keyword(keyword) => Ok(InferExpressionResult {
            type_value: Type::Keyword(keyword.representation.clone()),
            expression: InferredExpression::Keyword(keyword.clone()),
        }),
        Expression::InterpolatedString { sections, .. } => {
            let typechecked_sections = sections
                .clone()
                .into_vector()
                .into_iter()
                .map(|section| match section {
                    InterpolatedStringSection::String(string) => {
                        Ok(InferredInterpolatedStringSection::String(string.clone()))
                    }
                    InterpolatedStringSection::Expression(expression) => {
                        let expression =
                            infer_expression_type(module, Some(Type::String), expression.as_ref())?;
                        Ok(InferredInterpolatedStringSection::Expression(Box::new(
                            expression,
                        )))
                    }
                })
                .collect::<Result<Vec<InferredInterpolatedStringSection>, UnifyError>>()?;

            Ok(InferExpressionResult {
                type_value: Type::String,
                expression: InferredExpression::InterpolatedString {
                    sections: typechecked_sections.clone(),
                },
            })
        }
        Expression::Character(token) => Ok(InferExpressionResult {
            type_value: Type::Character,
            expression: InferredExpression::Character {
                representation: token.representation.clone(),
            },
        }),
        Expression::Float(token) => Ok(InferExpressionResult {
            type_value: Type::Float,
            expression: InferredExpression::Float {
                representation: token.representation.clone(),
            },
        }),
        Expression::Integer(token) => Ok(InferExpressionResult {
            type_value: Type::Integer,
            expression: InferredExpression::Integer {
                representation: token.representation.clone(),
            },
        }),
        Expression::EnumConstructor { name, payload, .. } => {
            let result = get_enum_type(module, expected_type, name)?;
            match (result.expected_payload_type, payload.clone()) {
                (None, None) => Ok(InferExpressionResult {
                    type_value: result.expected_enum_type,
                    expression: InferredExpression::EnumConstructor {
                        constructor_name: name.representation.clone(),
                        payload: None,
                    },
                }),
                (None, Some(payload)) => Err(UnifyError {
                    position: payload.position(),
                    kind: UnifyErrorKind::ThisEnumConstructorDoesNotRequirePayload,
                }),
                (Some(expected_payload_type), None) => Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::ThisEnumConstructorRequiresPaylod {
                        payload_type: expected_payload_type,
                    },
                }),
                (Some(expected_payload_type), Some(payload)) => {
                    let typechecked_payload =
                        infer_expression_type(module, Some(expected_payload_type), &payload)?;
                    Ok(InferExpressionResult {
                        type_value: result.expected_enum_type,
                        expression: InferredExpression::EnumConstructor {
                            constructor_name: name.representation.clone(),
                            payload: Some(Box::new(typechecked_payload.expression)),
                        },
                    })
                }
            }
        }
        Expression::Identifier(identifier) => {
            let result = module.get_value_symbol(
                &identifier,
                &expected_type,
                module.current_scope_name(),
            )?;

            // If this variable has type of type scheme, then instantiate the type scheme with fresh type variables.
            // Note that we have to bubble up the constraints provided by the type scheme if applicable.
            let identifier = Identifier {
                uid: result.symbol_uid,
                token: identifier.clone(),
            };
            let result = match result.type_value {
                Type::TypeScheme(type_scheme) => {
                    let type_value = instantiate_type_scheme(module, *type_scheme);

                    Ok(InferExpressionResult {
                        type_value,
                        expression: InferredExpression::Variable(identifier),
                    })
                }
                other => Ok(InferExpressionResult {
                    type_value: other,
                    expression: InferredExpression::Variable(identifier),
                }),
            }?;
            Ok(result)
        }
        Expression::RecordAccess {
            expression,
            property_name,
        } => {
            let subject = infer_expression_type(module, None, expression)?;
            let key_type_pairs = match subject.type_value {
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
                            argument: expression.clone(),
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
                    expression: InferredExpression::RecordAccess {
                        expression: Box::new(subject.expression),
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
            let subject = infer_expression_type(module, expected_type, expression)?;

            match &subject.type_value {
                Type::Record { key_type_pairs } => {
                    let typechecked_updates = updates
                        .iter()
                        .map(|update| {
                            let actual_key = match &update {
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
                                        Ok(InferredRecordUpdate::ValueUpdate {
                                            property_name: PropertyName(property_name.clone()),
                                            new_value: typechecked_value.expression,
                                        })
                                    }
                                },
                            }
                        })
                        .collect::<Result<Vec<InferredRecordUpdate>, UnifyError>>()?;
                    Ok(InferExpressionResult {
                        type_value: subject.type_value,
                        expression: InferredExpression::RecordUpdate {
                            expression: Box::new(subject.expression),
                            updates: typechecked_updates,
                        },
                    })
                }
                other_type => Err(UnifyError {
                    position: expression.as_ref().position(),
                    kind: UnifyErrorKind::CannotPerformRecordUpdateOnNonRecord {
                        actual_type: other_type.clone(),
                    },
                }),
            }
        }

        Expression::Function(function) => module.run_in_new_child_scope(|module| {
            let expected_function_type = match &expected_type {
                Some(Type::Function(function_type)) => Some(function_type.clone()),
                Some(Type::TypeScheme(type_scheme)) => match &type_scheme.type_value {
                    Type::Function(function_type) => Some(function_type.clone()),
                    _ => None,
                },
                _ => None,
            };

            // Insert variable that are expected to be existent into the current scope
            let implicit_variables = match &expected_function_type {
                Some(expected_function_type)
                    if !expected_function_type.type_constraints.is_empty() =>
                {
                    Some(
                        expected_function_type
                            .type_constraints
                            .iter()
                            .map(|type_constraint| {
                                Ok((
                                    type_constraint.name.clone(),
                                    module.insert_value_symbol_with_type(
                                        &Token::dummy_identifier(type_constraint.name.clone()),
                                        Some(type_constraint.type_value.clone()),
                                        Access::Protected,
                                        true,
                                    )?,
                                ))
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                }
                _ => None,
            };
            let typechecked_function =
                infer_branched_function(module, &expected_function_type, function)?;

            let type_value = match &expected_type {
                Some(Type::TypeScheme(type_scheme)) => Type::TypeScheme(Box::new(TypeScheme {
                    type_variables: type_scheme.type_variables.clone(),
                    type_value: Type::Function(typechecked_function.function_type),
                })),
                _ => Type::Function(typechecked_function.function_type),
            };

            let function = InferredExpression::BranchedFunction(Box::new(
                typechecked_function.function.clone(),
            ));

            Ok(InferExpressionResult {
                type_value: type_value.clone(),
                expression: {
                    // If expected function type contains constraints
                    //  Create a new parameter for this lambda, which is a dictionary that holds all
                    //  the variables in the constraints
                    match implicit_variables {
                        None => function,
                        Some(implicit_variables) => {
                            let dictionary_parameter = InferredDestructurePattern {
                                // Use dummy type
                                type_value: Type::Unit,
                                kind: InferredDestructurePatternKind::Record {
                                    left_curly_bracket: Token::dummy(),
                                    key_pattern_pairs: implicit_variables
                                        .into_iter()
                                        .map(|(name, (symbol_uid, type_value))| {
                                            (
                                                PropertyName(Token::dummy_identifier(name.clone())),
                                                InferredDestructurePattern {
                                                    type_value,
                                                    kind:
                                                        InferredDestructurePatternKind::Identifier(
                                                            Box::new(Identifier {
                                                                uid: symbol_uid,
                                                                token: Token::dummy_identifier(
                                                                    name,
                                                                ),
                                                            }),
                                                        ),
                                                },
                                            )
                                        })
                                        .collect(),
                                    right_curly_bracket: Token::dummy(),
                                },
                            };
                            InferredExpression::BranchedFunction(Box::new(
                                InferredBranchedFunction {
                                    branches: Box::new(NonEmpty {
                                        head: InferredFunctionBranch {
                                            parameter: Box::new(dictionary_parameter),
                                            body: Box::new(InferExpressionResult {
                                                expression: function,
                                                type_value,
                                            }),
                                        },
                                        tail: vec![],
                                    }),
                                },
                            ))
                        }
                    }
                },
            })
        }),
        Expression::FunctionCall(function_call) => {
            infer_function_call(module, expected_type, function_call)
        }
        Expression::Record {
            key_value_pairs,
            hash_left_curly_bracket: left_curly_bracket,
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

            let record_position = left_curly_bracket
                .position
                .join(right_curly_bracket.position);

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
                _ => Ok(Type::ImplicitTypeVariable(ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                })),
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
                expression: InferredExpression::Array {
                    elements: typechecked_elements
                        .into_iter()
                        .map(|element| element.expression)
                        .collect(),
                },
            })
        }
        Expression::Parenthesized { value, .. } => {
            infer_expression_type(module, expected_type, value)
        }
        Expression::Let {
            left,
            right,
            type_annotation,
            body,
            ..
        } => module.run_in_new_child_scope(|module| {
            let expected_right_type = optional_type_annotation_to_type(module, type_annotation)?;
            let typechecked_right = infer_expression_type(module, expected_right_type, right)?;
            let typechecked_left = infer_destructure_pattern(
                module,
                Some(typechecked_right.type_value.clone()),
                left,
            )?;
            let typechecked_body = infer_expression_type(module, expected_type.clone(), body)?;

            match check_exhaustiveness(
                module,
                typechecked_left.type_value.clone(),
                NonEmpty {
                    head: typechecked_left.kind.clone(),
                    tail: vec![],
                },
                left.position(),
            ) {
                Ok(_) => {
                    let typechecked_statement = InferredStatement::Let {
                        access: Access::Protected,
                        left: typechecked_left,
                        right: typechecked_right.expression,
                    };
                    Ok(InferExpressionResult {
                        type_value: typechecked_body.type_value,
                        expression: InferredExpression::Block {
                            statements: vec![typechecked_statement],
                            return_value: Box::new(typechecked_body.expression),
                        },
                    })
                }
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
        }),
        Expression::Statements { current, next } => {
            let current = infer_expression_type(module, None, &current)?;
            let next = infer_expression_type(module, None, &next)?;
            Ok(InferExpressionResult {
                type_value: next.type_value,
                expression: InferredExpression::Block {
                    statements: vec![InferredStatement::Expression(current.expression)],
                    return_value: Box::new(next.expression),
                },
            })
        }
        Expression::CpsClosure { expression, .. } => {
            // Collect CpsBangs
            let (bangs, expression) = expression.clone().collect_cps_bangs(module);

            let expression = bangs.into_iter().fold(expression, |expression, bang| {
                Expression::FunctionCall(Box::new(FunctionCall {
                    function: Box::new(Expression::FunctionCall(Box::new(FunctionCall {
                        function: Box::new(bang.function),
                        argument: Box::new(bang.argument),
                        type_arguments: None,
                    }))),
                    argument: Box::new(Expression::Function(Box::new(Function {
                        left_curly_bracket: Token::dummy(),
                        branches: NonEmpty {
                            head: FunctionBranch {
                                parameter: Box::new(bang.temporary_variable),
                                body: Box::new(expression),
                            },
                            tail: vec![],
                        },
                        right_curly_bracket: Token::dummy(),
                    }))),
                    type_arguments: None,
                }))
            });
            infer_expression_type(module, expected_type, &expression)
        }
        Expression::CpsBang {
            argument,
            bang,
            function,
        } => panic!("Missing CPS closure"),
    }?;
    Ok(InferExpressionResult {
        type_value: module.apply_subtitution_to_type(&result.type_value),
        expression: result.expression,
    })
}

fn infer_function_call(
    module: &mut Module,
    expected_type: Option<Type>,
    function_call: &FunctionCall,
) -> Result<InferExpressionResult, UnifyError> {
    let typechecked_function = if let Expression::Function(_) = function_call.argument.as_ref() {
        // Get the actual function type
        // note that we use infer_expression_type_ instead of infer_expression_type
        // This is so that we can throw the error of `cannot invoke non-function`
        infer_expression_type_(module, None, function_call.function.as_ref())?
    } else {
        let typechecked_argument =
            infer_expression_type(module, None, function_call.argument.as_ref())?;

        // Get expected function type
        let expected_function_type = Type::Function(FunctionType {
            parameter_type: Box::new(typechecked_argument.type_value),
            return_type: match &expected_type {
                Some(expected_type) => Box::new(expected_type.clone()),
                None => Box::new(module.introduce_implicit_type_variable(None)?),
            },
            type_constraints: vec![],
        });

        // Get the actual function type
        // note that we use infer_expression_type_ instead of infer_expression_type
        // This is so that we can throw the error of `cannot invoke non-function`
        let typechecked_function = infer_expression_type_(
            module,
            Some(expected_function_type),
            function_call.function.as_ref(),
        )?;
        typechecked_function
    };

    // Check if expression being invoked is a function
    match typechecked_function.type_value {
        Type::Function(expected_function_type) => {
            // Unify argument
            let typechecked_argument = infer_expression_type(
                module,
                Some(*expected_function_type.parameter_type),
                function_call.argument.as_ref(),
            )?;

            let type_value =
                module.apply_subtitution_to_type(expected_function_type.return_type.as_ref());

            if !expected_function_type.type_constraints.is_empty() {
                // Search for matching variables
                let matching_variables = expected_function_type
                    .type_constraints
                    .iter()
                    .map(|type_constraint| {
                        let symbol = match module.get_value_symbol(
                            &Token::dummy_identifier(type_constraint.name.clone()),
                            &Some(type_constraint.type_value.clone()),
                            module.current_scope_name(),
                        ) {
                            Ok(result) => Ok(result),
                            Err(_) => Err(UnifyError {
                                position: function_call.function.position(),
                                kind: UnifyErrorKind::UnsatisfiedConstraint {
                                    missing_constraint: TypeConstraint {
                                        name: type_constraint.name.clone(),
                                        type_value: module
                                            .apply_subtitution_to_type(&type_constraint.type_value),
                                    },
                                },
                            }),
                        }?;
                        Ok((type_constraint.name.clone(), symbol.symbol_uid))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                // Put the matching variables in a dictionary
                // And make the dictionary as an extra first argument to the existing function
                // call

                let dictionary = InferredExpression::Record {
                    key_value_pairs: matching_variables
                        .into_iter()
                        .map(|(name, symbol_uid)| {
                            (
                                PropertyName(Token::dummy_identifier(name.clone())),
                                InferredExpression::Variable(Identifier {
                                    uid: symbol_uid,
                                    token: Token::dummy_identifier(name),
                                }),
                            )
                        })
                        .collect(),
                };
                Ok(InferExpressionResult {
                    type_value,
                    expression: InferredExpression::FunctionCall(Box::new(InferredFunctionCall {
                        function: Box::new(InferredExpression::FunctionCall(Box::new(
                            InferredFunctionCall {
                                function: Box::new(typechecked_function.expression),
                                argument: Box::new(dictionary),
                            },
                        ))),
                        argument: Box::new(typechecked_argument.expression),
                    })),
                })
            } else {
                Ok(InferExpressionResult {
                    type_value,
                    expression: InferredExpression::FunctionCall(Box::new(InferredFunctionCall {
                        function: Box::new(typechecked_function.expression),
                        argument: Box::new(typechecked_argument.expression),
                    })),
                })
            }
        }
        Type::ImplicitTypeVariable(type_variable) => {
            let expected_function_type = Type::ImplicitTypeVariable(ImplicitTypeVariable {
                name: module.get_next_type_variable_name(),
            });

            let position = Expression::FunctionCall(Box::new(function_call.clone())).position();

            unify_type(
                module,
                &Type::ImplicitTypeVariable(type_variable),
                &expected_function_type,
                position,
            )?;

            let return_type = Type::ImplicitTypeVariable(ImplicitTypeVariable {
                name: module.get_next_type_variable_name(),
            });

            let typechecked_argument =
                infer_expression_type(module, None, function_call.argument.as_ref())?;

            let actual_function_type = Type::Function(FunctionType {
                parameter_type: Box::new(typechecked_argument.type_value),
                return_type: Box::new(return_type.clone()),
                type_constraints: vec![],
            });

            unify_type(
                module,
                &actual_function_type,
                &expected_function_type,
                position,
            )?;

            Ok(InferExpressionResult {
                type_value: module.apply_subtitution_to_type(&return_type),
                expression: InferredExpression::FunctionCall(Box::new(InferredFunctionCall {
                    function: Box::new(typechecked_function.expression),
                    argument: Box::new(typechecked_argument.expression),
                })),
            })
        }
        other => Err(UnifyError {
            position: function_call.function.position(),
            kind: UnifyErrorKind::CannotInvokeNonFunction { actual_type: other },
        }),
    }
}

impl Expression {
    /// Collect CPS bangs, and transformed those bangs into temporary variables
    fn collect_cps_bangs(self, module: &mut Module) -> (Vec<CollectCpsBangResult>, Expression) {
        match self {
            Expression::Unit { .. }
            | Expression::Float(_)
            | Expression::Integer(_)
            | Expression::String(_)
            | Expression::Character(_)
            | Expression::Identifier(_)
            | Expression::Keyword(_)
            | Expression::CpsClosure { .. } => (vec![], self),

            Expression::Statements { current, next } => {
                let (bangs1, current) = current.collect_cps_bangs(module);
                let (bangs2, next) = next.collect_cps_bangs(module);
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
                let (bangs, value) = value.collect_cps_bangs(module);
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
                        let (bangs, expression) = expression.collect_cps_bangs(module);
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
                    let (bangs, payload) = payload.collect_cps_bangs(module);
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
            Expression::Function(lambda) => {
                todo!()
            }
            Expression::FunctionCall(function_call) => {
                let (bangs1, function) = function_call.function.collect_cps_bangs(module);
                let (bangs2, argument) = function_call.argument.collect_cps_bangs(module);
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
                hash_left_curly_bracket: left_square_bracket,
                key_value_pairs,
                right_curly_bracket: right_square_bracket,
            } => {
                let (bangs, key_value_pairs): (
                    Vec<Vec<CollectCpsBangResult>>,
                    Vec<RecordKeyValue>,
                ) = key_value_pairs
                    .into_iter()
                    .map(|key_value_pair| {
                        let (bangs, value) = key_value_pair.value.collect_cps_bangs(module);
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
                        hash_left_curly_bracket: left_square_bracket,
                        key_value_pairs,
                        right_curly_bracket: right_square_bracket,
                    },
                )
            }
            Expression::RecordAccess {
                expression,
                property_name,
            } => {
                let (bangs, expression) = expression.collect_cps_bangs(module);
                (
                    bangs,
                    Expression::RecordAccess {
                        expression: Box::new(expression),
                        property_name,
                    },
                )
            }
            Expression::RecordUpdate {
                expression,
                hash_left_curly_bracket: left_curly_bracket,
                updates,
                right_curly_bracket,
            } => todo!(),
            Expression::Array {
                hash_left_parenthesis: left_square_bracket,
                elements,
                right_parenthesis: right_square_bracket,
            } => todo!(),
            Expression::Let {
                keyword_let,
                left,
                right,
                type_annotation,
                body,
            } => todo!(),
            Expression::CpsBang {
                argument,
                bang,
                function,
            } => {
                let temporary_variable = Token {
                    position: function.position().join(argument.position()),
                    token_type: TokenType::Identifier,
                    representation: format!("temp{}", module.get_next_symbol_uid().index),
                };
                (
                    vec![CollectCpsBangResult {
                        temporary_variable: DestructurePattern::Identifier(
                            temporary_variable.clone(),
                        ),
                        function: function.as_ref().clone(),
                        argument: argument.as_ref().clone(),
                    }],
                    Expression::Identifier(temporary_variable),
                )
            }
        }
    }
}

struct CollectCpsBangResult {
    temporary_variable: DestructurePattern,
    function: Expression,
    argument: Expression,
}

impl Function {
    fn position(&self) -> Position {
        self.left_curly_bracket
            .position
            .join(self.right_curly_bracket.position)
    }
}

impl Positionable for EnumConstructorDefinition {
    fn position(&self) -> Position {
        match &self.payload {
            None => self.name.position.clone(),
            Some(payload) => self.name.position.join(payload.position()),
        }
    }
}

impl Positionable for EnumConstructorDefinitionPayload {
    fn position(&self) -> Position {
        self.type_annotation.position()
    }
}

impl Positionable for InferredDestructurePattern {
    fn position(&self) -> Position {
        self.kind.position()
    }
}

fn infer_block_level_statements(
    module: &mut Module,
    expected_final_type: Option<Type>,
    statements: Vec<Statement>,
) -> Result<Vec<(InferredStatement, Type)>, UnifyError> {
    match statements.split_first() {
        None => Ok(vec![]),
        Some((head, tail)) => module.run_in_new_child_scope(|module| {
            // We only pass in the expected type for the last statement
            // As the last statement will be returned as expression
            let expected_type = if tail.is_empty() {
                expected_final_type.clone()
            } else {
                Some(Type::Unit)
            };
            let mut result = vec![infer_block_level_statement(module, expected_type, head)?];
            let tail =
                infer_block_level_statements(module, expected_final_type.clone(), tail.to_vec())?;
            result.extend(tail);
            Ok(result)
        }),
    }
}

fn infer_let_statement(
    module: &mut Module,
    LetStatement {
        type_annotation,
        name,
        expression,
        access,
        ..
    }: &LetStatement,
) -> Result<InferredStatement, UnifyError> {
    let type_annotation_type = type_annotation_to_type(module, type_annotation)?;
    let typechecked_right =
        { infer_expression_type(module, Some(type_annotation_type.clone()), expression)? };

    let typechecked_left = infer_destructure_pattern(
        module,
        Some(type_annotation_type),
        &DestructurePattern::Identifier(name.clone()),
    )?;

    Ok(InferredStatement::Let {
        access: access.clone(),
        left: typechecked_left,
        right: typechecked_right.expression,
    })
}

/// Returns the typechecked statement, the type value of the statement, and the accumulated constraints
fn infer_block_level_statement(
    module: &mut Module,
    expected_type: Option<Type>,
    statement: &Statement,
) -> Result<(InferredStatement, Type), UnifyError> {
    let result = match statement {
        Statement::Let(let_statement) => {
            Ok((infer_let_statement(module, let_statement)?, Type::Unit))
        }
        _ => {
            // TODO: separate Statement enum as TopLevelStatement and BlockLevelStatement
            panic!()
        }
    }?;
    Ok(result)
}

fn infer_record_type(
    module: &mut Module,
    expected_key_type_pairs: Option<Vec<(String, Type)>>,
    actual_key_value_pairs: Vec<RecordKeyValue>,
    record_position: Position,
) -> Result<InferExpressionResult, UnifyError> {
    let zipped = match expected_key_type_pairs {
        Some(expected_key_type_pairs) => match match_key_values_pairs(
            &expected_key_type_pairs,
            &actual_key_value_pairs,
            |(expected_key, _)| expected_key,
            |actual_key_value| &actual_key_value.key,
            |expected, actual| *expected == actual.representation,
        ) {
            Ok(zipped) => Ok(zipped
                .into_iter()
                .map(|(expected, actual)| (Some(expected), actual))
                .collect::<Vec<(Option<(String, Type)>, RecordKeyValue)>>()),
            Err(error) => match error {
                MatchKeyValueError::MissingKeys(missing_keys) => Err(UnifyError {
                    position: record_position,
                    kind: UnifyErrorKind::RecordMissingKeys { missing_keys },
                }),
                MatchKeyValueError::ExtraneousKey(extraenous_keys) => Err(UnifyError {
                    position: extraenous_keys.head.position,
                    kind: UnifyErrorKind::RecordExtraneousKey {
                        expected_keys: expected_key_type_pairs
                            .iter()
                            .map(|(key, _)| key.clone())
                            .collect(),
                    },
                }),
            },
        }?,

        None => iter::repeat(None)
            .take(actual_key_value_pairs.len())
            .collect::<Vec<_>>()
            .into_iter()
            .zip(actual_key_value_pairs)
            .collect(),
    };

    let typechecked_key_value_pairs = zipped
        .into_iter()
        .map(|(expected_key_type_pair, RecordKeyValue { key, value })| {
            let expected_type = expected_key_type_pair.map(|(_, type_value)| type_value);
            let value_type = infer_expression_type(module, expected_type, &value)?;
            Ok((key, value_type))
        })
        .collect::<Result<Vec<(Token, InferExpressionResult)>, UnifyError>>()?;

    Ok(InferExpressionResult {
        type_value: Type::Record {
            key_type_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (key.representation.clone(), result.type_value.clone()))
                .collect(),
        },
        expression: InferredExpression::Record {
            key_value_pairs: typechecked_key_value_pairs
                .iter()
                .map(|(key, result)| (PropertyName(key.clone()), result.expression.clone()))
                .collect(),
        },
    })
}

struct InferFunctionResult {
    function_type: FunctionType,
    function: InferredBranchedFunction,
}
fn infer_branched_function(
    module: &mut Module,
    expected_function_type: &Option<FunctionType>,
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
                &Some(typechecked_first_branch.function_type.clone()),
                &function_branch,
            )
        })
        .collect::<Result<Vec<InferFunctionBranchResult>, UnifyError>>()?;

    let function_type =
        module.apply_subtitution_to_function_type(&typechecked_first_branch.function_type);

    // Check for case exhaustiveness
    let expected_type = *function_type.clone().parameter_type;
    let actual_patterns = NonEmpty {
        head: typechecked_first_branch
            .function_branch
            .parameter
            .kind
            .clone(),
        tail: typechecked_rest_branches
            .iter()
            .map(|branch| branch.function_branch.parameter.kind.clone())
            .collect(),
    };

    check_exhaustiveness(
        module,
        expected_type,
        actual_patterns,
        Expression::Function(Box::new(function.clone())).position(),
    )?;

    Ok(InferFunctionResult {
        function_type,
        function: InferredBranchedFunction {
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

pub fn check_exhaustiveness(
    module: &Module,
    expected_type: Type,
    actual_patterns: NonEmpty<InferredDestructurePatternKind>,
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
    actual_patterns: NonEmpty<InferredDestructurePatternKind>,
    position: Position,
) -> Result<(), UnifyError> {
    let remaining_expected_patterns = actual_patterns
        .into_vector()
        .iter()
        .flat_map(|pattern| to_checkable_pattern(pattern, false))
        .fold(
            Ok(expected_patterns),
            |expected_patterns, actual_pattern| match expected_patterns {
                Err(error) => Err(error),
                Ok(expected_patterns) => {
                    // println!("actual_pattern = {:?}", actual_pattern);
                    // println!("expected_patterns = {:?}", expected_patterns);

                    let new_expanded_patterns =
                        match_patterns(module, &actual_pattern.kind, expected_patterns.clone());

                    if
                    // If there are still remaning actual_patterns but expected_patterns already exhausted
                    // Then this actual_pattern is an unreachable case
                    expected_patterns.is_empty()

                    ||
                    // If new_patterns equals to the current expected_patterns
                    // Then this actual_pattern is also an unreachable case
                    //  as it does not remove any patterns from expected_patterns
                    new_expanded_patterns.eq(&expected_patterns)
                    {
                        Err(UnifyError {
                            position: actual_pattern.kind.position(),
                            kind: if actual_pattern.is_result_of_expansion {
                                UnifyErrorKind::PartiallyUnreachableCase {
                                    redundant_expanded_pattern: actual_pattern.kind,
                                }
                            } else {
                                UnifyErrorKind::UnreachableCase
                            },
                        })
                    } else {
                        Ok(new_expanded_patterns)
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

/// This function will expand the OR pattern of InferredDestructurePatternKind
/// into a list of CheckablePattern (which does not contains OR pattern)
pub fn to_checkable_pattern(
    pattern: &InferredDestructurePatternKind,
    is_result_of_expansion: bool,
) -> Vec<CheckablePattern> {
    match pattern {
        InferredDestructurePatternKind::EnumConstructor {
            constructor_name,
            payload,
        } => match &payload {
            Some(payload) => to_checkable_pattern(&payload.kind, is_result_of_expansion)
                .into_iter()
                .map(|pattern| CheckablePattern {
                    is_result_of_expansion: pattern.is_result_of_expansion,
                    kind: CheckablePatternKind::EnumConstructor {
                        constructor_name: constructor_name.clone(),
                        payload: Some(Box::new(pattern)),
                    },
                })
                .collect(),
            None => {
                vec![CheckablePattern {
                    is_result_of_expansion,
                    kind: CheckablePatternKind::EnumConstructor {
                        constructor_name: constructor_name.clone(),
                        payload: None,
                    },
                }]
            }
        },
        InferredDestructurePatternKind::Record {
            left_curly_bracket,
            key_pattern_pairs,
            right_curly_bracket,
        } => {
            let expanded_key_pattern_pairs = key_pattern_pairs
                .iter()
                .map(|(property, pattern)| {
                    to_checkable_pattern(&pattern.kind, is_result_of_expansion)
                        .into_iter()
                        .map(|checkable_pattern| (property.clone(), checkable_pattern))
                        .collect::<Vec<(PropertyName, CheckablePattern)>>()
                })
                .collect::<Vec<Vec<(PropertyName, CheckablePattern)>>>();

            cartesian_product(expanded_key_pattern_pairs)
                .into_iter()
                .map(|key_pattern_pairs| CheckablePattern {
                    is_result_of_expansion: key_pattern_pairs
                        .iter()
                        .any(|(_, pattern)| pattern.is_result_of_expansion),
                    kind: CheckablePatternKind::Record {
                        left_curly_bracket: left_curly_bracket.clone(),
                        right_curly_bracket: right_curly_bracket.clone(),
                        key_pattern_pairs,
                    },
                })
                .collect()
        }

        InferredDestructurePatternKind::Tuple { patterns } => {
            let expanded_patterns = patterns
                .clone()
                .into_vector()
                .into_iter()
                .enumerate()
                .map(|(index, pattern)| {
                    to_checkable_pattern(&pattern.kind, is_result_of_expansion)
                        .into_iter()
                        .map(|checkable_pattern| (index, checkable_pattern))
                        .collect::<Vec<(usize, CheckablePattern)>>()
                })
                .collect::<Vec<Vec<(usize, CheckablePattern)>>>();

            cartesian_product(expanded_patterns)
                .into_iter()
                .map(|index_pattern_pairs| CheckablePattern {
                    is_result_of_expansion: index_pattern_pairs
                        .iter()
                        .any(|(_, pattern)| pattern.is_result_of_expansion),
                    kind: CheckablePatternKind::Tuple {
                        patterns: {
                            let patterns = index_pattern_pairs
                                .iter()
                                .map(|(_, pattern)| pattern.kind.clone())
                                .collect::<Vec<CheckablePatternKind>>();

                            let (head, tail) = patterns.split_first().unwrap();

                            Box::new(NonEmpty {
                                head: head.clone(),
                                tail: tail.to_vec(),
                            })
                        },
                    },
                })
                .collect()
        }
        InferredDestructurePatternKind::Array { .. } => {
            panic!()
        }
        InferredDestructurePatternKind::Or { patterns } => patterns
            .clone()
            .into_vector()
            .into_iter()
            .flat_map(|pattern| to_checkable_pattern(&pattern.kind, true))
            .collect(),
        InferredDestructurePatternKind::Infinite { kind, token } => {
            vec![CheckablePattern {
                is_result_of_expansion,
                kind: CheckablePatternKind::Infinite {
                    kind: kind.clone(),
                    token: token.clone(),
                },
            }]
        }
        InferredDestructurePatternKind::Boolean { token, value } => {
            vec![CheckablePattern {
                is_result_of_expansion,
                kind: CheckablePatternKind::Boolean {
                    token: token.clone(),
                    value: *value,
                },
            }]
        }
        InferredDestructurePatternKind::Unit {
            left_parenthesis,
            right_parenthesis,
        } => {
            vec![CheckablePattern {
                is_result_of_expansion,
                kind: CheckablePatternKind::Unit {
                    left_parenthesis: left_parenthesis.clone(),
                    right_parenthesis: right_parenthesis.clone(),
                },
            }]
        }
        InferredDestructurePatternKind::Underscore(token) => {
            vec![CheckablePattern {
                is_result_of_expansion,
                kind: CheckablePatternKind::Underscore(token.clone()),
            }]
        }
        InferredDestructurePatternKind::Identifier(identifier) => {
            vec![CheckablePattern {
                is_result_of_expansion,
                kind: CheckablePatternKind::Identifier(identifier.clone()),
            }]
        }
    }
}

pub fn unify_function_type(
    module: &mut Module,
    expected_function_type: &FunctionType,
    actual_function_type: &FunctionType,
    position: Position,
) -> Result<FunctionType, UnifyError> {
    // unify parameter type
    let unify_parameter_type_result = unify_type(
        module,
        &expected_function_type.parameter_type,
        &actual_function_type.parameter_type,
        position,
    );

    match unify_parameter_type_result {
        Ok(parameter_type) => {
            // unify return type
            match unify_type(
                module,
                expected_function_type.return_type.as_ref(),
                actual_function_type.return_type.as_ref(),
                position,
            ) {
                Ok(return_type) => {
                    // TODO: unify the type_constraints of expected_function_type and
                    // actual_function_type
                    Ok(FunctionType {
                        parameter_type: Box::new(parameter_type),
                        return_type: Box::new(return_type),
                        type_constraints: expected_function_type.type_constraints.clone(),
                    })
                }
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
    function_branch: InferredFunctionBranch,
    function_type: FunctionType,
}
fn infer_function_branch(
    module: &mut Module,
    expected_function_type: &Option<FunctionType>,
    function_branch: &FunctionBranch,
) -> Result<InferFunctionBranchResult, UnifyError> {
    module.run_in_new_child_scope(|module| {
        let typechecked_parameter = infer_destructure_pattern(
            module,
            expected_function_type
                .clone()
                .map(|function_type| *function_type.parameter_type),
            &function_branch.parameter,
        )?;

        let expected_return_type = expected_function_type
            .clone()
            .map(|expected_function_type| *expected_function_type.return_type);

        let typechecked_body =
            infer_expression_type(module, expected_return_type, &function_branch.body)?;

        let result_type = FunctionType {
            parameter_type: Box::new(typechecked_parameter.type_value.clone()),
            return_type: Box::new(module.apply_subtitution_to_type(&typechecked_body.type_value)),
            type_constraints: match expected_function_type {
                Some(function_type) => function_type.type_constraints.clone(),
                None => vec![],
            },
        };

        // Check for unused variables
        module.check_for_unused_symbols(module.current_scope_name())?;

        Ok(InferFunctionBranchResult {
            function_type: module.apply_subtitution_to_function_type(&result_type),
            function_branch: InferredFunctionBranch {
                parameter: Box::new(typechecked_parameter),
                body: Box::new(typechecked_body),
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
                type_annotation.position(),
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
        TypeAnnotation::Unit { .. } => Ok(Type::Unit),
        TypeAnnotation::Keyword { identifier } => {
            // Insert a value with the same type into the current module
            module.insert_keyword(identifier);

            Ok(Type::Keyword(identifier.representation.clone()))
        }
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
                            != actual_type_arguments.type_annotations.len()
                        {
                            Err(UnifyError {
                                position: actual_type_arguments
                                    .left_angular_bracket
                                    .position
                                    .join(actual_type_arguments.right_angular_bracket.position),
                                kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                                    actual_length: actual_type_arguments.type_annotations.len(),
                                    expected_type_parameter_names: expected_type_scheme
                                        .type_variables
                                        .map(|type_variable| type_variable.name)
                                        .into_vector(),
                                },
                            })
                        } else {
                            let type_arguments = actual_type_arguments
                                .type_annotations
                                .clone()
                                .into_vector()
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
                                    |result, (expected_type_variable, type_value)| match result {
                                        Err(error) => Err(error),
                                        Ok(result) => Ok(rewrite_type_variable_in_type(
                                            &expected_type_variable.name,
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
                                .map(|type_variable| type_variable.name)
                                .into_vector(),
                        },
                    }),
                    (_, Some(actual_type_arguments)) => Err(UnifyError {
                        position: actual_type_arguments
                            .left_angular_bracket
                            .position
                            .join(actual_type_arguments.right_angular_bracket.position),
                        kind: UnifyErrorKind::TypeArgumentsLengthMismatch {
                            actual_length: actual_type_arguments.type_annotations.len(),
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
        TypeAnnotation::Function(FunctionTypeAnnotation {
            parameter,
            return_type,
            type_constraints,
        }) => Ok(Type::Function(FunctionType {
            parameter_type: Box::new(type_annotation_to_type(module, &parameter)?),
            return_type: Box::new(type_annotation_to_type(module, return_type)?),
            type_constraints: match type_constraints {
                None => vec![],
                Some(type_constraints) => type_constraints
                    .type_constraints
                    .iter()
                    .map(|type_constraint| {
                        Ok(TypeConstraint {
                            name: type_constraint.identifier.representation.clone(),
                            type_value: type_annotation_to_type(
                                module,
                                &type_constraint.type_annotation,
                            )?,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            },
        })),
        TypeAnnotation::Scheme {
            type_variables,
            type_annotation,
        } => Ok(Type::TypeScheme(Box::new(TypeScheme {
            type_variables: type_variables.type_variables.clone().map(|type_variable| {
                ExplicitTypeVariable {
                    name: type_variable.representation,
                }
            }),
            type_value: module.run_in_new_child_scope(|module| {
                populate_explicit_type_variables(module, &Some(type_variables.clone()))?;

                type_annotation_to_type(module, type_annotation)
            })?,
        }))),
        TypeAnnotation::Parenthesized {
            type_annotation, ..
        } => type_annotation_to_type(module, type_annotation),
    }
}

/// This function is not pure, it will mutate the `module`, namely
/// when we encounter a variable binding (e.g. `x`), then `x` will be put in the environment of `module`
fn infer_destructure_pattern(
    module: &mut Module,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
) -> Result<InferredDestructurePattern, UnifyError> {
    // Same story as infer_expression_type
    let result = infer_destructure_pattern_(module, expected_type.clone(), destructure_pattern)?;

    let type_value = try_unify_type(
        module,
        expected_type,
        &result.type_value,
        destructure_pattern.position(),
    )?;

    Ok(InferredDestructurePattern {
        kind: result.kind,
        type_value,
    })
}

fn infer_destructure_pattern_(
    module: &mut Module,
    expected_type: Option<Type>,
    destructure_pattern: &DestructurePattern,
) -> Result<InferredDestructurePattern, UnifyError> {
    match destructure_pattern {
        DestructurePattern::Infinite { token, kind } => Ok(InferredDestructurePattern {
            type_value: match &kind {
                InfinitePatternKind::Character => Type::Character,
                InfinitePatternKind::String => Type::String,
                InfinitePatternKind::Integer => Type::Integer,
            },
            kind: InferredDestructurePatternKind::Infinite {
                kind: kind.clone(),
                token: token.clone(),
            },
        }),
        DestructurePattern::Unit {
            left_parenthesis,
            right_parenthesis,
        } => Ok(InferredDestructurePattern {
            type_value: Type::Unit,
            kind: InferredDestructurePatternKind::Unit {
                left_parenthesis: left_parenthesis.clone(),
                right_parenthesis: right_parenthesis.clone(),
            },
        }),
        DestructurePattern::Underscore(token) => Ok(InferredDestructurePattern {
            type_value: module.introduce_implicit_type_variable(None)?,
            kind: InferredDestructurePatternKind::Underscore(token.clone()),
        }),
        DestructurePattern::Identifier(identifier) => {
            match get_enum_type(module, expected_type.clone(), identifier) {
                Ok(result) => {
                    // Treat this as an enum constructor
                    Ok(InferredDestructurePattern {
                        type_value: match &expected_type {
                            Some(expected_type) => unify_type(
                                module,
                                &expected_type,
                                &result.expected_enum_type,
                                identifier.position,
                            )?,
                            None => result.expected_enum_type,
                        },
                        kind: InferredDestructurePatternKind::EnumConstructor {
                            constructor_name: identifier.clone(),
                            payload: None,
                        },
                    })
                }
                Err(_) => {
                    // Treat this identifier as a normal variable
                    let (uid, type_value) = module.insert_value_symbol_with_type(
                        identifier,
                        expected_type,
                        Access::Protected,
                        false,
                    )?;
                    Ok(InferredDestructurePattern {
                        type_value,
                        kind: InferredDestructurePatternKind::Identifier(Box::new(Identifier {
                            uid,
                            token: identifier.clone(),
                        })),
                    })
                }
            }
        }
        DestructurePattern::Tuple(tuple) => {
            let typechecked_values = tuple.values.clone().fold_result(|destructure_pattern| {
                // TODO: pass in expected_type
                infer_destructure_pattern(module, None, &destructure_pattern)
            })?;
            Ok(InferredDestructurePattern {
                type_value: Type::Tuple(Box::new(
                    typechecked_values.clone().map(|value| value.type_value),
                )),
                kind: InferredDestructurePatternKind::Tuple {
                    patterns: Box::new(typechecked_values),
                },
            })
        }
        DestructurePattern::EnumConstructor { name, payload, .. } => {
            let result = get_enum_type(module, expected_type, name)?;
            match (result.expected_payload_type, payload.clone()) {
                (None, None) => Ok(InferredDestructurePattern {
                    type_value: result.expected_enum_type,
                    kind: InferredDestructurePatternKind::EnumConstructor {
                        constructor_name: name.clone(),
                        payload: None,
                    },
                }),
                (None, Some(payload)) => Err(UnifyError {
                    position: payload.position(),
                    kind: UnifyErrorKind::ThisEnumConstructorDoesNotRequirePayload,
                }),
                (Some(expected_payload_type), None) => Err(UnifyError {
                    position: name.position,
                    kind: UnifyErrorKind::ThisEnumConstructorRequiresPaylod {
                        payload_type: expected_payload_type,
                    },
                }),
                (Some(expected_payload_type), Some(payload)) => {
                    let typechecked_payload =
                        infer_destructure_pattern(module, Some(expected_payload_type), &payload)?;
                    Ok(InferredDestructurePattern {
                        type_value: result.expected_enum_type,
                        kind: InferredDestructurePatternKind::EnumConstructor {
                            constructor_name: name.clone(),
                            payload: Some(Box::new(typechecked_payload)),
                        },
                    })
                }
            }
        }
        DestructurePattern::Array {
            spread: None,
            left_square_bracket,
            right_square_bracket,
        } => Ok(InferredDestructurePattern {
            type_value: Type::BuiltInOneArgumentType {
                kind: BuiltInOneArgumentTypeKind::Array,
                type_argument: Box::new(Type::ImplicitTypeVariable(ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                })),
            },
            kind: InferredDestructurePatternKind::Array {
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
                _ => Type::ImplicitTypeVariable(ImplicitTypeVariable {
                    name: module.get_next_type_variable_name(),
                }),
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

            Ok(InferredDestructurePattern {
                type_value: module.apply_subtitution_to_type(&expected_array_type),
                kind: InferredDestructurePatternKind::Array {
                    left_square_bracket: left_square_bracket.clone(),
                    right_square_bracket: right_square_bracket.clone(),
                    spread: Some(InferredDesturcturePatternArraySpread {
                        first_element: Box::new(typechecked_first_element),
                        rest_elements: Box::new(typechecked_rest_elements),
                    }),
                },
            })
        }
        DestructurePattern::Record {
            wildcard,
            key_value_pairs,
            hash_left_curly_bracket: left_curly_bracket,
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
                            )?;

                            Ok((actual_key, typechecked_destructure_pattern))
                        }
                        None => {
                            let (uid, type_value) = module.insert_value_symbol_with_type(
                                key,
                                expected_type,
                                Access::Protected,
                                false,
                            )?;
                            Ok((
                                actual_key.clone(),
                                InferredDestructurePattern {
                                    type_value,
                                    kind: InferredDestructurePatternKind::Identifier(Box::new(
                                        Identifier {
                                            uid,
                                            token: actual_key,
                                        },
                                    )),
                                },
                            ))
                        }
                    }
                })
                .collect::<Result<Vec<(Token, InferredDestructurePattern)>, UnifyError>>()?;

            Ok(InferredDestructurePattern {
                type_value: Type::Record {
                    key_type_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| {
                            (key.representation.clone(), pattern.type_value.clone())
                        })
                        .collect(),
                },
                kind: InferredDestructurePatternKind::Record {
                    left_curly_bracket: left_curly_bracket.clone(),
                    right_curly_bracket: right_curly_bracket.clone(),
                    key_pattern_pairs: typechecked_key_pattern_pairs
                        .iter()
                        .map(|(key, pattern)| (PropertyName(key.clone()), pattern.clone()))
                        .collect(),
                },
            })
        }
        DestructurePattern::Or(patterns) => {
            // 1. Check that all the patterns has the same type
            let first_pattern =
                infer_destructure_pattern(module, expected_type.clone(), &patterns.first())?;

            let tail_patterns = patterns
                .tail()
                .iter()
                .map(|pattern| {
                    // We have to infer each trailing pattern in different scope
                    // So that we will not get the duplicated binding errors in patterns like:
                    //  Pa(x) | Pb(x)
                    module.run_in_new_child_scope(|module| {
                        infer_destructure_pattern(
                            module,
                            // Note the each trailing pattern should has the type of the first pattern
                            Some(first_pattern.type_value.clone()),
                            &pattern,
                        )
                    })
                })
                .collect::<Result<Vec<InferredDestructurePattern>, UnifyError>>()?;

            // 2. Check that all the patterns has the equivalent bindings.

            // 3. Update the SymbolUid of each bindings in tail_patterns to follow that of first_pattern.
            // This is necessary because we infer the type of each pattern in different scope,
            // which will cause them to have different SymbolUid even though they have the same name.
            // For example, when we infer the pattern `Pa(x) | Pb(x)`
            // The `x` in `Pa(x)` and the `x` in `Pb(x)` will have different SymbolUid.
            // For the transpilation to work, we have to update the `x` in `Pb(x)` to use the same SymbolUid
            // as the `x` in `Pa(x)`

            // Note that 2. and 3. are done together in the following code using the `match_bindings` function.
            let first_bindings = first_pattern.bindings();
            let tail_patterns = tail_patterns
                .into_iter()
                .map(|pattern| {
                    let result = match_bindings(module, pattern.clone(), first_bindings.clone())?;
                    match result.remaining_expected_bindings.split_first() {
                        None => Ok(result.pattern),
                        Some((head, tail)) => Err(UnifyError {
                            position: pattern.position(),
                            kind: UnifyErrorKind::MissingBindings {
                                missing_expected_bindings: NonEmpty {
                                    head: head.clone(),
                                    tail: tail.to_vec(),
                                },
                            },
                        }),
                    }
                })
                .collect::<Result<Vec<InferredDestructurePattern>, UnifyError>>()?;

            Ok(InferredDestructurePattern {
                type_value: first_pattern.type_value.clone(),
                kind: InferredDestructurePatternKind::Or {
                    patterns: Box::new(NonEmpty {
                        head: first_pattern,
                        tail: tail_patterns,
                    }),
                },
            })
        }
        DestructurePattern::Parenthesized { pattern, .. } => {
            infer_destructure_pattern(module, expected_type, pattern)
        }
    }
}

struct MatchingBindingResult {
    pattern: InferredDestructurePattern,
    /// Bindings that were not matched at all
    remaining_expected_bindings: Vec<Binding>,
}

/// This function ensure that the `source` has **equivalent** bindings as `expected_bindings`.  
///
/// Namely,
/// 1. `source` has the same number of bindings as `expected_bindings`
/// 2. Each binding of `source` exists in `expected_bindings` (this is done by returning a non-empty remaining_expected_bindings)
/// 3. Each matching bindings between `source` and `expected_bindings` must have unifiable type
///
/// This function also update the SymbolUid of each bindings of `source` to match the
/// SymbolUid of each bindings of `expected_bindings`.  
///
/// Suppose in `A@n`, `A` means the identifier name, and `n` means its corresponding SymbolUid.  
///
/// For example, if `source` is `A@0` and `expected_bindings` is `A@1`,  
/// Then the result should be `A@1`.
fn match_bindings(
    module: &mut Module,
    source: InferredDestructurePattern,
    expected_bindings: Vec<Binding>,
) -> Result<MatchingBindingResult, UnifyError> {
    match source.kind {
        kind @ InferredDestructurePatternKind::Infinite { .. } => Ok(MatchingBindingResult {
            remaining_expected_bindings: expected_bindings,
            pattern: InferredDestructurePattern { kind, ..source },
        }),
        kind @ InferredDestructurePatternKind::Boolean { .. } => Ok(MatchingBindingResult {
            remaining_expected_bindings: expected_bindings,
            pattern: InferredDestructurePattern { kind, ..source },
        }),
        kind @ InferredDestructurePatternKind::Unit { .. } => Ok(MatchingBindingResult {
            remaining_expected_bindings: expected_bindings,
            pattern: InferredDestructurePattern { kind, ..source },
        }),
        kind @ InferredDestructurePatternKind::Underscore(_) => Ok(MatchingBindingResult {
            remaining_expected_bindings: expected_bindings,
            pattern: InferredDestructurePattern { kind, ..source },
        }),
        InferredDestructurePatternKind::Identifier(identifier) => {
            // Look for matching bindings
            let (matching_bindings, remaining_expected_bindings): (Vec<Binding>, Vec<Binding>) =
                expected_bindings.into_iter().partition(|binding| {
                    identifier.token.representation == binding.identifier.token.representation
                });
            match matching_bindings.first() {
                // If no matching binding found, it means that this identifier is an extraneous binding in the OR patterns
                None => Err(UnifyError {
                    position: identifier.token.position,
                    kind: UnifyErrorKind::ExtraneousBinding {
                        extraneous_binding: Binding {
                            type_value: source.type_value,
                            identifier: *identifier.clone(),
                        },
                        expected_bindings: remaining_expected_bindings,
                    },
                }),
                Some(matching_binding) => {
                    // If matching binding found, then unify the type of this identifier and that of matching_binding
                    unify_type(
                        module,
                        &matching_binding.type_value,
                        &source.type_value,
                        identifier.token.position,
                    )?;

                    Ok(MatchingBindingResult {
                        remaining_expected_bindings,
                        pattern: InferredDestructurePattern {
                            type_value: matching_binding.type_value.clone(),
                            kind: InferredDestructurePatternKind::Identifier(Box::new(
                                Identifier {
                                    // Note that we use the SymbolUid of matching_binding
                                    uid: matching_binding.identifier.uid.clone(),
                                    token: identifier.token,
                                },
                            )),
                        },
                    })
                }
            }
        }
        InferredDestructurePatternKind::EnumConstructor {
            constructor_name,
            payload,
        } => match payload {
            None => Ok(MatchingBindingResult {
                remaining_expected_bindings: expected_bindings,
                pattern: InferredDestructurePattern {
                    type_value: source.type_value,
                    kind: InferredDestructurePatternKind::EnumConstructor {
                        constructor_name,
                        payload: None,
                    },
                },
            }),
            Some(payload) => {
                let result = match_bindings(module, *payload.clone(), expected_bindings)?;
                Ok(MatchingBindingResult {
                    remaining_expected_bindings: result.remaining_expected_bindings,
                    pattern: InferredDestructurePattern {
                        type_value: source.type_value,
                        kind: InferredDestructurePatternKind::EnumConstructor {
                            constructor_name,
                            payload: Some(Box::new(result.pattern)),
                        },
                    },
                })
            }
        },
        InferredDestructurePatternKind::Record {
            left_curly_bracket,
            key_pattern_pairs,
            right_curly_bracket,
        } => {
            // We use fold instead of map because we need to exhaust expected_bindings with each iteration
            let init = Ok((vec![], expected_bindings));
            let (key_pattern_pairs, remaining_expected_bindings) = key_pattern_pairs
                .into_iter()
                .fold(init, |result, (property, pattern)| match result {
                    Err(error) => Err(error),
                    Ok((key_pattern_pairs, remaining_expected_bindings)) => {
                        let result = match_bindings(module, pattern, remaining_expected_bindings)?;
                        Ok((
                            key_pattern_pairs
                                .into_iter()
                                .chain(vec![(property, result.pattern)])
                                .collect(),
                            result.remaining_expected_bindings,
                        ))
                    }
                })?;
            Ok(MatchingBindingResult {
                remaining_expected_bindings,
                pattern: InferredDestructurePattern {
                    type_value: source.type_value,
                    kind: InferredDestructurePatternKind::Record {
                        left_curly_bracket,
                        key_pattern_pairs,
                        right_curly_bracket,
                    },
                },
            })
        }
        InferredDestructurePatternKind::Array { .. } => {
            panic!();
        }
        InferredDestructurePatternKind::Tuple { .. } => {
            // We use fold instead of map because we need to exhaust expected_bindings wit
            panic!()
        }
        InferredDestructurePatternKind::Or { patterns } => {
            // Note that we use `map` instead of `fold` here, because the expected bindings
            // should remain the same after each iteration
            // because all patterns in an OR-pattern is expected to have the same set of bindings
            let results = patterns.fold_result(|pattern| {
                match_bindings(module, pattern, expected_bindings.clone())
            })?;
            Ok(MatchingBindingResult {
                remaining_expected_bindings: results.head.remaining_expected_bindings.clone(),
                pattern: InferredDestructurePattern {
                    type_value: source.type_value,
                    kind: InferredDestructurePatternKind::Or {
                        patterns: Box::new(results.map(|result| result.pattern)),
                    },
                },
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub identifier: Identifier,
    pub type_value: Type,
}

impl InferredDestructurePattern {
    fn bindings(&self) -> Vec<Binding> {
        match &self.kind {
            InferredDestructurePatternKind::Infinite { .. }
            | InferredDestructurePatternKind::Boolean { .. }
            | InferredDestructurePatternKind::Unit { .. }
            | InferredDestructurePatternKind::Underscore(_) => vec![],
            InferredDestructurePatternKind::Identifier(identifier) => vec![Binding {
                identifier: *identifier.clone(),
                type_value: self.type_value.clone(),
            }],
            InferredDestructurePatternKind::EnumConstructor { payload, .. } => match payload {
                Some(payload) => payload.bindings(),
                None => vec![],
            },
            InferredDestructurePatternKind::Record {
                key_pattern_pairs, ..
            } => key_pattern_pairs
                .iter()
                .flat_map(|(_, pattern)| pattern.bindings())
                .collect(),
            InferredDestructurePatternKind::Array { .. } => {
                panic!("Array pattern is still pending design")
            }
            InferredDestructurePatternKind::Tuple { patterns }
            | InferredDestructurePatternKind::Or { patterns } => patterns
                .clone()
                .into_vector()
                .into_iter()
                .flat_map(|pattern| pattern.bindings())
                .collect(),
        }
    }
}

/// Apply a list of type variable substitutions to a list of types
fn apply_type_variable_substitutions_to_types(
    type_variable_substitutions: &Vec<TypeVariableSubstitution>,
    type_values: Vec<Type>,
) -> Vec<Type> {
    type_values
        .iter()
        .map(|type_value| {
            apply_type_variable_substitutions_to_type(type_variable_substitutions, type_value)
        })
        .collect()
}

/// Apply a list of type variable substitutions to the given type
fn apply_type_variable_substitutions_to_type(
    type_variable_substitutions: &Vec<TypeVariableSubstitution>,
    type_value: &Type,
) -> Type {
    type_variable_substitutions
        .iter()
        .fold(type_value.clone(), |result, substitution| {
            apply_type_variable_substitution_to_type(substitution, &result)
        })
}

/// For example, if `substitution` is `{A => a}` and the given type is `Result<A, Result<A, B>>`.
///
/// Then the result is `Result<a, Result<a, B>>`
///
/// Note that uppercase `A` means explicit type variable, lowercase `a` means implicit type variable.
fn apply_type_variable_substitution_to_type(
    substitution: &TypeVariableSubstitution,
    type_value: &Type,
) -> Type {
    match type_value {
        Type::String => Type::String,
        Type::Character => Type::Character,
        Type::Unit => Type::Unit,
        Type::Float => Type::Float,
        Type::Integer => Type::Integer,
        Type::Keyword(identifier) => Type::Keyword(identifier.to_string()),
        Type::BuiltInOneArgumentType {
            kind,
            type_argument,
        } => Type::BuiltInOneArgumentType {
            kind: kind.clone(),
            type_argument: Box::new(apply_type_variable_substitution_to_type(
                substitution,
                type_argument.as_ref(),
            )),
        },
        Type::ImplicitTypeVariable(type_variable) => {
            Type::ImplicitTypeVariable(type_variable.clone())
        }
        Type::ExplicitTypeVariable(type_variable) => {
            if *type_variable.name == *substitution.from_type_variable.name {
                Type::ImplicitTypeVariable(substitution.to_type_variable.clone())
            } else {
                type_value.clone()
            }
        }
        Type::Tuple(types) => Type::Tuple(Box::new(types.clone().map(|type_value| {
            apply_type_variable_substitution_to_type(substitution, &type_value)
        }))),
        Type::Function(FunctionType {
            parameter_type,
            return_type,
            type_constraints,
        }) => Type::Function(FunctionType {
            parameter_type: Box::new(apply_type_variable_substitution_to_type(
                substitution,
                &parameter_type,
            )),
            return_type: Box::new(apply_type_variable_substitution_to_type(
                substitution,
                return_type.as_ref(),
            )),
            type_constraints: type_constraints
                .iter()
                .map(|type_constraint| TypeConstraint {
                    name: type_constraint.name.clone(),
                    type_value: apply_type_variable_substitution_to_type(
                        substitution,
                        &type_constraint.type_value,
                    ),
                })
                .collect(),
        }),
        Type::Record { key_type_pairs } => Type::Record {
            key_type_pairs: key_type_pairs
                .iter()
                .map(|(key, type_value)| {
                    (
                        key.clone(),
                        apply_type_variable_substitution_to_type(substitution, type_value),
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
                        apply_type_variable_substitution_to_type(substitution, type_value),
                    )
                })
                .collect(),
        },
        Type::TypeScheme(type_scheme) => Type::TypeScheme(Box::new(TypeScheme {
            type_variables: type_scheme.type_variables.clone(),
            type_value: apply_type_variable_substitution_to_type(
                substitution,
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
        | Type::Unit
        | Type::Underscore
        | Type::Keyword(_)
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
        Type::ImplicitTypeVariable(type_variable) => {
            let mut result: HashSet<String> = HashSet::new();
            result.insert(type_variable.name.clone());
            result
        }
        Type::Function(FunctionType {
            parameter_type,
            return_type,
            ..
        }) => {
            let mut result: HashSet<String> = HashSet::new();
            result.extend(get_free_type_variables_in_type(parameter_type.as_ref()));
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
///
/// This function also returns the instantiated constraints of the given `type_scheme`,
/// which should be resolved after the unification of each top level let-binding
pub fn instantiate_type_scheme(module: &mut Module, type_scheme: TypeScheme) -> Type {
    let type_variable_substitutions =
        instantiate_type_variables(module, type_scheme.type_variables.into_vector());

    let instantiated_type = apply_type_variable_substitutions_to_type(
        &type_variable_substitutions,
        &type_scheme.type_value,
    );

    instantiated_type
}

#[derive(Debug, Clone)]
pub struct TypeVariableSubstitution {
    pub from_type_variable: ExplicitTypeVariable,
    pub to_type_variable: ImplicitTypeVariable,
}

pub fn instantiate_type_variables(
    module: &mut Module,
    type_variables: Vec<ExplicitTypeVariable>,
) -> Vec<TypeVariableSubstitution> {
    type_variables
        .into_iter()
        .map(|from_type_variable| TypeVariableSubstitution {
            from_type_variable,
            to_type_variable: ImplicitTypeVariable {
                name: module.get_next_type_variable_name(),
            },
        })
        .collect()
}
