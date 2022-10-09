use std::collections::HashMap;
use std::path::PathBuf;
use std::process;
use std::process::Command;

use crate::module::Access;
use crate::module::ScopeManager;
use crate::module::ScopeName;
use crate::non_empty::NonEmpty;
use crate::parse::ParseError;
use crate::parse::Parser;

use crate::partially_qualified_ast;
use crate::qualified_ast;
use crate::qualified_ast::ResolvedName;
use crate::qualified_ast::SymbolUid;
use crate::raw_ast;
use crate::raw_ast::RawModuleNameRoot;
use crate::raw_ast::RawName;
use crate::stringify_error::print_compile_error;

use crate::stringify_error::print_parse_error;
use crate::tokenize::Position;
use crate::tokenize::RawIdentifier;
use crate::tokenize::Token;
use crate::tokenize::Tokenizer;
use crate::transpile::transpile_program;
use crate::transpile::transpile_statements;
use crate::unify::unify_qualified_module;
use crate::unify::unify_statements;
use crate::unify::UnifyError;
use crate::unify::UnifyProgramResult;

pub enum CompileError {
    ParseError {
        filename: CanonicalizedPath,
        error: ParseError,
    },
    UnifyError {
        filename: CanonicalizedPath,
        error: UnifyError,
    },
    NameResolutionError {
        filename: CanonicalizedPath,
        error: NameResolutionError,
    },
    InvalidFilename {
        filename: String,
        /// This is optional if the filename is passed in from CLI
        position: Option<Position>,
        error: std::io::Error,
    },
}
pub fn compile(filename: String) {
    match compile_helper(filename) {
        Ok(_) => {}
        Err(compile_error) => print_compile_error(compile_error),
    }
}

pub fn compile_helper(filename: String) -> Result<(), CompileError> {
    // NEW Algorithm:
    // 1. Parse this file
    // 2. Get the top level symbols of this file
    // 3. Resolve module alias statements
    //    - Whenever encounter file import, parse the file and get the top level symbols of that file,
    //    store the raw AST into name resolver, so that it can be converted into qualified AST
    //    later
    //
    //  Lastly, drain the stored raw ASTs, until it is empty
    //  When processing each raw AST, use rib to store symbols
    let mut name_resolver = NameResolver::new();

    get_top_level_symbols(&mut name_resolver, filename, None)?;
    let mut qualified_modules = vec![];
    let qualified_modules = loop {
        if let Some(partially_qualified_module) = name_resolver.partially_qualified_modules.pop() {
            qualified_modules.push(partially_qualified_module.to_qualified(&mut name_resolver)?);
        } else {
            break qualified_modules;
        }
    };

    let inferred_statements = qualified_modules
        .into_iter()
        .enumerate()
        .map(|(index, qualified_module)| {
            let statements = unify_qualified_module(qualified_module)?;

            if index == 0 {
                // Then this is the entry point module, so the top level expressions can be
                // retained
                Ok(statements)
            }
            // Otherwise, remove the top level expressions
            else {
                use crate::inferred_ast::InferredStatement::*;
                Ok(statements
                    .into_iter()
                    .filter_map(|statement| match statement {
                        ImportStatement(statement) => Some(statement),
                        Let {
                            access,
                            left,
                            right,
                        } => Some(Let {
                            access,
                            left,
                            right,
                        }),
                        Expression(_) => None,
                    }))
            }
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    let javascript = transpile_program(inferred_statements);

    // println!("{}", javascript);
    let output = Command::new("node")
        .arg("-e")
        .arg(javascript)
        .output()
        .expect("Failed to run NodeJS binary");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    if !stdout.is_empty() {
        println!("{}", stdout.trim())
    }
    if !stderr.is_empty() {
        eprintln!("{}", stderr.trim())
    }
    if let Some(code) = output.status.code() {
        process::exit(code)
    }
    Ok(())
}

/// Name resolver will parse this file,
/// and collect the top level symbols of this file.
pub fn get_top_level_symbols(
    name_resolver: &mut NameResolver,
    filename: String,
    position: Option<Position>,
) -> Result<Rib, CompileError> {
    match std::fs::canonicalize(PathBuf::from(filename)) {
        Err(error) => Err(CompileError::InvalidFilename {
            filename,
            position,
            error,
        }),
        Ok(canonicalized_path) => {
            let canonicalized_path = CanonicalizedPath(canonicalized_path);
            // Check if this file has been read before or not
            match name_resolver.top_level_ribs.get(&canonicalized_path) {
                Some(rib) => {
                    // Do nothing if this file is parsed before
                    Ok(rib.clone())
                }
                None => match std::fs::read_to_string(filename) {
                    Err(error) => Err(CompileError::InvalidFilename {
                        filename,
                        position,
                        error,
                    }),
                    Ok(source_code) => {
                        let tokenizer = Tokenizer::new(source_code);
                        let parser = Parser::new(&mut tokenizer);
                        let module_name = ModuleName {
                            filename: canonicalized_path,
                            segments: vec![],
                        };

                        // Create a new rib for this file
                        name_resolver
                            .top_level_ribs
                            .insert(canonicalized_path, Rib::new());

                        // Add top level symbols to the newly created rib
                        name_resolver
                            .partially_qualified_modules
                            .push(PartiallyQualifiedModule {
                                filename: canonicalized_path,
                                statements: parser
                                    .parse_statements()
                                    .map_err(|error| CompileError::ParseError {
                                        filename: canonicalized_path,
                                        error,
                                    })?
                                    .into_iter()
                                    .map(|statement| {
                                        statement
                                            .to_partially_qualified(&module_name, name_resolver)
                                    })
                                    .collect::<Result<
                                        Vec<partially_qualified_ast::Statement>,
                                        NameResolutionError,
                                    >>()
                                    .map_err(|error| CompileError::NameResolutionError {
                                        error,
                                        filename: canonicalized_path,
                                    })?,
                            });
                        let rib =
                            name_resolver
                                .get_top_level_rib(&module_name)
                                .map_err(|error| CompileError::NameResolutionError {
                                    error,
                                    filename: canonicalized_path,
                                })?;
                        Ok(rib.clone())
                    }
                },
            }
        }
    }
}

struct PartiallyQualifiedModule {
    filename: CanonicalizedPath,
    statements: Vec<partially_qualified_ast::Statement>,
}

pub struct QualifiedModule {
    /// This is required for diagnostics of type checking
    pub filename: CanonicalizedPath,
    pub statements: Vec<qualified_ast::Statement>,
}

impl PartiallyQualifiedModule {
    fn to_qualified(
        self,
        name_resolver: &mut NameResolver,
    ) -> Result<QualifiedModule, CompileError> {
        let module_name = ModuleName {
            filename: self.filename,
            segments: vec![],
        };
        let statements = name_resolver.run_in_new_rib(|name_resolver: &mut NameResolver| {
            self.statements
                .into_iter()
                .map(|statement| statement.to_qualified(name_resolver, &module_name))
                .collect::<Result<Vec<_>, CompileError>>()
        })?;
        Ok(QualifiedModule {
            filename: self.filename,
            statements,
        })
    }
}

#[derive(Clone)]
struct ModuleName {
    filename: CanonicalizedPath,
    segments: Vec<RawIdentifier>,
}

impl raw_ast::Statement {
    fn to_partially_qualified(
        self,
        module_name: &ModuleName,
        name_resolver: &mut NameResolver,
    ) -> Result<partially_qualified_ast::Statement, NameResolutionError> {
        match self {
            raw_ast::Statement::Let(let_statement) => Ok(partially_qualified_ast::Statement::Let(
                partially_qualified_ast::LetStatement {
                    access: let_statement.access,
                    keyword_let: let_statement.keyword_let,
                    name: name_resolver
                        .insert_top_level_value_symbol(module_name, let_statement.name)?,
                    doc_string: let_statement.doc_string,
                    type_annotation: let_statement.type_annotation,
                    expression: let_statement.expression,
                },
            )),

            raw_ast::Statement::TypeAlias(type_statement) => {
                Ok(partially_qualified_ast::Statement::TypeAlias(
                    partially_qualified_ast::TypeAliasStatement {
                        access: type_statement.access,
                        keyword_type: type_statement.keyword_type,
                        name: name_resolver
                            .insert_top_level_type_symbol(module_name, type_statement.name)?,
                        right: type_statement.right,
                        type_variables_declaration: type_statement.type_variables_declaration,
                    },
                ))
            }
            raw_ast::Statement::Enum(enum_statement) => Ok(
                partially_qualified_ast::Statement::Enum(partially_qualified_ast::EnumStatement {
                    access: enum_statement.access,
                    keyword_type: enum_statement.keyword_type,
                    name: name_resolver
                        .insert_top_level_type_symbol(module_name, enum_statement.name)?,
                    type_variables_declaration: enum_statement.type_variables_declaration,
                    constructors: enum_statement.constructors,
                }),
            ),
            raw_ast::Statement::ModuleDefinition(module_definition_statement) => {
                Ok(partially_qualified_ast::Statement::ModuleDefinition(
                    partially_qualified_ast::ModuleDefinitionStatement {
                        name: name_resolver.insert_top_level_module_symbol(
                            module_name,
                            &module_definition_statement.name,
                            Rib::new(),
                        )?,
                        statements: {
                            let module_name =
                                module_name.append_segment(module_definition_statement.name);
                            module_definition_statement
                                .statements
                                .into_iter()
                                .map(|statement| {
                                    statement.to_partially_qualified(&module_name, name_resolver)
                                })
                                .collect::<Result<Vec<_>, _>>()?
                        },
                    },
                ))
            }
            raw_ast::Statement::ModuleAlias(module_alias) => Ok(
                partially_qualified_ast::Statement::ModuleAlias(module_alias),
            ),
            raw_ast::Statement::Entry(entry) => {
                Ok(partially_qualified_ast::Statement::Entry(entry))
            }
        }
    }
}

pub enum NameResolutionError {
    DuplicatedName {
        defined_before_at: Position,
        definted_now_at: Position,
    },
    ModuleSymbolNotFound {
        name: RawIdentifier,
    },
    ValueSymbolNotFound {
        name: RawIdentifier,
    },
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct CanonicalizedPath(PathBuf);

pub struct NameResolver {
    top_level_ribs: HashMap<CanonicalizedPath, Rib>,
    current_uid: SymbolUid,
    /// Stack of ribs, used for scoping
    ribs: Vec<Rib>,

    partially_qualified_modules: Vec<PartiallyQualifiedModule>,
}

/// Rib contains the symbols of a given scope, it's actually similar to Environment.
///
/// Reference: https://rustc-dev-guide.rust-lang.org/name-resolution.html#scopes-and-ribs
#[derive(Clone)]
pub struct Rib {
    value_symbols: Vec<ValueSymbol>,
    type_symbols: Vec<TypeSymbol>,
    module_symbols: Vec<ModuleSymbol>,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    name: ResolvedName,
}

#[derive(Clone)]
struct TypeSymbol {
    name: ResolvedName,
}

#[derive(Clone)]
struct ModuleSymbol {
    name: ResolvedName,
    rib: Rib,
}

impl NameResolver {
    pub fn new() -> Self {
        NameResolver {
            current_uid: SymbolUid::new(),
            partially_qualified_modules: Vec::new(),
            top_level_ribs: HashMap::new(),
            ribs: Vec::new(),
        }
    }

    pub fn insert_top_level_value_symbol(
        &mut self,
        path_name: &ModuleName,
        name: RawIdentifier,
    ) -> Result<ResolvedName, NameResolutionError> {
        let rib = self.get_top_level_rib(path_name)?;
        let uid = self.current_uid.next();
        let resolved_name = ResolvedName {
            uid,
            position: name.position,
            representation: name.representation,
        };
        let symbol = ValueSymbol {
            name: resolved_name.clone(),
        };
        // No name duplication check is required, because value symbols can be overloaded
        rib.insert_value_symbol(symbol);
        Ok(resolved_name)
    }

    /// Top level rib is either a file, or a module nested within a file
    fn get_top_level_rib(
        &mut self,
        path_name: &ModuleName,
    ) -> Result<&mut Rib, NameResolutionError> {
        let mut current = self.top_level_ribs.get(&path_name.filename).unwrap();
        for segment in path_name.segments {
            current = &current.lookup_module_symbol(&segment)?;
        }
        Ok(&mut current)
    }

    pub fn insert_top_level_type_symbol(
        &self,
        module_name: &ModuleName,
        name: RawIdentifier,
    ) -> Result<ResolvedName, NameResolutionError> {
        let rib = self.get_top_level_rib(module_name)?;
        let name = ResolvedName {
            uid: self.current_uid.next(),
            representation: name.representation,
            position: name.position,
        };
        let symbol = TypeSymbol { name };
        rib.insert_type_symbol(symbol)?;
        Ok(name)
    }

    fn insert_top_level_module_symbol(
        &mut self,
        module_symbol: &ModuleName,
        name: &RawIdentifier,
        module: Rib,
    ) -> Result<ResolvedName, NameResolutionError> {
        let rib = self.get_top_level_rib(module_symbol)?;
        let name = ResolvedName {
            uid: self.current_uid.next(),
            position: name.position,
            representation: name.representation,
        };
        let symbol = ModuleSymbol { name, rib: module };
        rib.insert_module_symbol(symbol)?;
        Ok(name)
    }

    fn lookup_symbol<T, F: Fn(&Rib) -> Result<T, NameResolutionError>>(
        &self,
        module_name: &ModuleName,
        name: &RawIdentifier,
        f: F,
    ) -> Result<T, NameResolutionError> {
        // Lookup from ribs first
        // We have to reverse the iterator because we have to look from the start of the stack
        // first
        for rib in self.ribs.iter().rev() {
            match f(rib) {
                Ok(symbol) => return Ok(symbol),
                Err(_) => {
                    // Ignore the error and keep searching on parent rib
                }
            }
        }

        // If the ribs does not contains symbols with the given `name`
        // Look up from the top level rib with the name of `current_module_name`
        f(self.get_top_level_rib(module_name)?)
    }

    fn lookup_value_symbol(
        &self,
        module_name: &ModuleName,
        name: &RawIdentifier,
    ) -> Result<NonEmpty<ValueSymbol>, NameResolutionError> {
        self.lookup_symbol(module_name, name, |rib| rib.lookup_value_symbol(&name))
    }

    fn lookup_module_symbol(
        &self,
        current_module_name: &ModuleName,
        name: &RawIdentifier,
    ) -> Result<Rib, NameResolutionError> {
        self.lookup_symbol(current_module_name, name, |rib| {
            rib.lookup_module_symbol(&name)
        })
    }

    fn run_in_new_rib<T, F: FnMut(&mut NameResolver) -> T>(&mut self, f: F) -> T {
        self.ribs.push(Rib::new());
        let result = f(self);
        self.ribs.pop();
        result
    }

    fn insert_local_value_symbol(&self, symbol: ValueSymbol) {
        self.ribs
            .last()
            .expect("Rib should be created before inserting value")
            .insert_value_symbol(symbol)
    }
}

impl raw_ast::ModuleAliasStatement {
    /// For example:
    ///   module { x = y, a = {..} } = z
    ///
    ///  Desugars into:
    ///    module y = z::x
    ///    module {..} = z::a
    pub fn desugar(self) -> Vec<DesugaredModuleAliasStatement> {
        self.left.desugar(self.right)
    }

    pub fn gather_imported_symbols(
        &self,
        name_resolver: &mut NameResolver,
        current_module_name: &ModuleName,
    ) -> Result<(), CompileError> {
        for statement in self.desugar() {
            match statement.left {
                DesugaredModuleAliasStatementLeft::Name(name) => {
                    for symbol in statement
                        .right
                        .to_value_symbols(name_resolver, current_module_name)?
                        .into_vector()
                    {
                        name_resolver.insert_local_value_symbol(ValueSymbol {
                            name: ResolvedName {
                                uid: symbol.name.uid, // <-- Important: use the UID of the imported symbol
                                position: name.position,
                                representation: name.representation,
                            },
                        });
                    }
                    // TODO: insert type symbols
                    // TODO: insert module symbols
                }
                DesugaredModuleAliasStatementLeft::TakeAllButExcludeSome { excluded_names } => {
                    todo!()
                }
            }
        }
        Ok(())
    }
}

struct DesugaredModuleAliasStatement {
    left: DesugaredModuleAliasStatementLeft,
    right: RawName,
}

enum DesugaredModuleAliasStatementLeft {
    Name(RawIdentifier),
    /// For example: {.., a, b = c}
    TakeAllButExcludeSome {
        excluded_names: Vec<RawIdentifier>,
    },
}

impl raw_ast::ModuleDestructurePattern {
    fn desugar(self, raw_name: RawName) -> Vec<DesugaredModuleAliasStatement> {
        match self {
            raw_ast::ModuleDestructurePattern::Identifier(name) => {
                vec![DesugaredModuleAliasStatement {
                    left: DesugaredModuleAliasStatementLeft::Name(name),
                    right: raw_name,
                }]
            }
            raw_ast::ModuleDestructurePattern::Record { spread, pairs, .. } => pairs
                .into_iter()
                .flat_map(|pair| {
                    let updated_raw_module_name = raw_name.append_segment(pair.name);
                    match pair.pattern {
                        None => vec![DesugaredModuleAliasStatement {
                            left: DesugaredModuleAliasStatementLeft::Name(pair.name),
                            right: updated_raw_module_name,
                        }],
                        Some(pattern) => pattern.desugar(updated_raw_module_name),
                    }
                })
                .chain(
                    match spread {
                        Some(spread) => vec![DesugaredModuleAliasStatement {
                            left: DesugaredModuleAliasStatementLeft::TakeAllButExcludeSome {
                                excluded_names: pairs.into_iter().map(|pair| pair.name).collect(),
                            },
                            right: raw_name,
                        }],
                        None => vec![],
                    }
                    .into_iter(),
                )
                .collect(),
        }
    }
}

impl RawName {
    pub fn to_value_symbols(
        self,
        name_resolver: &mut NameResolver,
        current_module_name: &ModuleName,
    ) -> Result<NonEmpty<ValueSymbol>, CompileError> {
        match self.qualifier {
            Some(qualifier) => {
                let root_rib = match qualifier.root {
                    RawModuleNameRoot::Filepath(filename) => get_top_level_symbols(
                        name_resolver,
                        filename.content,
                        Some(filename.position()),
                    ),
                    RawModuleNameRoot::Identifier(name) => name_resolver
                        .lookup_module_symbol(current_module_name, &name)
                        .map_err(|error| CompileError::NameResolutionError {
                            filename: current_module_name.filename,
                            error,
                        }),
                }?;

                // Resolve each segment
                let mut rib = root_rib;
                for segment in qualifier.segments {
                    rib = rib.lookup_module_symbol(&segment).map_err(|error| {
                        CompileError::NameResolutionError {
                            filename: current_module_name.filename,
                            error,
                        }
                    })?;
                }
                rib.lookup_value_symbol(&self.name).map_err(|error| {
                    CompileError::NameResolutionError {
                        filename: current_module_name.filename,
                        error,
                    }
                })
            }
            None => name_resolver
                .lookup_value_symbol(current_module_name, &self.name)
                .map_err(|error| CompileError::NameResolutionError {
                    filename: current_module_name.filename,
                    error,
                }),
        }
    }

    pub fn position(&self) -> Position {
        todo!()
    }
}
impl Rib {
    pub fn new() -> Rib {
        Rib {
            value_symbols: vec![],
            type_symbols: vec![],
            module_symbols: vec![],
        }
    }

    fn insert_value_symbol(&mut self, symbol: ValueSymbol) {
        self.value_symbols.push(symbol)
    }

    fn insert_type_symbol(&mut self, symbol: TypeSymbol) -> Result<(), NameResolutionError> {
        match self
            .type_symbols
            .iter()
            .find(|symbol| symbol.name.representation.eq(&symbol.name.representation))
        {
            Some(existing_symbol) => Err(NameResolutionError::DuplicatedName {
                defined_before_at: existing_symbol.name.position,
                definted_now_at: symbol.name.position,
            }),
            None => {
                self.type_symbols.push(symbol);
                Ok(())
            }
        }
    }

    fn insert_module_symbol(&self, symbol: ModuleSymbol) -> Result<(), NameResolutionError> {
        match self
            .module_symbols
            .iter()
            .find(|symbol| symbol.name.representation.eq(&symbol.name.representation))
        {
            Some(existing_symbol) => Err(NameResolutionError::DuplicatedName {
                defined_before_at: existing_symbol.name.position,
                definted_now_at: symbol.name.position,
            }),
            None => {
                self.module_symbols.push(symbol);
                Ok(())
            }
        }
    }

    fn lookup_module_symbol(&self, name: &RawIdentifier) -> Result<Rib, NameResolutionError> {
        match self
            .module_symbols
            .iter()
            .find(|symbol| symbol.name.representation.eq(&name.representation))
        {
            Some(symbol) => Ok(symbol.rib),
            None => Err(NameResolutionError::ModuleSymbolNotFound { name: name.clone() }),
        }
    }

    fn lookup_value_symbol(
        &self,
        name: &RawIdentifier,
    ) -> Result<NonEmpty<ValueSymbol>, NameResolutionError> {
        match self
            .value_symbols
            .iter()
            .filter_map(|symbol| {
                if symbol.name.representation.eq(&name.representation) {
                    Some(symbol.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .split_first()
        {
            Some((head, tail)) => Ok(NonEmpty {
                head: *head,
                tail: tail.to_vec(),
            }),
            None => Err(NameResolutionError::ValueSymbolNotFound { name: name.clone() }),
        }
    }
}

impl ModuleName {
    pub fn append_segment(&self, name: RawIdentifier) -> ModuleName {
        ModuleName {
            filename: self.filename.clone(),
            segments: self
                .segments
                .into_iter()
                .chain(vec![name].into_iter())
                .collect(),
        }
    }

    pub fn append_segments(&self, names: Vec<RawIdentifier>) -> ModuleName {
        ModuleName {
            filename: self.filename.clone(),
            segments: self.segments.into_iter().chain(names.into_iter()).collect(),
        }
    }
}

impl raw_ast::EntryStatement {
    fn to_qualified(
        self,
        current_module_name: &ModuleName,
        name_resolver: &mut NameResolver,
    ) -> Result<qualified_ast::EntryStatement, CompileError> {
        Ok(qualified_ast::EntryStatement {
            keyword_entry: self.keyword_entry,
            expression: self
                .expression
                .to_qualified(current_module_name, name_resolver)?,
        })
    }
}
impl raw_ast::Expression {
    fn to_qualified(
        &self,
        current_module_name: &ModuleName,
        name_resolver: &mut NameResolver,
    ) -> Result<qualified_ast::Expression, CompileError> {
        use raw_ast::*;
        match self {
            Expression::Identifier(name) => Ok(qualified_ast::Expression::Identifier {
                name: name.clone(),
                referring_to: name
                    .to_value_symbols(name_resolver, current_module_name)?
                    .clone(),
            }),
            Expression::Statements { current, next } => {
                todo!()
            }
            Expression::Unit {
                left_parenthesis,
                right_parenthesis,
            } => todo!(),
            Expression::Parenthesized {
                left_parenthesis,
                right_parenthesis,
                value,
            } => todo!(),
            Expression::Float(_) => todo!(),
            Expression::Integer(_) => todo!(),
            Expression::String(_) => todo!(),
            Expression::InterpolatedString {
                start_quotes,
                sections,
                end_quotes,
            } => todo!(),
            Expression::Character(_) => todo!(),
            Expression::EnumConstructor { name, payload } => todo!(),
            Expression::Function(_) => todo!(),
            Expression::FunctionCall(_) => todo!(),
            Expression::Record {
                left_parenthesis,
                wildcard,
                key_value_pairs,
                right_parenthesis,
            } => todo!(),
            Expression::RecordAccess {
                expression,
                property_name,
            } => todo!(),
            Expression::RecordUpdate {
                expression,
                left_parenthesis,
                updates,
                right_parenthesis,
            } => todo!(),
            Expression::Array {
                left_square_bracket,
                elements,
                right_square_bracket,
            } => todo!(),
            Expression::Let {
                keyword_let,
                left,
                right,
                type_annotation,
                body,
            } => todo!(),
            Expression::CpsClosure { tilde, expression } => todo!(),
            Expression::CpsBang {
                argument,
                bang,
                function,
            } => todo!(),
        }
    }
}

impl raw_ast::TypeAnnotation {
    fn to_qualified(self) -> Result<qualified_ast::TypeAnnotation, CompileError> {
        todo!()
    }
}

impl partially_qualified_ast::Statement {
    pub fn to_qualified(
        &self,
        name_resolver: &mut NameResolver,
        current_module_name: &ModuleName,
    ) -> Result<qualified_ast::Statement, CompileError> {
        use partially_qualified_ast::*;
        match self {
            Statement::Let(let_statement) => {
                let_statement.to_qualified(name_resolver, current_module_name)
            }
            Statement::TypeAlias(_) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::ModuleAlias(_) => todo!(),
            Statement::ModuleDefinition(_) => todo!(),
            Statement::Entry(entry) => todo!(),
        }
    }
}

impl partially_qualified_ast::LetStatement {
    fn to_qualified(
        self,
        name_resolver: &mut NameResolver,
        current_module_name: &ModuleName,
    ) -> Result<qualified_ast::Statement, CompileError> {
        Ok(qualified_ast::Statement::Let(qualified_ast::LetStatement {
            access: self.access,
            keyword_let: self.keyword_let,
            name: self.name,
            doc_string: self.doc_string,
            type_annotation: self.type_annotation.to_qualified()?,
            expression: self
                .expression
                .to_qualified(current_module_name, name_resolver)?,
        }))
    }
}
