use std::collections::HashMap;
use std::path::PathBuf;
use std::process;

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
            qualified_modules.push(
                partially_qualified_module
                    .to_qualified(&mut name_resolver)
                    .map_err(|error| CompileError::NameResolutionError {
                        filename: partially_qualified_module.filename,
                        error,
                    })?,
            );
        } else {
            break qualified_modules;
        }
    };

    // TODO: unify qualified modules
    match result {
        Err(compile_error) => print_compile_error(compile_error),
        Ok(mut typechecked_modules) => {
            // result.interpret(result..into());
            use std::process::Command;

            let (init_modules, last_module) = {
                let last_module = typechecked_modules
                    // Note that `split_off` will mutate the given IndexMap
                    .split_off(typechecked_modules.len() - 1)
                    .first()
                    .expect("typechecked_modules should have at least one element");
                (typechecked_modules, *last_module.1)
            };

            let javascript = transpile_program(UnifyProgramResult {
                // The last inferred module should be the entry point, if the topological sort is correct
                entrypoint: last_module,
                imported_modules: init_modules,
            });
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
        }
    };
    Ok(())
}

/// Name resolver will parse this file,
/// and collect the top level symbols of this file.
pub fn get_top_level_symbols(
    name_resolver: &mut NameResolver,
    filename: String,
    position: Option<Position>,
) -> Result<ModuleName, CompileError> {
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
                    Ok(rib.name)
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
                        Ok(module_name)
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

struct QualifiedModule {
    /// This is required for diagnostics of type checking
    filename: CanonicalizedPath,
    statements: Vec<qualified_ast::Statement>,
}

impl PartiallyQualifiedModule {
    fn to_qualified(
        self,
        name_resolver: &mut NameResolver,
    ) -> Result<QualifiedModule, NameResolutionError> {
        let module_name = ModuleName {
            filename: self.filename,
            segments: vec![],
        };
        Ok(QualifiedModule {
            filename: self.filename,
            statements: self
                .statements
                .into_iter()
                .map(|statement| statement.to_qualified(name_resolver, &module_name))
                .collect::<Result<Vec<_>, NameResolutionError>>()?,
        })
    }
}

#[derive(Clone)]
struct ModuleName {
    filename: CanonicalizedPath,
    segments: Vec<SymbolName>,
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
                    name: name_resolver.insert_value_symbol(module_name, let_statement.name)?,
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
                        name: name_resolver.insert_type_symbol(module_name, type_statement.name)?,
                        right: type_statement.right,
                        type_variables_declaration: type_statement.type_variables_declaration,
                    },
                ))
            }
            raw_ast::Statement::Enum(enum_statement) => Ok(
                partially_qualified_ast::Statement::Enum(partially_qualified_ast::EnumStatement {
                    access: enum_statement.access,
                    keyword_type: enum_statement.keyword_type,
                    name: name_resolver.insert_type_symbol(module_name, enum_statement.name)?,
                    type_variables_declaration: enum_statement.type_variables_declaration,
                    constructors: enum_statement.constructors,
                }),
            ),
            raw_ast::Statement::ModuleDefinition(module_definition_statement) => {
                Ok(partially_qualified_ast::Statement::ModuleDefinition(
                    partially_qualified_ast::ModuleDefinitionStatement {
                        name: name_resolver.insert_module_symbol(
                            module_name,
                            &module_definition_statement.name,
                            Rib::new(),
                        )?,
                        statements: {
                            let module_name = module_name.append_segment(SymbolName(
                                module_definition_statement.name.representation,
                            ));
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
    SymbolNotFound {
        name: RawIdentifier,
    },
    NotAValueSymbol(Symbol),
    NotAModuleSymbol(Symbol),
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct CanonicalizedPath(PathBuf);

pub struct NameResolver {
    top_level_ribs: HashMap<CanonicalizedPath, Rib>,
    current_uid: SymbolUid,
    /// Stack of ribs, used for scoping
    ribs: Vec<Rib>,

    partially_qualified_modules: Vec<PartiallyQualifiedModule>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct SymbolName(String);

/// Rib contains the symbols of a given scope, it's actually similar to Environment.
///
/// Reference: https://rustc-dev-guide.rust-lang.org/name-resolution.html#scopes-and-ribs
#[derive(Clone)]
pub struct Rib {
    symbols: HashMap<SymbolName, Symbol>,
}

#[derive(Clone)]
enum Symbol {
    /// Value name can be overloaded, therefore vector is used
    Value(NonEmpty<ResolvedName>),
    Type(ResolvedName),
    Module {
        name: ResolvedName,
        module: Rib,
    },
}

struct Prefix {
    path: CanonicalizedPath,
    segments: Vec<SymbolName>,
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

    pub fn insert_value_symbol(
        &mut self,
        path_name: &ModuleName,
        name: RawIdentifier,
    ) -> Result<ResolvedName, NameResolutionError> {
        let rib = self.get_top_level_rib(path_name);
        let symbol_name = SymbolName(name.representation);
        let uid = self.current_uid.next();
        let resolved_name = ResolvedName {
            uid,
            position: name.position,
        };
        let symbol = Symbol::Value(NonEmpty {
            head: resolved_name,
            tail: vec![],
        });
        match rib.get(&symbol_name) {
            Some(symbol) => match symbol {
                Symbol::Value(symbols) => {
                    symbols.push(resolved_name);
                    Ok(resolved_name)
                }
                Symbol::Type(_) | Symbol::Module { .. } => {
                    Err(NameResolutionError::DuplicatedName {
                        defined_before_at: symbol.position(),
                        definted_now_at: name.position,
                    })
                }
            },
            None => {
                rib.symbols.insert(symbol_name, symbol);
                Ok(resolved_name)
            }
        }
    }

    fn get_top_level_rib(&self, path_name: &ModuleName) -> &Rib {
        let mut current = self.top_level_ribs.get(&path_name.filename).unwrap();
        for segment in path_name.segments {
            current = match current.get(&segment).unwrap() {
                Symbol::Module { module, .. } => module,
                _ => unreachable!(),
            }
        }
        &current
    }

    pub fn insert_type_symbol(
        &self,
        path_name: &ModuleName,
        name: RawIdentifier,
    ) -> Result<ResolvedName, NameResolutionError> {
        let symbol_name = SymbolName(name.representation);
        let rib = self.get_top_level_rib(path_name);
        match rib.get(&symbol_name) {
            Some(other) => Err(NameResolutionError::DuplicatedName {
                defined_before_at: other.position(),
                definted_now_at: name.position,
            }),
            None => {
                let resolved_name = ResolvedName {
                    uid: self.current_uid.next(),
                    position: name.position,
                };
                rib.insert_symbol(symbol_name, Symbol::Type(resolved_name));

                Ok(resolved_name)
            }
        }
    }

    fn insert_module_symbol(
        &mut self,
        path_name: &ModuleName,
        name: &RawIdentifier,
        module: Rib,
    ) -> Result<ResolvedName, NameResolutionError> {
        let symbol_name = SymbolName(name.representation);
        let rib = self.get_top_level_rib(path_name);
        match rib.get(&symbol_name) {
            Some(symbol) => Err(NameResolutionError::DuplicatedName {
                defined_before_at: symbol.position(),
                definted_now_at: name.position.clone(),
            }),
            None => {
                let name = ResolvedName {
                    uid: self.current_uid.next(),
                    position: name.position,
                };
                rib.insert_symbol(symbol_name, Symbol::Module { name, module });
                Ok(name)
            }
        }
    }

    fn lookup_symbol(
        &self,
        module_name: &ModuleName,
        symbol: &RawIdentifier,
    ) -> Result<&Symbol, NameResolutionError> {
        // Lookup from ribs first
        // We have to reverse the iterator because we have to look from the start of the stack
        // first
        for rib in self.ribs.iter().rev() {
            match rib.lookup_symbol(symbol) {
                Some(symbol) => return Ok(symbol),
                None => {}
            }
        }

        // If the ribs does not contains symbols with the given `name`
        // Look up from the top level rib with the name of `current_module_name`
        match self.get_top_level_rib(module_name).lookup_symbol(symbol) {
            Some(names) => Ok(names),
            None => Err(NameResolutionError::SymbolNotFound {
                name: symbol.clone(),
            }),
        }
    }

    fn lookup_value_symbol(
        &self,
        module_name: &ModuleName,
        name: &RawIdentifier,
    ) -> Result<NonEmpty<ResolvedName>, NameResolutionError> {
        let symbol = self.lookup_symbol(module_name, name)?;
        match symbol {
            Symbol::Value(names) => Ok(names.clone()),
            _ => Err(NameResolutionError::NotAValueSymbol(symbol.clone())),
        }
    }

    fn lookup_module_symbol(
        &self,
        current_module_name: &ModuleName,
        name: &RawIdentifier,
    ) -> Result<ModuleName, NameResolutionError> {
        let symbol = self.lookup_symbol(current_module_name, name)?;
        match symbol {
            Symbol::Module { .. } => Ok(current_module_name.append_segment(name.to_symbol_name())),
            _ => Err(NameResolutionError::NotAModuleSymbol(symbol.clone())),
        }
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

    pub fn to_qualified(
        &self,
        name_resolver: &mut NameResolver,
        current_module_name: &ResolvedName,
    ) -> Result<Vec<qualified_ast::LetAliasStatement>, CompileError> {
        self.desugar()
            .into_iter()
            .map(|statement| match statement.left {
                DesugaredModuleAliasStatementLeft::Name(name) => {
                    let result = qualified_ast::LetAliasStatement {
                        keyword_module: self.keyword_module,
                        left: name.to_qualified(current_module_name),
                        right: statement
                            .right
                            .to_value_symbols(name_resolver, current_module_name)?,
                    };

                    // Insert this module alias to the name resolver
                    name_resolver
                        .insert_value_symbol(ModuleSymbol {
                            access: Access::Protected,
                            name: result.left.clone(),
                            scope: name_resolver.scope_manager.get_current_scope_name(),
                            kind: ModuleSymbolKind::ModuleAlias {
                                referring_to: result.right,
                            },
                        })
                        .map_err(|err| CompileError::NameResolutionError(Box::new(err)))?;
                    Ok(result)
                }
                DesugaredModuleAliasStatementLeft::TakeAllButExcludeSome { excluded_names } => {
                    todo!()
                }
            })
            .collect::<Result<Vec<_>, _>>()
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
    ) -> Result<NonEmpty<ResolvedName>, CompileError> {
        let module_name = match self.qualifier {
            Some(qualifier) => {
                let module_name_root = match qualifier.root {
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
                let mut module_name = module_name_root;
                for segment in qualifier.segments {
                    module_name = name_resolver
                        .lookup_module_symbol(&module_name, &segment)
                        .map_err(|error| CompileError::NameResolutionError {
                            filename: module_name_root.filename,
                            error,
                        })?;
                }
                &module_name
            }
            None => current_module_name,
        };
        name_resolver
            .lookup_value_symbol(module_name, &self.name)
            .map_err(|error| CompileError::NameResolutionError {
                filename: current_module_name.filename,
                error,
            })
    }

    pub fn position(&self) -> Position {
        todo!()
    }
}
impl Rib {
    pub fn new() -> Rib {
        Rib {
            symbols: HashMap::new(),
        }
    }

    fn insert_symbol(&mut self, name: SymbolName, symbol: Symbol) {
        self.symbols.insert(name, symbol);
    }

    fn get(&self, name: &SymbolName) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    fn lookup_symbol(&self, name: &RawIdentifier) -> Option<&Symbol> {
        self.symbols.get(&SymbolName(name.representation))
    }
}
impl Symbol {
    fn position(&self) -> Position {
        match self {
            Symbol::Value(symbols) => symbols.first().defined_at.clone(),
            Symbol::Type(symbol) => symbol.defined_at.clone(),
            Symbol::Module { defined_at, .. } => defined_at.clone(),
        }
    }
}
impl ModuleName {
    pub fn append_segment(&self, name: SymbolName) -> ModuleName {
        ModuleName {
            filename: self.filename.clone(),
            segments: self
                .segments
                .into_iter()
                .chain(vec![name].into_iter())
                .collect(),
        }
    }

    pub fn append_segments(&self, names: Vec<SymbolName>) -> ModuleName {
        ModuleName {
            filename: self.filename.clone(),
            segments: self.segments.into_iter().chain(names.into_iter()).collect(),
        }
    }
}

impl raw_ast::EntryStatement {
    fn to_qualified(
        self,
        name_resolver: &mut NameResolver,
    ) -> Result<qualified_ast::EntryStatement, NameResolutionError> {
        Ok(qualified_ast::EntryStatement {
            keyword_entry: self.keyword_entry,
            expression: self.expression.to_qualified(name_resolver)?,
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
                referring_to: name.to_value_symbols(name_resolver, current_module_name)?,
            }),
            Expression::Statements { current, next } => todo!(),
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
    fn to_qualified(self) -> Result<qualified_ast::TypeAnnotation, NameResolutionError> {
        todo!()
    }
}
impl RawIdentifier {
    pub fn to_qualified(self, current_module_name: &ResolvedName) -> qualified_ast::ResolvedName {
        current_module_name.append_segment(self)
    }
}

impl partially_qualified_ast::Statement {
    pub fn to_qualified(
        &self,
        name_resolver: &mut NameResolver,
        current_module_name: &ModuleName,
    ) -> Result<qualified_ast::Statement, NameResolutionError> {
        use partially_qualified_ast::*;
        match self {
            Statement::Let(let_statement) => {
                let_statement.to_qualified(name_resolver, current_module_name)
            }
            Statement::TypeAlias(_) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::ModuleAlias(_) => todo!(),
            Statement::ModuleDefinition(_) => todo!(),
            Statement::Entry(entry) => Ok(qualified_ast::Statement::Entry(entry.to_qualified()?)),
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

impl RawIdentifier {
    pub fn to_symbol_name(&self) -> SymbolName {
        SymbolName(self.representation.clone())
    }
}
