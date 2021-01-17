use crate::ast::*;
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

type Substitution = HashMap<String, SubstitutionItem>;

#[derive(Debug, Clone)]
pub struct Scope {
    /// Used for generating the next scope name
    next_scope_name: usize,

    /// Refer to the name of the current scope.
    /// This will be mutated when stepping in or out of a scope.
    /// This is used for querying and inserting new symbols.
    current_scope_name: usize,

    /// This graph store the relationship of each scopes.
    /// Used for querying the parent scope of a given scope.
    scope_graph: Vec<ScopePair>,
}

#[derive(Debug, Clone)]
pub struct ScopePair {
    child: usize,
    parent: usize,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            next_scope_name: 0,
            current_scope_name: 0,
            scope_graph: Vec::new(),
        }
    }

    pub fn step_into_new_child_scope(&mut self) {
        self.next_scope_name += 1;
        let scope_name = self.next_scope_name;

        self.scope_graph.push(ScopePair {
            child: scope_name,
            parent: self.current_scope_name,
        });

        self.current_scope_name = scope_name;
    }

    pub fn step_out_to_parent_scope(&mut self) {
        if let Some(scope_name) = self.get_parent_scope_name(self.current_scope_name) {
            self.current_scope_name = scope_name
        }
    }

    pub fn get_current_scope_name(&self) -> usize {
        self.current_scope_name
    }

    pub fn get_children_scope_names(&self, parent_scope_name: usize) -> Vec<usize> {
        self.scope_graph
            .iter()
            .filter_map(|ScopePair { child, parent }| {
                if *parent == parent_scope_name {
                    Some(*child)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>()
    }

    pub fn get_parent_scope_name(&self, child_scope_name: usize) -> Option<usize> {
        match self
            .scope_graph
            .iter()
            .filter_map(|ScopePair { child, parent }| {
                if *child == child_scope_name {
                    Some(*parent)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>()
            .first()
        {
            Some(parent_scope_name) => Some(*parent_scope_name),
            None => None,
        }
    }
}

/// This representation the identity of each symbol.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SymbolId {
    scope_name: usize,
    name: String,
}

/// This represents the meta data of a symbol
#[derive(Debug, Clone)]
pub struct SymbolMeta {
    pub name: String,
    pub exported: bool,
    pub declaration: Declaration,
    pub usage_references: RefCell<Vec<UsageReference>>,
}

#[derive(Debug, Clone)]
pub struct SymbolEntry<Symbol: Clone> {
    pub meta: SymbolMeta,
    pub value: Symbol,
}

#[derive(Debug, Clone)]
pub struct SymbolTable<Symbol: Clone> {
    symbols: HashMap<SymbolId, SymbolEntry<Symbol>>,
}

impl<Symbol: Clone> SymbolTable<Symbol> {
    pub fn new() -> SymbolTable<Symbol> {
        SymbolTable {
            symbols: Default::default(),
        }
    }

    pub fn symbols(&self) -> &HashMap<SymbolId, SymbolEntry<Symbol>> {
        &self.symbols
    }

    pub fn insert_symbol(
        &mut self,
        current_scope_name: usize,
        token: Token,
        source: &Source,
        new_symbol: SymbolEntry<Symbol>,
    ) -> Result<(), UnifyError> {
        let name = token.representation.clone();
        match self.symbols.get(&SymbolId {
            name: name.clone(),
            scope_name: current_scope_name,
        }) {
            Some(symbol) => Err(UnifyError {
                position: token.position,
                kind: UnifyErrorKind::DuplicatedIdentifier {
                    name,
                    first_declared_at: symbol.meta.declaration.clone(),
                    then_declared_at: Declaration::UserDefined {
                        source: source.clone(),
                        token,
                        scope_name: current_scope_name,
                    },
                },
            }),
            None => {
                self.symbols.insert(
                    SymbolId {
                        name,
                        scope_name: current_scope_name,
                    },
                    new_symbol,
                );
                Ok(())
            }
        }
    }

    /// NOTE: This method is not pure.
    /// When using this method to lookup say variable A,
    /// then the usage references of A will be updated.
    pub fn get_symbol(
        &self,
        scope: &Scope,
        source: &Source,
        symbol_name: &Token,
        scope_name: usize,
    ) -> Option<SymbolEntry<Symbol>> {
        let name = symbol_name.representation.clone();
        match self.symbols.get(&SymbolId { name, scope_name }) {
            Some(symbol) => {
                symbol
                    .meta
                    .usage_references
                    .borrow_mut()
                    .push(UsageReference {
                        position: symbol_name.position,
                        source: source.clone(),
                    });
                Some(symbol.clone())
            }
            None => match scope.get_parent_scope_name(scope_name) {
                None => None,
                Some(parent_scope_name) => {
                    self.get_symbol(scope, source, symbol_name, parent_scope_name)
                }
            },
        }
    }

    pub fn check_for_unused_value_symbols(
        &self,
        scope: &Scope,
        current_scope_name: usize,
    ) -> Result<(), UnifyError> {
        match self
            .symbols
            .iter()
            .filter_map(|(_, symbol)| {
                if symbol.meta.usage_references.borrow().is_empty() {
                    match &symbol.meta.declaration {
                        Declaration::UserDefined {
                            token, scope_name, ..
                        } => {
                            if *scope_name == current_scope_name {
                                Some(token.clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<Token>>()
            .first()
        {
            None => {
                // Check for unused variables in children scopes
                let children_scope_names = scope.get_children_scope_names(current_scope_name);
                for child_scope_name in &children_scope_names {
                    self.check_for_unused_value_symbols(scope, *child_scope_name)?;
                }
                Ok(())
            }
            Some(token) => Err(UnifyError {
                position: token.position,
                kind: UnifyErrorKind::UnusedVariale,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    value_symbols: SymbolTable<ValueSymbol>,
    type_symbols: SymbolTable<TypeSymbol>,
    constructor_symbols: SymbolTable<ConstructorSymbol>,
    type_variable_substitutions: Substitution,
    type_variable_index: Cell<usize>,
    scope: Scope,
    pub source: Source,

    exported: bool,
    namespace_name: String,
    namespaces: Vec<Environment>,
}

#[derive(Debug, Clone)]
enum SubstitutionItem {
    ImplicitTypeVariable(String),
    Type(Type),
    NotSubstituted,
}

/// NOTE: this list is reversed,
/// for example: `vec!['A', 'B', 'C']` means `C:B:A`
pub type Scoping = Vec<String>;
pub struct ScopedSymbol<Symbol> {
    scoping: Scoping,
    symbol: Symbol,
}
impl Environment {
    pub fn new(source: &Source, namespace_name: String) -> Environment {
        let mut result = Environment {
            source: source.clone(),
            value_symbols: SymbolTable::new(),
            type_symbols: SymbolTable::new(),
            constructor_symbols: SymbolTable::new(),
            type_variable_substitutions: (HashMap::new()),
            type_variable_index: Cell::new(0),
            scope: Scope::new(),

            namespace_name,
            exported: true,
            namespaces: vec![],
        };

        built_in_type_symbols()
            .into_iter()
            .for_each(|(name, type_symbol)| result.insert_built_in_type_symbol(name, type_symbol));

        built_in_value_symbols()
            .into_iter()
            .map(|(name, value_symbol)| {
                let token = Token {
                    token_type: TokenType::Identifier,
                    position: Position {
                        column_end: 0,
                        column_start: 0,
                        line_end: 0,
                        line_start: 0,
                        character_index_end: 0,
                        character_index_start: 0,
                    },
                    representation: name,
                };
                result.insert_value_symbol(&token, value_symbol)
            })
            .collect::<Result<Vec<()>, UnifyError>>()
            .expect("Compiler error");

        result
    }

    pub fn current_scope_name(&self) -> usize {
        self.scope.get_current_scope_name()
    }

    pub fn get_next_type_variable_name(&mut self) -> String {
        let type_variable_index = self.type_variable_index.get();
        let name = format!("@TVAR{}", type_variable_index);
        self.type_variable_index.set(type_variable_index + 1);

        self.type_variable_substitutions
            .insert(name.clone(), SubstitutionItem::NotSubstituted);

        name
    }

    pub fn insert_explicit_type_variable(
        &mut self,
        type_variable: &Token,
    ) -> Result<(), UnifyError> {
        self.insert_type_symbol(
            type_variable,
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::ExplicitTypeVariable {
                        name: type_variable.representation.clone(),
                    },
                },
            },
        )
    }

    pub fn introduce_implicit_type_variable(
        &mut self,
        variable_name: Option<&Token>,
    ) -> Result<Type, UnifyError> {
        let new_type_variable_name = self.get_next_type_variable_name();
        let type_value = Type::ImplicitTypeVariable {
            name: new_type_variable_name.clone(),
        };

        if let Some(variable_name) = variable_name {
            self.value_symbols.insert_symbol(
                self.scope.get_current_scope_name(),
                variable_name.clone(),
                &self.source,
                SymbolEntry {
                    meta: SymbolMeta {
                        exported: false,
                        name: variable_name.representation.clone(),
                        declaration: Declaration::UserDefined {
                            source: self.source.clone(),
                            token: variable_name.clone(),
                            scope_name: self.scope.get_current_scope_name(),
                        },
                        usage_references: Default::default(),
                    },
                    value: ValueSymbol {
                        type_scheme: TypeScheme {
                            type_variables: vec![],
                            type_value: type_value.clone(),
                        },
                    },
                },
            )?;
        }

        self.type_variable_substitutions
            .insert(new_type_variable_name, SubstitutionItem::NotSubstituted);

        Ok(type_value)
    }

    pub fn update_substitution(
        &mut self,
        type_variable_name: String,
        type_value: Type,
        position: Position,
    ) -> Result<(), UnifyError> {
        match self.get_type_variable_terminal_type(type_variable_name.clone()) {
            Some(terminal_type) => match type_value {
                Type::ImplicitTypeVariable { name } => {
                    self.update_substitution(name, terminal_type, position)
                }
                _ => {
                    let unified_type = unify_type(self, &terminal_type, &type_value, position)?;

                    let substitution_item = match unified_type {
                        Type::ImplicitTypeVariable { name } => {
                            SubstitutionItem::ImplicitTypeVariable(name)
                        }
                        other => SubstitutionItem::Type(other),
                    };

                    self.type_variable_substitutions
                        .insert(type_variable_name, substitution_item);
                    Ok(())
                }
            },
            None => {
                let substitution_item = match type_value {
                    Type::ImplicitTypeVariable { name } => {
                        SubstitutionItem::ImplicitTypeVariable(name)
                    }
                    other => SubstitutionItem::Type(other),
                };
                self.type_variable_substitutions
                    .insert(type_variable_name, substitution_item);
                Ok(())
            }
        }
    }

    /**
     * Apply subtitution to a type.  
     * For example, if we apply the subtitution <A= String, B= Int>
     * to type Foo<A, B>, we get Foo<A= String, B= Int>
     */
    pub fn apply_subtitution_to_type(&self, type_value: &Type) -> Type {
        match type_value {
            Type::Number => Type::Number,
            Type::Boolean => Type::Boolean,
            Type::String => Type::String,
            Type::Null => Type::Null,
            Type::ExplicitTypeVariable { name } => {
                Type::ExplicitTypeVariable { name: name.clone() }
            }
            Type::Array(type_value) => Type::Array(Box::new(
                self.apply_subtitution_to_type(type_value.as_ref()),
            )),
            Type::Tuple(types) => Type::Tuple(
                types
                    .iter()
                    .map(|type_value| self.apply_subtitution_to_type(type_value))
                    .collect(),
            ),
            Type::ImplicitTypeVariable { name } => {
                match self.get_type_variable_terminal_type(name.clone()) {
                    Some(type_value) => self.apply_subtitution_to_type(&type_value),
                    None => Type::ImplicitTypeVariable { name: name.clone() },
                }
            }
            Type::Function(function_type) => {
                Type::Function(self.apply_subtitution_to_function_type(function_type))
            }
            Type::Record { key_type_pairs } => Type::Record {
                key_type_pairs: key_type_pairs
                    .iter()
                    .map(|(key, type_value)| {
                        (key.clone(), self.apply_subtitution_to_type(type_value))
                    })
                    .collect(),
            },
            Type::Underscore => Type::Underscore,
            Type::Named {
                name,
                type_arguments,
            } => Type::Named {
                name: name.clone(),
                type_arguments: type_arguments
                    .iter()
                    .map(|(key, type_parameter)| {
                        (key.clone(), self.apply_subtitution_to_type(&type_parameter))
                    })
                    .collect(),
            },
        }
    }

    pub fn apply_subtitution_to_function_type(
        &self,
        FunctionType {
            first_argument_type,
            rest_arguments_types,
            return_type,
        }: &FunctionType,
    ) -> FunctionType {
        FunctionType {
            first_argument_type: Box::new(
                self.apply_subtitution_to_type(first_argument_type.as_ref()),
            ),
            rest_arguments_types: rest_arguments_types
                .iter()
                .map(|argument_type| self.apply_subtitution_to_type(argument_type))
                .collect(),
            return_type: Box::new(self.apply_subtitution_to_type(return_type.as_ref())),
        }
    }

    pub fn get_type_variable_terminal_type(&self, type_variable_name: String) -> Option<Type> {
        self.get_type_variable_terminal_type_(
            type_variable_name.clone(),
            type_variable_name.clone(),
            type_variable_name,
        )
    }

    /// Terminal means non-type variable type
    ///
    /// `initial_type_variable_name`:  
    ///     This variable is needed for breaking infinite recursion (i.e. circular reference)
    ///
    /// `previous_type_variable_name`:  
    ///     This variable is needed for breaking infinite recursion (i.e. circular reference)
    fn get_type_variable_terminal_type_(
        &self,
        initial_type_variable_name: String,
        previous_type_variable: String,
        type_variable_name: String,
    ) -> Option<Type> {
        match self.type_variable_substitutions.get(&type_variable_name) {
            Some(SubstitutionItem::Type(type_value)) => Some(type_value.clone()),
            Some(SubstitutionItem::ImplicitTypeVariable(type_variable_name)) => {
                if *type_variable_name == initial_type_variable_name
                    || *type_variable_name == previous_type_variable
                {
                    None
                } else {
                    match self.get_type_variable_terminal_type_(
                        initial_type_variable_name,
                        type_variable_name.clone(),
                        type_variable_name.clone(),
                    ) {
                        Some(type_value) => Some(type_value),
                        None => Some(Type::ImplicitTypeVariable {
                            name: type_variable_name.clone(),
                        }),
                    }
                }
            }
            Some(SubstitutionItem::NotSubstituted) => None,
            None => None, 
        }
    }

    pub fn step_into_new_child_scope(&mut self) {
        self.scope.step_into_new_child_scope()
    }

    pub fn step_out_to_parent_scope(&mut self) {
        self.scope.step_out_to_parent_scope()
    }

    pub fn insert_namespace(&mut self, new_namespace: Environment) -> Result<(), UnifyError> {
        match self
            .namespaces
            .iter()
            .find(|namespace| namespace.namespace_name == new_namespace.namespace_name)
        {
            Some(_) => {
                panic!("namespace already defined before")
            }
            None => {
                self.namespaces.push(new_namespace);
                Ok(())
            }
        }
    }

    pub fn insert_value_symbol(
        &mut self,
        symbol_name: &Token,
        value: ValueSymbol,
    ) -> Result<(), UnifyError> {
        self.value_symbols.insert_symbol(
            self.current_scope_name(),
            symbol_name.clone(),
            &self.source,
            SymbolEntry {
                meta: SymbolMeta {
                    name: symbol_name.representation.clone(),
                    declaration: Declaration::UserDefined {
                        source: self.source.clone(),
                        scope_name: self.current_scope_name(),
                        token: symbol_name.clone(),
                    },
                    usage_references: Default::default(),
                    exported: false,
                },
                value,
            },
        )
    }

    /// Insert a value symbol with a specific type
    /// If the type is None, then this variable will be
    /// instantiated to have a type of a new type variable
    pub fn insert_value_symbol_with_type(
        &mut self,
        variable_name: &Token,
        type_value: Option<Type>,
    ) -> Result<Type, UnifyError> {
        match type_value {
            None => self.introduce_implicit_type_variable(Some(variable_name)),
            Some(type_value) => {
                let current_scope_name = self.current_scope_name();
                let source = self.source.clone();
                let entry = SymbolEntry {
                    meta: SymbolMeta {
                        exported: false,
                        name: variable_name.representation.clone(),
                        declaration: Declaration::UserDefined {
                            source: self.source.clone(),
                            scope_name: self.current_scope_name(),
                            token: variable_name.clone(),
                        },
                        usage_references: Default::default(),
                    },
                    value: ValueSymbol {
                        type_scheme: TypeScheme {
                            type_variables: vec![],
                            type_value: type_value.clone(),
                        },
                    },
                };
                self.value_symbols.insert_symbol(
                    current_scope_name,
                    variable_name.clone(),
                    &source,
                    entry,
                )?;
                Ok(type_value)
            }
        }
    }

    pub fn check_for_unused_value_symbols(
        &self,
        current_scope_name: usize,
    ) -> Result<(), UnifyError> {
        self.value_symbols
            .check_for_unused_value_symbols(&self.scope, current_scope_name)
    }

    fn insert_built_in_type_symbol(&mut self, name: String, type_symbol: TypeSymbol) {
        self.insert_type_symbol(
            &Token {
                position: Position {
                    line_start: 0,
                    line_end: 0,
                    character_index_start: 0,
                    character_index_end: 0,
                    column_start: 0,
                    column_end: 0,
                },
                token_type: TokenType::Identifier,
                representation: name,
            },
            type_symbol,
        )
        .expect("Compiler error, built in type symbol should not clash with each other")
    }

    pub fn insert_type_symbol(
        &mut self,
        symbol_name: &Token,
        type_symbol: TypeSymbol,
    ) -> Result<(), UnifyError> {
        self.type_symbols.insert_symbol(
            self.current_scope_name(),
            symbol_name.clone(),
            &self.source,
            SymbolEntry {
                meta: SymbolMeta {
                    name: symbol_name.representation.clone(),
                    declaration: Declaration::UserDefined {
                        source: self.source.clone(),
                        token: symbol_name.clone(),
                        scope_name: self.current_scope_name(),
                    },
                    exported: false,
                    usage_references: Default::default(),
                },
                value: type_symbol,
            },
        )
    }

    pub fn insert_constructor_symbol(
        &mut self,
        token: &Token,
        constructor_symbol: ConstructorSymbol,
    ) -> Result<(), UnifyError> {
        self.constructor_symbols.insert_symbol(
            self.current_scope_name(),
            token.clone(),
            &self.source,
            SymbolEntry {
                meta: SymbolMeta {
                    name: token.representation.clone(),
                    exported: false,
                    declaration: Declaration::UserDefined {
                        source: self.source.clone(),
                        token: token.clone(),
                        scope_name: self.current_scope_name(),
                    },
                    usage_references: Default::default(),
                },
                value: constructor_symbol,
            },
        )
    }

    pub fn get_enum_constructors(&self, enum_name: &str) -> Vec<ConstructorSymbol> {
        self.constructor_symbols
            .symbols()
            .iter()
            .filter_map(|(_, constructor_symbol)| {
                if constructor_symbol.value.enum_name == enum_name {
                    Some(constructor_symbol.value.clone())
                } else {
                    None
                }
            })
            .chain(
                self.namespaces
                    .iter()
                    .flat_map(|namespace| namespace.get_enum_constructors(enum_name)),
            )
            .collect::<Vec<ConstructorSymbol>>()
    }

    pub fn get_type_symbol(&self, symbol_name: &Token) -> Option<SymbolEntry<TypeSymbol>> {
        self.type_symbols.get_symbol(
            &self.scope,
            &self.source,
            &symbol_name,
            self.current_scope_name(),
        )
    }

    pub fn get_value_symbol(&mut self, symbol_name: &Token) -> Option<SymbolEntry<ValueSymbol>> {
        self.value_symbols.get_symbol(
            &self.scope,
            &self.source,
            &symbol_name,
            self.current_scope_name(),
        )
    }

    fn get_matching_constructor_symbols(
        &self,
        symbol_name: &Token,
    ) -> Vec<ScopedSymbol<ConstructorSymbol>> {
        let mut symbols = self
            .namespaces
            .iter()
            .flat_map(|namespace| {
                namespace
                    .get_matching_constructor_symbols(symbol_name)
                    .into_iter()
                    .map(|mut scoped_symbol| {
                        scoped_symbol.scoping.push(namespace.namespace_name.clone());
                        scoped_symbol.scoping.push(self.namespace_name.clone());
                        scoped_symbol
                    })
                    .collect::<Vec<ScopedSymbol<ConstructorSymbol>>>()
            })
            .collect::<Vec<ScopedSymbol<ConstructorSymbol>>>();

        match self.constructor_symbols.get_symbol(
            &self.scope,
            &self.source,
            &symbol_name,
            self.current_scope_name(),
        ) {
            None => symbols,
            Some(symbol) => {
                symbols.push(ScopedSymbol {
                    scoping: vec![],
                    symbol: symbol.value,
                });
                symbols
            }
        }
    }

    /// Return a namespace, given a scoping, for example `A::B::C`
    fn resolve_scoping(&self, scoping: &Vec<Token>) -> Result<&Environment, UnifyError> {
        scoping
            .iter()
            .fold(Ok(&self), |result, namespace_name| match result {
                Err(error) => Err(error),
                Ok(namespace) => {
                    let matching_namespaces =
                        namespace.find_matching_namespaces(&namespace_name.representation);
                    match matching_namespaces.get(0) {
                        None => Err(UnifyError {
                            position: namespace_name.position,
                            kind: UnifyErrorKind::UnknownNamespace,
                        }),
                        Some((_, namespace)) => {
                            if matching_namespaces.len() > 1 {
                                Err(UnifyError {
                                    position: namespace_name.position,
                                    kind: UnifyErrorKind::AmbiguousNamespace {
                                        namespace_name: namespace_name.representation.clone(),
                                        possible_scopings: matching_namespaces
                                            .iter()
                                            .map(|(scoping, _)| scoping.clone())
                                            .collect(),
                                    },
                                })
                            } else {
                                Ok(*namespace)
                            }
                        }
                    }
                }
            })
    }

    fn find_matching_namespaces(&self, namespace_name: &str) -> Vec<(Scoping, &Environment)> {
        let mut namespaces = self
            .namespaces
            .iter()
            .flat_map(|namespace| {
                namespace
                    .find_matching_namespaces(namespace_name)
                    .into_iter()
                    .map(|(mut scoping, namespace)| {
                        scoping.push(self.namespace_name.clone());
                        (scoping, namespace)
                    })
                    .collect::<Vec<(Scoping, &Environment)>>()
            })
            .collect::<Vec<(Scoping, &Environment)>>();
        if self.namespace_name == *namespace_name {
            namespaces.push((vec![], self));
            namespaces
        } else {
            namespaces
        }
    }

    pub fn get_constructor_symbol(
        &mut self,
        scoped_name: &ScopedName,
    ) -> Result<Option<ConstructorSymbol>, UnifyError> {
        let namespace = self.resolve_scoping(&scoped_name.namespaces)?;
        let constructors = namespace.get_matching_constructor_symbols(&scoped_name.name);
        match constructors.get(0) {
            None => Ok(None),
            Some(constructor) => {
                if constructors.len() > 1 {
                    Err(UnifyError {
                        position: scoped_name.name.position,
                        kind: UnifyErrorKind::AmbiguousSymbolUsage {
                            symbol_name: scoped_name.name.representation.clone(),
                            possible_scopings: constructors
                                .into_iter()
                                .map(|constructor| constructor.scoping)
                                .collect(),
                        },
                    })
                } else {
                    Ok(Some(constructor.symbol.clone()))
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    UserDefined {
        source: Source,
        token: Token,
        scope_name: usize,
    },
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub type_scheme: TypeScheme,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    pub type_scheme: TypeScheme,
}

#[derive(Debug, Clone)]
pub struct ConstructorSymbol {
    /// Refers to the name of the enum that contains this constructor
    pub enum_name: String,
    pub constructor_name: String,
    pub type_variables: Vec<String>,
    pub payload: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UsageReference {
    position: Position,
    source: Source,
}

fn built_in_value_symbols() -> Vec<(String, ValueSymbol)> {
    let type_variable_name = "T".to_string();
    vec![(
        "print".to_string(),
        ValueSymbol {
            type_scheme: TypeScheme {
                type_variables: vec![type_variable_name.clone()],
                type_value: Type::Function(FunctionType {
                    first_argument_type: Box::new(Type::ImplicitTypeVariable {
                        name: type_variable_name,
                    }),
                    rest_arguments_types: vec![],
                    return_type: Box::new(Type::Null),
                }),
            },
        },
    )]
}

fn built_in_type_symbols() -> Vec<(String, TypeSymbol)> {
    vec![
        (
            "Array".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec!["Element".to_string()],
                    type_value: Type::Array(Box::new(Type::ImplicitTypeVariable {
                        name: "Element".to_string(),
                    })),
                },
            },
        ),
        (
            "string".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::String,
                },
            },
        ),
        (
            "number".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Number,
                },
            },
        ),
        (
            "null".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Null,
                },
            },
        ),
        (
            "boolean".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Boolean,
                },
            },
        ),
    ]
}
