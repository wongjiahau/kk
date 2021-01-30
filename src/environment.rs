use crate::ast::*;
use crate::non_empty::NonEmpty;
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

type Substitution = HashMap<String, SubstitutionItem>;

pub struct GetValueSymbolResult {
    pub symbol_uid: usize,
    pub type_scheme: TypeScheme,
}

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
    /// This is needed for single dispatch disambiguation during transpilation
    pub uid: usize,
    pub name: String,
    pub scope_name: usize,
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

    /// This function will return error if the symbol to be inserted clashes with existing names
    pub fn insert_symbol(
        &mut self,
        current_scope_name: usize,
        token: Token,
        source: &Source,
        new_symbol: SymbolEntry<Symbol>,
    ) -> Result<(), UnifyError> {
        let name = token.representation.clone();
        let symbol_id = SymbolId {
            name: name.clone(),
            scope_name: current_scope_name,
        };
        match self.symbols.get(&symbol_id) {
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
                self.symbols.insert(symbol_id, new_symbol);
                Ok(())
            }
        }
    }

    /// Unlike `get_symbol`, this method will not mutate anything
    pub fn get_symbol_from_current_scope(
        &self,
        symbol_name: &Token,
        scope_name: usize,
    ) -> Option<&SymbolEntry<Symbol>> {
        let name = symbol_name.representation.clone();
        self.symbols.get(&SymbolId { name, scope_name })
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
    /// Non-function symbols
    value_symbols: SymbolTable<ValueSymbol>,

    /// Function symbols is stored in Vector instead of HashMap.
    /// This is because to implement multi-dispatch, a simple name-lookup is not enough,
    /// we need to iterate through the list to find the best fit.
    function_symbols: Vec<FunctionSymbol>,
    type_symbols: SymbolTable<TypeSymbol>,
    constructor_symbols: SymbolTable<ConstructorSymbol>,
    type_variable_substitutions: Substitution,
    type_variable_index: Cell<usize>,

    /// Every symbol in an environment will be assigned a unique ID, regardless of the scope
    current_uid: Cell<usize>,
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
            function_symbols: vec![],
            type_symbols: SymbolTable::new(),
            constructor_symbols: SymbolTable::new(),
            type_variable_substitutions: (HashMap::new()),
            type_variable_index: Cell::new(0),
            scope: Scope::new(),
            current_uid: Cell::new(0),
            namespace_name,
            exported: true,
            namespaces: vec![],
        };

        built_in_type_symbols()
            .into_iter()
            .for_each(|(name, type_symbol)| result.insert_built_in_type_symbol(name, type_symbol));

        built_in_value_symbols()
            .into_iter()
            .map(|(name, uid, value_symbol)| {
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
                result.insert_value_symbol(&token, value_symbol, Some(uid))
            })
            .collect::<Result<Vec<usize>, UnifyError>>()
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

    fn get_next_symbol_uid(&mut self) -> usize {
        let result = self.current_uid.get();
        self.current_uid.set(result + 1);
        result
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
            self.insert_value_symbol(
                variable_name,
                TypeScheme {
                    type_variables: vec![],
                    type_value: type_value.clone(),
                },
                None,
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
            Type::Tuple(types) => Type::Tuple(Box::new(
                types
                    .clone()
                    .map(|type_value| self.apply_subtitution_to_type(&type_value)),
            )),
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
            parameters_types,
            return_type,
        }: &FunctionType,
    ) -> FunctionType {
        FunctionType {
            parameters_types: Box::new(
                parameters_types
                    .clone()
                    .map(|argument_type| self.apply_subtitution_to_type(&argument_type)),
            ),
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

    /// This function returns the uid for this symbol upon successful insertion
    pub fn insert_value_symbol(
        &mut self,
        symbol_name: &Token,
        type_scheme: TypeScheme,
        uid: Option<usize>,
    ) -> Result<usize, UnifyError> {
        let uid = match uid {
            Some(uid) => uid,
            None => self.get_next_symbol_uid(),
        };
        let meta = SymbolMeta {
            uid,
            name: symbol_name.representation.clone(),
            scope_name: self.current_scope_name(),
            declaration: Declaration::UserDefined {
                source: self.source.clone(),
                scope_name: self.current_scope_name(),
                token: symbol_name.clone(),
            },
            usage_references: Default::default(),
            exported: false,
        };
        match type_scheme.type_value {
            Type::Function(function_type) => {
                let value_symbol = self
                    .value_symbols
                    .get_symbol_from_current_scope(symbol_name, self.current_scope_name());
                match value_symbol {
                    None => {
                        // Check that there's no clashing function signature
                        let function_signature = FunctionSignature {
                            type_variables: type_scheme.type_variables,
                            function_type: function_type.clone(),
                        };
                        let matching_function_symbol =
                            self.function_symbols.iter().find(|symbol| {
                                symbol.meta.name == symbol_name.representation
                                    && symbol.meta.scope_name == self.current_scope_name()
                                    && overlap(
                                        symbol
                                            .function_signature
                                            .function_type
                                            .parameters_types
                                            .first(),
                                        function_type.parameters_types.first(),
                                    )
                            });

                        match matching_function_symbol {
                            Some(function_symbol) => Err(UnifyError {
                                position: symbol_name.position,
                                kind: UnifyErrorKind::ConflictingFunctionDefinition {
                                    function_name: symbol_name.representation.clone(),
                                    new_first_parameter_type: function_type
                                        .parameters_types
                                        .first()
                                        .clone(),
                                    existing_first_parameter_type: function_symbol
                                        .function_signature
                                        .function_type
                                        .parameters_types
                                        .first()
                                        .clone(),
                                    first_declared_at: function_symbol.meta.declaration.clone(),
                                },
                            }),
                            None => {
                                self.function_symbols.push(FunctionSymbol {
                                    meta,
                                    function_signature,
                                });
                                Ok(uid)
                            }
                        }
                    }
                    Some(symbol) => Err(UnifyError {
                        position: symbol_name.position,
                        kind: UnifyErrorKind::DuplicatedIdentifier {
                            name: symbol_name.representation.clone(),
                            first_declared_at: symbol.meta.declaration.clone(),
                            then_declared_at: Declaration::UserDefined {
                                source: self.source.clone(),
                                token: symbol_name.clone(),
                                scope_name: self.current_scope_name(),
                            },
                        },
                    }),
                }
            }

            // Not function
            _ => {
                self.value_symbols.insert_symbol(
                    self.current_scope_name(),
                    symbol_name.clone(),
                    &self.source,
                    SymbolEntry {
                        meta,
                        value: ValueSymbol { type_scheme },
                    },
                )?;
                Ok(uid)
            }
        }
    }

    /// Insert a value symbol with a specific type
    /// If the type is None, then this variable will be
    /// instantiated to have a type of a new type variable
    pub fn insert_value_symbol_with_type(
        &mut self,
        variable_name: &Token,
        type_value: Option<Type>,
    ) -> Result<(/*uid*/ usize, Type), UnifyError> {
        let type_value = match type_value {
            None => Type::ImplicitTypeVariable {
                name: self.get_next_type_variable_name(),
            },
            Some(type_value) => type_value,
        };
        let uid = self.insert_value_symbol(
            variable_name,
            TypeScheme {
                type_variables: vec![],
                type_value: type_value.clone(),
            },
            None,
        )?;
        Ok((uid, type_value))
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
        let uid = self.get_next_symbol_uid();
        self.type_symbols.insert_symbol(
            self.current_scope_name(),
            symbol_name.clone(),
            &self.source,
            SymbolEntry {
                meta: SymbolMeta {
                    uid,
                    name: symbol_name.representation.clone(),
                    scope_name: self.current_scope_name(),
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
        let uid = self.get_next_symbol_uid();
        self.constructor_symbols.insert_symbol(
            self.current_scope_name(),
            token.clone(),
            &self.source,
            SymbolEntry {
                meta: SymbolMeta {
                    uid,
                    name: token.representation.clone(),
                    scope_name: self.current_scope_name(),
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

    /// `expected_type` is required for single dispatch disambiguation
    pub fn get_value_symbol(
        &mut self,
        symbol_name: &Token,
        expected_type: &Option<Type>,
        scope_name: usize,
    ) -> Result<GetValueSymbolResult, UnifyError> {
        let symbol =
            self.value_symbols
                .get_symbol(&self.scope, &self.source, &symbol_name, scope_name);
        match symbol {
            Some(symbol) => Ok(GetValueSymbolResult {
                symbol_uid: symbol.meta.uid,
                type_scheme: symbol.value.type_scheme,
            }),
            None => {
                if self.function_symbols.is_empty() {
                    return Err(UnifyError {
                        position: symbol_name.position,
                        kind: UnifyErrorKind::UnknownValueSymbol,
                    });
                }

                let matching_function_symbols = self
                    .function_symbols
                    .iter()
                    .filter(|symbol| {
                        symbol.meta.name == symbol_name.representation
                            && symbol.meta.scope_name == scope_name
                    })
                    .collect::<Vec<&FunctionSymbol>>();

                match matching_function_symbols.get(0) {
                    Some(function_symbol) => {
                        if matching_function_symbols.len() == 1 {
                            Ok(GetValueSymbolResult {
                                symbol_uid: function_symbol.meta.uid,
                                type_scheme: function_symbol.function_signature.as_type_scheme(),
                            })
                        } else {
                            match expected_type {
                                Some(Type::Function(function_type)) => {
                                    // check if the actual first parameter type is not implicit type variable
                                    if let Type::ImplicitTypeVariable { .. } = self
                                        .apply_subtitution_to_type(
                                            function_type.parameters_types.first(),
                                        )
                                    {
                                        return Err(UnifyError {
                                            position: symbol_name.position,
                                            kind: UnifyErrorKind::AmbiguousFunction {
                                                available_function_signatures:
                                                    matching_function_symbols
                                                        .iter()
                                                        .map(|symbol| {
                                                            symbol.function_signature.clone()
                                                        })
                                                        .collect(),
                                            },
                                        });
                                    }

                                    // find matching function signatures based on concrete type
                                    let matching_function_signature =
                                        matching_function_symbols.iter().find(|signature| {
                                            overlap(
                                                signature
                                                    .function_signature
                                                    .function_type
                                                    .parameters_types
                                                    .first(),
                                                function_type.parameters_types.first(),
                                            )
                                        });

                                    match matching_function_signature {
                                        // If no matching function signature found, search in parent scope
                                        None => {
                                            let parent_scope_name =
                                                self.scope.get_parent_scope_name(scope_name);
                                            match parent_scope_name {
                                                None => Err(UnifyError {
                                                    position: symbol_name.position,
                                                    kind: UnifyErrorKind::UnknownValueSymbol,
                                                }),
                                                Some(parent_scope_name) => self.get_value_symbol(
                                                    symbol_name,
                                                    &Some(Type::Function(function_type.clone())),
                                                    parent_scope_name,
                                                ),
                                            }
                                        }

                                        // If found one matching function signature, return Ok
                                        Some(function_symbol) => Ok(GetValueSymbolResult {
                                            symbol_uid: function_symbol.meta.uid,
                                            type_scheme: function_symbol
                                                .function_signature
                                                .as_type_scheme(),
                                        }),
                                    }
                                }
                                _ => Err(UnifyError {
                                    position: symbol_name.position,
                                    kind: UnifyErrorKind::AmbiguousFunction {
                                        available_function_signatures: matching_function_symbols
                                            .iter()
                                            .map(|symbol| symbol.function_signature.clone())
                                            .collect(),
                                    },
                                }),
                            }
                        }
                    }

                    // If no matching function signatures found in this scope
                    // Look into parent scope
                    None => match self.scope.get_parent_scope_name(scope_name) {
                        None => panic!(
                            "no matching functions found for {}",
                            symbol_name.representation
                        ),
                        Some(scope_name) => {
                            self.get_value_symbol(symbol_name, expected_type, scope_name)
                        }
                    },
                }
            }
        }
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
    type_scheme: TypeScheme,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    meta: SymbolMeta,
    function_signature: FunctionSignature,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub type_variables: Vec<String>,
    pub function_type: FunctionType,
}

impl FunctionSignature {
    fn as_type_scheme(&self) -> TypeScheme {
        TypeScheme {
            type_variables: self.type_variables.clone(),
            type_value: Type::Function(self.function_type.clone()),
        }
    }
}

/// To check whether the given pair of types overlapped
/// This is used to prevent user from overloading a function that is
/// indistinguishable from some existing function
fn overlap(a: &Type, b: &Type) -> bool {
    match (a, b) {
        // Type variables overlap with any type
        // This is because KK does not support specialization
        (Type::ExplicitTypeVariable { .. }, _) | (_, Type::ExplicitTypeVariable { .. }) => true,
        (Type::ImplicitTypeVariable { .. }, _) | (_, Type::ImplicitTypeVariable { .. }) => true,
        (Type::Null, Type::Null) => true,
        (Type::String, Type::String) => true,
        (Type::Boolean, Type::Boolean) => true,
        (Type::Number, Type::Number) => true,
        (Type::Tuple(xs), Type::Tuple(ys)) => {
            xs.len() == ys.len() && xs.clone().zip(*ys.clone()).all(|(a, b)| overlap(a, b))
        }
        (Type::Record { key_type_pairs: a }, Type::Record { key_type_pairs: b }) => {
            let mut a = a.clone();
            let mut b = b.clone();
            a.sort_by(|a, b| a.0.cmp(&b.0));
            b.sort_by(|a, b| a.0.cmp(&b.0));
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|((key_a, type_a), (key_b, type_b))| {
                        key_a == key_b && overlap(type_a, type_b)
                    })
        }
        (Type::Function(a), Type::Function(b)) => {
            a.parameters_types.len() == b.parameters_types.len()
                && (a
                    .parameters_types
                    .clone()
                    .zip(*b.parameters_types.clone())
                    .all(|(a, b)| overlap(a, b))
                    && overlap(a.return_type.as_ref(), b.return_type.as_ref()))
        }
        (Type::Array(a), Type::Array(b)) => overlap(a.as_ref(), b.as_ref()),
        (
            Type::Named {
                name: name_a,
                type_arguments: type_arguments_a,
            },
            Type::Named {
                name: name_b,
                type_arguments: type_arguments_b,
            },
        ) => {
            *name_a == *name_b
                && type_arguments_a
                    .iter()
                    .zip(type_arguments_b.iter())
                    .all(|((_, a), (_, b))| overlap(a, b))
        }

        // otherwise
        _ => false,
    }
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

fn built_in_value_symbols() -> Vec<(String, usize, TypeScheme)> {
    let type_variable_name = "T".to_string();
    vec![(
        "print".to_string(),
        0, // uid
        TypeScheme {
            type_variables: vec![type_variable_name.clone()],
            type_value: Type::Function(FunctionType {
                parameters_types: Box::new(NonEmpty {
                    head: Type::ImplicitTypeVariable {
                        name: type_variable_name,
                    },
                    tail: vec![],
                }),
                return_type: Box::new(Type::Null),
            }),
        },
    )]
}

fn built_in_type_symbols() -> Vec<(String, TypeSymbol)> {
    vec![
        (
            "String".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::String,
                },
            },
        ),
        (
            "Number".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Number,
                },
            },
        ),
        (
            "Null".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Null,
                },
            },
        ),
        (
            "Boolean".to_string(),
            TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Boolean,
                },
            },
        ),
    ]
}
