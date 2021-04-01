use crate::ast::*;
use crate::non_empty::NonEmpty;
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ImportRelation {
    pub importer_path: String,
    pub importee_path: String,
}

/// Represents the meta data of this module.
#[derive(Debug, Clone)]
pub struct ModuleMeta {
    /// A unique identifier that can be used to uniquely identify this module.
    pub uid: ModuleUid,

    /// Represents the literal code of this module.
    pub code: String,

    /// This is used for checking circular references
    pub import_relations: Vec<ImportRelation>,
}

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

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    /// This is needed for single dispatch disambiguation during transpilation  
    /// Also needed for importing symbols
    /// This value should be unique across different modules
    pub uid: usize,

    /// Represents in which scope this symbol is declared.
    /// Needed to allow variable shadowing.
    pub scope_name: usize,

    pub symbol: Symbol,
}

/// This represents the meta data of a symbol
#[derive(Debug, Clone)]
pub struct SymbolMeta {
    pub name: Token,
    pub exported: bool,
}

/// Represent the unique identifier for a module
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum ModuleUid {
    /// For example, `https://raw.githubusercontent.com/foo/bar/v0.0.1/spam.kk`
    Remote { url: String },

    /// Must be relative path, cannot be absolute.  
    /// For example, `./foo/bar/spam.kk`
    Local { relative_path: String },
}

impl ModuleUid {
    pub fn string_value(&self) -> String {
        match self {
            ModuleUid::Remote { url: s } | ModuleUid::Local { relative_path: s } => s.clone(),
        }
    }
}

/// Module also means a single source file.
#[derive(Debug, Clone)]
pub struct Module {
    /// Every symbol in a module will be assigned a unique ID, regardless of the scope.
    current_uid: Cell<usize>,

    /// This represents the current scope. Needed for implementing variable shadowing.
    scope: Scope,

    /// List of symbols in this module
    symbol_entries: Vec<SymbolEntry>,

    /// This is used for Hindley-Milner type inference algorithm
    type_variable_substitutions: Substitution,
    type_variable_index: Cell<usize>,

    /// Represents the metadata of this module
    pub meta: ModuleMeta,
}

#[derive(Debug, Clone)]
enum SubstitutionItem {
    ImplicitTypeVariable(String),
    Type(Type),
    NotSubstituted,
}

impl Module {
    pub fn new(module_meta: ModuleMeta, starting_symbol_uid: usize) -> Module {
        let mut result = Module {
            symbol_entries: Vec::new(),
            type_variable_substitutions: (HashMap::new()),
            type_variable_index: Cell::new(0),
            scope: Scope::new(),
            current_uid: Cell::new(0),
            meta: module_meta,
        };

        let built_in_symbols = built_in_symbols();

        let built_in_symbols_length = built_in_symbols.len();
        built_in_symbols
            .into_iter()
            .map(|symbol| result.insert_symbol(None, symbol))
            .collect::<Result<Vec<usize>, UnifyError>>()
            .expect("Compiler error: built-in symbols should not have conflict");

        result
            .current_uid
            .set(built_in_symbols_length + starting_symbol_uid);

        result
    }

    pub fn get_current_uid(&self) -> usize {
        self.current_uid.get()
    }

    pub fn set_current_uid(&mut self, new_uid: usize) {
        self.current_uid.set(new_uid)
    }

    pub fn uid(&self) -> ModuleUid {
        self.meta.uid.clone()
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
        type_variable_name: &Token,
    ) -> Result<usize, UnifyError> {
        self.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: type_variable_name.clone(),
                    exported: false,
                },
                kind: SymbolKind::Type(TypeSymbol {
                    type_scheme: TypeScheme {
                        type_variables: vec![],
                        type_value: Type::ExplicitTypeVariable {
                            name: type_variable_name.representation.clone(),
                        },
                    },
                }),
            },
        )
    }

    /// Get the next symbol UID
    pub fn get_next_symbol_uid(&mut self) -> usize {
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
            self.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: variable_name.clone(),
                        exported: false,
                    },
                    kind: SymbolKind::Value(ValueSymbol {
                        type_scheme: TypeScheme {
                            type_variables: vec![],
                            type_value: type_value.clone(),
                        },
                    }),
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
            Type::Float => Type::Float,
            Type::Integer => Type::Integer,
            Type::Boolean => Type::Boolean,
            Type::String => Type::String,
            Type::Character => Type::Character,
            Type::Null => Type::Null,
            Type::ExplicitTypeVariable { name } => {
                Type::ExplicitTypeVariable { name: name.clone() }
            }
            Type::BuiltInOneArgumentType {
                kind,
                type_argument,
            } => Type::BuiltInOneArgumentType {
                kind: kind.clone(),
                type_argument: Box::new(self.apply_subtitution_to_type(type_argument.as_ref())),
            },
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
                symbol_uid,
                name,
                type_arguments,
            } => Type::Named {
                symbol_uid: *symbol_uid,
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

    /// Insert a value symbol with a specific type
    /// If the type is None, then this variable will be
    /// instantiated to have a type of a new type variable
    pub fn insert_value_symbol_with_type(
        &mut self,
        variable_name: &Token,
        type_value: Option<Type>,
        exported: bool,
    ) -> Result<(/*uid*/ usize, Type), UnifyError> {
        let type_value = match type_value {
            None => Type::ImplicitTypeVariable {
                name: self.get_next_type_variable_name(),
            },
            Some(type_value) => type_value,
        };
        let uid = self.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: variable_name.clone(),
                    exported,
                },
                kind: SymbolKind::Value(ValueSymbol {
                    type_scheme: TypeScheme {
                        type_variables: vec![],
                        type_value: type_value.clone(),
                    },
                }),
            },
        )?;
        Ok((uid, type_value))
    }

    pub fn check_for_unused_symbols(&self, current_scope_name: usize) -> Result<(), UnifyError> {
        // panic!("Not implemented yet")
        Ok(())
    }

    /// Inserts a new symbol into this module.  
    /// `uid` should be `None` under normal circumstances.
    ///     It should only be defined when for example we want to allow recursive definition.
    ///
    /// Returns the UID of the new_symbol upon successful insertion.
    pub fn insert_symbol(
        &mut self,
        uid: Option<usize>,
        new_symbol: Symbol,
    ) -> Result<usize, UnifyError> {
        let name = new_symbol.meta.name.representation.clone();
        let scope_name = self.current_scope_name();
        match &new_symbol.kind {
            SymbolKind::Type(_) => {
                if let Some(conflicting_entry) = self.symbol_entries.iter().find(|entry| {
                    entry.scope_name == scope_name
                        && entry.symbol.meta.name.representation == name
                        && matches!(entry.symbol.kind, SymbolKind::Type { .. })
                }) {
                    return Err(UnifyError {
                        position: new_symbol.meta.name.position,
                        kind: UnifyErrorKind::DuplicatedIdentifier {
                            name: conflicting_entry.symbol.meta.name.representation.clone(),
                            first_declared_at: conflicting_entry.symbol.meta.name.position,
                            then_declared_at: new_symbol.meta.name.position,
                        },
                    });
                }
            }
            SymbolKind::EnumConstructor(new_enum_constructor_symbol) => {
                if let Some(conflicting_entry) =
                    self.symbol_entries
                        .iter()
                        .find(|entry| match &entry.symbol.kind {
                            SymbolKind::EnumConstructor(EnumConstructorSymbol {
                                enum_name,
                                constructor_name,
                                ..
                            }) => {
                                *enum_name == new_enum_constructor_symbol.enum_name
                                    && *constructor_name
                                        == new_enum_constructor_symbol.constructor_name
                            }
                            _ => false,
                        })
                {
                    return Err(UnifyError {
                        position: new_symbol.meta.name.position,
                        kind: UnifyErrorKind::DuplicatedIdentifier {
                            name: conflicting_entry.symbol.meta.name.representation.clone(),
                            first_declared_at: conflicting_entry.symbol.meta.name.position,
                            then_declared_at: new_symbol.meta.name.position,
                        },
                    });
                }
            }
            SymbolKind::Value(new_value_symbol) => {
                self.symbol_entries
                    .iter()
                    .map(|entry| {
                        if entry.scope_name == scope_name && entry.symbol.meta.name.representation == name {
                            let error = Err(UnifyError {
                                position: new_symbol.meta.name.position,
                                kind: UnifyErrorKind::DuplicatedIdentifier {
                                    name: entry.symbol.meta.name.representation.clone(),
                                    first_declared_at: entry.symbol.meta.name.position,
                                    then_declared_at: new_symbol.meta.name.position,
                                },
                            });
                            match &entry.symbol.kind {
                                SymbolKind::EnumConstructor(constructor)
                                    if constructor.constructor_name == new_symbol.meta.name.representation => {
                                    error
                                }
                                SymbolKind::Value(value_symbol) => {
                                    match &value_symbol.type_scheme.type_value {
                                        Type::Function(function_type) => {
                                            match &new_value_symbol.type_scheme.type_value {
                                                Type::Function(new_function_type) => {
                                                    if overlap(
                                                        function_type.parameters_types.first(),
                                                        new_function_type.parameters_types.first(),
                                                    ) {
                                                        Err(UnifyError {
                                                            position: new_symbol.meta.name.position,
                                                            kind: UnifyErrorKind::ConflictingFunctionDefinition {
                                                                function_name: new_symbol.meta.name.representation.clone(),
                                                                existing_first_parameter_type: function_type.parameters_types.first().clone(),
                                                                new_first_parameter_type: new_function_type.parameters_types.first().clone(),
                                                                first_declared_at: entry.symbol.meta.name.position,
                                                            }
                                                        })
                                                    }
                                                    else {
                                                        Ok(())
                                                    }
                                                }

                                                // if the new value symbol to be inserted is not type of function
                                                _ => error,
                                            }
                                        }

                                        // If current entry is not type of function
                                        _ => error,
                                    }
                                }
                                _ => Ok(()),
                            }
                        } else {
                            Ok(())
                        }
                    })
                    .collect::<Result<Vec<()>, UnifyError>>()?;
            }
        };

        let uid = uid.unwrap_or_else(|| self.get_next_symbol_uid());
        self.symbol_entries.push(SymbolEntry {
            uid,
            scope_name: self.current_scope_name(),
            symbol: new_symbol,
        });
        Ok(uid)
    }

    pub fn get_all_matching_symbols(&self, symbol_name: &Token) -> Vec<SymbolEntry> {
        self.symbol_entries
            .iter()
            .flat_map(|entry| {
                if entry.symbol.meta.name.representation == symbol_name.representation {
                    match &entry.symbol.kind {
                        // Enum constructor cannot be imported,
                        // as they will be brought into scope by their enum name
                        SymbolKind::EnumConstructor(_) => vec![],
                        SymbolKind::Type(type_symbol) => {
                            match type_symbol.type_scheme.type_value {
                                // When importing enum type
                                // it's constructor will be brought into scope as well
                                Type::Named { symbol_uid, .. } => vec![entry.clone()]
                                    .into_iter()
                                    .chain(self.symbol_entries.iter().filter_map(|entry| {
                                        match &entry.symbol.kind {
                                            SymbolKind::EnumConstructor(
                                                enum_constructor_symbol,
                                            ) if enum_constructor_symbol.enum_uid == symbol_uid => {
                                                Some(entry.clone())
                                            }
                                            _ => None,
                                        }
                                    }))
                                    .collect(),
                                _ => vec![entry.clone()],
                            }
                        }
                        _ => vec![entry.clone()],
                    }
                } else {
                    vec![]
                }
            })
            .collect()
    }

    pub fn get_enum_constructors(&self, enum_uid: usize) -> Vec<EnumConstructorSymbol> {
        self.symbol_entries
            .iter()
            .filter_map(|entry| match &entry.symbol.kind {
                SymbolKind::EnumConstructor(enum_constructor)
                    if enum_constructor.enum_uid == enum_uid =>
                {
                    Some(enum_constructor.clone())
                }
                _ => None,
            })
            .collect()
    }

    fn get_symbol_entry(&self, symbol_name: &Token, scope_name: usize) -> Option<SymbolEntry> {
        match self.symbol_entries.iter().find(|entry| {
            entry.symbol.meta.name.representation == symbol_name.representation
                && entry.scope_name == scope_name
        }) {
            Some(entry) => Some(entry.clone()),
            None => match self.scope.get_parent_scope_name(scope_name) {
                None => None,
                Some(scope_name) => self.get_symbol_entry(symbol_name, scope_name),
            },
        }
    }

    pub fn get_type_symbol_by_name(&self, symbol_name: &Token) -> Option<TypeSymbol> {
        match self.get_symbol_entry(symbol_name, self.current_scope_name()) {
            None => None,
            Some(entry) => match entry.symbol.kind {
                SymbolKind::Type(type_symbol) => Some(type_symbol),
                _ => None,
            },
        }
    }

    pub fn get_type_symbol_by_uid(&self, symbol_uid: usize) -> Option<TypeSymbol> {
        self.symbol_entries
            .iter()
            .filter_map(|entry| {
                if entry.uid == symbol_uid {
                    match &entry.symbol.kind {
                        SymbolKind::Type(type_symbol) => Some(type_symbol.clone()),
                        _ => panic!("Compiler error, {:?}", entry.symbol.meta),
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<TypeSymbol>>()
            .first()
            .cloned()
    }

    /// `expected_type` is required for single dispatch disambiguation
    pub fn get_value_symbol(
        &mut self,
        symbol_name: &Token,
        expected_type: &Option<Type>,
        scope_name: usize,
    ) -> Result<GetValueSymbolResult, UnifyError> {
        let matching_value_symbols = self
            .symbol_entries
            .iter()
            .filter_map(|entry| match &entry.symbol.kind {
                SymbolKind::Value(value_symbol)
                    if entry.scope_name == scope_name
                        && entry.symbol.meta.name.representation == symbol_name.representation =>
                {
                    Some((entry.uid, value_symbol.clone()))
                }
                _ => None,
            })
            .collect::<Vec<(usize, ValueSymbol)>>();
        match matching_value_symbols.split_first() {
            None => match self.scope.get_parent_scope_name(scope_name) {
                None => Err(UnifyError {
                    position: symbol_name.position,
                    kind: UnifyErrorKind::UnknownValueSymbol,
                }),
                Some(scope_name) => self.get_value_symbol(symbol_name, expected_type, scope_name),
            },
            Some((head, tail)) => {
                if tail.is_empty() {
                    Ok(GetValueSymbolResult {
                        symbol_uid: head.0,
                        type_scheme: head.1.type_scheme.clone(),
                    })
                } else {
                    let matching_function_signatures = matching_value_symbols
                        .iter()
                        .filter_map(
                            |(symbol_uid, symbol)| match &symbol.type_scheme.type_value {
                                Type::Function(function_type) => Some(FunctionSignature {
                                    symbol_uid: *symbol_uid,
                                    type_variables: symbol.type_scheme.type_variables.clone(),
                                    function_type: function_type.clone(),
                                }),
                                _ => None,
                            },
                        )
                        .collect::<Vec<FunctionSignature>>();

                    match expected_type {
                        Some(Type::Function(expected_function_type)) => {
                            // check if the actual first parameter type is not implicit type variable
                            if let Type::ImplicitTypeVariable { .. } = self
                                .apply_subtitution_to_type(
                                    expected_function_type.parameters_types.first(),
                                )
                            {
                                return Err(UnifyError {
                                    position: symbol_name.position,
                                    kind: UnifyErrorKind::AmbiguousFunction {
                                        available_function_signatures: matching_function_signatures,
                                    },
                                });
                            }

                            // find matching function signatures based on concrete type
                            let matching_function_signature =
                                matching_function_signatures.iter().find(|signature| {
                                    overlap(
                                        signature.function_type.parameters_types.first(),
                                        expected_function_type.parameters_types.first(),
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
                                            kind: UnifyErrorKind::NoMatchingFunction {
                                                actual_first_argument_type: expected_function_type
                                                    .parameters_types
                                                    .first()
                                                    .clone(),
                                                expected_first_argument_types:
                                                    matching_function_signatures
                                                        .into_iter()
                                                        .map(|signature| {
                                                            signature
                                                                .function_type
                                                                .parameters_types
                                                                .first()
                                                                .clone()
                                                        })
                                                        .collect(),
                                            },
                                        }),
                                        Some(parent_scope_name) => self.get_value_symbol(
                                            symbol_name,
                                            &Some(Type::Function(expected_function_type.clone())),
                                            parent_scope_name,
                                        ),
                                    }
                                }

                                // If found one matching function signature, return Ok
                                Some(signature) => Ok(GetValueSymbolResult {
                                    symbol_uid: signature.symbol_uid,
                                    type_scheme: signature.as_type_scheme(),
                                }),
                            }
                        }
                        _ => Err(UnifyError {
                            position: symbol_name.position,
                            kind: UnifyErrorKind::AmbiguousFunction {
                                available_function_signatures: matching_function_signatures,
                            },
                        }),
                    }
                }
            }
        }
    }

    pub fn matches_some_enum_constructor(&self, name: &str) -> bool {
        self.symbol_entries
            .iter()
            .any(|entry| match &entry.symbol.kind {
                SymbolKind::EnumConstructor(enum_constructor_symbol)
                    if enum_constructor_symbol.constructor_name == *name =>
                {
                    true
                }
                _ => false,
            })
    }

    /// `expected_enum_name` -
    ///     This is for disambiguating constructors with the same name that belongs to different enums
    pub fn get_constructor_symbol(
        &self,
        expected_enum_uid: Option<usize>,
        constructor_name: &Token,
    ) -> Result<EnumConstructorSymbol, UnifyError> {
        let matching_constructors = self
            .symbol_entries
            .iter()
            .filter_map(|constructor| match &constructor.symbol.kind {
                SymbolKind::EnumConstructor(constructor_symbol)
                    if constructor_symbol.constructor_name == constructor_name.representation =>
                {
                    Some(constructor_symbol.clone())
                }
                _ => None,
            })
            .collect::<Vec<EnumConstructorSymbol>>();

        match matching_constructors.split_first() {
            None => Err(UnifyError {
                position: constructor_name.position,
                kind: UnifyErrorKind::UnknownEnumConstructor,
            }),
            Some((constructor, tail)) => {
                if tail.is_empty() {
                    Ok(EnumConstructorSymbol::clone(constructor))
                } else {
                    // If more than one matching constructors found
                    // Need to disambiguate using expected_enum_name
                    let error = Err(UnifyError {
                        position: constructor_name.position,
                        kind: UnifyErrorKind::AmbiguousConstructorUsage {
                            constructor_name: constructor_name.representation.clone(),
                            possible_enum_names: NonEmpty {
                                head: constructor.enum_name.clone(),
                                tail: tail
                                    .iter()
                                    .map(|constructor| constructor.enum_name.clone())
                                    .collect(),
                            },
                        },
                    });
                    match expected_enum_uid {
                        None => error,
                        Some(expected_enum_uid) => match matching_constructors
                            .into_iter()
                            .find(|constructor| constructor.enum_uid == expected_enum_uid)
                        {
                            Some(constructor_symbol) => Ok(constructor_symbol),
                            None => error,
                        },
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub meta: SymbolMeta,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Value(ValueSymbol),
    Type(TypeSymbol),
    EnumConstructor(EnumConstructorSymbol),
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
pub struct FunctionSymbol {
    pub meta: SymbolMeta,
    pub function_signature: FunctionSignature,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub symbol_uid: usize,
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
        (Type::Integer, Type::Integer) => true,
        (Type::Float, Type::Float) => true,
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
            if expected_kind.ne(&actual_kind) {
                false
            } else {
                overlap(
                    expected_type_argument.as_ref(),
                    actual_type_argument.as_ref(),
                )
            }
        }
        (
            Type::Named {
                symbol_uid: symbol_uid_a,
                type_arguments: type_arguments_a,
                ..
            },
            Type::Named {
                symbol_uid: symbol_uid_b,
                type_arguments: type_arguments_b,
                ..
            },
        ) => {
            *symbol_uid_a == *symbol_uid_b
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
pub struct EnumConstructorSymbol {
    /// Refers to the UID of the enum that contains this constructor
    pub enum_uid: usize,
    pub enum_name: String,
    pub constructor_name: String,
    pub type_variables: Vec<String>,
    pub payload: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UsageReference {
    position: Position,
}

fn built_in_symbols() -> Vec<Symbol> {
    fn meta(name: String) -> SymbolMeta {
        SymbolMeta {
            exported: false,
            name: Token {
                position: Position::dummy(),
                representation: name,
                token_type: TokenType::Identifier,
            },
        }
    }
    let type_variable_name = "T".to_string();
    vec![
        // build-in values
        Symbol {
            meta: meta("print".to_string()),
            kind: SymbolKind::Value(ValueSymbol {
                type_scheme: TypeScheme {
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
            }),
        },
        // built-in types
        Symbol {
            meta: meta("String".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::String,
                },
            }),
        },
        Symbol {
            meta: meta("Character".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Character,
                },
            }),
        },
        Symbol {
            meta: meta("Integer".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Integer,
                },
            }),
        },
        Symbol {
            meta: meta("Float".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Float,
                },
            }),
        },
        Symbol {
            meta: meta("Null".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Null,
                },
            }),
        },
        Symbol {
            meta: meta("Boolean".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Boolean,
                },
            }),
        },
    ]
}
