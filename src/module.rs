use crate::non_empty::NonEmpty;
use crate::stringify_error::stringify_type;
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use crate::{raw_ast::*, typ::*};
use std::cell::Cell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

type CanonicalizedPath = String;
#[derive(Debug, Clone)]
pub struct ImportRelation {
    pub importer_path: CanonicalizedPath,
    pub importee_path: CanonicalizedPath,
}

/// Represents the meta data of this module.
#[derive(Debug, Clone)]
pub struct ModuleMeta {
    /// A unique identifier that can be used to uniquely identify this module.
    pub uid: ModuleUid,

    /// This is used for checking circular references
    pub import_relations: Vec<ImportRelation>,
}

pub type Substitution = HashMap<String, SubstitutionItem>;

pub struct GetValueSymbolResult {
    pub symbol_uid: SymbolUid,
    pub type_value: Type,
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
    pub uid: SymbolUid,

    /// Represents in which scope this symbol is declared.
    /// Needed to allow variable shadowing.
    pub scope_name: usize,

    pub symbol: Symbol,
}

/// This value should be unique across modules
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolUid {
    /// This value is only unique within each module
    pub index: usize,

    /// This is needed for uniquely identify enum type across different modules
    pub module_uid: ModuleUid,
}

/// This represents the meta data of a symbol
#[derive(Debug, Clone)]
pub struct SymbolMeta {
    pub name: Token,
    pub access: Access,
}

#[derive(Debug, Clone)]
pub enum Access {
    /// Can be imported anywhere (even via remote URL)
    Public { keyword_public: Token },
    /// Can be imported via relative path
    Exported { keyword_export: Token },
    /// Default, can be use within the same module (folder)
    Protected,
}

/// Represent the unique identifier for a module
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum ModuleUid {
    /// For example, `https://raw.githubusercontent.com/foo/bar/v0.0.1/spam.kk`
    Remote { url: String },

    /// This should be a folder name, not a file name.
    /// Also it should be relative, not absolute (so that error message will not be horrendously
    /// long)
    /// For example, `./foo/bar`
    Local { folder_relative_path: String },
}

impl ModuleUid {
    pub fn string_value(&self) -> String {
        match self {
            ModuleUid::Remote { url: s }
            | ModuleUid::Local {
                folder_relative_path: s,
            } => s.clone(),
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
    ImplicitTypeVariable(ImplicitTypeVariable),
    Type(Type),
    NotSubstituted,
}

impl Module {
    pub fn new(module_meta: ModuleMeta) -> Module {
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
            .collect::<Result<Vec<SymbolUid>, UnifyError>>()
            .expect("Compiler error: built-in symbols should not have conflict");

        result.current_uid.set(built_in_symbols_length);

        result
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

    /// Insert a rigid type variable into the current scope
    pub fn insert_explicit_type_variable(
        &mut self,
        type_variable_name: &Token,
    ) -> Result<SymbolUid, UnifyError> {
        self.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: type_variable_name.clone(),
                    access: Access::Protected,
                },
                kind: SymbolKind::Type(TypeSymbol {
                    type_value: Type::ExplicitTypeVariable(ExplicitTypeVariable {
                        name: type_variable_name.representation.clone(),
                    }),
                }),
            },
        )
    }

    /// Get the next symbol UID.  
    /// This will mutate the `current_uid` of this module.
    pub fn get_next_symbol_uid(&mut self) -> SymbolUid {
        let result = self.current_uid.get();
        self.current_uid.set(result + 1);
        SymbolUid {
            index: result,
            module_uid: self.uid(),
        }
    }

    pub fn introduce_implicit_type_variable(
        &mut self,
        variable_name: Option<&Token>,
    ) -> Result<Type, UnifyError> {
        let new_type_variable_name = self.get_next_type_variable_name();
        let type_value = Type::ImplicitTypeVariable(ImplicitTypeVariable {
            name: new_type_variable_name.clone(),
        });

        if let Some(variable_name) = variable_name {
            self.insert_symbol(
                None,
                Symbol {
                    meta: SymbolMeta {
                        name: variable_name.clone(),
                        access: Access::Protected,
                    },
                    kind: SymbolKind::Value(ValueSymbol {
                        type_value: type_value.clone(),
                    }),
                },
            )?;
        }

        self.type_variable_substitutions
            .insert(new_type_variable_name, SubstitutionItem::NotSubstituted);

        Ok(type_value)
    }

    /// Try to substitute the given `type_variable` with the given `type_value`.  
    ///
    /// Will fail if `type_variable` was already substituted with another type, say `T2`,
    /// and `T2` is not unifiable with `type_value`.
    pub fn update_substitution(
        &mut self,
        type_variable: ImplicitTypeVariable,
        type_value: Type,
        position: Position,
    ) -> Result<(), UnifyError> {
        match self.get_type_variable_terminal_type(type_variable.name.clone()) {
            Some(terminal_type) => match type_value {
                Type::ImplicitTypeVariable(type_variable) => {
                    self.update_substitution(type_variable, terminal_type, position)
                }
                _ => {
                    let unified_type = unify_type(self, &terminal_type, &type_value, position)?;

                    let substitution_item = match unified_type {
                        Type::ImplicitTypeVariable(type_variable) => {
                            SubstitutionItem::ImplicitTypeVariable(type_variable)
                        }
                        other => SubstitutionItem::Type(other),
                    };

                    self.type_variable_substitutions
                        .insert(type_variable.name, substitution_item);
                    Ok(())
                }
            },
            None => {
                let substitution_item = match type_value {
                    Type::ImplicitTypeVariable(type_variable) => {
                        Ok(SubstitutionItem::ImplicitTypeVariable(type_variable))
                    }
                    other => Ok(SubstitutionItem::Type(other)),
                }?;
                self.type_variable_substitutions
                    .insert(type_variable.name, substitution_item);
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
            Type::String => Type::String,
            Type::Character => Type::Character,
            Type::Unit => Type::Unit,
            Type::Keyword(identifier) => Type::Keyword(identifier.clone()),
            Type::ExplicitTypeVariable(type_variable) => {
                Type::ExplicitTypeVariable(type_variable.clone())
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
            Type::ImplicitTypeVariable(type_variable) => {
                match self.get_type_variable_terminal_type(type_variable.name.clone()) {
                    Some(type_value) => self.apply_subtitution_to_type(&type_value),
                    None => Type::ImplicitTypeVariable(type_variable.clone()),
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
                symbol_uid: symbol_uid.clone(),
                name: name.clone(),
                type_arguments: type_arguments
                    .iter()
                    .map(|(key, type_parameter)| {
                        (key.clone(), self.apply_subtitution_to_type(&type_parameter))
                    })
                    .collect(),
            },
            Type::TypeScheme(type_scheme) => Type::TypeScheme(Box::new(TypeScheme {
                type_variables: type_scheme.type_variables.clone(),
                type_value: self.apply_subtitution_to_type(&type_scheme.type_value),
            })),
        }
    }

    pub fn apply_subtitution_to_function_type(
        &self,
        FunctionType {
            parameter_type,
            return_type,
            type_constraints,
        }: &FunctionType,
    ) -> FunctionType {
        FunctionType {
            parameter_type: Box::new(self.apply_subtitution_to_type(&parameter_type)),
            return_type: Box::new(self.apply_subtitution_to_type(return_type.as_ref())),
            type_constraints: type_constraints.to_vec(),
        }
    }

    /// Find the terminal type substitution for the given type variable.
    ///
    /// For example, suppose we have the following substitutions (where lowercase letter represents type variable):
    ///
    ///     a -> b
    ///     b -> c
    ///     c -> Integer
    ///     d -> e
    ///
    /// Then, the terminal type for type variable `a` is `Integer`.  
    /// And the terminal type of type variable `d` is type variable `e`.
    pub fn get_type_variable_terminal_type(&self, type_variable_name: String) -> Option<Type> {
        self.get_type_variable_terminal_type_(
            type_variable_name.clone(),
            type_variable_name.clone(),
            type_variable_name,
        )
    }

    /// Terminal means non-type-variable type
    ///
    /// `initial_type_variable_name`:  
    ///     This variable is needed for breaking infinite recursion (i.e. circular reference)
    ///
    /// `previous_type_variable_name`:  
    ///     This variable is needed for breaking infinite recursion (i.e. circular reference)
    fn get_type_variable_terminal_type_(
        &self,
        initial_type_variable_name: String,
        previous_type_variable_name: String,
        type_variable_name: String,
    ) -> Option<Type> {
        match self.type_variable_substitutions.get(&type_variable_name) {
            Some(SubstitutionItem::Type(type_value)) => Some(type_value.clone()),
            Some(SubstitutionItem::ImplicitTypeVariable(type_variable)) => {
                if *type_variable.name == initial_type_variable_name
                    || *type_variable.name == previous_type_variable_name
                {
                    None
                } else {
                    match self.get_type_variable_terminal_type_(
                        initial_type_variable_name,
                        type_variable.name.clone(),
                        type_variable.name.clone(),
                    ) {
                        Some(type_value) => Some(type_value),
                        None => Some(Type::ImplicitTypeVariable(type_variable.clone())),
                    }
                }
            }
            Some(SubstitutionItem::NotSubstituted) => None,
            None => None,
        }
    }

    /// Run a function in a new child scope.
    /// When the function ends, the scope will be returned to the original scope
    pub fn run_in_new_child_scope<F, A, E>(&mut self, mut f: F) -> Result<A, E>
    where
        F: FnMut(&mut Self) -> Result<A, E>,
    {
        self.step_into_new_child_scope();
        let result = f(self);
        self.step_out_to_parent_scope();
        result
    }

    fn step_into_new_child_scope(&mut self) {
        self.scope.step_into_new_child_scope()
    }

    fn step_out_to_parent_scope(&mut self) {
        self.scope.step_out_to_parent_scope()
    }

    /// Insert a value symbol with a specific type
    /// If the type is None, then this variable will be
    /// instantiated to have a type of a new type variable
    pub fn insert_value_symbol_with_type(
        &mut self,
        variable_name: &Token,
        type_value: Option<Type>,
        access: Access,
        is_implicit_variable: bool,
    ) -> Result<(SymbolUid, Type), UnifyError> {
        let type_value = match type_value {
            None => Type::ImplicitTypeVariable(ImplicitTypeVariable {
                name: self.get_next_type_variable_name(),
            }),
            Some(type_value) => type_value,
        };
        let uid = self.insert_symbol(
            None,
            Symbol {
                meta: SymbolMeta {
                    name: variable_name.clone(),
                    access,
                },
                kind: SymbolKind::Value(ValueSymbol {
                    type_value: type_value.clone(),
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
        uid: Option<SymbolUid>,
        new_symbol: Symbol,
    ) -> Result<SymbolUid, UnifyError> {
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
                        if entry.scope_name == scope_name
                            && entry.symbol.meta.name.representation == name
                        {
                            match &entry.symbol.kind {
                                SymbolKind::Value(existing_value_symbol) => {
                                    if overlap(
                                        &existing_value_symbol.type_value,
                                        &new_value_symbol.type_value,
                                        true,
                                    ) {
                                        Err(UnifyError {
                                            position: new_symbol.meta.name.position,
                                            kind: UnifyErrorKind::CannotBeOverloaded {
                                                name: new_symbol.meta.name.representation.clone(),
                                            },
                                        })
                                    } else {
                                        Ok(())
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
            uid: uid.clone(),
            scope_name: self.current_scope_name(),
            symbol: new_symbol,
        });
        Ok(uid)
    }

    pub fn get_all_exported_symbols(&self) -> Vec<SymbolEntry> {
        self.symbol_entries
            .iter()
            .filter_map(|entry| match entry.symbol.meta.access {
                Access::Exported { .. } => Some(entry.clone()),
                _ => None,
            })
            .collect()
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
                            match &type_symbol.type_value {
                                // When importing enum type
                                // it's constructor will be brought into scope as well
                                Type::Named { symbol_uid, .. } => vec![entry.clone()]
                                    .into_iter()
                                    .chain(self.symbol_entries.iter().filter_map(|entry| {
                                        match &entry.symbol.kind {
                                            SymbolKind::EnumConstructor(
                                                enum_constructor_symbol,
                                            ) if enum_constructor_symbol.enum_uid
                                                == *symbol_uid =>
                                            {
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

    pub fn get_enum_constructors(&self, enum_uid: SymbolUid) -> Vec<EnumConstructorSymbol> {
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
        self.get_symbol_entry_by(
            |entry| entry.symbol.meta.name.representation == symbol_name.representation,
            scope_name,
        )
    }

    fn get_symbol_entry_by<P>(&self, predicate: P, scope_name: usize) -> Option<SymbolEntry>
    where
        P: Fn(&SymbolEntry) -> bool,
    {
        match self
            .symbol_entries
            .iter()
            .find(|entry| predicate(entry) && entry.scope_name == scope_name)
        {
            Some(entry) => Some(entry.clone()),
            None => match self.scope.get_parent_scope_name(scope_name) {
                None => None,
                Some(scope_name) => self.get_symbol_entry_by(predicate, scope_name),
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

    pub fn get_type_symbol_by_uid(&self, symbol_uid: &SymbolUid) -> Option<TypeSymbol> {
        self.symbol_entries
            .iter()
            .filter_map(|entry| {
                if entry.uid == *symbol_uid {
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

    fn get_value_symbol_based_on_expected_type(
        &self,
        symbol_name: &Token,
        expected_type: &Type,
        scope_name: usize,
    ) -> Result<Option<GetValueSymbolResult>, UnifyError> {
        // Firstly, search for value symbols based on name and expected type
        let matching_symbols = self
            .symbol_entries
            .iter()
            .filter_map(|entry| match &entry.symbol.kind {
                SymbolKind::Value(value_symbol)
                    if entry.scope_name == scope_name
                        && entry.symbol.meta.name.representation == symbol_name.representation
                        && overlap(&value_symbol.type_value, expected_type, false) =>
                {
                    Some((entry.uid.clone(), value_symbol.clone()))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        match matching_symbols.split_first() {
            Some((matching_symbol, [])) => Ok(Some(GetValueSymbolResult {
                symbol_uid: matching_symbol.0.clone(),
                type_value: matching_symbol.1.type_value.clone(),
            })),
            Some(_) => Err(UnifyError {
                position: symbol_name.position,
                kind: UnifyErrorKind::AmbiguousSymbol {
                    matching_value_symbols: matching_symbols,
                },
            }),

            None => match self.scope.get_parent_scope_name(scope_name) {
                None => Ok(None),
                Some(parent_scope_name) => self.get_value_symbol_based_on_expected_type(
                    symbol_name,
                    expected_type,
                    parent_scope_name,
                ),
            },
        }
    }

    /// `expected_type` is required for single dispatch disambiguation
    pub fn get_value_symbol(
        &self,
        symbol_name: &Token,
        expected_type: &Option<Type>,
        scope_name: usize,
    ) -> Result<GetValueSymbolResult, UnifyError> {
        // Firstly, search for value symbols based on expected type and name
        let matching_symbol = match expected_type {
            Some(expected_type) => self.get_value_symbol_based_on_expected_type(
                symbol_name,
                &self.apply_subtitution_to_type(expected_type),
                scope_name,
            )?,
            None => None,
        };

        match matching_symbol {
            Some(matching_symbol) => return Ok(matching_symbol),
            None => {
                // Do nothing
            }
        }

        // If no matching symbol with expected type and name, then search for value symbols based on name only
        let value_symbols_with_matching_name = self
            .symbol_entries
            .iter()
            .filter_map(|entry| match &entry.symbol.kind {
                SymbolKind::Value(value_symbol)
                    if entry.scope_name == scope_name
                        && entry.symbol.meta.name.representation == symbol_name.representation =>
                {
                    Some((entry.uid.clone(), value_symbol.clone()))
                }
                _ => None,
            })
            .collect::<Vec<(SymbolUid, ValueSymbol)>>();

        match value_symbols_with_matching_name.split_first() {
            // If no symbols with matching name found, look in parent scope
            None => match self.scope.get_parent_scope_name(scope_name) {
                None => Err(UnifyError {
                    position: symbol_name.position,
                    kind: UnifyErrorKind::UnknownValueSymbol {
                        symbol_name: symbol_name.representation.clone(),
                    },
                }),
                Some(scope_name) => self.get_value_symbol(symbol_name, expected_type, scope_name),
            },

            // If there's only one value symbol with matching name in this scope,
            // then return the value.
            //
            // Even though it might not match the `expected_type`.
            //
            // This is so that we can get `function argument type` error, instead of `unknown symbol`,
            // for function that are not overloaded.
            Some((head, [])) => Ok(GetValueSymbolResult {
                symbol_uid: head.0.clone(),
                type_value: head.1.type_value.clone(),
            }),

            // Otherwise, the symbol is ambiguous
            Some(_) => Err(UnifyError {
                position: symbol_name.position,
                kind: UnifyErrorKind::AmbiguousSymbol {
                    matching_value_symbols: value_symbols_with_matching_name,
                },
            }),
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

    /// `constructor_name` -
    ///     This is for disambiguating constructors with the same name that belongs to different enums
    pub fn get_constructor_symbol(
        &self,
        expected_enum_uid: Option<SymbolUid>,
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

    pub fn insert_keyword(&mut self, keyword: &Token) {
        if !self
            .symbol_entries
            .iter()
            .any(|entry| match &entry.symbol.kind {
                SymbolKind::Value(value) => match &value.type_value {
                    Type::Keyword(name) if name.eq(&keyword.representation) => true,
                    _ => false,
                },
                _ => false,
            })
        {
            let uid = self.get_next_symbol_uid();
            self.symbol_entries.push(SymbolEntry {
                uid,
                scope_name: self.current_scope_name(),
                symbol: Symbol {
                    meta: SymbolMeta {
                        name: keyword.clone(),
                        access: Access::Protected,
                    },
                    kind: SymbolKind::Value(ValueSymbol {
                        type_value: Type::Keyword(keyword.representation.clone()),
                    }),
                },
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    /// Meta holds value such as name of this symbol
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
    pub type_value: Type,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    pub type_value: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub meta: SymbolMeta,
    pub function_signature: FunctionSignature,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub symbol_uid: SymbolUid,
    pub type_variables: Option<NonEmpty<ExplicitTypeVariable>>,
    pub function_type: FunctionType,
}

impl FunctionSignature {
    fn as_type_value(&self) -> Type {
        match &self.type_variables {
            Some(type_variables) => Type::TypeScheme(Box::new(TypeScheme {
                type_variables: type_variables.clone(),
                type_value: Type::Function(self.function_type.clone()),
            })),
            None => Type::Function(self.function_type.clone()),
        }
    }
}

/// To check whether the given pair of types overlapped
/// This is used to prevent user from overloading a function that is
/// indistinguishable from some existing function.
///
/// Note: only bounded explicit type variables overlap with any type,
///       in other words, free explicit type variables do not overlap with any type.
fn overlap(a: &Type, b: &Type, explicit_type_variable_overlaps_with_any_type: bool) -> bool {
    match (a, b) {
        (Type::ExplicitTypeVariable(explicit_type_variable), other_type)
        | (other_type, Type::ExplicitTypeVariable(explicit_type_variable)) => {
            // Since KK does not support specialization,
            if explicit_type_variable_overlaps_with_any_type {
                true
            } else {
                match other_type {
                    Type::ExplicitTypeVariable(other_type_variable) => {
                        explicit_type_variable.name.eq(&other_type_variable.name)
                    }
                    Type::ImplicitTypeVariable(_) => true,
                    _ => false,
                }
            }
        }

        (Type::ImplicitTypeVariable { .. }, _) | (_, Type::ImplicitTypeVariable { .. }) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::String, Type::String) => true,
        (Type::Character, Type::Character) => true,
        (Type::Integer, Type::Integer) => true,
        (Type::Float, Type::Float) => true,
        (Type::Keyword(a), Type::Keyword(b)) => a == b,
        (Type::Tuple(xs), Type::Tuple(ys)) => {
            xs.len() == ys.len()
                && xs
                    .clone()
                    .zip(*ys.clone())
                    .all(|(a, b)| overlap(a, b, explicit_type_variable_overlaps_with_any_type))
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
                        key_a == key_b
                            && overlap(
                                type_a,
                                type_b,
                                explicit_type_variable_overlaps_with_any_type,
                            )
                    })
        }
        (Type::Function(a), Type::Function(b)) => {
            overlap(
                &a.parameter_type,
                &b.parameter_type,
                explicit_type_variable_overlaps_with_any_type,
            ) && overlap(
                a.return_type.as_ref(),
                b.return_type.as_ref(),
                explicit_type_variable_overlaps_with_any_type,
            )
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
                    explicit_type_variable_overlaps_with_any_type,
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
                    .all(|((_, a), (_, b))| {
                        overlap(a, b, explicit_type_variable_overlaps_with_any_type)
                    })
        }
        (Type::TypeScheme(type_scheme_a), Type::TypeScheme(type_scheme_b)) => {
            let explicit_type_variable_overlaps_with_any_type = true;
            overlap(
                &type_scheme_a.type_value,
                &type_scheme_b.type_value,
                explicit_type_variable_overlaps_with_any_type,
            )
        }
        (Type::TypeScheme(type_scheme), other_type)
        | (other_type, Type::TypeScheme(type_scheme)) => {
            let explicit_type_variable_overlaps_with_any_type = true;
            overlap(
                &type_scheme.type_value,
                &other_type,
                explicit_type_variable_overlaps_with_any_type,
            )
        }

        // otherwise
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct EnumConstructorSymbol {
    /// Refers to the UID of the enum that contains this constructor
    pub enum_uid: SymbolUid,
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
            access: Access::Protected,
            name: Token {
                position: Position::dummy(),
                representation: name,
                token_type: TokenType::Identifier,
            },
        }
    }
    let type_variable = ExplicitTypeVariable {
        name: "T".to_string(),
    };
    vec![
        // build-in values
        Symbol {
            meta: meta("print".to_string()),
            kind: SymbolKind::Value(ValueSymbol {
                type_value: Type::TypeScheme(Box::new(TypeScheme {
                    type_variables: NonEmpty {
                        head: type_variable.clone(),
                        tail: vec![],
                    },
                    type_value: Type::Function(FunctionType {
                        parameter_type: Box::new(Type::ExplicitTypeVariable(type_variable)),
                        return_type: Box::new(Type::Unit),
                        type_constraints: vec![],
                    }),
                })),
            }),
        },
        // built-in types
        Symbol {
            meta: meta("String".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_value: Type::String,
            }),
        },
        Symbol {
            meta: meta("Character".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_value: Type::Character,
            }),
        },
        Symbol {
            meta: meta("Int".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_value: Type::Integer,
            }),
        },
        Symbol {
            meta: meta("Float".to_string()),
            kind: SymbolKind::Type(TypeSymbol {
                type_value: Type::Float,
            }),
        },
    ]
}
