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

#[derive(Debug, Clone)]
pub struct Environment {
    // parent: Option<&'a Environment<'a>>,
    value_symbols: HashMap<String, ValueSymbol>,
    type_symbols: HashMap<String, TypeSymbol>,
    constructor_symbols: HashMap<String, ConstructorSymbol>,
    type_variable_substitutions: Substitution,
    type_variable_index: Cell<usize>,
    scope: Scope,
    pub source: Source,
}

#[derive(Debug, Clone)]
enum SubstitutionItem {
    ImplicitTypeVariable(String),
    Type(Type),
    NotSubstituted,
}

impl Environment {
    pub fn new(source: &Source) -> Environment {
        let mut result = Environment {
            source: source.clone(),
            value_symbols: (HashMap::new()),
            type_symbols: (HashMap::new()),
            constructor_symbols: (HashMap::new()),
            type_variable_substitutions: (HashMap::new()),
            type_variable_index: Cell::new(0),
            scope: Scope::new(),
        };

        built_in_type_symbols()
            .into_iter()
            .for_each(|(name, type_symbol)| result.insert_built_in_type_symbol(name, type_symbol));

        built_in_value_symbols()
            .into_iter()
            .map(|(name, value_symbol)| {
                result.insert_value_symbol(
                    &Token {
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
                    },
                    value_symbol,
                )
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
                usage_references: Default::default(),
                declaration: Declaration::UserDefined {
                    source: self.source.clone(),
                    token: type_variable.clone(),
                    scope_name: self.current_scope_name(),
                },
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
            self.insert_value_symbol(
                &variable_name,
                ValueSymbol {
                    declaration: Declaration::UserDefined {
                        source: self.source.clone(),
                        token: variable_name.clone(),
                        scope_name: self.scope.get_current_scope_name(),
                    },
                    type_scheme: TypeScheme {
                        type_variables: vec![],
                        type_value: type_value.clone(),
                    },
                    usage_references: Default::default(),
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
            None => None, // panic!("No type variable has the name of {}", type_variable_name),
        }
    }

    pub fn step_into_new_child_scope(&mut self) {
        self.scope.step_into_new_child_scope()
    }

    pub fn step_out_to_parent_scope(&mut self) {
        self.scope.step_out_to_parent_scope()
    }

    pub fn insert_value_symbol(
        &mut self,
        token: &Token,
        value_symbol: ValueSymbol,
    ) -> Result<(), UnifyError> {
        Environment::insert_symbol(
            self.current_scope_name(),
            &mut self.value_symbols,
            SymbolName::Token(token.clone()),
            &self.source,
            value_symbol,
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
                self.insert_value_symbol(
                    variable_name,
                    ValueSymbol {
                        declaration: Declaration::UserDefined {
                            source: self.source.clone(),
                            scope_name: self.current_scope_name(),
                            token: variable_name.clone(),
                        },
                        type_scheme: TypeScheme {
                            type_variables: vec![],
                            type_value: type_value.clone(),
                        },
                        usage_references: Default::default(),
                    },
                )?;
                Ok(type_value)
            }
        }
    }

    pub fn check_for_unused_value_symbols(
        &self,
        current_scope_name: usize,
    ) -> Result<(), UnifyError> {
        match self
            .value_symbols
            .iter()
            .filter_map(|(_, symbol)| {
                if symbol.usage_references.borrow().is_empty() {
                    match &symbol.declaration {
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
                let children_scope_names = self.scope.get_children_scope_names(current_scope_name);
                for child_scope_name in &children_scope_names {
                    self.check_for_unused_value_symbols(*child_scope_name)?;
                }
                Ok(())
            }
            Some(token) => Err(UnifyError {
                position: token.position,
                kind: UnifyErrorKind::UnusedVariale,
            }),
        }
    }

    fn insert_built_in_type_symbol(&mut self, name: String, type_symbol: TypeSymbol) {
        match Environment::insert_symbol(
            self.current_scope_name(),
            &mut self.type_symbols,
            SymbolName::String(name),
            &self.source,
            type_symbol,
        ) {
            Ok(_) => (),
            Err(error) => panic!("Compiler error: {:#?}", error),
        }
    }

    pub fn insert_type_symbol(
        &mut self,
        token: &Token,
        type_symbol: TypeSymbol,
    ) -> Result<(), UnifyError> {
        Environment::insert_symbol(
            self.current_scope_name(),
            &mut self.type_symbols,
            SymbolName::Token(token.clone()),
            &self.source,
            type_symbol,
        )
    }

    pub fn insert_constructor_symbol(
        &mut self,
        token: &Token,
        constructor_symbol: ConstructorSymbol,
    ) -> Result<(), UnifyError> {
        Environment::insert_symbol(
            self.current_scope_name(),
            &mut self.constructor_symbols,
            SymbolName::Token(token.clone()),
            &self.source,
            constructor_symbol,
        )
    }

    fn insert_symbol<Symbol: Symbolizable>(
        current_scope_name: usize,
        symbols: &mut HashMap<String, Symbol>,
        symbol_name: SymbolName,
        source: &Source,
        new_symbol: Symbol,
    ) -> Result<(), UnifyError> {
        match symbol_name {
            SymbolName::String(name) => match symbols.get(&name) {
                Some(_) => panic!("Compiler error, duplicated built in symbol name {}", name),
                None => {
                    let name = Environment::get_symbol_name(name, current_scope_name);
                    symbols.insert(name, new_symbol);
                    Ok(())
                }
            },
            SymbolName::Token(token) => {
                let name = token.representation.clone();
                match symbols.get(&name) {
                    Some(symbol) => Err(UnifyError {
                        position: token.position,
                        kind: UnifyErrorKind::DuplicatedIdentifier {
                            first_declared_at: symbol.declaration(),
                            then_declared_at: Declaration::UserDefined {
                                source: source.clone(),
                                token,
                                scope_name: current_scope_name,
                            },
                            name,
                        },
                    }),
                    None => {
                        let name = Environment::get_symbol_name(name, current_scope_name);
                        symbols.insert(name, new_symbol);
                        Ok(())
                    }
                }
            }
        }
    }

    /// Get the symbol name that is stored in the symbols table
    /// The name contains:
    /// - scope name
    /// - symbol name
    fn get_symbol_name(symbol_name: String, scope_name: usize) -> String {
        format!("{}-{}", scope_name, symbol_name)
    }

    fn get_symbol<Symbol: Symbolizable>(
        &self,
        symbols: &HashMap<String, Symbol>,
        symbol_name: SymbolName,
        scope_name: usize,
    ) -> Option<Symbol> {
        let name = Environment::get_symbol_name(
            match &symbol_name {
                SymbolName::String(name) => name.clone(),
                SymbolName::Token(token) => token.representation.clone(),
            },
            scope_name,
        );
        match symbols.get(&name) {
            Some(symbol) => match symbol_name {
                SymbolName::Token(token) => {
                    symbol.add_usage_reference(token.position, self.source.clone());
                    // symbols.insert(name, symbol.clone());

                    Some(symbol.clone())
                }
                SymbolName::String(_) => Some(symbol.clone()),
            },
            None => match self.scope.get_parent_scope_name(scope_name) {
                None => None,
                Some(parent_scope_name) => self.get_symbol(symbols, symbol_name, parent_scope_name),
            },
        }
    }

    pub fn get_enum_constructors(&self, enum_name: &str) -> Vec<ConstructorSymbol> {
        self.constructor_symbols
            .iter()
            .filter_map(|(_, constructor_symbol)| {
                if constructor_symbol.enum_name == enum_name {
                    Some(constructor_symbol.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<ConstructorSymbol>>()
    }

    pub fn get_type_symbol(&self, name: SymbolName) -> Option<TypeSymbol> {
        self.get_symbol(
            &self.type_symbols,
            name,
            self.scope.get_current_scope_name(),
        )
    }

    pub fn get_value_symbol(&mut self, name: SymbolName) -> Option<ValueSymbol> {
        self.get_symbol(
            &self.value_symbols,
            name,
            self.scope.get_current_scope_name(),
        )
    }

    pub fn get_consructor_symbol(&mut self, name: SymbolName) -> Option<ConstructorSymbol> {
        self.get_symbol(
            &self.constructor_symbols,
            name,
            self.scope.get_current_scope_name(),
        )
    }
}

#[derive(Debug, Clone)]
pub enum SymbolName {
    /// This is for compiler internal usage only
    String(String),

    /// Use this when working with user-defined symbols
    Token(Token),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Unknown,
    BuiltIn,
    AutoGeneratedTypeVariable,
    UserDefined {
        source: Source,
        token: Token,
        scope_name: usize,
    },
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub declaration: Declaration,
    pub type_scheme: TypeScheme,
    pub usage_references: RefCell<Vec<UsageReference>>,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    pub declaration: Declaration,
    pub type_scheme: TypeScheme,
    pub usage_references: RefCell<Vec<UsageReference>>,
}

#[derive(Debug, Clone)]
pub struct ConstructorSymbol {
    pub declaration: Declaration,

    /// Refers to the name of the enum that contains this constructor
    pub enum_name: String,
    pub constructor_name: String,
    pub type_variables: Vec<String>,
    pub payload: Option<Type>,
    pub usage_references: RefCell<Vec<UsageReference>>,
}

pub trait Symbolizable: Clone {
    fn declaration(&self) -> Declaration;
    fn add_usage_reference(&self, position: Position, source: Source);
}

impl Symbolizable for ConstructorSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
    fn add_usage_reference(&self, position: Position, source: Source) {
        self.usage_references
            .borrow_mut()
            .push(UsageReference { position, source });
    }
}

impl Symbolizable for ValueSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
    fn add_usage_reference(&self, position: Position, source: Source) {
        self.usage_references
            .borrow_mut()
            .push(UsageReference { position, source });
    }
}

impl Symbolizable for TypeSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
    fn add_usage_reference(&self, position: Position, source: Source) {
        self.usage_references
            .borrow_mut()
            .push(UsageReference { position, source });
    }
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
            declaration: Declaration::BuiltIn,
            usage_references: Default::default(),
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
                declaration: Declaration::BuiltIn,
                type_scheme: TypeScheme {
                    type_variables: vec!["Element".to_string()],
                    type_value: Type::Array(Box::new(Type::ImplicitTypeVariable {
                        name: "Element".to_string(),
                    })),
                },
                usage_references: RefCell::new(Vec::new()),
            },
        ),
        (
            "string".to_string(),
            TypeSymbol {
                declaration: Declaration::BuiltIn,
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::String,
                },
                usage_references: RefCell::new(Vec::new()),
            },
        ),
        (
            "number".to_string(),
            TypeSymbol {
                declaration: Declaration::BuiltIn,
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Number,
                },
                usage_references: RefCell::new(Vec::new()),
            },
        ),
        (
            "null".to_string(),
            TypeSymbol {
                declaration: Declaration::BuiltIn,
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Null,
                },
                usage_references: RefCell::new(Vec::new()),
            },
        ),
        (
            "boolean".to_string(),
            TypeSymbol {
                declaration: Declaration::BuiltIn,
                type_scheme: TypeScheme {
                    type_variables: vec![],
                    type_value: Type::Boolean,
                },
                usage_references: RefCell::new(Vec::new()),
            },
        ),
    ]
}
