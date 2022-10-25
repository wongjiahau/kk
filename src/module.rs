use crate::non_empty::NonEmpty;
use crate::qualified_ast::{ResolvedName, Scope, ScopeName, SymbolUid};
use crate::stringify_error::stringify_type;
use crate::tokenize::{Position, Token};
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use crate::{raw_ast::*, typ::*};
use std::cell::Cell;
use std::collections::HashMap;

/// Used for unification
pub type Substitution = HashMap<String, SubstitutionItem>;

pub struct GetValueSymbolResult {
    pub symbol_uid: SymbolUid,
    pub type_value: Type,
}

#[derive(Debug, Clone)]
pub enum Access {
    /// Can be imported anywhere (even via remote URL)
    /// Used for auto semantic versioning
    Public { keyword_public: Token },
    /// Can only be used within one file
    Private { keyword_private: Token },
    /// Default, can be imported via relative path
    Protected,
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

/// TODO: rename this to Environment
#[derive(Debug, Clone)]
pub struct Module {
    value_symbols: Vec<ValueSymbol>,
    type_symbols: Vec<TypeSymbol>,
    enum_constructor_symbols: Vec<EnumConstructorSymbol>,

    /// This is used for Hindley-Milner type inference algorithm
    type_variable_substitutions: Substitution,
    type_variable_index: Cell<usize>,
}

#[derive(Debug, Clone)]
enum SubstitutionItem {
    ImplicitTypeVariable(ImplicitTypeVariable),
    Type(Type),
    NotSubstituted,
}

impl Module {
    pub fn new() -> Module {
        let mut module = Module {
            type_variable_substitutions: (HashMap::new()),
            type_variable_index: Cell::new(0),
            value_symbols: Vec::new(),
            type_symbols: Vec::new(),
            enum_constructor_symbols: Vec::new(),
        };

        let (value_symbols, type_symbols) = built_in_symbols();

        value_symbols
            .into_iter()
            .map(|symbol| module.insert_value_symbol(symbol))
            .collect::<Result<Vec<_>, UnifyError>>()
            .expect("Compiler error: built-in symbols should not have conflict");

        type_symbols
            .into_iter()
            .map(|symbol| module.insert_type_symbol(symbol))
            .collect::<Vec<_>>();

        module
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
    pub fn insert_explicit_type_variable(&mut self, type_variable_name: &ResolvedName) {
        self.insert_type_symbol(TypeSymbol {
            name: type_variable_name.clone(),
            type_value: Type::ExplicitTypeVariable(ExplicitTypeVariable {
                name: type_variable_name.representation.clone(),
            }),
        })
    }

    pub fn introduce_implicit_type_variable(&mut self) -> Result<Type, UnifyError> {
        let new_type_variable_name = self.get_next_type_variable_name();
        let type_value = Type::ImplicitTypeVariable(ImplicitTypeVariable {
            name: new_type_variable_name.clone(),
        });

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

    /// Insert a value symbol with a specific type
    /// If the type is None, then this variable will be
    /// instantiated to have a type of a new type variable
    pub fn insert_value_symbol_with_type(
        &mut self,
        variable_name: &ResolvedName,
        type_value: Option<Type>,
        access: Access,
        is_implicit_variable: bool,
    ) -> Result<Type, UnifyError> {
        let type_value = match type_value {
            None => Type::ImplicitTypeVariable(ImplicitTypeVariable {
                name: self.get_next_type_variable_name(),
            }),
            Some(type_value) => type_value,
        };
        self.insert_value_symbol(ValueSymbol {
            name: variable_name.clone(),
            type_value: type_value.clone(),
        })?;
        Ok(type_value)
    }

    pub fn insert_type_symbol(&mut self, type_symbol: TypeSymbol) {
        self.type_symbols.push(type_symbol)
    }

    pub fn insert_enum_constructor_symbol(
        &mut self,
        enum_constructor_symbol: EnumConstructorSymbol,
    ) {
        // TODO: remove special treatment for enum constructor, make them as values
        //       However, they are required by case exhaustiveness check
        self.enum_constructor_symbols.push(enum_constructor_symbol)
    }

    pub fn insert_value_symbol(&mut self, new_value_symbol: ValueSymbol) -> Result<(), UnifyError> {
        self.value_symbols
            .iter()
            .map(|existing_value_symbol| {
                if existing_value_symbol.name.representation == new_value_symbol.name.representation
                {
                    if overlap(
                        &existing_value_symbol.type_value,
                        &new_value_symbol.type_value,
                        true,
                    ) {
                        Err(UnifyError {
                            position: new_value_symbol.name.position,
                            kind: UnifyErrorKind::CannotBeOverloaded,
                        })
                    } else {
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            })
            .collect::<Result<Vec<()>, UnifyError>>()?;

        self.value_symbols.push(new_value_symbol);
        Ok(())
    }

    pub fn get_enum_constructors(&self, enum_uid: SymbolUid) -> Vec<EnumConstructorSymbol> {
        self.enum_constructor_symbols
            .iter()
            .filter_map(|enum_constructor| {
                if enum_constructor.enum_uid == enum_uid {
                    Some(enum_constructor.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_type_symbol_by_uid(&self, symbol_uid: &SymbolUid) -> TypeSymbol {
        self.type_symbols
            .iter()
            .find(|symbol| symbol.name.uid.eq(symbol_uid))
            .expect("Programming error, type symbols should already be inserted")
            .clone()
    }

    fn get_value_symbol_by_uid(&self, symbol_uid: &SymbolUid) -> ValueSymbol {
        self.value_symbols
            .iter()
            .find(|symbol| symbol.name.uid.eq(symbol_uid))
            .expect("Should already be inserted before")
            .clone()
    }

    pub fn get_value_symbol_by_name(
        &mut self,
        name: &String,
        expected_type: &Type,
        scope_name: &ScopeName,
        position: &Position,
    ) -> Result<SymbolUid, UnifyError> {
        match self.value_symbols.iter().find(|value_symbol| {
            value_symbol.name.representation.eq(name)
                && overlap(&value_symbol.type_value, expected_type, false)
        }) {
            Some(symbol) => Ok(symbol.name.uid.clone()),
            None => Err(UnifyError {
                position: position.clone(),
                kind: UnifyErrorKind::UnknownValueSymbol {
                    symbol_name: name.clone(),
                },
            }),
        }
    }

    /// `expected_type` is required for overloading disambiguation
    pub fn get_value_symbol_by_uids(
        &mut self,
        uids: &NonEmpty<SymbolUid>,
        expected_type: &Option<Type>,
        position: Position,
    ) -> Result<GetValueSymbolResult, UnifyError> {
        match uids.tail.is_empty() {
            // If there's only one value symbol with matching name in this scope,
            // then return the value
            true => Ok(GetValueSymbolResult {
                symbol_uid: uids.head,
                type_value: self.get_value_symbol_by_uid(&uids.head).type_value,
            }),

            // Otherwise, disambiguate based on `expected_type`
            false => {
                let matching_value_symbols = uids.map(|uid| self.get_value_symbol_by_uid(&uid));
                match expected_type {
                    None => Err(UnifyError {
                        position,
                        kind: UnifyErrorKind::AmbiguousSymbol {
                            matching_value_symbols,
                        },
                    }),
                    Some(expected_type) => {
                        let value_symbols_with_matching_type = matching_value_symbols
                            .into_vector()
                            .iter()
                            .filter(|value_symbol| {
                                overlap(&expected_type, &value_symbol.type_value, false)
                            })
                            .collect::<Vec<_>>();

                        match value_symbols_with_matching_type.split_first() {
                            None => Err(UnifyError {
                                position,
                                kind: UnifyErrorKind::NoMatchingValueSymbol {
                                    possible_value_symbols: matching_value_symbols,
                                },
                            }),
                            Some((head, [])) => Ok(GetValueSymbolResult {
                                symbol_uid: head.name.uid.clone(),
                                type_value: head.type_value.clone(),
                            }),
                            Some(_) => Err(UnifyError {
                                position,
                                kind: UnifyErrorKind::AmbiguousSymbol {
                                    matching_value_symbols,
                                },
                            }),
                        }
                    }
                }
            }
        }
    }

    pub fn matches_some_enum_constructor(&self, name: &str) -> bool {
        self.enum_constructor_symbols
            .iter()
            .any(|symbol| symbol.constructor_name.representation.eq(name))
    }

    pub fn get_enum_constructor_symbol_by_uid(&self, uid: &SymbolUid) -> EnumConstructorSymbol {
        self.enum_constructor_symbols
            .iter()
            .find(|symbol| symbol.constructor_name.uid.eq(uid))
            .expect("This should not happen")
            .clone()
    }

    /// `expected_enum_uid` -
    ///     This is for disambiguating constructors with the same name that belongs to different enums
    pub fn get_constructor_symbol(
        &self,
        expected_enum_uid: Option<SymbolUid>,
        constructor_uids: &NonEmpty<SymbolUid>,
        position: Position,
    ) -> Result<EnumConstructorSymbol, UnifyError> {
        if constructor_uids.tail.is_empty() {
            Ok(self.get_enum_constructor_symbol_by_uid(&constructor_uids.head))
        } else {
            let matching_constructors =
                constructor_uids.map(|uid| self.get_enum_constructor_symbol_by_uid(&uid));
            match expected_enum_uid {
                None => Err(UnifyError {
                    position,
                    kind: UnifyErrorKind::AmbiguousConstructorUsage {
                        possible_enum_names: matching_constructors
                            .map(|constructor| constructor.enum_name),
                    },
                }),
                Some(expected_enum_uid) => {
                    match matching_constructors.find_map(|constructor| {
                        if constructor.enum_uid.eq(&expected_enum_uid) {
                            Some(constructor)
                        } else {
                            None
                        }
                    }) {
                        Some(constructor) => Ok(constructor.clone()),
                        None => panic!("The expected enum does not contains this constructor"),
                    }
                }
            }
        }
    }

    pub fn unify_error(&self, position: &Position, kind: UnifyErrorKind) -> UnifyError {
        UnifyError {
            position: position.clone(),
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub name: ResolvedName,
    pub type_value: Type,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    pub name: ResolvedName,
    pub type_value: Type,
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
    pub constructor_name: ResolvedName,
    pub type_variables: Vec<String>,
    pub payload: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct UsageReference {
    position: Position,
}

fn built_in_symbols() -> (Vec<ValueSymbol>, Vec<TypeSymbol>) {
    fn name(name: String) -> ResolvedName {
        ResolvedName {
            // TODO: the UID of built-in symbols should be known to the name resolution phase
            uid: SymbolUid(0),
            position: Position::dummy(),
            representation: name,
            scope: Scope::dummy(),
        }
    }
    let type_variable = ExplicitTypeVariable {
        name: "T".to_string(),
    };
    let value_symbols = vec![ValueSymbol {
        name: name("print".to_string()),

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
    }];
    let type_symbols = vec![
        // built-in types
        TypeSymbol {
            name: name("String".to_string()),
            type_value: Type::String,
        },
        TypeSymbol {
            name: name("Character".to_string()),
            type_value: Type::Character,
        },
        TypeSymbol {
            name: name("Int".to_string()),
            type_value: Type::Integer,
        },
        TypeSymbol {
            name: name("Float".to_string()),
            type_value: Type::Float,
        },
    ];
    (value_symbols, type_symbols)
}
impl Access {
    pub fn is_private(&self) -> bool {
        match self {
            Access::Public { keyword_public } => false,
            Access::Private { keyword_private } => true,
            Access::Protected => false,
        }
    }
}
