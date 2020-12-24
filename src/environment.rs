use crate::ast::*;
use crate::unify::unify_type;
use crate::unify::{UnifyError, UnifyErrorKind};
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;

type Substitution = HashMap<String, SubstitutionItem>;

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    parent: Option<&'a Environment<'a>>,
    value_symbols: RefCell<HashMap<String, ValueSymbol>>,
    type_symbols: RefCell<HashMap<String, TypeSymbol>>,
    constructor_symbols: RefCell<HashMap<String, ConstructorSymbol>>,
    type_variable_substitutions: RefCell<Substitution>,
    type_variable_index: Cell<usize>,
    pub source: Source,
}

#[derive(Debug, Clone)]
enum SubstitutionItem {
    TypeVariable(String),
    Type(Type),
    NotSubstituted,
}

impl<'a> Environment<'a> {
    pub fn new_root(source: Source) -> Environment<'a> {
        Environment {
            parent: None,
            value_symbols: RefCell::new(HashMap::new()),
            type_symbols: RefCell::new(built_in_type_symbols()),
            constructor_symbols: RefCell::new(HashMap::new()),
            type_variable_substitutions: RefCell::new(HashMap::new()),
            type_variable_index: Cell::new(0),
            source,
        }
    }

    pub fn new(parent: &'a Environment) -> Environment<'a> {
        Environment {
            source: parent.source.clone(),
            parent: Some(parent),
            value_symbols: RefCell::new(HashMap::new()),
            type_symbols: RefCell::new(HashMap::new()),
            constructor_symbols: RefCell::new(HashMap::new()),
            type_variable_substitutions: RefCell::new(HashMap::new()),
            type_variable_index: Cell::new(0),
        }
    }

    pub fn get_next_type_variable_name(&self) -> String {
        let root = self.get_root_environment();
        let type_variable_index = root.type_variable_index.get();
        let name = format!("@TVAR{}", type_variable_index);
        root.type_variable_index.set(type_variable_index + 1);
        // self.type_variable_index += 1;

        let mut type_variable_substitutions = self.type_variable_substitutions.borrow_mut();

        type_variable_substitutions.insert(name.clone(), SubstitutionItem::NotSubstituted);

        name
    }

    fn get_root_environment(&self) -> &Environment {
        match self.parent {
            Some(parent) => parent.get_root_environment(),
            None => self,
        }
    }

    pub fn introduce_type_variable(
        &mut self,
        variable_name: Option<&Token>,
    ) -> Result<Type, UnifyError> {
        let new_type_variable_name = self.get_next_type_variable_name();
        let type_value = Type::TypeVariable {
            name: new_type_variable_name.clone(),
        };

        if let Some(variable_name) = variable_name {
            self.insert_value_symbol(
                &variable_name,
                ValueSymbol {
                    declaration: Declaration::UserDefined(
                        self.source.clone(),
                        variable_name.clone(),
                    ),
                    actual_type: TypeScheme {
                        type_variables: vec![],
                        type_value: type_value.clone(),
                    },
                    usage_references: vec![],
                },
            )?;
        }

        let mut type_variable_substitutions = self.type_variable_substitutions.borrow_mut();

        type_variable_substitutions
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
                Type::TypeVariable { name } => {
                    self.update_substitution(name, terminal_type, position)
                }
                _ => {
                    let unified_type = unify_type(self, &terminal_type, &type_value, position)?;

                    let substitution_item = match unified_type {
                        Type::TypeVariable { name } => SubstitutionItem::TypeVariable(name),
                        other => SubstitutionItem::Type(other),
                    };

                    self.type_variable_substitutions
                        .borrow_mut()
                        .insert(type_variable_name, substitution_item);
                    Ok(())
                }
            },
            None => {
                let substitution_item = match type_value {
                    Type::TypeVariable { name } => SubstitutionItem::TypeVariable(name),
                    other => SubstitutionItem::Type(other),
                };
                self.type_variable_substitutions
                    .borrow_mut()
                    .insert(type_variable_name, substitution_item);
                Ok(())
            }
        }
    }

    /**
     * Apply subtitution to a type.  
     * For example, if we apply the subtitution {A = String, B = Int}
     * to type Function<A, B>, we get Function<String, Int>
     */
    pub fn apply_subtitution_to_type(&self, type_value: &Type) -> Type {
        match type_value {
            Type::Number => Type::Number,
            Type::Boolean => Type::Boolean,
            Type::String => Type::String,
            Type::Null => Type::Null,
            Type::Array(type_value) => Type::Array(Box::new(
                self.apply_subtitution_to_type(type_value.as_ref()),
            )),
            Type::Tuple(types) => Type::Tuple(
                types
                    .iter()
                    .map(|type_value| self.apply_subtitution_to_type(type_value))
                    .collect(),
            ),
            Type::TypeVariable { name } => {
                match self.get_type_variable_terminal_type(name.clone()) {
                    Some(type_value) => self.apply_subtitution_to_type(&type_value),
                    None => Type::TypeVariable { name: name.clone() },
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
            Type::Union(UnionType {
                tags,
                bound,
                catch_all,
            }) => Type::Union(UnionType {
                bound: bound.clone(),
                catch_all: *catch_all,
                tags: tags
                    .iter()
                    .map(|tag_type| self.apply_subtitution_to_tag_type(&tag_type))
                    .collect(),
            }),
            Type::Named { name, arguments } => Type::Named {
                name: name.clone(),
                arguments: arguments
                    .iter()
                    .map(|argument| self.apply_subtitution_to_type(&argument))
                    .collect(),
            },
        }
    }

    pub fn apply_subtitution_to_tag_type(&self, TagType { tagname, payload }: &TagType) -> TagType {
        TagType {
            tagname: tagname.clone(),
            payload: match payload {
                Some(payload) => Some(Box::new(self.apply_subtitution_to_type(payload.as_ref()))),
                None => None,
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

    fn get_type_variable_terminal_type(&self, type_variable_name: String) -> Option<Type> {
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
        match self
            .type_variable_substitutions
            .borrow()
            .get(&type_variable_name)
        {
            Some(SubstitutionItem::Type(type_value)) => Some(type_value.clone()),
            Some(SubstitutionItem::TypeVariable(type_variable_name)) => {
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
                        None => Some(Type::TypeVariable {
                            name: type_variable_name.clone(),
                        }),
                    }
                }
            }
            Some(SubstitutionItem::NotSubstituted) => None,
            None => None, // panic!("No type variable has the name of {}", type_variable_name),
        }
    }

    pub fn insert_value_symbol(
        &mut self,
        token: &Token,
        value_symbol: ValueSymbol,
    ) -> Result<(), UnifyError> {
        let value_symbols = self.value_symbols.borrow_mut();
        insert_symbol(value_symbols, token, &self.source, value_symbol)
    }

    pub fn insert_type_symbol(
        &mut self,
        token: &Token,
        type_symbol: TypeSymbol,
    ) -> Result<(), UnifyError> {
        let type_symbols = self.type_symbols.borrow_mut();
        insert_symbol(type_symbols, token, &self.source, type_symbol)
    }

    pub fn insert_constructor_symbol(
        &mut self,
        token: &Token,
        constructor_symbol: ConstructorSymbol,
    ) -> Result<(), UnifyError> {
        let constructor_symbols = self.constructor_symbols.borrow_mut();
        insert_symbol(constructor_symbols, token, &self.source, constructor_symbol)
    }

    pub fn get_enum_constrctors(&self, enum_name: &str) -> Vec<ConstructorSymbol> {
        let constructor_symbols = self.constructor_symbols.borrow();
        let result = constructor_symbols
            .iter()
            .filter_map(|(_, constructor_symbol)| {
                if constructor_symbol.enum_name == enum_name {
                    Some(constructor_symbol.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<ConstructorSymbol>>();
        if !result.is_empty() {
            result
        } else if let Some(parent) = &self.parent {
            parent.get_enum_constrctors(enum_name)
        } else {
            vec![]
        }
    }

    pub fn get_type_symbol(&self, name: &str) -> Option<TypeSymbol> {
        if let Some(type_symbol) = self.type_symbols.borrow().get(name) {
            Some(type_symbol.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_type_symbol(name)
        } else {
            None
        }
    }

    pub fn get_value_symbol(&self, name: &str) -> Option<ValueSymbol> {
        if let Some(value_symbol) = self.value_symbols.borrow().get(name) {
            Some(value_symbol.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_value_symbol(name)
        } else {
            None
        }
    }

    pub fn get_consructor_symbol(&self, name: &str) -> Option<ConstructorSymbol> {
        if let Some(constructor_symbol) = self.constructor_symbols.borrow().get(name) {
            Some(constructor_symbol.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_consructor_symbol(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Unknown,
    BuiltIn,
    AutoGeneratedTypeVariable,
    UserDefined(Source, Token),
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
    pub declaration: Declaration,
    pub type_scheme: TypeScheme,
    pub usage_references: Vec<UsageReference>,
}

#[derive(Debug, Clone)]
pub struct ValueSymbol {
    pub declaration: Declaration,
    pub actual_type: TypeScheme,
    pub usage_references: Vec<UsageReference>,
}

#[derive(Debug, Clone)]
pub struct ConstructorSymbol {
    pub declaration: Declaration,

    /// Refers to the name of the enum that contains this constructor
    pub enum_name: String,
    pub constructor_name: String,
    pub type_variables: Vec<String>,
    pub payload: Option<Type>,
    pub usage_references: Vec<UsageReference>,
}

pub trait Symbolizable {
    fn declaration(&self) -> Declaration;
}

impl Symbolizable for ConstructorSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
}

impl Symbolizable for ValueSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
}

impl Symbolizable for TypeSymbol {
    fn declaration(&self) -> Declaration {
        self.declaration.clone()
    }
}

fn insert_symbol<Symbol: Symbolizable>(
    mut symbols: RefMut<HashMap<String, Symbol>>,
    token: &Token,
    source: &Source,
    new_symbol: Symbol,
) -> Result<(), UnifyError> {
    let name = token.representation.clone();
    match symbols.get(&name) {
        Some(symbol) => Err(UnifyError {
            position: token.position,
            kind: UnifyErrorKind::DuplicatedIdentifier {
                first_declared_at: symbol.declaration(),
                then_declared_at: Declaration::UserDefined(source.clone(), token.clone()),
                name,
            },
        }),
        None => {
            symbols.insert(name, new_symbol);
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct UsageReference {
    position: Position,
    source: Source,
}

fn built_in_type_symbols() -> HashMap<String, TypeSymbol> {
    let mut hash_map = HashMap::new();
    hash_map.insert(
        "string".to_string(),
        TypeSymbol {
            declaration: Declaration::BuiltIn,
            type_scheme: TypeScheme {
                type_variables: vec![],
                type_value: Type::String,
            },
            usage_references: vec![],
        },
    );
    hash_map.insert(
        "number".to_string(),
        TypeSymbol {
            declaration: Declaration::BuiltIn,
            type_scheme: TypeScheme {
                type_variables: vec![],
                type_value: Type::Number,
            },
            usage_references: vec![],
        },
    );

    hash_map.insert(
        "null".to_string(),
        TypeSymbol {
            declaration: Declaration::BuiltIn,
            type_scheme: TypeScheme {
                type_variables: vec![],
                type_value: Type::Null,
            },
            usage_references: vec![],
        },
    );

    hash_map.insert(
        "boolean".to_string(),
        TypeSymbol {
            declaration: Declaration::BuiltIn,
            type_scheme: TypeScheme {
                type_variables: vec![],
                type_value: Type::Boolean,
            },
            usage_references: vec![],
        },
    );

    let array_type_variable = "T".to_string();
    hash_map.insert(
        "Array".to_string(),
        TypeSymbol {
            declaration: Declaration::BuiltIn,
            type_scheme: TypeScheme {
                type_variables: vec![array_type_variable.clone()],
                type_value: Type::Array(Box::new(Type::TypeVariable {
                    name: array_type_variable,
                })),
            },
            usage_references: vec![],
        },
    );
    hash_map
}
