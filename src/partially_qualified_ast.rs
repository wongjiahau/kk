/// Partially Qualified means the name is resolved,
/// but the other parts remain unresolved
use crate::{
    module::Access,
    qualified_ast, raw_ast,
    tokenize::{StringLiteral, Token},
};

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),

    TypeAlias(TypeAliasStatement),

    Enum(EnumStatement),

    ModuleAlias(raw_ast::ModuleAliasStatement),
    ModuleDefinition(ModuleDefinitionStatement),

    Entry(raw_ast::EntryStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub access: Access,
    pub keyword_let: Token,
    pub name: qualified_ast::ResolvedName,
    pub doc_string: Option<StringLiteral>,
    pub type_annotation: raw_ast::TypeAnnotation,
    pub expression: raw_ast::Expression,
}

#[derive(Debug, Clone)]
pub struct TypeAliasStatement {
    pub access: Access,
    pub keyword_type: Token,
    pub name: qualified_ast::ResolvedName,
    pub right: raw_ast::TypeAnnotation,
    pub type_variables_declaration: Option<raw_ast::TypeVariablesDeclaration>,
}

#[derive(Debug, Clone)]
pub struct EnumStatement {
    pub access: Access,
    pub keyword_type: Token,
    pub name: qualified_ast::ResolvedName,
    pub type_variables_declaration: Option<raw_ast::TypeVariablesDeclaration>,
    pub constructors: Vec<raw_ast::EnumConstructorDefinition>,
}

#[derive(Debug, Clone)]
pub struct ModuleDefinitionStatement {
    pub name: qualified_ast::ResolvedName,
    pub statements: Vec<Statement>,
}
