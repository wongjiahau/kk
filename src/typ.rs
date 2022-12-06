// This module stores the representation of Type
use crate::{module::SymbolUid, non_empty::NonEmpty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Underscore,
    ExplicitTypeVariable(ExplicitTypeVariable),
    ImplicitTypeVariable(ImplicitTypeVariable),
    Record {
        key_type_pairs: Vec<(String, Type)>,
    },

    /// Also known as enum type or nominal type.
    Named {
        /// This is needed to differentiate two named types that has the same name
        ///  which are declared in different modules
        /// Also needed for looking up constructors for this type
        symbol_uid: SymbolUid,
        name: String,
        type_arguments: Vec<(String, Type)>,
    },
    BuiltInOneArgumentType {
        kind: BuiltInOneArgumentTypeKind,
        type_argument: Box<Type>,
    },
    Function(FunctionType),
    TypeScheme(Box<TypeScheme>),
    Tuple(Box<NonEmpty<Type>>),
    Float,
    Integer,
    String,
    Character,
    Unit,
    Keyword(String),
}

/// Type scheme means a type that is quantified over some type variables.  
/// In theoretical term, it means universal quantification.  
///
/// For example, the type scheme `<A> List<A>` means that for all type `A` we can have a type `List<A>`,
/// it further means that `A` can be substituted with any type value, say `String`, then we will have `List<String>`.
///
/// A type scheme might also be bounded by some constraints, which limits the set of types that can
/// be substituted with the quanitified type variables.
///
/// For example, if we have a type scheme `<A> List<A> where Printable<A>`,
/// it means that `A` can only be substituted if an implementation of `Printable<A>` exists.  
///
/// For more info, refer https://course.ccs.neu.edu/cs4410sp20/lec_type-inference_notes.html
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub type_variables: NonEmpty<ExplicitTypeVariable>,
    pub type_value: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameter_type: Box<Type>,
    pub return_type: Box<Type>,
    pub type_constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstraint {
    pub name: String,
    pub type_value: Type,
}

/// Type variable that is declared by user (a.k.a quantified). Cannot be substituted before instantiation.
/// Note that a type variable is only explicit within its own scope.
/// This is also commonly known as Rigid Type Variable.  
///
/// See https://stackoverflow.com/a/12719617/6587634
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExplicitTypeVariable {
    pub name: String,
}

/// Type variable that is implicitly created for unification.
/// Also known as Fresh Type Variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitTypeVariable {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInOneArgumentTypeKind {
    Quoted,
    Array,
}
