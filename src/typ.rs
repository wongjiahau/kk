// This module stores the representation of Type
use crate::{module::SymbolUid, non_empty::NonEmpty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Effect {
    name: String,
    arguments: Vec<Type>,
}

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
    Boolean,
    Float,
    Integer,
    String,
    Character,
    Unit,
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

    /// List of constraints that is bounded to this type scheme.  
    /// The order of constraints does not matter.
    pub constraints: Vec<InferredConstraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameter_type: Box<Type>,
    pub return_type: Box<Type>,
}

/// Example of constraint:
/// ```
/// Equatable<A, B>
/// Printable<A>
/// ```
/// `Equatable` is the interface name, while `A` or/and `B` are the type variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredConstraint {
    pub interface_uid: SymbolUid,

    /// Note that the order of these type variables matters.  
    /// For example, `Equatable<A, B>` is not always the same as `Equatable<B, A>`.
    pub type_variables: NonEmpty<ExplicitTypeVariable>,

    /// Variable that is bounded by this contraint should be parameterised using `injected_parameter_uid`
    pub injected_parameter_uid: SymbolUid,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstantiatedConstraint {
    pub interface_uid: SymbolUid,
    pub type_variables: NonEmpty<ImplicitTypeVariable>,
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
