// Mimicked from Glassgow Haskell Compiler (GHC)
mod core {
    use crate::inferred_ast::Identifier;

    pub enum Expression<BindingType> {
        Variable(Identifier),
        Literal(Literal),
        Application {
            function: Box<Expression<BindingType>>,
            argument: Box<Expression<BindingType>>,
        },
        Lambda {
            parameter: BindingType,
            body: Box<Expression<BindingType>>,
        },
        Let {
            bind: Bind<BindingType>,
            expression: Box<Expression<BindingType>>,
        },
        /// Also known as match or switch.
        Case {
            subject: Expression<BindingType>,
            binding_type: BindingType,
            type_value: Type,
            alternatives: Vec<Alternative<BindingType>>,
        },
        Type(Type),
    }
    struct Alternative<BindingType>(
        AlternativeConstructor,
        Vec<BindingType>,
        (Expression<BindingType>),
    );

    enum AlternativeConstructor {
        /// For example: `OK`
        Data(DataConstructor),
        Literal(Literal),

        /// Underscore case
        Default,
    }
}
