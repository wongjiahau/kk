use crate::{javascript_ast::javascript, non_empty::NonEmpty};

pub trait TailCall {
    /// Check if the given `identifier` is in tail position of `self`.  
    ///
    /// Refer https://wiki.haskell.org/Tail_recursion
    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool;

    /// Check if the given `identifier` occurs in `self`.  
    fn has(&self, identifier: &javascript::Identifier) -> bool;

    /// Convert function call of the given `function_name` in `self`
    /// with variables assignments using the given `parameter_names`
    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self;
}

/// Eliminate function call of the given `function`.  
///
/// Suppose `function_name` is `factorial`
/// and `function` is (note that the below code is Javascript):
/// ```js
///   (acc, n) => {
///     if(n <= 1) return acc
///     else return factorial(acc * n, n - 1)
///   }
/// ```
/// Then, the result should be:
/// ```js
///   (acc, n) => {
///     while(true) {
///       if(n <= 1) return acc
///       else {
///         acc = acc * n
///         n = n -1
///       }
///     }
///   }
/// ```
pub fn eliminate_tail_call(
    function_name: &javascript::Identifier,
    function: javascript::Function,
) -> javascript::Function {
    let parameter_names = function.parameters.clone();
    javascript::Function {
        parameters: function.parameters,
        body: vec![javascript::Statement::While {
            condition: javascript::Expression::Boolean(true),
            body: function
                .body
                .eliminate_tail_call(function_name, &parameter_names),
        }],
    }
}

impl<T: TailCall> TailCall for Vec<T> {
    fn has(&self, identifier: &javascript::Identifier) -> bool {
        self.iter().any(|t| t.has(identifier))
    }

    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        self.iter().all(|t| t.in_tail_position(identifier))
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        self.into_iter()
            .map(|t| t.eliminate_tail_call(function_name, parameter_names))
            .collect()
    }
}

impl<T: TailCall + Clone> TailCall for NonEmpty<T> {
    fn has(&self, identifier: &javascript::Identifier) -> bool {
        self.clone().into_vector().has(identifier)
    }

    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        self.clone().into_vector().in_tail_position(identifier)
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        NonEmpty {
            head: self
                .head
                .eliminate_tail_call(function_name, parameter_names),
            tail: self
                .tail
                .eliminate_tail_call(function_name, parameter_names),
        }
    }
}
impl TailCall for javascript::Assignment {
    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        !self.right.has(identifier) && self.left.in_tail_position(identifier)
    }

    fn has(&self, identifier: &javascript::Identifier) -> bool {
        self.right.has(identifier) || self.left.has(identifier)
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        javascript::Assignment {
            left: self
                .left
                .eliminate_tail_call(function_name, parameter_names),
            right: self
                .right
                .eliminate_tail_call(function_name, parameter_names),
        }
    }
}

impl TailCall for javascript::AssignmentLeft {
    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::AssignmentLeft::Variable(other) => !identifier.eq(other),
            javascript::AssignmentLeft::Object { name, accesses } => {
                !identifier.eq(name) && !accesses.has(identifier)
            }
        }
    }

    fn has(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::AssignmentLeft::Variable(other) => identifier.eq(other),
            javascript::AssignmentLeft::Object { name, accesses } => {
                identifier.eq(name) || accesses.has(identifier)
            }
        }
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        match self {
            javascript::AssignmentLeft::Variable(identifier) => {
                javascript::AssignmentLeft::Variable(identifier)
            }
            javascript::AssignmentLeft::Object { name, accesses } => {
                javascript::AssignmentLeft::Object {
                    name,
                    accesses: accesses.eliminate_tail_call(function_name, parameter_names),
                }
            }
        }
    }
}

impl TailCall for javascript::Expression {
    fn has(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::Expression::Sequence(expressions) => expressions.has(identifier),
            javascript::Expression::LogicalOr { left, right }
            | javascript::Expression::LogicalAnd { left, right }
            | javascript::Expression::Equals { left, right }
            | javascript::Expression::MemberAccess {
                object: left,
                property: right,
            } => left.has(identifier) || right.has(identifier),
            javascript::Expression::UnsafeJavascriptCode(_)
            | javascript::Expression::Null
            | javascript::Expression::Boolean(_)
            | javascript::Expression::String(_)
            | javascript::Expression::Number { .. } => false,
            javascript::Expression::Variable(other) => identifier.eq(other),
            javascript::Expression::Array(expressions)
            | javascript::Expression::StringConcat(expressions) => expressions.has(identifier),
            javascript::Expression::FunctionCall {
                function,
                arguments,
            } => function.has(identifier) || arguments.has(identifier),
            javascript::Expression::Function(javascript::Function { body, .. }) => {
                body.has(identifier)
            }
            javascript::Expression::Object(key_values) => key_values.has(identifier),
            javascript::Expression::ObjectWithSpread { spread, key_values } => {
                spread.has(identifier) || key_values.has(identifier)
            }
            javascript::Expression::Conditional {
                condition,
                if_true,
                if_false,
            } => condition.has(identifier) || if_true.has(identifier) || if_false.has(identifier),
            javascript::Expression::Assignment(assignment) => assignment.has(identifier),
        }
    }

    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::Expression::Array(expressions)
            | javascript::Expression::StringConcat(expressions) => !expressions.has(identifier),
            javascript::Expression::Sequence(expressions) => !expressions.has(identifier),
            javascript::Expression::LogicalOr { left, right }
            | javascript::Expression::LogicalAnd { left, right }
            | javascript::Expression::Equals { left, right }
            | javascript::Expression::MemberAccess {
                object: left,
                property: right,
            } => !left.has(identifier) && !right.has(identifier),
            javascript::Expression::Null
            | javascript::Expression::Boolean(_)
            | javascript::Expression::String(_)
            | javascript::Expression::Number { .. } => true,
            javascript::Expression::Variable(other) => identifier.eq(other),
            javascript::Expression::FunctionCall {
                function,
                arguments,
            } => function.in_tail_position(identifier) && !arguments.has(identifier),
            javascript::Expression::Function(javascript::Function { body, .. }) => {
                match body.split_last() {
                    None => true,
                    Some((last, initial)) => {
                        !initial.to_vec().has(identifier) && last.in_tail_position(identifier)
                    }
                }
            }
            javascript::Expression::Object(key_values) => !key_values.has(identifier),
            javascript::Expression::ObjectWithSpread { spread, key_values } => {
                !spread.has(identifier) && !key_values.has(identifier)
            }
            javascript::Expression::Conditional {
                condition,
                if_true,
                if_false,
            } => {
                !condition.has(identifier)
                    && if_true.in_tail_position(identifier)
                    && if_false.in_tail_position(identifier)
            }
            javascript::Expression::UnsafeJavascriptCode(_) => false,
            javascript::Expression::Assignment(assignment) => {
                !assignment.right.has(identifier)
                    && match &assignment.left {
                        javascript::AssignmentLeft::Variable(identifier) => {
                            !identifier.eq(identifier)
                        }
                        javascript::AssignmentLeft::Object { name, accesses } => {
                            !name.eq(identifier) && !accesses.has(identifier)
                        }
                    }
            }
        }
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        match &self {
            javascript::Expression::FunctionCall {
                function,
                arguments,
            } => {
                match *function.clone() {
                    javascript::Expression::Variable(name) if name.eq(function_name) => {
                        // Convert this to an immediately-invoked function call,
                        // also convert arguments into variable assignments.
                        // This is the core of part of tail call elimination.
                        javascript::Expression::FunctionCall {
                            arguments: vec![],
                            function: Box::new(javascript::Expression::Function(
                                javascript::Function {
                                    parameters: vec![],
                                    body: parameter_names
                                        .iter()
                                        .zip(arguments)
                                        .map(|(parameter_name, expression)| {
                                            javascript::Statement::Assignment {
                                                is_declaration: false,
                                                assignment: javascript::Assignment {
                                                    left: javascript::AssignmentLeft::Variable(
                                                        parameter_name.clone(),
                                                    ),
                                                    right: expression.clone(),
                                                },
                                            }
                                        })
                                        .collect(),
                                },
                            )),
                        }
                    }
                    _ => javascript::Expression::FunctionCall {
                        function: Box::new(
                            function
                                .clone()
                                .eliminate_tail_call(function_name, parameter_names),
                        ),
                        arguments: arguments.clone(),
                    },
                }
            }
            javascript::Expression::Conditional {
                condition,
                if_true,
                if_false,
            } => javascript::Expression::Conditional {
                condition: condition.clone(),
                if_true: Box::new(
                    if_true
                        .clone()
                        .eliminate_tail_call(function_name, parameter_names),
                ),
                if_false: Box::new(
                    if_false
                        .clone()
                        .eliminate_tail_call(function_name, parameter_names),
                ),
            },
            javascript::Expression::Function(javascript::Function { parameters, body }) => {
                javascript::Expression::Function(javascript::Function {
                    parameters: parameters.clone(),
                    body: body
                        .clone()
                        .eliminate_tail_call(function_name, parameter_names),
                })
            }

            // Return self for all other kinds of expressions
            _ => self,
        }
    }
}

impl TailCall for javascript::Statement {
    fn has(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::Statement::Assignment { assignment, .. } => assignment.has(identifier),
            javascript::Statement::Expression(expression)
            | javascript::Statement::Return(expression) => expression.has(identifier),
            javascript::Statement::While { condition, body }
            | javascript::Statement::If {
                condition,
                if_true: body,
            } => condition.has(identifier) || body.has(identifier),
        }
    }

    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        match self {
            javascript::Statement::Assignment { assignment, .. } => !assignment.has(identifier),
            javascript::Statement::Expression(expression) => !expression.has(identifier),
            javascript::Statement::While { condition, body }
            | javascript::Statement::If {
                condition,
                if_true: body,
            } => !condition.has(identifier) && !body.has(identifier),
            javascript::Statement::Return(expression) => expression.in_tail_position(identifier),
        }
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        match &self {
            javascript::Statement::Return(expression) => javascript::Statement::Expression(
                expression
                    .clone()
                    .eliminate_tail_call(function_name, parameter_names),
            ),
            _ => self,
        }
    }
}
impl TailCall for javascript::ObjectKeyValue {
    fn has(&self, identifier: &javascript::Identifier) -> bool {
        match &self.value {
            Some(expression) => expression.has(identifier),
            None => self.key.eq(identifier),
        }
    }

    fn in_tail_position(&self, identifier: &javascript::Identifier) -> bool {
        !self.has(identifier)
    }

    fn eliminate_tail_call(
        self,
        function_name: &javascript::Identifier,
        parameter_names: &Vec<javascript::Identifier>,
    ) -> Self {
        javascript::ObjectKeyValue {
            key: self.key,
            value: match self.value {
                Some(expression) => Some(Box::new(
                    expression.eliminate_tail_call(function_name, parameter_names),
                )),
                None => None,
            },
        }
    }
}
