use crate::raw_ast::Token;
use crossbeam_channel::{unbounded as channel, Receiver, Sender};
use std::collections::HashMap;
use std::thread;

use itertools::Itertools;

use crate::simple_ast::*;

#[derive(Clone, Debug)]
enum Value {
    Int64(i64),
    Float32(f64),
    Boolean(bool),
    String(Token),
    TagOnlyVariant(Token),
    Variant(ValueVariant),
    NativeFunction(NativeFunction),
    ResumeFunction(ResumeFunction),
    Object(ValueObject),
    Tuple(ValueTuple),
    Function(ValueFunction),
}

#[derive(Debug, Clone)]
struct ResumeFunction {
    /// Used for blocking the current thread (which is the handler thread)
    block_current_thread: Receiver<HandlerResponse>,

    /// Used for resuming the handled thread
    resume_handled_thread: Sender<Value>,

    handler: Handler,
}

#[derive(Debug, Clone)]
struct ValueVariant {
    tag: Token,
    left: Box<Value>,
    right: Box<Value>,
}

#[derive(Debug, Clone)]
struct ValueFunction {
    closure: Environment,
    parameter: Pattern,
    body: Expression,
}

#[derive(Clone, Debug)]
struct ValueObject {
    pairs: HashMap<String, Value>,
}

#[derive(Clone, Debug)]
struct ValueTuple {
    values: Vec<Value>,
}

impl Value {
    fn print(&self) -> String {
        match self {
            Value::Int64(integer) => integer.to_string(),
            Value::Float32(float) => float.to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::TagOnlyVariant(tag) => tag.representation.clone(),
            Value::Variant(variant) => format!(
                "{} {} {}",
                variant.left.print(),
                variant.tag.representation,
                variant.right.print()
            ),
            Value::NativeFunction(function) => format!("<native:{:#?}>", function),
            Value::Tuple(tuple) => {
                let mut values = tuple.values.iter().map(|value| value.print());
                format!("({})", values.join(", "))
            }
            Value::Object(object) => {
                let mut values = object
                    .pairs
                    .iter()
                    .sorted_by(|a, b| a.0.cmp(&b.0))
                    .map(|(name, value)| format!("{}: {}", name, value.print()));
                format!("{{{}}}", values.join(", "))
            }
            Value::Function(function) => format!("<function>:({:#?})", function),
            Value::String(string) => string.representation.to_string(),
            Value::ResumeFunction(resume_function) => format!(
                "<resume_function({})>",
                resume_function.handler.name.representation
            ),
        }
    }
}

#[derive(Debug, Clone)]
enum NativeFunction {
    Print,
    Plus,
    Multiply,
    LessThan,
}

#[derive(Debug, Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Value>,
    effect_handlers_stack: Vec<EffectHandler>,
}

impl Environment {
    fn new_child(&self) -> Environment {
        Environment::new(Some(Box::new(self.clone())))
    }
    fn new(parent: Option<Box<Environment>>) -> Environment {
        Environment {
            parent,
            variables: HashMap::new(),
            effect_handlers_stack: Vec::new(),
        }
    }
    fn set_parent(mut self, parent: Environment) -> Environment {
        match self.parent {
            Some(parent) => panic!("Parent is already set"),
            None => self.parent = Some(Box::new(parent)),
        }
        self
    }
    fn combine(&mut self, other: Environment) {
        for (key, value) in other.variables {
            self.variables.insert(key, value);
        }
    }

    fn global() -> Environment {
        Environment {
            parent: None,
            variables: HashMap::from(
                [
                    ("print", Value::NativeFunction(NativeFunction::Print)),
                    ("+", Value::NativeFunction(NativeFunction::Plus)),
                    ("*", Value::NativeFunction(NativeFunction::Multiply)),
                    ("<", Value::NativeFunction(NativeFunction::LessThan)),
                ]
                .iter()
                .map(|(name, f)| (name.to_string(), f.clone()))
                .collect::<HashMap<String, Value>>(),
            ),
            effect_handlers_stack: Vec::new(),
        }
    }
    fn get_value(&self, name: &String) -> Result<Value, EvalError> {
        match self.variables.get(name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                None => Err(EvalError::UnkwownVariable { name: name.clone() }),
                Some(env) => env.get_value(name),
            },
        }
    }
    fn set_value(&mut self, name: String, value: Value) -> Result<(), EvalError> {
        self.variables.insert(name, value);
        Ok(())
    }

    fn print(&self) -> String {
        let current = self
            .variables
            .iter()
            .map(|(key, value)| format!("  {}: {}", key, value.print()))
            .join("\n");
        let parent = match &self.parent {
            Some(parent) => parent.print(),
            None => "".to_string(),
        };
        format!("(current: {}\n\nparent: {})", current, parent)
    }

    fn find_nearest_effect_handler(&self, name: Token) -> Result<EffectHandler, EvalError> {
        match self
            .effect_handlers_stack
            .iter()
            // Have to reverse because we need to find the most recently added effect handlers
            .rev()
            .find_map(|effect_handler| {
                if effect_handler.name.representation == name.representation {
                    return Some(effect_handler.clone());
                } else {
                    return None;
                }
            }) {
            Some(effect_handler) => Ok(effect_handler),
            None => match &self.parent {
                Some(parent) => parent.find_nearest_effect_handler(name),
                None => Err(EvalError::NoEffectHandlerFound { name }),
            },
        }
    }

    fn push_effect_handler(&mut self, effect_handler: EffectHandler) -> () {
        self.effect_handlers_stack.push(effect_handler);
    }

    fn pop_effect_handler(&mut self) -> () {
        self.effect_handlers_stack.pop();
    }
}

#[derive(Debug, Clone)]
pub struct EffectHandler {
    name: Token,
    function: Function,
    resume_handler_thread: Sender<HandlerResponse>,
}

#[derive(Debug, Clone)]
pub struct EffectHandlerPerformValue {
    value: Value,
    resume_handled_thread: Sender<Value>,
}

#[derive(Clone, Debug)]
pub enum EvalError {
    NoEffectHandlerFound {
        name: Token,
    },
    UnkwownVariable {
        name: String,
    },
    NotAFunction {
        value: Value,
    },
    NativeFunctionCallFailed {
        function: NativeFunction,
        argument: Value,
    },
    RefutablePattern {
        pattern: Pattern,
        value: Value,
    },
    InvalidPattern(Option<Pattern>),
}

trait Eval {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError>;
}

pub fn interpret(expression: Expression) {
    let mut global = Environment::global();
    expression.eval(&mut global).unwrap();
}

#[derive(Debug, Clone)]
enum HandlerResponse {
    BodyEvalDone(Result<Value, EvalError>),
    BodyPerform(EffectHandlerPerformValue),
}

impl Eval for Expression {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        match self {
            Expression::FunctionCall(function_call) => function_call.eval(env),
            Expression::Object(object) => object.eval(env),
            Expression::ObjectAccess(object_access) => object_access.eval(env),
            Expression::Array(_) => todo!(),
            Expression::Tuple(tuple) => tuple.eval(env),
            Expression::Function(function) => function.eval(env),
            Expression::String(string) => Ok(Value::String(string)),
            Expression::Identifier(name) => env.get_value(&name.representation),
            Expression::Number(Number::Int64(integer)) => Ok(Value::Int64(integer)),
            Expression::Number(Number::Float64(float)) => todo!(),
            Expression::Variant(variant) => Ok(Value::Variant(ValueVariant {
                left: Box::new(variant.left.eval(env)?),
                tag: variant.tag,
                right: Box::new(variant.right.eval(env)?),
            })),
            Expression::TagOnlyVariant(variant) => Ok(Value::TagOnlyVariant(variant)),
            Expression::InternalOp(op) => op.eval(env),
            Expression::Match(m) => m.eval(env),
            Expression::Conditional(conditional) => conditional.eval(env),
            Expression::Assignment(assignment) => assignment.eval(env),
            Expression::Parenthesized(parenthesized) => parenthesized.expression.eval(env),
            Expression::Branch(branch) => branch.eval(env),
            Expression::EffectHandlerNode(handler) => handler.eval(env),
            Expression::Perform(perform) => perform.eval(env),
        }
    }
}

impl Eval for Perform {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let perform = self;
        let (sender, receiver) = channel();

        // search for nearest parent handlers in environment
        let handler = env.find_nearest_effect_handler(perform.name)?;

        // Unblock the handler thread by sending a value
        handler
            .resume_handler_thread
            .send(HandlerResponse::BodyPerform(EffectHandlerPerformValue {
                value: perform.value.eval(env)?,
                resume_handled_thread: sender,
            }))
            .unwrap();

        // pause the current thread, and wait for handler to resume or kill the thread
        let captured = receiver.recv().unwrap();
        Ok(captured)
    }
}

impl Eval for EffectHandlerNode {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let handler = self;

        // The implementation is roughly based on https://www.reddit.com/r/ProgrammingLanguages/comments/tpyi6p/comment/i2fbf8c
        let (sender, receiver) = channel();

        // Push the handle function to a stack
        env.push_effect_handler(EffectHandler {
            name: handler.handler.name.clone(),
            function: handler.handler.function.clone(),
            resume_handler_thread: sender.clone(),
        });

        // Run body in a new thread
        let mut child_env = env.new_child();

        let handled = handler.handled.clone();
        thread::spawn(move || {
            sender
                .send(HandlerResponse::BodyEvalDone(handled.eval(&mut child_env)))
                .unwrap();
        });

        let result = pause_current_thread(env, receiver, handler.handler);

        // Pop the current effect handler
        env.pop_effect_handler();

        // Return the value
        result
    }
}

/// Pause current thread, wait for handled thread to call `perform` or finishes evaluation.
///
/// `receiver` is for blocking the current thread
///
/// Note: this function is meant to be used recursively.
fn pause_current_thread(
    env: &mut Environment,
    receiver: Receiver<HandlerResponse>,
    handler: Handler,
) -> Result<Value, EvalError> {
    let received = receiver.recv().unwrap();
    match received {
        HandlerResponse::BodyEvalDone(value) => value,
        HandlerResponse::BodyPerform(perform) => {
            // Inject `resume` function into current environment
            let mut env = env.new_child();
            env.set_value(
                "resume".to_string(),
                Value::ResumeFunction(ResumeFunction {
                    block_current_thread: receiver,
                    resume_handled_thread: perform.resume_handled_thread,
                    handler: handler.clone(),
                }),
            )?;
            let function = handler.function.eval(&mut env)?;
            call_function(&mut env, function, perform.value)
        }
    }
}

impl Eval for Branch {
    /// TODO: Should return Option<T> instead of bool to be sound
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        match self.condition.eval(env)? {
            Value::Boolean(boolean) => {
                if boolean {
                    self.body.eval(env)
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            _ => panic!("Condition is not a boolean"),
        }
    }
}

impl Eval for ObjectAccess {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        match self.object.eval(env)? {
            Value::Object(object) => match object.pairs.get(&self.property.representation) {
                Some(value) => Ok(value.clone()),
                None => panic!("No such property"),
            },
            _ => panic!("cannot access property of a non-object"),
        }
    }
}

impl Eval for Assignment {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let value = self.value.eval(env)?;
        if let Some(bindings) = self.pattern.matches(&value)? {
            env.combine(bindings);
        }
        Ok(value)
    }
}

impl Eval for Conditional {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        for branch in self.branches {
            match branch.condition.eval(env)? {
                Value::Boolean(boolean) => {
                    if boolean {
                        return branch.body.eval(env);
                    }
                }
                other => {
                    panic!("{:#?} is not boolean", other)
                }
            }
        }
        return self.default.eval(env);
    }
}

impl Eval for Match {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let value = self.value.eval(env)?;
        for case in self.cases {
            if let Some(bindings) = case.pattern.matches(&value)? {
                let mut combined_env = bindings.set_parent(env.clone());
                return case.body.eval(&mut combined_env);
            }
        }
        panic!("No matching cases")
    }
}

impl Pattern {
    /// Check if the given `value` is bindable to this pattern.
    /// If bindable, a list of bindings will be returned,
    /// else an error will be thrown.
    fn matches(&self, value: &Value) -> Result<Option<Environment>, EvalError> {
        match (self, value) {
            (Pattern::Identifier(pattern), value) => {
                let mut env = Environment::new(None);
                env.set_value(pattern.representation.clone(), value.clone())?;
                Ok(Some(env))
            }
            (Pattern::TagOnlyVariant(pattern), Value::TagOnlyVariant(value)) => {
                Ok(if pattern.representation == value.representation {
                    Some(Environment::new(None))
                } else {
                    None
                })
            }
            (Pattern::Variant(pattern), Value::Variant(value)) => {
                if pattern.tag.representation != value.tag.representation {
                    Ok(None)
                } else {
                    match (
                        pattern.left.matches(value.left.as_ref())?,
                        pattern.right.matches(value.right.as_ref())?,
                    ) {
                        (Some(bindings_a), Some(bindings_b)) => {
                            let mut env = Environment::new(None);
                            env.combine(bindings_a);
                            env.combine(bindings_b);
                            Ok(Some(env))
                        }
                        _ => Ok(None),
                    }
                }
            }
            (Pattern::Object(pattern), Value::Object(value)) => {
                let mut env = Environment::new(None);
                for expected_pair in &pattern.pairs {
                    match &expected_pair.pattern {
                        Some(Pattern::Identifier(name)) => {
                            match value.pairs.get(&name.representation) {
                                Some(value) => {
                                    env.set_value(name.representation.clone(), value.clone())?
                                }
                                None => return Ok(None),
                            }
                        }
                        other_pattern => {
                            return Err(EvalError::InvalidPattern(other_pattern.clone()))
                        }
                    }
                }
                Ok(Some(env))
            }
            (Pattern::Tuple(pattern), Value::Tuple(value)) => {
                if pattern.values.len() != value.values.len() {
                    return Ok(None);
                }

                let envs: Vec<Option<Environment>> = pattern
                    .values
                    .iter()
                    .zip(value.values.iter())
                    .map(|(pattern, value)| pattern.matches(&value))
                    .collect::<Result<Vec<Option<Environment>>, EvalError>>()?;

                let mut result_env = Environment::new(None);
                for env in envs {
                    if let Some(env) = env {
                        result_env.combine(env);
                    }
                }
                Ok(Some(result_env))
            }
            other => panic!("{:#?}", other),
        }
    }
}

impl Eval for Function {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        Ok(Value::Function(ValueFunction {
            closure: env.new_child(),
            parameter: *self.parameter,
            body: *self.body,
        }))
    }
}

impl Eval for InternalOp {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        match self {
            InternalOp::Add(a, b) => match (a.eval(env)?, b.eval(env)?) {
                (Value::Int64(a), Value::Int64(b)) => Ok(Value::Int64(a + b)),
                _ => panic!(),
            },
            InternalOp::Multiply(a, b) => match (a.eval(env)?, b.eval(env)?) {
                (Value::Int64(a), Value::Int64(b)) => Ok(Value::Int64(a * b)),
                _ => panic!(),
            },
            InternalOp::LessThan(a, b) => match (a.eval(env)?, b.eval(env)?) {
                (Value::Int64(a), Value::Int64(b)) => Ok(Value::Boolean(a < b)),
                _ => panic!(),
            },
        }
    }
}

impl Eval for Tuple {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        Ok(Value::Tuple(ValueTuple {
            values: self
                .values
                .into_iter()
                .map(|value| value.eval(env))
                .collect::<Result<Vec<Value>, EvalError>>()?,
        }))
    }
}

impl Eval for Object {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let mut env = env.new_child();
        for pair in self.pairs {
            let value = pair.value.eval(&mut env)?;
            if let Some(pattern) = pair.pattern {
                if let Some(bindings) = pattern.matches(&value)? {
                    env.combine(bindings);
                } else {
                    return Err(EvalError::RefutablePattern { pattern, value });
                }
            }
        }
        Ok(Value::Object(ValueObject {
            pairs: env.variables,
        }))
    }
}

fn call_function(
    env: &mut Environment,
    function: Value,
    argument: Value,
) -> Result<Value, EvalError> {
    match function {
        Value::NativeFunction(native_function) => call_native_function(native_function, argument),
        Value::Function(function) => {
            let mut env = env.new_child();

            // TODO: consider removing the following code
            // let mut env = Environment::new(Some(Box::new(function.closure)));
            if let Some(bindings) = function.parameter.matches(&argument)? {
                env.combine(bindings);
                function.body.eval(&mut env)
            } else {
                Err(EvalError::RefutablePattern {
                    pattern: function.parameter,
                    value: argument,
                })
            }
        }
        Value::ResumeFunction(resume_function) => {
            // Send a value to resume the handled thread
            resume_function
                .resume_handled_thread
                .send(argument)
                .unwrap();
            pause_current_thread(
                env,
                resume_function.block_current_thread,
                resume_function.handler,
            )
        }
        value => Err(EvalError::NotAFunction { value }),
    }
}

impl Eval for FunctionCall {
    fn eval(self, env: &mut Environment) -> Result<Value, EvalError> {
        let argument = self.argument.eval(env)?;

        let function = self.function.eval(env)?;
        call_function(env, function, argument)
    }
}

fn call_native_function(function: NativeFunction, argument: Value) -> Result<Value, EvalError> {
    fn curry_binary_op(
        a: i64,
        internal_op: &dyn Fn(Expression, Expression) -> InternalOp,
    ) -> Value {
        let parameter = Expression::Identifier(Token::dummy_identifier("x".to_string()));
        Value::Function(ValueFunction {
            closure: Environment::new(None),
            parameter: parameter.clone(),
            body: Expression::InternalOp(Box::new(internal_op(
                parameter,
                Expression::Number(Number::Int64(a)),
            ))),
        })
    }
    match function {
        NativeFunction::Print => {
            println!("{}", argument.print());
            Ok(Value::Tuple(ValueTuple { values: vec![] }))
        }
        NativeFunction::Plus => match argument {
            Value::Int64(a) => Ok(curry_binary_op(a, &InternalOp::Add)),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
        NativeFunction::Multiply => match argument {
            Value::Int64(a) => Ok(curry_binary_op(a, &InternalOp::Multiply)),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
        NativeFunction::LessThan => match argument {
            Value::Int64(a) => Ok(curry_binary_op(
                a,
                // Have to invert the arguments position because of currying
                &|a, b| InternalOp::LessThan(b, a),
            )),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
    }
}
