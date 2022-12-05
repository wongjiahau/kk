use crate::inferred_ast::InferredStatement;
use crate::non_empty::NonEmpty;
use crate::raw_ast::Token;
use crate::transpile::javascript;
use crossbeam_channel::{unbounded as channel, Receiver, Sender};
use futures::future::{BoxFuture, FutureExt};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;

use itertools::Itertools;

use crate::simple_ast::*;

#[derive(Clone, Debug)]
enum Value {
    Int64(i64),
    Float32(f64),
    Boolean(bool),
    String(String),
    TagOnlyVariant(String),
    Variant {
        tag: String,
        payload: Box<Value>
    },
    NativeFunction(NativeFunction),
    ResumeFunction(ResumeFunction),
    Object(ValueObject),
    Function(ValueFunction),
    Unit,
    Array(Vec<Value>),
}

#[derive(Debug, Clone)]
struct ResumeFunction {
    /// Used for blocking the current thread (which is the handler thread)
    block_current_thread: Receiver<HandlerResponse>,

    /// Used for resuming the handled thread
    resume_handled_thread: Sender<Evalled>,

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
    parameter: String,
    body: Vec<javascript::Statement>
}

#[derive(Clone, Debug)]
struct ValueFunctionBranch {
    parameter: Pattern,
    body: Expression,
}

#[derive(Clone, Debug)]
struct ValueObject {
    pairs: HashMap<String, Value>,
}

impl Value {
    fn print(&self) -> String {
        match self {
            Value::Int64(integer) => integer.to_string(),
            Value::Float32(float) => float.to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::TagOnlyVariant(tag) => tag.clone(),
            Value::Variant { tag, payload } => format!(
                "{}({})",
                tag,
                payload.print()
            ),
            Value::NativeFunction(function) => format!("<native:{:#?}>", function),
            Value::Object(object) => {
                let mut values = object
                    .pairs
                    .iter()
                    .sorted_by(|a, b| a.0.cmp(&b.0))
                    .map(|(name, value)| format!("{} = {}", name, value.print()));
                format!("{{ {} }}", values.join(", "))
            }
            Value::Function(function) => format!("<function>:({:#?})", function),
            Value::String(string) => format!("\"{}\"", string.to_string()),
            Value::ResumeFunction(resume_function) => format!(
                "<resume_function({})>",
                resume_function.handler.name.representation
            ),
            Value::Unit => "()".to_string(),
            Value::Array(values) => format!("[ {} ]", values.into_iter().map(|value| value.print()).collect_vec().join(", "))
        }
    }
}

#[derive(Debug, Clone)]
enum NativeFunction {
    Print,
    Plus,
    Multiply,
    LessThan,
    ReadFile,
}

#[derive(Debug, Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    bindings: HashMap<String, Value>,
    effect_handlers_stack: Vec<EffectHandler>,
}

#[derive(Debug)]
struct Promise {
    handle: tokio::task::JoinHandle<Value>,
    callback: Value,
}

impl Environment {
    fn new_child(&self) -> Environment {
        Environment::new(Some(Box::new(self.clone())))
    }
    fn new(parent: Option<Box<Environment>>) -> Environment {
        Environment {
            bindings: HashMap::new(),
            effect_handlers_stack: Vec::new(),
            parent,
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
        for (key, value) in other.bindings {
            self.bindings.insert(key, value);
        }
    }

    fn global() -> Environment {
        Environment {
            parent: None,
            bindings: HashMap::from(
                [
                    ("print_0", Value::NativeFunction(NativeFunction::Print)),
                    ("+", Value::NativeFunction(NativeFunction::Plus)),
                    ("*", Value::NativeFunction(NativeFunction::Multiply)),
                    ("<", Value::NativeFunction(NativeFunction::LessThan)),
                    ("readFile", Value::NativeFunction(NativeFunction::ReadFile)),
                ]
                .iter()
                .map(|(name, f)| (name.to_string(), f.clone()))
                .collect::<HashMap<String, Value>>(),
            ),
            effect_handlers_stack: Vec::new(),
        }
    }
    fn get_value(&self, name: &Token) -> Result<Value, EvalError> {
        match self.bindings.get(&name.representation) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                None => Err(EvalError::UnkwownVariable { name: name.clone() }),
                Some(env) => env.get_value(name),
            },
        }
    }
    fn set_value(&mut self, name: String, value: Value) -> Result<(), EvalError> {
        self.bindings.insert(name, value);
        Ok(())
    }

    fn print(&self) -> String {
        let current = self
            .bindings
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

#[derive(Debug)]
pub struct EffectHandlerPerformValue {
    value: Evalled,
    resume_handled_thread: Sender<Evalled>,
}

#[derive(Debug)]
enum EvalError {
    Return((Value, Vec<Promise>)),



    /// This is used for exiting evaluation of a handled thread that is not resumed.
    Exit,
    NoEffectHandlerFound {
        name: Token,
    },
    UnkwownVariable {
        name: Token,
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

/// Evaluated result
type Evalled = (Value, Vec<Promise>);
trait Eval {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError>;
}

pub fn interpret_statements(statements: Vec<javascript::Statement>) {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            let mut global = Environment::global();

            let promises = statements
                .into_iter()
                .map(|statement| {
                    let (_, promises) = statement.eval(&mut global).unwrap();
                    promises
                })
                .collect_vec()
                .into_iter()
                .flatten()
                .collect();

            // Refer https://rust-lang.github.io/async-book/07_workarounds/04_recursion.html
            fn run_promises(promises: Vec<Promise>) -> BoxFuture<'static, ()> {
                async move {
                    for promise in promises {
                        let (result,) = tokio::join!(promise.handle);

                        let (_, promises) = call_function(
                            &mut Environment::global(),
                            promise.callback,
                            result.unwrap(),
                        )
                        .unwrap();

                        run_promises(promises).await
                    }
                }
                .boxed()
            }
            run_promises(promises).await
        });
}

pub fn interpret(expression: Expression) {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            let mut global = Environment::global();
            let (_, promises) = expression.eval(&mut global).unwrap();

            // Refer https://rust-lang.github.io/async-book/07_workarounds/04_recursion.html
            fn run_promises(promises: Vec<Promise>) -> BoxFuture<'static, ()> {
                async move {
                    for promise in promises {
                        let (result,) = tokio::join!(promise.handle);

                        let (_, promises) = call_function(
                            &mut Environment::global(),
                            promise.callback,
                            result.unwrap(),
                        )
                        .unwrap();

                        run_promises(promises).await
                    }
                }
                .boxed()
            }
            run_promises(promises).await
        });
}

#[derive(Debug)]
enum HandlerResponse {
    BodyEvalDone(Result<Evalled, EvalError>),
    BodyPerform(EffectHandlerPerformValue),
}

impl Eval for javascript::Statement {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        match self {
            javascript::Statement::Assignment {
                is_declaration,
                assignment,
            } => {
                let (value, promises) = assignment.right.eval(env)?;
                env.set_value(assignment.left.0, value)?;
                Ok((Value::Unit, promises))
            }
            javascript::Statement::Expression(expression) => expression.eval(env),
            javascript::Statement::Return(expression) => {
                Err(EvalError::Return(expression.eval(env)?))
            }
            javascript::Statement::If { condition, if_true } => {
                let (value, promises) = condition.eval(env)?;
                match value {
                    Value::Boolean(true) => { if_true.eval(env) }

                    // O.O Javascript-style falsy value???
                    _ => Ok((Value::Unit, vec![]))

                }
            },
        }
    }
}

impl Eval for javascript::Expression {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        match self {
            javascript::Expression::Sequence(expressions) => {
                let (expression, promises) = expressions.head.eval(env)?;
                let expressions = expressions
                    .tail
                    .into_iter()
                    .map(|expression| expression.eval(env))
                    .collect::<Result<Vec<_>, _>>()?;

                let (expressions, promisess): (Vec<Value>, Vec<Vec<Promise>>) = expressions.into_iter().unzip();
                let promises = promises.into_iter().chain(promisess.into_iter().flatten()).collect();
                match expressions.split_last() {
                    Some((last, _)) => Ok((last.clone(), promises)),
                    None => Ok((expression, promises))
                }
            },
            javascript::Expression::LogicalOr { left, right } => {
                let (left, promises1) = left.eval(env)?;
                match left {
                    Value::Boolean(a) if a => {
                        // Short circuit, no need to evaluate right
                        Ok((Value::Boolean(true), promises1))
                    },
                    _ => {
                        let (right, promises2) = right.eval(env)?;

                        let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                        match right {
                            Value::Boolean(b) => {
                                Ok((Value::Boolean(b), promises))
                            }
                            _ => unreachable!("left = {:#?}, right = {:#?}", left.print(), right)
                        }
                    },
                }
            },
            javascript::Expression::LogicalAnd { left, right } => {
                let (left, promises1) = left.eval(env)?;
                match left {
                    Value::Boolean(false) => Ok((Value::Boolean(false), promises1)),
                    _ => {
                        let (right, promises2) = right.eval(env)?;
                        let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                        match right {
                            Value::Boolean(b) => {
                                Ok((Value::Boolean(b), promises))
                            }
                            _ => unreachable!("left = {:#?}, right = {:#?}", left.print(), right)
                        }

                    }
                }

            },
            javascript::Expression::Equals { left, right } => {
                let (left, promises1) = left.eval(env)?;
                let (right, promises2) = right.eval(env)?;
                let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                match (left, right) {
                    (Value::String(a), Value::String(b)) => Ok((Value::Boolean(a == b), promises)),
                    (Value::Unit, Value::Unit) => Ok((Value::Boolean(true), vec![])),
                    (left, right) => todo!("(left, right)=({:#?},{:#?})", left, right)
                }
            },
            javascript::Expression::Null => Ok((Value::Unit, vec![])),
            javascript::Expression::Boolean(boolean) => Ok((Value::Boolean(boolean), vec![])),
            javascript::Expression::Variable(identifier) => Ok((env.get_value(&Token::dummy_identifier(identifier.0)).unwrap(), vec![])),
            javascript::Expression::String(string) => Ok((Value::String(string), vec![])),
            javascript::Expression::StringConcat(expressions) => {
                let strings = expressions
                    .into_iter()
                    .map(|expression| {
                        Ok(
                            match expression.eval(env)? {
                                (Value::String(string), promises) => {
                                    Some((string, promises))
                                },
                                _ => None


                            }
                            .unwrap()
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let (strings, promises): (Vec<String>, Vec<Vec<Promise>>) = strings.into_iter().unzip();
                Ok((Value::String(strings.join("")), promises.into_iter().flatten().collect()))
            },
            javascript::Expression::Array(expressions) => {
                let (values, promises): (Vec<Value>, Vec<Vec<Promise>>) = expressions
                    .into_iter()
                    .map(|expression| expression.eval(env))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip();
                Ok((Value::Array(values), promises.into_iter().flatten().collect()))
            },
            javascript::Expression::FunctionCall { function, argument } => {
                let mut argument = argument.eval(env)?;
                let mut function = function.eval(env)?;

                let mut result = call_function(env, function.0, argument.0)?;

                let mut promises = vec![];
                promises.append(&mut argument.1);
                promises.append(&mut function.1);
                promises.append(&mut result.1);

                Ok((result.0, promises))
            }
            javascript::Expression::ArrowFunction { parameter, body } => {
                Ok((
                        Value::Function(ValueFunction {
                            closure: env.clone(),
                            parameter: parameter.0,
                            body
                        }),
                        vec![],
                        ))
            },
            javascript::Expression::Object(key_values) => {
                let mut pairs = HashMap::new();
                let mut promises = vec![];
                for key_value in key_values {
                    let (value, new_promises) = key_value.value.eval(env)?;
                    pairs.insert(key_value.key.0, value);
                    promises.extend(new_promises)
                }

                Ok((Value::Object(ValueObject { pairs }), promises))
            },
            javascript::Expression::ObjectWithSpread { spread, key_values } => {
                let (object, promises) = spread.eval(env)?;
                let mut promises = promises;
                match object {
                    Value::Object(mut object) => {
                        for key_value in key_values {
                            let (value, new_promises) = key_value.value.eval(env)?;
                            promises.extend(new_promises);
                            object.pairs.insert(key_value.key.0, value);
                        }
                        Ok((Value::Object(object), promises))

                    }
                    _ => unreachable!()
                }
                
            }
            javascript::Expression::MemberAccess { object, property } => {
                let (object, promises) = object.eval(env)?;
                match object {
                    Value::Object(object) => {
                        let value = object
                            .pairs
                            .iter()
                            .find(|pair| pair.0.eq(&property))
                            .unwrap()
                            .1
                            .clone();
                        Ok((value, promises))
                    }
                    _ => unreachable!()
                }
            },
            javascript::Expression::UnsafeJavascriptCode(_) => todo!(),
            javascript::Expression::Assignment(assignment) => {
                let (value, promises) = assignment.right.eval(env)?;
                env.set_value(assignment.left.0, value.clone())?;
                Ok((value, promises))
            }
            javascript::Expression::Float(_) => todo!(),
            javascript::Expression::Int(int) => Ok((Value::Int64(int), vec![])),
            javascript::Expression::EnumConstructor { tag, payload } => {
                match payload {
                    None => Ok((Value::TagOnlyVariant(tag), vec![])),
                    Some(payload) => {
                        let (payload, promises) = payload.eval(env)?;
                        Ok((Value::Variant { tag, payload: Box::new(payload) }, promises))
                    }
                }
            },
            javascript::Expression::HasTag { expression, tag } => {
                let (value, promises) = expression.eval(env)?;
                match value {
                    Value::Variant { tag: tag2, .. } => {
                        Ok((Value::Boolean(tag == tag2), promises))
                    }
                    Value::TagOnlyVariant(tag2) => {
                        Ok((Value::Boolean(tag == tag2), promises))
                    }
                    _ => unreachable!()
                }
            }
            javascript::Expression::GetEnumPayload(expression) => {
                let (value, promises) = expression.eval(env)?;
                match value {
                    Value::Variant { tag, payload } => {
                        Ok((*payload, promises))
                    }
                    _ => unreachable!()
                }
            }
        }
    }
}


impl Eval for Expression {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        match self {
            Expression::FunctionCall(function_call) => function_call.eval(env),
            Expression::Object(object) => object.eval(env),
            Expression::ObjectAccess(object_access) => object_access.eval(env),
            Expression::Array(_) => todo!(),
            Expression::Function(function) => function.eval(env),
            Expression::String(string) => Ok((Value::String(string.representation), Vec::new())),
            Expression::Identifier(name) => Ok((env.get_value(&name)?, Vec::new())),
            Expression::Number(Number::Int64(integer)) => Ok((Value::Int64(integer), Vec::new())),
            Expression::Number(Number::Float64(float)) => todo!(),
            Expression::Variant(variant) => {
                let mut left = variant.left.eval(env)?;
                let mut right = variant.right.eval(env)?;
                left.1.append(&mut right.1);
                Ok((
                    todo!(),
                    left.1,
                ))
            }
            Expression::TagOnlyVariant(variant) => Ok((Value::TagOnlyVariant(variant.representation), Vec::new())),
            Expression::InternalOp(op) => op.eval(env),
            Expression::Parenthesized(parenthesized) => parenthesized.expression.eval(env),
            Expression::EffectHandlerNode(handler) => handler.eval(env),
            Expression::Perform(perform) => perform.eval(env),
        }
    }
}

impl Eval for PerformEffect {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
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
        match receiver.recv() {
            // If ok, it means that resume is called
            Ok(captured) => Ok(captured),

            // If err, it means that resume is not called.
            // and if resume is not called, exit the evaluation of `handled`.
            Err(_) => Err(EvalError::Exit),
        }
    }
}

impl Eval for EffectHandlerNode {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
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
        let child_env = Arc::new(Mutex::new(env.new_child()));

        let handled = handler.handled.clone();
        thread::spawn(move || {
            match handled.eval(&mut child_env.clone().lock().unwrap()) {
                Err(EvalError::Exit) => {
                    // no need to do anything, because `handled` will only return exit if `resume` is not called
                }
                other => {
                    // If `handled` returns a non-Exit value, resume the handler thread with this
                    // value
                    sender.send(HandlerResponse::BodyEvalDone(other)).unwrap();
                }
            }
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
) -> Result<Evalled, EvalError> {
    let received = receiver.recv().unwrap();
    match received {
        HandlerResponse::BodyEvalDone(value) => value,
        HandlerResponse::BodyPerform(mut perform) => {
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
            let mut function = handler.function.eval(&mut env)?;

            let mut result = call_function(&mut env, function.0, perform.value.0)?;

            let mut promises = vec![];
            promises.append(&mut function.1);
            promises.append(&mut perform.value.1);
            promises.append(&mut result.1);

            Ok((result.0, promises))
        }
    }
}

impl Eval for ObjectAccess {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        let expr = self.object.eval(env)?;
        match expr.0 {
            Value::Object(object) => match object.pairs.get(&self.property.representation) {
                Some(value) => Ok((value.clone(), expr.1)),
                None => panic!("No such property"),
            },
            _ => panic!("cannot access property of a non-object"),
        }
    }
}

impl Pattern {
    /// Check if the given `value` is bindable to this pattern.
    /// If bindable, a list of bindings will be returned,
    /// else `None` will be returned.
    fn matches(&self, value: &Value) -> Result<Option<Environment>, EvalError> {
        match (self, value) {
            (Pattern::Identifier(pattern), value) => {
                let mut env = Environment::new(None);
                env.set_value(pattern.representation.clone(), value.clone())?;
                Ok(Some(env))
            }
            (Pattern::TagOnlyVariant(pattern), Value::TagOnlyVariant(value)) => {
                todo!()
            }
            (Pattern::Variant { left, tag, right }, Value::Variant {tag: _, payload}) => {
                todo!()
            }
            (Pattern::Object { pairs, .. }, Value::Object(value)) => {
                let mut env = Environment::new(None);
                for (key, pattern) in pairs {
                    match value.pairs.get(&key.representation) {
                        Some(value) => match pattern.matches(value)? {
                            Some(bindings) => env.combine(bindings),
                            None => (),
                        },
                        None => return Ok(None),
                    }
                }
                Ok(Some(env))
            }
            other => panic!("{:#?}", other),
        }
    }
}

impl Eval for Function {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        todo!()
    }
}

impl Eval for InternalOp {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        fn eval_native<F>(
            env: &mut Environment,
            a: Expression,
            b: Expression,
            f: F,
        ) -> Result<Evalled, EvalError>
        where
            F: Fn(Value, Value) -> Result<Evalled, EvalError>,
        {
            let mut a = a.eval(env)?;
            let mut b = b.eval(env)?;
            let mut promises = vec![];
            promises.append(&mut a.1);
            promises.append(&mut b.1);
            let mut result = f(a.0, b.0)?;
            promises.append(&mut result.1);
            Ok((result.0, promises))
        }
        match self {
            InternalOp::Add(a, b) => eval_native(env, a, b, |a, b| match (a, b) {
                (Value::Int64(a), Value::Int64(b)) => Ok((Value::Int64(a + b), vec![])),
                _ => panic!(),
            }),
            InternalOp::Multiply(a, b) => eval_native(env, a, b, |a, b| match (a, b) {
                (Value::Int64(a), Value::Int64(b)) => Ok((Value::Int64(a * b), vec![])),
                _ => panic!(),
            }),
            InternalOp::LessThan(a, b) => eval_native(env, a, b, |a, b| match (a, b) {
                (Value::Int64(a), Value::Int64(b)) => Ok((Value::Boolean(a < b), vec![])),
                _ => panic!(),
            }),
            InternalOp::ReadFile { filename, callback } => {
                eval_native(env, callback, filename, |a, b| match (a, b) {
                    (Value::String(filename), callback) => {
                        let handle = tokio::spawn(async move {
                            let content = tokio::fs::read_to_string(filename)
                                .await
                                .unwrap();
                            Value::String(content)
                        });

                        Ok((unit(), vec![Promise { handle, callback }]))
                    }
                    other => panic!("{:#?}", other),
                })
            }
        }
    }
}

impl Eval for Object {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        let mut env = env.new_child();
        let mut promises = vec![];
        for pair in self.pairs {
            let (value, mut new_promises) = pair.value.eval(&mut env)?;
            promises.append(&mut new_promises);
            let pattern = pair.key;
            if let Some(bindings) = pattern.matches(&value)? {
                env.combine(bindings);
            } else {
                return Err(EvalError::RefutablePattern { pattern, value });
            }
        }
        Ok((
            Value::Object(ValueObject {
                pairs: env.bindings,
            }),
            promises,
        ))
    }
}

fn call_function(
    env: &mut Environment,
    function: Value,
    argument: Value,
) -> Result<Evalled, EvalError> {
    match function {
        Value::NativeFunction(native_function) => {
            call_native_function(env, native_function, argument)
        }
        Value::Function(function) => {
            let mut env = {
                // TODO: think about how to make this work for both closure and effect handler
                // Note: if the logic here is wrong, it will cause Effect handler to not work
                let mut child_env = env.new_child();
                let closure = function.closure.clone();
                child_env.combine(closure);
                child_env
            };

            env.set_value(function.parameter, argument)?;

            match function.body.eval(&mut env) {
                Err(EvalError::Return((value, promises))) => {
                    return Ok((value, promises))
                }
                _ => unreachable!("One of the child statement should be Return statement")

            }

            unreachable!("Transpilation issue, the body of a function should always has a reachable Return statement");

            // If no return statement found 
            Ok((Value::Unit, vec![]))
        }
        Value::ResumeFunction(resume_function) => {
            // Send a value to resume the handled thread
            resume_function
                .resume_handled_thread
                .send((argument, vec![]))
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

impl Eval for Vec<javascript::Statement> {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        let mut promises = vec![];
        for statement in self {
            let (_, new_promises) = statement.eval(env)?;
            promises.extend(new_promises);

        };
        Ok((Value::Unit, promises))
    }
}

impl Eval for FunctionCall {
    fn eval(self, env: &mut Environment) -> Result<Evalled, EvalError> {
        let mut argument = self.argument.eval(env)?;
        let mut function = self.function.eval(env)?;

        let mut result = call_function(env, function.0, argument.0)?;

        let mut promises = vec![];
        promises.append(&mut argument.1);
        promises.append(&mut function.1);
        promises.append(&mut result.1);

        Ok((result.0, promises))
    }
}

fn call_native_function(
    env: &mut Environment,
    function: NativeFunction,
    argument: Value,
) -> Result<Evalled, EvalError> {
    fn curry_binary_op(
        env: &mut Environment,
        expression: Expression,
        internal_op: &dyn Fn(Expression, Expression) -> InternalOp,
    ) -> Value {
        let dummy = Token::dummy_identifier("x".to_string());
        Value::Function(ValueFunction {
            closure: env.clone(),
            body: todo!(),
            parameter: todo!()
                // NonEmpty {
                // head: ValueFunctionBranch {
                //     parameter: Pattern::Identifier(dummy.clone()),
                //     body: Expression::InternalOp(Box::new(internal_op(
                //         Expression::Identifier(dummy),
                //         expression,
                //     ))),
                // },
                // tail: vec![],
            // },
        })
    }
    match function {
        NativeFunction::Print => {
            println!("{}", argument.print());
            Ok((unit(), vec![]))
        }
        NativeFunction::ReadFile => match argument {
            Value::String(filename) => Ok((
                curry_binary_op(env, Expression::String(Token::dummy_identifier(filename)), &|a, b| {
                    InternalOp::ReadFile {
                        filename: a,
                        callback: b,
                    }
                }),
                vec![],
            )),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
        NativeFunction::Plus => match argument {
            Value::Int64(a) => Ok((
                curry_binary_op(env, Expression::Number(Number::Int64(a)), &InternalOp::Add),
                vec![],
            )),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
        NativeFunction::Multiply => match argument {
            Value::Int64(a) => Ok((
                curry_binary_op(
                    env,
                    Expression::Number(Number::Int64(a)),
                    &InternalOp::Multiply,
                ),
                vec![],
            )),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
        NativeFunction::LessThan => match argument {
            Value::Int64(a) => Ok((
                curry_binary_op(
                    env,
                    Expression::Number(Number::Int64(a)),
                    // Have to invert the arguments position because of currying
                    &|a, b| InternalOp::LessThan(b, a),
                ),
                vec![],
            )),
            argument => Err(EvalError::NativeFunctionCallFailed { function, argument }),
        },
    }
}

fn unit() -> Value {
    Value::Object(ValueObject {
        pairs: HashMap::new(),
    })
}
