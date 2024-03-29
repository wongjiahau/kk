use crate::innate_function::InnateFunction;
use crate::non_empty::NonEmpty;
use crate::raw_ast::Token;
use crate::transpile::interpretable::{self, *};
use futures::future::{BoxFuture, FutureExt};
use std::collections::HashMap;

use itertools::Itertools;

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    TagOnlyVariant(String),
    Variant { tag: String, payload: Box<Value> },
    Object(ValueObject),
    Function(ValueFunction),
    Unit,
    Array(Vec<Value>),
    Tuple(Box<NonEmpty<Value>>),
}

#[derive(Debug, Clone)]
struct ValueFunction {
    closure: Environment,
    parameter: String,
    body: Vec<interpretable::Statement>,
}

#[derive(Clone, Debug)]
struct ValueObject {
    pairs: HashMap<String, Value>,
}

impl Value {
    fn print(&self) -> String {
        match self {
            Value::Int(integer) => integer.to_string(),
            Value::Float(float) => float.to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::TagOnlyVariant(tag) => tag.clone(),
            Value::Variant { tag, payload } => format!("{}({})", tag, payload.print()),
            Value::Object(object) => {
                let mut values = object
                    .pairs
                    .iter()
                    .sorted_by(|a, b| a.0.cmp(&b.0))
                    .map(|(name, value)| format!("{} = {}", name, value.print()));
                format!("{{ {} }}", values.join(", "))
            }
            Value::Function(_) => format!("<function>"),
            Value::String(string) => format!("\"{}\"", string.to_string()),
            Value::Unit => "()".to_string(),
            Value::Array(values) => format!(
                "[ {} ]",
                values
                    .into_iter()
                    .map(|value| value.print())
                    .collect_vec()
                    .join(", ")
            ),
            Value::Tuple(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    bindings: HashMap<String, Value>,
}

#[derive(Debug)]
struct Promise {
    handle: tokio::task::JoinHandle<Value>,
    callback: ValueFunction,
}

impl Environment {
    fn new_child(&self) -> Environment {
        Environment::new(Some(Box::new(self.clone())))
    }
    fn new(parent: Option<Box<Environment>>) -> Environment {
        Environment {
            bindings: HashMap::new(),
            parent,
        }
    }
    fn combine(&mut self, other: Environment) {
        for (key, value) in other.bindings {
            self.bindings.insert(key, value);
        }
    }

    fn global() -> Environment {
        Environment {
            parent: None,
            bindings: HashMap::new(),
        }
    }
    fn get_value(&self, name: &Token) -> Result<Value, ControlFlow> {
        match self.bindings.get(&name.representation) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                Some(env) => env.get_value(name),
                None => unreachable!(),
            },
        }
    }
    fn set_value(&mut self, name: String, value: Value) -> Result<(), ControlFlow> {
        self.bindings.insert(name, value);
        Ok(())
    }
}

#[derive(Debug)]
enum ControlFlow {
    Return((Value, Vec<Promise>)),
}

/// Evaluated result
type Evalled = (Value, Vec<Promise>);
trait Eval {
    fn eval(self, env: &mut Environment) -> Result<Evalled, ControlFlow>;
}

pub fn interpret_statements(statements: Vec<interpretable::Statement>) {
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

impl Eval for interpretable::Statement {
    fn eval(self, env: &mut Environment) -> Result<Evalled, ControlFlow> {
        match self {
            interpretable::Statement::Assignment { assignment } => {
                let (value, promises) = assignment.right.eval(env)?;
                env.set_value(assignment.left.0, value)?;
                Ok((Value::Unit, promises))
            }
            interpretable::Statement::Expression(expression) => expression.eval(env),
            interpretable::Statement::Return(expression) => {
                Err(ControlFlow::Return(expression.eval(env)?))
            }
            interpretable::Statement::If { condition, if_true } => {
                let (value, promises1) = condition.eval(env)?;
                match value {
                    Value::Boolean(true) => {
                        let (value, promises2) = if_true.eval(env)?;
                        let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                        Ok((value, promises))
                    }

                    // O.O Javascript-style falsy value???
                    _ => Ok((Value::Unit, vec![])),
                }
            }
        }
    }
}

impl Eval for NonEmpty<interpretable::Expression> {
    fn eval(self, env: &mut Environment) -> Result<Evalled, ControlFlow> {
        let (expression, promises) = self.head.eval(env)?;
        let expressions = self
            .tail
            .into_iter()
            .map(|expression| expression.eval(env))
            .collect::<Result<Vec<_>, _>>()?;

        let (expressions, promisess): (Vec<Value>, Vec<Vec<Promise>>) =
            expressions.into_iter().unzip();
        let promises = promises
            .into_iter()
            .chain(promisess.into_iter().flatten())
            .collect();
        Ok((
            Value::Tuple(Box::new(NonEmpty {
                head: expression,
                tail: expressions,
            })),
            promises,
        ))
    }
}

impl Eval for interpretable::Expression {
    fn eval(self, env: &mut Environment) -> Result<Evalled, ControlFlow> {
        match self {
            interpretable::Expression::Sequence(expressions) => {
                let (expression, promises) = expressions.head.eval(env)?;
                let expressions = expressions
                    .tail
                    .into_iter()
                    .map(|expression| expression.eval(env))
                    .collect::<Result<Vec<_>, _>>()?;

                let (expressions, promisess): (Vec<Value>, Vec<Vec<Promise>>) =
                    expressions.into_iter().unzip();
                let promises = promises
                    .into_iter()
                    .chain(promisess.into_iter().flatten())
                    .collect();
                match expressions.split_last() {
                    Some((last, _)) => Ok((last.clone(), promises)),
                    None => Ok((expression, promises)),
                }
            }
            interpretable::Expression::LogicalOr { left, right } => {
                let (left, promises1) = left.eval(env)?;
                match left {
                    Value::Boolean(a) if a => {
                        // Short circuit, no need to evaluate right
                        Ok((Value::Boolean(true), promises1))
                    }
                    _ => {
                        let (right, promises2) = right.eval(env)?;

                        let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                        match right {
                            Value::Boolean(b) => Ok((Value::Boolean(b), promises)),
                            _ => unreachable!("left = {:#?}, right = {:#?}", left.print(), right),
                        }
                    }
                }
            }
            interpretable::Expression::LogicalAnd { left, right } => {
                let (left, promises1) = left.eval(env)?;
                match left {
                    Value::Boolean(false) => Ok((Value::Boolean(false), promises1)),
                    _ => {
                        let (right, promises2) = right.eval(env)?;
                        let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                        match right {
                            Value::Boolean(b) => Ok((Value::Boolean(b), promises)),
                            _ => unreachable!("left = {:#?}, right = {:#?}", left.print(), right),
                        }
                    }
                }
            }
            interpretable::Expression::Equals { left, right } => {
                let (left, promises1) = left.eval(env)?;
                let (right, promises2) = right.eval(env)?;
                let promises = promises1.into_iter().chain(promises2.into_iter()).collect();
                match (left, right) {
                    (Value::String(a), Value::String(b)) => Ok((Value::Boolean(a == b), promises)),
                    (Value::Unit, Value::Unit) => Ok((Value::Boolean(true), vec![])),
                    (left, right) => todo!("(left, right)=({:#?},{:#?})", left, right),
                }
            }
            interpretable::Expression::Null => Ok((Value::Unit, vec![])),
            interpretable::Expression::Boolean(boolean) => Ok((Value::Boolean(boolean), vec![])),
            interpretable::Expression::Variable(identifier) => Ok((
                env.get_value(&Token::dummy_identifier(identifier.0))
                    .unwrap(),
                vec![],
            )),
            interpretable::Expression::String(string) => Ok((Value::String(string), vec![])),
            interpretable::Expression::StringConcat(expressions) => {
                let strings = expressions
                    .into_iter()
                    .map(|expression| {
                        Ok(match expression.eval(env)? {
                            (Value::String(string), promises) => Some((string, promises)),
                            _ => None,
                        }
                        .unwrap())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let (strings, promises): (Vec<String>, Vec<Vec<Promise>>) =
                    strings.into_iter().unzip();
                Ok((
                    Value::String(strings.join("")),
                    promises.into_iter().flatten().collect(),
                ))
            }
            interpretable::Expression::Array(expressions) => {
                let (values, promises): (Vec<Value>, Vec<Vec<Promise>>) = expressions
                    .into_iter()
                    .map(|expression| expression.eval(env))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip();
                Ok((
                    Value::Array(values),
                    promises.into_iter().flatten().collect(),
                ))
            }
            interpretable::Expression::FunctionCall { function, argument } => {
                let (argument, promises1) = argument.eval(env)?;
                let (function, promises2) = function.eval(env)?;

                let function = match function {
                    Value::Function(function) => function,
                    _ => unreachable!(),
                };

                let (result, promises3) = call_function(env, function, argument)?;

                let promises = promises1
                    .into_iter()
                    .chain(promises2.into_iter())
                    .chain(promises3.into_iter())
                    .collect();

                Ok((result, promises))
            }
            interpretable::Expression::ArrowFunction { parameter, body } => Ok((
                Value::Function(ValueFunction {
                    closure: env.clone(),
                    parameter: parameter.0,
                    body,
                }),
                vec![],
            )),
            interpretable::Expression::Object(key_values) => {
                let mut pairs = HashMap::new();
                let mut promises = vec![];
                for key_value in key_values {
                    let (value, new_promises) = key_value.value.eval(env)?;
                    pairs.insert(key_value.key.0, value);
                    promises.extend(new_promises)
                }

                Ok((Value::Object(ValueObject { pairs }), promises))
            }
            interpretable::Expression::ObjectWithSpread { spread, key_values } => {
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
                    _ => unreachable!(),
                }
            }
            interpretable::Expression::MemberAccess { object, property } => {
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
                    _ => unreachable!(),
                }
            }
            interpretable::Expression::Assignment(assignment) => {
                let (value, promises) = assignment.right.eval(env)?;
                env.set_value(assignment.left.0, value.clone())?;
                Ok((value, promises))
            }
            interpretable::Expression::Float(float) => Ok((Value::Float(float), vec![])),
            interpretable::Expression::Int(int) => Ok((Value::Int(int), vec![])),
            interpretable::Expression::EnumConstructor { tag, payload } => match payload {
                None => Ok((Value::TagOnlyVariant(tag), vec![])),
                Some(payload) => {
                    let (payload, promises) = payload.eval(env)?;
                    Ok((
                        Value::Variant {
                            tag,
                            payload: Box::new(payload),
                        },
                        promises,
                    ))
                }
            },
            interpretable::Expression::HasTag { expression, tag } => {
                let (value, promises) = expression.eval(env)?;
                match value {
                    Value::Variant { tag: tag2, .. } => Ok((Value::Boolean(tag == tag2), promises)),
                    Value::TagOnlyVariant(tag2) => Ok((Value::Boolean(tag == tag2), promises)),
                    _ => unreachable!(),
                }
            }
            interpretable::Expression::GetEnumPayload(expression) => {
                let (value, promises) = expression.eval(env)?;
                match value {
                    Value::Variant { payload, .. } => Ok((*payload, promises)),
                    _ => unreachable!(),
                }
            }
            Expression::Tuple(elements) => elements.eval(env),
            Expression::InnateFunctionCall { function, argument } => match function {
                InnateFunction::IntAdd => {
                    let (value, promises) = argument.eval(env)?;
                    match value {
                        Value::Tuple(elements) => match (&elements.head, &elements.tail[0]) {
                            (Value::Int(a), Value::Int(b)) => Ok((Value::Int(a + b), promises)),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                }
                InnateFunction::Print => {
                    let (value, promises) = argument.eval(env)?;
                    println!("{}", value.print());
                    Ok((Value::Unit, promises))
                }
                InnateFunction::ReadFile => {
                    let (value, promises) = argument.eval(env)?;
                    match value {
                        Value::Tuple(values) => {
                            let filename = match values.head {
                                Value::String(string) => string,
                                _ => unreachable!(),
                            };
                            let callback = match &values.tail[0] {
                                Value::Function(function) => function,
                                _ => unreachable!(),
                            };
                            let handle = tokio::spawn(async move {
                                let content = tokio::fs::read_to_string(filename).await.unwrap();
                                Value::String(content.trim().to_string())
                            });

                            Ok((
                                unit(),
                                vec![Promise {
                                    handle,
                                    callback: callback.clone(),
                                }]
                                .into_iter()
                                .chain(promises)
                                .collect(),
                            ))
                        }
                        _ => unreachable!(),
                    }
                }
            },
            Expression::TupleAccess { tuple, index } => {
                let (value, promises) = tuple.eval(env)?;
                match value {
                    Value::Tuple(tuple) => {
                        let value = if index == 0 {
                            tuple.head
                        } else {
                            tuple.tail[index - 1].clone()
                        };
                        Ok((value, promises))
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}

fn call_function(
    env: &mut Environment,
    function: ValueFunction,
    argument: Value,
) -> Result<Evalled, ControlFlow> {
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
        Err(ControlFlow::Return((value, promises))) => return Ok((value, promises)),
        _ => unreachable!("One of the child statement should be Return statement"),
    }
}

impl Eval for Vec<interpretable::Statement> {
    fn eval(self, env: &mut Environment) -> Result<Evalled, ControlFlow> {
        let mut promises = vec![];
        for statement in self {
            let (_, new_promises) = statement.eval(env)?;
            promises.extend(new_promises);
        }
        Ok((Value::Unit, promises))
    }
}

fn unit() -> Value {
    Value::Object(ValueObject {
        pairs: HashMap::new(),
    })
}
