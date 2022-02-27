use std::collections::HashMap;

use itertools::Itertools;

use crate::simple_ast::*;

#[derive(Clone, Debug)]
enum Value {
    Int64(i64),
    Float32(f64),
    Boolean(bool),
    String(String),
    TagOnlyVariant(String),
    Variant(ValueVariant),
    NativeFunction(NativeFunction),
    Object(ValueObject),
    Tuple(ValueTuple),
    Function(ValueFunction),
}

#[derive(Debug, Clone)]
struct ValueVariant {
    tag: String,
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
            Value::TagOnlyVariant(tag) => tag.clone(),
            Value::Variant(variant) => format!(
                "{} {} {}",
                variant.left.print(),
                variant.tag,
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
            Value::Function(_) => "<function>".to_string(),
            Value::String(string) => string.to_string(),
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
}

impl Environment {
    fn new_child(&self) -> Environment {
        Environment::new(Some(Box::new(self.clone())))
    }
    fn new(parent: Option<Box<Environment>>) -> Environment {
        Environment {
            parent,
            variables: HashMap::new(),
        }
    }
    fn set_parent(mut self, parent: Environment) -> Environment {
        match self.parent {
            Some(parent) => panic!("Parent is already set"),
            None => self.parent = Some(Box::new(parent)),
        }
        self
    }
    fn combine(mut self, other: Environment) -> Environment {
        for (key, value) in other.variables {
            self.variables.insert(key, value);
        }
        self
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
        }
    }
    fn lookup(&self, name: &String) -> Result<Value, EvalError> {
        match self.variables.get(name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                None => Err(EvalError::UnkwownVariable { name: name.clone() }),
                Some(env) => env.lookup(name),
            },
        }
    }
    fn set(&mut self, name: String, value: Value) -> Result<(), EvalError> {
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
}

#[derive(Debug)]
pub enum EvalError {
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
    fn eval(self, env: &Environment) -> Result<Value, EvalError>;
}

pub fn interpret(expression: Expression) {
    let global = Environment::global();
    expression.eval(&global).unwrap();
}

impl Eval for Expression {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        match self {
            Expression::FunctionCall(function_call) => function_call.eval(env),
            Expression::Object(object) => object.eval(env),
            Expression::ObjectAccess(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Tuple(tuple) => tuple.eval(env),
            Expression::Function(function) => function.eval(env),
            Expression::String(string) => Ok(Value::String(string)),
            Expression::Identifier(name) => env.lookup(&name),
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
        }
    }
}

impl Eval for Conditional {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
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
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        let value = self.value.eval(env)?;
        for case in self.cases {
            if let Some(bindings) = case.pattern.matches(&value)? {
                let combined_env = bindings.set_parent(env.clone());
                return case.body.eval(&combined_env);
            }
        }
        panic!("No matching cases")
    }
}

impl Pattern {
    fn matches(&self, value: &Value) -> Result<Option<Environment>, EvalError> {
        match (self, value) {
            (Pattern::Identifier(pattern), value) => {
                let mut env = Environment::new(None);
                env.set(pattern.clone(), value.clone());
                Ok(Some(env))
            }
            (Pattern::TagOnlyVariant(pattern), Value::TagOnlyVariant(value)) => {
                Ok(if pattern == value {
                    Some(Environment::new(None))
                } else {
                    None
                })
            }
            (Pattern::Variant(pattern), Value::Variant(value)) => {
                if pattern.tag != value.tag {
                    Ok(None)
                } else {
                    match (
                        pattern.left.matches(value.left.as_ref())?,
                        pattern.right.matches(value.right.as_ref())?,
                    ) {
                        (Some(bindings_a), Some(bindings_b)) => {
                            Ok(Some(bindings_a.combine(bindings_b)))
                        }
                        _ => Ok(None),
                    }
                }
            }
            (Pattern::Object(pattern), Value::Object(value)) => {
                let mut env = Environment::new(None);
                for expected_pair in &pattern.pairs {
                    match &expected_pair.pattern {
                        Some(Pattern::Identifier(name)) => match value.pairs.get(name) {
                            Some(value) => env.set(name.clone(), value.clone())?,
                            None => return Ok(None),
                        },
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

                let mut resultEnv = Environment::new(None);
                for env in envs {
                    if let Some(env) = env {
                        resultEnv = resultEnv.combine(env);
                    }
                }
                Ok(Some(resultEnv))
            }
            other => panic!("{:#?}", other),
        }
    }
}

impl Eval for Function {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        Ok(Value::Function(ValueFunction {
            closure: env.new_child(),
            parameter: *self.parameter,
            body: *self.body,
        }))
    }
}

impl Eval for InternalOp {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
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
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
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
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        let mut env = env.new_child();
        for pair in self.pairs {
            let value = pair.value.eval(&env)?;
            if let Some(pattern) = pair.pattern {
                if let Some(bindings) = pattern.matches(&value)? {
                    env = env.combine(bindings);
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

impl Eval for FunctionCall {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        let argument = self.argument.eval(env)?;
        let function = self.function.eval(env)?;
        match function {
            Value::NativeFunction(native_function) => {
                call_native_function(native_function, argument)
            }
            Value::Function(function) => {
                let env = Environment::new(Some(Box::new(function.closure)));
                if let Some(bindings) = function.parameter.matches(&argument)? {
                    let env = env.combine(bindings);
                    function.body.eval(&env)
                } else {
                    Err(EvalError::RefutablePattern {
                        pattern: function.parameter,
                        value: argument,
                    })
                }
            }
            value => Err(EvalError::NotAFunction { value }),
        }
    }
}

fn call_native_function(function: NativeFunction, argument: Value) -> Result<Value, EvalError> {
    fn curry_binary_op(
        a: i64,
        internal_op: &dyn Fn(Expression, Expression) -> InternalOp,
    ) -> Value {
        let parameter = Expression::Identifier("x".to_string());
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
