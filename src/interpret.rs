use std::collections::HashMap;

use crate::simple_ast::*;

#[derive(Clone, Debug)]
enum Value {
    Integer32(i32),
    TagOnlyVariant(TagOnlyVariant),
    NativeFunction(NativeFunction),
    Unit,
}

impl Value {
    fn print(self) -> String {
        match self {
            Value::Integer32(integer) => integer.to_string(),
            Value::TagOnlyVariant(variant) => variant.tag,
            Value::NativeFunction(function) => format!("<native:{:#?}>", function),
            Value::Unit => "()".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum NativeFunction {
    Print,
    Plus,
    Multiply,
}

struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Value>,
}

impl Environment {
    fn global() -> Environment {
        Environment {
            parent: None,
            variables: HashMap::from(
                [
                    ("print", Value::NativeFunction(NativeFunction::Print)),
                    ("+", Value::NativeFunction(NativeFunction::Plus)),
                    ("*", Value::NativeFunction(NativeFunction::Multiply)),
                ]
                .iter()
                .map(|(name, f)| (name.to_string(), f.clone()))
                .collect(),
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
        left: Value,
        right: Value,
    },
}

trait Eval {
    fn eval(self, env: &Environment) -> Result<Value, EvalError>;
}

pub fn intepret(expression: Expression) {
    let global = Environment::global();
    expression.eval(&global).unwrap();
}

impl Eval for Expression {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        match self {
            Expression::FunctionCall(function_call) => function_call.eval(env),
            Expression::Object(_) => todo!(),
            Expression::ObjectAccess(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::String(_) => todo!(),
            Expression::Identifier(name) => env.lookup(&name),
            Expression::Number(Number::Int32(integer)) => Ok(Value::Integer32(integer)),
            Expression::Number(Number::Float32(float)) => todo!(),
            Expression::Variant(_) => todo!(),
            Expression::TagOnlyVariant(variant) => Ok(Value::TagOnlyVariant(variant)),
        }
    }
}

impl Eval for FunctionCall {
    fn eval(self, env: &Environment) -> Result<Value, EvalError> {
        let left = self.left_argument.eval(env)?;
        let right = self.right_argument.eval(env)?;
        let function = self.function.eval(env)?;
        match function {
            Value::NativeFunction(native_function) => {
                call_native_function(native_function, left, right)
            }
            value => Err(EvalError::NotAFunction { value }),
        }
    }
}

fn call_native_function(
    function: NativeFunction,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    match function {
        NativeFunction::Print => {
            println!("{}", left.print());
            Ok(Value::Unit)
        }
        NativeFunction::Plus => match (left, right) {
            (Value::Integer32(a), Value::Integer32(b)) => Ok(Value::Integer32(a + b)),
            (left, right) => Err(EvalError::NativeFunctionCallFailed {
                function,
                left,
                right,
            }),
        },
        NativeFunction::Multiply => match (left, right) {
            (Value::Integer32(a), Value::Integer32(b)) => Ok(Value::Integer32(a * b)),
            (left, right) => Err(EvalError::NativeFunctionCallFailed {
                function,
                left,
                right,
            }),
        },
    }
}
