use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use crate::interpreter::environment::Environment;
use crate::ast::{Expression, Statement, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    F64(f64),
    I64(i64),
    Bool(bool),
    String(String),
    Fn(Vec<Expression>, Vec<Statement>, Rc<RefCell<Environment>>, Type),
    LibraryFn(fn(Vec<Object>) -> Object),
    Null,
    ReturnValue(Object),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::F64(ref value) => write!(f, "{value}"),
            Object::I64(ref value) => write!(f, "{value}"),
            Object::Bool(ref value) => write!(f, "{value}"),
            Object::String(ref value) => write!(f, "{value}"),
            Object::Fn(ref params, _, _, ref return_type) => {
                let mut result = String::new();

                for (i, s) in params.iter().enumerate() {
                    let Expression::Variable(name, _) = s else {
                        return write!(f,
                            "wrong expression: {:?} expected but {:?} given",
                            Expression::Variable("".to_string(), Type::None),
                            s,
                        );
                    };
                    if i < 1 {
                        result.push_str(&format!("{name}"));
                    } else {
                        result.push_str(&format!(", {name}"));
                    }
                }

                write!(f, "fn({result}) -> {return_type} {{ ... }}")
            }
            Object::LibraryFn(_) => write!(f, "LibraryFunction"),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{value}"),
            Object::Error(ref value) => write!(f, "{value}"),
        }
    }
}

impl Object {
    pub fn get_type(&self) -> Type{
        match self {
            Object::F64(_) => Type::F64,
            Object::I64(_) => Type::I64,
            Object::String(_) => Type::String,
            Object::Bool(_) => Type::Bool,
            Object::Null => Type::None,
            _ => Type::None
        }
    }
}