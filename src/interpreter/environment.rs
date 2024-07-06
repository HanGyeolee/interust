use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::Object;
use crate::Type;

#[derive(Debug, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let e = Environment {
            store: HashMap::new(),
            outer: None,
        };
        /*
        e.init(
            String::from("println"), &Object::LibraryFn(|vec| {
                let binding = vec[0].to_string();
                let fmt:&str = binding.as_str();
                let args:Vec<Object> = vec[1..vec.len()].to_vec();
                let formatted = format_string(fmt, args);
                println!("{}", formatted);
                return Object::Null;
            })
        );
        e.init(
            String::from("print"), &Object::LibraryFn(|vec| {
                let binding = vec[0].to_string();
                let fmt:&str = binding.as_str();
                let args:Vec<Object> = vec[1..vec.len()].to_vec();
                let formatted = format_string(fmt, args);
                print!("{}", formatted);
                return Object::Null;
            })
        );
        */
        e
    }

    pub fn reset(&mut self) {
        self.store.clear();
        self.outer = None;
    }

    pub fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    /**
    현재 로컬 환경에서 원하는 변수를 찾는 다.<br/>
    없는 경우 상위 환경에서 변수를 찾는 다.
    */
    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow_mut().get(name),
                None => None,
            },
        }
    }

    /**
     현재 로컬 환경에 변수를 초기화한다.
     */
    pub fn init(&mut self, name: String, value: &Object) {
        self.store.insert(name, value.clone());
    }

    /**
    현재 로컬 환경에 변수를 생성한다.
     */
    pub fn set(&mut self, name: String, typ:Type) {
        match typ {
            Type::F64 => self.store.insert(name, Object::F64(0.0)),
            Type::I64 => self.store.insert(name, Object::I64(0)),
            Type::String => self.store.insert(name, Object::String(String::new())),
            Type::Bool => self.store.insert(name, Object::Bool(false)),
            _ => self.store.insert(name, Object::Null),
        };
    }

    pub fn get_all(&self) -> &HashMap<String, Object> {
        &self.store
    }
}
