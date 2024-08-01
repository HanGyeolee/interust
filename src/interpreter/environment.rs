use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use crate::ast::{ClassMember, Expression, FieldAccess, Statement, Type};
use crate::Object;

#[derive(Debug, Clone)]
pub struct Method {
    access: FieldAccess,
    info: FunctionInfo, // FnDefine 등
}

impl Method {
    pub fn new(access: FieldAccess, info: FunctionInfo) -> Self {
        Method {
            access,
            info
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    access: FieldAccess,
    pub typ: Type,
}

impl Field {
    pub fn new(access: FieldAccess, typ: Type) -> Self {
        Field {
            access,
            typ
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    fields: HashMap<String, Field>,
    methods: HashMap<String, Method>,
}

//TODO ClassInfo
impl ClassInfo {
    pub fn get_field(&self, identifier:&String) -> Option<&Field> {
        self.fields.get(identifier)
    }
    pub fn get_method(&self, identifier:&String) -> Option<&Method> {
        self.methods.get(identifier)
    }
    pub fn get_method_info(&self, identifier:&String) -> Option<&FunctionInfo> {
        if let Some(method) = self.methods.get(identifier) {
            return Some(&method.info);
        }
        None
    }
    pub fn get_fields(&self) -> &HashMap<String, Field> {
        &self.fields
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub parameters: Vec<Expression>,
    pub body: Vec<Statement>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct MethodArea {
    classes: HashMap<String, Rc<ClassInfo>>,
    functions: HashMap<String, Rc<FunctionInfo>>,
}

impl MethodArea {
    pub fn new() -> Self {
        MethodArea {
            classes: HashMap::new(),
            functions: HashMap::new()
        }
    }

    pub fn reset(&mut self) {
        self.classes.clear();
        self.functions.clear();
    }

    pub fn add_functions(&mut self, fn_name:String, info:FunctionInfo) {
        self.functions.insert(fn_name, Rc::new(info));
    }
    pub fn get_functions(&self, fn_name:&String) -> Option<Weak<FunctionInfo>> {
        if let Some(info) = self.functions.get(fn_name){
            return Some(Rc::downgrade(info));
        }
        None
    }

    pub fn add_class_def(&mut self, class_name:String, info:ClassInfo) {
        self.classes.insert(class_name, Rc::new(info));
    }
    pub fn get_class_def(&self, class_name:&String) -> Option<Weak<ClassInfo>> {
        if let Some(info) = self.classes.get(class_name){
            return Some(Rc::downgrade(info));
        }
        None
    }
}

#[derive(Debug)]
pub struct ClassInstance {
    pub class_name: String,
    pub fields: HashMap<String, Object>,
}

#[derive(Debug)]
pub struct Heap {
    objects: HashMap<usize, ClassInstance>,
    next_id: usize,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objects: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn reset(&mut self) {
        self.objects.clear();
        self.next_id = 0;
    }

    pub fn add(&mut self, instance:ClassInstance) -> usize {
        let id = self.next_id;
        self.objects.insert(id, instance);
        self.next_id += 1;
        id
    }

    pub fn with_instance<F, R>(&mut self, id: &usize, f: F) -> Option<R>
    where F: FnOnce(&mut ClassInstance) -> R
    {
        self.objects.get_mut(id).map(f)
    }
}

#[derive(Debug)]
pub struct StackFrame {
    local_variables: HashMap<String, Object>,
    parent: Option<Rc<RefCell<StackFrame>>>,
}

impl StackFrame {
    pub fn new() -> Self {
        let e = StackFrame {
            local_variables: HashMap::new(),
            parent: None,
        };
        /*
        e.init(
            String::from("println"), Object::LibraryFn(|vec| {
                let binding = vec[0].to_string();
                let fmt:&str = binding.as_str();
                let args:Vec<Object> = vec[1..vec.len()].to_vec();
                let formatted = format_string(fmt, args);
                println!("{}", formatted);
                return Object::Null;
            })
        );
        e.init(
            String::from("print"), Object::LibraryFn(|vec| {
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
        self.local_variables.clear();
        self.parent = None;
    }

    pub fn new_with_outer(outer: Rc<RefCell<StackFrame>>) -> Self {
        StackFrame {
            local_variables: HashMap::new(),
            parent: Some(outer),
        }
    }

    /**
    현재 로컬 환경에 변수를 초기화한다.
     */
    pub fn add(&mut self, name: String, value: Object) {
        self.local_variables.insert(name, value.clone());
    }


    /**
    현재 로컬 환경에서 원하는 변수를 찾는 다.<br/>
    없는 경우 상위 환경에서 변수를 찾는 다.
     */
    pub fn get(&self, name: &String) -> Option<Object> {
        match self.local_variables.get(name) {
            Some(value) => Some(value.clone()),
            None => match self.parent {
                Some(ref parent) => parent.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn get_all(&self) -> &HashMap<String, Object> {
        &self.local_variables
    }
}

#[derive(Debug)]
pub struct InterpreterMemory {
    method_area: Rc<RefCell<MethodArea>>,
    heap: Rc<RefCell<Heap>>,
    stack: Rc<RefCell<StackFrame>>,
}

impl InterpreterMemory {
    pub fn new() -> Self {
        let method_area = MethodArea::new();
        let heap = Heap::new();
        let stack = StackFrame::new();
        InterpreterMemory {
            method_area: Rc::new(RefCell::new(method_area)),
            heap: Rc::new(RefCell::new(heap)),
            stack: Rc::new(RefCell::new(stack))
        }
    }
    pub fn from(method_area: Rc<RefCell<MethodArea>>,
               heap: Rc<RefCell<Heap>>,
               stack: Rc<RefCell<StackFrame>>) -> Self {
        InterpreterMemory {
            method_area,
            heap,
            stack
        }
    }

    pub fn reset(&self) {
        self.stack.borrow_mut().reset();
        self.heap.borrow_mut().reset();
        self.method_area.borrow_mut().reset();
    }

    pub fn set_variable(&self, identifier: String, value: Object) {
        self.stack.borrow_mut().add(identifier, value);
    }
    pub fn get_variable(&self, identifier: &String) -> Option<Object> {
        self.stack.borrow_mut().get(identifier)
    }
    pub fn set_instance(&self, class_name:String, fields: HashMap<String, Object>) -> Object{
        let instance = ClassInstance{class_name, fields};
        let id = self.heap.borrow_mut().add(instance);
        Object::Ref(id)
    }
    pub fn is_callable_member_variable(&self, id: &usize, identifier:&String) -> bool {
        if let Some(info) = self.get_class_def_from_id(&id) {
            if let Some(info) = info.upgrade() {
                if let Some(field) = info.fields.get(identifier) {
                    return field.access.is_public()
                }
            }
        }
        false
    }
    pub fn is_callable_member_method(&self, id: &usize, identifier:&String) -> (bool, bool) {
        if let Some(info) = self.get_class_def_from_id(&id) {
            if let Some(info) = info.upgrade() {
                if let Some(method) = info.methods.get(identifier) {
                    return (method.access.is_public(), method.access.is_static());
                }
            }
        }
        (false, false)
    }
    pub fn call_member_variable(&self, id: &usize, identifier:&String) -> Option<Object> {
        if let Some(instance) = self.heap.borrow().objects.get(id) {
            return instance.fields.get(identifier).cloned();
        }
        None
    }
    pub fn call_member_method(&self, id: &usize, identifier:&String) -> Option<FunctionInfo> {
        if let Some(info) = self.get_class_def_from_id(&id) {
            if let Some(info) = info.upgrade() {
                if let Some(method) = info.methods.get(identifier).cloned() {
                    return Some(method.info);
                }
            }
        }
        None
    }
    /// 클래스 내 멤버 변수를 변경할 때에만
    pub fn with_instance<F, R>(&self, id: &usize, f: F) -> Option<R>
    where F: FnOnce(&mut ClassInstance) -> R,
    {
        self.heap.borrow_mut().with_instance(id, f)
    }
    pub fn set_function(&self, fn_name:String, parameters: Vec<Expression>,
                        body: Vec<Statement>, return_type: Type,) {
        let info = FunctionInfo {
            parameters,
            body,
            return_type
        };
        self.method_area.borrow_mut().add_functions(fn_name, info);
    }

    pub fn get_function(&self, fn_name:&String) -> Option<Weak<FunctionInfo>> {
        self.method_area.borrow().get_functions(fn_name)
    }

    pub fn set_class_def(&self, class_name:String, members: Vec<ClassMember>) {
        let mut fields: HashMap<String, Field> = HashMap::new();
        let mut methods: HashMap<String, Method> = HashMap::new();

        for member in members {
            match member {
                ClassMember::Variable(access,
                        Expression::Variable(member_name, typ)) => {
                    fields.insert(member_name, Field::new(access, typ));
                },
                ClassMember::Method(access,
                        Statement::Fn {
                            identifier, parameters, body, return_type
                        }) => {
                    let info = FunctionInfo {
                        parameters,
                        body,
                        return_type
                    };
                    methods.insert(identifier, Method::new(access, info));
                }
                _ => {}
            }
        }

        let info = ClassInfo {
            fields,
            methods,
        };
        self.method_area.borrow_mut().add_class_def(class_name, info);
    }
    pub fn get_class_def(&self, class_name:&String) -> Option<Weak<ClassInfo>> {
        self.method_area.borrow().get_class_def(class_name)
    }
    pub fn get_class_def_from_id(&self, id: &usize) -> Option<Weak<ClassInfo>> {
        if let Some(instance) = self.heap.borrow().objects.get(id) {
            return self.get_class_def(&instance.class_name);
        }
        None
    }
    pub fn get_class_name_from_id(&self, id: &usize) -> Option<String> {
        if let Some(instance) = self.heap.borrow().objects.get(id) {
            return Some(instance.class_name.clone());
        }
        None
    }
    pub fn is_static_method(&self, class_name:&String, identifier:&String) -> (bool, bool) {
        if let Some(info) = self.get_class_def(class_name) {
            if let Some(info) = info.upgrade() {
                if let Some(method) = info.methods.get(identifier) {
                    return (method.access.is_public(), method.access.is_static());
                }
            }
        }
        (false, false)
    }

    pub fn push_stack(&mut self) -> Rc<RefCell<StackFrame>> {
        let current_env = Rc::clone(&self.stack);
        let scoped_env = StackFrame::new_with_outer(Rc::clone(&self.stack));
        self.stack = Rc::new(RefCell::new(scoped_env));
        current_env
    }

    pub fn pop_stack(&mut self, env: Rc<RefCell<StackFrame>>) {
        self.stack = env;
    }
}

