use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use crate::interpreter::environment::*;
use crate::{Expression, Infix, Literal, Object, Prefix, Program, Statement, Type};

pub mod environment;

#[derive(Debug)]
pub struct Interpreter {
    memory: InterpreterMemory,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            memory: InterpreterMemory::new()
        }
    }
    pub fn from(method_area: Rc<RefCell<MethodArea>>,
               heap: Rc<RefCell<Heap>>,
               stack: Rc<RefCell<StackFrame>>) -> Self {
        Interpreter {
            memory: InterpreterMemory::from(method_area, heap, stack)
        }
    }

    pub fn reset(&self) {
        self.memory.reset();
    }

    /**
    Type 에 맞게 Object Casting
     */
    fn eval_cast(&self, typ: &Type, object: &Object) -> Object{
        match typ {
            Type::F64 => match object {
                Object::F64(v) => Object::F64(v.clone()),
                Object::I64(v) => Object::F64(v.clone() as f64),
                Object::Bool(v) => Object::F64(if v.clone() { 1.0 } else { 0.0 }),
                _ => Object::Error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::I64 => match object {
                Object::F64(v) => Object::I64(v.clone() as i64),
                Object::I64(v) => Object::I64(v.clone()),
                Object::Bool(v) => Object::I64(if v.clone() { 1 } else { 0 }),
                _ => Object::Error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::String => match object {
                Object::String(v) => Object::String(v.clone()),
                _ => Object::Error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::Bool => Object::Bool(is_truthy(object)),
            Type::Class(name) => match object {
                Object::Ref(id) => {
                    if let Some(class_name) = self.memory.get_class_name_from_id(id){
                        if class_name.eq(name) {
                            return object.clone();
                        }
                        return Object::Error(format!(
                            "cannot cast: class {class_name} to class {name}",
                        ));
                    }
                    Object::Error(format!(
                        "cannot cast: {object} to  class {name}",
                    ))
                },
                _ => Object::Error(format!(
                    "cannot cast: {object} to  class {name}",
                )),
            }
            _ => object.clone()
        }
    }

    /**
     코드 실행
     */
    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(*value),
                Some(Object::Error(msg)) => return Some(Object::Error(msg.clone())),
                object => result = object,
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Let{variable, expression} => {
                if let Some(init) = expression {
                    let value = self.eval_expression(init)
                        .unwrap_or_else(|| Object::Error(String::from("RuntimeError")));

                    if is_error(&value) {
                        return Some(value);
                    } else {
                        let Expression::Variable(name, typ) = variable else {
                            return Some(Object::Error(format!(
                                "wrong expression: {:?} expected but {:?} given",
                                Expression::Variable("".to_string(), Type::None),
                                variable,
                            )));
                        };
                        let cast = self.eval_cast(&typ, &value);
                        self.memory.set_variable(name, cast);
                    }
                }
                else {
                    let Expression::Variable(name, typ) = variable else {
                        return Some(Object::Error(format!(
                            "wrong expression: {:?} expected but {:?} given",
                            Expression::Variable("".to_string(), Type::None),
                            variable,
                        )));
                    };
                    let value = match self.eval_expression(
                        Expression::Literal(Literal::None)
                    ) {
                        Some(value) => value,
                        None => return None,
                    };
                    let cast = self.eval_cast(&typ, &value);
                    self.memory.set_variable(name, cast);
                }

                return None;
            }
            Statement::Fn { identifier, parameters, body, return_type } => {
                self.memory.set_function(identifier, parameters, body, return_type);
                return None;
            },
            Statement::Class {identifier, members} => {
                self.memory.set_class_def(identifier, members);
                return None;
            },
            Statement::Return(expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                if is_error(&value) {
                    Some(value)
                } else {
                    Some(Object::ReturnValue(Box::new(value)))
                }
            }
            Statement::Expression(expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                Some(value)
            }
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Identifier(identifier) => Some(self.eval_identifier(identifier)),
            Expression::Insert {
                variable,
                expression
            } => self.eval_insert_expression(*variable, *expression),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(*condition, consequence, alternative),
            Expression::Call {
                identifier,
                arguments,
            } => Some(self.eval_call_expression(identifier, arguments)),
            Expression::CallMember {
                identifier,
                call
            } => Some(self.eval_call_member(identifier, *call)),
            Expression::CallStaticMember {
                identifier,
                call
            } => Some(self.eval_call_static_member(identifier, *call)),
            Expression::ClassInstance {
                identifier,
                inits
            } => {
                let mut fields = HashMap::new();
                for insert in inits {
                    if let Expression::Insert {
                        variable,
                        expression
                    } = insert {
                        if let Expression::Identifier(field_name) = *variable {
                            if let Some(obj) = self.eval_expression(*expression) {
                                fields.insert(field_name, obj.clone());
                            }
                        }
                    }
                }
                if let Some(class_info) = self.memory.get_class_def(&identifier){
                    if let Some(class_info) = class_info.upgrade() {
                        for (key, field) in class_info.get_fields() {
                            if let None = fields.get(key){
                                fields.insert(key.clone(), field.typ.default());
                            }
                        }
                    }
                }
                return Some(self.memory.set_instance(identifier, fields));
            }
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            Expression::Prefix(prefix, right_expression) => {
                if let Some(right) = self.eval_expression(*right_expression) {
                    Some(self.eval_prefix_expression(prefix, right))
                } else {
                    None
                }
            }
            Expression::Infix(infix, left_expression, right_expression) => {
                let left = self.eval_expression(*left_expression);
                let right = self.eval_expression(*right_expression);

                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expression(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            _ => None
        }
    }

    /**
     식별자 찾기<br/>
    변수 혹은 함수 이름, 클래스 이름 등
     */
    fn eval_identifier(&mut self, identifier: String) -> Object {
        self.memory.get_variable(&identifier)
            .unwrap_or_else(|| Object::Error(format!("identifier not found: {identifier}")))
    }

    /**
     Primitive 타입 연결
     */
    fn eval_literal(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::String(value) => Object::String(value),
            Literal::F64(value) => Object::F64(value),
            Literal::I64(value) => Object::I64(value),
            Literal::Bool(value) => Object::Bool(value),
            Literal::None => Object::Null
        }
    }

    /**
     접두사 연결
     */
    fn eval_prefix_expression(&mut self, prefix: Prefix, right: Object) -> Object {
        match prefix {
            Prefix::Not => self.eval_not_operator_expression(right),
            Prefix::Minus => self.eval_minus_prefix_expression(right),
        }
    }

    /**
    Not 접두사 연결
     */
    fn eval_not_operator_expression(&mut self, right: Object) -> Object {
        Object::Bool(!is_truthy(&right))
    }

    /**
    음수 접두사 연결
     */
    fn eval_minus_prefix_expression(&mut self, right: Object) -> Object {
        match right {
            Object::F64(value) => Object::F64(-value),
            Object::I64(value) => Object::I64(-value),
            _ => Object::Error(format!("unknown operator: -{right}")),
        }
    }

    /**
     삽입사 연결
     */
    fn eval_infix_expression(&mut self, infix: Infix, left: Object, right: Object) -> Object {
        match left {
            Object::I64(left_value) => {
                if let Object::I64(right_value) = right {
                    self.eval_infix_integer_expression(infix, left_value.clone(), right_value.clone())
                } else if let Object::F64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value.clone() as f64, right_value.clone())
                } else {
                    Object::Error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            Object::F64(left_value) => {
                if let Object::F64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value.clone(), right_value.clone())
                } else if let Object::I64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value.clone(), right_value.clone() as f64)
                }  else {
                    Object::Error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            Object::Bool(left_value) => {
                if let Object::Bool(right_value) = right {
                    self.eval_infix_boolean_expression(infix, left_value.clone(), right_value.clone())
                } else {
                    Object::Error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            _ => Object::Error(format!("unknown operator: {left} {infix} {right}")),
        }
    }


    /**
    I64 계산 방식 연결
     */
    fn eval_infix_integer_expression(
        &mut self,
        infix: Infix,
        left_value: i64,
        right_value: i64,
    ) -> Object {
        match infix {
            Infix::Plus =>          Object::I64(left_value + right_value),
            Infix::Minus =>         Object::I64(left_value - right_value),
            Infix::Multiply =>      Object::I64(left_value * right_value),
            Infix::Divide =>        Object::I64(left_value / right_value),
            Infix::Mod =>           Object::I64(left_value % right_value),
            Infix::Equal =>         Object::Bool(left_value == right_value),
            Infix::NotEqual =>      Object::Bool(left_value != right_value),
            Infix::LessThan =>      Object::Bool(left_value < right_value),
            Infix::GreaterThan =>   Object::Bool(left_value > right_value),
            Infix::BitAnd =>        Object::I64(left_value & right_value),
            Infix::BitOr =>         Object::I64(left_value | right_value),
            _ => Object::Error(format!("unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    F64 계산 방식  연결
     */
    fn eval_infix_float_expression(
        &mut self,
        infix: Infix,
        left_value: f64,
        right_value: f64,
    ) -> Object {
        match infix {
            Infix::Plus =>          Object::F64(left_value + right_value),
            Infix::Minus =>         Object::F64(left_value - right_value),
            Infix::Multiply =>      Object::F64(left_value * right_value),
            Infix::Divide =>        Object::F64(left_value / right_value),
            Infix::Equal =>         Object::Bool(left_value == right_value),
            Infix::NotEqual =>      Object::Bool(left_value != right_value),
            Infix::LessThan =>      Object::Bool(left_value < right_value),
            Infix::GreaterThan =>   Object::Bool(left_value > right_value),
            _ => Object::Error(format!("unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    논리 계산 방식  연결
     */
    fn eval_infix_boolean_expression(
        &mut self,
        infix: Infix,
        left_value: bool,
        right_value: bool,
    ) -> Object {
        match infix {
            Infix::Equal =>     Object::Bool(left_value == right_value),
            Infix::NotEqual =>  Object::Bool(left_value != right_value),
            Infix::And =>       Object::Bool(left_value && right_value),
            Infix::Or =>        Object::Bool(left_value || right_value),
            Infix::BitAnd =>    Object::Bool(left_value & right_value),
            Infix::BitOr =>     Object::Bool(left_value | right_value),
            _ => Object::Error(format!(
                "unknown operator: {left_value} {infix} {right_value}",
            )),
        }
    }

    /**
    Insert 삽입문 연결
     */
    fn eval_insert_expression(
        &mut self,
        variable: Expression,
        expression: Expression,
    ) -> Option<Object> {
        match variable {
            Expression::Identifier(name) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                if is_error(&value) {
                    Some(value)
                } else {
                    let object = self.memory.get_variable(&name);

                    if let Some(object) = object {
                        let typ = object.get_type();
                        let cast = self.eval_cast(&typ, &value);
                        self.memory.set_variable(name, cast);
                    }
                    None
                }
            },
            Expression::CallMember { identifier, call } => {
                let object = self.memory.get_variable(&identifier);

                if let Some(Object::Ref(id)) = object {
                    return self.eval_insert_class_member(identifier=="self" , &id, *call.clone(), expression);
                }
                None
            },
            _ => return Some(Object::Error(format!(
                "wrong expression: {:?} expected but {:?} given",
                Expression::Variable("".to_string(), Type::None),
                variable,
            )))
        }
    }

    fn eval_insert_class_member(
        &mut self,
        is_self: bool,
        id: &usize,
        variable: Expression,
        expression: Expression,
    ) -> Option<Object> {
        return match variable {
            Expression::Identifier(name) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                if is_error(&value) {
                    Some(value)
                } else {
                    let is_callable = self.memory.is_callable_member_variable(id, &name);
                    self.memory.with_instance(id, |instance| {
                        if is_self || is_callable {
                            let object = instance.fields.get(&name);

                            if let Some(object) = object {
                                let typ = object.get_type();
                                let cast = self.eval_cast(&typ, &value);
                                instance.fields.insert(name, cast);
                            }
                        }
                    });
                    None
                }
            },
            Expression::CallMember { identifier, call } => {
                let is_callable = self.memory.is_callable_member_variable(id, &identifier);
                if is_self || is_callable {
                    let object = self.memory.get_variable(&identifier);

                    if let Some(Object::Ref(id)) = object {
                        return self.eval_insert_class_member(false, &id, *call.clone(), expression);
                    }
                }
                None
            },
            _ => Some(Object::Error(format!(
                "wrong expression: {:?} expected but {:?} given",
                Expression::Variable("".to_string(), Type::None),
                variable,
            )))
        }
    }

    /**
    IF 조건문 연결
    */
    fn eval_if_expression(
        &mut self,
        condition: Expression,
        consquence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    ) -> Option<Object> {
        let condition = match self.eval_expression(condition) {
            Some(condition) => condition,
            None => return None,
        };

        if is_truthy(&condition) {
            self.eval_block_statement(consquence)
        } else if let Some(alternative) = alternative {
            self.eval_block_statement(alternative)
        } else {
            None
        }
    }


    /**
    함수 실행 연결
     */
    fn eval_call_expression(
        &mut self,
        identifier: String,
        arguments: Vec<Expression>,
    ) -> Object {
        let arguments = arguments
            .iter()
            .map(|expression| {
                self.eval_expression(expression.clone())
                    .unwrap_or(Object::Null)
            })
            .collect::<Vec<_>>();

        if let Some(info) = self.memory.get_function(&identifier){
            if let Some(info) = info.upgrade() {
                if info.parameters.len() != arguments.len() {
                    return Object::Error(format!(
                        "wrong number of arguments: {} expected but {} given",
                        info.parameters.len(),
                        arguments.len(),
                    ));
                }

                let current_env = self.memory.push_stack();
                let list = info.parameters.iter().zip(arguments.iter());

                for (_, (variable, object)) in list.enumerate() {
                    let Expression::Variable(name, typ) = variable else {
                        return Object::Error(format!(
                            "wrong expression: {:?} expected but {:?} given",
                            Expression::Variable("".to_string(), Type::None),
                            variable,
                        ));
                    };
                    let cast = self.eval_cast(typ, object);
                    self.memory.set_variable(name.clone(), cast);
                }
                let object = self.eval_block_statement(info.body.clone());

                self.memory.pop_stack(current_env);

                return match object {
                    Some(Object::ReturnValue(object)) => self.eval_cast(&info.return_type, &object),
                    _ => Object::Null,
                }
            }
        }
        Object::Null
    }

    /// 클래스 정적 멤버 접근
    fn eval_call_static_member(&mut self, identifier:String, called:Expression) -> Object {
        match called {
            Expression::Call { identifier: fn_name, arguments } => {
                let (is_pub, is_stat) = self.memory.is_static_method(&identifier, &fn_name);
                if is_pub && is_stat {
                    if let Some(info) = self.memory.get_class_def(&identifier) {
                        return self.eval_class_method_call(info, fn_name, arguments);
                    }
                }
                return Object::Error(format!("The method is inaccessible or does not exist."));
            },
            _ => {}
        }
        Object::Null
    }


    /// 클래스 멤버 접근
    fn eval_call_member(&mut self, identifier:String, called:Expression) -> Object {
        if let Object::Ref(id) = self.eval_identifier(identifier.clone()) {
            //println!("{identifier} == Ref:{:?}",id);
            let no_need_public = identifier == "self";
            return match called {
                Expression::Call { identifier: fn_name, mut arguments } => {
                    //println!("{identifier}.{fn_name}() 함수 호출");
                    let (is_pub, _) = self.memory.is_callable_member_method(&id, &fn_name);
                    //println!("{identifier}.{fn_name}() 함수 정보 : is_pub={is_pub}");
                    if no_need_public || is_pub {
                        if let Some(info) = self.memory.get_class_def_from_id(&id) {
                            let mut self_arguments = vec![Expression::Identifier(identifier)];
                            self_arguments.append(&mut arguments);
                            return self.eval_class_method_call(info, fn_name, self_arguments);
                        }
                    }
                    Object::Error(format!("The method is inaccessible or does not exist."))
                },
                Expression::Identifier(member_name) => {
                    //println!("{identifier}.{member_name} 변수 호출");
                    let is_pub = self.memory.is_callable_member_variable(&id, &member_name);
                    //println!("{identifier}.{member_name} 변수 정보 : is_pub={is_pub}");
                    if no_need_public || is_pub {
                        if let Some(object) = self.memory.call_member_variable(&id, &member_name) {
                            return object;
                        }
                    }
                    Object::Error(format!("The variable is inaccessible or does not exist."))
                },
                _ => Object::Error(format!(
                    "wrong expression: {:?} given",
                    called
                ))
            }
        }
        Object::Null
    }

    /**
    클래스 메소드 실행 연결, public, private, static 상관 없이 실행
     */
    fn eval_class_method_call(
        &mut self,
        info: Weak<ClassInfo>,
        identifier: String,
        arguments: Vec<Expression>
    ) -> Object {
        let Some(info) = info.upgrade() else {
            return Object::Error(format!("There is no class define."));
        };

        let arguments = arguments
            .iter()
            .map(|expression| {
                self.eval_expression(expression.clone())
                    .unwrap_or(Object::Null)
            })
            .collect::<Vec<_>>();

        if let Some(FunctionInfo{parameters, body, return_type})
            = info.get_method_info(&identifier) {
            if parameters.len() != arguments.len() {
                return Object::Error(format!(
                    "wrong number of arguments: {} expected but {} given",
                    parameters.len(),
                    arguments.len(),
                ));
            }

            let current_env = self.memory.push_stack();
            let list = parameters.iter().zip(arguments.iter());

            for (_, (variable, object)) in list.enumerate() {
                let Expression::Variable(name, typ) = variable else {
                    return Object::Error(format!(
                        "wrong expression: {:?} expected but {:?} given",
                        Expression::Variable("".to_string(), Type::None),
                        variable,
                    ));
                };
                let cast = self.eval_cast(typ, object);
                self.memory.set_variable(name.clone(), cast);
            }
            let object = self.eval_block_statement(body.clone());

            self.memory.pop_stack(current_env);

            return match object {
                Some(Object::ReturnValue(object)) => self.eval_cast(&return_type, &object),
                _ => Object::Null,
            }
        }
        Object::Null
    }

    fn eval_block_statement(&mut self, statements: Vec<Statement>) -> Option<Object> {
        let mut result = None;

        for statement in statements {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value.clone())),
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                object => result = object,
            }
        }

        result
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null | Object::Bool(false) |
        Object::I64(0) => false,
        Object::F64(v) => *v < f64::EPSILON || *v > -f64::EPSILON,
        _ => true
    }
}

fn is_error(object: &Object) -> bool {
    match object {
        Object::Error(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {

    use crate::interpreter::Interpreter;
    use crate::Object;
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    fn eval(input: &str) -> Option<Object> {
        let mut e = Interpreter::new();
        let t = Tokenizer::new(input).tokenize();
        e.eval(Parser::new(&t).parse())
    }

    #[test]
    fn test_integer_expression() {
        let tests = vec![
            ("5", Some(Object::I64(5))),
            ("10", Some(Object::I64(10))),
            ("-5", Some(Object::I64(-5))),
            ("-10", Some(Object::I64(-10))),
            ("5 + 5 + 5 + 5 - 10", Some(Object::I64(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::I64(32))),
            ("-50 + 100 + -50", Some(Object::I64(0))),
            ("5 * 2 + 10", Some(Object::I64(20))),
            ("5 + 2 * 10", Some(Object::I64(25))),
            ("20 + 2 * -10", Some(Object::I64(0))),
            ("50 / 2 * 2 + 10", Some(Object::I64(60))),
            ("2 * (5 + 10)", Some(Object::I64(30))),
            ("3 * 3 * 3 + 10", Some(Object::I64(37))),
            ("3 * (3 * 3) + 10", Some(Object::I64(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::I64(50))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_float_expression() {
        let tests = vec![
            ("5.0", Some(Object::F64(5.0))),
            ("10.0", Some(Object::F64(10.0))),
            ("-5.0", Some(Object::F64(-5.0))),
            ("-10.0", Some(Object::F64(-10.0))),
            ("5.0 + 5.0 + 5.0 + 5.0 - 10.0", Some(Object::F64(10.0))),
            ("2.0 * 2.0 * 2.0 * 2.0 * 20.", Some(Object::F64(320.))),
            ("-50.0 + 100.0 + -50.0", Some(Object::F64(0.0))),
            ("5.0 * 2.0 + 10.0", Some(Object::F64(20.0))),
            ("5.0 + 2.0 * 10.0", Some(Object::F64(25.0))),
            ("20.0 + 2.0 * -10.0", Some(Object::F64(0.0))),
            ("50.0 / 2.0 * 2.0 + 10.0", Some(Object::F64(60.0))),
            ("2.0 * (5.0 + 10.0)", Some(Object::F64(30.0))),
            ("3.0 * 3.0 * 3.0 + 10.0", Some(Object::F64(37.0))),
            ("3.0 * (3.0 * 3.0) + 10.0", Some(Object::F64(37.0))),
            ("(5.0 + 10.0 * 2.0 + 15.0 / 3.0) * 2.0 + -10.0", Some(Object::F64(50.0))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
            ("1 < 2", Some(Object::Bool(true))),
            ("1 > 2", Some(Object::Bool(false))),
            ("1 < 1", Some(Object::Bool(false))),
            ("1 > 1", Some(Object::Bool(false))),
            ("1 == 1", Some(Object::Bool(true))),
            ("1 != 1", Some(Object::Bool(false))),
            ("1 == 2", Some(Object::Bool(false))),
            ("1 != 2", Some(Object::Bool(true))),
            ("true == true", Some(Object::Bool(true))),
            ("false == false", Some(Object::Bool(true))),
            ("true == false", Some(Object::Bool(false))),
            ("true != false", Some(Object::Bool(true))),
            ("false != true", Some(Object::Bool(true))),
            ("(1 < 2) == true", Some(Object::Bool(true))),
            ("(1 < 2) == false", Some(Object::Bool(false))),
            ("(1 > 2) == true", Some(Object::Bool(false))),
            ("(1 > 2) == false", Some(Object::Bool(true))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_not_operator() {
        let tests = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!!true", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!5", Some(Object::Bool(true))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Some(Object::I64(10))),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(Object::I64(10))),
            ("if (1 < 2) { 10 }", Some(Object::I64(10))),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(Object::I64(20))),
            ("if (1 < 2) { 10 } else { 20 }", Some(Object::I64(10))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Some(Object::I64(10))),
            ("return 10; 9;", Some(Object::I64(10))),
            ("return 2 * 5; 9;", Some(Object::I64(10))),
            ("9; return 2 * 5; 9;", Some(Object::I64(10))),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }

                    return 1;
                }"#,
                Some(Object::I64(10)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Some(Object::I64(5))),
            ("let a = 5 * 5; a;", Some(Object::I64(25))),
            ("let a = 5; let b = a; b;", Some(Object::I64(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Some(Object::I64(15)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_function_object() {
        let input = r#"
            fn add2(x) { x + 2; };
            let b = add2(2);
            b;
        "#;

        assert_eq!(
            Some(Object::I64(4)),
            eval(input),
        )
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "5 + true; 5;",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "-true",
                Some(Object::Error(String::from("unknown operator: -true"))),
            ),
            (
                "5; true + false; 5;",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "if (10 > 1) { true + false; }",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                r#"
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }

                        return 1;
                    }
                "#,
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_casting() {
        let tests = vec![
            (
                r#"
                    let five:i64 = 5;
                    let ten:f64 = 10.5;

                    fn add(x:i64, y) -> f64 {
                        return x + y;
                    };

                    let result = add(ten, five);
                    result;
                "#,
                Some(Object::F64(15.0)),
            )
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_if_else_if() {
        let tests = vec![
            (r#"
            if (1 > 2) {
                10
            } else if (2.5 > 3) {
                let a = 20 + 3;
                a
            } else {
                let a = 5;
                let b = 8;
                let c = a + b;
                c
            }"#, Some(Object::I64(13))
            ),
            (r#"
            if (1 > 2) {
                10
            } else if (4 > 3) {
                let a = 20 + 3;
                a
            } else {
                let a = 5;
                let b = 8;
                let c = a + b;
                c
            }"#, Some(Object::I64(23))
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_class_define() {
        let tests = vec![
            (r#"
            class Test {
                private:i64;
                pub public:f64;
                pub fn new() -> Test {
                    return Test {
                        private: 0,
                        public: 0
                    };
                }
                pub fn add(&self) -> f64 {
                    self.public = self.multi() + 1;
                    return self.public;
                }
                fn multi(&self) {
                    self.public = self.public * 5;
                }
            }"#, None
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_class_call() {
        let tests = vec![
            (r#"
            class Test {
                private:i64;
                pub public:f64;
                pub fn new() -> Test {
                    return Test {
                        private: 0,
                        public: 0
                    };
                }
                pub fn add(&self) -> f64 {
                    self.public = self.multi() + 1;
                    return self.public;
                }
                fn multi(&self) -> f64 {
                    self.public = self.public * 5;
                }
            }
            let a:Test = Test::new();
            a"#, Some(Object::Ref(0))
            ),
            (r#"
            class Test {
                private:i64;
                pub public:f64;
                pub fn new() -> Test {
                    return Test {
                        private: 1,
                        public: 2
                    };
                }
                pub fn add(&self, i:i64) -> f64 {
                    return self.multi() + i;
                }
                fn multi(&self) -> f64 {
                    return self.public * 4
                }
            }
            let a:Test = Test::new();
            a.add(2)"#, Some(Object::F64(10.0))
            ),
            (r#"
            class Test {
                private:i64;
                pub public:f64;
                pub fn new() -> Test {
                    return Test {
                        private: 1,
                        public: 2
                    };
                }
                pub fn add(&self, i:i64) -> f64 {
                    return self.multi() + i;
                }
                fn multi(&self) -> f64 {
                    return self.public * 4
                }
            }
            let a:Test = Test::new();
            a.multi()"#, Some(Object::Error(format!("The method is inaccessible or does not exist.")))
            ),
        ];

        for (input, expect) in tests {
            let mut e = Interpreter::new();
            let t = Tokenizer::new(input).tokenize();
            let p = Parser::new(&t).parse();
            let eval= e.eval(p);
            assert_eq!(expect, eval);
        }
    }
}