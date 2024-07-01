use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreter::environment::Environment;
use crate::{Expression, Infix, Literal, Object, Prefix, Program, Statement, Type};

pub mod environment;

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new(environment: Rc<RefCell<Environment>>) -> Self {
        Interpreter {
            environment
        }
    }

    pub fn reset(&self) {
        self.environment.borrow_mut().reset();
    }

    fn is_truthy(object: Object) -> bool {
        match object {
            Object::Null | Object::Bool(false) |
            Object::I64(0) => false,
            Object::F64(v) => v < f64::EPSILON || v > -f64::EPSILON,
            _ => true
        }
    }

    fn error(msg: String) -> Object {
        Object::Error(msg)
    }

    fn is_error(object: &Object) -> bool {
        match object {
            Object::Error(_) => true,
            _ => false,
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
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
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

                    if Self::is_error(&value) {
                        return Some(value);
                    } else {
                        let Expression::Variable(name, typ) = variable else {
                            return Some(Self::error(format!(
                                "wrong expression: {:?} expected but {:?} given",
                                Expression::Variable("".to_string(), Type::None),
                                variable,
                            )));
                        };
                        let cast = self.eval_cast(&typ, &value);
                        self.environment.borrow_mut().init(name, &cast);
                    }
                }else {
                    let Expression::Variable(name, typ) = variable else {
                        return Some(Self::error(format!(
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
                    self.environment.borrow_mut().init(name, &cast);
                }

                return None;
            }
            Statement::Expression(expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                Some(value)
            }
            Statement::Return(expression) => {
                let value = match self.eval_expression(expression) {
                    Some(value) => value,
                    None => return None,
                };

                if Self::is_error(&value) {
                    Some(value)
                } else {
                    Some(Object::ReturnValue(Box::new(value)))
                }
            }
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Identifier(identifier) => Some(self.eval_identifier(identifier)),
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
            Expression::Insert { variable, expression} => self.eval_insert_expression(*variable, *expression),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(*condition, consequence, alternative),
            Expression::Fn { identifier, parameters, body, return_type } => {
                let value = Object::Fn(
                    parameters,
                    body,
                    Rc::clone(&self.environment),
                    return_type
                );
                self.environment.borrow_mut().init(identifier, &value);
                None
            },
            Expression::Call {
                function,
                arguments,
            } => Some(self.eval_call_expression(function, arguments)),
            _ => None
        }
    }

    /**
     식별자 찾기<br/>
    변수 혹은 함수 이름, 클래스 이름 등
     */
    fn eval_identifier(&mut self, identifier: String) -> Object {
        self.environment.borrow_mut().get(&identifier)
            .unwrap_or_else(|| Object::Error(String::from(format!("identifier not found: {identifier}"))))
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
        Object::Bool(!Self::is_truthy(right))
    }

    /**
    음수 접두사 연결
     */
    fn eval_minus_prefix_expression(&mut self, right: Object) -> Object {
        match right {
            Object::F64(value) => Object::F64(-value),
            Object::I64(value) => Object::I64(-value),
            _ => Self::error(format!("unknown operator: -{right}")),
        }
    }

    /**
     삽입사 연결
     */
    fn eval_infix_expression(&mut self, infix: Infix, left: Object, right: Object) -> Object {
        match left {
            Object::I64(left_value) => {
                if let Object::I64(right_value) = right {
                    self.eval_infix_integer_expression(infix, left_value, right_value)
                } else if let Object::F64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value as f64, right_value)
                } else {
                    Self::error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            Object::F64(left_value) => {
                if let Object::F64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value, right_value)
                } else if let Object::I64(right_value) = right {
                    self.eval_infix_float_expression(infix, left_value, right_value as f64)
                }  else {
                    Self::error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            Object::Bool(left_value) => {
                if let Object::Bool(right_value) = right {
                    self.eval_infix_boolean_expression(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {left} {infix} {right}"))
                }
            }
            _ => Self::error(format!("unknown operator: {left} {infix} {right}")),
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
            Infix::Plus => Object::I64(left_value + right_value),
            Infix::Minus => Object::I64(left_value - right_value),
            Infix::Multiply => Object::I64(left_value * right_value),
            Infix::Divide => Object::I64(left_value / right_value),
            Infix::Mod => Object::I64(left_value % right_value),
            Infix::Equal => Object::Bool(left_value == right_value),
            Infix::NotEqual => Object::Bool(left_value != right_value),
            Infix::LessThan => Object::Bool(left_value < right_value),
            Infix::GreaterThan => Object::Bool(left_value > right_value),
            Infix::BitAnd => Object::I64(left_value & right_value),
            Infix::BitOr => Object::I64(left_value | right_value),
            _ => Self::error(format!("unknown operator: {left_value} {infix} {right_value}")),
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
            Infix::Plus => Object::F64(left_value + right_value),
            Infix::Minus => Object::F64(left_value - right_value),
            Infix::Multiply => Object::F64(left_value * right_value),
            Infix::Divide => Object::F64(left_value / right_value),
            Infix::Equal => Object::Bool(left_value == right_value),
            Infix::NotEqual => Object::Bool(left_value != right_value),
            Infix::LessThan => Object::Bool(left_value < right_value),
            Infix::GreaterThan => Object::Bool(left_value > right_value),
            _ => Self::error(format!("unknown operator: {left_value} {infix} {right_value}")),
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
            Infix::Equal => Object::Bool(left_value == right_value),
            Infix::NotEqual => Object::Bool(left_value != right_value),
            Infix::And => Object::Bool(left_value && right_value),
            Infix::Or => Object::Bool(left_value || right_value),
            Infix::BitAnd => Object::Bool(left_value & right_value),
            Infix::BitOr => Object::Bool(left_value | right_value),
            _ => Self::error(format!(
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
        let value = match self.eval_expression(expression) {
            Some(value) => value,
            None => return None,
        };

        if Self::is_error(&value) {
            Some(value)
        } else {
            let Expression::Identifier(name) = variable else {
                return Some(Self::error(format!(
                    "wrong expression: {:?} expected but {:?} given",
                    Expression::Variable("".to_string(), Type::None),
                    variable,
                )));
            };
            let object = self.environment.borrow().get(&name);

            if let Some(object) = object {
                let typ = object.get_type();
                let cast = self.eval_cast(&typ, &value);
                self.environment.borrow_mut().init(name, &cast);
            }

            None
        }
    }

    /**
    Type 에 맞게 Object Casting
     */
    fn eval_cast(&mut self, typ: &Type, object: &Object) -> Object{
        match typ {
            Type::F64 => match object {
                Object::F64(v) => Object::F64(v.clone()),
                Object::I64(v) => Object::F64(v.clone() as f64),
                Object::Bool(v) => Object::F64(if v.clone() { 1.0 } else { 0.0 }),
                _ => Self::error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::I64 => match object {
                Object::F64(v) => Object::I64(v.clone() as i64),
                Object::I64(v) => Object::I64(v.clone()),
                Object::Bool(v) => Object::I64(if v.clone() { 1 } else { 0 }),
                _ => Self::error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::String => match object {
                Object::String(v) => Object::String(v.clone()),
                _ => Self::error(format!(
                    "cannot cast: {object} to {typ}",
                )),
            }
            Type::Bool => Object::Bool(Self::is_truthy(object.clone())),
            _ => object.clone()
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

        if Self::is_truthy(condition) {
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
        function: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Object {
        let arguments = arguments
            .iter()
            .map(|expression| {
                self.eval_expression(expression.clone())
                    .unwrap_or(Object::Null)
            })
            .collect::<Vec<_>>();

        let (parameters, body, environment, return_type) = match self.eval_expression(*function) {
            Some(Object::Fn(parameters, body, environment, return_type)) => {
                (parameters, body, environment, return_type)
            }
            //Some(Object::LibraryFn(function)) => return function(arguments),
            Some(object) => return Self::error(format!("{object} is not valid function")),
            None => return Object::Null,
        };

        if parameters.len() != arguments.len() {
            return Self::error(format!(
                "wrong number of arguments: {} expected but {} given",
                parameters.len(),
                arguments.len(),
            ));
        }

        let current_env = Rc::clone(&self.environment);
        let mut scoped_env = Environment::new_with_outer(Rc::clone(&environment));
        let list = parameters.iter().zip(arguments.iter());

        for (_, (variable, object)) in list.enumerate() {
            let Expression::Variable(name, typ) = variable else {
                return Self::error(format!(
                    "wrong expression: {:?} expected but {:?} given",
                    Expression::Variable("".to_string(), Type::None),
                    variable,
                ));
            };
            let cast = self.eval_cast(typ, object);
            scoped_env.init(name.clone(), &cast);
        }

        self.environment = Rc::new(RefCell::new(scoped_env));

        let object = self.eval_block_statement(body);

        self.environment = current_env;

        match object {
            Some(Object::ReturnValue(object)) => Object::ReturnValue(Box::new(self.eval_cast(&return_type, &object))),
            _ => Object::Null,
        }
    }

    fn eval_block_statement(&mut self, statements: Vec<Statement>) -> Option<Object> {
        let mut result = None;

        for statement in statements {
            match self.eval_statement(statement) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                object => result = object,
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::interpreter::environment::Environment;
    use crate::interpreter::Interpreter;
    use crate::Object;
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    fn eval(input: &str) -> Option<Object> {
        let mut e = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
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
    fn test_library() {
        let tests = vec![
            (r#"
                let a:i64 = 10;
                /* let b; */
                println("{:?}", a);
            "#, Some(Object::Null)
            ),
        ];

        for (input, expect) in tests {
            let mut e = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
            let t = Tokenizer::new(input).tokenize();
            println!("{:?}",t);
            let p = Parser::new(&t).parse();
            println!("{:?}",p);
            assert_eq!(expect, e.eval(p));
        }
    }
}