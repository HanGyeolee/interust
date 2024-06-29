use std::collections::HashMap;
use std::ops::Add;
use crate::ast::{Expression, Infix, Prefix, Type};
use crate::object::Object;

/// Java Stack 이나 Rust Interpreter 에 대해서 공부하는 게 좋을 것 같다.
/// CPU 명령이 왜 Stack 을 통해서 동작하는 가
///
/// Interpreter = 고수준 언어로 CPU를 흉내내기 위함


#[derive(Debug, Clone)]
enum Value {
    Null,
    I64(i64),
    F64(f64),
    Bool(bool),
    String(String),
    Function(usize), // 함수의 시작 주소
    Error(String)
}

struct VM {
    constants: Vec<Value>,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    ip: usize, // Instruction pointer
    bytecode: Vec<u8>,
}

struct CallFrame {
    function_index: usize,  // ??????
    ip: usize,              // 바이트 코드 인덱스
    base_pointer: usize,    // 함수의 스택 프레임 위치
}

impl VM {
    fn new(bytecode: Vec<u8>, constants: Vec<Value>) -> Self {
        VM {
            constants,
            stack: Vec::new(),
            call_stack: Vec::new(),
            ip: 0,
            bytecode,
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let value = u16::from_le_bytes([self.read_byte(), self.read_byte()]);
        value
    }

    fn push(&mut self, value: Value){
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn op_type(&mut self) -> Type {
        let opcode = self.read_byte();
        match opcode {
            0x71 => Type::None,
            0x72 => Type::I64,
            0x73 => Type::F64,
            0x7C => Type::Bool,
            0x7F => Type::String,
            _ => {
                eprintln!("디컴파일 오류 : Type 식별 바이트 코드({0:#02x}) 매치 불가)",
                          opcode);
                Type::None
            }
        }
    }

    fn is_error(object: &Value) -> bool {
        match object {
            Value::Error(_) => true,
            _ => false,
        }
    }

    fn run(&mut self) {
        while self.ip < self.bytecode.len() {
            self.op_statement();
        }
    }

    fn op_statement(&mut self) {
        let opcode = self.read_byte();
        match opcode {
            0x50 => self.op_let_statement(),
            0x51 => self.op_return_statement(),
            _ => self.op_expression(),
        }
    }

    fn op_let_statement(&mut self) {
        // from to[0x52 addr type]
        self.op_expression()
            .unwrap_or_else(|| Value::Error(String::from("RuntimeError: let의 초기화 값에 오류가 있습니다.")));
        self.op_define_variable();
    }

    fn op_return_statement(&mut self) {
        let return_value = self.pop();
        let frame = self.call_stack.pop().expect("Call Stack Underflow");
        self.ip = frame.ip;
        self.stack.truncate(frame.base_pointer);
        self.push(return_value);
    }

    fn op_expression(&mut self){
        let opcode = self.read_byte();
        match opcode {
            0x54 => self.op_insert_expression(),
            0x55 => self.op_if_expression(),
            0x56 => self.op_function_expression(),
            0x57 => self.op_call_expression(),
            /*
            0x57 => self.op_call(),
            0x5A => self.op_prefix(),
            0xFF => break, // Halt*/
            0x01..=0x0F => self.op_literal_expression(),
            0x20..=0x2C => self.op_infix_expression(),
            0x40..=0x41 => self.op_prefix_expression(),
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        };
    }

    fn op_identifier_expression(&mut self) -> usize{
        // 0x53 변수 로드 = addr
        let opcode = self.read_byte();
        match opcode {
            0x53 => self.read_u16() as usize,
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        }
    }

    fn op_define_variable(&mut self){
        // 0x52 addr type
        let opcode = self.read_byte();
        match opcode {
            0x52 => {
                self.read_u16();
                let typ = self.op_type();

                let value = self.pop();
                let cast = self.cast(&typ, &value);
                self.push(cast);
            },
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        }
    }

    fn op_function_expression(&mut self) {
        // addr return params_length body_length params[0x52 type, 0x52 type, ...] body
        self.read_u16();
        let return_type = self.read_byte();
        let param_count = self.read_u16();
        let body_size = self.read_u16() as usize;

        self.push(Value::Function(self.ip));

        // Store function metadata if needed
        self.ip += param_count + body_size; // 매개변수와 바디 크기 만큼 일단 패스
    }

    /**
    Primitive 타입 연결
     */
    fn op_literal_expression(&mut self){
        let constant_index = self.read_u16() as usize;
        self.stack.push(self.constants[constant_index].clone());
    }

    /**
    접두사 연산
     */
    fn op_prefix_expression(&mut self) {
        let operator = self.read_byte();
        let right = self.pop();
        let result = match operator {
            0x40 => self.op_minus_prefix_expression(right),
            0x41 => self.op_not_operator_expression(right),
            _ => panic!("RuntimeError: 잘못된 접두사 연산자"),
        };
        self.push(result);
    }

    /**
    Not 접두사 연산
     */
    fn op_not_operator_expression(&mut self, right: Value) -> Value {
        Value::Bool(!Self::is_truthy(right))
    }

    /**
    음수 접두사 연산
     */
    fn op_minus_prefix_expression(&mut self, right: Value) -> Value {
        match right {
            Value::F64(value) => Value::F64(-value),
            Value::I64(value) => Value::I64(-value),
            _ => Value::Error(format!("RuntimeError: Unknown operator: -{right}")),
        }
    }

    fn op_infix_expression(&mut self){
        let infix = self.read_byte();
        self.op_expression();
        self.op_expression();
        let right = self.pop();
        let left = self.pop();
        let result = match left {
            Value::I64(left_value) => {
                match right {
                    Value::I64(right_value) => self.op_infix_i64_expression(infix, left_value, right_value),
                    Value::F64(right_value) => self.op_infix_f64_expression(infix, left_value as f64, right_value),
                    Value::String(right_value) => self.op_infix_string_expression(infix, left_value.to_string(), right_value),
                    _ => Value::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            Value::F64(left_value) => {
                match right {
                    Value::F64(right_value) => self.op_infix_f64_expression(infix, left_value, right_value),
                    Value::I64(right_value) => self.op_infix_f64_expression(infix, left_value, right_value as f64),
                    Value::String(right_value) => self.op_infix_string_expression(infix, left_value.to_string(), right_value),
                    _ => Value::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            Value::Bool(left_value) => {
                match right {
                    Value::Bool(right_value) => self.op_infix_bool_expression(infix, left_value, right_value),
                    Value::String(right_value) => self.op_infix_string_expression(infix, left_value.to_string(), right_value),
                    _ => Value::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            Value::String(left_value) => {
                match right {
                    Value::I64(right_value) => self.op_infix_string_expression(infix, left_value, right_value.to_string()),
                    Value::F64(right_value) => self.op_infix_string_expression(infix, left_value, right_value.to_string()),
                    Value::Bool(right_value) => self.op_infix_string_expression(infix, left_value, right_value.to_string()),
                    Value::String(right_value) => self.op_infix_string_expression(infix, left_value, right_value),
                    _ => Value::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            _ => Value::Error(format!("RuntimeError: 잘못된 연산자: {left} {infix} {right}")),
        };
        self.stack.push(result);
    }

    /**
    I64 계산 방식 연산
     */
    fn op_infix_integer_expression(
        &mut self,
        infix: u8,
        left_value: i64,
        right_value: i64,
    ) -> Value {
        match infix {
            0x20 => Value::I64(left_value + right_value),
            0x21 => Value::I64(left_value - right_value),
            0x22 => Value::I64(left_value * right_value),
            0x23 => Value::I64(left_value / right_value),
            0x24 => Value::I64(left_value % right_value),
            0x25 => Value::Bool(left_value == right_value),
            0x26 => Value::Bool(left_value != right_value),
            0x27 => Value::Bool(left_value < right_value),
            0x28 => Value::Bool(left_value > right_value),
            0x2B => Value::I64(left_value & right_value),
            0x2C => Value::I64(left_value | right_value),
            _ => Value::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    F64 계산 방식 연산
     */
    fn op_infix_float_expression(
        &mut self,
        infix: u8,
        left_value: f64,
        right_value: f64,
    ) -> Value {
        match infix {
            0x20 => Value::F64(left_value + right_value),
            0x21 => Value::F64(left_value - right_value),
            0x22 => Value::F64(left_value * right_value),
            0x23 => Value::F64(left_value / right_value),
            0x25 => Value::Bool(left_value == right_value),
            0x26 => Value::Bool(left_value != right_value),
            0x27 => Value::Bool(left_value < right_value),
            0x28 => Value::Bool(left_value > right_value),
            _ => Value::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    논리 계산 방식 연산
     */
    fn op_infix_boolean_expression(
        &mut self,
        infix: u8,
        left_value: bool,
        right_value: bool,
    ) -> Value {
        match infix {
            0x25 => Value::Bool(left_value == right_value),
            0x26 => Value::Bool(left_value != right_value),
            0x29 => Value::Bool(left_value && right_value),
            0x2A => Value::Bool(left_value || right_value),
            0x2B => Value::Bool(left_value & right_value),
            0x2C => Value::Bool(left_value | right_value),
            _ => Value::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    문자열 계산 방식 연산
     */
    fn op_infix_string_expression(
        &mut self,
        infix: u8,
        left_value: String,
        right_value: String
    ) -> Value {
        match infix {
            0x20 => Value::String(left_value.clone().add(right_value.as_str())),
            0x25 => Value::Bool(left_value == right_value),
            0x26 => Value::Bool(left_value != right_value),
            0x27 => Value::Bool(left_value > right_value),
            0x28 => Value::Bool(left_value < right_value),
            0x29 => Value::Bool(left_value && right_value),
            0x2A => Value::Bool(left_value || right_value),
            _ => Value::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    Insert 삽입문
     */
    fn op_insert_expression(&mut self) {
        // 0x54 from to
        self.op_expression();
        let value = self.pop();
        let index = self.op_identifier_expression();
        self.stack[index] = value;
    }

    /**
    Type 에 맞게 Value Casting
     */
    fn cast(&mut self, typ: &Type, object: &Value) -> Value{
        match typ {
            Type::F64 => match object {
                Value::F64(v) => Value::F64(v.clone()),
                Value::I64(v) => Value::F64(v.clone() as f64),
                Value::Bool(v) => Value::F64(if v.clone() { 1.0 } else { 0.0 }),
                _ => Value::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::I64 => match object {
                Value::F64(v) => Value::I64(v.clone() as i64),
                Value::I64(v) => Value::I64(v.clone()),
                Value::Bool(v) => Value::I64(if v.clone() { 1 } else { 0 }),
                _ => Value::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::String => match object {
                Value::String(v) => Value::String(v.clone()),
                _ => Value::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::Bool => Value::Bool(Self::is_truthy(object.clone())),
            Type::None => object.clone()
        }
    }

    /**
     IF 조건문 연산
     */
    fn op_if_expression(&mut self) {
        self.op_expression();
        let condition = self.pop();
        let true_branch_size = self.read_u16() as usize;
        let false_branch_size = self.read_u16() as usize;

        match condition {
            Value::Bool(true) => {
                // Execute true branch
                self.execute_block(true_branch_size);
                self.ip += false_branch_size; // Skip false branch
            }
            Value::Bool(false) => {
                self.ip += true_branch_size; // Skip true branch
                self.execute_block(false_branch_size);
            }
            _ => panic!("Condition must be a boolean"),
        }
    }

    fn op_call_expression(&mut self) {
        // 0x57 addr args_length args[0x53 index, 0x53 index, ...]
        let function_index = self.read_u16() as usize;
        let arg_count = self.read_u16() as usize;

        // clone 최대한 자제
        let function = self.stack[function_index].clone();

        match function {
            Value::Function(index) => {
                self.call_function(index, arg_count);
            }
            _ => panic!("Can only call functions"),
        }
    }

    fn execute_block(&mut self, size: usize) {
        let end = self.ip + size;
        while self.ip < end {
            self.op_statement();
        }
    }

    fn call_function(&mut self, function_index: usize, arg_count: usize) {
        let frame = CallFrame {
            function_index,
            ip: self.ip,
            base_pointer: self.stack.len() - arg_count,
        };
        self.call_stack.push(frame);
        // Set IP to function start (you need to store function locations)
        // self.ip = function_start_address;
    }
}