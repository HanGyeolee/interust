use std::ops::Add;
use crate::{Scope, Type, ScriptObejct};

pub struct VM {
    constants: Vec<ScriptObejct>,
    register: Vec<ScriptObejct>,
    stack: Vec<ScriptObejct>,
    call_stack: Vec<CallFrame>,
    ip: usize,                  // Instruction pointer
    bytecode: Vec<u8>,
}

struct CallFrame {
    return_address: usize,  // 종료 후 주소
    base_pointer: usize,    // 함수 내부에서 사용하는 스택 부분
}

impl VM {
    pub fn new() -> Self {
        VM {
            constants: Vec::new(),
            register: Vec::with_capacity(3),
            stack: Vec::new(),
            call_stack: Vec::new(),
            ip: 0,
            bytecode: Vec::new(),
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

    fn push_stack(&mut self, value: ScriptObejct){
        self.stack.push(value);
    }
    fn truncate_stack(&mut self, len: usize) {
        // len 만 남기고 뒤에는 삭제
        self.stack.truncate(len);
    }

    fn push_register(&mut self, value: ScriptObejct){
        self.register.push(value);
    }

    fn pop_register(&mut self) -> ScriptObejct {
        self.register.pop().expect("Stack underflow")
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

    fn is_truthy(object: ScriptObejct) -> bool {
        match object {
            ScriptObejct::Null | ScriptObejct::Bool(false) | ScriptObejct::I64(0) => false,
            ScriptObejct::F64(v) => v < f64::EPSILON || v > -f64::EPSILON,
            _ => true
        }
    }

    pub fn run(&mut self, stack_size:usize, bytecode: Vec<u8>, constants: Vec<ScriptObejct>) {
        self.bytecode = bytecode;
        self.constants = constants;
        self.stack = Vec::with_capacity(stack_size);
        self.rerun();
    }

    pub fn rerun(&mut self) {
        while self.ip < self.bytecode.len() {
            self.op_statement();
        }
    }

    fn op_statement(&mut self) {
        let opcode = self.read_byte();
        match opcode {
            0x50 => self.op_let_statement(),
            0x54 => self.op_return_statement(),
            code => self.op_expression(code),
        }
    }

    fn op_let_statement(&mut self) {
        // from to[0x55 addr type]
        self.op_expression_read();//.unwrap_or_else(|| VMObejct::Error(String::from("RuntimeError: let의 초기화 값에 오류가 있습니다.")));
        self.op_define_variable();
    }

    fn op_return_statement(&mut self) {
        // 0x54 exp
        self.op_expression_read();
        let frame = self.call_stack.pop().expect("Call Stack Underflow");
        self.ip = frame.return_address;
        self.truncate_stack(frame.base_pointer);
    }

    fn op_expression(&mut self, opcode:u8) {
        match opcode {
            0x56 => self.op_identifier_expression(),
            0x57 => self.op_insert_expression(),
            0x58 => self.op_if_expression(),
            0x51 => self.op_function_expression(),
            0x5C => self.op_call_expression(),
            /*
            0x5C => self.op_call(),
            0x59 => self.op_prefix(),
            0xFF => break, // Halt*/
            0x01..=0x0F => self.op_literal_expression(),
            0x20..=0x2C => self.op_infix_expression(opcode),
            0x40..=0x41 => self.op_prefix_expression(opcode),
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        };
    }
    fn op_expression_read(&mut self){
        let opcode = self.read_byte();
        self.op_expression(opcode)
    }

    fn op_identifier_expression(&mut self){
        // 변수 로드 = addr
        let var_index = self.read_u16() as usize;
        self.push_register(self.stack[var_index].clone());
    }

    fn op_load_variable(&mut self) -> usize{
        // 0x56 변수 로드 = addr
        let opcode = self.read_byte();
        match opcode {
            0x56 => self.read_u16() as usize,
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        }
    }

    fn op_define_variable(&mut self){
        // 0x55 type
        let opcode = self.read_byte();
        match opcode {
            0x55 => {
                let typ = self.op_type();

                let value = self.pop_register();
                let cast = self.cast(&typ, &value);
                self.push_stack(cast);
            },
            _ => panic!("RuntimeError: 알 수 없는 바이트 코드: {}", opcode),
        }
    }

    fn op_function_expression(&mut self) {
        // return params_length body_length| params[0x55 type, 0x55 type, ...] body
        self.read_byte();
        let param_count = self.read_u16() as usize;
        let body_size = self.read_u16() as usize;

        self.push_stack(ScriptObejct::Fn(self.ip, param_count as u8, body_size));

        // Store function metadata if needed
        self.ip += 2 * param_count + body_size; // 매개변수와 바디 크기 만큼 일단 패스
    }

    /**
    Primitive 타입 연결
     */
    fn op_literal_expression(&mut self){
        let constant_index = self.read_u16() as usize;
        self.push_register(self.constants[constant_index].clone());
    }

    /**
    접두사 연산
     */
    fn op_prefix_expression(&mut self, prefix:u8) {
        let right = self.pop_register();
        let result = match prefix {
            0x40 => self.op_minus_prefix_expression(right),
            0x41 => self.op_not_operator_expression(right),
            _ => panic!("RuntimeError: 잘못된 접두사 연산자"),
        };
        self.push_register(result);
    }

    /**
    Not 접두사 연산
     */
    fn op_not_operator_expression(&mut self, right: ScriptObejct) -> ScriptObejct {
        ScriptObejct::Bool(!VM::is_truthy(right))
    }

    /**
    음수 접두사 연산
     */
    fn op_minus_prefix_expression(&mut self, right: ScriptObejct) -> ScriptObejct {
        match right {
            ScriptObejct::F64(value) => ScriptObejct::F64(-value),
            ScriptObejct::I64(value) => ScriptObejct::I64(-value),
            _ => ScriptObejct::Error(format!("RuntimeError: Unknown operator: -{right}")),
        }
    }

    fn op_infix_expression(&mut self, infix:u8){
        self.op_expression_read();
        self.op_expression_read();
        let right = self.pop_register();
        let left = self.pop_register();
        let result = match left {
            ScriptObejct::I64(left_value) => {
                match right {
                    ScriptObejct::I64(right_value) => self.op_infix_i64_expression(infix, left_value, right_value),
                    ScriptObejct::F64(right_value) => self.op_infix_f64_expression(infix, left_value as f64, right_value),
                    ScriptObejct::String(ref right_value) => self.op_infix_string_expression(infix, &left_value.to_string(), right_value),
                    _ => ScriptObejct::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            ScriptObejct::F64(left_value) => {
                match right {
                    ScriptObejct::F64(right_value) => self.op_infix_f64_expression(infix, left_value, right_value),
                    ScriptObejct::I64(right_value) => self.op_infix_f64_expression(infix, left_value, right_value as f64),
                    ScriptObejct::String(ref right_value) => self.op_infix_string_expression(infix, &left_value.to_string(), right_value),
                    _ => ScriptObejct::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            ScriptObejct::Bool(left_value) => {
                match right {
                    ScriptObejct::Bool(right_value) => self.op_infix_bool_expression(infix, left_value, right_value),
                    ScriptObejct::String(ref right_value) => self.op_infix_string_expression(infix, &left_value.to_string(), right_value),
                    _ => ScriptObejct::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            ScriptObejct::String(ref left_value) => {
                match right {
                    ScriptObejct::I64(right_value) => self.op_infix_string_expression(infix, left_value, &right_value.to_string()),
                    ScriptObejct::F64(right_value) => self.op_infix_string_expression(infix, left_value, &right_value.to_string()),
                    ScriptObejct::Bool(right_value) => self.op_infix_string_expression(infix, left_value, &right_value.to_string()),
                    ScriptObejct::String(ref right_value) => self.op_infix_string_expression(infix, left_value, right_value),
                    _ => ScriptObejct::Error(format!("RuntimeError: 타입 매칭 불가: {left} {infix} {right}")),
                }
            }
            _ => ScriptObejct::Error(format!("RuntimeError: 잘못된 연산자: {left} {infix} {right}")),
        };
        self.push_register(result);
    }

    /**
    I64 계산 방식 연산
     */
    fn op_infix_i64_expression(
        &mut self,
        infix: u8,
        left_value: i64,
        right_value: i64,
    ) -> ScriptObejct {
        match infix {
            0x20 => ScriptObejct::I64(left_value + right_value),
            0x21 => ScriptObejct::I64(left_value - right_value),
            0x22 => ScriptObejct::I64(left_value * right_value),
            0x23 => ScriptObejct::I64(left_value / right_value),
            0x24 => ScriptObejct::I64(left_value % right_value),
            0x25 => ScriptObejct::Bool(left_value == right_value),
            0x26 => ScriptObejct::Bool(left_value != right_value),
            0x27 => ScriptObejct::Bool(left_value < right_value),
            0x28 => ScriptObejct::Bool(left_value > right_value),
            0x2B => ScriptObejct::I64(left_value & right_value),
            0x2C => ScriptObejct::I64(left_value | right_value),
            _ => ScriptObejct::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    F64 계산 방식 연산
     */
    fn op_infix_f64_expression(
        &mut self,
        infix: u8,
        left_value: f64,
        right_value: f64,
    ) -> ScriptObejct {
        match infix {
            0x20 => ScriptObejct::F64(left_value + right_value),
            0x21 => ScriptObejct::F64(left_value - right_value),
            0x22 => ScriptObejct::F64(left_value * right_value),
            0x23 => ScriptObejct::F64(left_value / right_value),
            0x25 => ScriptObejct::Bool(left_value == right_value),
            0x26 => ScriptObejct::Bool(left_value != right_value),
            0x27 => ScriptObejct::Bool(left_value < right_value),
            0x28 => ScriptObejct::Bool(left_value > right_value),
            _ => ScriptObejct::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    논리 계산 방식 연산
     */
    fn op_infix_bool_expression(
        &mut self,
        infix: u8,
        left_value: bool,
        right_value: bool,
    ) -> ScriptObejct {
        match infix {
            0x25 => ScriptObejct::Bool(left_value == right_value),
            0x26 => ScriptObejct::Bool(left_value != right_value),
            0x29 => ScriptObejct::Bool(left_value && right_value),
            0x2A => ScriptObejct::Bool(left_value || right_value),
            0x2B => ScriptObejct::Bool(left_value & right_value),
            0x2C => ScriptObejct::Bool(left_value | right_value),
            _ => ScriptObejct::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    문자열 계산 방식 연산
     */
    fn op_infix_string_expression(
        &mut self,
        infix: u8,
        left_value: &String,
        right_value: &String
    ) -> ScriptObejct {
        match infix {
            0x20 => ScriptObejct::String(left_value.clone().add(right_value.as_str())),
            0x25 => ScriptObejct::Bool(left_value == right_value),
            0x26 => ScriptObejct::Bool(left_value != right_value),
            0x27 => ScriptObejct::Bool(left_value > right_value),
            0x28 => ScriptObejct::Bool(left_value < right_value),
            _ => ScriptObejct::Error(format!("RuntimeError: Unknown operator: {left_value} {infix} {right_value}")),
        }
    }

    /**
    Insert 삽입문
     */
    fn op_insert_expression(&mut self) {
        // 0x57 from to[0x56 addr]
        self.op_expression_read();
        let value = self.pop_register();
        let index = self.op_load_variable();
        self.stack[index] = value;
    }

    /**
    Type 에 맞게 Value Casting
     */
    fn cast(&mut self, typ: &Type, object: &ScriptObejct) -> ScriptObejct {
        match typ {
            Type::F64 => match object {
                ScriptObejct::F64(v) => ScriptObejct::F64(v.clone()),
                ScriptObejct::I64(v) => ScriptObejct::F64(v.clone() as f64),
                ScriptObejct::Bool(v) => ScriptObejct::F64(if v.clone() { 1.0 } else { 0.0 }),
                _ => ScriptObejct::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::I64 => match object {
                ScriptObejct::F64(v) => ScriptObejct::I64(v.clone() as i64),
                ScriptObejct::I64(v) => ScriptObejct::I64(v.clone()),
                ScriptObejct::Bool(v) => ScriptObejct::I64(if v.clone() { 1 } else { 0 }),
                _ => ScriptObejct::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::String => match object {
                ScriptObejct::String(v) => ScriptObejct::String(v.clone()),
                _ => ScriptObejct::Error(format!(
                    "형 변환 불가: {object} -> {typ}",
                )),
            }
            Type::Bool => ScriptObejct::Bool(Self::is_truthy(object.clone())),
            _ => object.clone()
        }
    }

    /**
     IF 조건문 연산
     */
    fn op_if_expression(&mut self) {
        // 0x58 cond cons_size alter_size cons (alter)
        self.op_expression_read();
        let condition = self.pop_register();
        let true_branch_size = self.read_u16() as usize;
        let false_branch_size = self.read_u16() as usize;

        match condition {
            ScriptObejct::Bool(true) => {
                // Execute true branch
                self.execute_block(true_branch_size);
                self.ip += false_branch_size; // Skip false branch
            }
            ScriptObejct::Bool(false) => {
                self.ip += true_branch_size; // Skip true branch
                self.execute_block(false_branch_size);
            }
            _ => panic!("Condition must be a boolean"),
        }
    }

    fn op_call_expression(&mut self) {
        // addr args_length args[0x56 index, 0x56 index, ...]
        let function_index = self.read_u16() as usize;
        let arg_count = self.read_u16() as usize;

        for _ in 0..arg_count {
            self.op_expression_read();
        }

        match self.stack[function_index] {
            ScriptObejct::Fn(index, params_count, body_size) => {
                if params_count == arg_count as u8 {
                    self.call_function_inner(index, self.ip, arg_count, body_size);
                } else {
                    panic!("RuntimeError: 함수 매개변수 개수 불일치: {params_count} == {arg_count}");
                }
            }
            _ => panic!("RuntimeError: 함수 이외 호출 불가능"),
        }
    }

    pub fn call_function(&mut self, function_index: usize, params: Vec<ScriptObejct>) -> bool {
        let arg_count = params.len();
        for param in params {
            self.push_register(param);
        }

        match self.stack[function_index] {
            ScriptObejct::Fn(index, params_count, body_size) => {
                if params_count == arg_count as u8 {
                    let return_address = index + 2 * params_count as usize + body_size;
                    return self.call_function_inner(index, return_address, arg_count, body_size);
                } else {
                    panic!("RuntimeError: 함수 매개변수 개수 불일치: {params_count} == {arg_count}");
                }
            }
            _ => panic!("RuntimeError: 함수 이외 호출 불가능"),
        }
    }

    pub fn call_variable(&mut self, variable_index: usize) -> ScriptObejct {
        self.stack[variable_index].clone()
    }

    fn call_function_inner(
        &mut self, function_index: usize, return_address:usize,
        arg_count: usize, body_size: usize) -> bool {
        let frame = CallFrame {
            return_address,
            base_pointer: self.stack.len(),
        };
        let end = self.call_stack.len();
        self.call_stack.push(frame);
        self.ip = function_index;
        // params[0x55 type, 0x55 type, ...] body
        for _ in 0..arg_count {
            self.op_define_variable();
        }
        self.execute_block(body_size);

        // return 명령어가 없는 void 를 출력하는 함수일 경우
        if self.call_stack.len() > end {
            let frame = self.call_stack.pop().expect("Call Stack Underflow");
            self.ip = frame.return_address;
            self.truncate_stack(frame.base_pointer);
            return false;
        }
        return true;
    }

    fn execute_block(&mut self, size: usize) {
        let end = self.ip + size;
        while self.ip < end {
            self.op_statement();
        }
    }
}

pub struct BytecodeEngine {
    pub virtual_machine:VM,
    scope:Scope
}

impl BytecodeEngine {
    pub fn new() -> Self {
        BytecodeEngine {
            virtual_machine:VM::new(),
            scope: Scope::new()
        }
    }

    pub fn set(&mut self, constants: Vec<ScriptObejct>,
               bytecode: Vec<u8>,
               scope:Scope) {
        self.scope = scope;
        self.virtual_machine.run(self.scope.table.len(), bytecode, constants);
    }

    pub fn call_function(&mut self, name:String, params: Vec<ScriptObejct>) -> Option<ScriptObejct> {
        if let Some((addr, _)) = self.scope.table.get(&name) {
            if self.virtual_machine.call_function(*addr, params) {
                return Some(self.virtual_machine.pop_register());
            }else {
                return None;
            }
        }
        return Some(ScriptObejct::Error(format!("해당하는 함수 식별자({0})가 없습니다.", name)));
    }

    pub fn call_variable(&mut self, name:String) -> ScriptObejct {
        if let Some((addr, _)) = self.scope.table.get(&name) {
            return self.virtual_machine.call_variable(*addr);
        }
        return ScriptObejct::Error(format!("해당하는 변수 식별자({0})가 없습니다.", name));
    }

    pub fn print(&self) {
        println!("{:?}", self.virtual_machine.constants);
        println!("{:?}", self.virtual_machine.stack);
    }
}

#[cfg(test)]
mod test {
    use crate::complie::compiler::Compiler;
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;
    use crate::virtualmachine::VM;
    use crate::ScriptObejct;

    #[test]
    fn test_set_constant(){
        let input = r#"
            let five:i64 = 5;
            let ten:f64 = 10;
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let compiled = compiler.compile(program);
        println!("{:02x?}", compiled.bytecode);

        let mut vm:VM = VM::new();
        vm.run(compiled.scopes.len(), compiled.bytecode, vec![
            ScriptObejct::I64(5), ScriptObejct::I64(10)
        ]);
        println!("{:?}", vm.stack);
    }

    #[test]
    fn test_function_use(){
        let input = r#"
            fn add(x:i64, y) -> i64 {
                return x + y;
            }
            let result = add(0, 1);
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let compiled = compiler.compile(program);
        println!("{:02x?}", compiled.bytecode);

        let mut virtual_m:VM = VM::new();
        virtual_m.run(compiled.scopes.len(), compiled.bytecode, vec![
            ScriptObejct::I64(0), ScriptObejct::I64(1)
        ]);
        println!("{:?}", virtual_m.stack);
    }

    #[test]
    fn test_all(){
        let input = r#"
            let count:i64 = 0;
            fn add(x:i64, y) -> i64 {
                count = count + 1;
                return x + y;
            }
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let compiled = compiler.compile(program);
        println!("{:02x?}", compiled.bytecode);

        let mut virtual_m:VM = VM::new();
        virtual_m.run(compiled.scopes.len(), compiled.bytecode, vec![
            ScriptObejct::I64(0), ScriptObejct::I64(1)
        ]);
        println!("{:?}", virtual_m.stack);
        println!("{:?}", virtual_m.register);
    }
}