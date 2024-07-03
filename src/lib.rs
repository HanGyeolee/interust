//! 해당 라이브러리는 Rust 문법을 사용하는 인터프리터 엔진을 제공합니다.
//!
//! # 자동 캐스팅 지원
//!
//! as Type을 작성하지 않아도 자동으로 암시적 캐스팅됩니다.<br/>
//! 반환 타입을 명시하면 함수 내에서 반환 시 타입 확인과 함께 암시적 캐스팅됩니다.
//!
//! # 사용 가능한 변수 타입
//! - *i64* : 64bit 부호있는 정수
//! - *f64* : 64bit 부호있는 부동소수점
//! - *bool* ( *Bool* | *boolean* ) : 1bit 논리값 = ``[true, True, TRUE, false, ...]``
//! - *str* ( *String* ) : 문자열 = 길이 제한 `usize`의 최대값 > Rust에서 `&str`이 아닌 `String`으로 변수 저장
//!

use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use crate::complie::compiler::{Compiler, MAGIC_NUMBER};
use crate::interpreter::environment::Environment;
use interpreter::Interpreter;
use std::fmt;
use crate::ast::*;
use crate::parser::parser::Parser;
use crate::tokenizer::tokenizer::Tokenizer;
use crate::virtualmachine::BytecodeEngine;
use crate::vm::*;

mod tokenizer;
mod parser;
mod interpreter;
mod complie;
mod virtualmachine;

// Java Stack 이나 Rust Interpreter 에 대해서 공부하는 게 좋을 것 같다.
// CPU 명령이 왜 Stack 을 통해서 동작하는 가
// RustC 혹은 Python Compiler
//
// Interpreter = 고수준 언어로 CPU를 흉내내기 위함


/// # 인터프리터 엔진
/// 쉬운 생성 및 쉬운 컴파일링이 가능합니다.
pub struct InterustCompiler {
    interpreter: Interpreter,
    compiler: Compiler,
    program: Program,
}

impl InterustCompiler {
    /// 인터프리터 엔진을 생성합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustCompiler;
    ///
    /// let mut interust = InterustCompiler::new();
    /// ```
    pub fn new() -> Self {
        InterustCompiler {
            interpreter: Interpreter::new(Rc::new(RefCell::new(Environment::new()))),
            compiler: Compiler::new(),
            program: Program::new()
        }
    }

    /// 인터프리터의 토크나이징을 진행합니다.
    ///
    /// # 매개변수
    /// - `input` : `&str` 타입
    ///
    /// # 반환값
    /// `Vec<Token>` 타입의 배열의 형태로 반환합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::{InterustCompiler, Token};
    ///
    /// let mut interust = InterustCompiler::new();
    /// let tokens = interust.tokenize(r#"
    ///     let a = 5;
    ///     a
    /// "#);
    /// let expect:Vec<Token> = vec![
    ///     Token::Let,
    ///     Token::Identifier(String::from("a")),
    ///     Token::Assign,
    ///     Token::I64(5),
    ///     Token::Semicolon,
    ///     Token::Identifier(String::from("a")),
    ///     Token::EOF
    /// ];
    ///
    /// assert_eq!( tokens, expect );
    /// ```
    pub fn tokenize(&self, input:&str) -> Vec<Token> {
        Tokenizer::new(input).tokenize()
    }

    /// 인터프리터의 토큰 파싱을 진행합니다.<br/>
    ///
    /// # 매개변수
    /// - `tokens` : `Vec<Token>` 타입의 배열
    ///
    /// # 반환값
    /// 추상 구문 트리(AST)를 `Program` 타입의 배열의 형태로 반환합니다.<br/>
    /// `Program` 타입은 `Vec<Statement>` 타입과 동일합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::{Expression, Literal, Statement, Token, Type};
    /// use interust::InterustCompiler;
    ///
    /// let mut interust = InterustCompiler::new();
    /// let tokens:Vec<Token> = vec![
    ///     Token::Let,
    ///     Token::Identifier(String::from("a")),
    ///     Token::Assign,
    ///     Token::I64(5),
    ///     Token::Semicolon,
    ///     Token::Identifier(String::from("a")),
    ///     Token::EOF
    /// ];
    /// let program = interust.parse(&tokens);
    /// let expect = vec![
    ///     Statement::Let {
    ///         variable: Expression::Variable("a".parse().unwrap(), Type::I64),
    ///         expression: Some(Expression::Literal(Literal::I64(5)))
    ///     },
    ///     Statement::Expression(Expression::Identifier("a".parse().unwrap()))
    /// ];
    ///
    /// assert_eq!( program, expect );
    /// ```
    pub fn parse(&self, tokens: &Vec<Token>) -> Program {
        Parser::new(&tokens).parse()
    }

    /// 인터프리터에서 프로그램을 실행시킵니다.<br/>
    /// 매개변수로 들어온 새로운 코드를 `self.program` 에 이어 붙입니다.
    ///
    /// # 매개변수
    /// - `program` : `Program` 타입
    ///
    /// # 반환값
    /// 실행 결과를 반환합니다.<br/>
    /// 삽입 혹은 반환 결과가 없는 함수 호출의 경우 `None` 이 반환됩니다.<br/>
    /// 단순 변수 호출 혹은 반환 결과가 있는 함수 호출의 경우 `Option<Object>` 가 반환됩니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::{Expression, Literal, Statement, Token, Type};
    /// use interust::InterustCompiler;
    /// use interust::Object;
    ///
    /// let mut interust = InterustCompiler::new();
    /// let program = vec![
    ///     Statement::Let {
    ///         variable: Expression::Variable(String::from("a"), Type::I64),
    ///         expression: Some(Expression::Literal(Literal::I64(5)))
    ///     },
    ///     Statement::Expression(Expression::Identifier(String::from("a")))
    /// ];
    /// if let Some(result) = interust.eval_program(program){
    ///     assert_eq!(result, Object::I64(5));
    /// }
    /// ```
    pub fn eval_program(&mut self, program: Program) -> Option<Object> {
        self.program.extend(program.clone());
        self.interpreter.eval(program)
    }

    /// 인터프리터에서 텍스트 코드를 실행시킵니다.<br/>
    /// 매개변수로 들어온 새로운 코드를 `self.program` 에 이어 붙입니다.
    ///
    /// # 매개변수
    /// - `input` : `&str` 타입, 텍스트 코드
    ///
    /// # 반환값
    /// 실행 결과를 반환합니다.<br/>
    /// 삽입 혹은 반환 결과가 없는 함수 호출의 경우 `None` 이 반환됩니다.<br/>
    /// 단순 변수 호출 혹은 반환 결과가 있는 함수 호출의 경우 `Option<Object>` 가 반환됩니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustCompiler;
    /// use interust::Object;
    ///
    /// let mut interust = InterustCompiler::new();
    /// if let Some(result) = interust.eval_string(r#"
    ///     let a = 5;
    ///     a
    /// "#){
    ///     assert_eq!(result, Object::I64(5));
    /// }
    /// ```
    pub fn eval_string(&mut self, input:&str) -> Option<Object> {
        let tokens = self.tokenize(input);
        self.eval_program(self.parse(&tokens))
    }

    /// 인터프리터를 초기화합니다.<br/>
    /// 변수, 함수 등 환경값들을 제거합니다.
    pub fn reset_interpreter(&mut self) {
        self.interpreter.reset();
    }

    /// 이전에 작성했던 코드를 제거합니다.<br/>
    pub fn reset_program(&mut self) {
        self.program.clear();
    }

    /// 인터프리터를 초기화합니다.<br/>
    /// 이전에 작성했던 코드와 변수, 함수 등 환경값들을 제거합니다.
    pub fn reset(&mut self) {
        self.reset_interpreter();
        self.reset_program();
    }

    /// 인터프리터에서 여태까지 작성된 코드를 컴파일하여 바이트 코드 파일로 출력합니다.<br/><br/>
    ///
    /// # 주의
    /// 컴파일하는 경우 전역 변수와 전역 함수로만 이루어지도록 `Program:Vec<Statement>` 변수 내부에
    /// `Statement::Let` 과 `Expression::Fn` 을 남기고 전부 **제거** 합니다.
    ///
    /// # 매개변수
    /// - `file_path` : `&str` 타입, 저장될 파일 위치<br/>
    /// 파일 확장자는 **.irs**, 반드시 작성할 필요는 없습니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustCompiler;
    /// use interust::Object;
    ///
    /// let mut interust = InterustCompiler::new();
    /// interust.eval_string(r#"
    ///     let a = 5;
    ///     fn add() -> i64 {
    ///         return 3;
    ///     }
    ///     a = add(); // 제거됨. 컴파일된 파일 내부에는 해당 명령어 없음.
    /// "#);
    /// interust.export("test");
    /// ```
    pub fn export(&mut self, file_path: &str) {
        let mut path = file_path.to_string();
        if !file_path.ends_with(".irs") {
            path = format!("{0}.irs",file_path);
        };
        self.compiler.export(path.as_str(), self.program.clone());
    }

    pub fn export_from_program(&mut self, file_path:&str, program:Program) {
        let mut path = file_path.to_string();
        if !file_path.ends_with(".irs") {
            path = format!("{0}.irs",file_path);
        };
        self.compiler.export(path.as_str(), program);
    }

    pub fn export_from_tokens(&mut self, file_path:&str, tokens:&Vec<Token>) {
        let program = self.parse(tokens);
        self.export_from_program(file_path, program);
    }

    pub fn export_from_str(&mut self, file_path:&str, input:&str) {
        let tokens = self.tokenize(input);
        self.export_from_tokens(file_path, &tokens);
    }

    /// 인터프리터에서 `self.program`에 저장된 전체 추상 구문 트리(AST)를 처음부터 실행합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustCompiler;
    /// use interust::Object;
    ///
    /// let mut interust = InterustCompiler::new();
    /// interust.run_interpreter();
    /// ```
    pub fn run_interpreter(&mut self) -> Option<Object> {
        self.interpreter.eval(self.program.clone())
    }
}

/// # 가상 머신
/// 컴파일된 파일 실행하고, 외부에서 함수 및 변수를 호출 가능합니다.
pub struct InterustVM {
    bytecode_engine:BytecodeEngine,
}

impl InterustVM{
    /// 가상 머신을 생성합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustVM;
    ///
    /// let mut interust_vm = InterustVM::new();
    /// ```
    pub fn new() -> Self {
        InterustVM {
            bytecode_engine: BytecodeEngine::new(),
        }
    }

    /// 가상 머신에서 컴파일된 파일을 읽고 상수와 바이트 코드,
    /// 외부에서 참조할 수 있는 전역 변수 및 전역 함수를 설정합니다.
    ///
    /// # 매개변수
    /// - `file_path` : `&str` 타입, 저장될 파일 위치<br/>
    /// 반드시 파일 확장자 **.irs**가 포함되어야 합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustVM;
    ///
    /// let mut interust_vm = InterustVM::new();
    /// interust_vm.import_compiled("test.irs");
    /// ```
    pub fn import_compiled(&mut self, file_path:&str) {
        let mut log:String = String::from(file_path);

        let mut open = File::open(file_path)
            .expect(format!("해당 \"{0}\" 파일을 찾을 수 없습니다.", file_path).as_str());
        let mut file = Vec::new();
        open.read_to_end(&mut file)
            .expect(format!("해당 \"{0}\" 파일을 읽을 수 없습니다.", file_path).as_str());

        let index =MAGIC_NUMBER.len();
        if MAGIC_NUMBER != file[0..index].to_vec().as_slice() {
            panic!("매직 넘버 확인 실패 {0}", index);
        }

        let (version, index) = self.import_version( &file, index);
        log = log + format!(" v{0}.{1}.{2}", version[0], version[1], version[2]).as_str();

        let (constants, index) = self.read_constant_pool(&file, index);
        let (bytecode, index) = self.read_code_section(&file, index);
        let scope:Scope = self.read_scope_info(&file, index);
        self.bytecode_engine.set(constants, bytecode, scope);
        println!("{}", log);
    }

    /// 외부에서 가상 머신 내에 존재하는 함수를 호출합니다.<br/>
    /// 함수는 가상 머신 내에서 동작하며 반환값이 없는 경우 None을 반환합니다.<br/>
    /// 연산 중 오류가 발생하거나 함수를 찾을 수 없으면, VMObject::Error() 를 반환합니다.
    /// # 매개변수
    /// - `name` : `String` 타입, 호출할 함수 식별자<br/>
    /// - `params` : `Vec<VMObejct>` 타입, 함수에 전달될 매개변수들<br/>
    /// # 예제
    /// ```
    /// use interust::{InterustCompiler, InterustVM};
    ///
    /// let mut interust_compiler = InterustCompiler::new();
    /// interust_compiler.export_from_str("test_export", r#"
    ///     let count:i64 = 0;
    ///     fn add() {
    ///         count = count + 1;
    ///     }
    /// "#);
    ///
    /// let mut interust_vm = InterustVM::new();
    /// interust_vm.import_compiled("test_export.irs");
    /// let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
    /// // count:1
    /// let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
    /// // count:2
    /// assert_eq!(return_value, None);
    /// ```
    pub fn call_compiled_fn(&mut self, name:String, params: Vec<VMObejct>) -> Option<VMObejct> {
        self.bytecode_engine.call_function(name, params)
    }
    /// 외부에서 가상 머신 내에 존재하는 변수를 호출합니다.<br/>
    /// 변수를 찾을 수 없으면, VMObject::Error() 를 반환합니다.
    /// # 매개변수
    /// - `name` : `String` 타입, 호출할 변수 식별자<br/>
    /// # 예제
    /// ```
    /// use interust::{InterustCompiler, InterustVM};
    /// use interust::VMObejct;
    ///
    /// let mut interust_compiler = InterustCompiler::new();
    /// interust_compiler.export_from_str("test_export", r#"
    ///     let count:i64 = 0;
    ///     fn add() {
    ///         count = count + 1;
    ///     }
    /// "#);
    ///
    /// let mut interust_vm = InterustVM::new();
    /// interust_vm.import_compiled("test_export.irs");
    /// let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
    /// // count:1
    /// let value = interust_vm.call_compiled_var(String::from("count"));
    /// assert_eq!(value, VMObejct::I64(1));
    /// ```
    pub fn call_compiled_var(&mut self, name:String) -> VMObejct {
        self.bytecode_engine.call_variable(name)
    }

    /// 가상 머신 내 상수와 식별자들을 출력합니다.<br/>
    /// 식별자에 대해 `call_compiled_var()` 을 통해서 접근합니다.
    ///
    /// # 예제
    ///
    /// ```
    ///  use interust::{InterustCompiler, InterustVM};
    /// use interust::VMObejct;
    ///
    /// let mut interust_compiler = InterustCompiler::new();
    /// interust_compiler.export_from_str("test_export", r#"
    ///     let count:i64 = 0;
    ///     fn add() {
    ///         count = count + 1;
    ///     }
    /// "#);
    ///
    /// let mut interust_vm = InterustVM::new();
    /// interust_vm.import_compiled("test_export.irs");
    /// interust_vm.call_compiled_fn(String::from("add"), vec![]);
    /// // count:1
    /// interust_vm.print();
    /// ```
    pub fn print(&self) {
        self.bytecode_engine.print();
    }

    fn read_constant_pool(&self, file: &Vec<u8>, index: usize) -> (Vec<VMObejct>, usize) {
        let size = std::mem::size_of::<u16>();
        let mut array = [0u8; std::mem::size_of::<u16>()];
        array.copy_from_slice(&file[index..index + size]);
        let length = u16::from_le_bytes(array);
        let mut index = index + size;
        let mut constant_pool:Vec<VMObejct> = Vec::new();
        for _ in 0..length {
            match file[index] {
                0x01 => {
                    index += 1;
                    constant_pool.push(VMObejct::Null);
                },
                0x02 => {
                    index += 1;
                    let length = std::mem::size_of::<i64>();
                    let mut array = [0u8; std::mem::size_of::<i64>()];
                    array.copy_from_slice(&file[index..index + length]);
                    let value = i64::from_le_bytes(array);
                    constant_pool.push(VMObejct::I64(value));
                    index += length;
                },
                0x03 => {
                    index += 1;
                    let length = std::mem::size_of::<f64>();
                    let mut array = [0u8; std::mem::size_of::<f64>()];
                    array.copy_from_slice(&file[index..index + length]);
                    let value = f64::from_le_bytes(array);
                    constant_pool.push(VMObejct::F64(value));
                    index += length;
                },
                0x0C => {
                    index += 1;
                    constant_pool.push(VMObejct::Bool(file[index] == 1));
                    index += 1;
                },
                0x0F => {
                    index += 1;
                    let length = std::mem::size_of::<u32>();
                    let mut array = [0u8; std::mem::size_of::<u32>()];
                    array.copy_from_slice(&file[index..index + length]);
                    let string_length = u32::from_le_bytes(array) as usize;
                    index += length;
                    if let Ok(import_magic) = String::from_utf8(file[index..index + string_length].to_vec()) {
                        constant_pool.push(VMObejct::String(import_magic));
                    }
                    index += string_length;
                },
                _ => panic!("알 수 없는 코드입니다.")
            }
        }

        (constant_pool, index)
    }

    fn read_code_section(&self, file:& Vec<u8>, index: usize) ->  (Vec<u8>, usize) {
        let (code_length, index) = self.import_usize(file, index);
        (file[index..index + code_length].to_vec(), index + code_length)
    }

    fn read_scope_info(&self, file:&Vec<u8>, index: usize) -> Scope {
        let size = std::mem::size_of::<u16>();
        let mut array = [0u8; std::mem::size_of::<u16>()];
        array.copy_from_slice(&file[index..index + size]);
        let identifier_count = u16::from_le_bytes(array);
        let mut index = index + size;
        let mut scope:Scope = Scope::new();
        for _ in 0..identifier_count {
            let string_length = file[index] as usize;
            index += 1;
            if let Ok(name) = String::from_utf8(file[index..index + string_length].to_vec()) {
                index += string_length;
                let size = std::mem::size_of::<u16>();
                let mut array = [0u8; std::mem::size_of::<u16>()];
                array.copy_from_slice(&file[index..index + size]);
                let addr = u16::from_le_bytes(array);
                index += size;
                let typ = self.decompile_type(file[index]);
                index += 1;
                scope.table.insert(name, (addr as usize, typ));
            }
        }
        scope
    }

    fn decompile_type(&self, byte:u8) -> Type {
        match byte {
            0x71 => Type::None,
            0x72 => Type::I64,
            0x73 => Type::F64,
            0x7C => Type::Bool,
            0x7F => Type::String,
            _ => panic!("디컴파일 오류 : Type 식별 바이트 코드({0:#02x}) 매치 불가", byte)
        }
    }

    fn import_version(&self, file: &Vec<u8>, index: usize) -> ([u8; 3], usize) {
        let mut array = [0u8; 3];
        array.copy_from_slice(&file[index..index + 3]);
        (array, index + 3)
    }

    fn import_usize(&self, file: &Vec<u8>, index: usize) -> (usize, usize) {
        let size = std::mem::size_of::<usize>();
        let mut array = [0u8; std::mem::size_of::<usize>()];
        array.copy_from_slice(&file[index..index + size]);
        (usize::from_le_bytes(array), index + size)
    }
}

#[cfg(test)]
mod test {
    use std::time::{Duration, Instant};
    use crate::{InterustCompiler, InterustVM};
    use crate::Object;

    #[test]
    fn test_token_eq() {
        let mut interust = InterustCompiler::new();
        if let Some(result) = interust.eval_string(r#"
            let a = 5;
            fn add(x:f64) -> i64 {
                return x + 3.5
            }
            add(a);
        "#){
            println!("{:?}", result);
            assert_eq!(result, Object::I64(8));
        }
    }

    #[test]
    fn test_export_import() {
        let mut interust = InterustCompiler::new();
        interust.export_from_str("test_export",r#"
            let a = 5;
            fn add(x:f64) -> i64 {
                return x + 3.5
            }
            let result = add(a);
        "#);

        let mut interust_vm = InterustVM::new();
        interust_vm.import_compiled("test_export.irs");
    }

    #[test]
    fn test_call_outside() {
        let mut interust_compiler = InterustCompiler::new();
        interust_compiler.export_from_str("test_export", r#"
            let count:i64 = 0;
            fn add() -> i64 {
                count = count + 1;
                return count;
            }
        "#);

        let mut interust_vm = InterustVM::new();
        interust_vm.import_compiled("test_export.irs");

        interust_vm.bytecode_engine.print();
        let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
        println!("{:?}", return_value);
        let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
        println!("{:?}", return_value);
        let return_value = interust_vm.call_compiled_fn(String::from("add"), vec![]);
        println!("{:?}", return_value);
        interust_vm.bytecode_engine.print();

        let value = interust_vm.call_compiled_var(String::from("count"));
        println!("{:?}", value);
    }

    #[test]
    fn test_hash_speed() {
        let iterator = 100000;
        let mut mean = Duration::new(0,0);
        let mut interust_compiler = InterustCompiler::new();
        interust_compiler.export_from_str("test_export", r#"
                let count:i64 = 0;
                fn add() -> i64 {
                    count = count + 1;
                }
            "#);

        let mut interust_vm = InterustVM::new();
        interust_vm.import_compiled("test_export.irs");

        for _ in 0..iterator {
            let start = Instant::now();

            interust_vm.call_compiled_fn(String::from("add"), vec![]);
            interust_vm.call_compiled_fn(String::from("add"), vec![]);
            interust_vm.call_compiled_fn(String::from("add"), vec![]);

            interust_vm.call_compiled_var(String::from("count"));
            let end = Instant::now();
            mean += end - start;
        }

        println!("runtime mean: {:?}", mean/iterator);
        // HashMap      = 10만번 : 2.103µs
        // FxHashMap    = 10만번 : 1.835µs
    }
}

/*
// [dependencies]
// winapi = { version = "0.3.9", features = ["consoleapi"] }
#[cfg(test)]
mod test {
    use std::{io, process, thread};
    use std::io::Write;
    use std::time::Duration;
    use winapi::um::consoleapi::SetConsoleCtrlHandler;
    use crate::InterustEngine;

    unsafe extern "system" fn ctrl_handler(_: u32) -> i32 {
        io::stdout().write("exit".as_bytes()).expect("");
        process::exit(0); // 프로그램을 종료하고 종료 코드 0으로 반환
    }

    fn main() {

        // Ctrl+C 신호를 처리하기 위한 플래그
        unsafe {
            SetConsoleCtrlHandler(Some(ctrl_handler), 1);
        }

        let mut interust = InterustEngine::new();
        // 사용자 입력을 받는 루프
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if let Some(result) = interust.run(input.as_str()){
                        println!("anv : {0}", result);
                    }
                }
                Err(_) => {}
            }

            // 잠시 대기 (optional: 너무 빠른 루프 방지)
            thread::sleep(Duration::from_millis(100));
        }

        println!("Program terminated.");
    }
}
*/

pub type Program = Vec<Statement>;

/// 인터프리터 해석기에서 토크나이징할 때 사용할 토큰들
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let,                    // let 변수 생성
    If,                     // if
    Else,                   // else
    Return,                 // return
    ReturnType,             // return type
    For,                    // for
    While,                  // while
    Fn,                     // function
    Class,                  // class
    Public,                 // pub
    SelfKeyword,            // self

    Identifier(String),     // 식별자
    F64(f64),               // 실수 숫자
    I64(i64),               // 정수 숫자
    Bool(bool),             // 논리
    String(String),         // 문자열

    Operator(String),       // 연산자

    OpenParen,              // (
    CloseParen,             // )
    OpenBrace,              // {
    CloseBrace,             // }
    OpenBracket,            // [
    CloseBracket,           // ]

    Assign,                 // =
    Ampersand,              // &

    Dot,                    // . 미구현
    Comma,                  // ,
    Colon,                  // :
    CallMethod,             // ::
    Semicolon,              // ;

    Comment,                // /* */
    EOF,                    // 종료
}

/// 인터프리터 해석기에서 프로그램을 실행할 때 사용할 객체들
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    F64(f64),
    I64(i64),
    Bool(bool),
    String(String),
    Fn(Vec<Expression>, Vec<Statement>, Rc<RefCell<Environment>>, Type),
    //LibraryFn(fn(Vec<Object>) -> Object),
    Null,
    ReturnValue(Box<Object>),
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
            //Object::LibraryFn(_) => write!(f, "LibraryFunction"),
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

/// # 인터프리터에서 활용할 추상 구문 트리(AST)
pub mod ast {
    /// AST 상수
    #[derive(Debug, PartialEq, Clone)]
    pub enum Literal {
        None,                   // 0x01
        I64(i64),               // 0x02
        F64(f64),               // 0x03
        //u64                   // 0x04
        //i32                   // 0x05
        //f32                   // 0x06
        //u32                   // 0x07
        //i16                   // 0x08
        //u16                   // 0x09
        //i8                    // 0x0A
        //u8                    // 0x0B
        Bool(bool),             // 0x0C
        //Vec                   // 0x0D
        //Map                   // 0x0E
        String(String),         // 0x0F
    }

    impl Literal{
        pub fn get_type(&self) -> Type{
            match self {
                Literal::F64(_) => Type::F64,
                Literal::I64(_) => Type::I64,
                Literal::String(_) => Type::String,
                Literal::Bool(_) => Type::Bool,
                Literal::None => Type::None,
            }
        }
    }

    /// AST 타입
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub enum Type {
        None             ,//=       0x71,
        I64              ,//=       0x72,
        F64              ,//=       0x73,
        //u64            ,//=       0x74,
        //i32            ,//=       0x75,
        //f32            ,//=       0x76,
        //u32            ,//=       0x77,
        //i16            ,//=       0x78,
        //u16            ,//=       0x79,
        //i8             ,//=       0x7A,
        //u8             ,//=       0x7B,
        Bool             ,//=       0x7C,
        //Vec(Type)      ,//=       0x7D,
        //Map            ,//=       0x7E,
        String           ,//=       0x7F,
        Class(String),    //=       0x80,
        Ref(Box<Type>),   //=       0x81,
    }

    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match *self {
                Type::F64 => write!(f, "f64"),
                Type::I64 => write!(f, "i64"),
                Type::String => write!(f, "string"),
                Type::Bool => write!(f, "bool"),
                Type::None => write!(f, "var"),
                Type::Class(_) => write!(f, "class"),
                Type::Ref(_) => write!(f, "ref"),
            }
        }
    }

    /// AST 연산
    #[derive(Debug, PartialEq, Clone)]
    pub enum Infix {
        Plus        = 0x20,     // +
        Minus       = 0x21,     // -
        Multiply    = 0x22,     // *
        Divide      = 0x23,     // /
        Mod         = 0x24,     // %
        Equal       = 0x25,     // ==
        NotEqual    = 0x26,     // !=
        LessThan    = 0x27,     // <
        GreaterThan = 0x28,     // >
        And         = 0x29,     // &&
        Or          = 0x2A,     // ||
        BitAnd      = 0x2B,     // &
        BitOr       = 0x2C,     // |
    }

    impl std::fmt::Display for Infix {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match *self {
                Infix::Plus => write!(f, "+"),
                Infix::Minus => write!(f, "-"),
                Infix::Multiply => write!(f, "*"),
                Infix::Divide => write!(f, "/"),
                Infix::Mod => write!(f, "%"),
                Infix::Equal => write!(f, "=="),
                Infix::NotEqual => write!(f, "!="),
                Infix::LessThan => write!(f, "<"),
                Infix::GreaterThan => write!(f, ">"),
                Infix::And => write!(f, "&&"),
                Infix::Or => write!(f, "||"),
                Infix::BitAnd => write!(f, "&"),
                Infix::BitOr => write!(f, "|"),
            }
        }
    }

    /// AST 접두연산
    #[derive(Debug, PartialEq, Clone)]
    pub enum Prefix {
        Minus       = 0x40,
        Not         = 0x41,
    }

    /// AST 상태
    #[derive(Debug, PartialEq, Clone)]
    pub enum Statement {
        Let{                                // 0x50 from to[0x54 addr type]
            variable: Expression,
            expression: Option<Expression>
        },
        Fn {                                // 0x51 return params_length body_size params[0x54 addr type, 0x54 addr type, ...] body
            identifier: String,
            return_type: Type,
            parameters: Vec<Expression>,
            body: Vec<Statement>,
        },
        Class {                             // 0x52
            identifier: String,
            members: Vec<ClassMember>
        },
        Return(Expression),                 // 0x53 exp
        Expression(Expression),
    }

    /// AST 구문
    #[derive(Debug, PartialEq, Clone)]
    pub enum Expression {
        Variable(String, Type),             // 0x54 type
        Identifier(String),                 // 변수 로드 = 0x55 addr
        Insert {                            // 0x56 from to
            variable:  Box<Expression>,
            expression: Box<Expression>
        },
        If {                                // 0x57 cond cons_length alter_length cons (alter)
            condition: Box<Expression>,
            consequence: Vec<Statement>,
            alternative: Option<Vec<Statement>>,
        },
        Call {                              // 0x58 addr args_length args[0x55 index, 0x55 index, ...]
            function: Box<Expression>,
            arguments: Vec<Expression>,
        },
        CallMethod {                        // 0x59
            class: Box<Expression>,
            call: Box<Expression>,
        },
        ClassVariable{                      // 0x60
            class:Box<Expression>,
            inits: Vec<Expression>
        },
        Literal(Literal),
        Prefix(Prefix, Box<Expression>),
        Infix(
            Infix,
            Box<Expression>,
            Box<Expression>
        ),
    }

    /// AST 우선순위
    #[derive(PartialEq, PartialOrd)]
    pub enum Precedence {
        Lowest,
        Assign,         // =
        Bool,           // && or ||
        Equals,         // == or !=
        LessGreater,    // > or <
        Sum,            // +
        Product,        // *
        Bit,            // & or |
        Prefix,         // -X or !X
        Call,           // myFunction(X)
    }

    /// AST 클래스 속 멤버
    #[derive(Debug, PartialEq, Clone)]
    pub enum ClassMember {
        Variable(bool, Expression),         // pub, let
        Method(bool, bool, Statement),      // pub, static, fn
    }
}

/// # 가상 머신에서 활용할 열거자 및 테이블
pub mod vm {
    use std::fmt;
    use rustc_hash::FxHashMap;
    use crate::ast::Type;

    /// 가상 머신 객체
    #[derive(Debug, PartialEq, Clone)]
    pub enum VMObejct {
        Null,
        I64(i64),
        F64(f64),
        Bool(bool),
        String(String),
        Fn(usize, u8, usize), // 함수의 시작 주소, 매개변수 개수, 바디의 크기
        Error(String)
    }

    impl fmt::Display for VMObejct {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                VMObejct::F64(ref value) => write!(f, "{value}"),
                VMObejct::I64(ref value) => write!(f, "{value}"),
                VMObejct::Bool(ref value) => write!(f, "{value}"),
                VMObejct::String(ref value) => write!(f, "{value}"),
                VMObejct::Fn(ref addr, ref params_count, ref body_size) =>
                    write!(f, "fn {addr}:({params_count}) {{{body_size}}}"),
                VMObejct::Null => write!(f, "null"),
                VMObejct::Error(ref value) => write!(f, "{value}"),
            }
        }
    }

    /// 가상 머신 상수
    #[derive(Debug, PartialEq, Clone)]
    pub enum Constant {
        None,                   // 0x01
        I64(i64),               // 0x02
        F64(f64),               // 0x03
        //u64                   // 0x04
        //i32                   // 0x05
        //f32                   // 0x06
        //u16                   // 0x07
        //i16                   // 0x08
        //u16                   // 0x09
        //i8                    // 0x0A
        //u8                    // 0x0B
        Bool(bool),             // 0x0C
        //Vec                   // 0x0D
        //Map                   // 0x0E
        String(String),         // 0x0F
    }

    /// 가상 머신 테이블
    #[derive(Debug, PartialEq, Clone)]
    pub struct Scope {
        /// 변수 혹은 함수 식별자를 담는 테이블: 식별자 이름, (주소, 타입)
        pub table: FxHashMap<String, (usize, Type)>,
    }

    impl Scope {
        pub fn new() -> Self {
            Scope {
                table: FxHashMap::default(),
            }
        }
    }
}
