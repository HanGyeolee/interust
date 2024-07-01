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
use crate::ast::{Program, Type};
use crate::complie::compiler::{Compiler, MAGIC_NUMBER};
use crate::interpreter::environment::Environment;
use interpreter::Interpreter;
use crate::object::Object;
use crate::parser::parser::Parser;
use crate::token::Token;
use crate::tokenizer::tokenizer::Tokenizer;
use crate::virtualmachine::{BytecodeEngine};
use crate::vmobject::{Scope, VMObejct};

mod tokenizer;
mod parser;
mod interpreter;
mod complie;

pub mod object;
pub mod token;
pub mod ast;
mod virtualmachine;
mod vmobject;

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
    /// use interust::InterustCompiler;
    /// use interust::token::Token;
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
    /// use interust::ast::*;
    /// use interust::token::Token;
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
    /// use interust::ast::*;
    /// use interust::token::Token;
    /// use interust::InterustCompiler;
    /// use interust::object::Object;
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
    /// use interust::object::Object;
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
    /// use interust::object::Object;
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
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustCompiler::new();
    /// interust.run_interpreter();
    /// ```
    pub fn run_interpreter(&mut self) -> Option<Object> {
        self.interpreter.eval(self.program.clone())
    }
}

pub struct InterustVM {
    bytecode_engine:BytecodeEngine,
}

impl InterustVM{
    /// VitualMachine 엔진을 생성합니다.
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

    /// 인터프리터에서 컴파일된 파일을 읽고 상수와 바이트 코드, 외부에서 참조할 수 있는 전역 변수를 설정합니다.
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

        let mut index =MAGIC_NUMBER.len();
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


    pub fn call_compiled_fn(&mut self, name:String, params: Vec<VMObejct>) -> VMObejct {
        self.bytecode_engine.call_function(name, params)
    }
    pub fn call_compiled_var(&mut self, name:String) -> VMObejct {
        self.bytecode_engine.call_variable(name)
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
                scope.stack.insert(name, (addr as usize, typ));
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
    use crate::{InterustCompiler, InterustVM};
    use crate::object::Object;

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
