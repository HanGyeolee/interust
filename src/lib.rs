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
use std::rc::Rc;
use crate::ast::Program;
use crate::complie::compiler::Compiler;
use crate::complie::decompiler::Decompiler;
use crate::interpreter::environment::Environment;
use crate::interpreter::interpreter::Interpreter;
use crate::object::Object;
use crate::parser::parser::Parser;
use crate::token::Token;
use crate::tokenizer::tokenizer::Tokenizer;

mod tokenizer;
mod parser;
mod interpreter;
mod complie;

pub mod object;
pub mod token;
pub mod ast;

pub struct InterustEngine {
    interpreter: Interpreter,
    compiler: Compiler,
    decompiler: Decompiler,
    program: Program,
}

impl InterustEngine {
    /// 인터프리터 엔진을 생성합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustEngine;
    ///
    /// let mut interust = InterustEngine::new();
    /// ```
    pub fn new() -> InterustEngine {
        InterustEngine {
            interpreter: Interpreter::new(Rc::new(RefCell::new(Environment::new()))),
            compiler: Compiler::new(),
            decompiler: Decompiler::new(),
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
    /// use interust::InterustEngine;
    /// use interust::token::Token;
    ///
    /// let mut interust = InterustEngine::new();
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
    /// use interust::InterustEngine;
    ///
    /// let mut interust = InterustEngine::new();
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
    /// use interust::InterustEngine;
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustEngine::new();
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
    /// use interust::InterustEngine;
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustEngine::new();
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

    /// 인터프리터에서 여태까지 작성된 코드를 컴파일하여 바이트 코드 파일로 출력합니다.
    ///
    /// # 매개변수
    /// - `file_path` : `&str` 타입, 저장될 파일 위치<br/>
    /// 파일 확장자는 **.irs**, 반드시 작성할 필요는 없다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustEngine;
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustEngine::new();
    /// interust.eval_string(r#"
    ///     let a = 5;
    ///     a
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

    /// 인터프리터에서 컴파일된 바이트 코드 파일을 읽고 추상 구문 트리(AST)로 디컴파일합니다.
    /// 디컴파일링으로 들어온 새로운 코드를 `self.program` 에 이어 붙입니다.
    ///
    /// # 매개변수
    /// - `file_path` : `&str` 타입, 저장될 파일 위치<br/>
    /// 반드시 파일 확장자 **.irs**가 포함되어야 한다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustEngine;
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustEngine::new();
    /// interust.import("test.irs");
    /// ```
    pub fn import(&mut self, file_path:&str) {
        let (program, log) = self.decompiler.import(file_path);
        println!("{}", log);
        self.program.extend(program);
    }

    /// 인터프리터에서 `self.program`에 저장된 전체 추상 구문 트리(AST)를 처음부터 실행합니다.
    ///
    /// # 예제
    ///
    /// ```
    /// use interust::InterustEngine;
    /// use interust::object::Object;
    ///
    /// let mut interust = InterustEngine::new();
    /// interust.import("test.irs");
    /// interust.run();
    /// ```
    pub fn run(&mut self) -> Option<Object> {
        self.interpreter.eval(self.program.clone())
    }
}

#[cfg(test)]
mod test {
    use crate::InterustEngine;
    use crate::object::Object;
    use crate::token::Token::*;

    #[test]
    fn test_token_eq() {
        let mut interust = InterustEngine::new();
        if let Some(result) = interust.eval_string(r#"
            let a = 5;
            fn add(x:f64) -> i64 {
                return a + 3.5
            }
            add(a);
        "#){
            println!("{:?}", result);
            assert_eq!(result, Object::I64(8));
        }
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
