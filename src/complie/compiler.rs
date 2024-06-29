use std::fs::File;
use std::io::Write;
use crate::complie::compile::{Compile, Compiling, Constant, Scope};
use crate::complie::decompiler::Descope;
use crate::ast::Program;

#[derive(Debug)]
pub struct Compiler;

const MAGIC_NUMBER: &[u8] = b"RVMB";

impl Compiler {
    pub fn new() -> Self {
        Compiler
    }

    pub fn export(&mut self, file_path:&str, program: Program){
        let mut compiling = Compiling::new();
        program.iter().for_each(|stmt| stmt.compile(&mut compiling));

        let mut file = File::create(file_path).expect("파일 생성 실패");

        // File Header
        file.write_all(MAGIC_NUMBER).expect("매직 넘버 작성 실패");
        file.write_all(&[0x00, 0x01, 0x00]).expect("파일 버전 크기 작성 실패"); // Version 1

        // Constant Pool
        self.write_constant_pool(&mut file, &compiling.constant_pool);

        // Code Section
        self.write_code_section(&mut file, &compiling.bytecode);

        // Scope Information
        self.write_scope_info(&mut file, &compiling.scopes);
    }

    fn write_constant_pool(&self, file: &mut File, constant_pool: &[Constant]) {
        file.write_all(&(constant_pool.len() as u16).to_le_bytes()).expect("Failed to write constant pool size");
        for constant in constant_pool {
            match constant {
                Constant::I64(i) => {
                    file.write_all(&[0x02]).expect("상수 타입 작성 실패");
                    file.write_all(&i.to_le_bytes()).expect("정수 값 작성 실패");
                }
                Constant::F64(f) => {
                    file.write_all(&[0x03]).expect("상수 타입 작성 실패");
                    file.write_all(&f.to_le_bytes()).expect("부동소수점 값 작성 실패");
                }
                Constant::Bool(b) => {
                    file.write_all(&[0x0C]).expect("상수 타입 작성 실패");
                    file.write_all(&[if *b { 1 } else { 0 }]).expect("논리 값 작성 실패");
                }
                Constant::String(s) => {
                    file.write_all(&[0x0F]).expect("상수 타입 작성 실패");
                    file.write_all(&(s.len() as u32).to_le_bytes()).expect("문자열 크기 작성 실패");
                    file.write_all(s.as_bytes()).expect("문자열 값 작성 실패");
                }
            }
        }
    }

    fn write_code_section(&self, file: &mut File, bytecode: &[u8]) {
        file.write_all(&(bytecode.len() as u32).to_le_bytes()).expect("바이트 코드 크기 작성 실패");
        file.write_all(bytecode).expect("바이트 코드 작성 실패");
    }

    fn write_scope_info(&self, file: &mut File, scopes: &[Scope]) {
        file.write_all(&(scopes.len() as u16).to_le_bytes()).expect("스코프 크기 작성 실패");
        for scope in scopes {
            file.write_all(&(scope.stack.len() as u16).to_le_bytes()).expect("변수 개수 작성 실패");
            for (name, (addr, typ)) in &scope.stack {
                file.write_all(&(name.len() as u8).to_le_bytes()).expect("변수 식별자 크기 작성 실패");
                file.write_all(name.as_bytes()).expect("변수 식별자 작성 실패");
                file.write_all(&(*addr as u16).to_le_bytes()).expect("변수 주소 작성 실패");
                file.write_all(&[typ.clone() as u8]).expect("변수 타입 작성 실패");
            }
            file.write_all(&(scope.functions.len() as u16).to_le_bytes()).expect("함수 개수 작성 실패");
            for (name, (addr, typ)) in &scope.functions {
                file.write_all(&(name.len() as u8).to_le_bytes()).expect("함수 식별자 크기 작성 실패");
                file.write_all(name.as_bytes()).expect("함수 식별자 작성 실패");
                file.write_all(&(*addr as u16).to_le_bytes()).expect("함수 주소 작성 실패");
                file.write_all(&[typ.clone() as u8]).expect("함수 타입 작성 실패");
            }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::complie::compiler::Compiler;
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    #[test]
    fn test_compile() {
        let input = r#"
        let five:i64 = 5;
        let ten:f64 = 10;

        fn add(x:i64, y) {
            return x + y;
        };

        let result = add(five, ten);
        result = five * ten;
        result
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:02x?}", program);

        let mut compiler = Compiler::new();
        let (_,byte_code) = compiler.compile(&program);

        println!("{:?}", byte_code);
    }

    #[test]
    fn test_export() {
        let input = r#"
        let five:i64 = 5;
        let ten:f64 = 10;

        fn add(x:i64, y) {
            return x + y;
        };

        let result = add(five, ten);
        result = five * ten;
        result
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();

        let mut compiler = Compiler::new();
        compiler.export("test.irs", program);
    }
}