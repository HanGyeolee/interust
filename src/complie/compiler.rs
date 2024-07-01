use std::fs::File;
use std::io::Write;
use crate::complie::compile::{Compile, Compiling};
use crate::ast::{Expression, Program, Statement};
use crate::vmobject::{Constant, Scope};

#[derive(Debug)]
pub struct Compiler;

pub const MAGIC_NUMBER: &[u8] = b"RVMB";

impl Compiler {
    pub fn new() -> Self {
        Compiler
    }

    pub fn compile(&mut self, program: Program) -> Compiling{
        let mut compiling = Compiling::new();
        program.iter().for_each(|stmt| stmt.compile(&mut compiling));

        compiling
    }

    pub fn export(&mut self, file_path:&str, program: Program){
        let removed = program.into_iter().filter(|x| {
            return match x {
                Statement::Let {..} => true,
                Statement::Return(_) => false,
                Statement::Expression(x) =>
                    match x {
                        Expression::Fn {..} => true,
                        _ => false
                    }
            };
        }).collect();
        let compiling:Compiling = self.compile(removed);
        self.export_from(file_path, &compiling);
    }

    fn export_from(&mut self, file_path:&str, compiling: &Compiling) {
        let mut byte_code:Vec<u8> = vec![];
        // File Header
        byte_code.write_all(MAGIC_NUMBER).expect("매직 넘버 작성 실패");
        byte_code.write_all(&[0x00, 0x01, 0x00]).expect("파일 버전 크기 작성 실패"); // Version 1

        // Constant Pool
        self.write_constant_pool(&mut byte_code, &compiling.constants);

        // Code Section
        self.write_code_section(&mut byte_code, &compiling.bytecode);

        let (first_scope, _) = &compiling.scopes[0];
        // Scope Information
        self.write_scope_info(&mut byte_code, first_scope);

        // println!("{:02x?}", byte_code);

        let mut file = File::create(file_path).expect("파일 생성 실패");
        file.write_all(byte_code.as_slice()).expect("버퍼 작성 실패");
    }

    fn write_constant_pool(&self, binary: &mut Vec<u8>, constant_pool: &[Constant]) {
        binary.write_all(&(constant_pool.len() as u16).to_le_bytes()).expect("Failed to write constant pool size");
        for constant in constant_pool {
            match constant {
                Constant::None => binary.write_all(&[0x01]).expect("상수 타입 작성 실패"),
                Constant::I64(i) => {
                    binary.write_all(&[0x02]).expect("상수 타입 작성 실패");
                    binary.write_all(&i.to_le_bytes()).expect("정수 값 작성 실패");
                }
                Constant::F64(f) => {
                    binary.write_all(&[0x03]).expect("상수 타입 작성 실패");
                    binary.write_all(&f.to_le_bytes()).expect("부동소수점 값 작성 실패");
                }
                Constant::Bool(b) => {
                    binary.write_all(&[0x0C]).expect("상수 타입 작성 실패");
                    binary.write_all(&[if *b { 1 } else { 0 }]).expect("논리 값 작성 실패");
                }
                Constant::String(s) => {
                    binary.write_all(&[0x0F]).expect("상수 타입 작성 실패");
                    binary.write_all(&(s.len() as u32).to_le_bytes()).expect("문자열 크기 작성 실패");
                    binary.write_all(s.as_bytes()).expect("문자열 값 작성 실패");
                }
            }
        }
    }

    fn write_code_section(&self, binary: &mut Vec<u8>, bytecode: &[u8]) {
        binary.write_all(&bytecode.len().to_le_bytes()).expect("바이트 코드 크기 작성 실패");
        binary.write_all(bytecode).expect("바이트 코드 작성 실패");
    }

    fn write_scope_info(&self, binary: &mut Vec<u8>, scope: &Scope) {
        binary.write_all(&(scope.stack.len() as u16).to_le_bytes()).expect("식별자 개수 작성 실패");
        for (name, (addr, typ)) in &scope.stack {
            binary.write_all(&(name.len() as u8).to_le_bytes()).expect("식별자 문자열 크기 작성 실패");
            binary.write_all(name.as_bytes()).expect("식별자 문자열 작성 실패");
            binary.write_all(&(*addr as u16).to_le_bytes()).expect("식별자 주소 작성 실패");
            binary.write_all(&[typ.clone() as u8]).expect("식별자 타입 작성 실패");
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
        let byte_code = compiler.compile(program);

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