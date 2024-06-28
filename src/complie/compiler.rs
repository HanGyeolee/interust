use std::fs::File;
use std::io::Write;
use crate::complie::compile::{Compile, Scope};
use crate::complie::decompiler::Descope;
use crate::ast::Program;

#[derive(Debug, PartialEq, Clone)]
pub struct Compiler {
    scopes: Vec<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            scopes: vec![Scope::new()],
        }
    }

    pub fn export(&mut self, file_path:&str, program: Program){
        let (scopes, byte_code):(Vec<Scope>, Vec<u8>) = self.compile(&program);
        let descopes:Vec<Descope> = scopes.iter().map(|x|x.to_descope()).collect();

        let magic = "interust".as_bytes();
        let file_version = env!("CARGO_PKG_VERSION");
        let version_length = file_version.as_bytes().len().to_le_bytes();
        let file_version = file_version.as_bytes();
        //data_offset;
        let mut tables:Vec<u8> = vec![];
        let dependency:Vec<u8> = vec![];

        let scopes_length = descopes.len();
        for scope in 0..scopes_length {
            tables.extend(scope.to_le_bytes());
            tables.extend(&descopes[scope].variables.len().to_le_bytes());
            for (index, name) in &descopes[scope].variables {
                tables.extend(name.len().to_le_bytes());
                tables.extend(name.as_bytes());
                tables.extend(index.to_le_bytes());
            }
            tables.extend(&descopes[scope].functions.len().to_le_bytes());
            for (index, name) in &descopes[scope].functions {
                tables.extend(name.len().to_le_bytes());
                tables.extend(name.as_bytes());
                tables.extend(index.to_le_bytes());
            }
        }

        let dl:Vec<u8> = vec![0xff];
        let data_offset = (magic.len() + version_length.len() +
            file_version.len() + 0usize.to_le_bytes().len() +
            tables.len() + dl.len() + dependency.len()).to_le_bytes();

        if let Ok(mut file) = File::create(file_path){
            file.write(magic).expect("매직 넘버 작성 실패");
            file.write(&version_length).expect("파일 버전 크기 작성 실패");
            file.write(file_version).expect("파일 버전 작성 실패");
            file.write(&data_offset).expect("데이터 섹션 오프셋 작성 실패");
            file.write(tables.as_slice()).expect("테이블 작성 실패");
            file.write(dl.as_slice()).expect("의존성 정보 토큰 작성 실패");
            file.write(dependency.as_slice()).expect("의존성 정보 작성 실패");
            file.write(byte_code.as_slice()).expect("바이트 코드 작성 실패");
        } else {
            eprintln!("파일 생성 실패");
        }
    }

    pub fn compile(&mut self, program: &Program) -> (Vec<Scope>, Vec<u8>) {
        let mut byte_code:Vec<u8> = vec![];
        program.iter().for_each(|stmt| stmt.compile(0, &mut self.scopes, &mut byte_code));
        byte_code.push(0xFF); // Halt 프로그램 종료
        return (self.scopes.clone(), byte_code);
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