use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use crate::complie::compile::Scope;
use crate::ast::{Expression, Infix, Literal, Prefix, Program, Statement, Type};

#[derive(Debug, PartialEq, Clone)]
pub struct Descope {
    pub(crate) variables: HashMap<usize, String>,
    pub(crate) functions: HashMap<usize, String>,
}

impl Descope {
    pub fn new() -> Descope {
        Descope {
            variables: HashMap::new(),
            functions: HashMap::new()
        }
    }
    fn get_variable(&self, index: usize) -> Option<String> {
        self.variables.get(&index).cloned()
    }
    fn get_function(&self, index: usize) -> Option<String> {
        self.functions.get(&index).cloned()
    }
}

impl Scope {
    pub(crate) fn to_descope(&self) -> Descope {
        let mut descope = Descope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        };

        for (name, (index,_ )) in &self.stack {
            descope.variables.insert(*index, name.clone());
        }
        for (name, (index,_ )) in &self.functions {
            descope.functions.insert(*index, name.clone());
        }

        descope
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Decompiler {
    index:usize,
    scopes:Vec<Descope>,
    bytecode:Vec<u8>
}
impl Decompiler {
    pub fn new() -> Decompiler {
        Decompiler {
            index: 0,
            scopes: vec![],
            bytecode: vec![]
        }
    }

    fn import_usize(&mut self, file: &Vec<u8>, index: usize) -> (usize, usize) {
        let size = std::mem::size_of::<usize>();
        let mut array = [0u8; std::mem::size_of::<usize>()];
        array.copy_from_slice(&file[index..index + size]);
        (usize::from_le_bytes(array), size)
    }

    fn import_v0_1_0(&mut self, file: &Vec<u8>, mut index: usize) -> usize {
        loop {
            let (scope, size) = self.import_usize(&file, index);
            index += size;

            let (variable_length, size) = self.import_usize(&file, index);
            index += size;
            for _ in 0..variable_length {
                let (name_length, size) = self.import_usize(&file, index);
                index += size;

                if let Ok(name) = String::from_utf8(file[index..index + name_length].to_vec()){
                    index += name_length;
                    let (v_index, size) = self.import_usize(&file, index);
                    index += size;

                    if self.scopes.len() == scope {
                        self.scopes.push(Descope::new());
                    }
                    self.scopes[scope].variables.insert(v_index, name);
                } else {
                    panic!("변수 이름 확인 실패 {0}", index);
                }
            }

            let (function_length, size) = self.import_usize(&file, index);
            index += size;
            for _ in 0..function_length {
                let (name_length, size) = self.import_usize(&file, index);
                index += size;

                if let Ok(name) = String::from_utf8(file[index..index + name_length].to_vec()){
                    index += name_length;
                    let (v_index, size) = self.import_usize(&file, index);
                    index += size;

                    if self.scopes.len() == scope {
                        self.scopes.push(Descope::new());
                    }
                    self.scopes[scope].functions.insert(v_index, name);
                } else {
                    panic!("함수 이름 확인 실패 {0}", index);
                }
            }

            if file[index] == 0xff {
                break;
            }
        }
        index
    }

    pub fn import(&mut self, file_path:&str) -> (Program, String) {
        self.scopes.clear();

        let mut log:String = String::from(file_path);
        let mut open = File::open(file_path)
            .expect(format!("해당 \"{0}\" 파일을 찾을 수 없습니다.", file_path).as_str());
        let mut file = Vec::new();
        open.read_to_end(&mut file)
            .expect(format!("해당 \"{0}\" 파일을 읽을 수 없습니다.", file_path).as_str());

        let magic = String::from("interust");
        let mut index =magic.len();
        if let Ok(import_magic) = String::from_utf8(file[0..index].to_vec()){
            if magic != import_magic {
                panic!("매직 넘버 확인 실패 {0}", index);
            }
        } else {
            panic!("매직 넘버 확인 실패 {0}", index);
        }
        let (version_length, size) = self.import_usize(&file, index);
        index += size;

        let file_version = String::from_utf8(file[index..index + version_length].to_vec())
            .expect(format!("파일 버전 확인 실패 {0}", index).as_str());
        log = log + format!(" v{}", file_version).as_str();

        index += version_length;

        let (data_offset, size) = self.import_usize(&file, index);
        index += size;

        if file[index] != 0xff {
            if file_version == "0.1.0" {
                index = self.import_v0_1_0(&file, index);
            }
        }
        index += 1;
        loop {
            // 의존성 정보 확인 시작

            if index == data_offset {
                break;
            }
        }

        (self.decompile(file[index..].to_vec()), log)
    }

    pub fn decompile(&mut self, bytecode:Vec<u8>) -> Program {
        self.index = 0;
        self.bytecode = bytecode;
        let mut program:Program = vec![];
        let length = self.bytecode.len();

        while self.index < length {
            if self.expect_code(0xff) {
                break;
            } else if let Some(state) = self.parse_statement() {
                program.push(state);
            } else {
                self.index += 1;
            }
        }

        program
    }

    fn parse_usize(&mut self) -> usize {
        let size = std::mem::size_of::<usize>();
        let mut array = [0u8; std::mem::size_of::<usize>()];
        array.copy_from_slice(&self.bytecode[self.index..self.index + size]);
        self.index += size;

        usize::from_le_bytes(array)
    }

    fn expect_code(&mut self, code:u8) -> bool {
        self.bytecode[self.index] == code
    }

    fn parse_literal(&mut self) -> Option<Expression> {
        match self.bytecode[self.index] {
            0x01 => {
                self.index += 1;
                Some(Expression::Literal(Literal::None))
            },
            0x02 => {
                self.index += 1;
                let mut array = [0u8; 8];
                array.copy_from_slice(&self.bytecode[self.index..self.index + 8]);
                self.index += 8;

                return Some(Expression::Literal(Literal::I64(i64::from_le_bytes(array))));
            },
            0x03 => {
                self.index += 1;
                let mut array = [0u8; 8];
                array.copy_from_slice(&self.bytecode[self.index..self.index + 8]);
                self.index += 8;

                return Some(Expression::Literal(Literal::F64(f64::from_le_bytes(array))));
            },
            0x04 => {
                self.index += 1;
                let b = self.bytecode[self.index] == 1;
                self.index += 1;
                return Some(Expression::Literal(Literal::Bool(b)));
            },
            0x0F => {
                self.index += 1;
                let length:usize = self.parse_usize();

                let mut array = vec![0u8; length];
                array.copy_from_slice(&self.bytecode[self.index..self.index + length]);
                self.index += length;

                if let Ok(string) = String::from_utf8(array){
                    return Some(Expression::Literal(Literal::String(string)));
                } else {
                    panic!("디컴파일 오류 : 잘못된 문자열");
                }
            },
            _ => {
                eprintln!("디컴파일 오류 : Literal 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                None
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let typ = match self.bytecode[self.index] {
            0x71 => Some(Type::None),
            0x72 => Some(Type::I64),
            0x73 => Some(Type::F64),
            0x7C => Some(Type::Bool),
            0x7F => Some(Type::String),
            _ => {
                eprintln!("디컴파일 오류 : Type 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                None
            }
        };
        self.index += 1;
        typ
    }

    fn parse_infix(&mut self) -> Option<Expression> {
        let infix:Infix = match self.bytecode[self.index] {
            0x20 => Infix::Plus,
            0x21 => Infix::Minus,
            0x22 => Infix::Multiply,
            0x23 => Infix::Divide,
            0x24 => Infix::Mod,
            0x25 => Infix::Equal,
            0x26 => Infix::NotEqual,
            0x27 => Infix::LessThan,
            0x28 => Infix::GreaterThan,
            0x29 => Infix::And,
            0x2A => Infix::Or,
            0x2B => Infix::BitAnd,
            0x2C => Infix::BitOr,
            _ => {
                panic!("디컴파일 오류 : 식별 바이트 코드 매치 불가");
            }
        };
        self.index += 1;
        let left = Box::new(self.parse_expression().unwrap());
        let right = Box::new(self.parse_expression().unwrap());
        Some(Expression::Infix(infix, left, right))
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        let prefix:Prefix = match self.bytecode[self.index] {
            0x40 => Prefix::Minus,
            0x41 => Prefix::Not,
            _ => {
                eprintln!("디컴파일 오류 : Prefix 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                return None
            }
        };
        self.index += 1;
        let expression = Box::new(self.parse_expression().unwrap());
        Some(Expression::Prefix(prefix, expression))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.bytecode[self.index] {
            0x50 => { // Let
                self.index += 1;
                if let Some(variable) = self.parse_expression() {
                    let mut expression: Option<Expression> = None;
                    if !self.expect_code(0x51) {
                        let init = self.parse_expression().unwrap();
                        expression = Some(init);
                    }
                    self.index += 1;

                    Some(Statement::Let { variable, expression })
                } else {
                    eprintln!("디컴파일 오류 : Let 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                    None
                }
            },
            0x52 => { // Return
                self.index += 1;
                if let Some(value) = self.parse_expression() {
                    Some(Statement::Return(value))
                } else {
                    eprintln!("디컴파일 오류 : Return 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                    None
                }
            },
            _ => { // Expression
                if let Some(expression) = self.parse_expression(){
                    Some(Statement::Expression(expression))
                } else {
                    None
                }
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        match self.bytecode[self.index] {
            0x53 => { // Variable
                self.index += 1;
                let scope:usize = self.parse_usize();
                let index:usize = self.parse_usize();
                let typ = self.parse_type();
                if let Some(typ) = typ {
                    if self.scopes.len() > scope {
                        if let Some(name) = self.scopes[scope].get_variable(index) {
                            return Some(Expression::Variable(name, typ))
                        } else {
                            println!("디컴파일 오류 : Variable Scope 해당 하는 변수 식별자 없음");
                            None
                        }
                    } else {
                        Some(Expression::Variable(format!("v{0}_{1}",scope, index), typ))
                    }
                } else {
                    eprintln!("디컴파일 오류 : Variable Type 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                    None
                }
            },
            0x54 => { // Identifier
                self.index += 1;
                let scope:usize = self.parse_usize();
                let index:usize = self.parse_usize();
                let variable:bool = self.bytecode[self.index] == 0;
                self.index += 1;
                if variable {
                    if self.scopes.len() > scope {
                        if let Some(name) = self.scopes[scope].get_variable(index) {
                            return Some(Expression::Identifier(name))
                        } else {
                            println!("디컴파일 오류 : Variable Scope 해당 하는 변수 식별자 없음");
                            None
                        }
                    } else {
                        Some(Expression::Identifier(format!("v{0}_{1}",scope, index)))
                    }
                } else {
                    if self.scopes.len() > scope {
                        if let Some(name) = self.scopes[scope].get_function(index) {
                            return Some(Expression::Identifier(name))
                        } else {
                            println!("디컴파일 오류 : Function Scope 해당 하는 함수 식별자 없음");
                            None
                        }
                    } else {
                        Some(Expression::Identifier(format!("f{0}_{1}",scope, index)))
                    }
                }
            },
            0x55 => { // Insert
                self.index += 1;
                let variable = Box::new(self.parse_expression().unwrap());
                let expression = Box::new(self.parse_expression().unwrap());
                Some(Expression::Insert{variable, expression})
            },
            0x56 => { // If
                self.index += 1;
                let condition = Box::new(self.parse_expression().unwrap());
                let length:usize = self.parse_usize();
                let mut consequence:Vec<Statement> = vec![];
                for _ in 0..length {
                    let expression = self.parse_statement().unwrap();
                    consequence.push(expression);
                }
                let mut alternative:Option<Vec<Statement>> = None;
                if !self.expect_code(0x57) {
                    let length:usize = self.parse_usize();
                    let mut alters:Vec<Statement> = vec![];
                    for _ in 0..length {
                        let expression = self.parse_statement().unwrap();
                        alters.push(expression);
                    }
                    alternative = Some(alters);
                }
                self.index += 1;

                Some(Expression::If{condition, consequence, alternative})
            },
            0x58 => { // Fn
                self.index += 1;
                let scope:usize = self.parse_usize();
                let index:usize = self.parse_usize();
                let return_type = self.parse_type();
                let length:usize = self.parse_usize();
                let mut parameters:Vec<Expression> = vec![];
                for _ in 0..length {
                    let expression = self.parse_expression().unwrap();
                    parameters.push(expression);
                }
                let length:usize = self.parse_usize();
                let mut body:Vec<Statement> = vec![];
                for _ in 0..length {
                    let expression = self.parse_statement().unwrap();
                    body.push(expression);
                }

                if let Some(return_type) = return_type {
                    if self.scopes.len() > scope {
                        if let Some(name) = self.scopes[scope].get_function(index) {
                            return Some(Expression::Fn {
                                identifier: name,
                                parameters,
                                body,
                                return_type
                            });
                        } else {
                            println!("디컴파일 오류 : Function Scope 해당 하는 함수 식별자 없음");
                            None
                        }
                    } else {
                        Some(Expression::Fn {
                            identifier: format!("f{0}_{1}",scope, index),
                            parameters,
                            body,
                            return_type
                        })
                    }
                } else {
                    eprintln!("디컴파일 오류 : Fn 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                    None
                }
            },
            0x59 => { // Call
                self.index += 1;
                let function = Box::new(self.parse_expression().unwrap());
                let length:usize = self.parse_usize();
                let mut arguments:Vec<Expression> = vec![];
                for _ in 0..length {
                    let expression = self.parse_expression().unwrap();
                    arguments.push(expression);
                }

                Some(Expression::Call {
                    function,
                    arguments
                })
            },
            0x01..=0x0F => {
                self.parse_literal()
            }
            0x20..=0x2C => {
                self.parse_infix()
            }
            0x40..=0x41 => {
                self.parse_prefix()
            }
            _ => {
                eprintln!("디컴파일 오류 : Expression 식별 바이트 코드({0:#02x}, {0:#02x}, index:{1}) 매치 불가 : 이전 코드({2:#02x})", self.bytecode[self.index], self.index, self.bytecode[self.index - 1]);
                None
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::complie::compiler::Compiler;
    use crate::complie::decompiler::{Decompiler, Descope};
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    #[test]
    fn test_compile_function() {
        let input = r#"
            fn f0_0(v1_0:i64, v1_1) {
                return v1_0 + v1_1;
            };
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let (_,byte_code) = compiler.compile(&program);
        println!("{:02x?}", byte_code);

        let mut decompiler = Decompiler::new();
        let decompiled_program = decompiler.decompile(byte_code);
        println!("{:?}", decompiled_program);
        assert_eq!(program, decompiled_program);
    }

    #[test]
    fn test_compile_ne() {
        let input = r#"
        let five:i64 = -5;
        let ten:f64 = 10;

        fn add(v1_0:i64, v1_1) {
            return v1_0 + v1_1;
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
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let (_, byte_code) = compiler.compile(&program);
        println!("{:02x?}", byte_code);

        let mut decompiler = Decompiler::new();
        let decompiled_program = decompiler.decompile(byte_code);
        println!("{:?}", decompiled_program);
        assert_ne!(program, decompiled_program);
    }

    #[test]
    fn test_descope() {
        let input = r#"
        let five:i64 = -5;
        let ten:f64 = 10;

        fn add(x:i64, y) {
            return x + y;
        }

        let result = add(five, ten);
        result = five * ten;
        result
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);

        let mut compiler = Compiler::new();
        let (scopes, byte_code) = compiler.compile(&program);
        let descopes:Vec<Descope> = scopes.iter().map(|x|x.to_descope()).collect();

        let mut decompiler = Decompiler::new();
        decompiler.scopes = descopes;
        let decompiled_program = decompiler.decompile(byte_code);
        println!("{:?}", decompiled_program);
        assert_eq!(program, decompiled_program);
    }

    #[test]
    fn test_import() {
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

        let mut decompiler = Decompiler::new();
        let (decompiled_program, log) = decompiler.import("test.irs");

        println!("{}", log);
        println!("{:?}", decompiled_program);
        assert_eq!(program, decompiled_program);
    }
}