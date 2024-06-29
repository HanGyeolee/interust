use std::collections::HashMap;
use crate::ast::*;

pub trait Compile {
    fn compile(&self, compiler: &mut Compiling);
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    //None,                   // 0x01
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

#[derive(Debug, PartialEq, Clone)]
pub struct Compiling {
    pub scopes: Vec<Scope>,
    pub constant_pool: Vec<Constant>,
    pub bytecode: Vec<u8>,
}

impl Compiling {
    pub fn new() -> Self {
        Compiling {
            scopes: vec![Scope::new()],
            constant_pool: Vec::new(),
            bytecode: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        let index = self.constant_pool.len();
        self.constant_pool.push(constant);
        index as u16
    }

    pub fn emit(&mut self, byte: u8) {
        self.bytecode.push(byte);
    }

    pub fn emit_u16(&mut self, value: u16) {
        self.bytecode.extend_from_slice(&value.to_le_bytes());
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub stack: HashMap<String, (usize, Type)>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            stack: HashMap::new(),
        }
    }
}

impl Compile for Literal {
    fn compile(&self, compiler: &mut Compiling){
        match self {
            Literal::None => compiler.emit(0x01),
            Literal::I64(value) => {
                let index = compiler.add_constant(Constant::I64(*value));
                compiler.emit(0x02);
                compiler.emit_u16(index);
            }
            Literal::F64(value) => {
                let index = compiler.add_constant(Constant::F64(*value));
                compiler.emit(0x03);
                compiler.emit_u16(index);
            }
            Literal::Bool(value) => {
                let index = compiler.add_constant(Constant::Bool(*value));
                compiler.emit(0x0C);
                compiler.emit_u16(index);
            }
            Literal::String(value) => {
                let index = compiler.add_constant(Constant::String(value.clone()));
                compiler.emit(0x0F);
                compiler.emit_u16(index);
            }
        };
    }
}

impl Compile for Type {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
}

impl Compile for Infix {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
}

impl Compile for Prefix {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
}

impl Compile for Statement {
    fn compile(&self, compiler: &mut Compiling){
        match self {
            Statement::Let{variable, expression} => {
                // 0x50 from to[0x52 addr type]
                compiler.emit(0x50); // LET
                if let Some(expression) = expression {
                    expression.compile(compiler);
                } else {
                    Literal::None.compile(compiler);
                }
                variable.compile(compiler);
            }
            Statement::Return(value) => {
                compiler.emit(0x51); // RETURN
                value.compile(compiler);
            }
            Statement::Expression(value) => {
                value.compile(compiler);
            }
        };
    }
}

impl Compile for Expression {
    fn compile(&self, compiler: &mut Compiling){
        match self {
            Expression::Variable(name, typ) => {
                let scope = compiler.current_scope_mut();
                let index = scope.stack.len();
                scope.stack.insert(name.clone(), (index, typ.clone()));

                // 0x52 addr type
                compiler.emit(0x52);
                compiler.emit_u16(index as u16);
                typ.compile(compiler);
            }
            Expression::Identifier(name) => {
                if let Some((addr, _)) = compiler.current_scope_mut().stack.get(name) {
                    compiler.emit(0x53); // 변수 로드 = 0x53 addr
                    compiler.emit_u16(*addr as u16);
                } else {
                    panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없음", name));
                }
            }
            Expression::Insert{variable, expression} => {
                // 0x54 from to
                compiler.emit(0x54);
                expression.compile(compiler);
                variable.compile(compiler);
            }
            Expression::If{condition, consequence, alternative} => {
                // 0x55 cond cons_length alter_length cons (alter)
                compiler.emit(0x55);
                condition.compile(compiler);

                compiler.emit_u16(consequence.len() as u16);
                if let Some(alternative) = alternative {
                    compiler.emit_u16(alternative.len() as u16);
                } else {
                    compiler.emit_u16(0u16);
                }

                compiler.push_scope();
                for stmt in consequence {
                    stmt.compile(compiler);
                }
                compiler.pop_scope();

                if let Some(alternative) = alternative {
                    compiler.push_scope();
                    for stmt in alternative {
                        stmt.compile(compiler);
                    }
                    compiler.pop_scope();
                } else {
                    compiler.emit_u16(0); // Else 없음
                }
            }
            Expression::Fn {identifier, parameters, body, return_type} => {
                let func_index = compiler.current_scope_mut().stack.len();
                compiler.current_scope_mut().stack.insert(identifier.clone(), (func_index, return_type.clone()));

                // 0x56 addr return params_length body_length params[0x52 type, 0x52 type, ...] body
                compiler.emit(0x56);
                compiler.emit_u16(func_index.len() as u16);
                return_type.compile(compiler);

                compiler.emit_u16(parameters.len() as u16);
                compiler.emit_u16(body.len() as u16);

                compiler.push_scope();
                for stmt in parameters {
                    stmt.compile(compiler);
                }
                for stmt in body {
                    stmt.compile(compiler);
                }
                compiler.pop_scope();
            }
            Expression::Call {function, arguments} => {
                // 0x57 addr args_length args[0x53 index, 0x53 index, ...]
                compiler.emit(0x57);
                if let Expression::Identifier(name) = function {
                    if let Some((addr, _)) = compiler.current_scope_mut().stack.get(name) {
                        compiler.emit_u16(*addr as u16); // 함수 로드 = addr
                    } else {
                        panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없음", name));
                    }
                }else {
                    panic!("컴파일 불가 : 잘못된 구문");
                }

                compiler.emit_u16(arguments.len() as u16);
                for stmt in arguments {
                    stmt.compile(compiler);
                }
            }
            Expression::Literal(value) => {
                value.compile(compiler);
            }
            Expression::Infix(infix, a, b) => {
                infix.compile(compiler);
                a.compile(compiler);
                b.compile(compiler);
            }
            Expression::Prefix(prefix, expression) => {
                prefix.compile(compiler);
                expression.compile(compiler);
            }
        };
    }
}