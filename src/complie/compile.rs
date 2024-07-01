use crate::{Constant, Expression, Infix, Literal, Prefix, Scope, Statement, Type};

pub trait Compile {
    fn compile(&self, compiler: &mut Compiling);
    fn get_length(&self) -> usize;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Compiling {
    pub scopes: Vec<(Scope, usize)>, // 스코프, 스코프 오프셋 인덱스
    pub scope_index: usize,
    pub constants: Vec<Constant>,
    pub bytecode: Vec<u8>,
}

impl Compiling {
    pub fn new() -> Self {
        Compiling {
            scopes: vec![(Scope::new(), 0usize)],
            scope_index: 0,
            constants: Vec::new(),
            bytecode: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        if let Some(index) = self.constants.iter().position(|x| {
           x.eq(&constant)
        }) {
            return index as u16;
        }
        let index = self.constants.len();
        self.constants.push(constant);
        index as u16
    }

    pub fn emit(&mut self, byte: u8) {
        self.bytecode.push(byte);
    }

    pub fn emit_u16(&mut self, value: u16) {
        self.bytecode.extend_from_slice(&value.to_le_bytes());
    }

    pub fn current_scope_mut(&mut self) -> &mut (Scope, usize) {
        self.scopes.last_mut().unwrap()
    }

    pub fn get_scope_mut(&mut self, index:usize) -> &mut (Scope, usize) {
        self.scopes.get_mut(index).unwrap()
    }

    pub fn push_scope(&mut self, index: usize) {
        self.scope_index += 1;
        self.scopes.push((Scope::new(), index));
    }

    pub fn pop_scope(&mut self) {
        self.scope_index -= 1;
        self.scopes.pop();
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

    fn get_length(&self) -> usize {
        match self {
            Literal::None => 1,
            _ => 3,
        }
    }
}

impl Compile for Type {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
    fn get_length(&self) -> usize { 1 }
}

impl Compile for Infix {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
    fn get_length(&self) -> usize { 1 }
}

impl Compile for Prefix {
    fn compile(&self, compiler: &mut Compiling){
        compiler.emit(self.clone() as u8);
    }
    fn get_length(&self) -> usize { 1 }
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
    fn get_length(&self) -> usize {
        let mut length = 0;
        match self {
            Statement::Let{variable, expression} => {
                // 0x50 from to[0x52 addr type]
                length += 1;
                if let Some(expression) = expression {
                    length += expression.get_length();
                } else {
                    length += Literal::None.get_length();
                }
                length += variable.get_length();
            }
            Statement::Return(value) => {
                length += 1;
                length += value.get_length();
            }
            Statement::Expression(value) => {
                length += value.get_length();
            }
        };
        length
    }
}

impl Compile for Expression {
    fn compile(&self, compiler: &mut Compiling){
        match self {
            Expression::Variable(name, typ) => {
                {
                    let (scope, offset) = compiler.current_scope_mut();
                    let index = scope.table.len() + offset.clone();
                    if let Some(_) = scope.table.get(name){
                        panic!("{}", format!("컴파일 불가 : 해당하는 변수 식별자({0})가 이미 있습니다.", name));
                    }
                    scope.table.insert(name.clone(), (index, typ.clone()));
                };

                // 0x52 type
                compiler.emit(0x52);
                typ.compile(compiler);
            }
            Expression::Identifier(name) => {
                let mut index = compiler.scope_index as i128;
                let mut not_found = true;

                while index > -1 {
                    let addr_opt = {
                        let (scope, _) = compiler.get_scope_mut(index as usize);
                        scope.table.get(name).map(|(addr, _)| addr.clone())
                    };

                    if let Some(addr) = addr_opt {
                        compiler.emit(0x53); // 변수 로드 = 0x53 addr
                        compiler.emit_u16(addr as u16);
                        not_found = false;
                        break;
                    } else {
                        index -= 1;
                    }
                }
                if not_found {
                    panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없습니다.", name));
                }
            }
            Expression::Insert{variable, expression} => {
                // 0x54 from to
                compiler.emit(0x54);
                expression.compile(compiler);
                variable.compile(compiler);
            }
            Expression::If{condition, consequence, alternative} => {
                // 0x55 cond cons_size alter_size cons (alter)
                compiler.emit(0x55);
                condition.compile(compiler);

                {
                    let mut size:usize = 0;
                    for stmt in consequence {
                        size += stmt.get_length();
                    }
                    compiler.emit_u16(size as u16);
                }
                if let Some(alternative) = alternative {
                    let mut size:usize = 0;
                    for stmt in alternative {
                        size += stmt.get_length();
                    }
                    compiler.emit_u16(size as u16);
                } else {
                    compiler.emit_u16(0u16);
                }

                let (scope, offset) = compiler.current_scope_mut();
                let new_scope_offset = scope.table.len() + offset.clone();
                compiler.push_scope(new_scope_offset);
                for stmt in consequence {
                    stmt.compile(compiler);
                }
                compiler.pop_scope();

                if let Some(alternative) = alternative {
                    let (scope, offset) = compiler.current_scope_mut();
                    let new_scope_offset = scope.table.len() + offset.clone();
                    compiler.push_scope(new_scope_offset);
                    for stmt in alternative {
                        stmt.compile(compiler);
                    }
                    compiler.pop_scope();
                }
            }
            Expression::Fn {identifier, parameters, body, return_type} => {
                {
                    let (scope, offset) = compiler.current_scope_mut();
                    let func_index = scope.table.len() + offset.clone();
                    if let Some(_) = scope.table.get(identifier){
                        panic!("{}", format!("컴파일 불가 : 해당하는 함수 식별자({0})가 이미 있습니다.", identifier));
                    }
                    scope.table.insert(identifier.clone(), (func_index, return_type.clone()));
                };

                // 0x56 return params_length body_size params[0x52 addr type, 0x52 addr type, ...] body
                compiler.emit(0x56);
                return_type.compile(compiler);

                compiler.emit_u16(parameters.len() as u16);
                {
                    let mut size:usize = 0;
                    for stmt in body {
                        size += stmt.get_length();
                    }
                    compiler.emit_u16(size as u16);
                }

                let new_scope_offset = {
                    let (scope, offset) = compiler.current_scope_mut();
                    scope.table.len() + offset.clone()
                };
                compiler.push_scope(new_scope_offset);
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
                if let Expression::Identifier(name) = function.as_ref() {
                    let mut index = compiler.scope_index as i128;
                    let mut not_found = true;

                    while index > -1 {
                        let addr_opt = {
                            let (scope, _) = compiler.get_scope_mut(index as usize);
                            scope.table.get(name).map(|(addr, _)| addr.clone())
                        };

                        if let Some(addr) = addr_opt {
                            compiler.emit_u16(addr as u16);
                            not_found = false;
                            break;
                        } else {
                            index -= 1;
                        }
                    }
                    if not_found {
                        panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없습니다.", name));
                    }
                }else {
                    panic!("컴파일 불가 : 잘못된 구문입니다.");
                }
                let mut reverse = arguments.clone();
                reverse.reverse();

                compiler.emit_u16(reverse.len() as u16);
                for stmt in reverse {
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
    fn get_length(&self) -> usize{
        let mut length = 0;
        match self {
            Expression::Variable(_, typ) => {
                // 0x52 type
                length += 1;
                length += typ.get_length();
            }
            Expression::Identifier(_) => {
                length += 1;
                length += 2;
            }
            Expression::Insert{variable, expression} => {
                length += 1;
                length += expression.get_length();
                length += variable.get_length();
            }
            Expression::If{condition, consequence, alternative} => {
                length += 1;
                length += condition.get_length();
                length += 4;
                for stmt in consequence {
                    length += stmt.get_length();
                }

                if let Some(alternative) = alternative {
                    for stmt in alternative {
                        length += stmt.get_length();
                    }
                }
            }
            Expression::Fn {parameters, body, return_type, .. } => {
                // 0x56 return params_length body_size params[0x52 addr type, 0x52 addr type, ...] body
                length += 1;
                length += return_type.get_length();
                length += 4;
                for stmt in parameters {
                    length += stmt.get_length();
                }
                for stmt in body {
                    length += stmt.get_length();
                }
            }
            Expression::Call {arguments, ..} => {
                // 0x57 addr args_length args[0x53 index, 0x53 index, ...]
                length += 1;
                length += 4;
                for stmt in arguments {
                    length += stmt.get_length();
                }
            }
            Expression::Literal(value) => {
                length += value.get_length();
            }
            Expression::Infix(infix, a, b) => {
                length += infix.get_length();
                length += a.get_length();
                length += b.get_length();
            }
            Expression::Prefix(prefix, expression) => {
                length += prefix.get_length();
                length += expression.get_length();
            }
        };
        length
    }
}