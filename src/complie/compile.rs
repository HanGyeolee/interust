use std::collections::HashMap;
use crate::ast::*;

pub trait Compile {
    fn compile(&self, scope_index:usize, scope: &mut Vec<Scope>, vec: &mut Vec<u8>);
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fn{
    /**
    상위 scope에서 함수의 Identifier 인덱스
     */
    pub index: usize,
    /**
    해당 함수가 목표로 하는 scope 인덱스
     */
    scope: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub variables: HashMap<String, usize>,
    variable_index: usize,
    pub functions: HashMap<String, Fn>,
    function_index: usize,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
            variable_index: 0,
            functions: HashMap::new(),
            function_index: 0,
        }
    }

    fn insert_variable(&mut self, name: String) -> usize{
        let index = self.variable_index;
        self.variables.insert(name, index);
        self.variable_index += 1;
        index
    }

    fn get_variable(&self, name: &str) -> Option<usize> {
        self.variables.get(name).cloned()
    }

    /**
    @param scope_index 해당 함수가 목표로 하는 scope 인덱스<br>
        함수 내부에서는 scope_index 에 있는 scope 에서 변수가 저장된다.
     */
    fn insert_function(&mut self, scope_index:usize, name:String) -> usize{
        let index  = self.function_index;
        self.functions.insert(name, Fn {
            index, scope: scope_index
        });
        self.function_index += 1;
        index
    }

    fn get_function(&self, name: &str) -> Option<Fn> {
        self.functions.get(name).cloned()
    }

    fn check_identifier_scope(&self, under_scope:usize) -> bool {
        self.functions.iter().all(|x| {
            x.1.scope == under_scope
        })
    }
}

impl Compile for Literal {
    fn compile(&self, _:usize, _: &mut Vec<Scope>, vec: &mut Vec<u8>){
        match self {
            Literal::None => vec.push(0x01),
            Literal::I64(value) => {
                vec.push(0x02);
                vec.extend(value.to_le_bytes())
            }
            Literal::F64(value) => {
                vec.push(0x03);
                vec.extend(value.to_le_bytes())
            }
            Literal::Bool(value) => {
                vec.push(0x0C);
                vec.push(if *value { 1 } else { 0 });
            }
            Literal::String(value) => {
                vec.push(0x0F);
                vec.extend(value.len().to_le_bytes());
                vec.extend(value.as_bytes());
            }
        };
    }
}

impl Compile for Type {
    fn compile(&self, _:usize, _: &mut Vec<Scope>, vec: &mut Vec<u8>){
        vec.push(self.clone() as u8);
    }
}

impl Compile for Infix {
    fn compile(&self, _:usize, _: &mut Vec<Scope>, vec: &mut Vec<u8>){
        vec.push(self.clone() as u8);
    }
}

impl Compile for Prefix {
    fn compile(&self, _:usize, _: &mut Vec<Scope>, vec: &mut Vec<u8>){
        vec.push(self.clone() as u8);
    }
}

impl Compile for Statement {
    fn compile(&self, scope_index:usize, scope: &mut Vec<Scope>, vec: &mut Vec<u8>){
        match self {
            Statement::Let{variable, expression} => {
                // 0x50 exp[0x52 type] (exp) 0x51
                vec.push(0x50);
                variable.compile(scope_index, scope, vec);
                if let Some(expression) = expression {
                    expression.compile(scope_index, scope, vec);
                } else {
                    Literal::None.compile(scope_index, scope, vec);
                }
                vec.push(0x51);
            }
            Statement::Return(value) => {
                vec.push(0x52);
                value.compile(scope_index, scope, vec);
            }
            Statement::Expression(value) => {
                value.compile(scope_index, scope, vec);
            }
        };
    }
}

impl Compile for Expression {
    fn compile(&self, scope_index:usize, scope: &mut Vec<Scope>, vec: &mut Vec<u8>){
        match self {
            Expression::Variable(name, typ) => {
                // 0x53 scope index type
                vec.push(0x53);
                vec.extend(scope_index.to_le_bytes());
                vec.extend(scope[scope_index].insert_variable(name.clone()).to_le_bytes());
                typ.compile(scope_index, scope, vec);
            }
            Expression::Identifier(name) => {
                // 0x54 scope index 0|1
                vec.push(0x54);
                let mut upper_scope = scope_index;
                if let Some(index) = scope[upper_scope].get_variable(name.clone().as_str()) {
                    vec.extend(upper_scope.to_le_bytes());
                    vec.extend(index.to_le_bytes());
                    vec.push(0x00);
                    return;
                } else if let Some(Fn { index, .. }) = scope[upper_scope].get_function(name.clone().as_str()) {
                    vec.extend(upper_scope.to_le_bytes());
                    vec.extend(index.to_le_bytes());
                    vec.push(0x01);
                    return;
                } else if upper_scope > 0{
                    upper_scope -= 1;
                } else {
                    panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없음", name));
                }

                let mut not_found = true;
                let mut under_index = scope_index;
                // 상위 스코프에서도 식별자를 찾는 다.
                while not_found {
                    // 하나씩 내려가면서 해당하는 목표 하위 스코프를 가지고 있는 지 판별한다.
                    if scope[upper_scope].check_identifier_scope(under_index) {
                        // if 와 같이 해당 상위 스코프에서 변수를 가져올 수 있을 때
                        if let Some(index) = scope[upper_scope].get_variable(name.clone().as_str()) {
                            vec.extend(upper_scope.to_le_bytes());
                            vec.extend(index.to_le_bytes());
                            vec.push(0x00);
                            not_found = false;
                        } else if let Some(Fn { index, .. }) = scope[upper_scope].get_function(name.clone().as_str()) {
                            vec.extend(upper_scope.to_le_bytes());
                            vec.extend(index.to_le_bytes());
                            vec.push(0x01);
                            not_found = false;
                        } else if upper_scope > 0{
                            // 해당 상위 스코프가 목표 하위 스코프와 관련 있음
                            // 하지만 해당 상위 스코프에는 식별자가 없음으로 상위 상위 스코프로 이동
                            under_index = upper_scope;
                            upper_scope -= 1;
                        } else {
                            break;
                        }
                    } else {
                        // 해당 상위 스코프에서 목표 하위 스코프가 없는 경우
                        // 해당 상위 스코프는 목표 하위 스코프와 관련 없음
                        upper_scope -= 1;
                    }
                }
                if not_found {
                    panic!("{}", format!("컴파일 불가 : 해당하는 식별자({0})를 찾을 수 없음", name));
                }
            }
            Expression::Insert{variable, expression} => {
                // 0x54 exp exp
                vec.push(0x55);
                variable.compile(scope_index, scope, vec);
                expression.compile(scope_index, scope, vec);
            }
            Expression::If{condition, consequence, alternative} => {
                vec.push(0x56);
                condition.compile(scope_index, scope, vec);

                let scope_new:usize = scope.len();
                scope.push(Scope::new());
                vec.extend(consequence.len().to_le_bytes());
                for stmt in consequence {
                    stmt.compile(scope_new, scope, vec);
                }

                if let Some(alternative) = alternative {
                    let scope_new:usize = scope.len();
                    scope.push(Scope::new());
                    vec.extend(alternative.len().to_le_bytes());
                    for stmt in alternative {
                        stmt.compile(scope_new, scope, vec);
                    }
                }
                vec.push(0x57);
            }
            Expression::Fn {identifier, parameters, body, return_type} => {
                // 0x58 scope index return params_length params[0x52 type, 0x52 type, ...] body_length body
                vec.push(0x58);
                let scope_new:usize = scope.len();
                scope.push(Scope::new());
                let index = scope[scope_index].insert_function(scope_new, identifier.clone());
                vec.extend(scope_index.to_le_bytes());
                vec.extend(index.to_le_bytes());
                return_type.compile(scope_index, scope, vec);
                let scope_index = scope_new;

                vec.extend(parameters.len().to_le_bytes());
                for stmt in parameters {
                    stmt.compile(scope_index, scope, vec);
                }

                vec.extend(body.len().to_le_bytes());
                for stmt in body {
                    stmt.compile(scope_index, scope, vec);
                }
            }
            Expression::Call {function, arguments} => {
                // 0x59 [0x53 scope index] 0|1 args_length args[0x53 index, 0x53 index, ...]
                vec.push(0x59);
                function.compile(scope_index, scope, vec);

                vec.extend(arguments.len().to_le_bytes());
                for stmt in arguments {
                    stmt.compile(scope_index, scope, vec);
                }
            }
            Expression::Literal(value) => {
                value.compile(scope_index, scope, vec);
            }
            Expression::Prefix(prefix, expression) => {
                prefix.compile(scope_index, scope, vec);
                expression.compile(scope_index, scope, vec);
            }
            Expression::Infix(infix, a, b) => {
                infix.compile(scope_index, scope, vec);
                a.compile(scope_index, scope, vec);
                b.compile(scope_index, scope, vec);
            }
        };
    }
}