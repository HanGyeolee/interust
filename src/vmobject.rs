use std::fmt;
use std::collections::HashMap;
use crate::ast::Type;

/// Java Stack 이나 Rust Interpreter 에 대해서 공부하는 게 좋을 것 같다.
/// CPU 명령이 왜 Stack 을 통해서 동작하는 가
/// RustC 혹은 Python Compiler
///
/// Interpreter = 고수준 언어로 CPU를 흉내내기 위함


#[derive(Debug, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    /// 식별자, (주소, 타입)
    pub stack: HashMap<String, (usize, Type)>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            stack: HashMap::new(),
        }
    }
}
