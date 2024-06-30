pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    None,                   // 0x01
    I64(i64),               // 0x02
    F64(f64),               // 0x03
    //u64                   // 0x04
    //i32                   // 0x05
    //f32                   // 0x06
    //u32                   // 0x07
    //i16                   // 0x08
    //u16                   // 0x09
    //i8                    // 0x0A
    //u8                    // 0x0B
    Bool(bool),             // 0x0C
    //Vec                   // 0x0D
    //Map                   // 0x0E
    String(String),         // 0x0F
}

impl Literal{
    pub fn get_type(&self) -> Type{
        match self {
            Literal::F64(_) => Type::F64,
            Literal::I64(_) => Type::I64,
            Literal::String(_) => Type::String,
            Literal::Bool(_) => Type::Bool,
            Literal::None => Type::None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    None        =       0x71,
    I64         =       0x72,
    F64         =       0x73,
    //u64       =       0x74,
    //i32       =       0x75,
    //f32       =       0x76,
    //u32       =       0x77,
    //i16       =       0x78,
    //u16       =       0x79,
    //i8        =       0x7A,
    //u8        =       0x7B,
    Bool        =       0x7C,
    //Vec       =       0x7D,
    //Map       =       0x7E,
    String      =       0x7F,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Type::F64 => write!(f, "f64"),
            Type::I64 => write!(f, "i64"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::None => write!(f, "var"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Plus        = 0x20,     // +
    Minus       = 0x21,     // -
    Multiply    = 0x22,     // *
    Divide      = 0x23,     // /
    Mod         = 0x24,     // %
    Equal       = 0x25,     // ==
    NotEqual    = 0x26,     // !=
    LessThan    = 0x27,     // <
    GreaterThan = 0x28,     // >
    And         = 0x29,     // &&
    Or          = 0x2A,     // ||
    BitAnd      = 0x2B,     // &
    BitOr       = 0x2C,     // |
}

impl std::fmt::Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Multiply => write!(f, "*"),
            Infix::Divide => write!(f, "/"),
            Infix::Mod => write!(f, "%"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::LessThan => write!(f, "<"),
            Infix::GreaterThan => write!(f, ">"),
            Infix::And => write!(f, "&&"),
            Infix::Or => write!(f, "||"),
            Infix::BitAnd => write!(f, "&"),
            Infix::BitOr => write!(f, "|"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus       = 0x40,
    Not         = 0x41,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let{                                // 0x50 from to[0x52 addr type]
        variable: Expression,
        expression: Option<Expression>
    },
    Return(Expression),                 // 0x51 exp
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Variable(String, Type),             // 0x52 type
    Identifier(String),                 // 변수 로드 = 0x53 addr
    Insert {                            // 0x54 from to
        variable:  Box<Expression>,
        expression: Box<Expression>
    },
    If {                                // 0x55 cond cons_length alter_length cons (alter)
        condition: Box<Expression>,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    },
    Fn {                                // 0x56 return params_length body_size params[0x52 addr type, 0x52 addr type, ...] body
        identifier: String,
        return_type: Type,
        parameters: Vec<Expression>,
        body: Vec<Statement>,
    },
    Call {                              // 0x57 addr args_length args[0x53 index, 0x53 index, ...]
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },

    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(
        Infix,
        Box<Expression>,
        Box<Expression>
    ),
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,         // =
    Bool,           // && or ||
    Equals,         // == or !=
    LessGreater,    // > or <
    Sum,            // +
    Product,        // *
    Bit,            // & or |
    Prefix,         // -X or !X
    Call,           // myFunction(X)
}