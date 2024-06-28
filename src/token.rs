#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let,                    // let 변수 생성
    If,                     // if
    Else,                   // else
    Return,                 // return
    ReturnType,             // return type
    For,                    // for
    While,                  // while            미구현
    Fn,                     // function
    Class,                  // class            미구현

    Identifier(String),     // 식별자
    F64(f64),               // 실수 숫자
    I64(i64),               // 정수 숫자
    Bool(bool),             // 논리
    String(String),         // 문자열

    Operator(String),       // 연산자

    OpenParen,              // (
    CloseParen,             // )
    OpenBrace,              // {
    CloseBrace,             // }
    OpenBracket,            // [
    CloseBracket,           // ]

    Assign,                 // =

    Comma,                  // ,
    Colon,                  // :
    Semicolon,              // ;

    Comment,                // /* */
    EOF,                    // 종료
}