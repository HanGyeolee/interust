use crate::Token;

struct InterustLog {
}

impl InterustLog {
    pub fn e(tag: &str, content: String){
        eprintln!("{0} : {1}", tag, content);
    }
    /*pub fn i(tag: &str, content: String){
        println!("{0} : {1}", tag, content);
    }*/
}

pub struct Tokenizer<'a> {
    input: &'a str,
    line: usize,
    position: usize,
    current_char: Option<char>,
}

impl<'a> Tokenizer<'a>{
    /// 새로운 토크나이저를 만든다.
    ///
    pub fn new(input: &'a str) -> Self {
        let mut tokenizer = Tokenizer {
            input,
            line: 0,
            position: 0,
            current_char: None
        };
        tokenizer.advance();
        tokenizer
    }

    /**
    현재 커서의 위치가 입력 문자열의 길이보다 작다면 문자 하나를 가져온다.<br/>
    문자열의 길이보다 크다면 None
     */
    fn advance(&mut self) {
        self.current_char = self.future();
        self.position += 1;
    }

    fn future(&mut self) -> Option<char> {
        if self.position < self.input.len() {
            Some(self.input.chars().nth(self.position).unwrap())
        } else {
            None
        }
    }

    /**
    토크나이징 실행
     */
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(current_char) = self.current_char {
            match current_char {
                '\n' => { self.advance(); self.line += 1; }
                ' ' | '\t' | '\r' => self.advance(),
                '(' => { tokens.push(Token::OpenParen); self.advance(); }
                ')' => { tokens.push(Token::CloseParen); self.advance(); }
                '{' => { tokens.push(Token::OpenBrace); self.advance(); }
                '}' => { tokens.push(Token::CloseBrace); self.advance(); }
                '[' => { tokens.push(Token::OpenBracket); self.advance(); }
                ']' => { tokens.push(Token::CloseBracket); self.advance(); }
                ',' => { tokens.push(Token::Comma); self.advance(); }
                ';' => { tokens.push(Token::Semicolon); self.advance(); }
                ':' => {
                    match self.future().unwrap() {
                        ':' => {
                            self.advance();
                            tokens.push(Token::CallMethod); self.advance();
                        },
                        _ => {
                            tokens.push(Token::Colon); self.advance();
                        }
                    };
                }
                '0'..='9' => tokens.push(self.read_number()),
                '"' => tokens.push(self.read_string()),
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(self.read_identifier()),
                '/' => {
                    match self.future().unwrap() {
                        '*' => {
                            self.advance();
                            self.read_comment();
                        },
                        _ => {
                            tokens.push(Token::Operator(String::from("/")));
                        }
                    };
                    self.advance();
                }
                '!' => {
                    match self.future().unwrap() {
                        '=' => {
                            self.advance();
                            tokens.push(Token::Operator(String::from("!=")));
                        },
                        _ => {
                            tokens.push(Token::Operator(String::from("!")));
                        }
                    };
                    self.advance();
                }
                '-' => {
                    match self.future().unwrap() {
                        '>' => {
                            self.advance();
                            tokens.push(Token::ReturnType);
                            self.advance();
                        },
                        _ => {
                            if self.is_operator(current_char) {
                                tokens.push(self.read_operator());
                            } else {
                                panic!("Unexpected character: {}", current_char);
                            }
                        }
                    };
                }
                _ => {
                    if self.is_operator(current_char) {
                        tokens.push(self.read_operator());
                    } else {
                        panic!("Unexpected character: {}", current_char);
                    }
                }
            }
        }

        tokens.push(Token::EOF);
        tokens
    }

    fn read_comment(&mut self) -> Token {
        self.advance();  // 시작 따옴표 건너뜀

        while let Some(current_char) = self.current_char {
            if current_char == '*' && self.future().unwrap() == '/' {
                // */ 발견 시 문자열 토크나이징 종료
                self.advance();  // * 건너뜀
                self.advance();  // / 건너뜀
                break;
            } else {
                self.advance();
            }
        }

        Token::Comment
    }

    /**
     숫자 토크나이징<br/>
    실수와 정수를 나눈다.
     */
    fn read_number(&mut self) -> Token {
        let mut number = String::new();

        while let Some(current_char) = self.current_char {
            if current_char.is_numeric() || current_char == '.' {
                number.push(current_char);
                self.advance();
            } else {
                break;
            }
        }
        if number.chars().filter(|c| *c == '.').count() > 1 {
            InterustLog::e("숫자 오류", format!("잘못된 숫자가 존재합니다:\"{1}\" line:{0}", self.line, number));
        }

        if number.find('.').is_some() {
            Token::F64(number.parse().unwrap())
        } else {
            Token::I64(number.parse().unwrap())
        }
    }

    /**
    문자열 토크나이징
     */
    fn read_string(&mut self) -> Token {
        let mut string = String::new();
        self.advance();  // 시작 따옴표 건너뜀

        while let Some(current_char) = self.current_char {
            if current_char == '"' {
                // 종료 따옴표 발견 시 문자열 토크나이징 종료
                self.advance();  // 종료 따옴표 건너뜀
                break;
            } else {
                string.push(current_char);
                self.advance();
            }
        }

        Token::String(string)
    }

    /**
     식별자 토크나이징<br/>
    식별자 뿐 아니라 if, for, fn, class 와 같은 예약어도 토크나이징 한다.
     */
    fn read_identifier(&mut self) -> Token {
        let mut identifier = String::new();

        while let Some(current_char) = self.current_char {
            if !self.is_operator(current_char) && (current_char.is_alphanumeric() || current_char == '_') {
                identifier.push(current_char);
                self.advance();
            } else {
                break;
            }
        }

        match identifier.as_str() {
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "for" => Token::For,
            "while" => Token::While,
            "fn" => Token::Fn,
            "class" => Token::Class,
            "pub" => Token::Public,
            "self" => Token::SelfKeyword,

            "true" => Token::Bool(true),
            "True" => Token::Bool(true),
            "TRUE" => Token::Bool(true),
            "false" => Token::Bool(false),
            "False" => Token::Bool(false),
            "FALSE" => Token::Bool(false),
            _ => Token::Identifier(identifier),
        }
    }

    /**
    연산자 토크나이징
     */
    fn read_operator(&mut self) -> Token {
        let mut operator = String::new();

        while let Some(current_char) = self.current_char {
            if self.is_operator(current_char) {
                operator.push(current_char);
                self.advance();
            } else {
                break;
            }
        }
        let bool_check: &[_] = &['<', '>', '&', '|'];
        let oper_check: &[_] = &['+', '-', '*', '/', '%'];
        let is_bool = operator.find(bool_check).is_some();
        let is_oper = operator.find(oper_check).is_some();
        if operator.len() > 1 {
            if is_bool && is_oper {
                InterustLog::e("연산자 오류", format!("잘못된 연산자가 존재합니다:\"{1}\" line:{0}", self.line, operator));
            }
        } else if operator.eq("=") {
            return Token::Assign;
        } else if operator.eq("&") {
            if let Some(Token::Identifier(_)) = self.tokenize().last(){ } else {
                return Token::Ampersand;
            }
        }
        Token::Operator(operator)
    }

    /**
    연산자 여부
     */
    fn is_operator(&self, char: char) -> bool {
        match char {
            '+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' |
            '&' | '|' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Token;
    use crate::tokenizer::tokenizer::Tokenizer;

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;

        fn add(x, y) {
            return x + y;
        };

        let result = add(five, ten);

        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        "#;

        let tests = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::I64(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::I64(10),
            Token::Semicolon,

            Token::Fn,
            Token::Identifier(String::from("add")),
            Token::OpenParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::Identifier(String::from("x")),
            Token::Operator(String::from("+")),
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::CloseBrace,
            Token::Semicolon,

            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::OpenParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::CloseParen,
            Token::Semicolon,

            Token::If,
            Token::OpenParen,
            Token::I64(5),
            Token::Operator(String::from("<")),
            Token::I64(10),
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::Bool(true),
            Token::Semicolon,
            Token::CloseBrace,
            Token::Else,
            Token::OpenBrace,
            Token::Return,
            Token::Bool(false),
            Token::Semicolon,
            Token::CloseBrace,

            Token::EOF,
        ];

        let mut tokenizer = Tokenizer::new(input);
        let validates = tokenizer.tokenize();

        let mut index = 0;
        while index < tests.len() {
            assert_eq!(tests[index], validates[index]);
            index = index + 1;
        }
    }
}