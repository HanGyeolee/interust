use crate::parser::error::{ParseError, ParseErrorKind};
use crate::{Expression, Infix, Literal, Precedence, Prefix, Program, Statement, Token, Type};
use crate::ast::ClassMember;

pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            position: 0,
            errors: Vec::new(),
        }
    }

    // past_token
    fn past_token(&self) -> &Token {
        &self.tokens[self.position - 1]
    }

    // cur_token
    fn current_token(&self) -> &Token {
        &self.tokens[self.position]
    }

    // peek_token
    fn future_token(&self) -> &Token {
        &self.tokens[self.position+1]
    }

    // next_token
    fn consume_token(&mut self) -> Token {
        let token = self.current_token().clone();
        self.position += 1;
        token
    }

    fn token_to_precedence(token: &Token) -> Precedence {
        match token {
            Token::Assign => Precedence::Assign,
            Token::Operator(operator) => {
                match operator.as_str() {
                    "&&" | "||" => Precedence::Bool,
                    "==" | "!=" => Precedence::Equals,
                    "<" | ">" => Precedence::LessGreater,
                    "+" | "-" => Precedence::Sum,
                    "*" | "/" | "%" => Precedence::Product,
                    "&" | "|" => Precedence::Bit,
                    _ => Precedence::Lowest
                }
            }
            Token::OpenParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    /*pub fn get_errors(&mut self) -> Vec<ParseError> {
        self.errors.clone()
    }*/

    fn past_token_is(&mut self, token: &Token) -> bool {
        token == self.past_token()
    }

    fn cur_token_is(&mut self, token: &Token) -> bool {
        token == self.current_token()
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        token == self.future_token()
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.consume_token();
            true
        } else {
            self.error_consume_token(token);
            false
        }
    }

    fn cur_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(self.current_token())
    }

    fn peek_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(self.future_token())
    }

    fn error_consume_token(&mut self, token: &Token) {
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken,
            format!(
                "expected next tokenizer to be {:?}, got {:?} instead",
                token, self.future_token()
            ),
        ));
    }
    fn error_variable_type(&mut self, typ1: &Type, typ2: &Type) {
        self.errors.push(ParseError::new(
            ParseErrorKind::IncorrectType,
            format!(
                "expected type to be {:?}, got {:?} instead",
                typ1, typ2
            ),
        ));
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Vec::new();

        while self.current_token() != &Token::EOF {
            match self.parse_statement() {
                Some(statement) => program.push(statement),
                None => {}
            }

            self.consume_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token() {
            Token::Let => self.parse_let_statement(),
            Token::Fn => self.parse_function_statement(),
            Token::Class => self.parse_class_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_variable(&mut self) -> Option<Expression> {
        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };

        let mut typ:Type = Type::None;
        if self.peek_token_is(&Token::Colon) {
            self.consume_token();
            typ = self.parse_type().unwrap_or_else(|| Type::None)
        }

        Some(Expression::Variable(
            identifier,
            typ
        ))
    }

    fn parse_type(&mut self) -> Option<Type>{
        let mut reffed = false;
        self.consume_token(); // :
        if self.peek_token_is(&Token::Ampersand){
            reffed = true;
        }
        match self.parse_identifier() {
            Some(identifier) =>
                Some(match identifier.as_str() {
                    "i64" => {
                        if reffed {
                            Type::Ref(Box::from(Type::I64))
                        } else {
                            Type::I64
                        }
                    },
                    "f64" => {
                        if reffed {
                            Type::Ref(Box::from(Type::F64))
                        } else {
                            Type::F64
                        }
                    },
                    "String" | "str" => {
                        if reffed {
                            Type::Ref(Box::from(Type::String))
                        } else {
                            Type::String
                        }
                    },
                    "bool" | "Bool" | "boolean" => {
                        if reffed {
                            Type::Ref(Box::from(Type::Bool))
                        } else {
                            Type::Bool
                        }
                    },
                    name => Type::Class(String::from(name))
                }),
            None => return None,
        }
    }

    fn parse_class_statement(&mut self) -> Option<Statement> {
        self.consume_token(); // 'class' 토큰 소비
        let identifier = match self.parse_identifier() {
            Some(name) => name,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenBrace) {
            return None;
        }
        self.consume_token();

        let members = self.parse_class_members(identifier.clone());

        if !self.expect_peek(&Token::CloseBrace) {
            return None;
        }

        Some(Statement::Class{
            identifier, members
        })
    }

    fn parse_class_members(&mut self, identifier:String) -> Vec<ClassMember> {
        let mut members = Vec::new();

        while !self.cur_token_is(&Token::CloseBrace) {
            if let Some(member) = self.parse_class_member(identifier.clone()) {
                members.push(member);
            } else {
                self.consume_token();
            }
        }

        members
    }

    fn parse_class_member(&mut self, class_name:String) -> Option<ClassMember> {
        let is_public = self.cur_token_is(&Token::Public);
        if is_public {
            self.consume_token();
        }

        match self.current_token() {
            Token::Let => {
                self.consume_token();
                let variable = match self.parse_variable() {
                    Some(variable) => variable,
                    None => return None,
                };

                self.consume_token();
                if self.cur_token_is(&Token::Semicolon) {
                    return Some(ClassMember::Variable(is_public, variable))
                } else {
                    return None;
                }
            }
            Token::Fn => {
                self.consume_token();
                let (is_static, func) = match self.parse_class_method_statement(class_name) {
                    Some((is_static, param)) => (is_static, param),
                    None => return None,
                };
                if self.cur_token_is(&Token::CloseBrace) {
                    return Some(ClassMember::Method(is_public, is_static, func))
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn parse_class_method_statement(&mut self, class_name:String) -> Option<(bool, Statement)> {
        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenParen) {
            return None;
        }

        let (is_static, parameters) = match self.parse_class_method_parameters(class_name) {
            Some(parameters) => parameters,
            None => return None,
        };

        let return_type = match self.parse_function_return_type() {
            Some(identifier) => identifier,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenBrace) {
            return None;
        }

        Some((is_static, Statement::Fn {
            identifier,
            parameters,
            body: self.parse_block_statement(),
            return_type
        }))
    }
    fn parse_class_method_parameters(&mut self, class_name:String) -> Option<(bool, Vec<Expression>)> {
        let mut is_static = true;
        let mut parameters = Vec::new();

        if self.peek_token_is(&Token::CloseParen) {
            self.consume_token();
            return Some((is_static, parameters));
        }
        self.consume_token();
        if self.expect_peek(&Token::Ampersand) {
            self.consume_token();
            if self.expect_peek(&Token::SelfKeyword) {
                is_static = false;
                parameters.push(Expression::Variable(String::from("self"),
                                         Type::Ref(Box::from(Type::Class(class_name)))));
                self.consume_token();
            } else {
                return None;
            }
        } else {
            match self.parse_variable() {
                Some(variable) => parameters.push(variable),
                None => return None,
            }
        }

        while self.peek_token_is(&Token::Comma) {
            self.consume_token();
            self.consume_token();

            match self.parse_variable() {
                Some(variable) => parameters.push(variable),
                None => return None,
            }
        }

        if !self.expect_peek(&Token::CloseParen) {
            return None;
        }

        Some((is_static, parameters))
    }

    fn parse_class_variable_expression(&mut self, class:Expression) -> Option<Expression> {
        let class = match class {
            Expression::Identifier(_) => Box::from(class),
            _ => return None,
        };
        let inits = match self.parse_class_expression_list(&Token::CloseBrace) {
            Some(arguments) => arguments,
            None => return None,
        };
        Some(Expression::ClassVariable{
            class,
            inits
        })
    }

    fn parse_call_method_expression(&mut self, class: Expression) -> Option<Expression> {
        let class = match class {
            Expression::Identifier(_) => Box::from(class),
            _ => return None,
        };
        if let Some(call) = self.parse_expression(Precedence::Lowest){
            return match call {
                Expression::Call{..} => {
                    return Some(Expression::CallMethod {
                        class,
                        call: Box::from(call)
                    });
                },
                _ => None
            };
        }
        None
    }

    fn parse_class_expression_list(&mut self, end: &Token) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(&end) {
            self.consume_token();
            return Some(list);
        }

        self.consume_token();

        match self.parse_class_create_insert() {
            Some(expression) => list.push(expression),
            None => return None,
        };

        while self.peek_token_is(&Token::Comma) {
            self.consume_token();
            self.consume_token();

            match self.parse_class_create_insert() {
                Some(expression) => list.push(expression),
                None => return None,
            };
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_class_create_insert(&mut self) -> Option<Expression> {
        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };
        if !self.expect_peek(&Token::Colon) {
            return None;
        }

        self.consume_token(); // colon
        let value = match self.parse_expression(Precedence::Lowest) {
            Some(parameters) => parameters,
            None => return None,
        };

        Some(Expression::Insert {
            variable: Box::from(Expression::Identifier(identifier)),
            expression: Box::from(value)
        })
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match self.future_token() {
            Token::Identifier(_) => self.consume_token(),
            _ => return None,
        };

        let variable = match self.parse_variable() {
            Some(variable) => variable,
            None => return None,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.consume_token();
            return Some(Statement::Let{
                variable,
                expression: None
            });
        }

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        self.consume_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };

        if self.cur_token_is(&Token::Semicolon) {
            self.consume_token();
        }

        match expression {
            Expression::Literal(lit) => {
                if let Expression::Variable(name, mut typ) = variable {
                    if Type::None == typ {
                        typ = lit.get_type();
                    } else if lit.get_type() != typ {
                        // 타입 다름 오류
                        self.error_variable_type(&typ, &lit.get_type());
                    }
                    return Some(Statement::Let{
                        variable: Expression::Variable(name, typ),
                        expression: Some(Expression::Literal(lit))
                    });
                }
                None
            },
            expression => Some(Statement::Let{
                variable,
                expression: Some(expression)
            })
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.consume_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };

        if self.cur_token_is(&Token::Semicolon) {
            self.consume_token();
        }

        Some(Statement::Return(expression))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expression) => {
                if self.peek_token_is(&Token::Semicolon) {
                    self.consume_token();
                }
                Some(Statement::Expression(expression))
            }
            None => None,
        }
    }

    fn parse_block_statement(&mut self) -> Vec<Statement> {
        self.consume_token();

        let mut block = Vec::new();

        let mut return_index:i128 = -1;
        while !self.cur_token_is(&Token::CloseBrace) && !self.cur_token_is(&Token::EOF) {
            match self.parse_statement() {
                Some(statement) => {
                    match statement {
                        Statement::Return(_) => return_index = block.len() as i128,
                        _ => {},
                    }
                    block.push(statement);
                },
                None => {}
            }
            self.consume_token();
        }

        if -1 < return_index && return_index < block.len() as i128 {
            block.truncate((return_index + 1) as usize);
        }

        block
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // Prefix
        let mut left = match self.current_token() {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::I64(_) => self.parse_i64_expression(),
            Token::F64(_) => self.parse_f64_expression(),
            Token::Bool(_) => self.parse_bool_expression(),
            Token::String(_) => self.parse_string_expression(),
            Token::Operator(operator) => {
                match operator.as_str() {
                    "!" | "-" => self.parse_prefix_expression(),
                    _ => None
                }
            }
            Token::OpenParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            _ => None,
        };

        if let Some(Expression::Identifier(_)) = left.clone() {
            if self.past_token_is(&Token::Return) {
                return match self.future_token() {
                    Token::OpenBrace => {
                        self.consume_token();
                        self.parse_class_variable_expression(left.unwrap())
                    },
                    _ => None
                };
            }
        }
        // Infix
        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_token_precedence() {
            match self.future_token() {
                Token::Assign => {
                    self.consume_token();
                    left = self.parse_insert_expression(left.unwrap());
                },
                Token::Operator(operator) => {
                    match operator.as_str() {
                        "+" | "-" | "*" | "/" | "%" |
                        "==" | "!=" | "<" | ">" |
                        "&&" | "||" | "&" | "|" => {
                            self.consume_token();
                            left = self.parse_infix_expression(left.unwrap());
                        },
                        _ => return left,
                    }
                },
                Token::OpenParen => {
                    self.consume_token();
                    left = self.parse_call_expression(left.unwrap());
                },
                Token::CallMethod => {
                    self.consume_token();
                    left = self.parse_call_method_expression(left.unwrap());
                }
                Token::Dot => {
                    self.consume_token();
                    left = self.parse_call_method_expression(left.unwrap());
                }
                _ => return left,
            }
        }

        left
    }

    fn parse_identifier(&mut self) -> Option<String> {
        match self.current_token() {
            Token::Identifier(ident) => Some(ident.clone()),
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match self.parse_identifier() {
            Some(ident) => Some(Expression::Identifier(ident)),
            None => None,
        }
    }

    fn parse_i64_expression(&mut self) -> Option<Expression> {
        match self.current_token() {
            Token::I64(int) => Some(Expression::Literal(Literal::I64(int.clone()))),
            _ => None,
        }
    }

    fn parse_f64_expression(&mut self) -> Option<Expression> {
        match self.current_token() {
            Token::F64(int) => Some(Expression::Literal(Literal::F64(int.clone()))),
            _ => None,
        }
    }

    fn parse_bool_expression(&mut self) -> Option<Expression> {
        match self.current_token() {
            Token::Bool(value) => Some(Expression::Literal(Literal::Bool(true == *value))),
            _ => None,
        }
    }

    fn parse_string_expression(&mut self) -> Option<Expression> {
        match self.current_token() {
            Token::String(s) => Some(Expression::Literal(Literal::String(s.clone()))),
            _ => None,
        }
    }

    fn parse_expression_list(&mut self, end: &Token) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(&end) {
            self.consume_token();
            return Some(list);
        }

        self.consume_token();

        match self.parse_expression(Precedence::Lowest) {
            Some(expression) => list.push(expression),
            None => return None,
        };

        while self.peek_token_is(&Token::Comma) {
            self.consume_token();
            self.consume_token();

            match self.parse_expression(Precedence::Lowest) {
                Some(expression) => list.push(expression),
                None => return None,
            };
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = match self.current_token() {
            Token::Operator(operator) => {
                match operator.as_str() {
                    "!" => Prefix::Not,
                    "-" => Prefix::Minus,
                    _ => return None,
                }
            }
            _ => return None,
        };

        self.consume_token();

        match self.parse_expression(Precedence::Prefix) {
            Some(expr) => Some(Expression::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let infix = match self.current_token() {
            Token::Operator(operator) => {
                match operator.as_str() {
                    "+" => Infix::Plus,
                    "-" => Infix::Minus,
                    "*" => Infix::Multiply,
                    "/" => Infix::Divide,
                    "%" => Infix::Mod,
                    "==" => Infix::Equal,
                    "!=" => Infix::NotEqual,
                    "<" => Infix::LessThan,
                    ">" => Infix::GreaterThan,
                    "&&" => Infix::And,
                    "||" => Infix::Or,
                    "&" => Infix::BitAnd,
                    "|" => Infix::BitOr,
                    _ => return None,
                }
            }
            _ => return None,
        };

        let precedence = self.cur_token_precedence();

        self.consume_token();

        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Infix(infix, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    fn parse_insert_expression(&mut self, left: Expression) -> Option<Expression> {
        self.consume_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };

        if self.cur_token_is(&Token::Semicolon) {
            self.consume_token();
        }

        Some(Expression::Insert {
            variable: Box::new(left),
            expression: Box::new(expression),
        })
    }

    fn parse_call_expression(&mut self, func: Expression) -> Option<Expression> {
        let arguments = match self.parse_expression_list(&Token::CloseParen) {
            Some(arguments) => arguments,
            None => return None,
        };

        Some(Expression::Call {
            function: Box::new(func),
            arguments,
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.consume_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::CloseParen) {
            None
        } else {
            expr
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        self.consume_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative = None;

        // current }
        if self.peek_token_is(&Token::Else) {
            self.consume_token();
            // current Else

            if self.peek_token_is(&Token::If) {
                self.consume_token();
                // current If
                let alter_if = match self.parse_expression(Precedence::Lowest) {
                    Some(expr) => expr,
                    None => return None,
                };
                alternative = Some(vec![Statement::Expression(alter_if)]);
            } else if self.peek_token_is(&Token::OpenBrace) {
                alternative = Some(self.parse_block_statement());
            } else {
                return None;
            }
        }

        Some(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        match self.future_token() {
            Token::Identifier(_) => self.consume_token(),
            _ => return None,
        };

        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenParen) {
            return None;
        }

        let parameters = match self.parse_function_parameters() {
            Some(parameters) => parameters,
            None => return None,
        };

        let return_type = match self.parse_function_return_type() {
            Some(identifier) => identifier,
            None => return None,
        };

        if !self.expect_peek(&Token::OpenBrace) {
            return None;
        }

        Some(Statement::Fn {
            identifier,
            parameters,
            body: self.parse_block_statement(),
            return_type
        })
    }

    fn parse_function_return_type(&mut self) -> Option<Type> {
        if self.peek_token_is(&Token::OpenBrace) {
            return Some(Type::None);
        }
        self.consume_token();

        if !self.cur_token_is(&Token::ReturnType) {
            return None;
        }

        self.parse_type()
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut parameters = Vec::new();

        if self.peek_token_is(&Token::CloseParen) {
            self.consume_token();
            return Some(parameters);
        }
        self.consume_token();

        match self.parse_variable() {
            Some(variable) => parameters.push(variable),
            None => return None,
        }

        while self.peek_token_is(&Token::Comma) {
            self.consume_token();
            self.consume_token();

            match self.parse_variable() {
                Some(variable) => parameters.push(variable),
                None => return None,
            }
        }

        if !self.expect_peek(&Token::CloseParen) {
            return None;
        }

        Some(parameters)
    }
}


#[cfg(test)]
mod test {
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    #[test]
    fn test_parser() {
        let input = r#"
        let five:i64 = 5;
        let ten:f64 = 10;

        fn add(x:i64, y) {
            return x + y;
        };

        let result = add(five, ten);
        result = five * ten;
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();

        println!("{:?}", program);
        println!("{:?}", parser.errors);
    }

    #[test]
    fn test_function_type() {
        let input = r#"
        let five:i64 = 5;
        let ten:f64 = 10;

        fn add(x:i64, y) -> i64 {
            return x + y;
        };

        let result = add(five, ten);
        result = five * ten;
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();

        println!("{:?}", tokens);
        println!("{:?}", program);
        println!("{:?}", parser.errors);
    }

    #[test]
    fn test_class_define() {
        let input = r#"
        class Test {
            let private:i64;
            pub let public:f64;
            pub fn new() -> Test {
                return Test {
                    private: 0,
                    public: 0
                };
            }
        }
        "#;

        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse();
        println!("{:?}", program);
        println!("{:?}", parser.errors);
    }
}