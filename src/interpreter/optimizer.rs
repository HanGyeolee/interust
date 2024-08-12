use std::collections::HashMap;
use std::fmt::Debug;
use crate::ast::{Expression, Infix, Statement, Literal, Prefix, ClassMember};
use crate::Program;

#[derive(Debug)]
pub struct Optimizer;

impl Optimizer {
    pub fn optimize(program: Program) -> Program {
        Self::dead_code_elimination(Self::constant_folding(program))
    }

    /// 상수 폴딩: 상수 표현식 압축
    fn constant_folding(program: Program) -> Program {
        program.into_iter().map(|stmt| Self::fold_statement(stmt)).collect()
    }
    fn fold_statement(stmt: Statement) -> Statement {
        match stmt {
            Statement::Expression(expr) => Statement::Expression(Self::fold_expression(expr)),
            Statement::Let { variable, expression } => Statement::Let {
                variable,
                expression: expression.map(Self::fold_expression),
            },
            Statement::Return(expr) => Statement::Return(Self::fold_expression(expr)),
            Statement::Fn { identifier, return_type, parameters, body } => Statement::Fn {
                identifier,
                return_type,
                parameters,
                body: body.into_iter().map(Self::fold_statement).collect(),
            },
            Statement::Class { identifier, members} => Statement::Class {
                identifier,
                members: members.into_iter().map(|member| {
                    match member {
                        ClassMember::Method(access, statement) =>
                            ClassMember::Method(
                                access,
                                Self::fold_statement(statement)
                            ),
                        ClassMember::Variable(access, expression) =>
                            ClassMember::Variable(
                                access,
                                Self::fold_expression(expression)
                            ),
                    }
                }).collect()
            },
        }
    }
    fn fold_expression(expr: Expression) -> Expression {
        match expr {
            Expression::Infix(op, left, right) => {
                let left = Box::new(Self::fold_expression(*left));
                let right = Box::new(Self::fold_expression(*right));
                if let (Expression::Literal(l), Expression::Literal(r)) = (&*left, &*right) {
                    if let Some(result) = Self::evaluate_constant_expression(&op, l, r) {
                        return Expression::Literal(result);
                    }
                }
                Expression::Infix(op, left, right)
            },
            Expression::Prefix(op, expr) => {
                let expr = Box::new(Self::fold_expression(*expr));
                if let Expression::Literal(lit) = &*expr {
                    if let Some(result) = Self::evaluate_constant_prefix(&op, lit) {
                        return Expression::Literal(result);
                    }
                }
                Expression::Prefix(op, expr)
            },
            Expression::If { condition, consequence, alternative } => Expression::If {
                condition: Box::new(Self::fold_expression(*condition)),
                consequence: consequence.into_iter().map(Self::fold_statement).collect(),
                alternative: alternative.map(|alt| alt.into_iter().map(Self::fold_statement).collect()),
            },
            _ => expr,
        }
    }
    fn evaluate_constant_expression(op: &Infix, left: &Literal, right: &Literal) -> Option<Literal> {
        match (left, right, op) {
            (Literal::I64(l), Literal::I64(r), Infix::Plus) => Some(Literal::I64(l + r)),
            (Literal::I64(l), Literal::I64(r), Infix::Minus) => Some(Literal::I64(l - r)),
            (Literal::I64(l), Literal::I64(r), Infix::Multiply) => Some(Literal::I64(l * r)),
            (Literal::I64(l), Literal::I64(r), Infix::Divide) if *r != 0 => Some(Literal::I64(l / r)),
            (Literal::I64(l), Literal::I64(r), Infix::BitAnd) => Some(Literal::I64(l & r)),
            (Literal::I64(l), Literal::I64(r), Infix::BitOr) => Some(Literal::I64(l | r)),
            (Literal::F64(l), Literal::F64(r), Infix::Plus) => Some(Literal::F64(l + r)),
            (Literal::F64(l), Literal::F64(r), Infix::Minus) => Some(Literal::F64(l - r)),
            (Literal::F64(l), Literal::F64(r), Infix::Multiply) => Some(Literal::F64(l * r)),
            (Literal::F64(l), Literal::F64(r), Infix::Divide) if *r != 0.0 => Some(Literal::F64(l / r)),
            (Literal::Bool(l), Literal::Bool(r), Infix::And) => Some(Literal::Bool(*l && *r)),
            (Literal::Bool(l), Literal::Bool(r), Infix::Or) => Some(Literal::Bool(*l || *r)),
            _ => None,
        }
    }
    fn evaluate_constant_prefix(op: &Prefix, expr: &Literal) -> Option<Literal> {
        match (op, expr) {
            (Prefix::Minus, Literal::I64(v)) => Some(Literal::I64(-v)),
            (Prefix::Minus, Literal::F64(v)) => Some(Literal::F64(-v)),
            (Prefix::Not, Literal::Bool(v)) => Some(Literal::Bool(!v)),
            _ => None,
        }
    }

    /// 죽은 코드 제거: 실행되지 않는 코드 제거
    fn dead_code_elimination(program: Program) -> Program {
        program.into_iter().filter_map(Self::eliminate_dead_code).collect()
    }
    fn eliminate_dead_code(stmt: Statement) -> Option<Statement> {
        match stmt {
            Statement::Fn { identifier, return_type, parameters, body } => Some(Statement::Fn {
                identifier,
                return_type,
                parameters,
                body: body.into_iter().filter_map(Self::eliminate_dead_code).collect(),
            }),
            Statement::Class { identifier, members } => Some(Statement::Class {
                identifier,
                members: members.into_iter().map(|member| {
                    match member {
                        ClassMember::Method(access, statement) =>
                            ClassMember::Method(
                                access,
                                Self::eliminate_dead_code(statement).unwrap()
                            ),
                        ClassMember::Variable(access, expression) =>
                            ClassMember::Variable(
                                access,
                                expression
                            ),
                    }
                }).collect()
            }),
            Statement::Expression(Expression::If {condition, consequence, alternative}) => Self::optimize_if(*condition, consequence, alternative),
            Statement::Expression(Expression::While {condition, body}) => Self::optimize_while(*condition, body),
            _ => Some(stmt)
        }
    }
    fn optimize_if(condition: Expression, consequence: Vec<Statement>, alternative: Option<Vec<Statement>>) -> Option<Statement> {
        match condition {
            Expression::Literal(Literal::Bool(true)) => {
                Some(Statement::Expression(Expression::If {
                    condition: Box::new(Expression::Literal(Literal::Bool(true))),
                    consequence: consequence.into_iter().filter_map(Self::eliminate_dead_code).collect(),
                    alternative: None,
                }))
            },
            Expression::Literal(Literal::Bool(false)) => {
                if let Some(alt) = alternative {
                    Some(Statement::Expression(Expression::If {
                        condition: Box::new(Expression::Literal(Literal::Bool(false))),
                        consequence: alt.into_iter().filter_map(Self::eliminate_dead_code).collect(),
                        alternative: None,
                    }))
                } else {
                    None
                }
            },
            _ => Some(Statement::Expression(Expression::If {
                condition: Box::new(condition),
                consequence: consequence.into_iter().filter_map(Self::eliminate_dead_code).collect(),
                alternative: alternative.map(|alt| alt.into_iter().filter_map(Self::eliminate_dead_code).collect()),
            })),
        }
    }
    fn optimize_while(condition: Expression, body: Vec<Statement>) -> Option<Statement> {
        match condition {
            Expression::Literal(Literal::Bool(false)) => {
                // 조건이 항상 거짓이면 while 루프를 제거
                None
            },
            Expression::Literal(Literal::Bool(true)) => {
                // 조건이 항상 참이면 무한 루프 경고를 출력하고 최적화된 루프를 반환
                println!("Warning: Infinite loop detected");
                Some(Statement::Expression(Expression::While {
                    condition: Box::from(Expression::Literal(Literal::Bool(true))),
                    body: body.into_iter().filter_map(Self::eliminate_dead_code).collect(),
                }))
            },
            _ => Some(Statement::Expression(Expression::While {
                condition: Box::from(condition),
                body: body.into_iter().filter_map(Self::eliminate_dead_code).collect(),
            })),
        }
    }

}


#[cfg(test)]
mod test {
    use crate::interpreter::optimizer::Optimizer;
    use crate::parser::parser::Parser;
    use crate::tokenizer::tokenizer::Tokenizer;

    #[test]
    fn test_optimize() {
        let input = r#"
        let five:i64 = 5 + 7;
        let ten:f64 = 10;

        fn add(x:i64, y) {
            return 5 + 7 + x + y;
        };

        if (false) {
            result = 5 + 7;
        }

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
        let optimized_program = Optimizer::optimize(program);
        println!("{:?}", optimized_program);
    }
}