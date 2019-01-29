use lexer::Token;

pub enum UnaryOpType {
    Complement,
    Negation,
    LogicalNegation,
}

pub enum BinOpType {
    Addition,
    Multiplication,
    Substraction,
    Division,
    Or,
    And,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
    Equal,
    NotEqual,
}

pub trait Expression {
    fn visit(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement {
    fn visit(&self) -> String;
    fn to_string(&self) -> String;
}

pub struct Program {
    pub func: Function,
}

pub struct Function {
    pub name: String,
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct DeclareStatement {
    pub var_name: String,
    pub expr: Option<Box<dyn Expression>>,
}

pub struct ReturnStatement {
    pub expr: Box<dyn Expression>,
}

pub struct ExprStatement {
    pub expr: Box<dyn Expression>,
}

pub struct IntExpression {
    pub val: u32,
}

pub struct AssignExpression {
    pub var_name: String,
    pub expr: Box<dyn Expression>,
}

pub struct VarExpression {
    pub var_name: String,
}

pub struct UnaryOpExpression {
    pub unary_op_type: UnaryOpType,
    pub expression: Box<dyn Expression>,
}

pub struct BinOpExpression {
    pub bin_op_type: BinOpType,
    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    let mut tokens = tokens;
    tokens.reverse();
    Ok(Program {
        func: parse_function(&mut tokens)?
    })
}

type StatementResult = Result<Box<dyn Statement>, &'static str>;
type StatementsResult = Result<Vec<Box<dyn Statement>>, &'static str>;
type ExpressionResult = Result<Box<dyn Expression>, &'static str>;

fn parse_function(tokens: &mut Vec<Token>) -> Result<Function, &'static str> {
    let f_name = match tokens.pop() {
        Some(t) => match t {
            Token::IntKeyword => match tokens.pop() {
                Some(t) => match t {
                    Token::Identifier(name) => name,
                    _ => return Err("No name is specified"),
                },
                None => {
                    return Err("Unexpected end of function definition (expected function name)");
                }
            },
            _ => return Err("Wrong function return type (only 'int' supported)"),
        },
        None => return Err("Unexpected end of function definition (expected function return type)"),
    };
    
    match tokens.pop() {
        Some(Token::OpenParenthesis) => {
            match tokens.pop() {
                Some(Token::CloseParenthesis) => {},
        _ => return Err("Syntax error")
            }
        },
        _ => return Err("Syntax error")
    }

    let stms = match tokens.pop() {
        Some(t) => match t {
            Token::OpenBrace => parse_stmts(tokens, false)?,
            _ => return Err("Function body syntax error"),
        },
        _ => Vec::new(),
    };

    Ok(Function {
        name: f_name,
        statements: stms,
    })
}

fn parse_stmts(tokens: &mut Vec<Token>, global: bool) -> StatementsResult {
    let mut statements: Vec<Box<dyn Statement>> = Vec::new();
    loop {
        match tokens.pop() {
            Some(t) => match t {
                Token::CloseBrace => break, // ok, quit
                t => {
                    tokens.push(t);
                    statements.push(parse_one_statement(tokens)?);
                }
            },
            None => {
                if !global {
                    return Err("Unexpected end of statements");
                } else {
                    break;
                }
            }
        };
    }

    Ok(statements)
}

fn parse_one_statement(tokens: &mut Vec<Token>) -> StatementResult {
    let stmt_result: StatementResult = match tokens.pop() {
        Some(t) => match t {
            Token::ReturnKeyword => Ok(Box::new(ReturnStatement {
                expr: parse_expression(tokens)?,
            })),
            Token::IntKeyword => parse_int_statement_or_function(tokens),
            t => {
                tokens.push(t);
                Ok(Box::new(ExprStatement {
                    expr: parse_expression(tokens)?
                }))
            }
        },
        None => return Err("Not a statement"),
    };

        match tokens.pop() {
            Some(t) => match t {
                Token::Semicolon => stmt_result,
                _ => Err("(;) expected"),
            },
            None => stmt_result,
        }
}

fn parse_int_statement_or_function(tokens: &mut Vec<Token>) -> StatementResult {
    let id: String = match tokens.pop() {
        Some(Token::Identifier(name)) => name,
        _ => return Err("Expected identifier, got something else")
    };

    let expr: Option<Box<dyn Expression>> = match tokens.pop() {
        Some(Token::Assignment) => Some(parse_expression(tokens)?),
        Some(_) => return Err("Syntax error"),
        None => None
    };

    Ok(Box::new(DeclareStatement {
        var_name: id,
        expr: expr
    }))
}

fn parse_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    match tokens.pop() {
        Some(Token::Identifier(name)) => {
            match tokens.pop() {
                Some(Token::Assignment) => {
                    Ok(Box::new(AssignExpression {
                        var_name: name,
                        expr: parse_expression(tokens)?
                    }))
                },
                Some(t) => {
                    tokens.push(t);
                    Ok(Box::new(VarExpression {
                        var_name: name
                    }))
                }
                _ => Err("Unexpected end of assignment")
            }
        }
        Some(t) => {
            tokens.push(t);
            parse_logical_or_expression(tokens)
        }
        None => Err("Unexpected end of expression")
    }
}

fn parse_logical_or_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_logical_and_expression, |t: &Token| {
        t == &Token::Or
    })
}

fn parse_logical_and_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_equality_expression, |t: &Token| {
        t == &Token::And
    })
}

fn parse_equality_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_relational_expression, |t: &Token| {
        t == &Token::Equal || t == &Token::NotEqual
    })
}

fn parse_relational_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_additive_expression, |t: &Token| {
        t == &Token::LessThan
            || t == &Token::GreaterThan
            || t == &Token::LessOrEqual
            || t == &Token::GreaterOrEqual
    })
}

fn parse_additive_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_term, |t: &Token| {
        t == &Token::Addition || t == &Token::Negation
    })
}

fn parse_term(tokens: &mut Vec<Token>) -> ExpressionResult {
    parse_rule(tokens, parse_factor, |t: &Token| {
        t == &Token::Multiplication || t == &Token::Division
    })
}

fn parse_rule<F, P>(tokens: &mut Vec<Token>, parse_one: F, predicate: P) -> ExpressionResult
where
    F: Fn(&mut Vec<Token>) -> ExpressionResult,
    P: Fn(&Token) -> bool,
{
    let mut first = parse_one(tokens)?;
    loop {
        let tok = tokens.pop();
        if tok.is_some() {
            let tok = tok.unwrap();
            if predicate(&tok) {
                let bin_op = token_to_bin_op_type(&tok)?;
                let next = parse_one(tokens)?;
                first = Box::new(BinOpExpression {
                    bin_op_type: bin_op,
                    left: first,
                    right: next,
                });
            } else {
                tokens.push(tok);
                break;
            }
        } else {
            break;
        }
    }
    Ok(first)
}

fn token_to_bin_op_type(token: &Token) -> Result<BinOpType, &'static str> {
    match token {
        &Token::Addition => Ok(BinOpType::Addition),
        &Token::Multiplication => Ok(BinOpType::Multiplication),
        &Token::Negation => Ok(BinOpType::Substraction),
        &Token::Division => Ok(BinOpType::Division),
        &Token::Or => Ok(BinOpType::Or),
        &Token::And => Ok(BinOpType::And),
        &Token::LessThan => Ok(BinOpType::Less),
        &Token::LessOrEqual => Ok(BinOpType::LessOrEq),
        &Token::GreaterThan => Ok(BinOpType::Greater),
        &Token::GreaterOrEqual => Ok(BinOpType::GreaterOrEq),
        &Token::Equal => Ok(BinOpType::Equal),
        &Token::NotEqual => Ok(BinOpType::NotEqual),
        _ => Err("Unknown binary operation"),
    }
}

fn token_to_unary_op_type(token: &Token) -> Result<UnaryOpType, &'static str> {
    match token {
        &Token::BitwiseComplement => Ok(UnaryOpType::Complement),
        &Token::Negation => Ok(UnaryOpType::Negation),
        &Token::LogicalNegation => Ok(UnaryOpType::LogicalNegation),
        _ => Err("Unknown unary operator"),
    }
}

fn parse_factor(tokens: &mut Vec<Token>) -> ExpressionResult {
    match tokens.pop() {
        Some(tok) => match tok {
            Token::ConstInt(n) => Ok(Box::new(IntExpression { val: n })),
            Token::Negation | Token::BitwiseComplement | Token::LogicalNegation => {
                let expr = parse_factor(tokens)?;
                Ok(Box::new(UnaryOpExpression {
                    unary_op_type: token_to_unary_op_type(&tok)?,
                    expression: expr,
                }))
            }
            Token::OpenParenthesis => {
                let expr = parse_expression(tokens)?;
                match tokens.pop() {
                    Some(tok) => {
                        if tok == Token::CloseParenthesis {
                            Ok(expr)
                        } else {
                            Err("Missing parenthesis (unexpected token)")
                        }
                    }
                    None => Err("Missing parenthesis (unexpected end of stream)"),
                }
            }
            _ => Err("Expression syntax error"),
        },
        None => Err("Unexpected end of expression"),
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn parse_unary_paren() {
        let tokens_no_paren: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            Negation,
            BitwiseComplement,
            ConstInt(3),
            Semicolon,
            CloseBrace,
        ];
        let tokens_paren: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            Negation,
            OpenParenthesis,
            BitwiseComplement,
            OpenParenthesis,
            ConstInt(3),
            CloseParenthesis,
            CloseParenthesis,
            Semicolon,
            CloseBrace,
        ];
        let res_no_paren = parse(tokens_no_paren).unwrap();
        let res_paren = parse(tokens_paren).unwrap();
        assert_eq!(res_no_paren.to_string(), res_paren.to_string());
    }

    #[test]
    fn parse_unary_and_binary_ops_precedence_parens() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            BitwiseComplement,
            OpenParenthesis,
            ConstInt(2),
            Addition,
            ConstInt(3),
            CloseParenthesis,
            Semicolon,
            CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let expected = Program {
            func: Function {
                name: String::from("main"),
                statements: vec![Box::new(ReturnStatement {
                    expr: Box::new(UnaryOpExpression {
                        unary_op_type: UnaryOpType::Complement,
                        expression: Box::new(BinOpExpression {
                            bin_op_type: BinOpType::Addition,
                            left: Box::new(IntExpression { val: 2 }),
                            right: Box::new(IntExpression { val: 3 }),
                        }),
                    }),
                })],
            },
        };
        assert_eq!(result.to_string(), expected.to_string());
    }

    #[test]
    fn parse_binary_ops_paren_same_precedence() {
        let tokens_paren: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            OpenParenthesis,
            ConstInt(2),
            Addition,
            OpenParenthesis,
            ConstInt(3),
            Multiplication,
            ConstInt(4),
            CloseParenthesis,
            CloseParenthesis,
            Semicolon,
            CloseBrace,
        ];
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(2),
            Addition,
            ConstInt(3),
            Multiplication,
            ConstInt(4),
            Semicolon,
            CloseBrace,
        ];
        let result_paren = parse(tokens_paren).unwrap();
        let result = parse(tokens).unwrap();
        assert_eq!(result_paren.to_string(), result.to_string());
    }

    #[test]
    fn parse_many_statements() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            IntKeyword,
            Identifier(String::from("a")),
            Assignment,
            ConstInt(3),
            Semicolon,
            Identifier(String::from("a")),
            Assignment,
            ConstInt(4),
            Semicolon,
            ReturnKeyword,
            Identifier(String::from("a")),
            Semicolon,
            CloseBrace
        ];
        let expected = Program {
            func: Function {
                name: String::from("main"),
                statements: vec![
                    Box::new(DeclareStatement {
                        var_name: String::from("a"),
                        expr: Some(Box::new(IntExpression { val: 3 }))
                    }),
                    Box::new(ExprStatement {
                        expr: Box::new(AssignExpression {
                            var_name: String::from("a"),
                            expr: Box::new(IntExpression {val: 4})
                        })
                    }),
                    Box::new(ReturnStatement {
                        expr: Box::new(VarExpression {
                            var_name: String::from("a")
                        })
                    })
                ]
            }
        };
        let ast = parse(tokens).unwrap();
        assert_eq!(ast.to_string(), expected.to_string());
    }

    #[test]
    #[should_panic]
    fn parse_missing_paren_func_body() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            Negation,
            OpenParenthesis,
            BitwiseComplement,
            OpenParenthesis,
            ConstInt(3),
            // CloseParenthesis,
            CloseParenthesis,
            Semicolon,
            CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_paren_func_params() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            // CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(3),
            Semicolon,
            CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_retval() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            // ConstInt(3),
            Semicolon,
            CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_brace() {
        let tokens: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(3),
            Semicolon,
            // CloseBrace,
        ];
        parse(tokens).unwrap();
    }
}
