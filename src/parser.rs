use lexer::Token;

pub trait Expression {
    fn visit(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement {
    fn visit(&self) -> String;
    fn to_string(&self) -> String;
}

pub struct ReturnStatement {
    pub expression: Box<dyn Expression>,
}

pub struct Function {
    pub name: String,
    pub params: Vec<Box<dyn Statement>>,
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct IntExpression {
    pub val: u32,
}

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
}

pub struct UnaryOp {
    pub unary_op_type: UnaryOpType,
    pub expression: Box<dyn Expression>,
}

pub struct BinaryOp {
    pub bin_op_type: BinOpType,
    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    let mut tokens = tokens;
    tokens.reverse();
    Ok(Program {
        statements: parse_stmts(&mut tokens, true)?,
    })
}

type StatementResult = Result<Box<dyn Statement>, &'static str>;
type StatementsResult = Result<Vec<Box<dyn Statement>>, &'static str>;
type ExpressionResult = Result<Box<dyn Expression>, &'static str>;

fn parse_function(tokens: &mut Vec<Token>) -> StatementResult {
    let f_name = match tokens.pop() {
        Some(t) => match t {
            Token::IntKeyword => match tokens.pop() {
                Some(t) => match t {
                    Token::Identifier(name) => name,
                    _ => return Err("No name is specified"),
                },
                None => return Err("Unexpted end of function definition (expected function name)"),
            },
            _ => return Err("Wrong function return type (only 'int' supported)"),
        },
        None => return Err("Unexpted end of function definition (expected function return type)"),
    };

    let f_params = match tokens.pop() {
        Some(t) => match t {
            Token::OpenParenthesis => parse_function_params(tokens)?,
            _ => return Err("Wrong function definition"),
        },
        _ => return Err("Unexpected end of function definition (expected function parameters)"),
    };

    let stms = match tokens.pop() {
        Some(t) => match t {
            Token::OpenBrace => parse_stmts(tokens, false)?,
            _ => return Err("Function body syntax error"),
        },
        _ => Vec::new(),
    };

    Ok(Box::new(Function {
        name: f_name,
        params: f_params,
        statements: stms,
    }))
}

fn parse_function_params(tokens: &mut Vec<Token>) -> StatementsResult {
    match tokens.pop() {
        Some(t) => match t {
            Token::CloseParenthesis => Ok(Vec::new()), // ok, quit
            _ => Err("Wrong function parameters syntax"),
        },
        _ => Err("Unexpected end of function parameters definition"),
    }
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
                expression: parse_expression(tokens)?,
            })),
            Token::IntKeyword => {
                tokens.push(t);
                parse_function(tokens)
            }
            _ => Err("Unknown statement"),
        },
        None => Err("Not a statement"),
    };

    match tokens.pop() {
        Some(t) => match t {
            Token::Semicolon => stmt_result,
            _ => Err("(;) expected"),
        },
        None => stmt_result,
    }
}

fn parse_expression(tokens: &mut Vec<Token>) -> ExpressionResult {
    let mut term = parse_term(tokens)?;
    loop {
        let tok = tokens.pop();
        if tok.is_some() {
            let tok = tok.unwrap();
            if tok == Token::Addition || tok == Token::Negation {
                let bin_op = token_to_bin_op_type(&tok)?;
                let next_term = parse_term(tokens)?;
                term = Box::new(BinaryOp {
                    bin_op_type: bin_op,
                    left: term,
                    right: next_term,
                });
            } else {
                tokens.push(tok);
                break;
            }
        } else {
            break;
        }
    }
    Ok(term)
}

fn parse_term(tokens: &mut Vec<Token>) -> ExpressionResult {
    let mut factor = parse_factor(tokens)?;
    loop {
        let tok = tokens.pop();
        if tok.is_some() {
            let tok = tok.unwrap();
            if tok == Token::Multiplication || tok == Token::Division {
                let bin_op = token_to_bin_op_type(&tok)?;
                let next_factor = parse_factor(tokens)?;
                factor = Box::new(BinaryOp {
                    bin_op_type: bin_op,
                    left: factor,
                    right: next_factor,
                });
            } else {
                tokens.push(tok);
                break;
            }
        } else {
            break;
        }
    }
    Ok(factor)
}

fn token_to_bin_op_type(token: &Token) -> Result<BinOpType, &'static str> {
    match token {
        &Token::Addition => Ok(BinOpType::Addition),
        &Token::Multiplication => Ok(BinOpType::Multiplication),
        &Token::Negation => Ok(BinOpType::Substraction),
        &Token::Division => Ok(BinOpType::Division),
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
                Ok(Box::new(UnaryOp {
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

// TODO: add tree inspecting tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let main_func = Function {
            name: String::from("main"),
            params: Vec::new(),
            statements: vec![Box::new(ReturnStatement {
                expression: Box::new(IntExpression { val: 3 }),
            })],
        };
        let stmts: Vec<Box<dyn Statement>> = vec![Box::new(main_func)];
        let program = Program { statements: stmts };
        assert_eq!(result.to_string(), program.to_string());
    }

    #[test]
    fn parse_unary_ops() {
        let tokens_no_paren: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::Negation,
            Token::BitwiseComplement,
            Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let tokens_paren: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::Negation,
            Token::OpenParenthesis,
            Token::BitwiseComplement,
            Token::OpenParenthesis,
            Token::ConstInt(3),
            Token::CloseParenthesis,
            Token::CloseParenthesis,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let res_no_paren = parse(tokens_no_paren).unwrap();
        let res_paren = parse(tokens_paren).unwrap();
        assert_eq!(res_no_paren.to_string(), res_paren.to_string());
    }

    #[test]
    fn parse_binary_ops_precedence() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(2),
            Token::Addition,
            Token::ConstInt(3),
            Token::Multiplication,
            Token::ConstInt(4),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let expected = Program {
            statements: vec![Box::new(Function {
                name: String::from("main"),
                params: Vec::new(),
                statements: vec![Box::new(ReturnStatement {
                    expression: Box::new(BinaryOp {
                        bin_op_type: BinOpType::Addition,
                        left: Box::new(IntExpression { val: 2 }),
                        right: Box::new(BinaryOp {
                            bin_op_type: BinOpType::Multiplication,
                            left: Box::new(IntExpression { val: 3 }),
                            right: Box::new(IntExpression { val: 4 }),
                        }),
                    }),
                })],
            })],
        };
        assert_eq!(result.to_string(), expected.to_string());
    }

    #[test]
    fn parse_unary_and_binary_ops_precedence() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::BitwiseComplement,
            Token::ConstInt(2),
            Token::Addition,
            Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let expected = Program {
            statements: vec![Box::new(Function {
                name: String::from("main"),
                params: Vec::new(),
                statements: vec![Box::new(ReturnStatement {
                    expression: Box::new(BinaryOp {
                        bin_op_type: BinOpType::Addition,
                        left: Box::new(UnaryOp {
                            unary_op_type: UnaryOpType::Complement,
                            expression: Box::new(IntExpression { val: 2 }),
                        }),
                        right: Box::new(IntExpression { val: 3 }),
                    }),
                })],
            })],
        };
        assert_eq!(result.to_string(), expected.to_string());
    }

    #[test]
    fn parse_unary_and_binary_ops_precedence_parens() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::BitwiseComplement,
            Token::OpenParenthesis,
            Token::ConstInt(2),
            Token::Addition,
            Token::ConstInt(3),
            Token::CloseParenthesis,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let expected = Program {
            statements: vec![Box::new(Function {
                name: String::from("main"),
                params: Vec::new(),
                statements: vec![Box::new(ReturnStatement {
                    expression: Box::new(UnaryOp {
                        unary_op_type: UnaryOpType::Complement,
                        expression: Box::new(BinaryOp {
                            bin_op_type: BinOpType::Addition,
                            left: Box::new(IntExpression { val: 2 }),
                            right: Box::new(IntExpression { val: 3 }),
                        }),
                    }),
                })],
            })],
        };
        assert_eq!(result.to_string(), expected.to_string());
    }

    #[test]
    fn parse_binary_ops_paren_precedence() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::OpenParenthesis,
            Token::ConstInt(2),
            Token::Addition,
            Token::ConstInt(3),
            Token::CloseParenthesis,
            Token::Multiplication,
            Token::ConstInt(4),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result = parse(tokens).unwrap();
        let expected = Program {
            statements: vec![Box::new(Function {
                name: String::from("main"),
                params: Vec::new(),
                statements: vec![Box::new(ReturnStatement {
                    expression: Box::new(BinaryOp {
                        bin_op_type: BinOpType::Multiplication,
                        left: Box::new(BinaryOp {
                            bin_op_type: BinOpType::Addition,
                            left: Box::new(IntExpression { val: 2 }),
                            right: Box::new(IntExpression { val: 3 }),
                        }),
                        right: Box::new(IntExpression { val: 4 }),
                    }),
                })],
            })],
        };
        assert_eq!(result.to_string(), expected.to_string());
    }

    #[test]
    fn parse_binary_ops_paren_same_precedence() {
        let tokens_paren: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::OpenParenthesis,
            Token::ConstInt(2),
            Token::Addition,
            Token::OpenParenthesis,
            Token::ConstInt(3),
            Token::Multiplication,
            Token::ConstInt(4),
            Token::CloseParenthesis,
            Token::CloseParenthesis,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(2),
            Token::Addition,
            Token::ConstInt(3),
            Token::Multiplication,
            Token::ConstInt(4),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let result_paren = parse(tokens_paren).unwrap();
        let result = parse(tokens).unwrap();
        assert_eq!(result_paren.to_string(), result.to_string());
    }

    #[test]
    #[should_panic]
    fn parse_missing_paren_func_body() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::Negation,
            Token::OpenParenthesis,
            Token::BitwiseComplement,
            Token::OpenParenthesis,
            Token::ConstInt(3),
            // Token::CloseParenthesis,
            Token::CloseParenthesis,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_paren_func_params() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            // Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_retval() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            // Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_brace() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(3),
            Token::Semicolon,
            // Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_no_semicolon() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(3),
            // Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_no_space_at_retval() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Identifier(String::from("return0")),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_return_uppercase() {
        let tokens: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Identifier(String::from("RETURN")),
            Token::ConstInt(3),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        parse(tokens).unwrap();
    }
}
