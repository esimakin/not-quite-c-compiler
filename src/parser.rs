use self::BinOpType::*;
use self::UnaryOpType::*;
use lexer::Token;
use lexer::Token::*;
use std::fmt::Display;

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

pub trait Visitor {
    fn visit(&self) -> String;
}

pub trait Node: Visitor + AstDisplay {}

pub struct Program {
    pub func: Function,
}

pub struct Function {
    pub name: String,
    pub statements: Vec<Box<dyn Node>>,
}

pub struct ReturnStatement {
    pub expr: Box<dyn Node>,
}

pub struct DeclareStatement {
    pub var_name: String,
    pub expr: Option<Box<dyn Node>>,
}

pub struct ExprStatement {
    pub expr: Box<dyn Node>,
}

pub struct AssignExpression {
    pub var_name: String,
    pub expr: Box<dyn Node>,
}

pub struct VarExpression {
    pub var_name: String,
}

pub struct BinOpExpression {
    pub bin_op_type: BinOpType,
    pub left: Box<dyn Node>,
    pub right: Box<dyn Node>,
}

pub struct UnaryOpExpression {
    pub unary_op_type: UnaryOpType,
    pub expr: Box<dyn Node>,
}

pub struct IntExpression {
    pub val: u32,
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    let mut tokens = tokens;
    tokens.reverse();
    let program = Program {
        func: parse_function(&mut tokens)?,
    };
    println!("{}", program.ast_to_string(0));
    Ok(program)
}

type StatementResult = Result<Box<dyn Node>, &'static str>;
type StatementsResult = Result<Vec<Box<dyn Node>>, &'static str>;
type ExpressionResult = Result<Box<dyn Node>, &'static str>;
type Tokens<'a> = &'a mut Vec<Token>;

fn parse_function(tokens: Tokens) -> Result<Function, &'static str> {
    let f_name = match tokens.pop() {
        Some(INT) => match tokens.pop() {
            Some(ID(name)) => Ok(name),
            _ => Err("Function identifier expected"),
        },
        _ => Err("Unexpected start of function declaration"),
    }?;

    match tokens.pop() {
        Some(LParen) => match tokens.pop() {
            Some(RParen) => Ok(RParen),
            _ => Err("Syntax error"),
        },
        _ => Err("Syntax error"),
    }?;

    let stms = match tokens.pop() {
        Some(LBrace) => Ok(parse_stmts(tokens)?),
        _ => Err("Function body syntax error"),
    }?;

    Ok(Function {
        name: f_name,
        statements: stms,
    })
}

fn parse_stmts(tokens: Tokens) -> StatementsResult {
    let mut statements: Vec<Box<dyn Node>> = Vec::new();
    loop {
        match tokens.pop() {
            Some(RBrace) => break, // ok, quit
            Some(t) => {
                tokens.push(t);
                statements.push(parse_one_statement(tokens)?);
            }
            None => {
                return Err("Unexpected end of statements");
            }
        };
    }

    Ok(statements)
}

fn parse_one_statement(tokens: Tokens) -> StatementResult {
    let stmt_result: StatementResult = match tokens.pop() {
        Some(RET) => Ok(Box::new(ReturnStatement {
            expr: parse_expression(tokens)?,
        })),
        Some(INT) => parse_int_statement(tokens),
        Some(t) => {
            tokens.push(t);
            Ok(Box::new(ExprStatement {
                expr: parse_expression(tokens)?,
            }))
        }
        None => return Err("Not a statement"),
    };

    match tokens.pop() {
        Some(t) => match t {
            Semi => stmt_result,
            _ => Err("Semicolon expected"),
        },
        None => stmt_result,
    }
}

fn parse_int_statement(tokens: Tokens) -> StatementResult {
    let id = match tokens.pop() {
        Some(ID(name)) => Ok(name),
        _ => Err("Expected identifier, got something else"),
    }?;

    let expr: Option<Box<dyn Node>> = match tokens.pop() {
        Some(Assign) => Some(parse_expression(tokens)?),
        Some(_) => return Err("Syntax error"),
        None => None,
    };

    Ok(Box::new(DeclareStatement {
        var_name: id,
        expr: expr,
    }))
}

fn parse_expression(tokens: Tokens) -> ExpressionResult {
    match tokens.pop() {
        Some(ID(name)) => match tokens.pop() {
            Some(Assign) => Ok(Box::new(AssignExpression {
                var_name: name,
                expr: parse_expression(tokens)?,
            })),
            Some(t) => {
                tokens.push(t);
                tokens.push(ID(name));
                parse_logical_or_expression(tokens)
            }
            _ => Err("Unexpected end of assignment"),
        },
        Some(t) => {
            tokens.push(t);
            parse_logical_or_expression(tokens)
        }
        None => Err("Unexpected end of expression"),
    }
}

fn parse_logical_or_expression(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_logical_and_expression, |t: &Token| {
        t == &OrTok
    })
}

fn parse_logical_and_expression(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_equality_expression, |t: &Token| t == &AndTok)
}

fn parse_equality_expression(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_relational_expression, |t: &Token| {
        t == &EqTok || t == &NEq
    })
}

fn parse_relational_expression(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_additive_expression, |t: &Token| {
        t == &Le || t == &Ge || t == &LeQ || t == &GeQ
    })
}

fn parse_additive_expression(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_term, |t: &Token| t == &Plus || t == &Minus)
}

fn parse_term(tokens: Tokens) -> ExpressionResult {
    parse_rule(tokens, parse_factor, |t: &Token| t == &Mul || t == &Div)
}

fn parse_rule<F, P>(tokens: Tokens, parse_one: F, predicate: P) -> ExpressionResult
where
    F: Fn(Tokens) -> ExpressionResult,
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
    Ok(match token {
        &Plus => Addition,
        &Mul => Multiplication,
        &Minus => Substraction,
        &Div => Division,
        &OrTok => Or,
        &AndTok => And,
        &Le => Less,
        &LeQ => LessOrEq,
        &Ge => Greater,
        &GeQ => GreaterOrEq,
        &EqTok => Equal,
        &NEq => NotEqual,
        _ => return Err("Unknown binary operation"),
    })
}

fn token_to_unary_op_type(token: &Token) -> Result<UnaryOpType, &'static str> {
    Ok(match token {
        &BtwCompl => Complement,
        &Minus => Negation,
        &Neg => LogicalNegation,
        _ => return Err("Unknown unary operator"),
    })
}

fn parse_factor(tokens: Tokens) -> ExpressionResult {
    match tokens.pop() {
        Some(tok) => match tok {
            CInt(n) => Ok(Box::new(IntExpression { val: n })),
            Neg | BtwCompl | Minus => {
                let expr = parse_factor(tokens)?;
                Ok(Box::new(UnaryOpExpression {
                    unary_op_type: token_to_unary_op_type(&tok)?,
                    expr: expr,
                }))
            }
            LParen => {
                let expr = parse_expression(tokens)?;
                match tokens.pop() {
                    Some(tok) => {
                        if tok == RParen {
                            Ok(expr)
                        } else {
                            Err("Missing parenthesis (unexpected token)")
                        }
                    }
                    None => Err("Missing parenthesis (unexpected end of stream)"),
                }
            }
            ID(name) => Ok(Box::new(VarExpression { var_name: name })),
            _ => Err("Expression syntax error"),
        },
        None => Err("Unexpected end of expression"),
    }
}

impl AstDisplay for ReturnStatement {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for ReturnStatement {}

impl AstDisplay for DeclareStatement {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for DeclareStatement {}

impl AstDisplay for ExprStatement {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for ExprStatement {}

impl AstDisplay for AssignExpression {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for AssignExpression {}

impl AstDisplay for VarExpression {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for VarExpression {}

impl AstDisplay for UnaryOpExpression {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for UnaryOpExpression {}

impl AstDisplay for BinOpExpression {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for BinOpExpression {}

impl AstDisplay for IntExpression {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}
impl Node for IntExpression {}

impl AstDisplay for Function {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        val
    }
}

impl AstDisplay for UnaryOpType {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        match self {
            Complement => val.push_str("~"),
            Negation => val.push_str("-"),
            LogicalNegation => val.push_str("!"),
        }
        val
    }
}

impl AstDisplay for BinOpType {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("");
        match self {
            Addition => val.push_str("+"),
            Multiplication => val.push_str("*"),
            Substraction => val.push_str("-"),
            Division => val.push_str("/"),
            Or => val.push_str("||"),
            And => val.push_str("&&"),
            Less => val.push_str("<"),
            LessOrEq => val.push_str("<="),
            Greater => val.push_str(">"),
            GreaterOrEq => val.push_str(">="),
            Equal => val.push_str("=="),
            NotEqual => val.push_str("!="),
        }
        val
    }
}

impl AstDisplay for Program {
    fn ast_to_string(&self, depth: u32) -> String {
        let mut val = String::from("PROGRAM\n");
        val.push_str(&self.func.ast_to_string(depth + 1));
        val
    }
}

trait AstDisplay {
    fn ast_to_string(&self, depth: u32) -> String;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::tokenize;

    #[test]
    #[ignore]
    fn parse_unary_paren() {
        let tokens_no_paren = tokenize("int main() { return -!3; }");
        let tokens_paren = tokenize("int main() { return -(!(3)); }");
        let ast_no_paren = parse(tokens_no_paren).unwrap();
        let ast_paren = parse(tokens_paren).unwrap();
        // assert_eq!(ast_no_paren.to_string(), ast_paren.to_string());
    }

    #[test]
    #[ignore]
    fn parse_unary_and_binary_ops_precedence_parens() {
        let tokens = tokenize("int main() { return ~(2 + 3); }");
        let ast = parse(tokens).unwrap();
        let expected = Program {
            func: Function {
                name: String::from("main"),
                statements: vec![Box::new(ReturnStatement {
                    expr: Box::new(UnaryOpExpression {
                        unary_op_type: Complement,
                        expr: Box::new(BinOpExpression {
                            bin_op_type: Addition,
                            left: Box::new(IntExpression { val: 2 }),
                            right: Box::new(IntExpression { val: 3 }),
                        }),
                    }),
                })],
            },
        };
        // assert_eq!(ast.to_string(), expected.to_string());
    }

    #[test]
    #[ignore]
    fn parse_binary_ops_paren_same_precedence() {
        let tokens_paren = tokenize("int main() { return (2 + (3 * 4)); }");
        let tokens = tokenize("int main() { return 2 + 3 * 4; }");
        let ast_paren = parse(tokens_paren).unwrap();
        let ast = parse(tokens).unwrap();
        // assert_eq!(ast_paren.to_string(), ast.to_string());
    }

    #[test]
    #[ignore]
    fn parse_many_statements() {
        let tokens = tokenize("int main() { int a = 3; a = 4; return a; }");
        let expected = Program {
            func: Function {
                name: String::from("main"),
                statements: vec![
                    Box::new(DeclareStatement {
                        var_name: String::from("a"),
                        expr: Some(Box::new(IntExpression { val: 3 })),
                    }),
                    Box::new(ExprStatement {
                        expr: Box::new(AssignExpression {
                            var_name: String::from("a"),
                            expr: Box::new(IntExpression { val: 4 }),
                        }),
                    }),
                    Box::new(ReturnStatement {
                        expr: Box::new(VarExpression {
                            var_name: String::from("a"),
                        }),
                    }),
                ],
            },
        };
        let ast = parse(tokens).unwrap();
        // assert_eq!(ast.to_string(), expected.to_string());
    }

    #[test]
    #[should_panic]
    fn parse_missing_paren_func_body() {
        let tokens = tokenize("int main() { return -(~(2); }");
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_retval() {
        let tokens = tokenize("int main() { return ; }");
        parse(tokens).unwrap();
    }

    #[test]
    #[should_panic]
    fn parse_missing_brace() {
        let tokens = tokenize("int main() {return 3;");
        parse(tokens).unwrap();
    }
}
