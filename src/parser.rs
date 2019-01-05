use lexer::Token;

pub trait Node {
    fn visit(&self) -> String;
}

pub struct ReturnStatement {
    pub expression: Box<dyn Node>,
}

pub struct MainFunction {
    pub statements: Vec<Box<dyn Node>>,
}

pub struct IntExpression {
    pub val: u32,
}

pub struct Program {
    pub main: MainFunction,
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, &'static str> {
    if !tokens.contains(&Token::Identifier(String::from("main"))) {
        return Err("Then 'main' function is not found");
    }

    let program = parse_program(tokens)?;
    Ok(program)
}

fn parse_program(mut tokens: Vec<Token>) -> Result<Program, &'static str> {
    let function = parse_function(&mut tokens)?;
    Ok(Program { main: function })
}

fn parse_function(tokens: &mut Vec<Token>) -> Result<MainFunction, &'static str> {
    let token = tokens.pop();
    if token.is_none() {
        return Err("err 1");
    }
    let token = token.unwrap();
    if !token.eq(&Token::IntKeyword) {
        return Err("err 2");
    }

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 3");
    }
    let token = token.unwrap();
    if !token.eq(&Token::Identifier(String::from("main"))) {
        return Err("err 4");
    }

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 5");
    }
    let token = token.unwrap();
    if !token.eq(&Token::OpenParenthesis) {
        return Err("err 6");
    }

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 7");
    }
    let token = token.unwrap();
    if !token.eq(&Token::CloseParenthesis) {
        return Err("err 8");
    }

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 9");
    }
    let token = token.unwrap();
    if !token.eq(&Token::OpenBrace) {
        return Err("err 10");
    }

    let func = MainFunction {
        statements: parse_statements(tokens)?,
    };

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 11");
    }
    let token = token.unwrap();
    if !token.eq(&Token::CloseBrace) {
        return Err("err 12");
    }

    Ok(func)
}

fn parse_statements(tokens: &mut Vec<Token>) -> Result<Vec<Box<dyn Node>>, &'static str> {
    let mut statements: Vec<Box<dyn Node>> = Vec::new();
    let token = tokens.pop();
    if token.is_none() {
        return Ok(statements);
    }
    let token = token.unwrap();
    if !token.eq(&Token::ReturnKeyword) {
        return Err("err 13");
    }

    let expr = parse_expression(tokens)?;
    statements.push(Box::new(ReturnStatement { expression: expr }));

    let token = tokens.pop();
    if token.is_none() {
        return Err("err 16");
    }
    let token = token.unwrap();
    if !token.eq(&Token::Semicolon) {
        return Err("err 17");
    }

    Ok(statements)
}

fn parse_expression(tokens: &mut Vec<Token>) -> Result<Box<dyn Node>, &'static str> {
    match tokens.pop() {
        Some(tok) => match tok {
            Token::IntLiteral(n) => return Ok(Box::new(IntExpression { val: n })),
            _ => return Err("err 14"),
        },
        None => return Err("err 15"),
    }
}
