#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    IntKeyword,
    ReturnKeyword,
    Identifier(String),
    ConstInt(u32),
    Negation,
    BitwiseComplement,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    Modulo,
    LogicalNegation,
    Addition,
    Multiplication,
    Division,
    Equal,
    NotEqual,
    Or,
    And,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

pub fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = contents.chars().peekable();
    let mut curr_char_opt = chars.next();
    while curr_char_opt.is_some() {
        match curr_char_opt.unwrap() {
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),
            '(' => tokens.push(Token::OpenParenthesis),
            ')' => tokens.push(Token::CloseParenthesis),
            ';' => tokens.push(Token::Semicolon),
            '-' => tokens.push(Token::Negation),
            '^' => tokens.push(Token::BitwiseXor),
            '%' => tokens.push(Token::Modulo),
            '~' => tokens.push(Token::BitwiseComplement),
            '+' => tokens.push(Token::Addition),
            '*' => tokens.push(Token::Multiplication),
            '/' => tokens.push(Token::Division),
            ch => {
                if ch.is_numeric() {
                    let mut num_chars: Vec<char> = Vec::new();
                    num_chars.push(ch);
                    loop {
                        {
                            let opt = chars.peek();
                            if opt.is_none() || !opt.unwrap().is_numeric() {
                                break;
                            }
                        }
                        num_chars.push(chars.next().unwrap());
                    }
                    let int_val: String = num_chars.into_iter().collect();
                    match int_val.parse::<u32>() {
                        Ok(n) => {
                            tokens.push(Token::ConstInt(n));
                        }
                        Err(_) => {
                            tokens.push(Token::Identifier(int_val));
                        }
                    };
                } else if ch.is_alphabetic() {
                    let mut some_chars: Vec<char> = Vec::new();
                    some_chars.push(ch);
                    loop {
                        {
                            let opt = chars.peek();
                            if opt.is_none() || !opt.unwrap().is_alphanumeric() {
                                break;
                            }
                        }
                        some_chars.push(chars.next().unwrap());
                    }
                    let keyword_or_identifier: String = some_chars.into_iter().collect();
                    if keyword_or_identifier.eq("int") {
                        tokens.push(Token::IntKeyword);
                    } else if keyword_or_identifier.eq("return") {
                        tokens.push(Token::ReturnKeyword);
                    } else {
                        tokens.push(Token::Identifier(keyword_or_identifier));
                    }
                } else if ch == '<' || ch == '>' || ch == '=' || ch == '|' || ch == '&' || ch == '!'
                {
                    let mut skip = false;
                    {
                        let peek = chars.peek();
                        match ch {
                            '<' => match peek {
                                Some(&'=') => {
                                    tokens.push(Token::LessOrEqual);
                                    skip = true;
                                }
                                Some(&'<') => {
                                    tokens.push(Token::BitwiseShiftLeft);
                                    skip = true;
                                }
                                _ => tokens.push(Token::LessThan),
                            },
                            '>' => match peek {
                                Some(&'=') => {
                                    tokens.push(Token::GreaterOrEqual);
                                    skip = true;
                                }
                                Some(&'>') => {
                                    tokens.push(Token::BitwiseShiftRight);
                                    skip = true;
                                }
                                _ => tokens.push(Token::GreaterThan),
                            },
                            '=' => match peek {
                                Some(&'=') => {
                                    tokens.push(Token::Equal);
                                    skip = true;
                                }
                                _ => (),
                            },
                            '|' => match peek {
                                Some(&'|') => {
                                    tokens.push(Token::Or);
                                    skip = true;
                                }
                                _ => tokens.push(Token::BitwiseOr),
                            },
                            '&' => match peek {
                                Some(&'&') => {
                                    tokens.push(Token::And);
                                    skip = true;
                                }
                                _ => tokens.push(Token::BitwiseAnd),
                            },
                            '!' => match peek {
                                Some(&'=') => {
                                    tokens.push(Token::NotEqual);
                                    skip = true;
                                }
                                _ => tokens.push(Token::LogicalNegation),
                            },
                            _ => (),
                        }
                    }
                    if skip {
                        chars.next();
                    }
                }
            }
        }
        curr_char_opt = chars.next();
    }
    return tokens;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use super::Token::*;

    #[test]
    fn tokenize_simple() {
        let result = tokenize("int main() {\n return 3; \n}");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(3),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_min_spaces() {
        let result = tokenize("int main(){return;}");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_spaces() {
        let result = tokenize("   int    main ( ) {   return    100  ; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(100),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_unary_ops() {
        let result = tokenize("int main() { return !~-4; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            LogicalNegation,
            BitwiseComplement,
            Negation,
            ConstInt(4),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_no_space_at_retval() {
        let result = tokenize("int main() { return0; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            Identifier(String::from("return0")),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_return_uppercase() {
        let result = tokenize("int main() { RETURN 3; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            Identifier(String::from("RETURN")),
            ConstInt(3),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_bitwise() {
        let result = tokenize("int main() { return <<>>100; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            BitwiseShiftLeft,
            BitwiseShiftRight,
            ConstInt(100),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
        let result = tokenize("int main() { return ^&|1; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            BitwiseXor,
            BitwiseAnd,
            BitwiseOr,
            ConstInt(1),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_relational() {
        let result = tokenize("int main() { return 1 < 2 <= 3 == 3 >= 2 > 1; }");
        let expected: Vec<Token> = vec![
            IntKeyword,
            Identifier(String::from("main")),
            OpenParenthesis,
            CloseParenthesis,
            OpenBrace,
            ReturnKeyword,
            ConstInt(1),
            LessThan,
            ConstInt(2),
            LessOrEqual,
            ConstInt(3),
            Equal,
            ConstInt(3),
            GreaterOrEqual,
            ConstInt(2),
            GreaterThan,
            ConstInt(1),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(result, expected);
    }
}
