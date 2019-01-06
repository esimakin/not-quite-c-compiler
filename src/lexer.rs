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
    LogicalNegation,
}

pub fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars: Vec<char> = contents.chars().collect();
    chars.reverse();
    let mut curr_char_opt = chars.pop();
    while curr_char_opt.is_some() {
        match curr_char_opt.unwrap() {
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),
            '(' => tokens.push(Token::OpenParenthesis),
            ')' => tokens.push(Token::CloseParenthesis),
            ';' => tokens.push(Token::Semicolon),
            '-' => tokens.push(Token::Negation),
            '!' => tokens.push(Token::LogicalNegation),
            '~' => tokens.push(Token::BitwiseComplement),
            ch => {
                if ch.is_numeric() {
                    let mut num_chars: Vec<char> = Vec::new();
                    num_chars.push(ch);
                    let mut opt = chars.pop();
                    while opt.is_some() {
                        let some_ch = opt.unwrap();
                        if some_ch.is_numeric() {
                            num_chars.push(some_ch);
                        } else {
                            chars.push(some_ch);
                            break;
                        }
                        opt = chars.pop();
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
                    let mut opt = chars.pop();
                    while opt.is_some() {
                        let some_ch = opt.unwrap();
                        if some_ch.is_alphanumeric() {
                            some_chars.push(some_ch);
                        } else {
                            chars.push(some_ch);
                            break;
                        }
                        opt = chars.pop();
                    }
                    let keyword_or_identifier: String = some_chars.into_iter().collect();
                    if keyword_or_identifier.eq("int") {
                        tokens.push(Token::IntKeyword);
                    } else if keyword_or_identifier.eq("return") {
                        tokens.push(Token::ReturnKeyword);
                    } else {
                        tokens.push(Token::Identifier(keyword_or_identifier));
                    }
                }
            }
        }
        curr_char_opt = chars.pop();
    }
    return tokens;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_simple() {
        let result = tokenize("int main() {\n return 3; \n}");
        let expected: Vec<Token> = vec![
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
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_min_spaces() {
        let result = tokenize("int main(){return;}");
        let expected: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::Semicolon,
            Token::CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_spaces() {
        let result = tokenize("   int    main ( ) {   return    100  ; }");
        let expected: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstInt(100),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn tokenize_unary_ops() {
        let result = tokenize("int main() { return !~-4; }");
        let expected: Vec<Token> = vec![
            Token::IntKeyword,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::LogicalNegation,
            Token::BitwiseComplement,
            Token::Negation,
            Token::ConstInt(4),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        assert_eq!(result, expected);
    }
}
