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
    Assignment,
}

use std::fmt;
use self::Token::*;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = contents.chars().peekable();
    let mut curr_char_opt = chars.next();
    while curr_char_opt.is_some() {
        match curr_char_opt.unwrap() {
            '{' => tokens.push(OpenBrace),
            '}' => tokens.push(CloseBrace),
            '(' => tokens.push(OpenParenthesis),
            ')' => tokens.push(CloseParenthesis),
            ';' => tokens.push(Semicolon),
            '-' => tokens.push(Negation),
            '^' => tokens.push(BitwiseXor),
            '%' => tokens.push(Modulo),
            '~' => tokens.push(BitwiseComplement),
            '+' => tokens.push(Addition),
            '*' => tokens.push(Multiplication),
            '/' => tokens.push(Division),
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
                            tokens.push(ConstInt(n));
                        }
                        Err(_) => {
                            tokens.push(Identifier(int_val));
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
                        tokens.push(IntKeyword);
                    } else if keyword_or_identifier.eq("return") {
                        tokens.push(ReturnKeyword);
                    } else {
                        tokens.push(Identifier(keyword_or_identifier));
                    }
                } else if ch == '<' || ch == '>' || ch == '=' || ch == '|' || ch == '&' || ch == '!'
                {
                    let mut skip = false;
                    {
                        let peek = chars.peek();
                        match ch {
                            '<' => match peek {
                                Some(&'=') => {
                                    tokens.push(LessOrEqual);
                                    skip = true;
                                }
                                Some(&'<') => {
                                    tokens.push(BitwiseShiftLeft);
                                    skip = true;
                                }
                                _ => tokens.push(LessThan),
                            },
                            '>' => match peek {
                                Some(&'=') => {
                                    tokens.push(GreaterOrEqual);
                                    skip = true;
                                }
                                Some(&'>') => {
                                    tokens.push(BitwiseShiftRight);
                                    skip = true;
                                }
                                _ => tokens.push(GreaterThan),
                            },
                            '=' => match peek {
                                Some(&'=') => {
                                    tokens.push(Equal);
                                    skip = true;
                                }
                                _ => tokens.push(Assignment),
                            },
                            '|' => match peek {
                                Some(&'|') => {
                                    tokens.push(Or);
                                    skip = true;
                                }
                                _ => tokens.push(BitwiseOr),
                            },
                            '&' => match peek {
                                Some(&'&') => {
                                    tokens.push(And);
                                    skip = true;
                                }
                                _ => tokens.push(BitwiseAnd),
                            },
                            '!' => match peek {
                                Some(&'=') => {
                                    tokens.push(NotEqual);
                                    skip = true;
                                }
                                _ => tokens.push(LogicalNegation),
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
    // use super::Token::*;

    #[test]
    fn tokenize_simple() {
        let tokens = tokenize("int main() {\n\tint a = 3; return a;\n}");
        let expected: Vec<Token> = vec![
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
            ReturnKeyword,
            Identifier(String::from("a")),
            Semicolon,
            CloseBrace,
        ];
        assert_eq!(tokens, expected);
    }

}
