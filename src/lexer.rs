#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Semi,
    INT,
    RET,
    ID(String),
    CInt(u32),
    Minus,
    BtwCompl,
    BtwOr,
    BtwAnd,
    BtwXor,
    BtwLShift,
    BtwRShift,
    Mod,
    Neg,
    Plus,
    Mul,
    Div,
    EqTok,
    NEq,
    OrTok,
    AndTok,
    Le,
    LeQ,
    Ge,
    GeQ,
    Assign,
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
            '{' => tokens.push(LBrace),
            '}' => tokens.push(RBrace),
            '(' => tokens.push(LParen),
            ')' => tokens.push(RParen),
            ';' => tokens.push(Semi),
            '-' => tokens.push(Minus),
            '^' => tokens.push(BtwXor),
            '%' => tokens.push(Mod),
            '~' => tokens.push(BtwCompl),
            '+' => tokens.push(Plus),
            '*' => tokens.push(Mul),
            '/' => tokens.push(Div),
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
                            tokens.push(CInt(n));
                        }
                        Err(_) => {
                            tokens.push(ID(int_val));
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
                        tokens.push(INT);
                    } else if keyword_or_identifier.eq("return") {
                        tokens.push(RET);
                    } else {
                        tokens.push(ID(keyword_or_identifier));
                    }
                } else if ch == '<' || ch == '>' || ch == '=' || ch == '|' || ch == '&' || ch == '!'
                {
                    let mut skip = false;
                    {
                        let peek = chars.peek();
                        match ch {
                            '<' => match peek {
                                Some(&'=') => {
                                    tokens.push(LeQ);
                                    skip = true;
                                }
                                Some(&'<') => {
                                    tokens.push(BtwLShift);
                                    skip = true;
                                }
                                _ => tokens.push(Le),
                            },
                            '>' => match peek {
                                Some(&'=') => {
                                    tokens.push(GeQ);
                                    skip = true;
                                }
                                Some(&'>') => {
                                    tokens.push(BtwRShift);
                                    skip = true;
                                }
                                _ => tokens.push(Ge),
                            },
                            '=' => match peek {
                                Some(&'=') => {
                                    tokens.push(EqTok);
                                    skip = true;
                                }
                                _ => tokens.push(Assign),
                            },
                            '|' => match peek {
                                Some(&'|') => {
                                    tokens.push(OrTok);
                                    skip = true;
                                }
                                _ => tokens.push(BtwOr),
                            },
                            '&' => match peek {
                                Some(&'&') => {
                                    tokens.push(AndTok);
                                    skip = true;
                                }
                                _ => tokens.push(BtwAnd),
                            },
                            '!' => match peek {
                                Some(&'=') => {
                                    tokens.push(NEq);
                                    skip = true;
                                }
                                _ => tokens.push(Neg),
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
            INT,
            ID(String::from("main")),
            LParen,
            RParen,
            LBrace,
            INT,
            ID(String::from("a")),
            Assign,
            CInt(3),
            Semi,
            RET,
            ID(String::from("a")),
            Semi,
            RBrace,
        ];
        assert_eq!(tokens, expected);
    }

}
