use std::iter::Peekable;
use std::str::Chars;

use crate::token;
use token::{Token, TokenType};

#[derive(Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().peekable(),
            ch: char::from(0),
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.ch = self.input.next().unwrap_or(char::from(0));
    }

    fn peek_char(&mut self) -> char {
        let nil_char = char::from(0);
        let next_char = self.input.peek().unwrap_or(&nil_char);
        *next_char
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        while self.ch.is_alphabetic() {
            identifier.push(self.ch);
            self.read_char();
        }
        identifier
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();

        while self.ch.is_digit(10) {
            number.push(self.ch);
            self.read_char();
        }
        number
    }

    fn read_string(&mut self) -> String {
        let mut string = String::new();

        self.read_char();

        while self.ch != '"' && self.ch != '\0' {
            if self.ch == '\\' {
                self.read_char();
            }
            string.push(self.ch);

            self.read_char();
        }
        string
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        use TokenType::*;

        self.skip_whitespace();

        let string_char = self.ch.to_string();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let mut literal = self.ch.to_string();
                    self.read_char();
                    literal.push(self.ch);

                    Token::new(Equal, literal)
                } else {
                    Token::new(Assign, string_char)
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let mut literal = self.ch.to_string();
                    self.read_char();
                    literal.push(self.ch);

                    Token::new(NotEqual, literal)
                } else {
                    Token::new(Bang, string_char)
                }
            }
            ';' => Token::new(Semicolon, string_char),
            '(' => Token::new(LParen, string_char),
            ')' => Token::new(RParen, string_char),
            '[' => Token::new(LBracket, string_char),
            ']' => Token::new(RBracket, string_char),
            ',' => Token::new(Comma, string_char),
            '+' => Token::new(Plus, string_char),
            '-' => Token::new(Minus, string_char),
            '*' => Token::new(Asterisk, string_char),
            '/' => Token::new(Slash, string_char),
            '<' => Token::new(Lt, string_char),
            '>' => Token::new(Gt, string_char),
            '{' => Token::new(LBrace, string_char),
            '}' => Token::new(RBrace, string_char),
            '\0' => return None,
            '"' => Token::new(Str, self.read_string()),
            ch => {
                if ch.is_alphabetic() {
                    let ident = self.read_identifier();
                    let token_type = token::Token::lookup_ident(ident.as_str());
                    return Some(Token::new(token_type, ident));
                } else if ch.is_digit(10) {
                    return Some(Token::new(Int, self.read_number()));
                } else {
                    Token::new(Illegal, string_char)
                }
            }
        };
        self.read_char();

        Some(token)
    }
}
