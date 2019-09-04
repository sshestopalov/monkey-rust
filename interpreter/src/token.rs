use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Illegal,

    // Identifiers + literals
    Identifier,
    Int,
    Str,
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Equal,
    NotEqual,

    // Delimeters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywors
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token = match self {
            TokenType::Int => "int".to_owned(),
            TokenType::Str => "str".to_owned(),
            TokenType::Assign => "=".to_owned(),
            TokenType::Plus => "+".to_owned(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Bang => "!".to_owned(),
            TokenType::Asterisk => "*".to_owned(),
            TokenType::Slash => "/".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::Semicolon => ";".to_owned(),
            TokenType::Lt => "<".to_owned(),
            TokenType::Gt => ">".to_owned(),
            TokenType::Equal => "==".to_owned(),
            TokenType::NotEqual => "!=".to_owned(),
            TokenType::LBrace => "{".to_owned(),
            TokenType::RBrace => "}".to_owned(),
            TokenType::LParen => "(".to_owned(),
            TokenType::RParen => ")".to_owned(),
            TokenType::LBracket => "[".to_owned(),
            TokenType::RBracket => "]".to_owned(),
            TokenType::Function => "fucntion".to_owned(),
            TokenType::Let => "let".to_owned(),
            TokenType::If => "if".to_owned(),
            TokenType::Else => "else".to_owned(),
            TokenType::Return => "return".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::False => "false".to_owned(),
            _ => format!("{:?}", self),
        };

        write!(f, "{}", token.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type: token_type,
            literal: literal,
        }
    }

    pub fn lookup_ident(identifier: &str) -> TokenType {
        use TokenType::*;
        match identifier {
            "let" => Let,
            "fn" => Function,
            "if" => If,
            "else" => Else,
            "return" => Return,
            "true" => True,
            "false" => False,
            _ => Identifier,
        }
    }
}
