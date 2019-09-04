use super::*;
use crate::token::{Token, TokenType};
use TokenType::*;
#[test]
fn test_next_token() {
    let input = r#"let five = 55;
        
            let ten = 10;
            
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
            	return true;
            } else {
            	return false;
            }

            10 == 10;
            10 != 9;
            ""
            "\\"
            "foobar"
            "foo bar"
            "foo \"bar\""
            [1,2]
            "#;
    let tests = [
        Token::new(Let, "let".to_string()),
        Token::new(Identifier, "five".to_string()),
        Token::new(Assign, "=".to_string()),
        Token::new(Int, "55".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Let, "let".to_string()),
        Token::new(Identifier, "ten".to_string()),
        Token::new(Assign, "=".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Let, "let".to_string()),
        Token::new(Identifier, "add".to_string()),
        Token::new(Assign, "=".to_string()),
        Token::new(Function, "fn".to_string()),
        Token::new(LParen, "(".to_string()),
        Token::new(Identifier, "x".to_string()),
        Token::new(Comma, ",".to_string()),
        Token::new(Identifier, "y".to_string()),
        Token::new(RParen, ")".to_string()),
        Token::new(LBrace, "{".to_string()),
        Token::new(Identifier, "x".to_string()),
        Token::new(Plus, "+".to_string()),
        Token::new(Identifier, "y".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(RBrace, "}".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Let, "let".to_string()),
        Token::new(Identifier, "result".to_string()),
        Token::new(Assign, "=".to_string()),
        Token::new(Identifier, "add".to_string()),
        Token::new(LParen, "(".to_string()),
        Token::new(Identifier, "five".to_string()),
        Token::new(Comma, ",".to_string()),
        Token::new(Identifier, "ten".to_string()),
        Token::new(RParen, ")".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Bang, "!".to_string()),
        Token::new(Minus, "-".to_string()),
        Token::new(Slash, "/".to_string()),
        Token::new(Asterisk, "*".to_string()),
        Token::new(Int, "5".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Int, "5".to_string()),
        Token::new(Lt, "<".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(Gt, ">".to_string()),
        Token::new(Int, "5".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(If, "if".to_string()),
        Token::new(LParen, "(".to_string()),
        Token::new(Int, "5".to_string()),
        Token::new(Lt, "<".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(RParen, ")".to_string()),
        Token::new(LBrace, "{".to_string()),
        Token::new(Return, "return".to_string()),
        Token::new(True, "true".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(RBrace, "}".to_string()),
        Token::new(Else, "else".to_string()),
        Token::new(LBrace, "{".to_string()),
        Token::new(Return, "return".to_string()),
        Token::new(False, "false".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(RBrace, "}".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(Equal, "==".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Int, "10".to_string()),
        Token::new(NotEqual, "!=".to_string()),
        Token::new(Int, "9".to_string()),
        Token::new(Semicolon, ";".to_string()),
        Token::new(Str, "".to_string()),
        Token::new(Str, "\\".to_string()),
        Token::new(Str, "foobar".to_string()),
        Token::new(Str, "foo bar".to_string()),
        Token::new(Str, "foo \"bar\"".to_string()),
        Token::new(LBracket, "[".to_string()),
        Token::new(Int, "1".to_string()),
        Token::new(Comma, ",".to_string()),
        Token::new(Int, "2".to_string()),
        Token::new(RBracket, "]".to_string()),
    ];
    let mut lexer = Lexer::new(input);
    for t in tests.iter() {
        let token = lexer.next().unwrap();
        println!("{:?}", token);
        assert_eq!(token.token_type, t.token_type);
        assert_eq!(token.literal, t.literal);
    }
}
