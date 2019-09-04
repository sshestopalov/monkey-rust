use crate::ast::boolean::Boolean;
use crate::ast::expression::Expression;
use crate::ast::identifier::Identifier;
use crate::ast::infix::Infix;
use crate::ast::int::IntegerLiteral;
use crate::ast::statement::Statement;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::{Token, TokenType};

#[test]
fn test_let_statements() {
    struct Test<'a> {
        name: &'a str,
        value: i64,
    }

    let tests = vec![
        Test {
            name: "x",
            value: 5,
        },
        Test {
            name: "y",
            value: 10,
        },
        Test {
            name: "foobar",
            value: 838383,
        },
    ];

    let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program();

    if parser.errors.len() > 0 {
        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }
        panic!("errors occurred!");
    }

    match program {
        Ok(program) => {
            assert_eq!(program.statements.len(), 3);
            for i in 0..tests.len() {
                match &program.statements[i] {
                    Statement::Let(stmt) => {
                        match &stmt.name {
                            Some(identifier) => assert_eq!(identifier.value, tests[i].name),
                            None => panic!("not an identifier"),
                        };

                        match &stmt.value {
                            Some(Expression::Int(int)) => assert_eq!(int.value, tests[i].value),
                            _ => panic!("not an integer expression"),
                        };
                    }
                    _ => panic!("Not a let statement"),
                }
            }
        }
        Err(error) => panic!(format!("{}", error)),
    }
}

#[test]
fn test_return_statement() {
    let input = r#"
            return 5;
            return 10;
            return 340930;
        "#;
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.len() > 0 {
        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }
        panic!("errors occurred!");
    }

    match program {
        Ok(program) => {
            assert_eq!(program.statements.len(), 3);
            let tests: [&str; 3] = ["5", "10", "340930"];
            for i in 0..tests.len() {
                match &program.statements[i] {
                    Statement::Return(stmt) => match &stmt.return_value {
                        Some(value) => assert_eq!(value.token_literal(), tests[i]),
                        None => {}
                    },
                    _ => panic!("Not a return statement"),
                }
            }
        }
        Err(error) => panic!(format!("{}", error)),
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program();

    if parser.errors.len() > 0 {
        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }
        panic!("errors occurred!");
    }

    match program {
        Ok(p) => {
            assert_eq!(p.statements.len(), 1);

            match &p.statements[0] {
                Statement::Expression(statement) => {
                    assert_eq!(statement.token.token_type, TokenType::Identifier);
                    assert_eq!(statement.token.literal, "foobar");
                    assert!(statement.expression.is_some());

                    match &statement.expression {
                        Some(Expression::Identifier(identifier)) => {
                            assert_eq!(identifier.value, "foobar");
                            assert_eq!(identifier.token.token_type, TokenType::Identifier);
                            assert_eq!(identifier.token.literal, "foobar");
                        }
                        Some(_) => {}
                        None => {}
                    }
                }
                _ => panic!("statment is not an expression"),
            }
        }
        Err(err) => panic!(err),
    }
}

#[test]
fn test_integer_literal() {
    let input = "5;";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.len() > 0 {
        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }
        panic!("errors occurred!");
    }

    match program {
        Ok(p) => match &p.statements[0] {
            Statement::Expression(stmt) => {
                assert_eq!(stmt.token.token_type, TokenType::Int);
                assert_eq!(stmt.token.literal, "5");
                assert!(stmt.expression.is_some());
            }
            _ => panic!("statement is not an expression"),
        },
        Err(err) => panic!(err),
    }
}

#[test]
fn test_prefix_expressions() {
    struct Test<'a> {
        input: &'a str,
        operator: &'a str,
        right_value: Expression,
    }

    impl<'a> Test<'a> {
        pub fn new(input: &'a str, operator: &'a str, right_value: Expression) -> Self {
            Test {
                input,
                operator,
                right_value,
            }
        }
    }

    let tests = vec![
        Test::new(
            "!5",
            "!",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "!true",
            "!",
            Expression::Bool(Boolean::new(
                &Token::new(TokenType::True, "true".to_owned()),
                true,
            )),
        ),
        Test::new(
            "-variable",
            "-",
            Expression::Identifier(Identifier::new(
                &Token::new(TokenType::Identifier, "variable".to_owned()),
                "variable",
            )),
        ),
    ];

    for test in tests.iter() {
        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("failed to parse program");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(expression_statement) => match &expression_statement.expression {
                Some(Expression::Prefix(prefix)) => {
                    assert_eq!(prefix.operator, test.operator);

                    match &*prefix.right {
                        Some(expression) => assert_eq!(expression, &test.right_value),
                        None => panic!("no right expression found"),
                    };
                }
                Some(_) | None => panic!("no prefix expression found"),
            },
            _ => panic!("no statement found"),
        };
    }
}

#[test]
fn test_infix_expressions() {
    struct Test<'a> {
        input: &'a str,
        left_value: Expression,
        operator: &'a str,
        right_value: Expression,
    }

    impl<'a> Test<'a> {
        pub fn new(
            input: &'a str,
            left_value: Expression,
            operator: &'a str,
            right_value: Expression,
        ) -> Self {
            Test {
                input,
                left_value,
                operator,
                right_value,
            }
        }
    }

    let tests: Vec<Test> = vec![
        Test::new(
            "5 + 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "+",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 - 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "-",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 * 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "*",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 / 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "/",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 > 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            ">",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 < 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "<",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 == 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "==",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "5 != 5",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
            "!=",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "5".to_owned()),
                "5",
            )),
        ),
        Test::new(
            "true == true",
            Expression::Bool(Boolean::new(
                &Token::new(TokenType::True, "true".to_owned()),
                true,
            )),
            "==",
            Expression::Bool(Boolean::new(
                &Token::new(TokenType::True, "true".to_owned()),
                true,
            )),
        ),
        Test::new(
            "true != 100",
            Expression::Bool(Boolean::new(
                &Token::new(TokenType::True, "true".to_owned()),
                true,
            )),
            "!=",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "100".to_owned()),
                "100",
            )),
        ),
        Test::new(
            "variable != 100",
            Expression::Identifier(Identifier::new(
                &Token::new(TokenType::Identifier, "variable".to_owned()),
                "variable",
            )),
            "!=",
            Expression::Int(IntegerLiteral::new(
                &Token::new(TokenType::Int, "100".to_owned()),
                "100",
            )),
        ),
    ];

    for test in tests.iter() {
        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("failed to parse program");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(expression_statement) => match &expression_statement.expression {
                Some(Expression::Infix(infix)) => {
                    match &*infix.left {
                        Some(expression) => assert_eq!(expression, &test.left_value),
                        None => panic!("no left expression found"),
                    };

                    assert_eq!(infix.operator, test.operator);

                    match &*infix.right {
                        Some(expression) => assert_eq!(expression, &test.right_value),
                        None => panic!("no left expression found"),
                    };
                }
                Some(_) | None => panic!("expected infix expression"),
            },
            _ => panic!("expected infix expression"),
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct Test<'a> {
        input: &'a str,
        expected: &'a str,
    }

    let tests = vec![
        Test {
            input: "true",
            expected: "true",
        },
        Test {
            input: "false",
            expected: "false",
        },
        Test {
            input: "-1 * 2 + 3",
            expected: "(((-1) * 2) + 3)",
        },
        Test {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        Test {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
        Test {
            input: "-3 * 5 != true / -100",
            expected: "(((-3) * 5) != (true / (-100)))",
        },
        Test {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)",
        },
        Test {
            input: "(2 + 3) + 1",
            expected: "((2 + 3) + 1)",
        },
        Test {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d)",
        },
        Test {
            input: "a * [1, 2, 3, 4][b * c] * d",
            expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        Test {
            input: "add(a * b[2], b[1], 2 * [1, 2][1])",
            expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    ];

    for test in tests.iter() {
        let lexer = Lexer::new(test.input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("parsing failed");

        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }

        assert_eq!(test.expected, program.to_string());
    }
}

#[test]
fn test_if_expressions() {
    let input = "if (x < y) { x }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(statement) => {
            assert_eq!(statement.token.token_type, TokenType::If);

            match &statement.expression {
                Some(Expression::If(if_expression)) => {
                    assert_eq!(if_expression.token.token_type, TokenType::If);

                    if let Some(consequence) = &if_expression.consequence {
                        assert_eq!(consequence.token.token_type, TokenType::LBrace);

                        match &consequence.statements[0] {
                            Statement::Expression(statement) => match &statement.expression {
                                Some(Expression::Identifier(ident)) => {
                                    assert_eq!(ident.token.token_type, TokenType::Identifier);
                                    assert_eq!(ident.value, "x");
                                }
                                Some(_) | None => panic!("not an identifier expression"),
                            },
                            _ => panic!("not an expression statement"),
                        }
                    }

                    assert!(if_expression.alternative.is_none());
                }
                _ => panic!("not an if expression"),
            }
        }
        _ => panic!("not an expression statement"),
    }
}

#[test]
fn test_if_else_expressions() {
    let input = "if (x < y) { return x; } else { return y; }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(statement) => {
            assert_eq!(statement.token.token_type, TokenType::If);

            match &statement.expression {
                Some(Expression::If(if_expression)) => {
                    assert_eq!(if_expression.token.token_type, TokenType::If);

                    if let Some(consequence) = &if_expression.consequence {
                        assert_eq!(consequence.token.token_type, TokenType::LBrace);

                        match &consequence.statements[0] {
                            Statement::Return(statement) => match &statement.return_value {
                                Some(Expression::Identifier(ident)) => {
                                    assert_eq!(ident.token.token_type, TokenType::Identifier);
                                    assert_eq!(ident.value, "x");
                                }
                                Some(_) | None => panic!("not an identifier expression"),
                            },
                            _ => panic!("not if expression"),
                        }
                    }

                    assert!(if_expression.alternative.is_some());

                    if let Some(alternative) = &if_expression.alternative {
                        assert_eq!(alternative.token.token_type, TokenType::LBrace);

                        match &alternative.statements[0] {
                            Statement::Return(statement) => match &statement.return_value {
                                Some(Expression::Identifier(ident)) => {
                                    assert_eq!(ident.token.token_type, TokenType::Identifier);
                                    assert_eq!(ident.value, "y");
                                }
                                Some(_) | None => panic!("not an identifier expression"),
                            },
                            _ => panic!("not an expression statement"),
                        }
                    }
                }
                _ => panic!("not an if expression"),
            }
        }
        _ => panic!("not an expression statement"),
    }
}

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(statement) => {
            assert_eq!(statement.token.token_type, TokenType::Function);

            match &statement.expression {
                Some(Expression::Function(function_expression)) => {
                    assert_eq!(function_expression.token.token_type, TokenType::Function);

                    assert_eq!(
                        function_expression.parameters[0].token,
                        Token::new(TokenType::Identifier, "x".to_owned())
                    );
                    assert_eq!(
                        function_expression.parameters[1].token,
                        Token::new(TokenType::Identifier, "y".to_owned())
                    );

                    if let Some(body) = &function_expression.body {
                        assert_eq!(body.token.token_type, TokenType::LBrace);

                        match &body.statements[0] {
                            Statement::Expression(statement) => match &statement.expression {
                                Some(Expression::Infix(infix)) => {
                                    assert_eq!(infix.token.token_type, TokenType::Plus);
                                    assert_eq!(infix.operator, "+");

                                    match &*infix.left {
                                        Some(Expression::Identifier(ident)) => {
                                            assert_eq!(ident.value, "x");
                                            assert_eq!(
                                                ident.token.token_type,
                                                TokenType::Identifier
                                            );
                                        }
                                        Some(_) => {
                                            panic!("Expected identifier expression. Got None.")
                                        }
                                        None => panic!("Expected integer expression. Got None."),
                                    }

                                    match &*infix.right {
                                        Some(Expression::Identifier(ident)) => {
                                            assert_eq!(ident.value, "y");
                                            assert_eq!(
                                                ident.token.token_type,
                                                TokenType::Identifier
                                            );
                                        }
                                        Some(_) => {
                                            panic!("Expected identifier expression. Got None.")
                                        }
                                        None => panic!("Expected integer expression. Got None."),
                                    }
                                }
                                Some(_) | None => panic!("not an identifier expression"),
                            },
                            _ => panic!("not an expression statement"),
                        }
                    }
                }
                _ => panic!("not an if expression"),
            }
        }
        _ => panic!("not an expression statement"),
    }
}

#[test]
fn test_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("parsing failed");

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(statement) => {
            assert_eq!(statement.token.token_type, TokenType::Identifier);

            match &statement.expression {
                Some(Expression::Call(call_expression)) => {
                    assert_eq!(call_expression.token.token_type, TokenType::LParen);
                    assert_eq!(call_expression.function.token_literal(), "add");

                    assert_eq!(call_expression.arguments.len(), 3);
                }
                _ => panic!("not a call expression"),
            }
        }
        _ => panic!("not an expression statement"),
    }
}

#[test]
fn test_string_literal() {
    let input = r#""hello world""#;
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");

    let stmt = &program.statements[0];

    if let Statement::Expression(expression) = stmt {
        if let Some(Expression::Str(string_literal)) = &expression.expression {
            assert_eq!(string_literal.value, "hello world");
        } else {
            panic!("not a string expression")
        }
    } else {
        panic!("not an expression");
    }
}

#[test]
fn test_array_literal() {
    let input = "[1, 1+2, 1*2]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");

    let stmt = &program.statements[0];

    if let Statement::Expression(expression) = stmt {
        if let Some(Expression::Array(array)) = &expression.expression {
            assert_eq!(array.elements.len(), 3);
            assert_eq!(
                array.elements[0],
                Expression::Int(IntegerLiteral::new(
                    &Token::new(TokenType::Int, "1".to_string()),
                    "1"
                ))
            );
            assert_eq!(
                array.elements[1],
                Expression::Infix(Infix::new(
                    &Token::new(TokenType::Plus, "+".to_string()),
                    Some(Expression::Int(IntegerLiteral::new(
                        &Token::new(TokenType::Int, "1".to_string()),
                        "1"
                    ))),
                    "+",
                    Some(Expression::Int(IntegerLiteral::new(
                        &Token::new(TokenType::Int, "2".to_string()),
                        "2"
                    ))),
                ))
            );
            assert_eq!(
                array.elements[2],
                Expression::Infix(Infix::new(
                    &Token::new(TokenType::Asterisk, "*".to_string()),
                    Some(Expression::Int(IntegerLiteral::new(
                        &Token::new(TokenType::Int, "1".to_string()),
                        "1"
                    ))),
                    "*",
                    Some(Expression::Int(IntegerLiteral::new(
                        &Token::new(TokenType::Int, "2".to_string()),
                        "2"
                    ))),
                ))
            );
        } else {
            panic!("not an array expression");
        }
    } else {
        panic!("not an expression");
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "[1][0]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("failed to parse program");
    let stmt = &program.statements[0];

    if let Statement::Expression(expression) = stmt {
        if let Some(Expression::Index(index_expression)) = &expression.expression {
            if let Expression::Array(array) = &*index_expression.left {
                assert_eq!(array.elements.len(), 1);
                assert_eq!(
                    array.elements[0],
                    Expression::Int(IntegerLiteral::new(
                        &Token::new(TokenType::Int, "1".to_string()),
                        "1"
                    ))
                );
            } else {
                panic!("Not an array expression.");
            }

            if let Expression::Int(integer) = &*index_expression.index {
                assert_eq!(
                    integer,
                    &IntegerLiteral::new(&Token::new(TokenType::Int, "0".to_string()), "0")
                )
            } else {
                panic!("Not an integer expression.");
            }
        }
    } else {
        panic!("Not an expression");
    }
}
