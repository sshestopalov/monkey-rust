use super::{object::Object, Evaluator};
use crate::lexer::Lexer;
use crate::parser::Parser;

fn test_eval(input: &str) -> Option<Object> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let mut evaluator = Evaluator::new();

    if let Ok(program) = parser.parse_program() {
        return evaluator.eval(&program);
    } else {
        panic!("failed to parse program");
    }
}

fn compare(input: &str, expected: Object) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let mut evaluator = Evaluator::new();

    if let Ok(program) = parser.parse_program() {
        let evaluated = evaluator.eval(&program).unwrap();

        assert_eq!(evaluated, expected);
    } else {
        panic!("failed to parse program");
    }
}

#[test]
fn test_eval_integer_expression() {
    compare("5", Object::Integer(5));
    compare("10", Object::Integer(10));
    compare("-5", Object::Integer(-5));
    compare("-10", Object::Integer(-10));
    compare("5 + 5 + 5 + 5 - 10", Object::Integer(10));
    compare("2 * 2 * 2 * 2 * 2", Object::Integer(32));
    compare("-50 + 100 + -50", Object::Integer(0));
    compare("5 * 2 + 10", Object::Integer(20));
    compare("5 + 2 * 10", Object::Integer(25));
    compare("20 + 2 * -10", Object::Integer(0));
    compare("50 / 2 * 2 + 10", Object::Integer(60));
    compare("2 * (5 + 10)", Object::Integer(30));
    compare("3 * (3 * 3) + 10", Object::Integer(37));
    compare("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50));
}

#[test]
fn test_eval_boolean_expression() {
    compare("true", Object::Boolean(true));
    compare("false", Object::Boolean(false));
    compare("1 < 2", Object::Boolean(true));
    compare("1 > 2", Object::Boolean(false));
    compare("1 < 1", Object::Boolean(false));
    compare("1 > 1", Object::Boolean(false));
    compare("1 == 1", Object::Boolean(true));
    compare("1 != 1", Object::Boolean(false));
    compare("1 == 2", Object::Boolean(false));
    compare("1 != 2", Object::Boolean(true));
    compare("true == true", Object::Boolean(true));
    compare("false == false", Object::Boolean(true));
    compare("true == false", Object::Boolean(false));
    compare("true != false", Object::Boolean(true));
    compare("false != true", Object::Boolean(true));
    compare("(1 < 2) == true", Object::Boolean(true));
    compare("(1 < 2) == false", Object::Boolean(false));
    compare("(1 > 2) == true", Object::Boolean(false));
    compare("(1 > 2) == false", Object::Boolean(true));
}

#[test]
fn test_bang_operator() {
    use Object::*;

    compare("!true", Boolean(false));
    compare("!false", Boolean(true));
    compare("!5", Boolean(false));
    compare("!!true", Boolean(true));
    compare("!!false", Boolean(false));
    compare("!!5", Boolean(true));
}

#[test]
fn test_if_else_expressions() {
    compare("if (true) { 10 }", Object::Integer(10));
    compare("if (false) { 10 }", Object::Null);
    compare("if (1) { 10 }", Object::Integer(10));
    compare("if (1 < 2) { 10 }", Object::Integer(10));
    compare("if (1 > 2) { 10 }", Object::Null);
    compare("if (1 > 2) { 10 } else { 20 }", Object::Integer(20));
    compare("if (1 < 2) { 10 } else { 20 }", Object::Integer(10));
}

#[test]
fn test_eval_return_statements() {
    compare(
        "return 10",
        Object::ReturnValue(Box::new(Object::Integer(10))),
    );
    compare(
        "return 10; 9;",
        Object::ReturnValue(Box::new(Object::Integer(10))),
    );
    compare(
        "return 2 * 5; 9;",
        Object::ReturnValue(Box::new(Object::Integer(10))),
    );
    compare(
        "9; return 2 * 5; 9;",
        Object::ReturnValue(Box::new(Object::Integer(10))),
    );
    compare(
        "if (true) {
    if (5 == 5) {
        return 10;
    }

    return 1;
}
",
        Object::ReturnValue(Box::new(Object::Integer(10))),
    );
}

#[test]
fn test_error_handling() {
    compare(
        "5 + true",
        Object::Error(format!("{}", "type mismatch: INTEGER + BOOLEAN")),
    );
    compare(
        "5 + true; 5;",
        Object::Error(format!("{}", "type mismatch: INTEGER + BOOLEAN")),
    );
    compare(
        "-true",
        Object::Error(format!("{}", "unknown operator: -BOOLEAN")),
    );
    compare(
        "true + false;",
        Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")),
    );
    compare(
        "5; true + false; 5",
        Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")),
    );
    compare(
        "if (10 > 1) { true + false }",
        Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")),
    );
    compare(
        "
if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }

    return 1;
}
",
        Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")),
    );
    compare(
        "foobar",
        Object::Error(format!("{}", "identifier not found: foobar")),
    );
    compare(
        r#""hello" - "world""#,
        Object::Error(format!("{}", "unknown operator: STRING - STRING")),
    );
}

#[test]
fn test_eval_let_statements() {
    compare("let a = 5; a;", Object::Integer(5));
    compare("let a = 5 * 5; a;", Object::Integer(25));
    compare("let a = 5; let b = a; b;", Object::Integer(5));
    compare(
        "let a = 5; let b = a; let c = a + b + 5; c;",
        Object::Integer(15),
    );
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";
    let evaluated = test_eval(input).expect("failed to evaluate");

    if let Object::Function(parameters, body, _) = evaluated {
        assert_eq!(parameters.len(), 1);
        assert_eq!(parameters[0].to_string(), "x");

        let expected_body = "(x + 2)";

        assert_eq!(body.to_string(), expected_body);
    } else {
        panic!("not a function object: {}", evaluated.object_type());
    }
}

#[test]
fn test_eval_function() {
    compare(
        "let identity = fn(x) { x; }; identity(5)",
        Object::Integer(5),
    );
    compare(
        "let identity = fn(x) { x; }; identity(5)",
        Object::Integer(5),
    );
    compare(
        "let double = fn(x) { x * 2 }; double(5)",
        Object::Integer(10),
    );
    compare(
        "let add = fn(x, y) { x + y }; add(5, 5);",
        Object::Integer(10),
    );
    compare(
        "let add = fn(x, y) { x + y }; add(5, 5);",
        Object::Integer(10),
    );
    compare(
        "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));",
        Object::Integer(20),
    );
    compare("fn(x) { x; }(5)", Object::Integer(5));
}

#[test]
fn test_eval_string_expressions() {
    compare(r#""hello world""#, Object::Str(String::from("hello world")));
    compare(
        r#""hello \"world\"""#,
        Object::Str(String::from("hello \"world\"")),
    );
}

#[test]
fn test_builtin_functions() {
    compare(r#"len("")"#, Object::Integer(0));
    compare(r#"len("four")"#, Object::Integer(4));
    compare(r#"len("hello world")"#, Object::Integer(11));
    compare(
        r#"len(1)"#,
        Object::Error(format!(
            "{}",
            "Argument to 'len' not supported, got INTEGER"
        )),
    );
    compare(
        r#"len("one", "two")"#,
        Object::Error(format!(
            "{}",
            "Wrong number of arguments supplied. Expected 1, found 2."
        )),
    );
    compare("len([1,2])", Object::Integer(2));
}

#[test]
fn test_eval_array() {
    compare(
        "[1, 2, 3, 4]",
        Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
            Object::Integer(4),
        ]),
    );
}

#[test]
fn test_eval_index_expressions() {
    compare("[1][0]", Object::Integer(1));
    compare("[1 + 1, 2 * 5][1]", Object::Integer(10));
    compare("[len(\"four\")][0]", Object::Integer(4));
}
