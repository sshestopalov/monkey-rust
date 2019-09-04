use std::io::{self, Write};

use crate::evaluator::{object::Object, Evaluator};
use crate::lexer::Lexer;
use crate::parser::Parser;

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start() {
    let mut buffer = String::new();

    println!(
        "{}",
        "Hello mrnugget! This is the Monkey programming language!"
    );
    println!("{}", "Feel free to type in commands.");

    let mut evaluator = Evaluator::new();

    loop {
        buffer.clear();
        print!("{}", PROMPT);

        io::stdout().flush().expect("Error flushing stdout");
        let bytes = io::stdin()
            .read_line(&mut buffer)
            .expect("Error reading from STDIN");

        if bytes == 0 {
            continue;
        }

        let lexer = Lexer::new(&buffer);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Parsing failed");

        if !parser.errors.is_empty() {
            println!("{}", MONKEY_FACE);
            println!("Whoops! We ran into some monkey business here!");
            println!("parse errors:");
            for e in parser.errors.iter() {
                println!("\t{}", e);
            }

            continue;
        }

        if let Some(evaluated) = evaluator.eval(&program) {
            if let Object::Error(_) = &evaluated {
                println!("Whoops! We ran into some monkey business here!");
                println!("evaluation error:");
            }

            println!("{}", evaluated);
        }
    }
}
