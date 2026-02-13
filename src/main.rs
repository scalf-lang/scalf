use std::env;
use std::fs;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        match fs::read_to_string(&path) {
            Ok(source) => run_source(&source),
            Err(err) => eprintln!("failed to read '{}': {}", path, err),
        }
        return;
    }

    if let Err(err) = rask::repl::run() {
        eprintln!("repl error: {}", err);
    }
}

fn run_source(source: &str) {
    match rask::lexer::lex(source) {
        Ok(tokens) => {
            let mut parser = rask::parser::Parser::new(tokens);
            match parser.parse_program() {
                Ok(program) => {
                    let mut checker = rask::typechecker::TypeChecker::new();
                    match checker.check_program(&program) {
                        Ok(_) => {
                            let mut runtime = rask::runtime::Runtime::new();
                            match runtime.run_program(&program) {
                                Ok(value) => {
                                    if !matches!(value, rask::runtime::value::Value::Nil) {
                                        println!("{}", value);
                                    }
                                }
                                Err(err) => eprintln!("{}", err),
                            }
                        }
                        Err(errors) => {
                            for error in errors {
                                eprintln!("{}", error);
                            }
                        }
                    }
                }
                Err(err) => eprintln!("{}", err),
            }
        }
        Err(err) => eprintln!("{}", err),
    }
}
