mod highlighter;

use std::error::Error;
use std::io::{self, Write};

pub fn run() -> Result<(), Box<dyn Error>> {
    run_with_permissions(crate::runtime::Permissions::default())
}

pub fn run_with_permissions(
    permissions: crate::runtime::Permissions,
) -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut line = String::new();
    let mut checker = crate::typechecker::TypeChecker::new();
    let mut runtime =
        crate::runtime::Runtime::with_permissions(permissions).with_source_label("<repl>");

    println!("Rask REPL (Phase 3 runtime mode)");
    println!("Type 'exit' or 'quit' to stop.");

    loop {
        print!("rask> ");
        io::stdout().flush()?;

        line.clear();
        if stdin.read_line(&mut line)? == 0 {
            break;
        }

        let input = line.trim();
        if input.eq_ignore_ascii_case("exit") || input.eq_ignore_ascii_case("quit") {
            break;
        }
        if input.is_empty() {
            continue;
        }

        let input = highlighter::normalize_line(input);
        match crate::lexer::lex(input.as_str()) {
            Ok(tokens) => {
                let mut parser = crate::parser::Parser::new(tokens);
                match parser.parse_program() {
                    Ok(program) => match checker.check_program(&program) {
                        Ok(_) => match runtime.run_program(&program) {
                            Ok(value) => {
                                if !matches!(value, crate::runtime::value::Value::Nil) {
                                    println!("{}", value);
                                }
                            }
                            Err(err) => eprintln!("{}", err),
                        },
                        Err(errors) => {
                            for rendered in crate::errors::pretty::format_type_errors("<repl>", &errors) {
                                eprintln!("{}", rendered);
                            }
                        }
                    },
                    Err(err) => eprintln!(
                        "{}",
                        crate::errors::pretty::format_parse_error("<repl>", input.as_str(), &err)
                    ),
                }
            }
            Err(err) => eprintln!(
                "lex error [LEX0001]: {}\n--> <repl>:{}:{}\ndocs: https://rask-lang.dev/errors/LEX0001",
                err.message, err.line, err.column
            ),
        }
    }

    Ok(())
}
