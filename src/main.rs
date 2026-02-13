use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();

    if let Some(result) = maybe_run_subcommand(&args) {
        if let Err(err) = result {
            eprintln!("{}", err);
            std::process::exit(1);
        }
        return;
    }

    match parse_cli(args) {
        Ok((script_path, permissions)) => {
            if let Some(path) = script_path {
                match fs::read_to_string(&path) {
                    Ok(source) => {
                        if let Err(err) = run_source_file(&path, &source, permissions) {
                            eprintln!("{}", err);
                            std::process::exit(1);
                        }
                    }
                    Err(err) => {
                        eprintln!("failed to read '{}': {}", path, err);
                        std::process::exit(1);
                    }
                }
            } else if let Err(err) = rask::repl::run_with_permissions(permissions) {
                eprintln!("repl error: {}", err);
                std::process::exit(1);
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

fn maybe_run_subcommand(args: &[String]) -> Option<Result<(), String>> {
    let command = args.first()?.as_str();
    match command {
        "docs" => Some(run_docs_command(args)),
        "fmt" => Some(run_fmt_command(args)),
        "check" => Some(run_check_command(args)),
        "test" => Some(run_test_command(args)),
        _ => None,
    }
}

fn run_docs_command(args: &[String]) -> Result<(), String> {
    let mut output = PathBuf::from("docs/stdlib_reference.md");
    for arg in args.iter().skip(1) {
        if let Some(value) = arg.strip_prefix("--out=") {
            output = PathBuf::from(value);
            continue;
        }
        return Err(format!(
            "unknown docs option '{}'; supported: --out=<path>",
            arg
        ));
    }

    rask::docgen::generate_stdlib_reference(&output)
        .map(|report| {
            println!(
                "generated stdlib docs: {} ({} module files)",
                report.output_path.display(),
                report.module_count
            );
        })
        .map_err(|err| err.to_string())
}

fn run_fmt_command(args: &[String]) -> Result<(), String> {
    let mut check_only = false;
    let mut file = None;
    for arg in args.iter().skip(1) {
        if arg == "--check" {
            check_only = true;
            continue;
        }
        if file.is_none() {
            file = Some(arg.clone());
            continue;
        }
        return Err(format!("unknown fmt option '{}'", arg));
    }

    let Some(file) = file else {
        return Err("fmt usage: rask fmt [--check] <file>".to_string());
    };

    let source =
        fs::read_to_string(&file).map_err(|err| format!("failed to read '{}': {}", file, err))?;
    let program = parse_and_typecheck(&source, &file)?;
    let formatted = rask::formatter::format_program(&program);

    if check_only {
        if source == formatted {
            println!("fmt check passed: {}", file);
            Ok(())
        } else {
            Err(format!(
                "formatting differs for '{}'; run `rask fmt {}`",
                file, file
            ))
        }
    } else {
        fs::write(&file, formatted)
            .map_err(|err| format!("failed to write '{}': {}", file, err))?;
        println!("formatted {}", file);
        Ok(())
    }
}

fn run_check_command(args: &[String]) -> Result<(), String> {
    let Some(file) = args.get(1) else {
        return Err("check usage: rask check <file>".to_string());
    };
    let source =
        fs::read_to_string(file).map_err(|err| format!("failed to read '{}': {}", file, err))?;
    let program = parse_and_typecheck(&source, file)?;

    let warnings = rask::lint::lint_program(&program);
    if warnings.is_empty() {
        println!("check passed: {}", file);
    } else {
        for warning in &warnings {
            println!(
                "{}",
                rask::errors::pretty::format_lint_warning(warning, file)
            );
        }
        println!("check passed with {} warning(s): {}", warnings.len(), file);
    }
    Ok(())
}

fn run_test_command(args: &[String]) -> Result<(), String> {
    let (script_path, permissions) = parse_cli(args.iter().skip(1).cloned().collect())?;
    let Some(path) = script_path else {
        return Err("test usage: rask test <file> [permissions]".to_string());
    };

    let source =
        fs::read_to_string(&path).map_err(|err| format!("failed to read '{}': {}", path, err))?;
    let program = parse_and_typecheck(&source, &path)?;

    let mut runtime =
        rask::runtime::Runtime::with_permissions(permissions).with_source_label(path.clone());
    let report = runtime.run_tests(&program);

    for case in &report.results {
        if case.passed {
            println!("PASS {}", case.name);
        } else if let Some(err) = &case.error {
            println!("FAIL {}\n{}", case.name, err);
        } else {
            println!("FAIL {}", case.name);
        }
    }

    if report.failed == 0 {
        println!("test result: ok. {} passed; 0 failed", report.passed);
        Ok(())
    } else {
        Err(format!(
            "test result: FAILED. {} passed; {} failed",
            report.passed, report.failed
        ))
    }
}

fn parse_cli(args: Vec<String>) -> Result<(Option<String>, rask::runtime::Permissions), String> {
    let mut script_path: Option<String> = None;
    let mut permissions = rask::runtime::Permissions::default();

    for arg in args {
        if let Some(value) = arg.strip_prefix("--allow-read=") {
            for path in split_csv(value) {
                permissions.allow_read.push(normalize_cli_path(path));
            }
            continue;
        }

        if let Some(value) = arg.strip_prefix("--allow-write=") {
            for path in split_csv(value) {
                permissions.allow_write.push(normalize_cli_path(path));
            }
            continue;
        }

        if let Some(value) = arg.strip_prefix("--allow-net=") {
            permissions.allow_net.extend(split_csv(value));
            continue;
        }

        if arg == "--allow-env" {
            permissions.allow_env = true;
            continue;
        }

        if arg == "--allow-all" {
            permissions = rask::runtime::Permissions::allow_all();
            continue;
        }

        if arg.starts_with("--") {
            return Err(format!("unknown flag '{}'", arg));
        }

        if script_path.is_none() {
            script_path = Some(arg);
        } else {
            return Err("multiple script paths provided".to_string());
        }
    }

    Ok((script_path, permissions))
}

fn split_csv(value: &str) -> Vec<String> {
    value
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn normalize_cli_path(path: String) -> PathBuf {
    let candidate = PathBuf::from(&path);
    if candidate.is_absolute() {
        normalize_pathbuf(candidate)
    } else {
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        normalize_pathbuf(cwd.join(candidate))
    }
}

fn normalize_pathbuf(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            _ => normalized.push(component.as_os_str()),
        }
    }
    normalized
}

fn parse_and_typecheck(
    source: &str,
    source_label: &str,
) -> Result<rask::parser::ast::Program, String> {
    let tokens = rask::lexer::lex(source).map_err(|err| {
        format!(
            "lex error [LEX0001]: {}\n--> {}:{}:{}\ndocs: https://rask-lang.dev/errors/LEX0001",
            err.message, source_label, err.line, err.column
        )
    })?;

    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser
        .parse_program()
        .map_err(|err| rask::errors::pretty::format_parse_error(source_label, source, &err))?;

    let mut checker = rask::typechecker::TypeChecker::new();
    checker.check_program(&program).map_err(|errors| {
        rask::errors::pretty::format_type_errors(source_label, &errors).join("\n\n")
    })?;

    Ok(program)
}

fn run_source_file(
    path: &str,
    source: &str,
    permissions: rask::runtime::Permissions,
) -> Result<(), String> {
    let program = parse_and_typecheck(source, path)?;
    let mut runtime = rask::runtime::Runtime::with_permissions(permissions).with_source_label(path);
    let value = runtime
        .run_program(&program)
        .map_err(|err| err.to_string())?;
    if !matches!(value, rask::runtime::value::Value::Nil) {
        println!("{}", value);
    }
    Ok(())
}
