use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

struct RunCli {
    script_path: Option<String>,
    permissions: rask::runtime::Permissions,
    use_vm: bool,
}

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
        Ok(run_cli) => {
            if let Some(path) = run_cli.script_path {
                match fs::read_to_string(&path) {
                    Ok(source) => {
                        if let Err(err) =
                            run_source_file(&path, &source, run_cli.permissions, run_cli.use_vm)
                        {
                            eprintln!("{}", err);
                            std::process::exit(1);
                        }
                    }
                    Err(err) => {
                        eprintln!("failed to read '{}': {}", path, err);
                        std::process::exit(1);
                    }
                }
            } else if let Some((source_label, source)) = embedded_script() {
                if let Err(err) =
                    run_source_file(source_label, source, run_cli.permissions, run_cli.use_vm)
                {
                    eprintln!("{}", err);
                    std::process::exit(1);
                }
            } else if run_cli.use_vm {
                eprintln!("--vm requires a script path");
                std::process::exit(1);
            } else if let Err(err) = rask::repl::run_with_permissions(run_cli.permissions) {
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
        "build" => Some(run_build_command(args)),
        "startup" => Some(run_startup_command(args)),
        _ => None,
    }
}

fn run_startup_command(args: &[String]) -> Result<(), String> {
    let mut script_path: Option<String> = None;
    let mut iterations: usize = 25;
    let mut budget_ms: Option<f64> = Some(10.0);
    let mut use_vm = false;

    for arg in args.iter().skip(1) {
        if let Some(value) = arg.strip_prefix("--iterations=") {
            iterations = value
                .parse::<usize>()
                .map_err(|_| format!("invalid --iterations value '{}'", value))?;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--budget-ms=") {
            budget_ms = Some(
                value
                    .parse::<f64>()
                    .map_err(|_| format!("invalid --budget-ms value '{}'", value))?,
            );
            continue;
        }
        if arg == "--no-budget" {
            budget_ms = None;
            continue;
        }
        if arg == "--vm" {
            use_vm = true;
            continue;
        }
        if arg.starts_with("--") {
            return Err(format!(
                "unknown startup option '{}'; supported: --iterations=<n>, --budget-ms=<ms>, --no-budget, --vm",
                arg
            ));
        }
        if script_path.is_none() {
            script_path = Some(arg.clone());
            continue;
        }
        return Err("startup usage: rask startup <file> [--iterations=<n>] [--budget-ms=<ms>|--no-budget] [--vm]".to_string());
    }

    let Some(script_path) = script_path else {
        return Err(
            "startup usage: rask startup <file> [--iterations=<n>] [--budget-ms=<ms>|--no-budget] [--vm]"
                .to_string(),
        );
    };
    if iterations == 0 {
        return Err("--iterations must be >= 1".to_string());
    }

    let source = fs::read_to_string(&script_path)
        .map_err(|err| format!("failed to read '{}': {}", script_path, err))?;

    let mut samples_ms = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let started = Instant::now();
        let program = parse_and_typecheck(&source, &script_path)?;
        if use_vm {
            let _ = rask::vm::compile_program(&program).map_err(|err| err.to_string())?;
        }
        let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;
        samples_ms.push(elapsed_ms);
    }

    let min = samples_ms
        .iter()
        .copied()
        .fold(f64::INFINITY, |acc, value| acc.min(value));
    let max = samples_ms
        .iter()
        .copied()
        .fold(0.0_f64, |acc, value| acc.max(value));
    let avg = samples_ms.iter().sum::<f64>() / samples_ms.len() as f64;

    println!(
        "startup benchmark ({}, iterations={}): min={:.3}ms avg={:.3}ms max={:.3}ms",
        if use_vm {
            "lex+parse+typecheck+vm-compile"
        } else {
            "lex+parse+typecheck"
        },
        iterations,
        min,
        avg,
        max
    );

    if let Some(budget) = budget_ms {
        if min <= budget {
            println!("startup budget met: {:.3}ms <= {:.3}ms", min, budget);
            Ok(())
        } else {
            Err(format!(
                "startup budget not met: {:.3}ms > {:.3}ms (use --no-budget to skip failure)",
                min, budget
            ))
        }
    } else {
        Ok(())
    }
}

fn run_build_command(args: &[String]) -> Result<(), String> {
    let mut script_path: Option<String> = None;
    let mut target: Option<String> = None;
    let mut out: Option<PathBuf> = None;
    let mut release = true;

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg == "--debug" {
            release = false;
            i += 1;
            continue;
        }
        if arg == "--target" {
            i += 1;
            let Some(value) = args.get(i) else {
                return Err("build option '--target' requires a value".to_string());
            };
            target = Some(value.clone());
            i += 1;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--target=") {
            target = Some(value.to_string());
            i += 1;
            continue;
        }
        if arg == "--out" {
            i += 1;
            let Some(value) = args.get(i) else {
                return Err("build option '--out' requires a value".to_string());
            };
            out = Some(PathBuf::from(value));
            i += 1;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--out=") {
            out = Some(PathBuf::from(value));
            i += 1;
            continue;
        }
        if arg.starts_with("--") {
            return Err(format!(
                "unknown build option '{}'; supported: --target=<triple|alias>, --out=<path>, --debug",
                arg
            ));
        }
        if script_path.is_none() {
            script_path = Some(arg.clone());
            i += 1;
            continue;
        }
        return Err(
            "build usage: rask build <file> [--target=<alias|triple>] [--out=<path>] [--debug]"
                .to_string(),
        );
    }

    let Some(script_path) = script_path else {
        return Err(
            "build usage: rask build <file> [--target=<alias|triple>] [--out=<path>] [--debug]"
                .to_string(),
        );
    };

    let source = fs::read_to_string(&script_path)
        .map_err(|err| format!("failed to read '{}': {}", script_path, err))?;
    let program = parse_and_typecheck(&source, &script_path)?;
    let required_features = script_required_cargo_features(&program);

    let resolved_target = target.as_deref().map(resolve_target_alias).transpose()?;
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let caller_cwd = env::current_dir()
        .map_err(|err| format!("failed to determine current directory: {}", err))?;
    let build_target_dir = match env::var("CARGO_TARGET_DIR") {
        Ok(value) => {
            let configured = PathBuf::from(value);
            if configured.is_absolute() {
                configured
            } else {
                normalize_pathbuf(caller_cwd.join(configured))
            }
        }
        Err(_) => default_temp_embedded_build_dir(),
    };
    let manifest_path = manifest_dir.join("Cargo.toml");

    let mut command = Command::new("cargo");
    command
        .arg("build")
        .arg("--bin")
        .arg("rask")
        .arg("--manifest-path")
        .arg(&manifest_path)
        .current_dir(&caller_cwd)
        .env("CARGO_TARGET_DIR", &build_target_dir)
        .env("RASK_EMBEDDED_SCRIPT", &source)
        .env("RASK_EMBEDDED_SOURCE_LABEL", &script_path);
    command.arg("--no-default-features");
    if !required_features.is_empty() {
        command.arg("--features").arg(required_features.join(","));
    }
    if release {
        command.arg("--release");
    }
    if let Some(target) = &resolved_target {
        command.arg("--target").arg(target);
    }

    let status = command
        .status()
        .map_err(|err| format!("failed to run cargo build: {}", err))?;
    if !status.success() {
        return Err("cargo build failed for embedded script".to_string());
    }

    let output_is_windows = is_windows_target(resolved_target.as_deref());
    let built_binary = locate_built_binary(
        &build_target_dir,
        resolved_target.as_deref(),
        release,
        output_is_windows,
    );
    if !built_binary.exists() {
        return Err(format!(
            "build succeeded but binary was not found at '{}'",
            built_binary.display()
        ));
    }

    let output_path =
        out.unwrap_or_else(|| default_build_output_path(&script_path, output_is_windows));
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|err| {
                format!(
                    "failed to create output directory '{}': {}",
                    parent.display(),
                    err
                )
            })?;
        }
    }

    if built_binary != output_path {
        fs::copy(&built_binary, &output_path).map_err(|err| {
            format!(
                "failed to write build output '{}': {}",
                output_path.display(),
                err
            )
        })?;
    }

    let features_label = if required_features.is_empty() {
        "none".to_string()
    } else {
        required_features.join(",")
    };
    println!(
        "built standalone binary: {} (target: {}, features: {})",
        output_path.display(),
        resolved_target.as_deref().unwrap_or("host"),
        features_label
    );
    Ok(())
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
    let run_cli = parse_cli(args.iter().skip(1).cloned().collect())?;
    let Some(path) = run_cli.script_path else {
        return Err("test usage: rask test <file> [permissions]".to_string());
    };
    if run_cli.use_vm {
        return Err("test command does not support --vm yet".to_string());
    }

    let source =
        fs::read_to_string(&path).map_err(|err| format!("failed to read '{}': {}", path, err))?;
    let program = parse_and_typecheck(&source, &path)?;

    let mut runtime = rask::runtime::Runtime::with_permissions(run_cli.permissions)
        .with_source_label(path.clone());
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

fn parse_cli(args: Vec<String>) -> Result<RunCli, String> {
    let mut script_path: Option<String> = None;
    let mut permissions = rask::runtime::Permissions::default();
    let mut use_vm = false;

    for arg in args {
        if arg == "--vm" {
            use_vm = true;
            continue;
        }

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

        if arg == "--prompt-permissions" {
            permissions.prompt_permissions = true;
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

    Ok(RunCli {
        script_path,
        permissions,
        use_vm,
    })
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
    use_vm: bool,
) -> Result<(), String> {
    let program = parse_and_typecheck(source, path)?;
    if use_vm {
        let _ = rask::vm::run_program(&program).map_err(|err| err.to_string())?;
    } else {
        let mut runtime =
            rask::runtime::Runtime::with_permissions(permissions).with_source_label(path);
        let _ = runtime
            .run_program(&program)
            .map_err(|err| err.to_string())?;
    }
    Ok(())
}

fn embedded_script() -> Option<(&'static str, &'static str)> {
    let source = option_env!("RASK_EMBEDDED_SCRIPT")?;
    let label = option_env!("RASK_EMBEDDED_SOURCE_LABEL").unwrap_or("<embedded>");
    Some((label, source))
}

fn resolve_target_alias(target: &str) -> Result<String, String> {
    let resolved = match target {
        "linux-x64" => "x86_64-unknown-linux-gnu",
        "windows-x64" => "x86_64-pc-windows-msvc",
        "macos-arm64" => "aarch64-apple-darwin",
        other => {
            if other.trim().is_empty() || other.chars().any(char::is_whitespace) {
                return Err(format!("invalid target '{}'", target));
            }
            return Ok(other.to_string());
        }
    };
    Ok(resolved.to_string())
}

fn is_windows_target(target: Option<&str>) -> bool {
    if let Some(target) = target {
        target.contains("windows")
    } else {
        cfg!(windows)
    }
}

fn locate_built_binary(
    build_target_dir: &PathBuf,
    target: Option<&str>,
    release: bool,
    is_windows: bool,
) -> PathBuf {
    let mut path = build_target_dir.clone();
    if let Some(target) = target {
        path.push(target);
    }
    path.push(if release { "release" } else { "debug" });
    let binary_name = if is_windows { "rask.exe" } else { "rask" };
    path.push(binary_name);
    path
}

fn default_build_output_path(script_path: &str, is_windows: bool) -> PathBuf {
    let stem = PathBuf::from(script_path)
        .file_stem()
        .and_then(|value| value.to_str())
        .unwrap_or("app")
        .to_string();
    if is_windows {
        PathBuf::from(format!("{}.exe", stem))
    } else {
        PathBuf::from(stem)
    }
}

fn default_temp_embedded_build_dir() -> PathBuf {
    env::temp_dir().join("rask-embedded-build-cache")
}

fn script_required_cargo_features(program: &rask::parser::ast::Program) -> Vec<String> {
    let mut features = Vec::new();
    if program_requires_net(program) {
        features.push("net".to_string());
    }
    features
}

fn program_requires_net(program: &rask::parser::ast::Program) -> bool {
    program.statements.iter().any(stmt_requires_net)
}

fn stmt_requires_net(stmt: &rask::parser::ast::Stmt) -> bool {
    use rask::parser::ast::{Stmt, UseTarget};

    match stmt {
        Stmt::Use { target, .. } => match target {
            UseTarget::Url(_) => true,
            UseTarget::ModulePath(path) => path.last().is_some_and(|part| part == "http"),
        },
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            expr_requires_net(condition)
                || then_branch.iter().any(stmt_requires_net)
                || else_branch
                    .as_ref()
                    .is_some_and(|branch| branch.iter().any(stmt_requires_net))
        }
        Stmt::While { condition, body } => {
            expr_requires_net(condition) || body.iter().any(stmt_requires_net)
        }
        Stmt::For {
            initializer,
            condition,
            increment,
            body,
        } => {
            initializer
                .as_ref()
                .is_some_and(|stmt| stmt_requires_net(stmt))
                || condition.as_ref().is_some_and(expr_requires_net)
                || increment.as_ref().is_some_and(expr_requires_net)
                || body.iter().any(stmt_requires_net)
        }
        Stmt::ForIn { iterable, body, .. } => {
            expr_requires_net(iterable) || body.iter().any(stmt_requires_net)
        }
        Stmt::Test { body, .. } => body.iter().any(stmt_requires_net),
        Stmt::Assert { condition, message } => {
            expr_requires_net(condition) || message.as_ref().is_some_and(expr_requires_net)
        }
        Stmt::VarDecl { initializer, .. } => expr_requires_net(initializer),
        Stmt::DestructureDecl { initializer, .. } => expr_requires_net(initializer),
        Stmt::FunctionDef { body, .. } => body.iter().any(stmt_requires_net),
        Stmt::Return { value } => value.as_ref().is_some_and(expr_requires_net),
        Stmt::Print { expr } | Stmt::Expr(expr) => expr_requires_net(expr),
    }
}

fn expr_requires_net(expr: &rask::parser::ast::Expr) -> bool {
    use rask::parser::ast::Expr;

    match expr {
        Expr::Variable(name) => name == "http",
        Expr::Unary { rhs, .. } | Expr::PanicUnwrap(rhs) | Expr::Grouping(rhs) => {
            expr_requires_net(rhs)
        }
        Expr::Binary { lhs, rhs, .. } | Expr::Coalesce { lhs, rhs } => {
            expr_requires_net(lhs) || expr_requires_net(rhs)
        }
        Expr::Assign { value, .. } => expr_requires_net(value),
        Expr::Call { callee, args } => {
            expr_requires_net(callee) || args.iter().any(expr_requires_net)
        }
        Expr::Member {
            object, property, ..
        } => {
            expr_requires_net(object)
                || (matches!(object.as_ref(), Expr::Variable(name) if name == "http")
                    && matches!(property.as_str(), "get" | "post" | "put" | "delete"))
        }
        Expr::OrReturn { lhs, return_value } => {
            expr_requires_net(lhs) || expr_requires_net(return_value)
        }
        Expr::Match { subject, arms } => {
            expr_requires_net(subject) || arms.iter().any(|arm| expr_requires_net(&arm.value))
        }
        Expr::ListLiteral(items) => items.iter().any(expr_requires_net),
        Expr::ListComprehension {
            expr,
            iterable,
            condition,
            ..
        } => {
            expr_requires_net(expr)
                || expr_requires_net(iterable)
                || condition
                    .as_ref()
                    .is_some_and(|value| expr_requires_net(value))
        }
        Expr::MapLiteral(entries) => entries.iter().any(|entry| expr_requires_net(&entry.value)),
        Expr::Index { object, index } => expr_requires_net(object) || expr_requires_net(index),
        Expr::Int(_) | Expr::Float(_) | Expr::String { .. } | Expr::Bool(_) | Expr::Nil => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_program(source: &str) -> rask::parser::ast::Program {
        let tokens = rask::lexer::lex(source).expect("lex should succeed");
        let mut parser = rask::parser::Parser::new(tokens);
        parser.parse_program().expect("parse should succeed")
    }

    #[test]
    fn build_feature_detection_skips_net_when_unused() {
        let program = parse_program("x = 1 + 2\nprint(x)");
        let features = script_required_cargo_features(&program);
        assert!(features.is_empty());
    }

    #[test]
    fn build_feature_detection_enables_net_for_http_and_url_imports() {
        let program = parse_program("response = http.get(\"https://example.com\")");
        let features = script_required_cargo_features(&program);
        assert_eq!(features, vec!["net".to_string()]);

        let program = parse_program("use \"https://example.com/mod.rask\" as remote");
        let features = script_required_cargo_features(&program);
        assert_eq!(features, vec!["net".to_string()]);
    }
}
