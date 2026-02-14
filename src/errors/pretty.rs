pub fn underline(line: &str, column: usize) -> String {
    let mut marker = String::new();
    for _ in 1..column {
        marker.push(' ');
    }
    marker.push('^');
    format!("{}\n{}", line, marker)
}

pub fn format_parse_error(
    source_label: &str,
    source: &str,
    err: &crate::parser::ParseError,
) -> String {
    let line = source
        .lines()
        .nth(err.line.saturating_sub(1))
        .unwrap_or_default();
    format!(
        "parse error [PARSE0001]: {}\n--> {}:{}:{}\n{}\nhelp: check syntax near this token\ndocs: https://scalf-lang.dev/errors/PARSE0001",
        err.message,
        source_label,
        err.line,
        err.column,
        underline(line, err.column),
    )
}

pub fn format_type_errors(
    source_label: &str,
    errors: &[crate::typechecker::TypeError],
) -> Vec<String> {
    errors
        .iter()
        .map(|err| {
            format!(
                "type error [TYPE0001]: {}\n--> {}\nhelp: check type annotations and function signatures\ndocs: https://scalf-lang.dev/errors/TYPE0001",
                err.message, source_label
            )
        })
        .collect()
}

pub fn format_lint_warning(warn: &crate::lint::LintWarning, source_label: &str) -> String {
    let mut rendered = format!(
        "warning [{}]: {}\n--> {}",
        warn.code, warn.message, source_label
    );
    if let Some(hint) = &warn.hint {
        rendered.push_str(&format!("\nhelp: {}", hint));
    }
    rendered.push_str(&format!(
        "\ndocs: https://scalf-lang.dev/errors/{}",
        warn.code
    ));
    rendered
}
