#[test]
fn repl_highlighter_detects_multiline_input() {
    assert!(scalf::repl::highlighter::needs_more_input("if true {"));
    assert!(scalf::repl::highlighter::needs_more_input("x = (1 + 2"));
    assert!(!scalf::repl::highlighter::needs_more_input("x = 1 + 2"));
}

#[test]
fn repl_completion_suggests_keywords_and_symbols() {
    let symbols = vec![
        "http".to_string(),
        "historyCount".to_string(),
        "helloWorld".to_string(),
    ];
    let completions = scalf::repl::highlighter::complete("he", &symbols);
    assert!(completions.contains(&"helloWorld".to_string()));
}
