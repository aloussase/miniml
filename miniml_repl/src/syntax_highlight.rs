use std::collections::HashMap;
use std::default::Default;

pub struct SyntaxHighlighter<'a> {
    highlights: HashMap<&'a str, &'a str>,
}

impl Default for SyntaxHighlighter<'_> {
    fn default() -> Self {
        let mut highlights = HashMap::new();
        highlights.insert("fun", "\x1b[34m");
        highlights.insert("is", "\x1b[34m");
        highlights.insert("let", "\x1b[31m");
        highlights.insert("if", "\x1b[31m");
        highlights.insert("then", "\x1b[31m");
        highlights.insert("else", "\x1b[31m");
        highlights.insert("<", "\x1b[32m");
        highlights.insert("+", "\x1b[32m");
        highlights.insert("-", "\x1b[32m");
        highlights.insert("*", "\x1b[32m");
        highlights.insert("=", "\x1b[32m");
        highlights.insert("(", "\x1b[32m");
        highlights.insert(")", "\x1b[32m");
        highlights.insert(":", "\x1b[32m");
        highlights.insert("int", "\x1b[33m");
        highlights.insert("bool", "\x1b[33m");
        highlights.insert(";;", "\x1b[36m");
        Self { highlights }
    }
}

impl<'a> SyntaxHighlighter<'a> {
    fn highlight_word(&self, word: &str) -> String {
        let mut highlighted_word = String::new();
        let maybe_highlight = self.highlights.get(word);

        if let Some(ref highlight) = maybe_highlight {
            highlighted_word.push_str(highlight);
        }

        highlighted_word.push_str(&word);

        if let Some(_) = maybe_highlight {
            highlighted_word.push_str("\x1b[m");
        }

        highlighted_word
    }

    pub fn syntax_highlight(&self, s: String) -> String {
        let mut result = String::new();
        let mut current_word = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '(' | ')' | '+' | '-' | '*' | ':' => {
                    let highlighted_word = self.highlight_word(&current_word);
                    let highlighted_operator = self.highlight_word(&c.to_string());

                    result.push_str(&highlighted_word);
                    result.push_str(&highlighted_operator);

                    current_word.clear();
                }
                '\n' | ' ' => {
                    let highlighted_word = self.highlight_word(&current_word);
                    current_word.clear();

                    result.push_str(&highlighted_word);
                    result.push(c);

                    while let Some(c) = chars.peek() {
                        match c {
                            ' ' | '\n' => result.push(chars.next().unwrap()),
                            _ => break,
                        }
                    }
                }
                c => current_word.push(c),
            }
        }

        if !current_word.is_empty() {
            let highlighted_word = self.highlight_word(&current_word);
            result.push_str(&highlighted_word);
        }

        result
    }
}
