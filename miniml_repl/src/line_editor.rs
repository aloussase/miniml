use std::collections::VecDeque;
use std::default::Default;
use std::io::{self, Read, Write};
use std::ops::Drop;
use termios::{tcsetattr, Termios, ECHO, ICANON, TCSANOW};

use crate::syntax_highlight::SyntaxHighlighter;

pub struct LineEditor<'a> {
    prompt: String,
    reader: std::io::Stdin,
    writer: std::io::Stdout,
    current_char: [u8; 1],
    current_line: String,
    future_history: VecDeque<String>,
    past_history: VecDeque<String>,
    orig_termios: Termios,
    syntax_highlighter: SyntaxHighlighter<'a>,
}

pub enum LineResult<'a> {
    Line(&'a str),
    Eof,
}

enum ReadCharResult {
    NewLine,
    Character(u8),
    Eof,
}

impl Default for LineEditor<'_> {
    fn default() -> Self {
        let orig_termios = Termios::from_fd(0).unwrap();
        let mut new_termios = orig_termios.clone();

        new_termios.c_lflag &= !(ICANON | ECHO);
        tcsetattr(0, TCSANOW, &mut new_termios).unwrap();

        LineEditor {
            prompt: "> ".to_string(),
            reader: io::stdin(),
            writer: io::stdout(),
            current_char: [0; 1],
            current_line: "".to_string(),
            future_history: VecDeque::new(),
            past_history: VecDeque::new(),
            orig_termios,
            syntax_highlighter: SyntaxHighlighter::default(),
        }
    }
}

impl Drop for LineEditor<'_> {
    fn drop(&mut self) {
        tcsetattr(0, TCSANOW, &self.orig_termios).unwrap();
    }
}

impl LineEditor<'_> {
    pub fn read_line(&mut self) -> LineResult<'_> {
        self.print_prompt();

        loop {
            match self.read_char() {
                ReadCharResult::Eof => return LineResult::Eof,
                ReadCharResult::Character(c) => match c {
                    b'\x1b' => {
                        self.read_char();
                        match self.read_char() {
                            ReadCharResult::Character(b'A') => self.move_up_in_history(),
                            ReadCharResult::Character(b'B') => self.move_down_in_history(),
                            _ => {}
                        }
                    }
                    127 => {
                        if !self.current_line.is_empty() {
                            self.current_line.pop();
                        }
                    }
                    c => self.current_line.push(c as char),
                },
                ReadCharResult::NewLine => {
                    println!("");
                    if !self.current_line.is_empty() {
                        let line = self.current_line.clone();
                        self.current_line.clear();
                        self.add_history(line.clone());
                        return LineResult::Line(self.past_history.front().unwrap());
                    }
                }
            }

            self.move_to_start_of_line();
            self.erase_line();
            self.print_prompt();

            let line = self
                .syntax_highlighter
                .syntax_highlight(self.current_line.clone());
            print!("{}", line);

            self.writer.flush().unwrap();
        }
    }

    fn move_down_in_history(&mut self) {
        if self.future_history.len() == 1 {
            self.current_line = self.future_history.front().unwrap().clone();
            return;
        }

        if let Some(line) = self.future_history.pop_front() {
            self.current_line = line.clone();
            self.past_history.push_front(line);
        }
    }

    fn move_up_in_history(&mut self) {
        if self.past_history.len() == 1 {
            self.current_line = self.past_history.front().unwrap().clone();
            return;
        }

        if let Some(line) = self.past_history.pop_front() {
            self.current_line = line.clone();
            self.future_history.push_front(line);
        }
    }

    fn add_history(&mut self, line: String) {
        while let Some(line) = self.future_history.pop_front() {
            self.past_history.push_front(line);
        }
        self.past_history.push_front(line);
    }

    fn print_prompt(&mut self) {
        print!("{}", self.prompt);
        self.writer.flush().unwrap();
    }

    fn move_to_start_of_line(&self) {
        // FIXME: A little hack here: move the cursor 100 columns to the left.
        // A better solution would be to get the cursor position somehow and
        // move to column 0 but maintaining the current line.
        print!("\x1b[100D");
    }

    fn erase_line(&self) {
        print!("\x1b[2K");
    }

    fn read_char(&mut self) -> ReadCharResult {
        if let Err(_) = self.reader.read(&mut self.current_char) {
            return ReadCharResult::Eof;
        }

        match self.current_char[0] {
            b'\n' => ReadCharResult::NewLine,
            c => ReadCharResult::Character(c),
        }
    }
}
