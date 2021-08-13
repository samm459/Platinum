#[allow(dead_code)]
mod diagnostics {
    #[derive(Clone, Debug, PartialEq)]
    pub enum Stage {
        Syntax,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Message {
        pub stage: Stage,
        pub position: usize,
        pub line: usize,
        pub offset: usize,
        pub text: String,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Report {
        Error(Message),
        Warning(Message),
    }

    #[derive(Debug)]
    pub struct Diagnostics {
        pub reports: Vec<Report>,
    }

    impl Diagnostics {
        pub fn new() -> Diagnostics {
            Diagnostics { reports: vec![] }
        }
    }
}

#[allow(dead_code)]
mod lexer {
    use crate::diagnostics::{Diagnostics, Message, Report, Stage};

    #[derive(Clone, PartialEq, Debug)]
    enum Syntax {
        Space,
        Line,
        Numeric,
        Alphanum,
        True,
        False,
        OpenParenthesis,
        CloseParenthesis,
        Fn,
        OpenBrackets,
        CloseBrackets,
        Return,
        Plus,
        Minus,
        Slash,
        Star,
        Percent,
        DoubleAmp,
        DoublePipe,
        If,
        Bang,
        BangEq,
        DoubleEq,
        Eq,
        Bad,
    }

    #[derive(Clone, PartialEq, Debug)]
    enum Value {
        Number(i32),
        Boolean(bool),
        String(String),
        None,
    }

    #[derive(Clone, PartialEq, Debug)]
    struct Token {
        syntax: Syntax,
        position: usize,
        line: usize,
        offset: usize,
        text: String,
        value: Value,
    }

    #[derive(Debug)]
    struct Lexer<'a> {
        src: String,
        tokens: Vec<Token>,
        position: usize,
        line: usize,
        offset: usize,
        diagnostics: &'a mut Diagnostics,
    }

    impl<'a> Lexer<'a> {
        fn new(src: &str, diagnostics: &'a mut Diagnostics) -> Lexer<'a> {
            Lexer {
                src: String::from(src),
                tokens: Vec::new(),
                position: 0,
                line: 1,
                offset: 0,
                diagnostics,
            }
        }

        fn peek(&self, length: usize) -> char {
            match self.src.chars().nth(self.position + length) {
                Some(character) => character,
                None => '\0',
            }
        }

        fn current(&self) -> char {
            self.peek(0)
        }

        fn next(&mut self) {
            self.position += 1;
            self.offset += 1;
        }

        fn skip(&mut self, length: usize) -> usize {
            let position = self.position;
            self.position += length;
            self.offset += length;
            position
        }

        fn step(&mut self) -> usize {
            self.skip(1)
        }

        fn place(&mut self) -> (usize, usize) {
            (self.offset, self.position)
        }

        fn is<F>(&mut self, condition: F) -> (String, usize, usize)
        where
            F: Fn(char) -> bool,
        {
            let (offset, position) = self.place();
            while condition(self.current()) {
                self.next();
            }
            let text = self
                .src
                .chars()
                .skip(position)
                .take(self.position - position)
                .collect::<String>();

            (text, offset, position)
        }

        fn keyword(text: String) -> Option<(Syntax, Value)> {
            if text == "true" {
                Some((Syntax::True, Value::Boolean(true)))
            } else if text == "false" {
                Some((Syntax::False, Value::Boolean(false)))
            } else if text == "if" {
                Some((Syntax::If, Value::None))
            } else if text == "fn" {
                Some((Syntax::Fn, Value::None))
            } else {
                None
            }
        }

        fn tokenize(&mut self) -> Token {
            if self.current() == '\n' {
                let token = Token {
                    syntax: Syntax::Line,
                    text: String::from("\n"),
                    line: self.line,
                    offset: self.offset,
                    value: Value::None,
                    position: self.step(),
                };
                self.line += 1;
                self.offset = 0;
                token
            } else if self.current().is_whitespace() {
                let (text, offset, position) = self.is(|c| c.is_whitespace());
                Token {
                    syntax: Syntax::Space,
                    text: text.clone(),
                    line: self.line,
                    offset,
                    value: Value::None,
                    position,
                }
            } else if self.current().is_numeric() {
                let (text, offset, position) = self.is(|c| c.is_numeric());
                Token {
                    syntax: Syntax::Numeric,
                    text: text.clone(),
                    line: self.line,
                    offset,
                    value: Value::Number(text.parse().unwrap()),
                    position,
                }
            } else if self.current().is_alphabetic() {
                let (text, offset, position) = self.is(|c| c.is_alphanumeric());
                match Lexer::keyword(text.clone()) {
                    Some((syntax, value)) => Token {
                        syntax,
                        text: text.clone(),
                        line: self.line,
                        offset,
                        position,
                        value,
                    },
                    None => Token {
                        syntax: Syntax::Alphanum,
                        text: text.clone(),
                        line: self.line,
                        offset,
                        position,
                        value: Value::None,
                    },
                }
            } else if self.current() == '(' {
                Token {
                    syntax: Syntax::OpenParenthesis,
                    text: String::from("("),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == ')' {
                Token {
                    syntax: Syntax::CloseParenthesis,
                    text: String::from(")"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '{' {
                Token {
                    syntax: Syntax::OpenBrackets,
                    text: String::from("{"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '}' {
                Token {
                    syntax: Syntax::CloseBrackets,
                    text: String::from("}"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '+' {
                Token {
                    syntax: Syntax::Plus,
                    text: String::from("+"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '-' {
                if self.peek(1) == '>' {
                    Token {
                        syntax: Syntax::Return,
                        text: String::from("->"),
                        line: self.line,
                        offset: self.offset,
                        position: self.skip(2),
                        value: Value::None,
                    }
                } else {
                    Token {
                        syntax: Syntax::Minus,
                        text: String::from("-"),
                        line: self.line,
                        offset: self.offset,
                        position: self.step(),
                        value: Value::None,
                    }
                }
            } else if self.current() == '*' {
                Token {
                    syntax: Syntax::Star,
                    text: String::from("*"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '/' {
                Token {
                    syntax: Syntax::Slash,
                    text: String::from("/"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '%' {
                Token {
                    syntax: Syntax::Percent,
                    text: String::from("%"),
                    line: self.line,
                    offset: self.offset,
                    position: self.step(),
                    value: Value::None,
                }
            } else if self.current() == '&' && self.peek(1) == '&' {
                Token {
                    syntax: Syntax::DoubleAmp,
                    text: String::from("&&"),
                    line: self.line,
                    offset: self.offset,
                    position: self.skip(2),
                    value: Value::None,
                }
            } else if self.current() == '|' && self.peek(1) == '|' {
                Token {
                    syntax: Syntax::DoublePipe,
                    text: String::from("||"),
                    line: self.line,
                    offset: self.offset,
                    position: self.skip(2),
                    value: Value::None,
                }
            } else if self.current() == '!' {
                if self.peek(1) == '=' {
                    Token {
                        syntax: Syntax::BangEq,
                        text: String::from("!="),
                        line: self.line,
                        offset: self.offset,
                        position: self.skip(2),
                        value: Value::None,
                    }
                } else {
                    Token {
                        syntax: Syntax::Bang,
                        text: String::from("!"),
                        line: self.line,
                        offset: self.offset,
                        position: self.step(),
                        value: Value::None,
                    }
                }
            } else if self.current() == '=' {
                if self.peek(1) == '=' {
                    Token {
                        syntax: Syntax::DoubleEq,
                        text: String::from("=="),
                        line: self.line,
                        offset: self.offset,
                        position: self.skip(2),
                        value: Value::None,
                    }
                } else {
                    Token {
                        syntax: Syntax::Eq,
                        text: String::from("="),
                        line: self.line,
                        offset: self.offset,
                        position: self.step(),
                        value: Value::None,
                    }
                }
            } else {
                self.diagnostics.reports.push(Report::Error(Message {
                    stage: Stage::Syntax,
                    line: self.line,
                    offset: self.offset,
                    position: self.position,
                    text: format!("Unknown character {}", self.current()),
                }));
                Token {
                    syntax: Syntax::Bad,
                    text: String::from(self.current()),
                    line: self.line,
                    offset: self.offset,
                    value: Value::None,
                    position: self.step(),
                }
            }
        }

        fn lex(&mut self) -> &Self {
            while self.position < self.src.len() {
                let token = self.tokenize();
                self.tokens.push(token);
            }
            self
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::diagnostics::{Diagnostics, Message, Report, Stage};
        use crate::lexer::{Lexer, Syntax, Token, Value};

        fn token(src: &str, i: usize) -> Token {
            Lexer::new(src, &mut Diagnostics::new()).lex().tokens[i].clone()
        }

        fn report(src: &str, i: usize) -> Report {
            let mut diagnostics = Diagnostics::new();
            Lexer::new(src, &mut diagnostics).lex();
            diagnostics.reports[i].clone()
        }

        #[test]
        fn it_lexes_lines() {
            assert_eq!(
                token("\n", 0),
                Token {
                    syntax: Syntax::Line,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("\n"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_space() {
            assert_eq!(
                Lexer::new(" ", &mut Diagnostics::new()).lex().tokens[0],
                Token {
                    syntax: Syntax::Space,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from(" "),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_numeric() {
            assert_eq!(
                token("1000", 0),
                Token {
                    syntax: Syntax::Numeric,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("1000"),
                    value: Value::Number(1000)
                }
            );
        }

        #[test]
        fn it_lexes_alphanum() {
            assert_eq!(
                token("a1b2c3", 0),
                Token {
                    syntax: Syntax::Alphanum,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("a1b2c3"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_true() {
            assert_eq!(
                token("true", 0),
                Token {
                    syntax: Syntax::True,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("true"),
                    value: Value::Boolean(true)
                }
            );
        }

        #[test]
        fn it_lexes_false() {
            assert_eq!(
                token("false", 0),
                Token {
                    syntax: Syntax::False,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("false"),
                    value: Value::Boolean(false)
                }
            );
        }

        #[test]
        fn it_lexes_open_parenthesis() {
            assert_eq!(
                token("(", 0),
                Token {
                    syntax: Syntax::OpenParenthesis,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("("),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_close_parenthesis() {
            assert_eq!(
                token(")", 0),
                Token {
                    syntax: Syntax::CloseParenthesis,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from(")"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_plus() {
            assert_eq!(
                token("+", 0),
                Token {
                    syntax: Syntax::Plus,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("+"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_minus() {
            assert_eq!(
                token("-", 0),
                Token {
                    syntax: Syntax::Minus,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("-"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_star() {
            assert_eq!(
                token("*", 0),
                Token {
                    syntax: Syntax::Star,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("*"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_slash() {
            assert_eq!(
                token("/", 0),
                Token {
                    syntax: Syntax::Slash,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("/"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_percent() {
            assert_eq!(
                token("%", 0),
                Token {
                    syntax: Syntax::Percent,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("%"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_double_amp() {
            assert_eq!(
                token("&&", 0),
                Token {
                    syntax: Syntax::DoubleAmp,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("&&"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_double_pipe() {
            assert_eq!(
                token("||", 0),
                Token {
                    syntax: Syntax::DoublePipe,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("||"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_bang() {
            assert_eq!(
                token("!", 0),
                Token {
                    syntax: Syntax::Bang,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("!"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_bang_eq() {
            assert_eq!(
                token("!=", 0),
                Token {
                    syntax: Syntax::BangEq,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("!="),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_eq() {
            assert_eq!(
                token("=", 0),
                Token {
                    syntax: Syntax::Eq,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("="),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_double_eq() {
            assert_eq!(
                token("==", 0),
                Token {
                    syntax: Syntax::DoubleEq,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("=="),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_open_brakets() {
            assert_eq!(
                token("{", 0),
                Token {
                    syntax: Syntax::OpenBrackets,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("{"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_close_brakets() {
            assert_eq!(
                token("}", 0),
                Token {
                    syntax: Syntax::CloseBrackets,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("}"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_fn() {
            assert_eq!(
                token("fn", 0),
                Token {
                    syntax: Syntax::Fn,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("fn"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_return() {
            assert_eq!(
                token("->", 0),
                Token {
                    syntax: Syntax::Return,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("->"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_if() {
            assert_eq!(
                token("if", 0),
                Token {
                    syntax: Syntax::If,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("if"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_lexes_bad() {
            assert_eq!(
                token("^", 0),
                Token {
                    syntax: Syntax::Bad,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("^"),
                    value: Value::None
                }
            );
        }

        #[test]
        fn it_handles_positions_lines_and_offsets() {
            assert_eq!(
                token("1000\n 1", 3),
                Token {
                    syntax: Syntax::Numeric,
                    position: 6,
                    line: 2,
                    offset: 1,
                    text: String::from("1"),
                    value: Value::Number(1)
                }
            );
        }

        #[test]
        fn it_adds_syntax_errors_to_diagnostics() {
            assert_eq!(
                report("^", 0),
                Report::Error(Message {
                    stage: Stage::Syntax,
                    position: 0,
                    line: 1,
                    offset: 0,
                    text: String::from("Unknown character ^")
                })
            );
        }
    }
}
