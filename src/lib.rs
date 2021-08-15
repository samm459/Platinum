#[allow(dead_code)]
mod diagnostics {
    pub type At = (usize, usize, usize);
    pub type Diagnostics = Vec<Report>;

    #[derive(Clone, Debug, PartialEq)]
    pub enum Stage {
        Syntax,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Report {
        Error(At, Stage, String),
        Warning(At, Stage, String),
    }
}

#[allow(dead_code)]
mod lexer {
    use crate::diagnostics::{At, Diagnostics, Report, Stage};

    #[derive(Clone, PartialEq, Debug)]
    pub enum Syntax {
        Space(At, String),
        Numeric(At, String, u32),
        Alphanum(At, String),
        Bool(At, bool),
        OpenP(At),
        CloseP(At),
        Fn(At),
        OpenB(At),
        CloseB(At),
        Return(At),
        Plus(At),
        Minus(At),
        Slash(At),
        Star(At),
        Percent(At),
        DoubleAmp(At),
        DoublePipe(At),
        If(At),
        Bang(At),
        BangEq(At),
        DoubleEq(At),
        Eq(At),
        Line(At),
        Bad(At, u8),
    }

    impl Syntax {
        fn from_word(text: String, at: At) -> Syntax {
            let s = text.as_str();
            println!("{}", s);
            if s == "true" {
                Syntax::Bool(at, true)
            } else if s == "false" {
                Syntax::Bool(at, false)
            } else if s == "if" {
                Syntax::If(at)
            } else if s == "fn" {
                Syntax::Fn(at)
            } else {
                Syntax::Alphanum(at, String::from(text))
            }
        }

        fn text(&self) -> String {
            match self {
                Syntax::Space(_, text) => String::from(text),
                Syntax::Numeric(_, text, _) => String::from(text),
                Syntax::Alphanum(_, text) => String::from(text),
                Syntax::Bool(_, val) => match val {
                    true => String::from("true"),
                    false => String::from("false"),
                },
                Syntax::OpenP(_) => String::from("("),
                Syntax::CloseP(_) => String::from(")"),
                Syntax::Fn(_) => String::from("fn"),
                Syntax::OpenB(_) => String::from("{"),
                Syntax::CloseB(_) => String::from("}"),
                Syntax::Return(_) => String::from("->"),
                Syntax::Plus(_) => String::from("+"),
                Syntax::Minus(_) => String::from("-"),
                Syntax::Slash(_) => String::from("/"),
                Syntax::Star(_) => String::from("*"),
                Syntax::Percent(_) => String::from("%"),
                Syntax::DoubleAmp(_) => String::from("&&"),
                Syntax::DoublePipe(_) => String::from("||"),
                Syntax::If(_) => String::from("if"),
                Syntax::Bang(_) => String::from("!"),
                Syntax::BangEq(_) => String::from("!="),
                Syntax::DoubleEq(_) => String::from("=="),
                Syntax::Eq(_) => String::from("="),
                Syntax::Line(_) => String::from("\n"),
                Syntax::Bad(_, byte) => {
                    String::from(std::str::from_utf8(std::slice::from_ref(byte)).unwrap())
                }
            }
        }

        fn length(&self) -> usize {
            self.text().len()
        }
    }

    type Stream = Vec<Syntax>;
    type LexerResult = Result<Stream, (Stream, Diagnostics)>;

    #[derive(Debug)]
    pub struct Lexer {
        src: Vec<u8>,
        pub tokens: Vec<Syntax>,
    }

    impl Lexer {
        pub fn new(src: String) -> Lexer {
            Lexer {
                src: src.into_bytes(),
                tokens: Vec::new(),
            }
        }

        fn peek(&self, position: usize) -> u8 {
            match self.src.iter().nth(position) {
                Some(byte) => *byte,
                None => b'\n',
            }
        }

        fn at(&self, position: usize) -> u8 {
            self.src[position]
        }

        fn is<F>(&self, position: usize, condition: F) -> String
        where
            F: Fn(u8) -> bool,
        {
            let mut length = 0;
            while condition(self.peek(position + length)) {
                length += 1;
            }
            let text = std::str::from_utf8(&self.src[position..position + length]).unwrap();

            String::from(text)
        }

        fn current(&self, at: At) -> Syntax {
            let (position, _, _) = at;
            let b = self.at(position);

            match b {
                b'\n' => Syntax::Line(at),
                b'(' => Syntax::OpenP(at),
                b')' => Syntax::CloseP(at),
                b'{' => Syntax::OpenB(at),
                b'}' => Syntax::CloseB(at),
                b'+' => Syntax::Plus(at),
                b'*' => Syntax::Star(at),
                b'/' => Syntax::Slash(at),
                b'%' => Syntax::Percent(at),

                b'-' => match self.peek(position + 1) {
                    b'>' => Syntax::Return(at),
                    _ => Syntax::Minus(at),
                },

                b'=' => match self.peek(position + 1) {
                    b'=' => Syntax::DoubleEq(at),
                    _ => Syntax::Eq(at),
                },

                b'!' => match self.peek(position + 1) {
                    b'=' => Syntax::BangEq(at),
                    _ => Syntax::Bang(at),
                },

                b'&' => match self.peek(position + 1) {
                    b'&' => Syntax::DoubleAmp(at),
                    _ => Syntax::Bad(at, b),
                },

                b'|' => match self.peek(position + 1) {
                    b'|' => Syntax::DoublePipe(at),
                    _ => Syntax::Bad(at, b),
                },

                _ => {
                    if b.is_ascii_digit() {
                        let text = self.is(position, |x| x.is_ascii_digit());
                        let n = text.parse::<u32>().unwrap();
                        return Syntax::Numeric(at, text.clone(), n);
                    } else if b == b' ' {
                        let text = self.is(position, |x| x == b' ');
                        return Syntax::Space(at, text.clone());
                    } else if b.is_ascii_alphabetic() {
                        let text = self.is(position, |x| x.is_ascii_alphanumeric());
                        return Syntax::from_word(text.clone(), at);
                    }

                    Syntax::Bad(at, b)
                }
            }
        }

        fn tokenize(&self) -> LexerResult {
            let (mut position, mut line, mut offset): At = (0, 1, 0);
            let at = (position, line, offset);
            let mut diagnostics = Vec::new();
            let mut tokens = Vec::new();

            let err = |b: u8| {
                Report::Error(
                    at,
                    Stage::Syntax,
                    format!("Unknown character {:?}", std::str::from_utf8(&[b]).unwrap()),
                )
            };

            while position < self.src.len() {
                let token = self.current((position, line, offset));
                match token {
                    Syntax::Bad(_, b) => {
                        diagnostics.push(err(b));
                    }
                    Syntax::Line(_) => {
                        line += 1;
                        offset = 0;
                    }
                    _ => (),
                };
                position += token.length();
                offset += token.length();
                tokens.push(token);
            }

            if diagnostics.is_empty() {
                Ok(tokens)
            } else {
                Err((tokens, diagnostics))
            }
        }

        pub fn lex(src: String) -> LexerResult {
            let lexer = Lexer::new(src);
            lexer.tokenize()
        }

        pub fn lex_from(src: &str) -> LexerResult {
            let lexer = Lexer::new(String::from(src));
            lexer.tokenize()
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::diagnostics::{At, Report, Stage};
        use crate::lexer::{Lexer, Syntax};

        const AT: At = (0, 1, 0);

        #[test]
        fn it_lexes_lines() {
            assert_eq!(Lexer::lex_from("\n").unwrap()[0], Syntax::Line(AT));
        }

        #[test]
        fn it_lexes_space() {
            assert_eq!(
                Lexer::lex_from(" ").unwrap()[0],
                Syntax::Space(AT, String::from(" "))
            )
        }

        #[test]
        fn it_lexes_numeric() {
            assert_eq!(
                Lexer::lex_from("1001").unwrap()[0],
                Syntax::Numeric(AT, String::from("1001"), 1001)
            )
        }

        #[test]
        fn it_lexes_alphanum() {
            assert_eq!(
                Lexer::lex_from("a1b2c3").unwrap()[0],
                Syntax::Alphanum(AT, String::from("a1b2c3"))
            );
        }

        #[test]
        fn it_lexes_true() {
            assert_eq!(Lexer::lex_from("true").unwrap()[0], Syntax::Bool(AT, true));
        }

        #[test]
        fn it_lexes_false() {
            assert_eq!(
                Lexer::lex_from("false").unwrap()[0],
                Syntax::Bool(AT, false)
            );
        }

        #[test]
        fn it_lexes_open_parenthesis() {
            assert_eq!(Lexer::lex_from("(").unwrap()[0], Syntax::OpenP(AT));
        }

        #[test]
        fn it_lexes_close_parenthesis() {
            assert_eq!(Lexer::lex_from(")").unwrap()[0], Syntax::CloseP(AT));
        }

        #[test]
        fn it_lexes_plus() {
            assert_eq!(Lexer::lex_from("+").unwrap()[0], Syntax::Plus(AT));
        }

        #[test]
        fn it_lexes_minus() {
            assert_eq!(Lexer::lex_from("-").unwrap()[0], Syntax::Minus(AT));
        }

        #[test]
        fn it_lexes_star() {
            assert_eq!(Lexer::lex_from("*").unwrap()[0], Syntax::Star(AT));
        }

        #[test]
        fn it_lexes_slash() {
            assert_eq!(Lexer::lex_from("/").unwrap()[0], Syntax::Slash(AT));
        }

        #[test]
        fn it_lexes_percent() {
            assert_eq!(Lexer::lex_from("%").unwrap()[0], Syntax::Percent(AT));
        }

        #[test]
        fn it_lexes_double_amp() {
            assert_eq!(Lexer::lex_from("&&").unwrap()[0], Syntax::DoubleAmp(AT));
        }

        #[test]
        fn it_lexes_double_pipe() {
            assert_eq!(Lexer::lex_from("||").unwrap()[0], Syntax::DoublePipe(AT));
        }

        #[test]
        fn it_lexes_bang() {
            assert_eq!(Lexer::lex_from("!").unwrap()[0], Syntax::Bang(AT));
        }

        #[test]
        fn it_lexes_bang_eq() {
            assert_eq!(Lexer::lex_from("!=").unwrap()[0], Syntax::BangEq(AT));
        }

        #[test]
        fn it_lexes_eq() {
            assert_eq!(Lexer::lex_from("=").unwrap()[0], Syntax::Eq(AT));
        }

        #[test]
        fn it_lexes_double_eq() {
            assert_eq!(Lexer::lex_from("==").unwrap()[0], Syntax::DoubleEq(AT));
        }

        #[test]
        fn it_lexes_open_brakets() {
            assert_eq!(Lexer::lex_from("{").unwrap()[0], Syntax::OpenB(AT));
        }

        #[test]
        fn it_lexes_close_brakets() {
            assert_eq!(Lexer::lex_from("}").unwrap()[0], Syntax::CloseB(AT));
        }

        #[test]
        fn it_lexes_fn() {
            assert_eq!(Lexer::lex_from("fn").unwrap()[0], Syntax::Fn(AT));
        }

        #[test]
        fn it_lexes_return() {
            assert_eq!(Lexer::lex_from("->").unwrap()[0], Syntax::Return(AT));
        }

        #[test]
        fn it_lexes_if() {
            assert_eq!(Lexer::lex_from("if").unwrap()[0], Syntax::If(AT));
        }

        #[test]
        fn it_lexes_bad() {
            assert_eq!(
                Lexer::lex_from("^").unwrap_err().0[0],
                Syntax::Bad(AT, b'^')
            );
        }

        #[test]
        fn it_handles_positions_lines_and_offsets() {
            println!("{:?}", Lexer::lex_from("1000\n 1").unwrap());
            assert_eq!(
                Lexer::lex_from("1000\n 1").unwrap()[3],
                Syntax::Numeric((6, 2, 2), String::from("1"), 1)
            );
        }

        #[test]
        fn it_reports_issues() {
            assert_eq!(
                Lexer::lex_from("^").unwrap_err(),
                (
                    Vec::from([Syntax::Bad(AT, b'^')]),
                    Vec::from([Report::Error(
                        AT,
                        Stage::Syntax,
                        String::from("Unknown character \"^\"")
                    )])
                )
            );
        }
    }
}
