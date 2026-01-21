use crate::Token;
use crate::token::{LiteralKind, TokenKind};

pub struct Cursor<'a> {
    input: &'a str,
    pos: usize,
    at_line_start: bool,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            at_line_start: true,
        }
    }

    fn first(&self) -> char {
        self.input[self.pos..].chars().next().unwrap_or('\0')
    }

    fn second(&self) -> char {
        let mut chars = self.input[self.pos..].chars();
        chars.next();
        chars.next().unwrap_or('\0')
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.input[self.pos..].chars().next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while !self.is_eof() && predicate(self.first()) {
            self.bump();
        }
    }

    fn slice_from(&self, start: usize) -> &'a str {
        &self.input[start..self.pos]
    }

    pub fn advance_token(&mut self) -> Token {
        let start = self.pos;
        let kind = if self.is_eof() {
            TokenKind::Eof
        } else {
            self.token_kind()
        };
        let len = (self.pos - start) as u32;
        Token::new(kind, len)
    }

    fn token_kind(&mut self) -> TokenKind {
        let first = self.first();
        let at_line_start = self.at_line_start;
        self.at_line_start = false;

        match first {
            ' ' | '\t' => {
                self.eat_while(|c| c == ' ' || c == '\t');
                TokenKind::Whitespace
            }

            '\n' => {
                self.bump();
                self.at_line_start = true;
                TokenKind::Newline
            }
            '\r' => {
                self.bump();
                if self.first() == '\n' {
                    self.bump();
                }
                self.at_line_start = true;
                TokenKind::Newline
            }

            '*' if at_line_start => {
                self.bump();
                self.eat_while(|c| c != '\n' && c != '\r');
                TokenKind::CommentStar
            }

            '&' if self.second() == '&' => {
                self.bump();
                self.bump();
                self.eat_while(|c| c != '\n' && c != '\r');
                TokenKind::CommentAmpAmp
            }

            '&' => {
                self.bump();
                TokenKind::Amp
            }

            ';' => {
                self.bump();
                let ws_start = self.pos;
                self.eat_while(|c| c == ' ' || c == '\t');
                if self.first() == '\n' {
                    self.bump();
                    self.at_line_start = true;
                    TokenKind::LineContinuation
                } else if self.first() == '\r' {
                    self.bump();
                    if self.first() == '\n' {
                        self.bump();
                    }
                    self.at_line_start = true;
                    TokenKind::LineContinuation
                } else {
                    self.pos = ws_start;
                    TokenKind::Semi
                }
            }

            '#' => self.preprocessor_or_hash(),
            '.' => self.dot_token(),
            '"' => self.string_double(),
            '\'' => self.string_single(),

            '[' => {
                self.bump();
                TokenKind::LBracket
            }

            '{' => self.brace_or_date(),

            '=' => {
                self.bump();
                if self.first() == '=' {
                    self.bump();
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }
            '!' => {
                self.bump();
                if self.first() == '=' {
                    self.bump();
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                }
            }
            '<' => {
                self.bump();
                match self.first() {
                    '=' => {
                        self.bump();
                        TokenKind::LtEq
                    }
                    '>' => {
                        self.bump();
                        TokenKind::BangEq
                    }
                    '<' => {
                        self.bump();
                        TokenKind::TextMergeOpen
                    }
                    _ => TokenKind::Lt,
                }
            }
            '>' => {
                self.bump();
                match self.first() {
                    '=' => {
                        self.bump();
                        TokenKind::GtEq
                    }
                    '>' => {
                        self.bump();
                        TokenKind::TextMergeClose
                    }
                    _ => TokenKind::Gt,
                }
            }
            '+' => {
                self.bump();
                TokenKind::Plus
            }
            '-' => {
                self.bump();
                if self.first() == '>' {
                    self.bump();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                self.bump();
                if self.first() == '*' {
                    self.bump();
                    TokenKind::Caret
                } else {
                    TokenKind::Star
                }
            }
            '/' => {
                self.bump();
                TokenKind::Slash
            }
            '%' => {
                self.bump();
                TokenKind::Percent
            }
            '^' => {
                self.bump();
                TokenKind::Caret
            }
            '$' => {
                self.bump();
                TokenKind::Dollar
            }
            '@' => {
                self.bump();
                TokenKind::At
            }
            '|' => {
                self.bump();
                TokenKind::Pipe
            }
            '?' => {
                self.bump();
                TokenKind::Question
            }

            '(' => {
                self.bump();
                TokenKind::LParen
            }
            ')' => {
                self.bump();
                TokenKind::RParen
            }
            ']' => {
                self.bump();
                TokenKind::RBracket
            }
            '}' => {
                self.bump();
                TokenKind::RBrace
            }
            ',' => {
                self.bump();
                TokenKind::Comma
            }
            ':' => {
                self.bump();
                if self.first() == ':' {
                    self.bump();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }

            '0'..='9' => self.number(),

            c if crate::is_ident_start(c) => self.ident_or_note(),

            _ => {
                self.bump();
                TokenKind::Unknown
            }
        }
    }

    fn preprocessor_or_hash(&mut self) -> TokenKind {
        self.bump();

        if !crate::is_ident_start(self.first()) {
            return TokenKind::Hash;
        }

        let start = self.pos;
        self.eat_while(crate::is_ident_continue);
        let ident = self.slice_from(start).to_ascii_uppercase();

        match ident.as_str() {
            "DEFINE" | "DEF" => TokenKind::PreDefine,
            "UNDEF" => TokenKind::PreUndef,
            "IF" => TokenKind::PreIf,
            "IFDEF" => TokenKind::PreIfDef,
            "IFNDEF" => TokenKind::PreIfNDef,
            "ELSE" => TokenKind::PreElse,
            "ELIF" => TokenKind::PreElif,
            "ENDIF" => TokenKind::PreEndIf,
            "INCLUDE" => TokenKind::PreInclude,
            _ => {
                self.pos = start;
                TokenKind::Hash
            }
        }
    }

    fn dot_token(&mut self) -> TokenKind {
        self.bump();

        if crate::is_ident_start(self.first()) {
            let start = self.pos;
            self.eat_while(crate::is_ident_continue);

            if self.first() == '.' {
                let ident = self.slice_from(start).to_ascii_uppercase();
                self.bump();

                return match ident.as_str() {
                    "T" | "TRUE" => TokenKind::True,
                    "F" | "FALSE" => TokenKind::False,
                    "NULL" => TokenKind::Null,
                    "AND" => TokenKind::DotAnd,
                    "OR" => TokenKind::DotOr,
                    "NOT" => TokenKind::DotNot,
                    _ => {
                        self.pos = start;
                        TokenKind::Dot
                    }
                };
            } else {
                self.pos = start;
            }
        }

        TokenKind::Dot
    }

    fn string_double(&mut self) -> TokenKind {
        self.bump();
        let mut terminated = false;

        while !self.is_eof() {
            match self.first() {
                '"' => {
                    self.bump();
                    terminated = true;
                    break;
                }
                '\n' | '\r' => break,
                _ => {
                    self.bump();
                }
            }
        }

        TokenKind::Literal {
            kind: LiteralKind::StringDouble { terminated },
        }
    }

    fn string_single(&mut self) -> TokenKind {
        self.bump();
        let mut terminated = false;

        while !self.is_eof() {
            match self.first() {
                '\'' => {
                    self.bump();
                    terminated = true;
                    break;
                }
                '\n' | '\r' => break,
                _ => {
                    self.bump();
                }
            }
        }

        TokenKind::Literal {
            kind: LiteralKind::StringSingle { terminated },
        }
    }

    fn brace_or_date(&mut self) -> TokenKind {
        self.bump();

        if self.first() == '^' {
            self.bump();
            let start = self.pos;
            self.eat_while(|c| c != '}' && c != '\n' && c != '\r');

            if self.first() == '}' {
                self.bump();
                let content = self.slice_from(start);
                if content.contains(':') || content.contains(' ') || content.contains('T') {
                    TokenKind::Literal {
                        kind: LiteralKind::DateTime,
                    }
                } else {
                    TokenKind::Literal {
                        kind: LiteralKind::Date,
                    }
                }
            } else {
                TokenKind::Literal {
                    kind: LiteralKind::Date,
                }
            }
        } else {
            TokenKind::LBrace
        }
    }

    fn number(&mut self) -> TokenKind {
        let first = self.first();

        if first == '0' {
            let second = self.second();
            if second == 'x' || second == 'X' || second == 'h' || second == 'H' {
                self.bump();
                self.bump();
                self.eat_while(|c| c.is_ascii_hexdigit());
                return TokenKind::Literal {
                    kind: LiteralKind::Hex,
                };
            }
        }

        self.eat_while(|c| c.is_ascii_digit());

        if self.first() == '.' && self.second().is_ascii_digit() {
            self.bump();
            self.eat_while(|c| c.is_ascii_digit());

            if self.first() == 'e' || self.first() == 'E' {
                self.bump();
                if self.first() == '+' || self.first() == '-' {
                    self.bump();
                }
                self.eat_while(|c| c.is_ascii_digit());
            }

            TokenKind::Literal {
                kind: LiteralKind::Float,
            }
        } else if self.first() == 'e' || self.first() == 'E' {
            self.bump();
            if self.first() == '+' || self.first() == '-' {
                self.bump();
            }
            self.eat_while(|c| c.is_ascii_digit());
            TokenKind::Literal {
                kind: LiteralKind::Float,
            }
        } else {
            TokenKind::Literal {
                kind: LiteralKind::Int,
            }
        }
    }

    fn ident_or_note(&mut self) -> TokenKind {
        let start = self.pos;
        self.eat_while(crate::is_ident_continue);
        let ident = self.slice_from(start);

        if ident.eq_ignore_ascii_case("NOTE") {
            let next = self.first();
            if next == ' ' || next == '\t' || next == '\n' || next == '\r' || self.is_eof() {
                self.eat_while(|c| c != '\n' && c != '\r');
                return TokenKind::CommentNote;
            }
        }

        TokenKind::Ident
    }
}
