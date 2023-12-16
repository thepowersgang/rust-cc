#[derive(Debug)]
pub enum Token<'a>
{
    Ident(&'a str),
    String(StringLit<'a>),
    Integer(u128),
    Float(f64),
    Sym(&'a str),
}
#[derive(Debug)]
pub struct StringLit<'a>(&'a str);
impl<'a> StringLit<'a> {
    pub fn parse_string(&self) -> String {
        match String::from_utf8(self.parse_bytes()) {
        Err(e) => panic!("Malformed String : \"{}\" - {:?}", self.0, e),
        Ok(rv) => rv,
        }
    }
    pub fn parse_bytes(&self) -> Vec<u8> {
        let mut rv = Vec::new();
        let mut it = self.0.split('\\');
        let c = it.next().unwrap();
        rv.extend(c.as_bytes().iter().copied());
        while let Some(c) = it.next() {
            let (ch,next) = match c.as_bytes() {
                [] => (b'\\',"".as_bytes()),
                &[b'n', ref rest @ ..] => (b'\n', rest),
                &[b'0', ref rest @ ..] => (b'\0', rest),
                &[b'x', b1, b2, ref rest @ ..] => {
                    let v1 = (b1 as char).to_digit(16).unwrap() as u8;
                    let v2 = (b2 as char).to_digit(16).unwrap() as u8;
                    (v1 * 16 + v2, rest)
                },
                &[b, ..] => todo!("\\{}", b as char),
                };
            rv.push(ch);
            rv.extend(next.iter().copied());
        }
        rv
    }
}

#[derive(Clone)]
pub struct Lexer<'p,'a> {
    path: &'p ::std::path::Path,
    rem: &'a str,
    init: &'a str,
    line: usize
}
impl<'p, 'a> Lexer<'p, 'a> {
    pub fn new(path: &'p ::std::path::Path, v: &'a str) -> Self {
        Lexer { path, rem: v, init: v, line: 1 }
    }

    pub fn get_tok(&mut self) -> Option<Token<'a>> {
        while let Some(_) = self.consume_comment() {
        }
        if false
        {
            let line_count = {
                let len = self.rem.as_ptr() as usize - self.init.as_ptr() as usize;
                1 + self.init[..len].bytes().filter(|&b| b == b'\n').count()
                };
            debug_assert!(line_count == self.line, "{}: Line count diverge: {} != {}", self, line_count, self.line);
        }
        Some(match self.rem.as_bytes()
        {
        [] => return None,
        [b'0', b'x', ..] => todo!("{self}: hex numbers"),
        [b'0', b'b', ..] => todo!("{self}: binary numbers"),
        [b'0', b'o', ..] => todo!(),
        [b'0'..=b'9', ..] => {
            let len = self.rem.as_bytes().iter().position(|&b| !b.is_ascii_digit()).unwrap_or(self.rem.len());
            if let [b'.', b'0'..=b'9', ..] = self.rem.as_bytes()[len..] {
                let len2 = self.rem[len+1..].as_bytes().iter().position(|&b| !b.is_ascii_digit()).unwrap_or(self.rem.len());
                assert!(len2 > 0);
                let s = self.consume(len+1+len2);
                Token::Float(s.parse().unwrap())
            }
            else {
                let s = self.consume(len);
                Token::Integer(u128::from_str_radix(s, 10).unwrap())
            }
        },
        [b'a'..=b'z'|b'A'..=b'Z'|b'_'|0x80.., ..] => {
            let len = self.rem.as_bytes().iter().position(|&b| !(b.is_ascii_alphanumeric() || b == b'_' || b >= 0x80)).unwrap_or(self.rem.len());
            Token::Ident( self.consume(len) )
        },

        [b'"', ..] => {
            self.consume(1);
            let end = 'o: {
                let mut prev = b' ';
                for (i,b) in self.rem.bytes().enumerate() {
                    prev = match b {
                        b'"' if prev != b'\\' => break 'o i,
                        b'\\' if prev == b'\\' => { b' ' },
                        b => b,
                        };
                }
                self.rem.len()
            };
            let rv = self.consume(end);
            self.consume(1);    // Eat the closing double-quote
            Token::String(StringLit(rv))
        },

        [b'.',b'.',b'.', ..] => Token::Sym(self.consume(3)),

        [b'-', b'>', ..] => Token::Sym(self.consume(2)),    // Thin arrow
        [b'=', b'>', ..] => Token::Sym(self.consume(2)),    // Fat arrow, is this used?
        [b'=', b'=', ..] => Token::Sym(self.consume(2)),    // Equality
        [b'!', b'=', ..] => Token::Sym(self.consume(2)),    // Inequality
        [b'<', b'=', ..] => Token::Sym(self.consume(2)),    // LE
        [b'>', b'=', ..] => Token::Sym(self.consume(2)),    // GE
        [b'<', b'<', ..] => Token::Sym(self.consume(2)),    // SHL
        [b'>', b'>', ..] => Token::Sym(self.consume(2)),    // SHR

        [b';'|b':', ..] => Token::Sym(self.consume(1)),
        [b','|b'.', ..] => Token::Sym(self.consume(1)),
        [b'{'|b'}', ..] => Token::Sym(self.consume(1)),
        [b'['|b']', ..] => Token::Sym(self.consume(1)),
        [b'('|b')', ..] => Token::Sym(self.consume(1)),
        [b'-'|b'+', ..] => Token::Sym(self.consume(1)),
        [b'|'|b'^', ..] => Token::Sym(self.consume(1)),
        [b'*'|b'/', ..] => Token::Sym(self.consume(1)),
        [b'<'|b'>', ..] => Token::Sym(self.consume(1)),
        [b'@', ..] => Token::Sym(self.consume(1)),
        [b'&', ..] => Token::Sym(self.consume(1)),
        [b'=', ..] => Token::Sym(self.consume(1)),
        [b'!', ..] => Token::Sym(self.consume(1)),
        &[byte, ..] => todo!("{self}: Unexpected byte {:#x} ({:?})", byte, byte as char)
        })
    }
    fn consume(&mut self, count: usize) -> &'a str {
        let rv = &self.rem[..count];
        self.rem = &self.rem[count..];
        rv
    }
    pub fn get_tok_noeof(&mut self) -> Token<'a> {
        match self.get_tok() {
        Some(v) => v,
        None => panic!("{self}: Unexpected EOF"),
        }
    }
    pub fn consume_comment(&mut self) -> Option<&'a str> {
        self.consume_whitespace();
        if self.rem.starts_with("//") {
            self.rem = &self.rem[2..];
            let end = self.rem.bytes().position(|b| b == b'\n' || b == b'\r').unwrap_or(self.rem.len());
            let rv = &self.rem[..end];
            self.rem = &self.rem[end..];
            //println!(">> {} {:?}", self, &self.rem[..5]);
            Some(rv)
        }
        else if self.rem.starts_with("/*") {
            self.rem = &self.rem[2..];
            let end = 'o: {
                let mut level = 0;
                let mut prev = b' ';
                for (i,b) in self.rem.bytes().enumerate() {
                    prev = match b {
                        b'/' if prev == b'*' => if level == 0 {
                            break 'o i
                        }
                        else {
                            level -= 1;
                            b' '
                        },
                        b'*' if prev == b'/' => { level += 1; b' ' },
                        b @ b'\n' => {
                            self.line += 1;
                            b
                        },
                        b => b,
                        };
                }
                self.rem.len()
            };
            let rv = &self.rem[..end - 1]; // Minus 1, because `[end]` is the closing `/`
            self.rem = &self.rem[end+1..];
            //println!(">> {} {:?}", self, &self.rem[..5]);
            Some(rv)
        }
        else {
            None
        }
    }
    fn consume_whitespace(&mut self) {
        let mut it = self.rem.chars();
        loop {
            match it.next()
            {
            Some(c) if c.is_whitespace() => {
                if c == '\n' {
                    self.line += 1;
                }
                self.rem = it.as_str();
            }
            _ => break,
            }
        }
    }
}
#[allow(dead_code)]
impl<'a> Lexer<'_, 'a> {
    pub fn consume_if_str(&mut self) -> Option<StringLit<'a>> {
        let mut s = self.clone();
        if let Token::String(rv) = s.get_tok_noeof() {
            *self = s;
            Some(rv)
        }
        else {
            None
        }
    }
    pub fn consume_if_int(&mut self) -> Option<u128> {
        let mut s = self.clone();
        if let Token::Integer(rv) = s.get_tok_noeof() {
            *self = s;
            Some(rv)
        }
        else {
            None
        }
    }
    pub fn consume_if_ident(&mut self) -> Option<&'a str> {
        let mut s = self.clone();
        if let Token::Ident(rv) = s.get_tok_noeof() {
            *self = s;
            Some(rv)
        }
        else {
            None
        }
    }
    pub fn consume_if_keyword(&mut self, i: &str) -> bool {
        let mut s = self.clone();
        match s.get_tok_noeof()
        {
        Token::Ident(rv) if rv == i => {
            *self = s;
            true
            },
        _ => false,
        }
    }
    pub fn consume_if_sym(&mut self, i: &str) -> bool {
        let mut s = self.clone();
        match s.get_tok_noeof()
        {
        Token::Sym(rv) if rv == i => {
            *self = s;
            true
            },
        _ => false,
        }
    }
}
impl<'a> Lexer<'_, 'a> {
    #[track_caller]
    pub fn consume_str(&mut self) -> StringLit<'a> {
        match self.get_tok_noeof() {
        Token::String(rv) => rv,
        t => panic!("{self}: Unexpected {t:?}, expected String"),
        }
    }
    #[track_caller]
    pub fn consume_int(&mut self) -> u128 {
        match self.get_tok_noeof() {
        Token::Integer(rv) => rv,
        t => panic!("{self}: Unexpected {t:?}, expected Integer"),
        }
    }
    #[track_caller]
    pub fn consume_ident(&mut self) -> &'a str {
        match self.get_tok_noeof() {
        Token::Ident(rv) => rv,
        t => panic!("{self}: Unexpected {t:?}, expected Identifier"),
        }
    }
    #[track_caller]
    pub fn consume_keyword(&mut self, i: &str) {
        match self.get_tok_noeof() {
        Token::Ident(rv) if rv == i => {},
        t => panic!("{self}: Unexpected {t:?}, expected Ident({i:?})"),
        }
    }
    #[track_caller]
    pub fn consume_sym(&mut self, i: &str) {
        match self.get_tok_noeof() {
        Token::Sym(rv) if rv == i => {},
        t => panic!("{self}: Unexpected {t:?}, expected Sym({i:?})"),
        }
    }
}

impl ::std::fmt::Display for Lexer<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.path.display(), self.line)
    }
}