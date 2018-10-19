/*
 */
use parse::lex;
use std::collections::HashMap;
use std::default::Default;

trait ReadExt: ::std::io::Read {
	fn chars(self) -> ::utf8reader::UTF8Reader<Self> where Self: Sized;
}
impl<T: ::std::io::Read> ReadExt for T {
	fn chars(self) -> ::utf8reader::UTF8Reader<Self> {
		::utf8reader::UTF8Reader::new(self)
	}
}

#[derive(Default)]
pub struct Preproc
{
	lexers: ::std::vec::Vec<LexHandle>,
	saved_tok: Option<::parse::lex::Token>,
	macros: HashMap<String,Vec<lex::Token>>
}

struct LexHandle
{
	lexer: ::parse::lex::Lexer,
	filename: String,
	line: usize,
}

macro_rules! syntax_assert{ ($tok:expr, $pat:pat => $val:expr) => ({ let v = try!($tok); match v {
	$pat => $val,
	_ => panic!("TODO: Syntax errors, assert {}, got {:?}", stringify!($pat), v)
	}})}

impl Preproc
{
	pub fn new(filename: Option<&::std::path::Path>) -> ::parse::ParseResult<Preproc>
	{
		let lexer = if let Some(filename) = filename
			{
				::parse::lex::Lexer::new(box match ::std::fs::File::open(filename)
					{
					Ok(f) => f.chars(),
					Err(e) => return Err(::parse::Error::IOError(e)),
					})
			}
			else
			{
				::parse::lex::Lexer::new(box ::std::io::stdin().chars())
			};
		Ok(Preproc {
			lexers: vec![ LexHandle {
				lexer: lexer,
				filename: filename.map(|x| x.display().to_string()).unwrap_or_else(|| "-".to_string()),
				line: 1,
				} ],
			.. Default::default()
		})
	}

	pub fn put_back(&mut self, tok: lex::Token)
	{
		assert!( self.saved_tok.is_none() );
		self.saved_tok = Some( tok );
	}
	pub fn get_token(&mut self) -> ::parse::ParseResult<lex::Token>
	{
		if self.saved_tok.is_some()
		{
			let tok = self.saved_tok.take().unwrap();
			debug!("get_token = {:?} (saved)", tok);
			return Ok( tok );
		}
		loop
		{
			let lexer_h = &mut self.lexers.last_mut().unwrap();
			let lexer = &mut lexer_h.lexer;
			match try!(lexer.get_token())
			{
			lex::Token::Newline => {
				lexer_h.line += 1;
				},
			lex::Token::LineComment(_) => {},
			lex::Token::BlockComment(_) => {},
			lex::Token::Hash => {
				match try!(lexer.get_token())
				{
				lex::Token::Ident(name) => {
					panic!("TODO: Preprocessor '{}'", name);
					},
				lex::Token::Integer(line, _) => {
					let file = syntax_assert!(lexer.get_token(), lex::Token::String(s) => s);
					lexer_h.filename = file;
					lexer_h.line = line as usize;
					while try!(lexer.get_token()) != lex::Token::Newline
					{
					}
					
					debug!("Set locaion to \"{}\":{}", lexer_h.filename, line);
					},
				_ => {
					panic!("TODO: Syntax errors");
					},
				}
				},
			lex::Token::Ident(v) => {
				match self.macros.get(&v) {
				Some(macro_def) => panic!("TODO: Macro expansion"),
				_ => {}
				}
				let ret = lex::Token::Ident(v);
				debug!("get_token = {:?}", ret);
				return Ok( ret );
				},
			tok @ _ => {
				debug!("get_token = {:?}", tok);
				return Ok(tok)
				}
			}
		}
	}
}

impl ::std::fmt::Display for Preproc
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		let h = self.lexers.last().unwrap();
		write!(f, "{}:{}: ", h.filename, h.line)
	}
}

// vim: ft=rust

