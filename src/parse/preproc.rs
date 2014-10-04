/*
 */
use parse::lex;

pub struct Preproc
{
	lexers: ::std::vec::Vec<LexHandle>,
	saved_tok: Option<::parse::lex::Token>,
	macros: ::std::collections::TreeMap<String,Vec<lex::Token>>
}

struct LexHandle
{
	lexer: ::parse::lex::Lexer,
	filename: String,
	line: uint,
}

macro_rules! syntax_assert( ($tok:expr, $pat:pat => $val:expr) => ({ let v = try!($tok); match v {
	$pat => $val,
	_ => fail!("TODO: Syntax errors, assert {}, got {}", stringify!($pat), v)
	}}))

impl Preproc
{
	pub fn new(filename: &str) -> ::parse::ParseResult<Preproc>
	{
		let file_stream = if filename == "-"
			{
				box ::std::io::stdio::stdin() as Box<Reader>
			}
			else
			{
				box match ::std::io::File::open(&::std::path::Path::new(filename))
				{
				Ok(f) => f,
				Err(e) => return Err(::parse::IOError(e)),
				} as Box<Reader>
			};
		Ok(Preproc {
			lexers: vec![ LexHandle {
				lexer: ::parse::lex::Lexer::new(file_stream),
				filename: filename.to_string(),
				line: 1,
				} ],
			saved_tok: None,
			macros: ::std::collections::TreeMap::new(),
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
			let tok = self.saved_tok.take_unwrap();
			debug!("get_token = {} (saved)", tok);
			return Ok( tok );
		}
		loop
		{
			let lexer_h = &mut self.lexers.mut_last().unwrap();
			let lexer = &mut lexer_h.lexer;
			let tok = try!(lexer.get_token());
			match tok
			{
			lex::TokNewline => {
				lexer_h.line += 1;
				},
			lex::TokLineComment(_) => {},
			lex::TokBlockComment(_) => {},
			lex::TokHash => {
				match try!(lexer.get_token())
				{
				lex::TokIdent(name) => {
					fail!("TODO: Preprocessor '{}'", name);
					},
				lex::TokInteger(line, _) => {
					let file = syntax_assert!(lexer.get_token(), lex::TokString(s) => s);
					lexer_h.filename = file;
					lexer_h.line = line as uint;
					while try!(lexer.get_token()) != lex::TokNewline
					{
					}
					
					debug!("Set locaion to \"{}\":{}", lexer_h.filename, line);
					},
				_ => {
					fail!("TODO: Syntax errors");
					},
				}
				},
			lex::TokIdent(v) => {
				match self.macros.find(&v) {
				Some(macro) => fail!("TODO: Macro expansion"),
				_ => {}
				}
				let ret = lex::TokIdent(v);
				debug!("get_token = {}", ret);
				return Ok( ret );
				},
			_ => {
				debug!("get_token = {}", tok);
				return Ok(tok)
				}
			}
		}
	}
}

// vim: ft=rust

