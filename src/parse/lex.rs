/*
 */
extern crate libc;

use parse::ParseResult;

#[allow(non_camel_case_types)]
#[deriving(Show)]
#[deriving(PartialEq)]
pub enum Token
{
	TokInval(char),
	TokEOF,
	
	// Ignored Tokens
	TokLineComment(String),
	TokBlockComment(String),
	TokNewline,

	// Leaves
	TokInteger(u64, ::types::IntClass),
	TokFloat(f64),
	TokChar(u32),
	TokString(String),
	TokIdent(String),
	
	// Symbols
	TokHash,
	TokPeriod,
	TokComma,
	TokSemicolon,
	TokStar,
	TokSlash,
	TokAssign,
	TokVargs,
	
	//
	TokBraceOpen,
	TokBraceClose,
	TokParenOpen,
	TokParenClose,
	TokSquareOpen,
	TokSquareClose,
	
	// Reserved Words
	TokRword_typedef,
	TokRword_auto,
	TokRword_extern,
	TokRword_static,
	TokRword_register,
	TokRword_inline,
	TokRword_const,
	TokRword_volatile,
	// - Types
	TokRword_void,
	TokRword_Bool,
	TokRword_signed,
	TokRword_unsigned,
	TokRword_char,
	TokRword_short,
	TokRword_int,
	TokRword_long,
	TokRword_float,
	TokRword_double,
	// - Composites
	TokRword_enum,
	TokRword_union,
	TokRword_struct,
	// - Blocks
	TokRword_if,
	TokRword_else,
	TokRword_while,
	TokRword_do,
	TokRword_for,
	// - Flow
	TokRword_goto,
	TokRword_continue,
	TokRword_break,
	TokRword_return,
}

pub struct Lexer
{
	instream: Box<Reader>,

	lastchar: Option<char>,
}

macro_rules! try_eof( ($fcn:expr, $eofval:expr) => (
	match $fcn {
	Ok(v) => v,
	Err(e) => match e {
		::parse::IOError(ioe) => match ioe.kind {
			::std::io::EndOfFile => return Ok($eofval),
			_ => return Err(::parse::IOError(ioe)),
			},
		_ => return Err(e),
		},
	}
))

impl Lexer
{
	pub fn new(instream: Box<Reader>) -> Lexer {
		Lexer {
			instream: instream,
			
			lastchar: None,
		}
	}
	
	fn getc(&mut self) -> ParseResult<char> {
		let ret = match self.lastchar {
			Some(ch) => ch,
			None => (match self.instream.read_byte() {
				Ok(ch) => ch,
				Err(e) => return Err(::parse::IOError(e))
				} as char)
			};
		self.lastchar = None;
		return Ok(ret)
	}
	fn ungetc(&mut self, ch: char) {
		self.lastchar = Some(ch)
	}
	
	// Eat as many spaces as possible, returns errors verbatim
	fn eat_whitespace(&mut self) -> ParseResult<()>
	{
		loop
		{
			let ch = try!(self.getc());
			if !isspace(ch) || ch == '\n' {
				self.ungetc(ch);
				break;
			}
		}
		Ok(())
	}
	// Read and return the rest of the line
	// - Eof is converted to return value
	fn read_to_eol(&mut self) -> ParseResult<String>
	{
		let mut ret = String::new();
		loop
		{
			let ch = try_eof!(self.getc(), ret);
			if ch == '\n' { break; }
			ret.push_char( ch );
		}
		return Ok(ret);
	}
	// Read and return a sequence of "identifier" characters
	fn read_ident(&mut self) -> ParseResult<String>
	{
		let mut name = String::new();
		loop
		{
			let ch = try_eof!(self.getc(), name);
			if !(isalnum(ch) || ch == '_') {
				self.ungetc(ch);
				break;
			}
			name.push_char( ch );
		}
		return Ok(name);
	}
	// Read a number from the input stream
	fn read_number(&mut self, base: uint) -> ParseResult<u64>
	{
		let mut val = 0;
		loop
		{
			let ch = try_eof!(self.getc(), val);
			match ch.to_digit(base) {
			Some(d) => {
				val *= base as u64;
				val += d as u64
				},
			None => {
				self.ungetc(ch);
				break;
				}
			}
		}
		return Ok(val);
	}
	// Read a double-quoted string
	// - NOTE: has no EOF processing, as an EOF in a double-quoted string is invalid
	fn read_string(&mut self) -> ParseResult<String>
	{
		let mut ret = String::new();
		loop
		{
			let ch = try!(self.getc());
			if ch == '\"' {
				break;
			}
			if ch == '\\' {
				let codechar = try!(self.getc());
				match codechar {
				'\\' => ret.push_char('\\'),
				'"' => ret.push_char('"'),
				'n' => ret.push_char('\n'),
				'r' => ret.push_char('\r'),
				'\n' => (),
				_ => fail!("Unexpected escape code in string '\\{}'", codechar)
				}
			}
			ret.push_char( ch );
		}
		return Ok(ret);
	}
	// Read a single token from the stream
	pub fn get_token(&mut self) -> ParseResult<Token>
	{
		try_eof!(self.eat_whitespace(), TokEOF);
	
		let mut ch = try_eof!(self.getc(), TokEOF);
		let ret = match ch
		{
		'\n' => TokNewline,
		'#' => TokHash,
		';' => TokSemicolon,
		',' => TokComma,
		'.' => {
			match try_eof!(self.getc(), TokPeriod)
			{
			'.' => {
				if try_eof!(self.getc(), TokPeriod) != '.' {
					error!("Lex error '..' hit");
					return Err( ::parse::BadCharacter('.') );
				}
				TokVargs
				},
			ch @ _ => {
				self.ungetc(ch);
				TokPeriod
				}
			}
			},
		'(' => TokParenOpen,	')' => TokParenClose,
		'{' => TokBraceOpen,	'}' => TokBraceClose,
		'[' => TokSquareOpen,	']' => TokSquareClose,
		'*' => TokStar,
		'/' => {
			ch = try_eof!(self.getc(), TokSlash);
			match ch {
			'/' => TokLineComment(try!(self.read_to_eol())),
			'*' => {
				let mut comment = String::new();
				loop {
					match try_eof!(self.getc(), TokBlockComment(comment)) {
					'*' => {
						match try_eof!(self.getc(), TokBlockComment(comment)) {
						'/' => break,
						c @ _ => comment.push_char(c)
						}
						},
					c @ _ => comment.push_char(c)
					}
				}
				TokBlockComment(comment)
				},
			_ => {
				self.ungetc(ch);
				TokSlash
				},
			}
			}
		'"' => TokString( try!(self.read_string()) ),
		
		'0' .. '9' => {
			let (base, whole) = if ch == '0' {
					let ch2 = try_eof!(self.getc(), TokInteger(0,::types::IntClass_Int(false)));
					match ch2 {
					'1' .. '7' => {
						self.ungetc(ch);
						(8u, try!(self.read_number( 8)))
						},
					'x' => (16u, try!(self.read_number(16))),
					'b' => ( 2u, try!(self.read_number( 2))),
					_ => {
						self.ungetc(ch);
						(10u, 0)
						}
					}
				}
				else {
					self.ungetc(ch);
					(10, try!(self.read_number(10)))
				};
			// Check for a decimal point
			let intret = TokInteger(whole, ::types::IntClass_Int(false));
			ch = try_eof!(self.getc(), intret);
			if ch == '.'
			{
				// Float
				fail!("TODO: lexing floating point values");
			}
			else
			{
				// Integer
				let is_unsigned = if ch == 'u' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_long     = if ch == 'l' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_longlong = if ch == 'l' { ch = try_eof!(self.getc(), intret); true } else { false };
				self.ungetc(ch);
				TokInteger( whole, match (is_long,is_longlong) {
					(false,false) => ::types::IntClass_Int(is_unsigned),
					(true, false) => ::types::IntClass_Long(is_unsigned),
					(true, true ) => ::types::IntClass_LongLong(is_unsigned),
					(false, true) => fail!("BUGCHECK: LongLong set, but Long unset")
					} )
			}
			},
		'a'..'z'|'A'..'Z'|'_' => {
			self.ungetc(ch);
			let ident = try!(self.read_ident());
			match ident.as_slice()
			{
			"typedef" => TokRword_typedef,
			"static"  => TokRword_static,
			"extern"  => TokRword_extern,
			
			"const"    => TokRword_const,
			"volatile" => TokRword_volatile,
			"signed"   => TokRword_signed,
			"unsigned" => TokRword_unsigned,
			"void"  => TokRword_void,
			"_Bool" => TokRword_Bool,
			"char"  => TokRword_char,
			"short" => TokRword_short,
			"int"   => TokRword_int,
			"long"   => TokRword_long,
			
			"enum"   => TokRword_enum,
			"union"  => TokRword_union,
			"struct" => TokRword_struct,
			_ => TokIdent(ident)
			}
			},
		_ => {
			error!("Bad character #{} hit", ch as u32);
			return Err(::parse::BadCharacter(ch))
			}
		};
		debug!("get_token: {}", ret);
		Ok(ret)
	}
}

fn isspace(ch: char) -> bool {
	unsafe {
		return libc::funcs::c95::ctype::isspace(ch as i32) != 0
	}
}
fn isalnum(ch: char) -> bool {
	unsafe {
		return libc::funcs::c95::ctype::isalnum(ch as i32) != 0
	}
}

// vim: ft=rust
