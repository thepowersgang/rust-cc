/*
 */
extern crate libc;

use parse::ParseResult;

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token
{
	Inval(char),
	EOF,
	
	// Ignored Tokens
	LineComment(String),
	BlockComment(String),
	Newline,

	// Leaves
	Integer(u64, ::types::IntClass),
	Float(f64),
	Char(u32),
	String(String),
	Ident(String),
	
	// Symbols
	Hash,
	Tilde,
	Exclamation,
	Period,
	DerefMember,
	Comma,
	Semicolon,
	Star,
	Slash,
	Vargs,
	QuestionMark,
	Colon,
	
	Assign,
	AssignAdd,
	AssignSub,
	AssignMul,
	AssignDiv,
	AssignMod,
	AssignLogicOr,
	AssignLogicAnd,
	AssignBitOr,
	AssignBitAnd,
	
	ShiftRight,
	ShiftLeft,
	
	Equality,
	NotEquals,
	Lt,
	Gt,
	LtE,
	GtE,
	
	Percent,
	Plus,
	Minus,
	DoublePlus,	// ungood (bad joke, sorry)
	DoubleMinus,
	
	Ampersand,
	Pipe,
	Caret,
	DoubleAmpersand,
	DoublePipe,
	
	//
	BraceOpen,
	BraceClose,
	ParenOpen,
	ParenClose,
	SquareOpen,
	SquareClose,
	
	// Reserved Words
	Rword_typedef,
	Rword_auto,
	Rword_extern,
	Rword_static,
	Rword_register,
	Rword_inline,
	Rword_const,
	Rword_volatile,
	// - Types
	Rword_void,
	Rword_Bool,
	Rword_signed,
	Rword_unsigned,
	Rword_char,
	Rword_short,
	Rword_int,
	Rword_long,
	Rword_float,
	Rword_double,
	// - Composites
	Rword_enum,
	Rword_union,
	Rword_struct,
	// - Blocks
	Rword_if,
	Rword_else,
	Rword_while,
	Rword_do,
	Rword_for,
	Rword_switch,
	// - Flow
	Rword_goto,
	Rword_continue,
	Rword_break,
	Rword_return,
	Rword_case,
	Rword_default,
	// - Meta
	Rword_sizeof,
	Rword_gcc_attribute,
	Rword_gcc_va_arg,
}

pub type LexerInput = Box< ::std::iter::Iterator<Item=Result<char,::std::io::CharsError>> + 'static >;

pub struct Lexer
{
	instream: LexerInput,
	lastchar: Option<char>,
}

macro_rules! try_eof {
	($fcn:expr, $eofval:expr) => (
		match $fcn {
		Ok(v) => v,
		Err(::parse::Error::EOF) => return Ok($eofval),
		Err(e) => return Err(e),
		}
		);
}

macro_rules! match_ch {
	($_self:ident, $def:expr, $($p:pat => $v:expr),* ) => (
		match try_eof!($_self.getc(),$def) {
		$($p => $v),*,
		ch @ _ => { $_self.ungetc(ch); $def }
		}
	);
	($_self:ident, $def:expr, $($p:pat => $v:expr),*, ) => (match_ch!($_self,$def,$($p=>$v),*));
}

impl Lexer
{
	pub fn new(instream: LexerInput) -> Lexer {
		Lexer {
			instream: instream,
			lastchar: None,
		}
	}
	
	fn getc(&mut self) -> ParseResult<char>
	{
		if let Some(ch) = self.lastchar.take()
		{
			Ok(ch)
		}
		else if let Some(r) = self.instream.next()
		{
			match r
			{
			Ok(ch) => Ok(ch),
			Err(::std::io::CharsError::NotUtf8) => Err(::parse::Error::BadCharacter('\0')),
			Err(::std::io::CharsError::Other(e)) => Err(::parse::Error::IOError(e)),
			}
		}
		else
		{
			Err(::parse::Error::EOF)
		}
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
			ret.push( ch );
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
			if !(isalnum(ch) || ch == '_' || ch == '$') {
				self.ungetc(ch);
				break;
			}
			name.push( ch );
		}
		return Ok(name);
	}
	// Read a number from the input stream
	fn read_number(&mut self, base: u32) -> ParseResult<u64>
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
	
	fn read_escape(&mut self) -> ParseResult<Option<char>>
	{
		Ok(match try!(self.getc())
		{
		'\\' => Some('\\'),
		'"' => Some('"'),
		'n' => Some('\n'),
		'r' => Some('\r'),
		'0' => Some('\0'),
		'\n' => None,
		c @ _ => panic!("Unexpected escape code in string '\\{}'", c)
		})
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
				match try!(self.read_escape())
				{
				Some(c) => ret.push(c),
				None => {},
				}
			}
			else {
				ret.push( ch );
			}
		}
		return Ok(ret);
	}
	// Read a single-quoted character constant
	fn read_charconst(&mut self) -> ParseResult<u64>
	{
		let mut ret = String::new();
		loop
		{
			let ch = try!(self.getc());
			if ch == '\'' {
				break;
			}
			if ch == '\\' {
				match try!(self.read_escape())
				{
				Some(c) => ret.push(c),
				None => {},
				}
			}
			else {
				ret.push( ch );
			}
		}
		match ret.len()
		{
		0 => Err( ::parse::Error::SyntaxError(format!("Empty chracter constant")) ),
		1 => Ok( ret.as_slice().char_at(0) as u64 ),
		_ => Err( ::parse::Error::SyntaxError(format!("Over-long character constant")) ),
		}
	}
	// Read a single token from the stream
	pub fn get_token(&mut self) -> ParseResult<Token>
	{
		try_eof!(self.eat_whitespace(), Token::EOF);
	
		let mut ch = try_eof!(self.getc(), Token::EOF);
		let ret = match ch
		{
		'\n' => Token::Newline,
		'#' => Token::Hash,
		'~' => Token::Tilde,
		'!' => match_ch!(self, Token::Exclamation,
			'=' => Token::NotEquals,
			),
		';' => Token::Semicolon,
		',' => Token::Comma,
		'?' => Token::QuestionMark,
		':' => Token::Colon,
		'.' => match_ch!(self, Token::Period,
			'.' => {
				if try_eof!(self.getc(), Token::Period) != '.' {
					error!("Lex error '..' hit");
					return Err( ::parse::Error::BadCharacter('.') );
				}
				Token::Vargs
				},
			),
		'=' => match_ch!(self, Token::Assign,
			'=' => Token::Equality,
			),
		'+' => match_ch!(self, Token::Plus,
			'+' => Token::DoublePlus,
			'=' => Token::AssignAdd,
			),
		'-' => match_ch!(self, Token::Minus,
			'-' => Token::DoubleMinus,
			'=' => Token::AssignSub,
			'>' => Token::DerefMember,
			),
		'>' => match_ch!(self, Token::Gt,
			'>' => Token::ShiftRight,
			'=' => Token::GtE,
			),
		'<' => match_ch!(self, Token::Lt,
			'<' => Token::ShiftLeft,
			'=' => Token::LtE,
			),
		'|' => match_ch!(self, Token::Pipe,
			'|' => match_ch!(self, Token::DoublePipe,
				'=' => Token::AssignLogicOr,
				),
			'=' => Token::AssignBitOr,
			),
		'&' => match_ch!(self, Token::Ampersand,
			'&' => match_ch!(self, Token::DoubleAmpersand,
				'=' => Token::AssignLogicAnd,
				),
			'=' => Token::AssignBitAnd,
			),
		'(' => Token::ParenOpen,	')' => Token::ParenClose,
		'{' => Token::BraceOpen,	'}' => Token::BraceClose,
		'[' => Token::SquareOpen,	']' => Token::SquareClose,
		'%' => match_ch!(self, Token::Percent,
			'=' => Token::AssignMod,
			),
		'*' => match_ch!(self, Token::Star,
			'=' => Token::AssignMul,
			),
		'/' => match_ch!(self, Token::Slash,
			'/' => Token::LineComment(try!(self.read_to_eol())),
			'*' => {
				let mut comment = String::new();
				loop {
					match try_eof!(self.getc(), Token::BlockComment(comment)) {
					'*' => {
						match try_eof!(self.getc(), Token::BlockComment(comment)) {
						'/' => break,
						'*' => self.ungetc('*'),	// Handles '**/'
						c @ _ => comment.push(c)
						}
						},
					c @ _ => comment.push(c)
					}
				}
				Token::BlockComment(comment)
				},
			),

		'"' => Token::String( try!(self.read_string()) ),
		'\'' => Token::Integer( try!(self.read_charconst()), ::types::IntClass::Int(false) ),
		
		'0' ... '9' => {
			let (base, whole) = if ch == '0' {
					let ch2 = try_eof!(self.getc(), Token::Integer(0,::types::IntClass::Int(false)));
					match ch2 {
					'1' ... '7' => {
						self.ungetc(ch2);
						(8u, try!(self.read_number( 8)))
						},
					'x' => (16u, try!(self.read_number(16))),
					'b' => ( 2u, try!(self.read_number( 2))),
					_ => {
						self.ungetc(ch2);
						(10u, 0)
						}
					}
				}
				else {
					self.ungetc(ch);
					(10, try!(self.read_number(10)))
				};
			// Check for a decimal point
			let intret = Token::Integer(whole, ::types::IntClass::Int(false));
			ch = try_eof!(self.getc(), intret);
			if ch == '.'
			{
				// Float
				panic!("TODO: lexing floating point values");
			}
			else
			{
				// Integer
				let is_unsigned = if ch=='u'||ch=='U' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_long     = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_longlong = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret); true } else { false };
				self.ungetc(ch);
				Token::Integer( whole, match (is_long,is_longlong) {
					(false,false) => ::types::IntClass::Int(is_unsigned),
					(true, false) => ::types::IntClass::Long(is_unsigned),
					(true, true ) => ::types::IntClass::LongLong(is_unsigned),
					(false, true) => panic!("BUGCHECK: LongLong set, but Long unset")
					} )
			}
			},
		'a'...'z'|'A'...'Z'|'_'|'$' => {
			self.ungetc(ch);
			let ident = try!(self.read_ident());
			match ident.as_slice()
			{
			"typedef" => Token::Rword_typedef,
			"static"  => Token::Rword_static,
			"extern"  => Token::Rword_extern,
			"inline"  => Token::Rword_inline,
			
			"const"    => Token::Rword_const,
			"volatile" => Token::Rword_volatile,
			"signed"   => Token::Rword_signed,
			"unsigned" => Token::Rword_unsigned,
			"void"  => Token::Rword_void,
			"_Bool" => Token::Rword_Bool,
			"char"  => Token::Rword_char,
			"short" => Token::Rword_short,
			"int"   => Token::Rword_int,
			"long"   => Token::Rword_long,
			
			"sizeof" => Token::Rword_sizeof,
			"enum"   => Token::Rword_enum,
			"union"  => Token::Rword_union,
			"struct" => Token::Rword_struct,
			
			"if"    => Token::Rword_if,
			"else"  => Token::Rword_else,
			"do"    => Token::Rword_do,
			"while" => Token::Rword_while,
			"for"   => Token::Rword_for,
			"switch" => Token::Rword_switch,
			
			"case" => Token::Rword_case,
			"default" => Token::Rword_default,
			"return" => Token::Rword_return,
			"break"  => Token::Rword_break,
			"continue" => Token::Rword_continue,
			"goto"   => Token::Rword_goto,
			
			"__attribute__" => Token::Rword_gcc_attribute,
			"__builtin_va_arg" => Token::Rword_gcc_va_arg,
			_ => Token::Ident(ident)
			}
			},
		_ => {
			error!("Bad character #{} hit", ch as u32);
			return Err(::parse::Error::BadCharacter(ch))
			}
		};
		debug!("get_token: {:?}", ret);
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
