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
	TokTilde,
	TokExclamation,
	TokPeriod,
	TokDerefMember,
	TokComma,
	TokSemicolon,
	TokStar,
	TokSlash,
	TokVargs,
	TokQuestionMark,
	TokColon,
	
	TokAssign,
	TokAssignAdd,
	TokAssignSub,
	TokAssignMul,
	TokAssignDiv,
	TokAssignMod,
	TokAssignLogicOr,
	TokAssignLogicAnd,
	TokAssignBitOr,
	TokAssignBitAnd,
	
	TokShiftRight,
	TokShiftLeft,
	
	TokEquality,
	TokNotEquals,
	TokLt,
	TokGt,
	TokLtE,
	TokGtE,
	
	TokPercent,
	TokPlus,
	TokMinus,
	TokDoublePlus,	// ungood (bad joke, sorry)
	TokDoubleMinus,
	
	TokAmpersand,
	TokPipe,
	TokCaret,
	TokDoubleAmpersand,
	TokDoublePipe,
	
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
	TokRword_switch,
	// - Flow
	TokRword_goto,
	TokRword_continue,
	TokRword_break,
	TokRword_return,
	TokRword_case,
	TokRword_default,
	// - Meta
	TokRword_sizeof,
	TokRword_gcc_attribute,
	TokRword_gcc_va_arg,
}

pub struct Lexer
{
	instream: Box<Reader+'static>,

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
	pub fn new(instream: Box<Reader+'static>) -> Lexer {
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
		c @ _ => fail!("Unexpected escape code in string '\\{}'", c)
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
		0 => Err( ::parse::SyntaxError(format!("Empty chracter constant")) ),
		1 => Ok( ret.as_slice().char_at(0) as u64 ),
		_ => Err( ::parse::SyntaxError(format!("Over-long character constant")) ),
		}
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
		'~' => TokTilde,
		'!' => match_ch!(self, TokExclamation,
			'=' => TokNotEquals,
			),
		';' => TokSemicolon,
		',' => TokComma,
		'?' => TokQuestionMark,
		':' => TokColon,
		'.' => match_ch!(self, TokPeriod,
			'.' => {
				if try_eof!(self.getc(), TokPeriod) != '.' {
					error!("Lex error '..' hit");
					return Err( ::parse::BadCharacter('.') );
				}
				TokVargs
				},
			),
		'=' => match_ch!(self, TokAssign,
			'=' => TokEquality,
			),
		'+' => match_ch!(self, TokPlus,
			'+' => TokDoublePlus,
			'=' => TokAssignAdd,
			),
		'-' => match_ch!(self, TokMinus,
			'-' => TokDoubleMinus,
			'=' => TokAssignSub,
			'>' => TokDerefMember,
			),
		'>' => match_ch!(self, TokGt,
			'>' => TokShiftRight,
			'=' => TokGtE,
			),
		'<' => match_ch!(self, TokLt,
			'<' => TokShiftLeft,
			'=' => TokLtE,
			),
		'|' => match_ch!(self, TokPipe,
			'|' => match_ch!(self, TokDoublePipe,
				'=' => TokAssignLogicOr,
				),
			'=' => TokAssignBitOr,
			),
		'&' => match_ch!(self, TokAmpersand,
			'&' => match_ch!(self, TokDoubleAmpersand,
				'=' => TokAssignLogicAnd,
				),
			'=' => TokAssignBitAnd,
			),
		'(' => TokParenOpen,	')' => TokParenClose,
		'{' => TokBraceOpen,	'}' => TokBraceClose,
		'[' => TokSquareOpen,	']' => TokSquareClose,
		'%' => match_ch!(self, TokPercent,
			'=' => TokAssignMod,
			),
		'*' => match_ch!(self, TokStar,
			'=' => TokAssignMul,
			),
		'/' => match_ch!(self, TokSlash,
			'/' => TokLineComment(try!(self.read_to_eol())),
			'*' => {
				let mut comment = String::new();
				loop {
					match try_eof!(self.getc(), TokBlockComment(comment)) {
					'*' => {
						match try_eof!(self.getc(), TokBlockComment(comment)) {
						'/' => break,
						'*' => self.ungetc('*'),	// Handles '**/'
						c @ _ => comment.push(c)
						}
						},
					c @ _ => comment.push(c)
					}
				}
				TokBlockComment(comment)
				},
			),

		'"' => TokString( try!(self.read_string()) ),
		'\'' => TokInteger( try!(self.read_charconst()), ::types::IntClass_Int(false) ),
		
		'0' ... '9' => {
			let (base, whole) = if ch == '0' {
					let ch2 = try_eof!(self.getc(), TokInteger(0,::types::IntClass_Int(false)));
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
				let is_unsigned = if ch=='u'||ch=='U' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_long     = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret); true } else { false };
				let is_longlong = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret); true } else { false };
				self.ungetc(ch);
				TokInteger( whole, match (is_long,is_longlong) {
					(false,false) => ::types::IntClass_Int(is_unsigned),
					(true, false) => ::types::IntClass_Long(is_unsigned),
					(true, true ) => ::types::IntClass_LongLong(is_unsigned),
					(false, true) => fail!("BUGCHECK: LongLong set, but Long unset")
					} )
			}
			},
		'a'...'z'|'A'...'Z'|'_'|'$' => {
			self.ungetc(ch);
			let ident = try!(self.read_ident());
			match ident.as_slice()
			{
			"typedef" => TokRword_typedef,
			"static"  => TokRword_static,
			"extern"  => TokRword_extern,
			"inline"  => TokRword_inline,
			
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
			
			"sizeof" => TokRword_sizeof,
			"enum"   => TokRword_enum,
			"union"  => TokRword_union,
			"struct" => TokRword_struct,
			
			"if"    => TokRword_if,
			"else"  => TokRword_else,
			"do"    => TokRword_do,
			"while" => TokRword_while,
			"for"   => TokRword_for,
			"switch" => TokRword_switch,
			
			"case" => TokRword_case,
			"default" => TokRword_default,
			"return" => TokRword_return,
			"break"  => TokRword_break,
			"continue" => TokRword_continue,
			"goto"   => TokRword_goto,
			
			"__attribute__" => TokRword_gcc_attribute,
			"__builtin_va_arg" => TokRword_gcc_va_arg,
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
