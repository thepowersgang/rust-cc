/*!
 * Converts a source file into a stream of tokens
 */
use parse::ParseResult;

use super::token::Token;

pub type LexerInput<'a> = Box< ::std::iter::Iterator<Item=::std::io::Result<char>> + 'a >;

pub struct Lexer<'a>
{
	instream: LexerInput<'a>,
	lastchar: Option<char>,

	/// String currently being captured (for floats/intergers)
	capture: Option<String>,
}
struct CaptureHandle;
impl Drop for CaptureHandle {
	fn drop(&mut self) { panic!("BUG: CaptureHandle not consumed"); }
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

impl<'a> Lexer<'a>
{
	pub fn new(instream: LexerInput<'a>) -> Lexer {
		Lexer {
			instream: instream,
			lastchar: None,
			capture: None,
		}
	}
	
	fn getc(&mut self) -> ParseResult<char>
	{
		let ch = if let Some(ch) = self.lastchar.take()
			{
				ch
			}
			else
			{
				match self.instream.next()
				{
				Some(Ok(ch)) => ch,
				Some(Err(e)) => return Err(::parse::Error::IOError(e)),
				None => return Err(::parse::Error::EOF),
				}
			};
		if let Some(cap) = self.capture.as_mut()
		{
			cap.push(ch)
		}
		Ok(ch)
	}
	fn ungetc(&mut self, ch: char) {
		self.lastchar = Some(ch);
		if let Some(cap) = self.capture.as_mut()
		{
			cap.pop();
		}
	}

	fn start_capture(&mut self, ch: char) -> CaptureHandle {
		self.capture = Some(::std::iter::once(ch).collect());
		CaptureHandle
	}
	fn end_capture(&mut self, handle: CaptureHandle) -> String {
		::std::mem::forget(handle);
		self.capture.take().unwrap()
	}
	
	// Eat as many spaces as possible, returns errors verbatim
	fn eat_whitespace(&mut self) -> ParseResult<usize>
	{
		let mut n = 0;
		loop
		{
			let ch = try!(self.getc());
			if !ch.is_ascii_whitespace() || ch == '\n' {
				self.ungetc(ch);
				break;
			}
			n += 1;
		}
		Ok(n)
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
		self.ungetc('\n');
		return Ok(ret);
	}
	// Read and return a sequence of "identifier" characters
	fn read_ident(&mut self) -> ParseResult<String>
	{
		let mut name = String::new();
		loop
		{
			let ch = try_eof!(self.getc(), name);
			if !(ch.is_alphanumeric() || ch == '_' || ch == '$') {
				self.ungetc(ch);
				break;
			}
			name.push( ch );
		}
		return Ok(name);
	}
	// Read a number from the input stream
	fn read_number_with_len(&mut self, base: u32) -> ParseResult<(u64,usize)>
	{
		let mut val = 0;
		let mut len = 0;
		loop
		{
			let ch = try_eof!( self.getc(), (val,len) );
			match ch.to_digit(base) {
			Some(d) => {
				val *= base as u64;
				val += d as u64;
				len += 1;
				},
			None => {
				self.ungetc(ch);
				return Ok( (val, len) );
				}
			}
		}
	}
	fn read_number(&mut self, base: u32) -> ParseResult<u64>
	{
		Ok( self.read_number_with_len(base)?.0 )
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
		1 => Ok( ret.chars().next().unwrap() as u64 ),
		_ => Err( ::parse::Error::SyntaxError(format!("Over-long character constant")) ),
		}
	}

	pub fn get_token_includestr(&mut self) -> ParseResult<Option<String>>
	{
		try_eof!(self.eat_whitespace(), None);

		let mut ch = try_eof!(self.getc(), None);
		if ch == '<'
		{
			let mut rv = String::new();
			loop
			{
				ch = self.getc()?;
				if ch == '>' {
					break ;
				}
				rv.push(ch);
			}
			Ok( Some(rv) )
		}
		else
		{
			self.ungetc(ch);
			Ok( None )
		}
	}
	// Read a single token from the stream
	pub fn get_token(&mut self) -> ParseResult<Token>
	{
		if try_eof!(self.eat_whitespace(), Token::EOF) > 0 {
			return Ok(Token::Whitespace);
		}
	
		let mut ch = try_eof!(self.getc(), Token::EOF);
		let ret = match ch
		{
		'\n' => Token::Newline,
		'\\' =>
			match try_eof!(self.getc(), Token::EOF)
			{
			'\n' => Token::EscapedNewline,
			ch => { self.ungetc(ch); Token::Backslash },
			},
		'#' => Token::Hash,
		'~' => Token::Tilde,
		'!' => match_ch!(self, Token::Exclamation,
			'=' => Token::NotEquals,
			),
		';' => Token::Semicolon,
		',' => Token::Comma,
		'?' => Token::QuestionMark,
		':' => Token::Colon,
		'^' => Token::Caret,
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
			'=' => Token::AssignDiv,
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
		'\'' => Token::Character( try!(self.read_charconst()) ),
		
		'0' ... '9' => {
			let caph = self.start_capture(ch);
			let (base, whole) = if ch == '0' {
					let ch2 = try_eof!(self.getc(), Token::Integer(0, ::types::IntClass::int(), self.end_capture(caph)));
					match ch2 {
					'1' ... '7' => {
						self.ungetc(ch2);
						(8, self.read_number( 8)?)
						},
					'x' => (16, try!(self.read_number(16))),
					'b' => ( 2, try!(self.read_number( 2))),
					_ => {
						self.ungetc(ch2);
						(10, 0)
						}
					}
				}
				else {
					self.ungetc(ch);
					(10, try!(self.read_number(10)))
				};
			// Check for a decimal point
			let intret = |s: &mut Self, c| Token::Integer(whole, ::types::IntClass::int(), s.end_capture(c));
			ch = try_eof!(self.getc(), intret(self,caph));
			if ch == '.' || (base == 10 && (ch == 'e' || ch == 'E')) || (base == 16 && (ch == 'p' || ch == 'P'))
			{
				// Floating point
				if base != 10 && base != 16 {
					syntax_error!("Only hex and decimal floats are supported");
				}
				let (frac_value, frac_len) = self.read_number_with_len(base)?;
				let (exp_is_neg,exponent) = if match self.getc()
						{
						Ok('e') | Ok('E') if base != 16 => true,
						Ok('p') | Ok('P') if base == 16 => true,
						Ok(ch) => { self.ungetc(ch); false },
						Err(::parse::Error::EOF) => false,
						Err(e) => return Err(e),
						}
					{
						// e, E, p or P were seen, parse the exponent (in base 10)
						let is_neg = match self.getc()
							{
							Ok('-') => true,
							Ok('+') => false,
							Ok(ch) => { self.ungetc(ch); false },
							Err(::parse::Error::EOF) => false,
							Err(e) => return Err(e),
							};
						let (ev, el) = self.read_number_with_len(10)?;
						if el == 0 { syntax_error!("Exponent has no digits"); }
						(is_neg, ev)
					}
					else
					{
						(false, 0)
					};

				let float_val = make_float(base, whole, frac_len, frac_value, exp_is_neg, exponent);
				let ty = match self.getc()
					{
					Ok('f') | Ok('F') => ::types::FloatClass::Float,
					Ok('l') | Ok('L') => ::types::FloatClass::LongDouble,
					Ok(_) | Err(::parse::Error::EOF) => ::types::FloatClass::Double,
					Err(e) => return Err(e),
					};
				Token::Float( float_val, ty, self.end_capture(caph) )
			}
			else
			{
				// Integer
				let is_unsigned = ::types::Signedness::from_bool_signed(if ch=='u'||ch=='U' { ch = try_eof!(self.getc(), intret(self,caph)); true } else { false });
				let is_long     = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret(self,caph)); true } else { false };
				let is_longlong = if ch=='l'||ch=='L' { ch = try_eof!(self.getc(), intret(self,caph)); true } else { false };
				self.ungetc(ch);
				Token::Integer( whole, match (is_long,is_longlong) {
					(false,false) => ::types::IntClass::Int(is_unsigned),
					(true, false) => ::types::IntClass::Long(is_unsigned),
					(true, true ) => ::types::IntClass::LongLong(is_unsigned),
					(false, true) => panic!("BUGCHECK: LongLong set, but Long unset")
					}, self.end_capture(caph) )
			}
			},
		'a'...'z'|'A'...'Z'|'_'|'$' => {
			self.ungetc(ch);
			Token::Ident( try!(self.read_ident()) )
			},
		_ => {
			error!("Bad character #{} hit", ch as u32);
			return Err(::parse::Error::BadCharacter(ch))
			}
		};
		trace!("get_token: {:?}", ret);
		Ok(ret)
	}
}

pub(super) fn map_keywords(tok: Token) -> Token
{
	match tok
	{
	Token::Ident(ident) => match &ident[..]
		{
		"typedef" => Token::Rword_typedef,
		"static"  => Token::Rword_static,
		"extern"  => Token::Rword_extern,
		"inline"  => Token::Rword_inline,
		
		"const"    => Token::Rword_const,
		"volatile" => Token::Rword_volatile,
		"restrict" => Token::Rword_restrict,

		"auto"     => Token::Rword_auto,
		"register" => Token::Rword_register,
		"signed"   => Token::Rword_signed,
		"unsigned" => Token::Rword_unsigned,
		"void"  => Token::Rword_void,
		"_Bool" => Token::Rword_Bool,
		"char"  => Token::Rword_char,
		"short" => Token::Rword_short,
		"int"   => Token::Rword_int,
		"long"  => Token::Rword_long,
		"float"  => Token::Rword_float,
		"double" => Token::Rword_double,
		
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
		
		_ => Token::Ident(ident)
		},
	t => t,
	}
}

fn make_float(base: u32, whole: u64, frac_len: usize, frac_value: u64, exp_is_neg: bool, exponent: u64) -> f64 //Float32_128
{
	if frac_value == 0 && exponent == 0 {
		whole as f64
	}
	else {
		// 1. Convert the fraction value into an ecoded fraction. (5 => 0x5 => 0x8, 123 => 0x7B => 0x0214D)
		panic!("TODO: Create floating point values (base={}, whole={},frac={}:{},exponent={}{}", base, whole, frac_len, frac_value, if exp_is_neg { "-" } else { "" }, exponent);
	}
}

// vim: ft=rust
