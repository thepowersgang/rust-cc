/*!
 * C Parser, containing the lexer, pre-processor, and AST generation
 *
 * See `self::parse` for the top-level parser function
 */

// Root parsing function
pub use crate::preproc::Token;

macro_rules! syntax_error
{
	($msg:expr) => ({ return Err(::parse::Error::SyntaxError(format!("{}",$msg))) });
	($fmt:expr, $($arg:tt)*) => ({ return Err(::parse::Error::SyntaxError(format!($fmt, $($arg)*))) });
}
macro_rules! syntax_assert
{
	($lex:expr => Token::$exp:ident) => (syntax_assert!($lex.get_token()?, Token::$exp));
	($lex:expr => Token::$exp:ident($($a:ident),+) @ $v:expr) => (syntax_assert!($lex.get_token()?, Token::$exp($($a),+) => $v));
	($tok:expr, Token::$exp:ident) => (match $tok {
		Token::$exp => {},
		tok @ _ => syntax_error!("Unexpected token {:?}, expected {:?}", tok, stringify!($exp)),
		});
	($tok:expr, Token::$exp:ident($($a:ident),+) => $v:expr) => (match $tok {
		Token::$exp($($a),+) => $v,
		tok @ _ => syntax_error!("Unexpected token {:?}, expected {:?}", tok, stringify!($exp)),
		})
}
macro_rules! peek_token_nc
{
	($lex:expr, $tok:pat) => ({
		let lex = &mut $lex;
		match lex.get_token()? {
		tok @ $tok => { lex.put_back(tok); true },
		tok @ _ => { lex.put_back(tok); false }
		}
		})
}
macro_rules! peek_token
{
	($lex:expr, $tok:pat) => ({
		let lex = &mut $lex;
		match lex.get_token()? {
		$tok => true,
		tok @ _ => { lex.put_back(tok); false }
		}
		});
	($lex:expr, $tok:pat if $cond:expr) => ({
		let lex = &mut $lex;
		match lex.get_token()? {
		$tok if $cond => true,
		tok @ _ => { lex.put_back(tok); false }
		}
		});
}
macro_rules! parse_todo
{
	($str:expr) => (return Err(::parse::Error::Todo($str)))
}

mod parsing;
mod expr;
mod types;

#[derive(Debug)]
pub enum Error
{
	Todo(&'static str),
	EOF,
	IOError(::std::io::Error),
	BadCharacter(char),
	SyntaxError(String),
}
impl From<::preproc::Error> for Error
{
	fn from(v: ::preproc::Error) -> Self
	{
		match v
		{
		::preproc::Error::EOF => Error::EOF,
		::preproc::Error::IoError(e) => Error::IOError(e),
		::preproc::Error::UnexpectedEof => Error::SyntaxError(format!("Unexpected EOF in preprocessor")),
		::preproc::Error::MalformedLiteral(v) => Error::SyntaxError(format!("Malformed literal: {}", v)),
		::preproc::Error::BadCharacter(c) => Error::BadCharacter(c),
		}
	}
}

pub type ParseResult<T> = Result<T,Error>;

struct ParseState<'ast>
{
	ast: &'ast mut ::ast::Program,
	lex: ::preproc::Preproc,
}

/// Parse a file into the passed AST program representation
pub fn parse(ast: &mut ::ast::Program, filename: &::std::path::Path, include_paths: Vec<::std::path::PathBuf>, defines: &[String]) -> ParseResult<()>
{
	let pp_opts = {
		let mut pp_opts = super::preproc::Options::default();
		pp_opts.include_paths = include_paths;
		pp_opts
		};

	let mut self_ = ParseState {
		ast: ast,
		lex: ::preproc::Preproc::new( Some(filename), pp_opts )?,
		};
	
	// Process command-line `-D` arguments
	self_.lex.parse_define_str("__RCC__").unwrap();
	for d in defines
	{
		self_.lex.parse_define_str(d)?;
	}
	
	match self_.parseroot()
	{
	Ok(o) => Ok(o),
	Err(e) => {
		error!("Parse error {:?} at {}", e, self_.lex);
		match e
		{
		::parse::Error::SyntaxError(s) => Err( ::parse::Error::SyntaxError(format!("{} {}", self_.lex, s)) ),
		_ => Err( e ),
		}
		}
	}
}

// vim: ft=rust

