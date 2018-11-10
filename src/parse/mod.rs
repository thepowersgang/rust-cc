/*!
 * C Parser, containing the lexer, pre-processor, and AST generation
 *
 * See `self::parse` for the top-level parser function
 */

// Root parsing function
pub use self::parsing::parse;
pub use self::token::Token;

macro_rules! syntax_error
{
	($msg:expr) => ({ return Err(::parse::Error::SyntaxError(format!("{}",$msg))) });
	($fmt:expr, $($arg:tt)*) => ({ return Err(::parse::Error::SyntaxError(format!($fmt, $($arg)*))) });
}
macro_rules! syntax_assert
{
	($lex:expr => Token::$exp:ident) => (syntax_assert!(try!($lex.get_token()), Token::$exp));
	($lex:expr => Token::$exp:ident($($a:ident),+) @ $v:expr) => (syntax_assert!(try!($lex.get_token()), Token::$exp($($a),+) => $v));
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
		match try!(lex.get_token()) {
		tok @ $tok => { lex.put_back(tok); true },
		tok @ _ => { lex.put_back(tok); false }
		}
		})
}
macro_rules! peek_token
{
	($lex:expr, $tok:pat) => ({
		let lex = &mut $lex;
		match try!(lex.get_token()) {
		$tok => true,
		tok @ _ => { lex.put_back(tok); false }
		}
		})
}
macro_rules! parse_todo
{
	($str:expr) => (return Err(::parse::Error::Todo($str)))
}

mod lex;
mod preproc;
mod parsing;
mod types;
pub mod token;

#[derive(Debug)]
pub enum Error
{
	Todo(&'static str),
	EOF,
	IOError(::std::io::Error),
	BadCharacter(char),
	SyntaxError(String),
}

#[must_use]
pub type ParseResult<T> = Result<T,Error>;

struct ParseState<'ast>
{
	ast: &'ast mut ::ast::Program,
	lex: ::parse::preproc::Preproc,
}

// vim: ft=rust

