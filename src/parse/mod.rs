

pub use self::parsing::parse;

mod lex;
mod preproc;
mod parsing;

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

// vim: ft=rust

