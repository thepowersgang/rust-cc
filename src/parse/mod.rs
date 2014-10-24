

pub use self::parsing::parse;

mod lex;
mod preproc;
mod parsing;

#[deriving(Show)]
enum Error
{
	Todo(&'static str),
	IOError(::std::io::IoError),
	BadCharacter(char),
	SyntaxError(String),
}

#[must_use]
type ParseResult<T> = Result<T,Error>;

// vim: ft=rust

