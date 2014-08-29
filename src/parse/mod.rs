

pub use self::parsing::parse;

mod lex;
mod preproc;
mod parsing;

#[deriving(Show)]
enum Error
{
	IOError(::std::io::IoError),
	BadCharacter(char),
	SyntaxError(String),
}

type ParseResult<T> = Result<T,Error>;

// vim: ft=rust

