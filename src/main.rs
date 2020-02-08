/*!
 * C parser (and eventual) compiler
 */
#![feature(box_syntax)]
#![feature(nll)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate utf8reader;
#[macro_use]
extern crate structopt;

mod preproc;
mod parse;
mod types;
mod ast;


#[derive(StructOpt)]
struct Options
{
	#[structopt(parse(from_os_str))]
	input: ::std::path::PathBuf,

	#[structopt(short="I",parse(from_os_str))]
	include_dirs: Vec<::std::path::PathBuf>,

	/// `-D FOO=bar`
	#[structopt(short="D")]
	defines: Vec<String>,
}

fn main()
{
	env_logger::init();

	// 1. Parse command line arguments
	let args: Options = ::structopt::StructOpt::from_args();
	
	let mut program = ::ast::Program::new();

	// - Parse into the AST
	match ::parse::parse(&mut program, &args.input, args.include_dirs, &args.defines)
	{
	Err(e) => {
		panic!("Error parsing file: {:?}", e);
		},
	Ok(_) => {}
	}

	// TODO: Type check/annotate?
	// TODO: Convert to cranelift?

	let stdout = ::std::io::stdout();
	::ast::pretty_print::write(stdout.lock(), &program);
}

// vim: ft=rust
