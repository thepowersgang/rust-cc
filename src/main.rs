/*
 */
#![feature(box_syntax)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate utf8reader;
#[macro_use]
extern crate structopt;

mod parse;
mod types;
mod ast;


#[derive(StructOpt)]
struct Options
{
	#[structopt(parse(from_os_str))]
	input: ::std::path::PathBuf,
}

fn main()
{
	// 1. Parse command line arguments
	let args: Options = ::structopt::StructOpt::from_args();
	
	let mut program = ::ast::Program::new();
	match ::parse::parse(&mut program, &args.input)
	{
	Err(e) => {
		panic!("Error parsing file: {:?}", e);
		},
	Ok(_) => {}
	}
}

// vim: ft=rust
