/*
 */
#![feature(macro_rules)]
#![feature(box_syntax)]

#[macro_use] extern crate log;
extern crate env_logger;

extern crate getopts;
extern crate collections;

mod parse;
mod types;
mod ast;

fn main()
{
	// 1. Parse command line arguments
        let opts = [
		getopts::optflag("h", "help", "Print help text"),
		];
	let args = match getopts::getopts(std::os::args().as_slice(), opts) {
		Ok(m) => m,
		Err(f) => panic!(f.to_string()),
		};
	
	let mut program = ::ast::Program::new();
	match ::parse::parse(&mut program, args.free[1].as_slice())
	{
	Err(e) => {
		panic!("Error parsing file: {}", e);
		},
	Ok(_) => {}
	}
}

// vim: ft=rust
