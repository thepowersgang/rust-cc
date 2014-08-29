/*
 */

#![feature(macro_rules)]

#![feature(phase)]
#[phase(plugin, link)] extern crate log;

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
		Err(f) => fail!(f.to_string()),
		};
	
	let mut program = ::ast::Program::new();
	match ::parse::parse(&mut program, args.free[1].as_slice())
	{
	Err(e) => {
		fail!("Error parsing file: {}", e);
		},
	Ok(_) => {}
	}
	println!("Hello, world!")
}

// vim: ft=rust
