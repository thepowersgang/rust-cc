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

extern crate cranelift_codegen;
extern crate cranelift_frontend;

mod preproc;
mod parse;
mod types;
mod ast;

mod codegen;

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
	// - Need to annotate types and implicit conversion positions
	//   > All node types
	//   > Add promotions/demotions/conversions
	//   > Annotate ternary with if it's in LValue position
	//   > Collate variables and determine if they're addressed

	// Convert to cranelift?
	if false
	{
		let mut c = codegen::Context::new();
		for (name,ty,code) in program.iter_functions()
		{
			match ty.basetype
			{
			crate::types::BaseType::Function(ref fcn) => c.lower_function(name, fcn, code),
			_ => {},
			}
		}
	}

	if true
	{
		let stdout = ::std::io::stdout();
		::ast::pretty_print::write(stdout.lock(), &program);
	}
}

// vim: ft=rust
