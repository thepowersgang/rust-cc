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
extern crate cranelift_module;
extern crate cranelift_object;

mod preproc;
mod parse;
mod types;
mod ast;

mod codegen;
mod typecheck;

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

	if false
	{
		let ofp = ::std::fs::File::create("DUMP_parsed.c").unwrap();
		::ast::pretty_print::write(ofp, &program);
	}

	// TODO: Type check/annotate?
	// - Need to annotate types and implicit conversion positions
	//   > All node types
	//   > Add promotions/demotions/conversions
	//   > Annotate ternary with if it's in LValue position
	//   > Collate variables and determine if they're addressed
	for (name,ty,sym) in program.iter_symbols()
	{
		match sym
		{
		ast::SymbolValue::Code(ref fcn) => {
			let fcn_ty = match ty.basetype
				{
				crate::types::BaseType::Function(ref fcn_ty) => fcn_ty,
				_ => panic!("ERROR: Code without a function type"),
				};
			typecheck::handle_function(&program, name, fcn_ty, &mut fcn.borrow_mut())
			},
		ast::SymbolValue::Value(ref init) => {
			typecheck::handle_global(&program, name, ty, &mut init.borrow_mut());
			},
		}
	}

	// Convert to cranelift?
	if true
	{
		let mut c = codegen::Context::new();
		for (name,ty,sym) in program.iter_symbols_with_prototypes()
		{
			match sym
			{
			None => match ty.basetype
				{
				crate::types::BaseType::Function(ref fcn_ty) => c.declare_function(name, fcn_ty),
				_ => c.declare_value(name, ty),
				},
			Some(ast::SymbolValue::Code(ref fcn)) => {
				let fcn_ty = match ty.basetype
					{
					crate::types::BaseType::Function(ref fcn_ty) => fcn_ty,
					_ => panic!("ERROR: Code without a function type"),
					};
				c.lower_function(name, fcn_ty, &fcn.borrow());
				},
			Some(ast::SymbolValue::Value(ref init)) => {
				c.lower_value(name, ty, &init.borrow());
				},
			}
		}
		let ofp = match std::fs::File::create("a.out")
			{
			Ok(v) => v,
			Err(e) => panic!("Unable to open `a.out` for writing: {}", e),
			};
		c.finish(ofp).unwrap();
	}

	if true
	{
		let stdout = ::std::io::stdout();
		::ast::pretty_print::write(stdout.lock(), &program);
	}
}

// vim: ft=rust
