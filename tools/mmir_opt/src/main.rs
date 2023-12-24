
#[macro_use]
mod logger;

mod lexer;
mod parser;
mod mir;
mod modtree;
mod types;
mod optimise;
mod dump;

#[derive(::structopt::StructOpt)]
struct Options
{
	#[structopt(parse(from_os_str))]
	input: Vec<::std::path::PathBuf>,

	#[structopt(short="o", parse(from_os_str))]
	output: ::std::path::PathBuf,

	#[structopt(long="--only", short="O")]
	only: Vec<String>,
}

fn main()
{
	let opts: Options = ::structopt::StructOpt::from_args();
	let mut tree = crate::modtree::Root::new();

	for path in &opts.input {
		parser::parse_file(&mut tree, path);
	}

	let mut logbuf = String::new();
	for (name, fcn) in tree.functions.iter_mut() {
		if ! opts.only.is_empty() {
			if !opts.only.iter().any(|v| v == name) {
				continue ;
			}
		}
		if let Some(ref mut b) = fcn.body {
			let mut logger = crate::logger::Logger::new(&mut logbuf, name, ! opts.only.is_empty());
			println!("--- {}", name);
			optimise::optimise_function(&mut logger, b, &fcn.sig);
		}
	}

	dump::dump_tree(&opts.output, &tree)
		.expect("IO error while writing");
}
