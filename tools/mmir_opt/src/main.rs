
#[macro_use]
mod logger;

mod helper_types;

mod lexer;
mod parser;
mod mir;
mod modtree;
mod types;
mod validate;
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

	#[structopt(long="--pre-validate")]
	pre_validate: bool,
	#[structopt(long="--no-optimise")]
	no_optimise: bool,
}

fn main()
{
	let opts: Options = ::structopt::StructOpt::from_args();
	let mut tree = crate::modtree::Root::new();

	for path in &opts.input {
		parser::parse_file(&mut tree, path);
	}

	if opts.pre_validate
	{
		let mut logbuf = String::new();
		for (name, fcn) in tree.functions.iter_mut() {
			if ! opts.only.is_empty() {
				if !opts.only.iter().any(|v| v == name) {
					continue ;
				}
			}
			if let Some(ref mut b) = fcn.body {
				let mut logger = crate::logger::Logger::new(&mut logbuf, name, ! opts.only.is_empty());
				println!("--- PV: {}", name);
				validate::validate_function(&mut logger, b, &fcn.sig);
			}
		}
	}
	
	if !opts.no_optimise
	{
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

				validate::validate_function(&mut logger, b, &fcn.sig);
			}
		}
	}

	dump::dump_tree(&opts.output, &tree)
		.expect("IO error while writing");
}


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LineRef {
	bb_idx: usize,
	stmt_idx: StmtIdx,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum StmtIdx {
	Stmt(u16),
	Term,
}
impl ::core::fmt::Display for LineRef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "BB{}/", self.bb_idx)?;
		match self.stmt_idx {
		StmtIdx::Stmt(i) => write!(f, "{}", i),
		StmtIdx::Term => f.write_str("TERM"),
		}
	}
}
impl ::core::fmt::Debug for LineRef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		::std::fmt::Display::fmt(self, f)
	}
}