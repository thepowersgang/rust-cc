//! C Pre-processor handling
use std::collections::HashMap;
use std::default::Default;

pub use self::token::Token;
pub mod token;
mod lex;

#[derive(Debug)]
pub enum Error
{
	/// any EOF (may not be an error)
	EOF,
	/// Any form of IO error
	IoError(::std::io::Error),
	/// An unexpected character was encountered in the input stream
	BadCharacter(char),
	/// A literal (integer, float, string) wasn't able to be lexed correctly
	MalformedLiteral(&'static str),
	/// An unexpected EOF
	UnexpectedEof,
}

pub type Result<T> = ::std::result::Result<T,Error>;


trait ReadExt: ::std::io::Read {
	fn chars(self) -> ::utf8reader::UTF8Reader<Self> where Self: Sized;
}
impl<T: ::std::io::Read> ReadExt for T {
	fn chars(self) -> ::utf8reader::UTF8Reader<Self> {
		::utf8reader::UTF8Reader::new(self)
	}
}

pub struct Preproc
{
	/// Stack of active lexers
	lexers: TokenSourceStack,
	/// Marker used to know if `#foo` should be parsed (i.e. we're at the start of a line)
	start_of_line: bool,
	/// Saved token for `put_back`
	saved_tok: Option<Token>,
	/// Parsed macros
	macros: HashMap<String,MacroDefinition>,
	/// Stack of active `#if`/`#else` statements
	if_stack: Vec<Conditional>,

	/// A hashset used to implement `#pragma once`
	pragma_once_set: ::std::collections::HashSet<::std::path::PathBuf>,

	/// User-provided pre-processor options
	options: Options,
}

pub struct Options
{
	/// How `#include`s should be handled
	pub include_handling: Handling,
	/// How `#define`s should be handled
	pub define_handling: Handling,
	/// Return define expansions wrapped in Token::MacroExpansion
	pub wrap_define_expansion: bool,	// Only matters if `define_handling` is not PropagateOnly
	/// Return (most) comments in the tokens steam. This still strips comments that would impede preprocessing
	pub return_most_comments: bool,

	pub include_paths: Vec<::std::path::PathBuf>,
}
impl ::std::default::Default for Options {
	fn default() -> Self {
		Options {
			include_handling: Handling::InternalOnly,
			define_handling: Handling::InternalOnly,
			wrap_define_expansion: false,
			return_most_comments: false,

			include_paths: Vec::new(),
			}
	}
}
#[allow(dead_code)]
#[derive(PartialEq)]
pub enum Handling
{
	/// Internally handle the item (don't yield the magic token)
	InternalOnly,
	/// Handle internally after yeilding the magic marker token
	InternalAndPropagate,
	/// Just yield magic marker token (no internal handling)
	PropagateOnly,
}

struct MacroDefinition
{
	arg_names: Option<MacroArgs>,
	expansion: Vec<Token>,
}
struct MacroArgs
{
	names: Vec<String>,
	va_args_name: Option<String>,
}
#[derive(Default,Debug)]
struct Conditional
{
	is_else: bool,
	has_run: bool,	// no else/elif should run
	is_active: bool,	// If the contents of this block should be emitted.
}
impl Conditional {
	fn new(is_active: bool) -> Conditional {
		Conditional {
			is_active,
			.. Default::default()
			}
	}
}

struct TokenSourceStack
{
	lexers: Vec<InnerLexer>,
	include_inner_eof: bool,
}
enum InnerLexer
{
	File(LexHandle),
	MacroExpansion(MacroExpansion),
}
struct LexHandle
{
	lexer: lex::Lexer<'static>,
	filename: Option<::std::rc::Rc<::std::path::PathBuf>>,
	line: usize,
}
struct MacroExpansion
{
	name: String,
	idx: usize,
	tokens: ::std::vec::IntoIter<Token>,	// TODO: Instead store Rc<Vec<Token>> to MacroDefinition.expansion and HashMap<String,Vec<Tokens>>
}

/*
pub struct ProtoSpan
{
	include_path: Vec<(::std::path::PathBuf,usize)>,
	root_file: ::std::path::PathBuf,
	root_line: usize,
	root_ofs: usize,
}
*/

macro_rules! syntax_assert{ ($tok:expr, $pat:pat => $val:expr) => ({ let v = $tok?; match v {
	$pat => $val,
	_ => panic!("TODO: Syntax errors, assert {}, got {:?}", stringify!($pat), v)
	}})}

impl Preproc
{
	pub fn new(filename: Option<&::std::path::Path>, options: Options) -> Result<Preproc>
	{
		let lexer = if let Some(filename) = filename
			{
				lex::Lexer::new(Box::new(match ::std::fs::File::open(filename)
					{
					Ok(f) => ::std::io::BufReader::new(f).chars(),
					Err(e) => return Err(Error::IoError(e)),
					}))
			}
			else
			{
				lex::Lexer::new(Box::new( ::std::io::stdin().chars() ))
			};
		Ok(Preproc {
			lexers: TokenSourceStack::new( lexer, filename.map(|x| x.to_owned()), options.include_handling != Handling::InternalOnly ),
			start_of_line: true,
			saved_tok: None,
			macros: Default::default(),
			if_stack: Default::default(),
			pragma_once_set: Default::default(),
			options: options,
			})
	}

	/*
	pub fn start_span(&self) -> ProtoSpan {
		ProtoSpan {
			root_file: self.lexers.last().fmt_lineno(f)
		}
	}
	pub fn end_span(&self) -> crate::ast::Span {
		crate::ast::Span
	}
	*/
	pub fn point_span(&self) -> crate::ast::Span {
		crate::ast::Span {
			layers: ::std::rc::Rc::new(self.lexers.lexers.iter().map(|v| {
					v.span().to_string()
				}).collect()),
		}
	}

	pub fn parse_define_str(&mut self, s: &str) -> Result<()>
	{
		let mut lex = lex::Lexer::new(Box::new(s.chars().map(Ok)));

		let ident = syntax_assert!(lex.get_token(), Token::Ident(n) => n);
		match lex.get_token()?
		{
		Token::EOF => {
			self.macros.insert(ident, MacroDefinition { arg_names: None, expansion: Vec::new() });
			Ok( () )
			},
		Token::Assign => {
			panic!("TODO: Define to a value from string");
			},
		Token::ParenOpen => {
			panic!("TODO: Define function-like from a string");
			},
		t => panic!("TODO: error for bad token {:?} in define string", t),
		}
	}

	pub fn put_back(&mut self, tok: Token)
	{
		assert!( self.saved_tok.is_none() );
		self.saved_tok = Some( tok );
	}

	fn eat_comments(&mut self) -> Result<Token>
	{
		self.lexers.get_token_nospace()
	}

	fn is_conditional_active(&self) -> bool
	{
		self.if_stack.iter()
			.all(|v| v.is_active)
	}

	pub fn get_token(&mut self) -> Result<Token>
	{
		if self.saved_tok.is_some()
		{
			let tok = self.saved_tok.take().unwrap();
			trace!("get_token = {:?} (saved)", tok);
			Ok( tok )
		}
		else
		{
			let tok = lex::map_keywords( self.get_token_int()? );
			trace!("get_token = {:?} (new)", tok);
			Ok(tok)
		}
	}
	pub fn get_token_int(&mut self) -> Result<Token>
	{
		'outer: loop
		{
			// ---
			// Handle #if-ed out blocks (only used when internal handling is enabled)
			// TODO: May want to propagate the ignored tokens if Internal+Propagate is enabled?
			// ---
			if ! self.is_conditional_active() {
				match self.lexers.get_token()?
				{
				Token::EOF => {
					panic!("Unexpected EOF - {:?}", self.if_stack);
					//return Ok(Token::EOF);
					},
				Token::Whitespace => {},
				Token::EscapedNewline => {},
				Token::Newline => {
					self.start_of_line = true;
					},
				Token::LineComment(_) | Token::BlockComment(_) => {
					// No need to handle comment propagation when handling disabled #if blocks
					},
				Token::Hash if self.start_of_line =>
					match self.eat_comments()?
					{
					Token::Ident(ref name) if name == "if" => {
						while self.eat_comments()? != Token::Newline {
						}
						self.if_stack.push(Conditional::default());
						},
					Token::Ident(ref name) if name == "elif" => {
						let mut tokens = Vec::new();
						loop
						{
							match self.eat_comments()?
							{
							Token::Newline => break,
							t => tokens.push(t),
							}
						}
						let is_true = self.parse_if_expr(tokens)?;

						match self.if_stack.last_mut()
						{
						None => panic!("{}TODO: Error for unmatched #elif", self),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #elif"),
						Some(v) => {
							if v.is_active {
								v.is_active = false;
								v.has_run = true;
							}
							else if !v.has_run {
								v.is_active = is_true;
							}
							else {
							}
							},
						}
						},
					Token::Ident(ref name) if name == "ifdef" || name == "ifndef" => {
						let _ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						self.if_stack.push(Conditional::default());
						},
					Token::Ident(ref name) if name == "else" => {
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #else"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #else"),
						Some(v) => {
							v.is_else = true;
							if !v.has_run {
								v.is_active = true;
							}
							},
						}
						},
					Token::Ident(ref name) if name == "endif" => {
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						match self.if_stack.pop()
						{
						None => panic!("TODO: Error for unmatched #endif"),
						_ => {},
						}
						},
					_ => {},
					},
				_ => {},
				}
				continue ;
			}

			match self.lexers.get_token()?
			{
			Token::Whitespace => {},
			Token::EscapedNewline => {},
			Token::Newline => {
				self.start_of_line = true;
				},
			t @ Token::LineComment(_) | t @ Token::BlockComment(_) => {
				// Optionally propagate comments to caller
				if self.options.return_most_comments {
					return Ok(t);
				}
				},
			Token::Hash if self.start_of_line => {
				match self.eat_comments()?
				{
				// #include
				Token::Ident(ref name) if name == "include" => {
					let (was_angle, path) = if let Some(s) = self.lexers.get_includestr()?
						{
							// `#include <foo>`
							(true, s)
						}
						else
						{
							// String literals (maybe with pre-processor expansions?)
							match self.lexers.get_token()?
							{
							Token::String(s) => { (false, s) },
							tok @ _ => panic!("TODO: Syntax error, unexpected {:?}", tok),
							}
						};
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					if self.options.include_handling != Handling::PropagateOnly {
						let file_path = if was_angle {
								// Search the include directories for the first entry that contains the specified file
								self.options.include_paths.iter()
									.flat_map(|include_path| {
										let mut p = include_path.to_owned();
										p.push(&path);
										if p.is_file() {
											Some(p)
										}
										else {
											None
										}
										})
									.next()
									.unwrap_or_else(|| panic!("{}: TODO: Proper error when `#include '<' {:?} '>' fails", self, path))
							}
							else {
								let mut p = self.lexers.cur_path().parent().unwrap_or(::std::path::Path::new(".")).to_owned();
								p.push(&path);
								p
							};
						self.lexers.push_file(file_path)?;
					}
					if self.options.include_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::Include { angle_brackets: was_angle, path: path }.into());
					}
					// Continue loop
					},

				// ---
				// Conditionals
				// ---
				// #if[n]def
				Token::Ident(ref name) if name == "ifdef" || name == "ifndef" => {
					let cnd = name == "ifdef";
					let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					// Push to #if stack, only pass tokens if entire #if stack is true
					// - Requires handling to be active 
					if self.options.define_handling != Handling::PropagateOnly {
						self.if_stack.push(Conditional::new(self.macros.contains_key(&ident) == cnd));
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::IfDef { is_not_defined: !cnd, ident: ident }.into());
					}
					},
				// #if
				Token::Ident(ref name) if name == "if" => {
					let mut tokens = Vec::new();
					loop
					{
						match self.eat_comments()?
						{
						Token::Newline => break,
						t => tokens.push(t),
						}
					}
					if self.options.define_handling != Handling::PropagateOnly {
						// Parse and evaluate the expression
						let is_true = self.parse_if_expr(tokens.clone())?;
						self.if_stack.push(Conditional::new(is_true));
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::If { tokens: tokens }.into());
					}
					},
				// #elif
				Token::Ident(ref name) if name == "elif" => {
					let mut tokens = Vec::new();
					loop
					{
						match self.eat_comments()?
						{
						Token::Newline => break,
						t => tokens.push(t),
						}
					}
					// Parse and evaluate the expression
					if self.options.define_handling != Handling::PropagateOnly {
						// NOTE: In this branch, the outer #if (or previous #elif) is active, so this expression doesn't matter.
						//let is_true = self.parse_if_expr(&tokens)?;

						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #elif"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #elif"),
						Some(v) => { v.has_run = true; v.is_active = false; },
						}
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::ElseIf { tokens: tokens }.into());
					}
					},
				// #else
				Token::Ident(ref name) if name == "else" => {
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					if self.options.define_handling != Handling::PropagateOnly {
						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #else"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #else"),
						Some(v) => { v.is_else = true; v.has_run = true; v.is_active = !v.is_active; },
						}
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::Else.into());
					}
					},
				// #endif
				Token::Ident(ref name) if name == "endif" => {
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					match self.if_stack.pop()
					{
					None => panic!("TODO: Error for unmatched #endif"),
					_ => {},
					}
					},

				// ---
				// Macro definition
				// ---
				// #define
				Token::Ident(ref name) if name == "define" => {
						let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
						let mut tokens = Vec::new();
						let (cont, args, variable) =
							match self.lexers.get_token()?
							{
							Token::Whitespace => (true, None, None),
							Token::Newline => (false, None, None),
							Token::ParenOpen => {
								let mut args = Vec::new();
								let mut variable = None;
								loop
								{
									match self.eat_comments()?
									{
									Token::ParenClose if args.len() == 0 => break,
									Token::Ident(s) => args.push(s),
									Token::Vargs => {
										variable = Some("__VA_ARGS__".to_owned());
										syntax_assert!(self.eat_comments(), Token::ParenClose => ());
										break
										},
									tok @ _ => panic!("{}: TODO: Error, unexpected {:?} in macro argument list", self, tok),
									}
									match self.eat_comments()?
									{
									Token::Vargs => {
										syntax_assert!(self.eat_comments(), Token::ParenClose => ());
										variable = args.pop();
										break
										},
									Token::ParenClose => break,
									Token::Comma => {},
									tok @ _ => panic!("{}: TODO: Error, unexpected {:?} in macro argument list", self, tok),
									}
								}
								(true, Some(args), variable)
								},
							tok @ _ => panic!("TODO: Unexpected {:?} after #define name", tok),
							};
						if cont
						{
							loop
							{
								// TODO: Should whitespace be included in the expansion?
								match self.lexers.get_token()?
								{
								Token::Whitespace => {},
								Token::LineComment(_) => {},
								Token::BlockComment(_) => {},
								Token::Newline => break,
								tok => tokens.push(tok),
								}
							}
						}

						match self.options.define_handling
						{
						Handling::InternalOnly => {
							info!("Define {} = {:?} {:?}", ident, args, tokens);
							self.macros.insert(ident, MacroDefinition {
								arg_names: args.map(|v| MacroArgs { names: v, va_args_name: variable }),
								expansion: tokens,
								});
							// Continue loop
							},
						Handling::InternalAndPropagate => {
							// - Clone into the local map
							self.macros.insert(ident.clone(), MacroDefinition {
								arg_names: args.clone().map(|v| MacroArgs { names: v, va_args_name: variable }),
								expansion: tokens.clone(),
								});
							return Ok(token::Preprocessor::MacroDefine {
								name: ident,
								arg_names: args,
								expansion: tokens,
								}.into());
							},
						Handling::PropagateOnly => {
							return Ok(token::Preprocessor::MacroDefine {
								name: ident,
								arg_names: args,
								expansion: tokens,
								}.into());
							}
						}
					},
				Token::Ident(ref name) if name == "undef" => {
					let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
					// TODO: Do function-like macros need the parens when being un-defined?
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					if self.options.define_handling != Handling::PropagateOnly {
						self.macros.remove(&ident);
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::MacroUndefine { name: ident }.into());
					}
					},
				// #pragma
				Token::Ident(ref name) if name == "pragma" => {
					let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
					match &ident[..]
					{
					"once" => {
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						if !self.pragma_once_set.insert(self.lexers.cur_path().to_owned()) {
							self.lexers.lexers.pop();
							continue 'outer;
						}
						// TODO: Ensure single-inclusion of this file
						// TODO: If enabled, propagate a token::Preprocessor::PragmaOnce
						},
					n => panic!("{}: TODO: Unknown pragma `{}`", self, n),
					}
					},
				// Unknown identifier
				Token::Ident(name) => panic!("{}TODO: Preprocessor '{}'", self, name),
				

				//Token::Integer(line, _,_) => {
				//	let file = syntax_assert!(self.lexers.get_token(), Token::String(s) => s);
				//	if let InnerLexer::File(lexer_h) = self.lexers.last_mut()
				//	{
				//		lexer_h.filename = file;
				//		lexer_h.line = line as usize;
				//		debug!("Set locaion to \"{}\":{}", lexer_h.filename, line);
				//	}
				//	while self.lexers.get_token()? != Token::Newline
				//	{
				//	}
				//	},
				tok @ _ => {
					panic!("TODO: Unexpected token after # - {:?}", tok);
					},
				}
				},
			Token::Ident(v) => {
				self.start_of_line = false;

				match &v[..]
				{
				"__FILE__" => return Ok(Token::String(format!("{}", self.lexers.cur_path().display()))),
				"__LINE__" => {
					let line = 0;
					return Ok(Token::Integer(line, ::types::IntClass::Int(::types::Signedness::Signed), format!("{}", line)))
					},
				_ => {},
				}

				match self.macros.get(&v) {
				Some(macro_def) => {
					let arg_mapping: HashMap<&str,Vec<Token>> = if let Some(ref args) = macro_def.arg_names {
							match self.lexers.get_token_nospace()?
							{
							Token::ParenOpen => {},
							tok => {
								assert!( self.saved_tok.is_none() );
								self.saved_tok = Some( lex::map_keywords(tok) );
								return Ok(Token::Ident(v));
								}
							}
							debug!("Macro {} with args", v);
							let l = &mut self.lexers;
							Self::parse_macro_args(args, &mut || l.get_token())?
						}
						else {
							HashMap::new()
						};

					debug!("Macro expansion {} args={:?}", v, arg_mapping);
					let output_tokens = self.do_macro_expansion(macro_def, &arg_mapping);
					debug!("=> output_tokens={:?}", output_tokens);

					if self.options.wrap_define_expansion {
						return Ok(token::Preprocessor::MacroInvocaton {
							// - Re-create (a variant) the input tokens
							input: {
								let mut arg_mapping = arg_mapping;	// re-map as mutable, so we can remove stuff.
								let mut input = Vec::new();
								input.push(Token::Ident(v));
								if let Some(ref args) = macro_def.arg_names
								{
									input.push(Token::ParenOpen);
									let mut first = true;
									for a in &args.names
									{
										if !first {
											input.push(Token::Comma);
										}
										input.extend( arg_mapping.remove(&a[..]).unwrap().into_iter() );
										first = false;
									}
									input.push(Token::ParenClose);
								}
								input
								},
							// - Include the previously-calculated output tokens
							output: output_tokens,
							}.into());
					}
					else if macro_def.expansion.len() > 0 {
						// Push this macro as a new underlying lexer.
						self.lexers.push_macro(v, output_tokens);
						// Keep looping (next iteration will use the macro as a token source)
						continue
					}
					else {
						// Empty macro, and wrapping disabled, keep looping
						continue
					}
					},
				_ => {
					let ret = Token::Ident(v);
					trace!("get_token = {:?}", ret);
					return Ok( ret );
					}
				}
				},
			tok @ _ => {
				self.start_of_line = false;
				trace!("get_token = {:?}", tok);
				return Ok(tok)
				}
			}
		}
	}

	fn parse_macro_args<'a>(mac_args: &'a MacroArgs, get_token: &mut dyn FnMut()->Result<Token>) -> Result<HashMap<&'a str,Vec<Token>>>
	{
		// Read tokens, handling nested parens
		let mut args: HashMap<&str,_> = HashMap::new();
		let mut cur_arg_idx: usize = 0;
		let mut cur_arg_toks = Vec::new();
		let mut paren_level: usize = 0;

		Ok(loop
		{
			match get_token()?
			{
			Token::ParenClose if paren_level == 0 => {
				if cur_arg_idx < mac_args.names.len() {
					args.insert( &mac_args.names[cur_arg_idx], cur_arg_toks );
					if let Some(v) = mac_args.va_args_name.as_ref() {
						args.insert(v, vec![]);
					}
				}
				else if cur_arg_idx == 0 && cur_arg_toks.is_empty() && mac_args.va_args_name.is_none() {
				}
				else {
					if let Some(v) = mac_args.va_args_name.as_ref() {
						args.insert( v, cur_arg_toks );
					}
					else {
						panic!("Bugcheck: va_args_name is None, but out of arguments - args={:?}", args);
					}
				}
				break args;
				},
			// TODO: Handle gcc-style named variadics?
			Token::Comma if paren_level == 0 && cur_arg_idx < mac_args.names.len() => {
				args.insert( &mac_args.names[cur_arg_idx], cur_arg_toks );
				cur_arg_toks = Vec::new();
				cur_arg_idx += 1;
				if cur_arg_idx == mac_args.names.len() && mac_args.va_args_name.is_none() {
					panic!("TODO: Error message when too many arguments are passed");
				}
				},
			t @ Token::ParenOpen => {
				paren_level += 1;
				cur_arg_toks.push(t);
				},
			t @ Token::ParenClose => {
				paren_level -= 1;
				cur_arg_toks.push(t);
				},
			Token::Whitespace | Token::EscapedNewline | Token::Newline if cur_arg_toks.len() == 0 => {},
			t => cur_arg_toks.push(t),
			}
		})
	}

	fn do_macro_expansion(&self, macro_def: &MacroDefinition, arg_mapping: &HashMap<&str,Vec<Token>>) -> Vec<Token>
	{
		let mut output_tokens = Vec::new();
		enum Mode {
			Normal, Stringify, Concat,
		}
		let mut mode = Mode::Normal;
		for tok in &macro_def.expansion
		{
			match tok
			{
			Token::Ident(i) =>
				match ::std::mem::replace(&mut mode, Mode::Normal)
				{
				Mode::Normal =>
					// No concat/stringify - expand tokens directly
					match arg_mapping.get(&i[..])
					{
					Some(v) => output_tokens.extend( v.iter().cloned() ),
					None => output_tokens.push( Token::Ident(i.clone()) ),
					},
				Mode::Stringify =>
					match arg_mapping.get(&i[..])
					{
					Some(v) => {
						let mut s = String::new();
						for t in v.iter()
						{
							s.push_str(match t
							{
							&Token::EOF
							| &Token::BlockComment(_)
							| &Token::LineComment(_)
								=> "",
							&Token::Whitespace
							| &Token::Newline
							| &Token::EscapedNewline
								=> " ",
							&Token::Preprocessor(_) => panic!("{:?} in macro argument", t),
							// -- Expression leaves
							&Token::Integer(_, _, ref s) => s,
							&Token::Float(_, _, ref s) => s,
							&Token::Character(ch) => panic!("TODO: Stringify char constant {}", ch),
							&Token::String(ref s) => panic!("TODO: Stringify string constant {:?}", s),
							&Token::Ident(ref n) => n,
							
							// -- Symbols
							&Token::Hash => "#",
							&Token::Tilde => "~",
							&Token::Exclamation => "!",
							&Token::Period => ".",
							&Token::DerefMember => "->",
							&Token::Comma => ",",
							&Token::Semicolon => ";",
							&Token::Star => "*",
							&Token::Slash => "/",
							&Token::Backslash => "\\",
							&Token::Vargs => "...",
							&Token::QuestionMark => "?",
							&Token::Colon => ":",

							&Token::Assign => "=",
							&Token::AssignAdd => "+=",
							&Token::AssignSub => "-=",
							&Token::AssignMul => "*=",
							&Token::AssignDiv => "/=",
							&Token::AssignMod => "%=",
							&Token::AssignLogicOr => "||=",
							&Token::AssignLogicAnd => "&&=",
							&Token::AssignBitOr => "|=",
							&Token::AssignBitAnd => "&=",
							
							&Token::ShiftRight => ">>",
							&Token::ShiftLeft => "<<",
							
							&Token::Equality => "==",
							&Token::NotEquals => "!=",
							&Token::Lt => "<",
							&Token::Gt => ">",
							&Token::LtE => "<=",
							&Token::GtE => ">=",
							
							&Token::Percent => "%",
							&Token::Plus => "+",
							&Token::Minus => "-",
							&Token::DoublePlus => "++",	// ungood (bad joke, sorry)
							&Token::DoubleMinus => "--",
							
							&Token::Ampersand => "&",
							&Token::Pipe => "|",
							&Token::Caret => "^",
							&Token::DoubleAmpersand => "&&",
							&Token::DoublePipe => "||",
							
							// -- Brackets
							&Token::BraceOpen => "{",
							&Token::BraceClose => "}",
							&Token::ParenOpen => "(",
							&Token::ParenClose => ")",
							&Token::SquareOpen => "[",
							&Token::SquareClose => "]",
							
							// -- Reserved Words
							// - Storage classes
							&Token::Rword_typedef
							| &Token::Rword_auto
							| &Token::Rword_extern
							| &Token::Rword_static
							| &Token::Rword_register
							| &Token::Rword_inline
							| &Token::Rword_const
							| &Token::Rword_volatile
							| &Token::Rword_restrict
							| &Token::Rword_void
							| &Token::Rword_Bool
							| &Token::Rword_signed
							| &Token::Rword_unsigned
							| &Token::Rword_char
							| &Token::Rword_short
							| &Token::Rword_int
							| &Token::Rword_long
							| &Token::Rword_float
							| &Token::Rword_double
							| &Token::Rword_enum
							| &Token::Rword_union
							| &Token::Rword_struct
							| &Token::Rword_if
							| &Token::Rword_else
							| &Token::Rword_while
							| &Token::Rword_do
							| &Token::Rword_for
							| &Token::Rword_switch
							| &Token::Rword_goto
							| &Token::Rword_continue
							| &Token::Rword_break
							| &Token::Rword_return
							| &Token::Rword_case
							| &Token::Rword_default
							| &Token::Rword_sizeof
								=> panic!("Reserved word mapped to token variant in preprocessor, should still be an ident"),
							});
						}
						output_tokens.push(Token::String(s))
						},
					None => output_tokens.push( Token::String(i.clone()) ),
					},
				Mode::Concat =>
					match output_tokens.last_mut()
					{
					Some(Token::Ident(prev)) =>
						match arg_mapping.get(&i[..])
						{
						Some(v) =>
							for v in v {
								match v
								{
								&Token::Whitespace => {},
								&Token::Ident(ref i) => prev.push_str(i),
								&Token::Integer(_v, _, ref s) => prev.push_str(s),
								_ => panic!("{}: TODO: Concat `{}` with expanded - {:?}", self, prev, v),
								}
							},
						None => {
							prev.push_str(i);
							},
						},
					Some(Token::Comma) =>
						match arg_mapping.get(&i[..])
						{
						Some(v) if v.len() == 0 => { output_tokens.pop(); },
						Some(v) => output_tokens.extend( v.iter().cloned() ),
						None =>
							panic!("TODO: ', ## {}' - no expansion for {}", i, i),
						}
					opt_t => {
						panic!("TODO: Concat after non-ident - {:?}", opt_t);
						},
					},
				},
			Token::Hash => match mode
				{
				Mode::Normal => { mode = Mode::Stringify; },
				Mode::Stringify => { mode = Mode::Concat; },
				Mode::Concat => { panic!("TODO: Error on `###` in stream"); }
				},
				//{
				//	// Mode change to do a concatenation
				//}
			tok => {
				match mode
				{
				Mode::Normal => { },
				Mode::Stringify => { panic!("TODO: Error for # followed by non-ident?"); },
				Mode::Concat => { panic!("TODO: Error for ## followed by non-ident?"); },
				}
				output_tokens.push(tok.clone());
				}
			}
		}
		output_tokens
	}

	fn parse_if_expr(&self, tokens: Vec<Token>) -> Result<bool>
	{
		// TODO: Pass expansion over the tokens first? (Macro expansion rules aren't trivial)
		struct Parser<'a>
		{
			self_: &'a Preproc,
			tokens: ::std::vec::IntoIter<Token>,
			macro_expansions: Vec<(usize, ::std::vec::IntoIter<Token>)>,
			
			tmp: Option<Token>,
		}
		impl<'a> Parser<'a>
		{
			fn peek_tok<'b>(&'b mut self) -> Option<&'b Token> {
				if self.tmp.is_none() {
					self.tmp = self.get_tok();
				}
				self.tmp.as_ref()
			}
			fn get_tok(&mut self) -> Option<Token> {
				let rv = if self.tmp.is_some() {
						self.tmp.take()
					}
					else {
						loop
						{
							match self.get_tok_int()
							{
							Some(Token::EscapedNewline) => {},
							v => break v,
							}
						}
					};
				//debug!("if::Parser::get_tok: {:?}", rv);
				rv
			}
			fn get_tok_int(&mut self) -> Option<Token> {
				loop
				{
					if let Some(&mut (ref mut pos, ref mut me)) = self.macro_expansions.last_mut()
					{
						*pos += 1;
						if let Some(v) = me.next() {
							return Some(v);
						}
						else if *pos == 1 {
							// HACK: Empty macros expand to `1` in #if
							return Some(Token::Integer(1, ::types::IntClass::int(), String::new()));
						}
					}

					// If there was a expansion being processed (which has to have just reached the end)
					if let Some(_) = self.macro_expansions.pop() {
						// Loop again
						continue ;
					}

					return match self.tokens.next()
						{
						None => None,
						Some(rv) => {
							if let Token::Ident(n) = rv
							{
								if let Some(md) = self.self_.macros.get(&n)
								{
									let args = if let Some(ref args) = md.arg_names {
											if self.peek_tok() != Some(&Token::ParenOpen) {
												return Some(Token::Ident(n));
											}
											match Preproc::parse_macro_args(args, &mut || self.tokens.next().ok_or(Error::UnexpectedEof))
											{
											Err(e) => panic!("TODO: Error when failing to parse macro args {:?}", e),
											Ok(v) => v,
											}
										}
										else {
											HashMap::new()
										};
									debug!("{}: Expand `{}` to {:?} with args={:?}", self.self_, n, md.expansion, args);
									let exp = self.self_.do_macro_expansion(md, &args);
									// TODO: Calculate expansion, with recursion
									self.macro_expansions.push( (0, exp.into_iter()) );
									continue ;
								}
								else
								{
									return Some(Token::Ident(n));
								}
							}
							Some(rv)
							}
						};
				}
			}

			fn get_tok_noexpand(&mut self) -> Option<Token>
			{
				if self.tmp.is_some() {
					self.tmp.take()
				}
				else if self.macro_expansions.len() > 0 {
					panic!("{}TODO: `defined` in macro exapnsion, valid?", self.self_);
				}
				else if let Some(t) = self.tokens.next() {
					Some(t)
				}
				else {
					None
				}
			}
		}

		// TODO: use a value enum to handle quirks?
		//enum Value {
		//	Int(i64),
		//	Name(String),
		//	EmptyExpansion,
		//}
		type Value = i64;
		type Result = crate::preproc::Result<Value>;
		impl<'a> Parser<'a>
		{
			fn evaluate_expr_value(&mut self) -> Result {
				Ok(match self.get_tok()
				{
				Some(Token::Integer(v,_,_)) => v as Value,
				Some(Token::Ident(ref n)) if n == "defined" => {
					let i = match self.get_tok_noexpand()
						{
						Some(Token::Ident(i)) => i,
						Some(Token::ParenOpen) =>
							match self.get_tok_noexpand()
							{
							Some(Token::Ident(i)) =>
								match self.get_tok_noexpand()
								{
								Some(Token::ParenClose) => i,
								t => panic!("{}TODO: Error in #if parsing - `defined(NAME` followed by invalid token - {:?}", self.self_, t), 
								},
							t => panic!("{}TODO: Error in #if parsing - `defined(` followed by invalid token - {:?}", self.self_, t), 
							},
						t => panic!("{}TODO: Error in #if parsing - defined followed by invalid token - {:?}", self.self_, t), 
						};
					debug!("> defined {:?}", i);
					if self.self_.macros.contains_key(&i) {
						1
					}
					else {
						0
					}
					},
				Some(Token::Ident(n)) => {
					// TODO: `#if foo == foo` should be true, but `#if foo == bar` should be false?
					warn!("{}: Undefined identifier {}, evaluating to 0", self.self_, n);
					if self.peek_tok() == Some(&Token::ParenOpen) {
						error!("{}Was {} meant to be a function-like macro?", self.self_, n);
					}
					0
					},
				Some(Token::ParenOpen) => {
					let rv = self.evaluate_expr()?;
					match self.get_tok().ok_or(Error::UnexpectedEof)?
					{
					Token::ParenClose => {},
					t => panic!("{}TODO: Error in #if parsing - Expected ')' got '{:?}' - {:?}", self.self_, t, self.tokens),
					}
					rv
					},
				t @ _ => panic!("{}TODO: Error in #if parsing - unexpected token {:?}", self.self_, t),
				})
			}
			fn evaluate_expr_unary(&mut self) -> Result {
				let cur = Self::evaluate_expr_unary;
				let next = Self::evaluate_expr_value;
				match self.peek_tok()
				{
				Some(Token::Exclamation) => { self.get_tok(); Ok( (cur(self)? == 0) as _ ) },
				Some(Token::Minus) => { self.get_tok(); Ok( -cur(self)? ) },
				Some(Token::Tilde) => { self.get_tok(); Ok( !cur(self)? ) },
				_ => next(self),
				}
			}
			fn evaluate_expr_muldiv(&mut self) -> Result {
				let next = Self::evaluate_expr_unary;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::Star ) => { self.get_tok(); v * next(self)? },
						Some(Token::Slash) => { self.get_tok(); v / next(self)? },
						_ => return Ok(v),
						};
				}
			}
			fn evaluate_expr_shift(&mut self) -> Result {
				let next = Self::evaluate_expr_muldiv;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::ShiftLeft) => { self.get_tok(); v << next(self)? },
						Some(Token::ShiftRight) => { self.get_tok(); v >> next(self)? },
						_ => return Ok(v),
						};
				}
			}
			fn evaluate_expr_addsub(&mut self) -> Result {
				let next = Self::evaluate_expr_shift;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::Plus ) => { self.get_tok(); v + next(self)? },
						Some(Token::Minus) => { self.get_tok(); v - next(self)? },
						_ => return Ok(v),
						//t @ _ => { debug!("evaluate_expr_addsub: t={:?} v={}", t, v); return Ok(v) },
						};
				}
			}
			fn evaluate_expr_cmp(&mut self) -> Result {
				let next = Self::evaluate_expr_addsub;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::Lt ) => { self.get_tok(); (v <  next(self)?) as _ },
						Some(Token::LtE) => { self.get_tok(); (v <= next(self)?) as _ },
						Some(Token::Gt ) => { self.get_tok(); (v >  next(self)?) as _ },
						Some(Token::GtE) => { self.get_tok(); (v >= next(self)?) as _ },
						Some(Token::Equality) => { self.get_tok(); (v == next(self)?) as _ },
						Some(Token::NotEquals) => { self.get_tok(); (v != next(self)?) as _ },
						_ => return Ok(v),
						//t @ _ => { debug!("evaluate_expr_cmp: t={:?} v={}", t, v); return Ok(v) },
						};
				}
			}
			fn evaluate_expr_bool(&mut self) -> Result {
				debug!("evaluate_expr_bool");
				let next = Self::evaluate_expr_cmp;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::DoubleAmpersand) => { self.get_tok(); let oth = next(self)?; (oth != 0 && v != 0) as _ },
						Some(Token::DoublePipe) => { self.get_tok(); let oth = next(self)?; (oth != 0 || v != 0) as _ },
						//t @ _ => { debug!("evaluate_expr_bool: t={:?} v={}", t, v); return Ok(v) },
						_ => return Ok(v),
						};
				}
			}
			fn evaluate_expr_ternary(&mut self) -> Result {
				let next = Self::evaluate_expr_bool;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::QuestionMark) => {
							self.get_tok();
							let a = next(self)?;
							match self.get_tok()
							{
							Some(Token::Colon) => {},
							t => panic!("{:?}", t),
							}
							let b = next(self)?;
							if v != 0 { a } else { b }
							},
						_ => return Ok(v),
						}
				}
			}
			fn evaluate_expr(&mut self) -> Result {
				self.evaluate_expr_ternary()
			}
		}

		let mut p = Parser { self_: self, tokens: tokens.clone().into_iter(), tmp: None, macro_expansions: Vec::new() };
		let rv = p.evaluate_expr()?;
		match p.get_tok()
		{
		Some(t) => panic!("{}: TODO: Error in PP if/elif - Unhandled token {:?}", self, t),
		None => {},
		}
		debug!("{}: Parsed and evaluated #if/#elif expression - {:?} = {}", self, tokens, rv);
		Ok(rv != 0)
	}
}

impl ::std::fmt::Display for Preproc
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		for l in &self.lexers.lexers[.. self.lexers.lexers.len() - 1]
		{
			l.fmt_lineno(f)?;
			write!(f, "\n")?;
		}
		self.lexers.last().fmt_lineno(f)
	}
}
#[derive(Clone)]
struct SpanPoint {
	filename: Option<::std::rc::Rc<::std::path::PathBuf>>,
	line: usize,
	//ofs: usize,
}
impl ::std::fmt::Display for SpanPoint {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match &self.filename {
		Some(p) => write!(f, "{}:{}", p.display(), self.line),
		None => write!(f, "<stdin>:{}", self.line),
		}
	}
}
impl InnerLexer
{
	fn fmt_lineno(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		::std::fmt::Display::fmt(&self.span(), f)
	}
	fn span(&self) -> SpanPoint {
		match self
		{
		InnerLexer::File(h) => {
			SpanPoint { filename: h.filename.clone(), line: h.line }
			},
		InnerLexer::MacroExpansion(h) => {
			// Massive hack: Much better to get a span for the position in the macro expansion instead
			SpanPoint { filename: Some(::std::rc::Rc::new(::std::path::Path::new(&format!("<macro {}>", h.name)).to_owned())), line: h.idx }
			},
		}
	}
}


impl TokenSourceStack
{
	fn new(lexer: lex::Lexer<'static>, filename: Option<::std::path::PathBuf>, include_inner_eof: bool) -> TokenSourceStack
	{
		TokenSourceStack {
			lexers: vec![ InnerLexer::File(LexHandle {
				lexer: lexer,
				filename: filename.map(::std::rc::Rc::new),
				line: 1,
				}) ],
			include_inner_eof,
			}
	}

	fn push_file(&mut self, path: ::std::path::PathBuf) -> Result<()>
	{
		self.lexers.push(InnerLexer::File(LexHandle {
			lexer: lex::Lexer::new(Box::new(match ::std::fs::File::open(&path)
				{
				Ok(f) => ::std::io::BufReader::new(f).chars(),
				Err(e) => return Err(Error::IoError(e)),
				})),
			filename: Some(::std::rc::Rc::new(path)),
			line: 1,
			}));
		Ok( () )
	}
	fn push_macro(&mut self, name: String, tokens: Vec<Token>)
	{
		self.lexers.push(InnerLexer::MacroExpansion(MacroExpansion {
			name: name,
			tokens: tokens.into_iter(),
			idx: 0,
			}));
	}

	fn last(&self) -> &InnerLexer {
		assert!( self.lexers.len() >= 1 );
		self.lexers.last().unwrap()
	}
	fn last_mut(&mut self) -> &mut InnerLexer {
		assert!( self.lexers.len() >= 1 );
		self.lexers.last_mut().unwrap()
	}

	fn cur_path(&self) -> &::std::path::Path
	{
		for e in self.lexers.iter().rev()
		{
			match e
			{
			InnerLexer::File(h) => return h.filename.as_ref().expect("TODO: Error when using include in stdin"),
			InnerLexer::MacroExpansion(_) => {},
			}
		}
		panic!("");
	}

	fn get_token(&mut self) -> Result<Token>
	{
		loop
		{
			let t = match self.lexers.last_mut()
				{
				None => return Ok(Token::EOF),
				Some(InnerLexer::File(h)) => h.get_token()?,
				Some(InnerLexer::MacroExpansion(h)) => h.get_token()?,
				};
			match t
			{
			Token::EOF if self.lexers.len() > 1 => {
				// EOF on inner parse: Pop and continue
				self.lexers.pop();
				// - Optionally return a marker token that indicates the end of a file.
				if self.include_inner_eof {
					return Ok(token::Preprocessor::EndOfInclude.into());
				}
				},
			t => return Ok(t),
			}
		}
	}
	fn get_token_nospace(&mut self) -> Result<Token>
	{
		loop
		{
			match self.get_token()?
			{
			Token::Whitespace => {},
			Token::EscapedNewline => {},
			Token::LineComment(_) => {},
			Token::BlockComment(_) => {},
			tok => return Ok(tok),
			}
		}
	}

	fn get_includestr(&mut self) -> Result<Option<String>>
	{
		match self.last_mut()
		{
		InnerLexer::File(h) => h.lexer.get_token_includestr(),
		InnerLexer::MacroExpansion(_) => Ok(None),
		}
	}
}
impl LexHandle
{
	fn get_token(&mut self) -> Result<Token>
	{
		match self.lexer.get_token()?
		{
		t @ Token::Newline | t @ Token::EscapedNewline => {
			self.line += 1;
			Ok(t)
			},
		// - Line comments still return the trailing newline
		// - Block comments can have newlines.
		Token::BlockComment(bc) => {
			self.line += bc.bytes().filter(|x| *x == b'\n').count();
			Ok( Token::BlockComment(bc) )
			},
		t => Ok(t),
		}
	}
}
impl MacroExpansion
{
	fn get_token(&mut self) -> Result<Token>
	{
		Ok(match self.tokens.next()
			{
			None => Token::EOF,
			Some(Token::EOF) => panic!("How did an EOF end up in a macro expansion?"),
			Some(t) => { self.idx += 1; t }
			})
	}
}

// vim: ft=rust

