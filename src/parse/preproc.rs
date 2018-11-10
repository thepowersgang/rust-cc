//! C Pre-processor handling
use super::Token;
use super::token;
use std::collections::HashMap;
use std::default::Default;

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
	arg_names: Option<Vec<String>>,
	expansion: Vec<Token>,
}
#[derive(Default)]
struct Conditional
{
	is_else: bool,
	is_active: bool,	// If the contents of this block should be emitted.
}

struct TokenSourceStack
{
	lexers: Vec<InnerLexer>,
}
enum InnerLexer
{
	File(LexHandle),
	MacroExpansion(MacroExpansion),
}
struct LexHandle
{
	lexer: ::parse::lex::Lexer<'static>,
	filename: Option<::std::path::PathBuf>,
	line: usize,
}
struct MacroExpansion
{
	tokens: ::std::vec::IntoIter<Token>,	// TODO: Instead store Rc<Vec<Token>> to MacroDefinition.expansion and HashMap<String,Vec<Tokens>>
}

macro_rules! syntax_assert{ ($tok:expr, $pat:pat => $val:expr) => ({ let v = try!($tok); match v {
	$pat => $val,
	_ => panic!("TODO: Syntax errors, assert {}, got {:?}", stringify!($pat), v)
	}})}

impl Preproc
{
	pub fn new(filename: Option<&::std::path::Path>, options: Options) -> ::parse::ParseResult<Preproc>
	{
		let lexer = if let Some(filename) = filename
			{
				::parse::lex::Lexer::new(box match ::std::fs::File::open(filename)
					{
					Ok(f) => f.chars(),
					Err(e) => return Err(::parse::Error::IOError(e)),
					})
			}
			else
			{
				::parse::lex::Lexer::new(box ::std::io::stdin().chars())
			};
		Ok(Preproc {
			lexers: TokenSourceStack::new( lexer, filename.map(|x| x.to_owned()) ),
			start_of_line: true,
			saved_tok: None,
			macros: Default::default(),
			if_stack: Default::default(),
			options: options,
			})
	}

	pub fn parse_define_str(&mut self, s: &str) -> ::parse::ParseResult<()>
	{
		let mut lex = ::parse::lex::Lexer::new(box s.chars().map(Ok));

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

	fn eat_comments(&mut self) -> ::parse::ParseResult<Token>
	{
		loop
		{
			match self.lexers.get_token()?
			{
			Token::Whitespace => {},
			Token::LineComment(_) => {},
			Token::BlockComment(_) => {},
			tok => return Ok(tok),
			}
		}
	}

	fn is_conditional_active(&self) -> bool
	{
		self.if_stack.iter()
			.all(|v| v.is_active)
	}

	pub fn get_token(&mut self) -> ::parse::ParseResult<Token>
	{
		if self.saved_tok.is_some()
		{
			let tok = self.saved_tok.take().unwrap();
			trace!("get_token = {:?} (saved)", tok);
			return Ok( tok );
		}
		loop
		{
			// ---
			// Handle #if-ed out blocks (only used when internal handling is enabled)
			// TODO: May want to propagate the ignored tokens if Internal+Propagate is enabled?
			// ---
			if ! self.is_conditional_active() {
				match try!(self.lexers.get_token())
				{
				Token::Whitespace => {},
				Token::Newline => {
					self.start_of_line = true;
					},
				Token::LineComment(_) | Token::BlockComment(_) => {
					// No need to handle comment propagation when handling disabled #if blocks
					},
				Token::Hash if self.start_of_line =>
					match self.eat_comments()?
					{
					Token::Rword_if => {
						while self.eat_comments()? != Token::Newline {
						}
						self.if_stack.push(Conditional::default());
						},
					Token::Ident(ref name) if name == "elif" => {
						while self.eat_comments()? != Token::Newline {
						}
						self.if_stack.push(Conditional::default());
						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #elif"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #elif"),
						_ => {},
						}
						},
					Token::Ident(ref name) if name == "ifdef" || name == "ifndef" => {
						let _ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						self.if_stack.push(Conditional::default());
						},
					Token::Rword_else => {
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #else"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #else"),
						Some(v) => { v.is_else = true; },
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

			match try!(self.lexers.get_token())
			{
			Token::Whitespace => {},
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
					let cnd = (name == "ifdef");
					let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					// Push to #if stack, only pass tokens if entire #if stack is true
					// - Requires handling to be active 
					if self.options.define_handling != Handling::PropagateOnly {
						self.if_stack.push(Conditional { is_else: false, is_active: self.macros.contains_key(&ident) == cnd, });
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::IfDef { is_not_defined: !cnd, ident: ident }.into());
					}
					},
				// #if
				Token::Rword_if => {
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
						let is_true = self.parse_if_expr(&tokens)?;
						self.if_stack.push(Conditional { is_else: false, is_active: is_true, });
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
						let is_true = self.parse_if_expr(&tokens)?;

						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #elif"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #elif"),
						Some(v) => { v.is_active = is_true; },
						}
					}
					if self.options.define_handling != Handling::InternalOnly {
						return Ok(token::Preprocessor::ElseIf { tokens: tokens }.into());
					}
					},
				// #else
				Token::Rword_else => {
					syntax_assert!(self.eat_comments(), Token::Newline => ());
					if self.options.define_handling != Handling::PropagateOnly {
						match self.if_stack.last_mut()
						{
						None => panic!("TODO: Error for unmatched #else"),
						Some(ref v) if v.is_else => panic!("TODO: Error for duplicate #else"),
						Some(v) => { v.is_else = true; v.is_active = !v.is_active; },
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
						let (cont, args) =
							match self.lexers.get_token()?
							{
							Token::Whitespace => (true, None),
							Token::Newline => (false, None),
							Token::ParenOpen => {
								let mut args = Vec::new();
								loop
								{
									match self.eat_comments()?
									{
									Token::ParenClose if args.len() == 0 => break,
									Token::Ident(s) => args.push(s),
									Token::Vargs => {
										args.push("".to_owned());
										syntax_assert!(self.eat_comments(), Token::ParenClose => ());
										break
										},
									tok @ _ => panic!("{}: TODO: Error, unexpected {:?} in macro argument list", self, tok),
									}
									match self.eat_comments()?
									{
									Token::ParenClose => break,
									Token::Comma => {},
									tok @ _ => panic!("{}: TODO: Error, unexpected {:?} in macro argument list", self, tok),
									}
								}
								(true, Some(args))
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
							self.macros.insert(ident, MacroDefinition {
								arg_names: args,
								expansion: tokens,
								});
							// Continue loop
							},
						Handling::InternalAndPropagate => {
							// - Clone into the local map
							self.macros.insert(ident.clone(), MacroDefinition {
								arg_names: args.clone(),
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
				// #pragma
				Token::Ident(ref name) if name == "pragma" => {
					let ident = syntax_assert!(self.eat_comments(), Token::Ident(s) => s);
					match &ident[..]
					{
					"once" => {
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						// TODO: Ensure single-inclusion of this file
						},
					n => panic!("{}: TDOO: Unknown pragma `{}`", self, n),
					}
					},
				// Unknown identifier
				Token::Ident(name) => panic!("TODO: Preprocessor '{}'", name),
				

				//Token::Integer(line, _) => {
				//	let file = syntax_assert!(self.lexers.get_token(), Token::String(s) => s);
				//	if let InnerLexer::File(lexer_h) = self.lexers.last_mut()
				//	{
				//		lexer_h.filename = file;
				//		lexer_h.line = line as usize;
				//		debug!("Set locaion to \"{}\":{}", lexer_h.filename, line);
				//	}
				//	while try!(self.lexers.get_token()) != Token::Newline
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

				match self.macros.get(&v) {
				Some(macro_def) => {
					let arg_mapping: HashMap<&str,Vec<Token>> = if let Some(ref arg_names) = macro_def.arg_names {
							match self.lexers.get_token()?
							{
							Token::ParenOpen => {},
							tok => {
								assert!( self.saved_tok.is_none() );
								self.saved_tok = Some( tok );
								return Ok(Token::Ident(v));
								}
							}
							// Read tokens, handling nested parens
							let mut args: HashMap<&str,_> = HashMap::new();
							let mut cur_arg_idx = 0;
							let mut cur_arg_toks = Vec::new();
							let mut paren_level = 0;

							loop
							{
								match self.lexers.get_token()?
								{
								Token::ParenClose if paren_level == 0 => {
									args.insert( &arg_names[cur_arg_idx], cur_arg_toks );
									break args;
									},
								Token::Comma if paren_level == 0 => {
									args.insert( &arg_names[cur_arg_idx], cur_arg_toks );
									cur_arg_toks = Vec::new();
									cur_arg_idx += 1;
									},
								t @ Token::ParenOpen => {
									paren_level -= 1;
									cur_arg_toks.push(t);
									},
								t @ Token::ParenClose => {
									paren_level -= 1;
									cur_arg_toks.push(t);
									},
								t => cur_arg_toks.push(t),
								}
							}
						}
						else {
							HashMap::new()
						};

					let output_tokens = {
						let mut output_tokens = Vec::new();
						for tok in &macro_def.expansion
						{
							match tok
							{
							Token::Ident(i) => match arg_mapping.get(&i[..])
								{
								Some(v) => output_tokens.extend( v.iter().cloned() ),
								None => output_tokens.push( Token::Ident(i.clone()) ),
								},
							tok => output_tokens.push(tok.clone()),
							}
						}
						output_tokens
						};

					if self.options.wrap_define_expansion {
						return Ok(token::Preprocessor::MacroInvocaton {
							// - Re-create (a variant) the input tokens
							input: {
								let mut arg_mapping = arg_mapping;	// re-map as mutable, so we can remove stuff.
								let mut input = Vec::new();
								input.push(Token::Ident(v));
								if let Some(ref arg_names) = macro_def.arg_names
								{
									input.push(Token::ParenOpen);
									let mut first = true;
									for a in arg_names
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
						self.lexers.push_macro(output_tokens);
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


	fn parse_if_expr(&self, tokens: &[Token]) -> super::ParseResult<bool>
	{
		// TODO: Pass expansion over the tokens first? (Macro expansion rules aren't trivial)
		struct Parser<'a>
		{
			self_: &'a Preproc,
			tokens: &'a [Token],
			macro_expansions: Vec<(usize, &'a MacroDefinition)>,	// TODO: Arguments?
			
			tmp: Option<&'a Token>,
		}
		impl<'a> Parser<'a>
		{
			fn peek_tok(&mut self) -> Option<&'a Token> {
				if self.tmp.is_none() {
					self.tmp = self.get_tok();
				}
				self.tmp
			}
			fn get_tok(&mut self) -> Option<&'a Token> {
				if self.tmp.is_some() {
					return self.tmp.take();
				}
				loop
				{
					if let Some(&mut (ref mut pos, me)) = self.macro_expansions.last_mut()
					{
						if *pos == 0 && me.expansion.len() == 0
						{
							// HACK: Empty macros expand to `1` in #if
							static TOK_ONE: Token = Token::Integer(1, ::types::IntClass::int());
							*pos += 1;
							return Some(&TOK_ONE);
						}
						if *pos < me.expansion.len()
						{
							*pos += 1;
							return Some(&me.expansion[*pos - 1]);
						}
					}

					// If there was a expansion being processed (which has to have just reached the end)
					if let Some(_) = self.macro_expansions.pop() {
						// Loop again
						continue ;
					}

					return match self.tokens.split_first()
						{
						None => None,
						Some( (rv, new) ) => {
							self.tokens = new;
							if let &Token::Ident(ref n) = rv
							{
								if let Some(md) = self.self_.macros.get(n)
								{
									debug!("{}: Expand `{}` to {:?}", self.self_, n, md.expansion);
									self.macro_expansions.push( (0, md) );
									continue ;
								}
							}
							Some(rv)
							}
						};
				}
			}

			fn evaluate_expr_value(&mut self) -> super::ParseResult<i64> {
				Ok(match self.get_tok()
				{
				Some(&Token::Integer(v,_)) => v as i64,
				//Some(&Token::Ident(ref n)) => panic!("{}: TODO: Look up #define macros - {:?}", self.self_, n),
				Some(&Token::Ident(ref n)) => {
					warn!("{}: Undefined identifier {}, evaluating to 0", self.self_, n);
					0
					},
				_ => panic!("TODO: Error in #if parsing"),
				})
			}
			fn evaluate_expr_muldiv(&mut self) -> super::ParseResult<i64> {
				let next = Self::evaluate_expr_value;
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
			fn evaluate_expr_addsub(&mut self) -> super::ParseResult<i64> {
				let next = Self::evaluate_expr_muldiv;
				let mut v = next(self)?;
				loop
				{
					v = match self.peek_tok()
						{
						Some(Token::Plus ) => { self.get_tok(); v + next(self)? },
						Some(Token::Minus) => { self.get_tok(); v - next(self)? },
						_ => return Ok(v),
						//_ => { debug!("v={}", v); return Ok(v) },
						};
				}
			}
			fn evaluate_expr_cmp(&mut self) -> super::ParseResult<i64> {
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
						//t @ _ => { debug!("t={:?} v={}", t, v); return Ok(v) },
						};
				}
			}
		}

		let mut p = Parser { self_: self, tokens, tmp: None, macro_expansions: Vec::new() };
		let rv = p.evaluate_expr_cmp()?;
		match p.get_tok()
		{
		Some(t) => panic!("{}: TODO: Error in PP if/elif - Unhandled token {:?}", self, t),
		None => {},
		}
		debug!("{}: Parse+evaluate a #if/#elif expression - {:?} = {}", self, tokens, rv);
		Ok(rv != 0)
	}
}

impl ::std::fmt::Display for Preproc
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		match self.lexers.last()
		{
		InnerLexer::File(h) => {
			if let Some(ref p) = h.filename
			{
				write!(f, "{}:{}: ", p.display(), h.line)
			}
			else
			{
				write!(f, "<stdin>:{}: ", h.line)
			}
			},
		InnerLexer::MacroExpansion(_h) => {
			// TODO: Print something sane for macro expansions (macro source location?)
			write!(f, "?MACRO?: ")
			},
		}
	}
}


impl TokenSourceStack
{
	fn new(lexer: super::lex::Lexer<'static>, filename: Option<::std::path::PathBuf>) -> TokenSourceStack
	{
		TokenSourceStack {
			lexers: vec![ InnerLexer::File(LexHandle {
				lexer: lexer,
				filename: filename,
				line: 1,
				}) ]
			}
	}

	fn push_file(&mut self, path: ::std::path::PathBuf) -> super::ParseResult<()>
	{
		self.lexers.push(InnerLexer::File(LexHandle {
			lexer: super::lex::Lexer::new(box match ::std::fs::File::open(&path)
				{
				Ok(f) => f.chars(),
				Err(e) => return Err(::parse::Error::IOError(e)),
				}),
			filename: Some(path),
			line: 1,
			}));
		Ok( () )
	}
	fn push_macro(&mut self, tokens: Vec<Token>)
	{
		self.lexers.push(InnerLexer::MacroExpansion(MacroExpansion {
			tokens: tokens.into_iter(),
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
		match self.last()
		{
		InnerLexer::File(h) => h.filename.as_ref().expect("TODO: Error when using include in stdin"),
		InnerLexer::MacroExpansion(_) => panic!(""),
		}
	}

	fn get_token(&mut self) -> super::ParseResult<Token>
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
				// TODO: Return a marker token that indicates the end of a file?
				self.lexers.pop();
				},
			t => return Ok(t),
			}
		}
	}
	fn get_includestr(&mut self) -> ::parse::ParseResult<Option<String>>
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
	fn get_token(&mut self) -> super::ParseResult<Token>
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
	fn get_token(&mut self) -> super::ParseResult<Token>
	{
		Ok(match self.tokens.next()
			{
			None => Token::EOF,
			Some(Token::EOF) => panic!("How did an EOF end up in a macro expansion?"),
			Some(t) => t
			})
	}
}

// vim: ft=rust

