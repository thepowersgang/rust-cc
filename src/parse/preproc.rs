/// C Pre-processor handling
use super::Token;
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
	///// Return (most) comments in the tokens steam. This still strips comments that would impede preprocessing
	//pub return_most_comments: bool,
}
impl ::std::default::Default for Options {
	fn default() -> Self {
		Options {
			include_handling: Handling::InternalOnly,
			define_handling: Handling::InternalOnly,
			wrap_define_expansion: false,
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
	lexer: ::parse::lex::Lexer,
	filename: String,
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
	pub fn new(filename: Option<&::std::path::Path>) -> ::parse::ParseResult<Preproc>
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
			lexers: TokenSourceStack::new( lexer, filename.map(|x| x.display().to_string()).unwrap_or_else(|| "-".to_string()) ),
			start_of_line: true,
			saved_tok: None,
			macros: Default::default(),
			options: Default::default(),
			})
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
			Token::BlockComment(_) => {},	// TODO: Line counting?
			tok => return Ok(tok),
			}
		}
	}

	fn inner_get_token_includestr(&mut self) -> ::parse::ParseResult<Option<String>>
	{
		// TODO: Move to TokenSourceStack
		match self.lexers.last_mut()
		{
		InnerLexer::File(h) => h.lexer.get_token_includestr(),
		InnerLexer::MacroExpansion(_) => Ok(None),
		}
	}

	pub fn get_token(&mut self) -> ::parse::ParseResult<Token>
	{
		if self.saved_tok.is_some()
		{
			let tok = self.saved_tok.take().unwrap();
			debug!("get_token = {:?} (saved)", tok);
			return Ok( tok );
		}
		loop
		{
			match try!(self.lexers.get_token())
			{
			Token::Whitespace => {},
			Token::Newline => {
				self.start_of_line = true;
				},
			Token::LineComment(_) => {},
			Token::BlockComment(_) => {},
			Token::Hash if self.start_of_line => {
				// TODO: only do this handling if just after a newline
				match self.eat_comments()?
				{
				Token::Ident(name) => {
					match &*name
					{
					// #include
					"include" => {
						let path = if let Some(s) = self.inner_get_token_includestr()?
							{
								// `#include <foo>`
								s
							}
							else
							{
								// String literals (maybe with pre-processor expansions?)
								match self.lexers.get_token()?
								{
								Token::String(s) => { s },
								tok @ _ => panic!("TODO: Syntax error, unexpected {:?}", tok),
								}
							};
						syntax_assert!(self.eat_comments(), Token::Newline => ());
						if self.options.include_handling != Handling::PropagateOnly {
							error!("TODO: #include {:?} - Handle", path);
						}
						if self.options.include_handling != Handling::InternalOnly {
							return Ok(Token::PreprocessorInclude(path));
						}
						// Continue loop
						},
					// #define
					"define" => {
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
									Token::Ident(s) => args.push(s),
									Token::Vargs => {
										args.push("".to_owned());
										syntax_assert!(self.eat_comments(), Token::ParenClose => ());
										break
										},
									tok @ _ => panic!("TODO: Error, unexpected {:?} in macro argument list", tok),
									}
									match self.eat_comments()?
									{
									Token::ParenClose => break,
									Token::Comma => {},
									tok @ _ => panic!("TODO: Error, unexpected {:?} in macro argument list", tok),
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
							return Ok(Token::MacroDefine {
								name: ident,
								arg_names: args,
								expansion: tokens,
								});
							},
						Handling::PropagateOnly => {
							return Ok(Token::MacroDefine {
								name: ident,
								arg_names: args,
								expansion: tokens,
								});
							}
						}
						},
					// TODO: #pragma
					_ => panic!("TODO: Preprocessor '{}'", name),
					}
					},
				Token::Integer(line, _) => {
					let file = syntax_assert!(self.lexers.get_token(), Token::String(s) => s);
					if let InnerLexer::File(lexer_h) = self.lexers.last_mut()
					{
						lexer_h.filename = file;
						lexer_h.line = line as usize;
						debug!("Set locaion to \"{}\":{}", lexer_h.filename, line);
					}
					while try!(self.lexers.get_token()) != Token::Newline
					{
					}
					},
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
						return Ok(Token::MacroInvocaton {
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
							});
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
					debug!("get_token = {:?}", ret);
					return Ok( ret );
					}
				}
				},
			tok @ _ => {
				self.start_of_line = false;
				debug!("get_token = {:?}", tok);
				return Ok(tok)
				}
			}
		}
	}
}

impl ::std::fmt::Display for Preproc
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		match self.lexers.last()
		{
		InnerLexer::File(h) => {
			write!(f, "{}:{}: ", h.filename, h.line)
			},
		InnerLexer::MacroExpansion(_h) => {
			// TODO: Print something sane for macro expansions
			write!(f, "?MACRO?: ")
			},
		}
	}
}


impl TokenSourceStack
{
	fn new(lexer: super::lex::Lexer, filename: String) -> TokenSourceStack
	{
		TokenSourceStack {
			lexers: vec![ InnerLexer::File(LexHandle {
				lexer: lexer,
				filename: filename,
				line: 1,
				}) ]
			}
	}

	fn push_macro(&mut self, tokens: Vec<Token>)
	{
		self.lexers.push(InnerLexer::MacroExpansion(MacroExpansion {
			tokens: tokens.into_iter(),
			}));
	}

	fn last(&self) -> &InnerLexer {
		assert!( self.lexers.len() > 1 );
		self.lexers.last().unwrap()
	}
	fn last_mut(&mut self) -> &mut InnerLexer {
		assert!( self.lexers.len() > 1 );
		self.lexers.last_mut().unwrap()
	}

	fn get_token(&mut self) -> super::ParseResult<Token>
	{
		loop
		{
			let t = match self.lexers.last_mut().unwrap()
				{
				InnerLexer::File(h) => h.get_token()?,
				InnerLexer::MacroExpansion(h) => h.get_token()?,
				};
			match t
			{
			Token::EOF if self.lexers.len() > 1 => {
				// EOF on inner parse: Pop and continue
				self.lexers.pop();
				},
			t => return Ok(t),
			}
		}
	}
}
impl LexHandle
{
	fn get_token(&mut self) -> super::ParseResult<Token>
	{
		match self.lexer.get_token()?
		{
		t @ Token::Whitespace => {
			self.line += 1;
			Ok(t)
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

