/*
 */

use parse::lex;
use parse::ParseResult;

struct ParseState<'ast>
{
	ast: &'ast mut ::ast::Program,
	lex: ::parse::preproc::Preproc,
}

pub fn parse(ast: &mut ::ast::Program, filename: &str) -> ParseResult<()>
{
	let mut self_ = ParseState {
		ast: ast,
		lex: try!(::parse::preproc::Preproc::new(filename))
		};
	
	self_.parseroot()
}

#[deriving(Show)]
enum TypeNode
{
	TypeNodeLeaf(String),
	TypeNodePtr(Box<TypeNode>, bool,bool),
	TypeNodeFcn(Box<TypeNode>, Vec<(::types::TypeRef,String)>),
}

macro_rules! syntax_error(
	($msg:expr) => ({ return Err(::parse::SyntaxError(format!("{}",$msg))) });
	($fmt:expr, $($arg:tt)*) => ({ return Err(::parse::SyntaxError(format!($fmt, $($arg)*))) });
	)
macro_rules! syntax_assert( ($tok:expr, $exp:pat) => (match $tok {
	$exp => {},
	tok @ _ => syntax_error!("Unexpected token {}, expected {}", tok, stringify!($exp)),
	})
	)

impl<'ast> ParseState<'ast>
{
	fn parseroot(&mut self) -> ParseResult<()>
	{
		loop
		{
			let tok = try!(self.lex.get_token());
			debug!("parseroot: tok={}", tok);
			match tok
			{
			lex::TokEOF => {
				break;
				},
			lex::TokRword_typedef => {
				let basetype = try!(self.get_base_type());
				debug!("do_typedef: basetype={}", basetype);
				let (typeid, name) = try!(self.get_full_type(basetype));
				self.ast.set_typedef(name, typeid);
				syntax_assert!( try!(self.lex.get_token()), lex::TokSemicolon );
				},
			_ => {
				self.lex.put_back(tok);
				try!( self.do_definition() )
				},
			}
		}
		
		Ok( () )
	}

	fn do_definition(&mut self) -> ParseResult<()>
	{
		// 1. Get base type
		let basetype = try!(self.get_base_type());
		debug!("do_definition: basetype={}", basetype);
		// 2. Get extended type and identifier
		let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
		// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
		match try!(self.lex.get_token())
		{
		// NOTE: The following would need changes for C++11 array literals
		lex::TokBraceOpen => return self.parse_function(typeid, ident),
		tok @ _ => {
			self.lex.put_back(tok);
			try!(self.parse_variable_def(typeid, ident));
			return self.parse_variable_list(basetype)
			}
		}
	}
	
	/// Read a single basic type.
	/// - Could be a primitive, a verbatim struct/union/enum, or a typedef
	fn get_base_type(&mut self) -> ParseResult<::types::TypeRef>
	{
		let mut is_const = false;
		let mut is_volatile = false;
		
		let mut typeid = None;
		let mut is_primitive = false;	// Set on any primitive specifier
		let mut is_signed = true;
		let mut intsize: Option<uint> = None;
		let mut int_seen = false;
		let mut double_seen = false;
		let mut is_unsigned = false;
		// 1. Storage classes (extern, static, auto, register)
		// 2. Type (with const mixed in)
		loop
		{
			match try!(self.lex.get_token())
			{
			// Const/Volatile
			lex::TokRword_const    => {is_const    = true; },
			lex::TokRword_volatile => {is_volatile = true; },
			// Primitives (Integer and Double)
			lex::TokRword_signed   => {is_signed = true ; },
			lex::TokRword_unsigned => {is_signed = false; },
			lex::TokRword_int => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				if int_seen { syntax_error!("Multiple 'int' keywords in type") }
				if intsize == Some(0) { syntax_error!("Invalid use of 'int'") }
				int_seen = true;
				intsize = Some(2);
				},
			lex::TokRword_char => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				if intsize.is_some() { syntax_error!("Invalid use of 'char'") }
				intsize = Some(0);
				},
			lex::TokRword_short => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				intsize = match intsize
					{
					None => Some(1),	// none => 'short'
					Some(2) => Some(1),	// 'int' => 'short'
					_ => syntax_error!("Invalid use of 'short'")
					};
				},
			lex::TokRword_long => {
				if double_seen {
					typeid = Some(::types::TypeFloat(::types::FloatClass_LongDouble));
					double_seen = false;
				}
				else {
					if typeid.is_some() { syntax_error!("Multiple types in definition") }
					intsize = match intsize
						{
						None => Some(3),	// no size yet, set to 'long'
						Some(2) => Some(3),	// 'int' => 'long'
						Some(3) => Some(4),	// 'long' => 'long long'
						_ => syntax_error!("Invalid use of 'long'")
						};
				}
				},
			lex::TokRword_float => {
				if typeid.is_some() || int_seen { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeFloat(::types::FloatClass_Float));
				},
			lex::TokRword_double => {
				if typeid.is_some() || int_seen { syntax_error!("Multiple types in definition") }
				if intsize == Some(3) {
					typeid = Some(::types::TypeFloat(::types::FloatClass_LongDouble));
				}
				else {
					typeid = Some(::types::TypeFloat(::types::FloatClass_Double));
					double_seen = true;
				}
				},
			// Simple types
			lex::TokRword_void => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeVoid);
				},
			lex::TokRword_struct => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeStruct(try!(self.get_struct())));
				},
			lex::TokIdent(n) => {
				if typeid.is_some() {
					self.lex.put_back( lex::TokIdent(n) );
					break;
				}
				match self.ast.get_typedef( n.as_slice() )
				{
				Some(v) => {
					typeid = Some(v.basetype.clone());
					is_const |= v.is_const;
					is_volatile |= v.is_volatile;
					},
				None => {
					self.lex.put_back( lex::TokIdent(n) );
					break;
					}
				}
				},
			tok @ _ => {
				self.lex.put_back(tok);
				break;
				}
			}
		}
		
		let rv =
			if typeid.is_some() {
				typeid.unwrap()
			}
			else if intsize.is_some() {
				::types::TypeInteger( match intsize.unwrap() {
				0 => ::types::IntClass_Char(is_signed),
				1 => ::types::IntClass_Short(is_signed),
				2 => ::types::IntClass_Int(is_signed),
				3 => ::types::IntClass_Long(is_signed),
				4 => ::types::IntClass_LongLong(is_signed),
				_ => fail!("BUGCHECK")
				})
			}
			else {
				syntax_error!("No type provided");
			};
		
		Ok( ::types::Type::new_ref( rv, is_const, is_volatile ) )
	}
	
	fn get_struct(&mut self) -> ParseResult<::types::StructRef>
	{
		fail!("TODO: get_struct");
	}
	
	fn get_full_type(&mut self, basetype: ::types::TypeRef) -> ParseResult<(::types::TypeRef,String)>
	{
		// 1. Handle pointers, with const-ness
		// 2. If brackets, recurse and expect close braket
		// 3. else, Expect ident
		// 4. Handle quare brackets and function args
		let mut typenode = try!(self.get_fulltype_ptr());
		debug!("get_full_type: typenode={}", typenode);
		let mut rettype = basetype;
		loop
		{
			typenode = match typenode
			{
			TypeNodeLeaf(name) => { return Ok( (rettype, name) ); },
			TypeNodePtr(sub,is_c,is_v) => {
				rettype = ::types::Type::new_ref( ::types::TypePointer(rettype), is_c, is_v );
				*sub
				},
			TypeNodeFcn(sub, args) => {
				fail!("TODO: Function type - rettype={}, sub={}, args={}", rettype, sub, args);
				*sub
				}
			}
		}
		// TODO: Unwrap typenode over basetype
		fail!("TODO: get_full_type");
	}
	fn get_fulltype_ptr(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokStar => {
			// TODO: Get const/volatile
			let is_const = false;
			let is_volatile = false;
			Ok( TypeNodePtr( box try!(self.get_fulltype_ptr()), is_const, is_volatile ) )
			},
		tok @ _ => {
			self.lex.put_back(tok);
			let mut rv = try!(self.get_fulltype_bottom());
			rv = try!(self.get_fulltype_fcn(rv));
			rv = try!(self.get_fulltype_array(rv));
			Ok( rv )
			}
		}
	}
	fn get_fulltype_bottom(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokParenOpen => {
			let rv = try!(self.get_fulltype_ptr());
			if try!(self.lex.get_token()) != lex::TokParenClose { syntax_error!("Expected ')')") }
			Ok(rv)
			},
		lex::TokIdent(v) => {
			Ok(TypeNodeLeaf(v))
			},
		tok @ _ => {
			syntax_error!("Unexpected {}, expected TokParenOpen or TokIdent", tok);
			}
		}
	}
	fn get_fulltype_fcn(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokParenOpen => {
			let mut args = Vec::new();
			loop
			{
				let basetype = try!(self.get_base_type());
				args.push( try!(self.get_full_type(basetype)) );
				match try!(self.lex.get_token())
				{
				lex::TokComma => {},
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
			syntax_assert!(try!(self.lex.get_token()), lex::TokParenClose);
			Ok( TypeNodeFcn(box inner, args) )
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(inner)
			}
		}
	}
	fn get_fulltype_array(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokSquareOpen => fail!("TODO: Arrays"),
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(inner)
			}
		}
	}
	
	fn parse_function(&mut self, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		// Opening brace has been eaten
		fail!("TODO: parse_function");
	}
	
	fn parse_variable_list(&mut self, basetype: ::types::TypeRef) -> ParseResult<()>
	{
		loop
		{
			match try!(self.lex.get_token())
			{
			lex::TokComma => {},
			lex::TokSemicolon => break,
			_ => return Err( ::parse::SyntaxError(format!("syntax error[parse_variable_list]")) )
			}
			
			let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
			try!(self.parse_variable_def(typeid, ident));
		}
		
		Ok( () )
	}
	
	fn parse_variable_def(&mut self, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		match try!(self.lex.get_token())
		{
		lex::TokAssign => {
			let value = try!(self.parse_expr());
			self.ast.define_variable(typeid, ident, Some(value));
			},
		tok @ _ => {
			self.lex.put_back(tok);
			self.ast.define_variable(typeid, ident, None);
			}
		}

		Ok( () )
	}
	
	// Expressions!
	fn parse_expr(&mut self) -> ParseResult<::ast::Node>
	{
		fail!("TODO: parse_expr");
	}
}

// vim: ft=rust ts=4 sw=4
