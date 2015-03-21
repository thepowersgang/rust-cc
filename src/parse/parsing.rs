/*
 */

use parse::lex;
use parse::lex::Token;
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
	
	match self_.parseroot()
	{
	Ok(o) => Ok(o),
	Err(e) => {
		error!("Parse error {:?} at {}", e, self_.lex);
		match e
		{
		::parse::Error::SyntaxError(s) => Err( ::parse::Error::SyntaxError(format!("{} {}", self_.lex, s)) ),
		_ => Err( e ),
		}
		}
	}
}

#[derive(Debug)]
enum TypeNode
{
	Leaf(String),
	Ptr(Box<TypeNode>, bool,bool),
	Fcn(Box<TypeNode>, Vec<(::types::TypeRef,String)>),
	Array(Box<TypeNode>, Option<::ast::Node>),
}

macro_rules! syntax_error{
	($msg:expr) => ({ return Err(::parse::Error::SyntaxError(format!("{}",$msg))) });
	($fmt:expr, $($arg:tt)*) => ({ return Err(::parse::Error::SyntaxError(format!($fmt, $($arg)*))) });
	}
macro_rules! syntax_assert {
	($lex:expr => Token::$exp:ident) => (syntax_assert!(try!($lex.get_token()), Token::$exp));
	($lex:expr => Token::$exp:ident($($a:ident),+) @ $v:expr) => (syntax_assert!(try!($lex.get_token()), Token::$exp($($a),+) => $v));
	($tok:expr, Token::$exp:ident) => (match $tok {
		Token::$exp => {},
		tok @ _ => syntax_error!("Unexpected token {:?}, expected {:?}", tok, stringify!($exp)),
		});
	($tok:expr, Token::$exp:ident($($a:ident),+) => $v:expr) => (match $tok {
		Token::$exp($($a),+) => $v,
		tok @ _ => syntax_error!("Unexpected token {:?}, expected {:?}", tok, stringify!($exp)),
		})
	}
macro_rules! peek_token_nc{ ($lex:expr, $tok:pat) => ({
	let lex = &mut $lex;
	match try!(lex.get_token()) {
	tok @ $tok => { lex.put_back(tok); true },
	tok @ _ => { lex.put_back(tok); false }
	}
	})
	}
macro_rules! peek_token{ ($lex:expr, $tok:pat) => ({
	let lex = &mut $lex;
	match try!(lex.get_token()) {
	$tok => true,
	tok @ _ => { lex.put_back(tok); false }
	}
	})
	}
macro_rules! parse_todo{ ($str:expr) => (return Err(::parse::Error::Todo($str))) }

impl<'ast> ParseState<'ast>
{
	fn parseroot(&mut self) -> ParseResult<()>
	{
		loop
		{
			let tok = try!(self.lex.get_token());
			debug!("parseroot: tok={:?}", tok);
			match tok
			{
			Token::EOF => {
				break;
				},
			Token::Rword_typedef => {
				let basetype = try!(self.get_base_type());
				debug!("do_typedef: basetype={:?}", basetype);
				let (typeid, name) = try!(self.get_full_type(basetype));
				self.ast.set_typedef(name, typeid);
				syntax_assert!( try!(self.lex.get_token()), Token::Semicolon );
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
		debug!("do_definition: basetype={:?}", basetype);
		// 2. Get extended type and identifier
		let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
		// - Ignore empty ident
		if ident.as_slice() != ""
		{
			// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
			match try!(self.lex.get_token())
			{
			// NOTE: The following would need changes for C++11 array literals
			Token::BraceOpen => return self.parse_function(typeid, ident),
			tok @ _ => {
				self.lex.put_back(tok);
				try!(self.parse_variable_def(typeid, ident));
				return self.parse_variable_list(basetype)
				}
			}
		}
		else
		{
			syntax_assert!( try!(self.lex.get_token()), Token::Semicolon );
			return Ok( () );
		}
	}
	
	fn get_base_type(&mut self) -> ParseResult<::types::TypeRef> {
		match try!(self.get_base_type_opt())
		{
		Some(t) => Ok(t),
		None => syntax_error!("No type provided, got token {:?}", try!(self.lex.get_token())),
		}
	}
	
	/// Read a single basic type.
	/// - Could be a primitive, a verbatim struct/union/enum, or a typedef
	fn get_base_type_opt(&mut self) -> ParseResult<Option<::types::TypeRef>>
	{
		let mut is_const = false;
		let mut is_volatile = false;
		let mut storageclass = None;
		
		let mut typeid = None;
		let mut is_primitive = false;	// Set on any primitive specifier
		let mut is_signed = true;
		let mut seen_sign = false;
		let mut intsize: Option<u8> = None;
		let mut int_seen = false;
		let mut double_seen = false;
		let mut is_unsigned = false;
		// 1. Storage classes (extern, static, auto, register)
		loop
		{
			match try!(self.lex.get_token())
			{
			Token::Rword_extern =>   { storageclass = Some(::types::StorageClass::Extern); },
			Token::Rword_auto =>     { storageclass = Some(::types::StorageClass::Auto); },
			Token::Rword_static =>   { storageclass = Some(::types::StorageClass::Static); },
			Token::Rword_register => { storageclass = Some(::types::StorageClass::Register); },
			Token::Rword_inline =>   { error!("TODO: Handle 'inline'"); },
			tok @ _ => {
				self.lex.put_back(tok);
				break;
				}
			}
		}
		// 2. Type (with const mixed in)
		loop
		{
			match try!(self.lex.get_token())
			{
			// Const/Volatile
			Token::Rword_const    => {is_const    = true; },
			Token::Rword_volatile => {is_volatile = true; },
			// Primitives (Integer and Double)
			Token::Rword_signed   => {is_signed = true ; seen_sign = true; },
			Token::Rword_unsigned => {is_signed = false; seen_sign = true; },
			Token::Rword_int => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				if int_seen { syntax_error!("Multiple 'int' keywords in type") }
				if intsize == Some(0) { syntax_error!("Invalid use of 'int'") }
				int_seen = true;
				intsize = Some(2);
				},
			Token::Rword_char => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				if intsize.is_some() { syntax_error!("Invalid use of 'char'") }
				intsize = Some(0);
				},
			Token::Rword_short => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				intsize = match intsize
					{
					None => Some(1),	// none => 'short'
					Some(2) => Some(1),	// 'int' => 'short'
					_ => syntax_error!("Invalid use of 'short'")
					};
				},
			Token::Rword_long => {
				if double_seen {
					typeid = Some(::types::BaseType::Float(::types::FloatClass::LongDouble));
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
			Token::Rword_float => {
				if typeid.is_some() || int_seen { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Float(::types::FloatClass::Float));
				},
			Token::Rword_double => {
				if typeid.is_some() || int_seen { syntax_error!("Multiple types in definition") }
				if intsize == Some(3) {
					typeid = Some(::types::BaseType::Float(::types::FloatClass::LongDouble));
				}
				else {
					typeid = Some(::types::BaseType::Float(::types::FloatClass::Double));
					double_seen = true;
				}
				},
			// Simple types
			Token::Rword_Bool => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Bool);
				},
			Token::Rword_void => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Void);
				},
			Token::Rword_struct => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Struct(try!(self.get_struct())));
				},
			Token::Rword_union => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Union(try!(self.get_union())));
				},
			Token::Rword_enum => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Enum(try!(self.get_enum())));
				},
			Token::Ident(n) => {
				if typeid.is_some() {
					self.lex.put_back( Token::Ident(n) );
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
					debug!("Encountered non-type ident {:?}", n);
					self.lex.put_back( Token::Ident(n) );
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
				::types::BaseType::Integer( match intsize.unwrap() {
				0 => ::types::IntClass::Char(is_signed),
				1 => ::types::IntClass::Short(is_signed),
				2 => ::types::IntClass::Int(is_signed),
				3 => ::types::IntClass::Long(is_signed),
				4 => ::types::IntClass::LongLong(is_signed),
				_ => panic!("BUGCHECK")
				})
			}
			else if seen_sign {
				::types::BaseType::Integer( ::types::IntClass::Int(is_signed) )
			}
			else {
				// If any tokens were consumed during this function, we have to error
				if is_const || is_volatile || storageclass.is_some() {
					syntax_error!("No type provided, got token {:?}", try!(self.lex.get_token()));
				}
				else {
					// Otherwise, leave it up to the caller
					return Ok( None )
				}
			};
		
		Ok( Some(::types::Type::new_ref( rv, is_const, is_volatile )) )
	}
	
	fn _get_ident_or_blank(&mut self) -> ParseResult<String> {
		Ok( match try!(self.lex.get_token())
		{
		Token::Ident(n) => n,
		tok @ _ => {
			self.lex.put_back(tok);
			"".to_string()
			}
		})
	}
	
	fn get_struct(&mut self) -> ParseResult<::types::StructRef>
	{
		let structname = try!(self._get_ident_or_blank());
		
		let ret = self.ast.get_struct(structname.as_slice());
		// Check for defining the structure's contents
		match try!(self.lex.get_token())
		{
		Token::BraceOpen => {
			if ret.borrow().is_populated() { syntax_error!("Multiple defintions of struct '{}'", structname); }
			try!(self.populate_struct(ret.clone()));
			},
		tok @ _ => {
			self.lex.put_back(tok);
			if structname.as_slice() == "" { syntax_error!("Nameless struct with no definition"); }
			}
		}
		Ok( ret )
	}
	fn populate_struct(&mut self, structinfo: ::types::StructRef) -> ParseResult<()>
	{
		assert!( !structinfo.borrow().is_populated() );
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, Token::BraceClose) {
				break;
			}
			
			// 1. Get base type
			let basetype = try!(self.get_base_type());
			debug!("do_definition: basetype={:?}", basetype);
			// 2. Get extended type and identifier
			let (ft, ident) = try!(self.get_full_type(basetype.clone()));
			
			if peek_token!(self.lex, Token::Colon)
			{
				// Ensure that `ft` is the correct type (an integer)
				let sign = match &ft.basetype
					{
					&::types::BaseType::Integer(::types::IntClass::Int(s)) => s,
					ft @ _ => syntax_error!("Invalid type for bitfield, expected signed/unsigned, got {:?}", ft),
					};
				let i = syntax_assert!( try!(self.lex.get_token()), Token::Integer(i,_class) => i as u8 );
				let bt = ::types::BaseType::Integer(::types::IntClass::Bits(sign, i));
				items.push( (::types::Type::new_ref(bt, false, false), ident) );
				
				if peek_token!(self.lex, Token::Comma) { parse_todo!("Comma separated bitfields"); }
				/*
				while( peek_token!(self.lex, Token::Comma) )
				{
					items.push( try!(self.get_full_type(basetype.clone())) );
				}
				*/
			}
			else
			{
				items.push( (ft, ident) );
				while( peek_token!(self.lex, Token::Comma) )
				{
					items.push( try!(self.get_full_type(basetype.clone())) );
				}
			}
			syntax_assert!( try!(self.lex.get_token()), Token::Semicolon );
		}
		
		if peek_token!(self.lex, Token::Rword_gcc_attribute)
		{
			parse_todo!("Handle GCC __attribute__ on struct");
		}
		
		structinfo.borrow_mut().set_items( items );
		Ok( () )
	}

	fn get_union(&mut self) -> ParseResult<::types::UnionRef>
	{
		let name = try!(self._get_ident_or_blank());
		
		// Check for defining the enum's contents
		match try!(self.lex.get_token())
		{
		Token::BraceOpen => {
			let fields = try!(self.populate_union());
			match self.ast.make_union(name.as_slice(), fields)
			{
			Ok(er) => Ok(er),
			Err( () ) => syntax_error!("Multiple definitions of union '{}'", name),
			}
			},
		tok @ _ => {
			self.lex.put_back(tok);
			if name.as_slice() == "" { syntax_error!("Nameless union with no definition"); }
			Ok( self.ast.get_union(name.as_slice()) )
			}
		}
	}
	fn populate_union(&mut self) -> ParseResult<Vec<(::types::TypeRef,String)>>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, Token::BraceClose) {
				break;
			}
			
			// 1. Get base type
			let basetype = try!(self.get_base_type());
			debug!("populate_union: basetype={:?}", basetype);
			// 2. Get extended type and identifier
			items.push( try!(self.get_full_type(basetype.clone())) );
			
			while( peek_token!(self.lex, Token::Comma) )
			{
				items.push( try!(self.get_full_type(basetype.clone())) );
			}
			syntax_assert!( try!(self.lex.get_token()), Token::Semicolon );
		}
		
		Ok( items )
	}
	
	// ---
	// enums
	// ---
	fn get_enum(&mut self) -> ParseResult<::types::EnumRef>
	{
		let name = try!(self._get_ident_or_blank());
		
		// Check for defining the enum's contents
		match try!(self.lex.get_token())
		{
		Token::BraceOpen => {
			let fields = try!(self.populate_enum());
			match self.ast.make_enum(name.as_slice(), fields)
			{
			Ok(er) => Ok(er),
			Err(opt_str) => match opt_str
				{
				Some(fname) => syntax_error!("Multiple definitions of name '{}'", fname),
				None => syntax_error!("Multiple definitions of enum '{}'", name),
				},
			}
			},
		tok @ _ => {
			self.lex.put_back(tok);
			if name.as_slice() == "" { syntax_error!("Nameless enum with no definition"); }
			Ok( self.ast.get_enum(name.as_slice()) )
			}
		}
	}
	fn populate_enum(&mut self) -> ParseResult<Vec<(u64,String)>>
	{
		let mut curval = 0;
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, Token::BraceClose) {
				break;
			}
			let name = syntax_assert!( try!(self.lex.get_token()), Token::Ident(v) => v );
			
			if peek_token!(self.lex, Token::Assign) {
				// This can be a constant expression
				let node = try!(self.parse_expr());
				let val = match node.literal_integer()
					{
					Some(v) => v as u64,
					None => syntax_error!("Non-literal used to set enum value"),
					};
				curval = val;
			}
			items.push( (curval, name) );
			curval += 1;
			match try!(self.lex.get_token())
			{
			Token::Comma => continue,
			Token::BraceClose => break,
			t @ _ => syntax_error!("Unexpected token {:?}, expected Token::Comma or TokBraceClose", t),
			}
		}
	
		Ok( items )
	}
	
	/// Parse a full type (Pointers, arrays, and functions)
	///	
	/// Implements a recursive-descent parser, to handle function types correctly
	fn get_full_type(&mut self, basetype: ::types::TypeRef) -> ParseResult<(::types::TypeRef,String)>
	{
		let mut typenode = try!(self.get_fulltype_ptr());
		debug!("get_full_type: typenode={:?}", typenode);
		let mut rettype = basetype;
		loop
		{
			typenode = match typenode
				{
				TypeNode::Leaf(name) => { return Ok( (rettype, name) ); },
				TypeNode::Ptr(sub,is_c,is_v) => {
					rettype = ::types::Type::new_ref( ::types::BaseType::Pointer(rettype), is_c, is_v );
					*sub
					},
				TypeNode::Fcn(sub, args) => {
					rettype = ::types::Type::new_ref( ::types::BaseType::Function(rettype, args), false, false );
					*sub
					},
				TypeNode::Array(sub, size) => {
					// TODO: Parse size somehow. Need to propagate the size up the chain
					rettype = ::types::Type::new_ref( ::types::BaseType::Array(rettype), false, false );
					*sub
					},
				};
			debug!("get_full_type: rettype={:?}, typenode={:?}", rettype, typenode);
		}
	}
	
	/// Handle pointers in types
	fn get_fulltype_ptr(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		Token::Star => {
			// Get const/volatile
			let mut is_const = false;
			let mut is_volatile = false;
			loop
			{
				match try!(self.lex.get_token())
				{
				Token::Rword_const    => { is_const    = true; },
				Token::Rword_volatile => { is_volatile = true; },
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
			Ok( TypeNode::Ptr( box try!(self.get_fulltype_ptr()), is_const, is_volatile ) )
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
	/// Handle the bottom layer (either parentheses, or an identifier)
	fn get_fulltype_bottom(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		Token::ParenOpen => {
			debug!("get_fulltype_bottom - Parentheses");
			let rv = try!(self.get_fulltype_ptr());
			if try!(self.lex.get_token()) != Token::ParenClose { syntax_error!("Expected ')')") }
			Ok(rv)
			},
		Token::Ident(v) => {
			Ok(TypeNode::Leaf(v))
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(TypeNode::Leaf("".to_string()))
			}
		}
	}
	/// Handle function types (parentheses after identifier)
	fn get_fulltype_fcn(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		Token::ParenOpen => {
			debug!("get_fulltype_fcn - Parentheses");
			let mut args = Vec::new();
			// Arguments!
			loop
			{
				if peek_token!(self.lex, Token::Vargs) {
					args.push( ( ::types::Type::new_ref(::types::BaseType::Void,false,false), "...".to_string()) );
					break;
				}
				let basetype = try!(self.get_base_type());
				args.push( try!(self.get_full_type(basetype)) );
				match try!(self.lex.get_token())
				{
				Token::Comma => {},
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
			// Special case handling of (void)
			if args.len() == 1 && args[0] == (::types::Type::new_ref(::types::BaseType::Void,false,false),"".to_string()) {
				args.clear();
			}
			syntax_assert!(try!(self.lex.get_token()), Token::ParenClose);
			Ok( TypeNode::Fcn(box inner, args) )
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(inner)
			}
		}
	}
	/// Handle array definition
	fn get_fulltype_array(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		Token::SquareOpen => {
			// If next token == Token::SquareClose, return an array with null node
			let sizenode = match try!(self.lex.get_token())
				{
				Token::SquareClose => None,
				t @ _ => {
					self.lex.put_back(t);
					let size = try!(self.parse_expr());
					syntax_assert!( try!(self.lex.get_token()), Token::SquareClose );
					Some( size )
					},
				};
			Ok( TypeNode::Array(box inner, sizenode) )
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(inner)
			}
		}
	}
	
	fn parse_function(&mut self, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		let code = try!(self.parse_block());
		debug!("parse_function: code = {:?}", code);
		self.ast.define_variable(typeid, ident, Some(code));
		Ok( () )
	}
	
	fn parse_opt_block(&mut self) -> ParseResult<::ast::Node>
	{
		let mut exprs = try!(self.parse_block_line());
		if exprs.len() == 1 {
			Ok( exprs.remove(0) )
		}
		else {
			Ok( ::ast::Node::Block(exprs) )
		}
	}
	
	fn parse_block(&mut self) -> ParseResult<::ast::Node>
	{
		// Opening brace has been eaten
		let mut statements = Vec::new();
		
		while !peek_token!(self.lex, Token::BraceClose)
		{
			statements.extend( try!(self.parse_block_line()).into_iter() );
		}
		
		Ok( ::ast::Node::Block(statements) )
	}
	
	fn try_parse_local_var(&mut self) -> ParseResult<Option<Vec<::ast::Node>>>
	{
		Ok(match try!(self.get_base_type_opt())
		{
		Some(basetype) => {
			debug!("parse_block_line - basetype={:?}", basetype);
			// Definition!
			let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
			let rv = if ident.as_slice() != ""
				{
					// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
					if peek_token!(self.lex, Token::BraceOpen)
					{
						// NOTE: The following would need changes for C++11 array literals
						parse_todo!("Nested functions");
					}
					else
					{
						// TODO: Create local
						let init = if peek_token!(self.lex, Token::Assign) {
								Some(box try!(self.parse_expr()))
							}
							else {
								None
							};
						let mut rv = vec![ ::ast::Node::DefVar(typeid, ident, init) ];
						while peek_token!(self.lex, Token::Comma)
						{
							let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
							let init = if peek_token!(self.lex, Token::Assign) {
									Some(box try!(self.parse_expr()))
								}
								else {
									None
								};
							
							rv.push( ::ast::Node::DefVar(typeid, ident, init) );
						}
						Some(rv)
					}
				}
				else
				{
					Some(vec![])
				};
			rv
			},
		None => None
		})
	}
	
	/// Parse a single line in a block
	fn parse_block_line(&mut self) -> ParseResult<Vec<::ast::Node>>
	{
		// Attempt to get a type, returns None if no type was present
		Ok(match try!(self.try_parse_local_var())
		{
		Some(n) => {
			syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
			n
			},
		None => vec![match try!(self.lex.get_token())
			{
			Token::Semicolon => return Ok(vec![]),
			Token::BraceOpen => try!(self.parse_block()),
			Token::Rword_return => if peek_token!(self.lex, Token::Semicolon) {
					::ast::Node::Return( None )
				} else {
					let rv = ::ast::Node::Return( Some(box try!(self.parse_expr_list())) );
					syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
					rv
				},
			Token::Rword_break => {
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Node::Break
				},
			Token::Rword_continue => {
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Node::Continue
				},
			Token::Rword_goto => {
				let dest = syntax_assert!(self.lex => Token::Ident(s) @ s);
				syntax_assert!(self.lex => Token::Semicolon);
				::ast::Node::Goto(dest)
				},
			Token::Rword_while => {
				let cnd = box try!(self.parse_expr_list());
				let code = box try!(self.parse_opt_block());
				::ast::Node::WhileLoop(cnd,code)
				},
			Token::Rword_do => {
				let code = box try!(self.parse_opt_block());
				syntax_assert!(self.lex => Token::Rword_while);
				let cnd = box try!(self.parse_expr_list());
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Node::DoWhileLoop(cnd,code)
				},
			Token::Rword_for => try!(self.parse_for_loop()),
			Token::Rword_if => {
				let cnd = box try!(self.parse_expr());
				let tcode = box try!(self.parse_opt_block());
				let fcode = if peek_token!(self.lex, Token::Rword_else) {
						Some( box try!(self.parse_opt_block()) )
					} else {
						None
					};
				::ast::Node::IfStatement(cnd,tcode,fcode)
				},
			Token::Rword_switch => try!(self.parse_switch_statement()),
			
			t @ Token::Ident(_) => {
				self.lex.put_back(t);
				let rv = try!(self.parse_expr_list());
				if let ::ast::Node::Identifier(i) = rv
				{
					if peek_token!(self.lex, Token::Colon) {
						::ast::Node::Label(i)
					}
					else {
						syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
						::ast::Node::Identifier(i)
					}
				}
				else {
					syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
					rv
				}
				},
			
			// Expression
			t @ _ => {
				self.lex.put_back(t);
				let rv = try!(self.parse_expr_list());
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				rv
				}
			}], 
		})
	}
	
	/// Parse a for loop
	// ('for' has been eaten)
	fn parse_for_loop(&mut self) -> ParseResult<::ast::Node>
	{
		syntax_assert!(self.lex => Token::ParenOpen);
		debug!("parse_for_loop");
		let init = if peek_token_nc!(self.lex, Token::Semicolon) {
				None
			} else {
				match try!(self.try_parse_local_var())
				{
				Some(n) => Some(box ::ast::Node::StmtList(n)),
				None => Some(box try!(self.parse_expr_list())),
				}
			};
		syntax_assert!(self.lex => Token::Semicolon);
		debug!("parse_for_loop: init = {:?}", init);
		let cnd = if peek_token_nc!(self.lex, Token::Semicolon) {
				None
			} else {
				Some(box try!(self.parse_expr_list()))
			};
		syntax_assert!(self.lex => Token::Semicolon);
		debug!("parse_for_loop: cnd = {:?}", cnd);
		let inc = if peek_token_nc!(self.lex, Token::ParenClose) {
				None
			} else {
				Some(box try!(self.parse_expr_list()))
			};
		syntax_assert!(self.lex => Token::ParenClose);
		debug!("parse_for_loop: inc = {:?}", inc);
		let code = box try!(self.parse_opt_block());
		Ok( ::ast::Node::ForLoop(init,cnd,inc,code) )
	}
	
	fn parse_switch_statement(&mut self) -> ParseResult<::ast::Node>
	{
		let cnd = box try!(self.parse_expr());
		let mut code = Vec::new();
		syntax_assert!(self.lex => Token::BraceOpen);
		loop
		{
			match try!(self.lex.get_token())
			{
			Token::BraceClose => break,
			Token::Rword_default => {
				code.push( ::ast::Node::CaseDefault );
				syntax_assert!(self.lex => Token::Colon);
				},
			Token::Rword_case => {
				let first = match try!(self.parse_expr_0()).literal_integer()
					{
					Some(i) => i as u64,
					None => syntax_error!("Case value is not literal"),
					};
				if peek_token!(self.lex, Token::Vargs) {
					let last = match try!(self.parse_expr_0()).literal_integer()
						{
						Some(i) => i as u64,
						None => syntax_error!("Case value is not literal"),
						};
					code.push( ::ast::Node::CaseRange(first, last) );
				}
				else {
					code.push( ::ast::Node::CaseSingle(first) );
				}
				syntax_assert!(self.lex => Token::Colon);
				},
			t @ _ => {
				self.lex.put_back(t);
				code.extend( try!(self.parse_block_line()).into_iter() );
				}
			}
		}
		Ok( ::ast::Node::Switch(cnd, code) )
	}
	
	fn parse_variable_list(&mut self, basetype: ::types::TypeRef) -> ParseResult<()>
	{
		loop
		{
			match try!(self.lex.get_token())
			{
			Token::Comma => {},
			Token::Semicolon => break,
			_ => return Err( ::parse::Error::SyntaxError(format!("syntax error[parse_variable_list]")) )
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
		Token::Assign => {
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
}

// Parse, left associative
macro_rules! parse_left_assoc{
	($_self:ident, $name:ident, $next:ident, $rv:ident, { $($patterns:pat => $vals:expr),*, }) => {
		fn $name(&mut $_self) -> ParseResult<::ast::Node> {
			let mut $rv = try!($_self.$next());
			loop
			{
				$rv = match try!($_self.lex.get_token())
					{
					$($patterns => $vals),*,
					t @ _ => {
						$_self.lex.put_back(t);
						break;
						}
					};
			}
			Ok($rv)
		}
	}
}

impl<'ast> ParseState<'ast>
{
	// ----------------------------------------------------------------
	// Expressions!
	// ----------------------------------------------------------------
	fn parse_expr(&mut self) -> ParseResult<::ast::Node>
	{
		return self.parse_expr_0();
	}
	fn parse_expr_list(&mut self) -> ParseResult<::ast::Node>
	{
		let exp = try!(self.parse_expr());
		if peek_token_nc!(self.lex, Token::Comma)
		{
			let mut exprs = vec![exp];
			while peek_token!(self.lex, Token::Comma)
			{
				exprs.push( try!(self.parse_expr()) );
			}
			Ok(::ast::Node::StmtList(exprs))
		}
		else
		{
			Ok(exp)
		}
	}
	
	/// Parse #0 : Assignment
	fn parse_expr_0(&mut self) -> ParseResult<::ast::Node>
	{
		let rv = try!(self.parse_expr_1());
		Ok( match try!(self.lex.get_token())
		{
		Token::Assign => ::ast::Node::Assign(box rv, box try!(self.parse_expr_0())),
		Token::AssignBitAnd => ::ast::Node::AssignOp(::ast::BinOp::BitAnd, box rv, box try!(self.parse_expr_0())),
		Token::AssignBitOr  => ::ast::Node::AssignOp(::ast::BinOp::BitOr,  box rv, box try!(self.parse_expr_0())),
		Token::AssignAdd  => ::ast::Node::AssignOp(::ast::BinOp::Add,  box rv, box try!(self.parse_expr_0())),
		Token::AssignSub  => ::ast::Node::AssignOp(::ast::BinOp::Sub,  box rv, box try!(self.parse_expr_0())),
		Token::AssignMul  => ::ast::Node::AssignOp(::ast::BinOp::Mul,  box rv, box try!(self.parse_expr_0())),
		Token::AssignDiv  => ::ast::Node::AssignOp(::ast::BinOp::Div,  box rv, box try!(self.parse_expr_0())),
		Token::AssignMod  => ::ast::Node::AssignOp(::ast::BinOp::Mod,  box rv, box try!(self.parse_expr_0())),
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #1 - Ternary
	fn parse_expr_1(&mut self) -> ParseResult<::ast::Node>
	{
		let rv = try!(self.parse_expr_2());
		
		Ok( match try!(self.lex.get_token())
		{
		Token::QuestionMark => {
			debug!("Ternary, rv (cnd) = {:?}", rv);
			let tv = box try!(self.parse_expr_1());
			debug!("Ternary - tv = {:?}", tv);
			syntax_assert!(try!(self.lex.get_token()), Token::Colon);
			let fv = box try!(self.parse_expr_1());
			debug!("Ternary - fv = {:?}", fv);
			::ast::Node::Ternary(box rv, tv, fv)
			}
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #2 - Boolean AND/OR
	parse_left_assoc!{self, parse_expr_2, parse_expr_3, rv, {
		Token::DoublePipe      => ::ast::Node::BinOp(::ast::BinOp::LogicOr,  box rv, box try!(self.parse_expr_3())),
		Token::DoubleAmpersand => ::ast::Node::BinOp(::ast::BinOp::LogicAnd, box rv, box try!(self.parse_expr_3())),
	}}
	
	/// Expresission #3 - Bitwise
	parse_left_assoc!{self, parse_expr_3, parse_expr_4, rv, {
		Token::Pipe      => ::ast::Node::BinOp(::ast::BinOp::BitOr , box rv, box try!(self.parse_expr_4())),
		Token::Ampersand => ::ast::Node::BinOp(::ast::BinOp::BitAnd, box rv, box try!(self.parse_expr_4())),
		Token::Caret     => ::ast::Node::BinOp(::ast::BinOp::BitXor, box rv, box try!(self.parse_expr_4())),
	}}
	
	/// Expression #4 - Comparison Operators
	parse_left_assoc!{self, parse_expr_4, parse_expr_5, rv, {
		Token::Equality => ::ast::Node::BinOp(::ast::BinOp::CmpEqu, box rv, box try!(self.parse_expr_5())),
		Token::NotEquals => ::ast::Node::BinOp(::ast::BinOp::CmpNEqu, box rv, box try!(self.parse_expr_5())),
		Token::Lt  => ::ast::Node::BinOp(::ast::BinOp::CmpLt,  box rv, box try!(self.parse_expr_5())),
		Token::LtE => ::ast::Node::BinOp(::ast::BinOp::CmpLtE, box rv, box try!(self.parse_expr_5())),
		Token::Gt  => ::ast::Node::BinOp(::ast::BinOp::CmpGt,  box rv, box try!(self.parse_expr_5())),
		Token::GtE => ::ast::Node::BinOp(::ast::BinOp::CmpGtE, box rv, box try!(self.parse_expr_5())),
	}}
	
	/// Expression #5 - Bit Shifts
	parse_left_assoc!{self, parse_expr_5, parse_expr_6, rv, {
		Token::ShiftLeft  => ::ast::Node::BinOp(::ast::BinOp::ShiftLeft,  box rv, box try!(self.parse_expr_6())),
		Token::ShiftRight => ::ast::Node::BinOp(::ast::BinOp::ShiftRight, box rv, box try!(self.parse_expr_6())),
	}}
	
	/// Expresion #6 - Arithmatic
	parse_left_assoc!{self, parse_expr_6, parse_expr_7, rv, {
		Token::Plus  => ::ast::Node::BinOp(::ast::BinOp::Add, box rv, box try!(self.parse_expr_7())),
		Token::Minus => ::ast::Node::BinOp(::ast::BinOp::Sub, box rv, box try!(self.parse_expr_7())),
	}}
	
	/// Expression #7 - Multiply/Divide
	parse_left_assoc!{self, parse_expr_7, parse_expr_8, rv, {
		Token::Star  => ::ast::Node::BinOp(::ast::BinOp::Mul, box rv, box try!(self.parse_expr_8())),
		Token::Slash => ::ast::Node::BinOp(::ast::BinOp::Div, box rv, box try!(self.parse_expr_8())),
		Token::Percent => ::ast::Node::BinOp(::ast::BinOp::Mod, box rv, box try!(self.parse_expr_8())),
	}}
	
	/// Expression #8 - Unary Righthand
	parse_left_assoc!{self, parse_expr_8, parse_expr_9, rv, {
		Token::DoublePlus  => ::ast::Node::UniOp(::ast::UniOp::PostInc, box rv),
		Token::DoubleMinus => ::ast::Node::UniOp(::ast::UniOp::PostDec, box rv),
	}}
	
	/// Expression #9 - Unary left
	fn parse_expr_9(&mut self) -> ParseResult<::ast::Node>
	{
		Ok( match try!(self.lex.get_token())
		{
		Token::Minus       => ::ast::Node::UniOp(::ast::UniOp::Neg,      box try!(self.parse_expr_9())),
		Token::Tilde       => ::ast::Node::UniOp(::ast::UniOp::BitNot,   box try!(self.parse_expr_9())),
		Token::Exclamation => ::ast::Node::UniOp(::ast::UniOp::LogicNot, box try!(self.parse_expr_9())),
		Token::DoublePlus  => ::ast::Node::UniOp(::ast::UniOp::PreInc,   box try!(self.parse_expr_9())),
		Token::DoubleMinus => ::ast::Node::UniOp(::ast::UniOp::PreDec,   box try!(self.parse_expr_9())),
		Token::Star        => ::ast::Node::UniOp(::ast::UniOp::Deref,    box try!(self.parse_expr_9())),
		Token::Ampersand   => ::ast::Node::UniOp(::ast::UniOp::Address, box try!(self.parse_expr_member())),	// different, as double addr is inval
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_expr_member())
			},
		})
	}
	
	
	/// Expression - Member access
	parse_left_assoc!{self, parse_expr_member, parse_expr_P, rv, {
		Token::DerefMember => ::ast::Node::DerefMember(box rv, syntax_assert!(self.lex => Token::Ident(i) @ i)),
		Token::Period      => ::ast::Node::Member(     box rv, syntax_assert!(self.lex => Token::Ident(i) @ i)),
		Token::SquareOpen => {
				let idx = box try!(self.parse_expr());
				syntax_assert!(self.lex => Token::SquareClose);
				::ast::Node::Index(box rv, idx)
				},
		Token::ParenOpen => {
			let mut args = Vec::new();
			if ! peek_token!(self.lex, Token::ParenClose)
			{
				loop
				{
					args.push( try!(self.parse_expr()) );
					if peek_token!(self.lex, Token::ParenClose) {
						break;
					}
					syntax_assert!(self.lex => Token::Comma);
				}
			}
			::ast::Node::FcnCall(box rv, args)
			},
	}}
	
	/// Expression - Parens
	fn parse_expr_P(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		// - Either a cast, or a grouped expression
		Token::ParenOpen => match try!(self.get_base_type_opt())
			{
			Some(basetype) => {
				let (fulltype, ident) = try!(self.get_full_type(basetype));
				if ident.as_slice() != "" {
					syntax_error!("Unexpected identifier in cast");
				}
				syntax_assert!(self.lex => Token::ParenClose);
				::ast::Node::Cast(fulltype, box try!(self.parse_expr_P()))
				},
			None => {
					let rv = try!(self.parse_expr());
					syntax_assert!(try!(self.lex.get_token()), Token::ParenClose);
					rv
					},
			},
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_expr_Z())
			}
		})
	}
	/// Expression - Leaf nodes
	fn parse_expr_Z(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		Token::Ident(id) => ::ast::Node::Identifier(id),
		Token::String(s) => {
			let mut val = s;
			loop
			{
				match try!(self.lex.get_token()) {
				Token::String(s) => { val.push_str(s.as_slice()); },
				t @ _ => { self.lex.put_back(t); break; }
				}
			}
			::ast::Node::String(val)
			},
		Token::Integer(v,_) => ::ast::Node::Integer(v),
		Token::BraceOpen => try!(self.parse_composite_lit()),
		Token::Rword_sizeof => {
			let expect_paren = peek_token!(self.lex, Token::ParenOpen);
			let rv = match try!(self.get_base_type_opt())
				{
				Some(t) => {
					let (tr, name) = try!(self.get_full_type(t));
					if ! name.is_empty() {
						syntax_error!("Unexpected name in sizeof");
					}
					::ast::Node::SizeofType(tr)
					},
				None => {
					let val = if expect_paren { box try!(self.parse_expr_0()) } else { box try!(self.parse_expr_P()) };
					::ast::Node::SizeofExpr(val)
					},
				};
			if expect_paren {
				syntax_assert!(self.lex => Token::ParenClose);
			}
			rv
			},
		t @ _ => syntax_error!("Unexpected {:?}, expected value", t),
		})
	}
	
	fn parse_composite_lit(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		Token::BraceClose => {
			::ast::Node::ListLiteral(vec![])
			},
		t @ Token::Period => {
			self.lex.put_back(t);
			try!(self.parse_struct_lit())
			},
		t @ Token::SquareOpen => {
			self.lex.put_back(t);
			try!(self.parse_array_lit())
			},
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_list_lit())
			},
		})
	}
	
	fn parse_list_lit(&mut self) -> ParseResult<::ast::Node>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token_nc!(self.lex, Token::BraceClose) {
				break;
			}
			let val = try!(self.parse_expr());
			items.push(val);
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);

		
		Ok( ::ast::Node::ListLiteral(items) )
	}
	
	fn parse_struct_lit(&mut self) -> ParseResult<::ast::Node>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token_nc!(self.lex, Token::BraceClose) {
				break;
			}
			syntax_assert!(self.lex => Token::Period);
			let name = syntax_assert!(self.lex => Token::Ident(i) @ i);
			syntax_assert!(self.lex => Token::Assign);
			let val = try!(self.parse_expr());
			
			items.push( (name,val) );
			
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);
		
		Ok( ::ast::Node::StructLiteral(items) )
	}
	
	fn parse_array_lit(&mut self) -> ParseResult<::ast::Node>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token_nc!(self.lex, Token::BraceClose) {
				break;
			}
			syntax_assert!(self.lex => Token::SquareOpen);
			let idx_expr = try!(self.parse_expr());
			let idx = match idx_expr.literal_integer()
				{
				Some(i) => i as usize,
				None => syntax_error!("Non-constant expression {:?} used for array initialise index", idx_expr),
				};
			syntax_assert!(self.lex => Token::SquareClose);
			syntax_assert!(self.lex => Token::Assign);
			let val = try!(self.parse_expr());
			
			items.push( (idx,val) );
			
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);
		
		Ok( ::ast::Node::ArrayLiteral(items) )
	}
}

// vim: ft=rust ts=4 sw=4
