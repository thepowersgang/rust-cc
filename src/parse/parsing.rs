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
	
	match self_.parseroot()
	{
	Ok(o) => Ok(o),
	Err(e) => {
		error!("Parse error {} at {}", e, self_.lex);
		match e
		{
		::parse::SyntaxError(s) => Err( ::parse::SyntaxError(format!("{} {}", self_.lex, s)) ),
		_ => Err( e ),
		}
		}
	}
}

#[deriving(Show)]
enum TypeNode
{
	TypeNodeLeaf(String),
	TypeNodePtr(Box<TypeNode>, bool,bool),
	TypeNodeFcn(Box<TypeNode>, Vec<(::types::TypeRef,String)>),
	TypeNodeArray(Box<TypeNode>, Option<::ast::Node>),
}

macro_rules! syntax_error(
	($msg:expr) => ({ return Err(::parse::SyntaxError(format!("{}",$msg))) });
	($fmt:expr, $($arg:tt)*) => ({ return Err(::parse::SyntaxError(format!($fmt, $($arg)*))) });
	)
macro_rules! syntax_assert(
	($lex:expr : lex::$exp:ident) => (syntax_assert!(try!($lex.get_token()), lex::$exp));
	($lex:expr : lex::$exp:ident($($a:ident),+) => $v:expr) => (syntax_assert!(try!($lex.get_token()), lex::$exp($($a),+) => $v));
	($tok:expr, lex::$exp:ident) => (match $tok {
		lex::$exp => {},
		tok @ _ => syntax_error!("Unexpected token {}, expected {}", tok, stringify!($exp)),
		});
	($tok:expr, lex::$exp:ident($($a:ident),+) => $v:expr) => (match $tok {
		lex::$exp($($a),+) => $v,
		tok @ _ => syntax_error!("Unexpected token {}, expected {}", tok, stringify!($exp)),
		})
	)
macro_rules! peek_token( ($lex:expr, $tok:pat) => ({
	let lex = &mut $lex;
	match try!(lex.get_token()) {
	$tok => true,
	tok @ _ => { lex.put_back(tok); false }
	}
	})
	)
macro_rules! parse_todo( ($str:expr) => (return Err(::parse::Todo($str))) )

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
		// - Ignore empty ident
		if ident.as_slice() != ""
		{
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
		else
		{
			syntax_assert!( try!(self.lex.get_token()), lex::TokSemicolon );
			return Ok( () );
		}
	}
	
	fn get_base_type(&mut self) -> ParseResult<::types::TypeRef> {
		match try!(self.get_base_type_opt())
		{
		Some(t) => Ok(t),
		None => syntax_error!("No type provided, got token {}", try!(self.lex.get_token())),
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
		let mut intsize: Option<uint> = None;
		let mut int_seen = false;
		let mut double_seen = false;
		let mut is_unsigned = false;
		// 1. Storage classes (extern, static, auto, register)
		loop
		{
			match try!(self.lex.get_token())
			{
			lex::TokRword_extern =>   { storageclass = Some(::types::StorageExtern); },
			lex::TokRword_auto =>     { storageclass = Some(::types::StorageAuto); },
			lex::TokRword_static =>   { storageclass = Some(::types::StorageStatic); },
			lex::TokRword_register => { storageclass = Some(::types::StorageRegister); },
			lex::TokRword_inline =>   { error!("TODO: Handle 'inline'"); },
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
			lex::TokRword_const    => {is_const    = true; },
			lex::TokRword_volatile => {is_volatile = true; },
			// Primitives (Integer and Double)
			lex::TokRword_signed   => {is_signed = true ; seen_sign = true; },
			lex::TokRword_unsigned => {is_signed = false; seen_sign = true; },
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
			lex::TokRword_Bool => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeBool);
				},
			lex::TokRword_void => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeVoid);
				},
			lex::TokRword_struct => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeStruct(try!(self.get_struct())));
				},
			lex::TokRword_union => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeUnion(try!(self.get_union())));
				},
			lex::TokRword_enum => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::TypeEnum(try!(self.get_enum())));
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
					debug!("Encountered non-type ident {}", n);
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
			else if seen_sign {
				::types::TypeInteger( ::types::IntClass_Int(is_signed) )
			}
			else {
				// If any tokens were consumed during this function, we have to error
				if is_const || is_volatile || storageclass.is_some() {
					syntax_error!("No type provided, got token {}", try!(self.lex.get_token()));
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
		lex::TokIdent(n) => n,
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
		lex::TokBraceOpen => {
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
			if peek_token!(self.lex, lex::TokBraceClose) {
				break;
			}
			
			// 1. Get base type
			let basetype = try!(self.get_base_type());
			debug!("do_definition: basetype={}", basetype);
			// 2. Get extended type and identifier
			let (ft, ident) = try!(self.get_full_type(basetype.clone()));
			
			if peek_token!(self.lex, lex::TokColon)
			{
				// Ensure that `ft` is the correct type (an integer)
				let sign = match &ft.basetype
					{
					&::types::TypeInteger(::types::IntClass_Int(s)) => s,
					ft @ _ => syntax_error!("Invalid type for bitfield, expected signed/unsigned, got {}", ft),
					};
				let i = syntax_assert!( try!(self.lex.get_token()), lex::TokInteger(i,_class) => i as uint );
				let bt = ::types::TypeInteger(::types::IntClass_Bits(sign, i));
				items.push( (::types::Type::new_ref(bt, false, false), ident) );
				
				if peek_token!(self.lex, lex::TokComma) { parse_todo!("Comma separated bitfields"); }
				/*
				while( peek_token!(self.lex, lex::TokComma) )
				{
					items.push( try!(self.get_full_type(basetype.clone())) );
				}
				*/
			}
			else
			{
				items.push( (ft, ident) );
				while( peek_token!(self.lex, lex::TokComma) )
				{
					items.push( try!(self.get_full_type(basetype.clone())) );
				}
			}
			syntax_assert!( try!(self.lex.get_token()), lex::TokSemicolon );
		}
		
		if peek_token!(self.lex, lex::TokRword_gcc_attribute)
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
		lex::TokBraceOpen => {
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
			match self.ast.get_union(name.as_slice())
			{
			Some(er) => Ok(er),
			None => syntax_error!("No union by the name '{}'", name),
			}
			}
		}
	}
	fn populate_union(&mut self) -> ParseResult<Vec<(::types::TypeRef,String)>>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, lex::TokBraceClose) {
				break;
			}
			
			// 1. Get base type
			let basetype = try!(self.get_base_type());
			debug!("populate_union: basetype={}", basetype);
			// 2. Get extended type and identifier
			items.push( try!(self.get_full_type(basetype.clone())) );
			
			while( peek_token!(self.lex, lex::TokComma) )
			{
				items.push( try!(self.get_full_type(basetype.clone())) );
			}
			syntax_assert!( try!(self.lex.get_token()), lex::TokSemicolon );
		}
		
		Ok( items )
	}
	
	fn get_enum(&mut self) -> ParseResult<::types::EnumRef>
	{
		let name = try!(self._get_ident_or_blank());
		
		// Check for defining the enum's contents
		match try!(self.lex.get_token())
		{
		lex::TokBraceOpen => {
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
			match self.ast.get_enum(name.as_slice())
			{
			Some(er) => Ok(er),
			None => syntax_error!("No enum by the name '{}'", name),
			}
			}
		}
	}
	fn populate_enum(&mut self) -> ParseResult<Vec<(uint,String)>>
	{
		let mut curval = 0;
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, lex::TokBraceClose) {
				break;
			}
			let name = syntax_assert!( try!(self.lex.get_token()), lex::TokIdent(v) => v );
			
			if peek_token!(self.lex, lex::TokAssign) {
				// This can be a constant expression
				let node = try!(self.parse_expr());
				let val = match node.literal_integer()
					{
					Some(v) => v as uint,
					None => syntax_error!("Non-literal used to set enum value"),
					};
				curval = val;
			}
			items.push( (curval, name) );
			curval += 1;
			match try!(self.lex.get_token())
			{
			lex::TokComma => continue,
			lex::TokBraceClose => break,
			t @ _ => syntax_error!("Unexpected token {}, expected TokComma or TokBraceClose", t),
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
					rettype = ::types::Type::new_ref( ::types::TypeFunction(rettype, args), false, false );
					*sub
					},
				TypeNodeArray(sub, size) => {
					// TODO: Parse size somehow. Need to propagate the size up the chain
					rettype = ::types::Type::new_ref( ::types::TypeArray(rettype), false, false );
					*sub
					},
				};
			debug!("get_full_type: rettype={}, typenode={}", rettype, typenode);
		}
	}
	
	/// Handle pointers in types
	fn get_fulltype_ptr(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokStar => {
			// Get const/volatile
			let mut is_const = false;
			let mut is_volatile = false;
			loop
			{
				match try!(self.lex.get_token())
				{
				lex::TokRword_const    => { is_const    = true; },
				lex::TokRword_volatile => { is_volatile = true; },
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
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
	/// Handle the bottom layer (either parentheses, or an identifier)
	fn get_fulltype_bottom(&mut self) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokParenOpen => {
			debug!("get_fulltype_bottom - Parentheses");
			let rv = try!(self.get_fulltype_ptr());
			if try!(self.lex.get_token()) != lex::TokParenClose { syntax_error!("Expected ')')") }
			Ok(rv)
			},
		lex::TokIdent(v) => {
			Ok(TypeNodeLeaf(v))
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(TypeNodeLeaf("".to_string()))
			}
		}
	}
	/// Handle function types (parentheses after identifier)
	fn get_fulltype_fcn(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokParenOpen => {
			debug!("get_fulltype_fcn - Parentheses");
			let mut args = Vec::new();
			// Arguments!
			loop
			{
				if peek_token!(self.lex, lex::TokVargs) {
					args.push( ( ::types::Type::new_ref(::types::TypeVoid,false,false), "...".to_string()) );
					break;
				}
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
			// Special case handling of (void)
			if args.len() == 1 && args[0] == (::types::Type::new_ref(::types::TypeVoid,false,false),"".to_string()) {
				args.clear();
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
	/// Handle array definition
	fn get_fulltype_array(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match try!(self.lex.get_token())
		{
		lex::TokSquareOpen => {
			// If next token == TokSquareClose, return an array with null node
			let sizenode = match try!(self.lex.get_token())
				{
				lex::TokSquareClose => None,
				t @ _ => {
					self.lex.put_back(t);
					let size = try!(self.parse_expr());
					syntax_assert!( try!(self.lex.get_token()), lex::TokSquareClose );
					Some( size )
					},
				};
			Ok( TypeNodeArray(box inner, sizenode) )
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
		debug!("parse_function: code = {}", code);
		self.ast.define_variable(typeid, ident, Some(code));
		Ok( () )
	}
	
	fn parse_opt_block(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.parse_block_line())
		{
		Some(n) => n,
		None => ::ast::NodeBlock(Vec::new()),
		})
	}
	
	fn parse_block(&mut self) -> ParseResult<::ast::Node>
	{
		// Opening brace has been eaten
		let mut statements = Vec::new();
		
		while !peek_token!(self.lex, lex::TokBraceClose)
		{
			match try!(self.parse_block_line())
			{
			Some(stmt) => statements.push(stmt),
			None => {},
			}
		}
		
		Ok( ::ast::NodeBlock(statements) )
	}
	
	/// Parse a single line in a block
	fn parse_block_line(&mut self) -> ParseResult<Option<::ast::Node>>
	{
		Ok(match try!(self.get_base_type_opt())
		{
		Some(basetype) => {
			debug!("parse_block_line - basetype={}", basetype);
			// Definition!
			let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
			let rv = if ident.as_slice() != ""
				{
					// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
					if peek_token!(self.lex, lex::TokBraceOpen)
					{
						// NOTE: The following would need changes for C++11 array literals
						parse_todo!("Nested functions");
					}
					else
					{
						// TODO: Create local
						let init = if peek_token!(self.lex, lex::TokAssign) {
								Some(box try!(self.parse_expr()))
							}
							else {
								None
							};
						let rv = ::ast::NodeDefVar(typeid, ident, init);
						while peek_token!(self.lex, lex::TokComma)
						{
							parse_todo!("Multiple local variables");
						}
						Some( rv )
					}
				}
				else
				{
					None
				};
			syntax_assert!(try!(self.lex.get_token()), lex::TokSemicolon);
			rv
			},
		None => Some(match try!(self.lex.get_token())
			{
			lex::TokSemicolon => return Ok(None),
			lex::TokBraceOpen => try!(self.parse_block()),
			lex::TokRword_return => if peek_token!(self.lex, lex::TokSemicolon) {
					::ast::NodeReturn( None )
				} else {
					let rv = ::ast::NodeReturn( Some(box try!(self.parse_expr())) );
					syntax_assert!(try!(self.lex.get_token()), lex::TokSemicolon);
					rv
				},
			lex::TokRword_while => {
				let cnd = box try!(self.parse_expr());
				let code = box try!(self.parse_opt_block());
				::ast::NodeWhileLoop(cnd,code)
				},
			lex::TokRword_if => {
				let cnd = box try!(self.parse_expr());
				let tcode = box try!(self.parse_opt_block());
				let fcode = if peek_token!(self.lex, lex::TokRword_else) {
						Some( box try!(self.parse_opt_block()) )
					} else {
						None
					};
				::ast::NodeIfStatement(cnd,tcode,fcode)
				},
			lex::TokRword_switch => try!(self.parse_switch_statement()),
			// Expression
			t @ _ => {
				self.lex.put_back(t);
				let rv = try!(self.parse_expr());
				syntax_assert!(try!(self.lex.get_token()), lex::TokSemicolon);
				rv
				}
			}), 
		})
	}
	
	fn parse_switch_statement(&mut self) -> ParseResult<::ast::Node>
	{
		let cnd = box try!(self.parse_expr());
		let mut code = Vec::new();
		syntax_assert!(self.lex : lex::TokBraceOpen);
		loop
		{
			match try!(self.lex.get_token())
			{
			lex::TokBraceClose => break,
			lex::TokRword_default => {
				code.push( ::ast::NodeCaseDefault );
				syntax_assert!(self.lex : lex::TokColon);
				},
			lex::TokRword_case => {
				let first = match try!(self.parse_expr_0()).literal_integer()
					{
					Some(i) => i as uint,
					None => syntax_error!("Case value is not literal"),
					};
				let last = if peek_token!(self.lex, lex::TokVargs) {
						Some( match try!(self.parse_expr_0()).literal_integer()
						{
						Some(i) => i as uint,
						None => syntax_error!("Case value is not literal"),
						})
					} else {
						None
					};
				syntax_assert!(self.lex : lex::TokColon);
				match last
				{
				Some(last) => code.push( ::ast::NodeCaseRange(first, last) ),
				None => code.push( ::ast::NodeCaseSingle(first) ),
				}
				},
			t @ _ => {
				self.lex.put_back(t);
				match try!(self.parse_block_line()) {
				Some(n) => code.push(n),
				None => {},
				}
				}
			}
		}
		Ok( ::ast::NodeSwitch(cnd, code) )
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
}

// Parse, left associative
macro_rules! parse_left_assoc(
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
)

impl<'ast> ParseState<'ast>
{
	// ----------------------------------------------------------------
	// Expressions!
	// ----------------------------------------------------------------
	fn parse_expr(&mut self) -> ParseResult<::ast::Node>
	{
		return self.parse_expr_0();
	}
	
	/// Parse #0 : Assignment
	fn parse_expr_0(&mut self) -> ParseResult<::ast::Node>
	{
		let rv = try!(self.parse_expr_1());
		Ok( match try!(self.lex.get_token())
		{
		lex::TokAssign => ::ast::NodeAssign(box rv, box try!(self.parse_expr_0())),
		lex::TokAssignBitAnd => ::ast::NodeAssignOp(::ast::BinOpBitAnd, box rv, box try!(self.parse_expr_0())),
		lex::TokAssignBitOr  => ::ast::NodeAssignOp(::ast::BinOpBitOr,  box rv, box try!(self.parse_expr_0())),
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
		lex::TokQuestionMark => {
			debug!("Ternary, rv (cnd) = {}", rv);
			let tv = box try!(self.parse_expr_1());
			debug!("Ternary - tv = {}", tv);
			syntax_assert!(try!(self.lex.get_token()), lex::TokColon);
			let fv = box try!(self.parse_expr_1());
			debug!("Ternary - fv = {}", fv);
			::ast::NodeTernary(box rv, tv, fv)
			}
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #2 - Boolean AND/OR
	parse_left_assoc!(self, parse_expr_2, parse_expr_3, rv, {
		lex::TokDoublePipe      => ::ast::NodeBinOp(::ast::BinOpLogicOr,  box rv, box try!(self.parse_expr_3())),
		lex::TokDoubleAmpersand => ::ast::NodeBinOp(::ast::BinOpLogicAnd, box rv, box try!(self.parse_expr_3())),
	})
	
	/// Expresission #3 - Bitwise
	parse_left_assoc!(self, parse_expr_3, parse_expr_4, rv, {
		lex::TokPipe      => ::ast::NodeBinOp(::ast::BinOpBitOr , box rv, box try!(self.parse_expr_4())),
		lex::TokAmpersand => ::ast::NodeBinOp(::ast::BinOpBitAnd, box rv, box try!(self.parse_expr_4())),
		lex::TokCaret     => ::ast::NodeBinOp(::ast::BinOpBitXor, box rv, box try!(self.parse_expr_4())),
	})
	
	/// Expression #4 - Comparison Operators
	parse_left_assoc!(self, parse_expr_4, parse_expr_5, rv, {
		lex::TokEquality => ::ast::NodeBinOp(::ast::BinOpCmpEqu, box rv, box try!(self.parse_expr_5())),
		lex::TokNotEquals => ::ast::NodeBinOp(::ast::BinOpCmpNEqu, box rv, box try!(self.parse_expr_5())),
		lex::TokLt  => ::ast::NodeBinOp(::ast::BinOpCmpLt,  box rv, box try!(self.parse_expr_5())),
		lex::TokLtE => ::ast::NodeBinOp(::ast::BinOpCmpLtE, box rv, box try!(self.parse_expr_5())),
		lex::TokGt  => ::ast::NodeBinOp(::ast::BinOpCmpGt,  box rv, box try!(self.parse_expr_5())),
		lex::TokGtE => ::ast::NodeBinOp(::ast::BinOpCmpGtE, box rv, box try!(self.parse_expr_5())),
	})
	
	/// Expression #5 - Bit Shifts
	parse_left_assoc!(self, parse_expr_5, parse_expr_6, rv, {
		lex::TokShiftLeft  => ::ast::NodeBinOp(::ast::BinOpShiftLeft,  box rv, box try!(self.parse_expr_6())),
		lex::TokShiftRight => ::ast::NodeBinOp(::ast::BinOpShiftRight, box rv, box try!(self.parse_expr_6())),
	})
		
	/// Expresion #6 - Arithmatic
	parse_left_assoc!(self, parse_expr_6, parse_expr_7, rv, {
		lex::TokPlus  => ::ast::NodeBinOp(::ast::BinOpAdd, box rv, box try!(self.parse_expr_7())),
		lex::TokMinus => ::ast::NodeBinOp(::ast::BinOpSub, box rv, box try!(self.parse_expr_7())),
	})
	
	/// Expression #7 - Multiply/Divide
	parse_left_assoc!(self, parse_expr_7, parse_expr_8, rv, {
		lex::TokStar  => ::ast::NodeBinOp(::ast::BinOpMul, box rv, box try!(self.parse_expr_8())),
		lex::TokSlash => ::ast::NodeBinOp(::ast::BinOpDiv, box rv, box try!(self.parse_expr_8())),
		lex::TokPercent => ::ast::NodeBinOp(::ast::BinOpMod, box rv, box try!(self.parse_expr_8())),
	})
	
	/// Expression #8 - Unary Righthand
	parse_left_assoc!(self, parse_expr_8, parse_expr_9, rv, {
		lex::TokDoublePlus  => ::ast::NodeUniOp(::ast::UniOpPostInc, box rv),
		lex::TokDoubleMinus => ::ast::NodeUniOp(::ast::UniOpPostDec, box rv),
	})
	
	/// Expression #9 - Unary left
	fn parse_expr_9(&mut self) -> ParseResult<::ast::Node>
	{
		Ok( match try!(self.lex.get_token())
		{
		lex::TokMinus       => ::ast::NodeUniOp(::ast::UniOpNeg,      box try!(self.parse_expr_9())),
		lex::TokTilde       => ::ast::NodeUniOp(::ast::UniOpBitNot,   box try!(self.parse_expr_9())),
		lex::TokExclamation => ::ast::NodeUniOp(::ast::UniOpLogicNot, box try!(self.parse_expr_9())),
		lex::TokDoublePlus  => ::ast::NodeUniOp(::ast::UniOpPreInc,   box try!(self.parse_expr_9())),
		lex::TokDoubleMinus => ::ast::NodeUniOp(::ast::UniOpPreDec,   box try!(self.parse_expr_9())),
		lex::TokAmpersand   => ::ast::NodeUniOp(::ast::UniOpAddress, box try!(self.parse_expr_member())),	// different, as double addr is inval
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_expr_member())
			},
		})
	}
	
	
	/// Expression - Member access
	parse_left_assoc!(self, parse_expr_member, parse_expr_P, rv, {
		lex::TokDerefMember => ::ast::NodeDerefMember(box rv, syntax_assert!(self.lex : lex::TokIdent(i) => i)),
		lex::TokPeriod      => ::ast::NodeMember(     box rv, syntax_assert!(self.lex : lex::TokIdent(i) => i)),
		lex::TokSquareOpen => {
				let idx = box try!(self.parse_expr());
				syntax_assert!(self.lex : lex::TokSquareClose);
				::ast::NodeIndex(box rv, idx)
				},
		lex::TokParenOpen => {
			let mut args = Vec::new();
			if ! peek_token!(self.lex, lex::TokParenClose)
			{
				loop
				{
					args.push( try!(self.parse_expr()) );
					if peek_token!(self.lex, lex::TokParenClose) {
						break;
					}
					syntax_assert!(self.lex : lex::TokComma);
				}
			}
			::ast::NodeFcnCall(box rv, args)
			},
	})
	
	/// Expression - Parens
	fn parse_expr_P(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		// - Either a cast, or a grouped expression
		lex::TokParenOpen => match try!(self.get_base_type_opt())
			{
			Some(basetype) => parse_todo!("Cast"),
			None => {
					let rv = try!(self.parse_expr_0());
					syntax_assert!(try!(self.lex.get_token()), lex::TokParenClose);
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
		match try!(self.lex.get_token())
		{
		lex::TokIdent(id) => Ok( ::ast::NodeIdentifier(id) ),
		lex::TokString(s) => Ok( ::ast::NodeString(s) ),
		lex::TokInteger(v,_) => Ok( ::ast::NodeInteger(v) ),
		t @ _ => Err( ::parse::SyntaxError(format!("Unexpected {}, expected value", t)) ),
		}
	}
}

// vim: ft=rust ts=4 sw=4
