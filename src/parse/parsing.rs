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
	($tok:expr, lex::$exp:ident) => (match $tok {
		lex::$exp => {},
		tok @ _ => syntax_error!("Unexpected token {}, expected {}", tok, stringify!($exp)),
		});
	($tok:expr, lex::$exp:ident($a:ident) => $v:expr) => (match $tok {
		lex::$exp($a) => $v,
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
	
	/// Read a single basic type.
	/// - Could be a primitive, a verbatim struct/union/enum, or a typedef
	fn get_base_type(&mut self) -> ParseResult<::types::TypeRef>
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
				syntax_error!("No type provided, got token {}", try!(self.lex.get_token()));
			};
		
		Ok( ::types::Type::new_ref( rv, is_const, is_volatile ) )
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
			items.push( try!(self.get_full_type(basetype.clone())) );
			
			while( peek_token!(self.lex, lex::TokComma) )
			{
				items.push( try!(self.get_full_type(basetype.clone())) );
			}
			syntax_assert!( try!(self.lex.get_token()), lex::TokSemicolon );
		}
		
		if peek_token!(self.lex, lex::TokRword_gcc_attribute)
		{
			return Err( ::parse::Todo("Handle GCC __attribute__ on struct") );
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
		// TODO: Unwrap typenode over basetype
		return Err(::parse::Todo("get_full_type"));
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
	fn parse_block(&mut self) -> ParseResult<::ast::Node>
	{
		// Opening brace has been eaten
		let mut statements = Vec::new();
		
		loop
		{
			statements.push( match try!(self.lex.get_token())
			{
			lex::TokBraceClose => break,
			lex::TokRword_return => {
				::ast::NodeReturn( box try!(self.parse_expr()) )
				},
			t @ _ => {
				self.lex.put_back(t);
				try!(self.parse_expr())
				}
			} );
			syntax_assert!(try!(self.lex.get_token()), lex::TokSemicolon);
		}
		
		Ok( ::ast::NodeBlock(statements) )
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
	})
	
	/// Expression #8 - Unary Righthand
	parse_left_assoc!(self, parse_expr_8, parse_expr_9, rv, {
		lex::TokDoublePlus  => ::ast::NodeUniOp(::ast::UniOpPostInc, box rv),
		//lex::TokDoubleMinus => ::ast::NodeUniOp(::ast::UniOpPostDec, box rv),
	})
	
	/// Expression #9 - Unary left
	fn parse_expr_9(&mut self) -> ParseResult<::ast::Node>
	{
		Ok( match try!(self.lex.get_token())
		{
		lex::TokMinus => ::ast::NodeBinOp(::ast::BinOpSub, box ::ast::NodeInteger(0), box try!(self.parse_expr_P())),
		lex::TokDoublePlus  => ::ast::NodeUniOp(::ast::UniOpPreInc, box try!(self.parse_expr_P())),
		//lex::TokDoubleMinus => ::ast::NodeUniOp(::ast::UniOpPreDec, box try!(self.parse_expr_P())),
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_expr_P())
			},
		})
	}
	
	
	/// Expression - Member access
	
	/// Expression - Parens
	fn parse_expr_P(&mut self) -> ParseResult<::ast::Node>
	{
		loop
		{
			match try!(self.lex.get_token())
			{
			lex::TokParenOpen => {
					return Err( ::parse::Todo("Paren/Cast") );
				},
			t @ _ => {
				self.lex.put_back(t);
				return self.parse_expr_Z();
				}
			}
		}
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
