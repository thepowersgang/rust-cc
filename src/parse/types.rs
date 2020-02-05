//! Parser for types
use super::ParseResult;
use super::Token;

#[derive(Debug)]
enum TypeNode
{
	/// Inner identifier (e.g. `foo` in `int *foo[2]`
	Leaf(String),
	/// A pointer (with qualifiers for the pointer value - not the pointee)
	Ptr(Box<TypeNode>, ::types::Qualifiers),
	/// Function type (return type and arguments, with optional names)
	Fcn(Box<TypeNode>, Vec<(::types::TypeRef,String)>),
	/// Array type, with optional size node
	Array(Box<TypeNode>, Option<::ast::Node>),
}

impl<'ast> super::ParseState<'ast>
{
	/// Parse a bare type name (no pointer handling)
	pub fn get_base_type(&mut self) -> ParseResult<::types::TypeRef> {
		match self.get_base_type_opt()?
		{
		Some(t) => Ok(t),
		None => syntax_error!("No type provided, got token {:?}", self.lex.get_token()?),
		}
	}
	
	/// Read a single basic type.
	/// - Could be a primitive, a verbatim struct/union/enum, or a typedef
	pub fn get_base_type_opt(&mut self) -> ParseResult<Option<::types::TypeRef>>
	{
		let mut qualifiers = ::types::Qualifiers::new();
		let mut storageclass = None;
		
		let mut typeid = None;
		//let mut is_primitive = false;	// Set on any primitive specifier
		let mut is_signed: Option<bool> = None;	// 
		let mut intsize: Option<u8> = None;
		let mut int_seen = false;
		let mut double_seen = false;
		
		if peek_token!(self.lex, Token::Ident(ref n) if n == "__attribute__") {
			panic!("{}TODO: Handle gcc __attribute__ at start of type", self.lex);
		}

		// 1. Storage classes (extern, static, auto, register)
		loop
		{
			match self.lex.get_token()?
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
			match self.lex.get_token()?
			{
			// Const/Volatile
			Token::Rword_const    => { qualifiers.set_const(); },
			Token::Rword_volatile => { qualifiers.set_volatile(); },
			Token::Rword_restrict => { qualifiers.set_restrict(); },
			// Primitives (Integer and Double)
			Token::Rword_signed   => {is_signed = Some(true ); },
			Token::Rword_unsigned => {is_signed = Some(false); },
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
				typeid = Some(::types::BaseType::Struct(self.get_struct()?));
				},
			Token::Rword_union => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Union(self.get_union()?));
				},
			Token::Rword_enum => {
				if typeid.is_some() { syntax_error!("Multiple types in definition") }
				typeid = Some(::types::BaseType::Enum(self.get_enum()?));
				},
			Token::Ident(ref n) if n == "__gnuc_va_list" => {
				typeid = Some(::types::BaseType::MagicType(::types::MagicType::VaList));
				},
			Token::Ident(ref n) if n == "__magictype__" => {
				syntax_assert!(self.lex.get_token()?, Token::ParenOpen);
				let s = syntax_assert!(self.lex.get_token()?, Token::String(s) => s);
				syntax_assert!(self.lex.get_token()?, Token::ParenClose);
				let (name, repr) = {
					let mut it = s.splitn(2, ':');
					( it.next().unwrap().to_owned(), it.next().expect("No repr in magic type").to_owned() )
					};
				typeid = Some(::types::BaseType::MagicType(::types::MagicType::Named(name, repr)));
				},
			Token::Ident(n) => {
				if typeid.is_some() {
					self.lex.put_back( Token::Ident(n) );
					break;
				}
				match self.ast.get_typedef( &n )
				{
				Some(v) => {
					typeid = Some(v.basetype.clone());
					qualifiers.merge_from( &v.qualifiers );
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
				let signedness = ::types::Signedness::from_bool_signed(is_signed.unwrap_or(true));
				::types::BaseType::Integer( match intsize.unwrap() {
				0 => ::types::IntClass::Char(is_signed.map(|_| signedness)),
				1 => ::types::IntClass::Short(signedness),
				2 => ::types::IntClass::Int(signedness),
				3 => ::types::IntClass::Long(signedness),
				4 => ::types::IntClass::LongLong(signedness),
				_ => panic!("BUGCHECK")
				})
			}
			else if let Some(is_signed) = is_signed {
				::types::BaseType::Integer( ::types::IntClass::Int( ::types::Signedness::from_bool_signed(is_signed) ) )
			}
			else {
				// If any tokens were consumed during this function, we have to error
				if qualifiers != ::types::Qualifiers::new() || storageclass.is_some() {
					syntax_error!("No type provided, got token {:?}", self.lex.get_token()?);
				}
				else {
					// Otherwise, leave it up to the caller
					return Ok( None )
				}
			};
		
		Ok( Some(::types::Type::new_ref(rv, qualifiers)) )
	}
	/// Parse a full type (Pointers, arrays, and functions) from a base (starting) type
	///	
	/// Implements a separate recursive-descent parser, to handle function types correctly
	///
	/// Returns both the fully-generated type, and the inner identifier of the type
	pub fn get_full_type(&mut self, basetype: ::types::TypeRef) -> ParseResult<(::types::TypeRef,String)>
	{
		let mut typenode = self.get_fulltype_ptr()?;
		debug!("get_full_type: typenode={:?}", typenode);
		let mut rettype = basetype;
		loop
		{
			typenode = match typenode
				{
				TypeNode::Leaf(name) => { return Ok( (rettype, name) ); },
				TypeNode::Ptr(sub, qual) => {
					rettype = ::types::Type::new_ref( ::types::BaseType::Pointer(rettype), qual );
					*sub
					},
				TypeNode::Fcn(sub, args) => {
					rettype = ::types::Type::new_ref_bare( ::types::BaseType::Function( ::types::FunctionType { ret: rettype, args } ) );
					*sub
					},
				TypeNode::Array(sub, size) => {
					let array_size = if let Some(size_expr) = size {
							match size_expr.literal_integer()
							{
							Some(v) => ::types::ArraySize::Fixed(v),
							None => ::types::ArraySizeExpr::new(size_expr).into(),
							}
						}
						else {
							::types::ArraySize::None
						};
					rettype = ::types::Type::new_ref_bare( ::types::BaseType::Array(rettype, array_size)  );
					*sub
					},
				};
			debug!("get_full_type: rettype={:?}, typenode={:?}", rettype, typenode);
		}
	}
	
	/// Handle pointers in types
	fn get_fulltype_ptr(&mut self) -> ParseResult<TypeNode>
	{
		match self.lex.get_token()?
		{
		Token::Star => {
			// Get const/volatile
			let mut qualifiers = ::types::Qualifiers::new();
			loop
			{
				match self.lex.get_token()?
				{
				Token::Rword_const    => { qualifiers.set_const(); }
				Token::Rword_volatile => { qualifiers.set_volatile(); }
				Token::Rword_restrict => { qualifiers.set_restrict(); }
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
			Ok( TypeNode::Ptr( box self.get_fulltype_ptr()?, qualifiers ) )
			},
		tok @ _ => {
			self.lex.put_back(tok);
			let rv = self.get_fulltype_bottom()?;
			let rv = self.get_fulltype_fcn(rv)?;
			let rv = self.get_fulltype_array(rv)?;
			Ok( rv )
			}
		}
	}
	/// Handle the bottom layer (either parentheses, or an identifier)
	fn get_fulltype_bottom(&mut self) -> ParseResult<TypeNode>
	{
		match self.lex.get_token()?
		{
		Token::ParenOpen => {
			debug!("get_fulltype_bottom - Parentheses");
			let rv = self.get_fulltype_ptr()?;
			if self.lex.get_token()? != Token::ParenClose {
				syntax_error!("Expected ')')")
			}
			Ok(rv)
			},
		Token::Ident(v) => {
			Ok(TypeNode::Leaf(v))
			},
		tok @ _ => {
			self.lex.put_back(tok);
			Ok(TypeNode::Leaf("".to_owned()))
			}
		}
	}
	/// Handle function types (parentheses after identifier)
	fn get_fulltype_fcn(&mut self, inner: TypeNode) -> ParseResult<TypeNode>
	{
		match self.lex.get_token()?
		{
		Token::ParenOpen => {
			debug!("get_fulltype_fcn - Parentheses");
			let mut args = Vec::new();
			// Arguments!
			loop
			{
				if peek_token!(self.lex, Token::Vargs) {
					args.push( ( ::types::Type::new_ref_bare(::types::BaseType::Void), "...".to_string()) );
					break;
				}
				let basetype = self.get_base_type()?;
				args.push( self.get_full_type(basetype)? );
				match self.lex.get_token()?
				{
				Token::Comma => {},
				tok @ _ => {
					self.lex.put_back(tok);
					break;
					}
				}
			}
			// Special case handling of (void)
			if args.len() == 1 && args[0] == (::types::Type::new_ref_bare(::types::BaseType::Void),"".to_string()) {
				args.clear();
			}
			syntax_assert!(self.lex.get_token()?, Token::ParenClose);

			if peek_token!(self.lex, Token::Ident(ref n) if n == "__attribute__") {
				self.parse_gcc_attributes(|self_, name, _opts|
					match &name[..]
					{
					"noreturn" => {
						/* TODO: noreturn */
						Ok( () )
						},
					"warn_unused_result" => {
						/* TODO: warn_unused_result */
						Ok( () )
						},
					"deprecated" => {
						/* TODO: deprecated */
						Ok( () )
						},
					_ => panic!("{}: TODO - Handle GCC __attribute__(({})) on function", self_.lex, name),
					}
					)?;
			}

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
		match self.lex.get_token()?
		{
		Token::SquareOpen => {
			// If next token == Token::SquareClose, return an array with null node
			let sizenode = match self.lex.get_token()?
				{
				Token::SquareClose => None,
				t @ _ => {
					self.lex.put_back(t);
					let size = self.parse_expr()?;
					syntax_assert!(self.lex.get_token()?, Token::SquareClose);
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
}

impl<'ast> super::ParseState<'ast>
{
	fn _get_ident_or_blank(&mut self) -> ParseResult<String> {
		Ok( match self.lex.get_token()?
		{
		Token::Ident(n) => n,
		tok @ _ => {
			self.lex.put_back(tok);
			String::new()
			}
		})
	}
	
	fn get_struct(&mut self) -> ParseResult<::types::StructRef>
	{
		let name = self._get_ident_or_blank()?;
		
		// Check for defining the structure's contents
		match self.lex.get_token()?
		{
		Token::BraceOpen => {
			let fields = self.populate_struct()?;
			match self.ast.make_struct(&name, fields)
			{
			Ok(sr) => Ok(sr),
			Err( () ) => syntax_error!("Multiple definitions of struct '{}'", name),
			}
			},
		tok @ _ => {
			self.lex.put_back(tok);
			if name == "" { syntax_error!("Nameless struct with no definition"); }
			Ok( self.ast.get_struct(&name) )
			}
		}
	}
	fn populate_struct(&mut self) -> ParseResult<Vec<(::types::TypeRef,String)>>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token!(self.lex, Token::BraceClose) {
				break;
			}
			
			// 1. Get base type
			let basetype = self.get_base_type()?;
			debug!("do_definition: basetype={:?}", basetype);
			// 2. Get extended type and identifier
			let (ft, ident) = self.get_full_type(basetype.clone())?;
			
			// - Handle bitfields
			if peek_token!(self.lex, Token::Colon)
			{
				// Ensure that `ft` is the correct type (an integer)
				let sign = match &ft.basetype
					{
					&::types::BaseType::Integer(::types::IntClass::Int(s)) => s,
					ft @ _ => syntax_error!("Invalid type for bitfield, expected signed/unsigned, got {:?}", ft),
					};
				let i = syntax_assert!( self.lex.get_token()?, Token::Integer(i,_class,_s) => i as u8 );
				let bt = ::types::BaseType::Integer(::types::IntClass::Bits(sign, i));
				items.push( (::types::Type::new_ref_bare(bt), ident) );
				
				if peek_token!(self.lex, Token::Comma) { parse_todo!("Comma separated bitfields"); }
				/*
				while peek_token!(self.lex, Token::Comma)
				{
					items.push( self.get_full_type(basetype.clone())? );
				}
				*/
			}
			else
			{
				items.push( (ft, ident) );
				while peek_token!(self.lex, Token::Comma)
				{
					items.push( self.get_full_type(basetype.clone())? );
				}
			}
			syntax_assert!( self.lex.get_token()?, Token::Semicolon );
		}
		
		if peek_token!(self.lex, Token::Ident(ref n) if n == "__attribute__")
		{
			self.parse_gcc_attributes(|self_, name, _opts|
				match &name[..]
				{
				"packed" => { /*TODO: Store packed flag*/; Ok( () ) },
				_ => panic!("{}: TODO - Handle GCC __attribute__(({})) on struct", self_.lex, name),
				}
				)?;
		}
		
		Ok( items )
	}

	fn get_union(&mut self) -> ParseResult<::types::UnionRef>
	{
		let name = self._get_ident_or_blank()?;
		
		// Check for defining the enum's contents
		match self.lex.get_token()?
		{
		Token::BraceOpen => {
			let fields = self.populate_union()?;
			match self.ast.make_union(&name, fields)
			{
			Ok(er) => Ok(er),
			Err( () ) => syntax_error!("Multiple definitions of union '{}'", name),
			}
			},
		tok @ _ => {
			self.lex.put_back(tok);
			if name == "" { syntax_error!("Nameless union with no definition"); }
			Ok( self.ast.get_union(&name) )
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
			let basetype = self.get_base_type()?;
			debug!("populate_union: basetype={:?}", basetype);
			// 2. Get extended type and identifier
			items.push( self.get_full_type(basetype.clone())? );
			
			while peek_token!(self.lex, Token::Comma)
			{
				items.push( self.get_full_type(basetype.clone())? );
			}
			syntax_assert!( self.lex.get_token()?, Token::Semicolon );
		}
		
		Ok( items )
	}
	
	// ---
	// enums
	// ---
	fn get_enum(&mut self) -> ParseResult<::types::EnumRef>
	{
		let name = self._get_ident_or_blank()?;
		
		// Check for defining the enum's contents
		match self.lex.get_token()?
		{
		Token::BraceOpen => {
			let fields = self.populate_enum()?;
			match self.ast.make_enum(&name, fields)
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
			if name == "" { syntax_error!("Nameless enum with no definition"); }
			Ok( self.ast.get_enum(&name) )
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
			let name = syntax_assert!( self.lex.get_token()?, Token::Ident(v) => v );
			
			if peek_token!(self.lex, Token::Assign) {
				// This can be a constant expression
				let node = self.parse_expr()?;
				let val = match node.literal_integer()
					{
					Some(v) => v as u64,
					None => syntax_error!("Non-literal used to set enum value"),
					};
				curval = val;
			}
			items.push( (curval, name) );
			match self.lex.get_token()?
			{
			Token::Comma => {
				curval += 1;
				continue
				},
			Token::BraceClose => break,
			t @ _ => syntax_error!("Unexpected token {:?}, expected Token::Comma or TokBraceClose", t),
			}
		}
	
		Ok( items )
	}
}
