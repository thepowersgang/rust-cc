/*
 * Top-level parser
 */
use parse::Token;
use parse::ParseResult;

impl<'ast> super::ParseState<'ast>
{
	pub(super) fn parseroot(&mut self) -> ParseResult<()>
	{
		loop
		{
			let tok = self.lex.get_token()?;
			match tok
			{
			Token::EOF => {
				break;
				},
			Token::Semicolon => {},
			Token::Rword_typedef => {
				let (_sc, basetype) = self.get_base_type()?;
				debug!("do_typedef: basetype={:?}", basetype);
				let (typeid, name) = self.get_full_type(basetype)?;
				self.ast.set_typedef(name, typeid);
				syntax_assert!( self.lex.get_token()?, Token::Semicolon );
				},
			_ => {
				self.lex.put_back(tok);
				self.do_definition()?
				},
			}
			debug!("--- {}", self.lex);
		}
		debug!("=== {}", self.lex);
		
		Ok( () )
	}

	/// Parse the body of a GCC `__attribute__(...)`
	pub(super) fn parse_gcc_attributes(&mut self, mut cb: impl FnMut(&mut Self, String, Vec<Token>)->ParseResult<()>) -> ParseResult<()>
	{
		syntax_assert!( self.lex.get_token()?, Token::ParenOpen );
		let is_double_wrapped = peek_token!(self.lex, Token::ParenOpen);
		loop {
			let name = syntax_assert!( self.lex.get_token()?, Token::Ident(n) => n );
			let opts = if peek_token!(self.lex, Token::ParenOpen) {
					let mut toks = vec![];
					let mut depth = 0;
					loop
					{
						let t = self.lex.get_token()?;
						match t
						{
						Token::ParenOpen => depth += 1,
						Token::ParenClose if depth == 0 => break toks,
						Token::ParenClose => depth -= 1,
						_ => {},
						}
						toks.push(t);
					}
				}
				else {
					vec![]
				};
			cb(self, name, opts)?;
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		if is_double_wrapped {
			syntax_assert!( self.lex.get_token()?, Token::ParenClose );
		}
		syntax_assert!( self.lex.get_token()?, Token::ParenClose );
		Ok( () )
	}

	fn do_definition(&mut self) -> ParseResult<()>
	{
		let span = self.lex.point_span();
		// 1. Get base type
		let (storage_class, basetype) = self.get_base_type()?;
		debug!("do_definition: basetype={:?}", basetype);
		// 2. Get extended type and identifier
		let (typeid, ident) = self.get_full_type(basetype.clone())?;
		// - if the ident is non-empty, parse the variable definition.
		if ident != ""
		{
			// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
			match self.lex.get_token()?
			{
			// NOTE: The following would need changes for C++11 array literals
			Token::BraceOpen => return self.parse_function(span, storage_class, typeid, ident),
			tok @ _ => {
				self.lex.put_back(tok);

				self.parse_variable_def(span, storage_class.clone(), typeid, ident)?;
				return self.parse_variable_list(storage_class, basetype)
				}
			}
		}
		else
		{
			syntax_assert!( self.lex.get_token()?, Token::Semicolon );
			return Ok( () );
		}
	}
}

// ---
// Function bodies
// ---
impl<'ast> super::ParseState<'ast>
{
	fn parse_function(&mut self, span: crate::ast::Span, storage_class: Option<crate::types::StorageClass>, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		// TODO: Store function name for `__func__`
		let code = self.parse_block()?;
		debug!("parse_function: code = {:?}", code);
		self.ast.define_function(span, storage_class, typeid, ident, Some(code));
		Ok( () )
	}
	
	fn parse_opt_block(&mut self) -> ParseResult<::ast::Block>
	{
		let exprs = self.parse_block_line()?;
		if let ::ast::Statement::Block(b) = exprs {
			Ok( b )
		}
		else {
			Ok( vec![ exprs ] )
		}
	}
	
	fn parse_block(&mut self) -> ParseResult<::ast::Block>
	{
		// Opening brace has been eaten
		let mut statements = Vec::new();
		
		while !peek_token!(self.lex, Token::BraceClose)
		{
			let s = self.parse_block_line()?;
			debug!("> {:?}", s);
			statements.push( s );
		}
		
		Ok( statements )
	}
	
	fn try_parse_local_var(&mut self) -> ParseResult<Option<Vec<::ast::VariableDefinition>>>
	{
		let sp = self.lex.point_span();
		Ok(match self.get_base_type_opt()?
		{
		Some((storage_class, basetype)) => {
			debug!("try_parse_local_var - basetype={:?}", basetype);
			// Definition!
			let (typeid, ident) = self.get_full_type(basetype.clone())?;
			let rv = if ident != ""
				{
					// 3. Check for a: Semicolon, Comma, Open Brace, or Assignment
					if peek_token!(self.lex, Token::BraceOpen)
					{
						// NOTE: The following would need changes for C++11 struct initialisation
						self.parse_function(sp, storage_class, typeid, ident)?;
						return Ok(Some(vec![]));
						//parse_todo!("Nested functions");
					}
					else
					{
						let init = self.parse_var_init()?;
						let mut rv = vec![ ::ast::VariableDefinition { span: sp.clone(), ty: typeid, name: ident, index: None, value: init } ];
						while peek_token!(self.lex, Token::Comma)
						{
							let (typeid, ident) = self.get_full_type(basetype.clone())?;
							let init = self.parse_var_init()?;
							
							rv.push( ::ast::VariableDefinition { span: sp.clone(), ty: typeid, name: ident, index: None, value: init } );
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
	fn parse_var_init(&mut self) -> ParseResult<Option<::ast::Initialiser>>
	{
		Ok(if !peek_token!(self.lex, Token::Assign) {
			// No assignment
			None
		} else if !peek_token!(self.lex, Token::BraceOpen) {
			// `=` ...
			Some( ::ast::Initialiser::Value( self.parse_expr()? ) )
		}
		else {
			// `=` `{`
			Some( self.parse_composite_lit()? )
		})
	}
	
	/// Parse a single line in a block
	fn parse_block_line(&mut self) -> ParseResult<::ast::Statement>
	{
		debug!(">>> {}", self.lex);
		// Attempt to get a type, returns None if no type was present
		Ok(match self.try_parse_local_var()?
		{
		Some(n) => {
			if !n.is_empty() {
				syntax_assert!(self.lex.get_token()?, Token::Semicolon);
			}
			::ast::Statement::VarDef(n)
			},
		None => match self.lex.get_token()?
			{
			Token::Semicolon => ::ast::Statement::Empty,
			Token::BraceOpen => ::ast::Statement::Block( self.parse_block()? ),
			Token::Rword_return => if peek_token!(self.lex, Token::Semicolon) {
					::ast::Statement::Return( None )
				} else {
					let rv = ::ast::Statement::Return( Some(self.parse_expr_list()?) );
					syntax_assert!(self.lex.get_token()?, Token::Semicolon);
					rv
				},
			Token::Rword_break => {
				syntax_assert!(self.lex.get_token()?, Token::Semicolon);
				::ast::Statement::Break
				},
			Token::Rword_continue => {
				syntax_assert!(self.lex.get_token()?, Token::Semicolon);
				::ast::Statement::Continue
				},
			Token::Rword_goto => {
				let dest = syntax_assert!(self.lex => Token::Ident(s) @ s);
				syntax_assert!(self.lex => Token::Semicolon);
				::ast::Statement::Goto(dest)
				},
			Token::Rword_while => {
				syntax_assert!(self.lex => Token::ParenOpen);
				let cnd = self.parse_expr_list()?;
				syntax_assert!(self.lex => Token::ParenClose);
				let code = self.parse_opt_block()?;
				::ast::Statement::WhileLoop {
					cond: ::ast::ExprOrDef::Expr(cnd),
					body: code,
					}
				},
			Token::Rword_do => {
				let code = self.parse_opt_block()?;
				syntax_assert!(self.lex => Token::Rword_while);
				syntax_assert!(self.lex => Token::ParenOpen);
				let cnd = self.parse_expr_list()?;
				syntax_assert!(self.lex => Token::ParenClose);
				syntax_assert!(self.lex => Token::Semicolon);
				::ast::Statement::DoWhileLoop {
					body: code,
					cond: cnd,
					}
				},
			Token::Rword_for => self.parse_for_loop()?,
			Token::Rword_if => {
				syntax_assert!(self.lex => Token::ParenOpen);
				let cnd = self.parse_expr()?;
				syntax_assert!(self.lex => Token::ParenClose);
				let tcode = self.parse_opt_block()?;
				let fcode = if peek_token!(self.lex, Token::Rword_else) {
						Some( self.parse_opt_block()? )
					} else {
						None
					};
				debug!("{}IF: {:?} {:?} {:?}", self.lex, cnd, tcode, fcode);
				::ast::Statement::IfStatement {
					cond: ::ast::ExprOrDef::Expr(cnd),
					true_arm: tcode,
					else_arm: fcode,
					}
				},
			Token::Rword_switch => self.parse_switch_statement()?,
			
			t @ Token::Ident(_) => {
				self.lex.put_back(t);
				let sp = self.lex.point_span();
				let rv = self.parse_expr_list()?;
				// If the expression was just an ident, AND it's followed by a colon: It's a label
				if let ::ast::NodeKind::Identifier(i,_) = rv.kind
				{
					if peek_token!(self.lex, Token::Colon) {
						::ast::Statement::Label(i)
					}
					else {
						// Otherwise, check for the semicolon and return a bare ident
						syntax_assert!(self.lex.get_token()?, Token::Semicolon);
						::ast::Statement::Expr( ::ast::Node::new( sp, ::ast::NodeKind::Identifier(i, None) ) )
					}
				}
				else {
					syntax_assert!(self.lex.get_token()?, Token::Semicolon);
					::ast::Statement::Expr(rv)
				}
				},
			
			// Expression
			t @ _ => {
				self.lex.put_back(t);
				let rv = self.parse_expr_list()?;
				syntax_assert!(self.lex.get_token()?, Token::Semicolon);
				::ast::Statement::Expr(rv)
				}
			}, 
		})
	}
	
	/// Parse a for loop
	// ('for' has been eaten)
	fn parse_for_loop(&mut self) -> ParseResult<::ast::Statement>
	{
		syntax_assert!(self.lex => Token::ParenOpen);
		debug!("parse_for_loop");
		let init = if peek_token_nc!(self.lex, Token::Semicolon) {
				None
			} else {
				match self.try_parse_local_var()?
				{
				Some(n) => Some(::ast::ExprOrDef::Definition(n)),
				None => Some(::ast::ExprOrDef::Expr( self.parse_expr_list()? )),
				}
			};
		syntax_assert!(self.lex => Token::Semicolon);
		debug!("parse_for_loop: init = {:?}", init);
		let cnd = if peek_token_nc!(self.lex, Token::Semicolon) {
				None
			} else {
				Some(self.parse_expr_list()?)
			};
		syntax_assert!(self.lex => Token::Semicolon);
		debug!("parse_for_loop: cnd = {:?}", cnd);
		let inc = if peek_token_nc!(self.lex, Token::ParenClose) {
				None
			} else {
				Some(self.parse_expr_list()?)
			};
		syntax_assert!(self.lex => Token::ParenClose);
		debug!("parse_for_loop: inc = {:?}", inc);
		let code = self.parse_opt_block()?;
		Ok( ::ast::Statement::ForLoop {
			init,
			cond: cnd,
			inc,
			body: code,
			} )
	}
	
	fn parse_switch_statement(&mut self) -> ParseResult<::ast::Statement>
	{
		let cnd = self.parse_expr()?;
		let mut code = Vec::new();
		syntax_assert!(self.lex => Token::BraceOpen);
		loop
		{
			match self.lex.get_token()?
			{
			Token::BraceClose => break,
			Token::Rword_default => {
				code.push( ::ast::Statement::CaseDefault );
				syntax_assert!(self.lex => Token::Colon);
				},
			Token::Rword_case => {
				// TODO: Allow named constants (from #define)?
				let first = match self.parse_expr()?.literal_integer()
					{
					Some(i) => i as u64,
					None => syntax_error!("Case value is not literal"),
					};
				if peek_token!(self.lex, Token::Vargs) {
					let last = match self.parse_expr()?.literal_integer()
						{
						Some(i) => i as u64,
						None => syntax_error!("Case value is not literal"),
						};
					code.push( ::ast::Statement::CaseRange(first, last) );
				}
				else {
					code.push( ::ast::Statement::CaseSingle(first) );
				}
				syntax_assert!(self.lex => Token::Colon);
				},
			t @ _ => {
				self.lex.put_back(t);
				code.push( self.parse_block_line()? );
				}
			}
		}
		Ok( ::ast::Statement::Switch(cnd, code) )
	}
	
	fn parse_variable_list(&mut self, storage_class: Option<crate::types::StorageClass>, basetype: ::types::TypeRef) -> ParseResult<()>
	{
		loop
		{
			match self.lex.get_token()?
			{
			Token::Comma => {},
			Token::Semicolon => break,
			t @ _ => {
				error!("{}: Unexpected token {:?}", self.lex, t);
				return Err( ::parse::Error::SyntaxError(format!("syntax error[parse_variable_list]")) );
				}
			}
			
			let span = self.lex.point_span();
			let (typeid, ident) = self.get_full_type(basetype.clone())?;
			self.parse_variable_def(span, storage_class.clone(), typeid, ident)?;

		}
		
		Ok( () )
	}
	
	fn parse_variable_def(&mut self, span: crate::ast::Span, storage_class: Option<crate::types::StorageClass>, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		if peek_token!(self.lex, Token::Ident(ref n) if n == "__attribute__")
		{
			self.parse_gcc_attributes(|self_, name, _args| {
				match &name[..]
				{
				"unused" => Ok( () ),
				"section" => {
					// TODO: Handle `section("FOO")` properly.
					Ok( () )
					},
				_ => panic!("{} TODO: __attribute__({} {:?}) on variable definition (before value)", self_.lex, name, _args),
				}
				})?;
		}

		let init = self.parse_var_init()?;
		match init {
		None => {
			self.ast.define_variable(span, storage_class, typeid, ident, None);
			}
		Some(init) => {
			let mut typeid = typeid;
			// If the type is an array with no size, get the size from the initialiser
			if let ::types::BaseType::Array(ref ty, ::types::ArraySize::None) = typeid.basetype {
				let len = match init
					{
					::ast::Initialiser::ListLiteral(ref elems) => elems.len(),
					::ast::Initialiser::Value(::ast::Node { kind: ::ast::NodeKind::String(ref val), .. }) => val.len() + 1,
					_ => todo!("Get array size from {:?}", init),
					};
				typeid = ::types::Type::new_ref(::types::BaseType::Array(ty.clone(), ::types::ArraySize::Fixed(len as u64)), typeid.qualifiers.clone());
			}
			self.ast.define_variable(span, storage_class, typeid, ident, Some(init));
			}
		}

		//if peek_token!(self.lex, Token::Ident(ref n) if n == "__attribute__")
		//{
		//	self.parse_gcc_attributes(|self_, name, _args| {
		//		match &name[..]
		//		{
		//		_ => panic!("{} TODO: __attribute__({}) on variable definition (after value)", self_.lex, name),
		//		}
		//		})?;
		//}

		Ok( () )
	}
}
	
// ---
// Composite literals
// ---
impl<'ast> super::ParseState<'ast>
{
	fn parse_composite_lit(&mut self) -> ParseResult<::ast::Initialiser>
	{
		Ok(match self.lex.get_token()?
		{
		Token::BraceClose => {
			::ast::Initialiser::ListLiteral(vec![])
			},
		t @ Token::Period => {
			self.lex.put_back(t);
			::ast::Initialiser::StructLiteral( self.parse_struct_lit()? )
			},
		t @ Token::SquareOpen => {
			self.lex.put_back(t);
			::ast::Initialiser::ArrayLiteral( self.parse_array_lit()? )
			},
		t @ _ => {
			self.lex.put_back(t);
			::ast::Initialiser::ListLiteral( self.parse_list_lit()? )
			},
		})
	}
	fn parse_lit_inner(&mut self) -> ParseResult<::ast::Initialiser> {
		if !peek_token!(self.lex, Token::BraceOpen) {
			// `=` ...
			Ok( ::ast::Initialiser::Value( self.parse_expr()? ) )
		}
		else {
			// `=` `{`
			self.parse_composite_lit()
		}
	}
	
	fn parse_list_lit(&mut self) -> ParseResult<Vec<::ast::Initialiser>>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token_nc!(self.lex, Token::BraceClose) {
				break;
			}
			let val = self.parse_lit_inner()?;
			items.push(val);
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);

		
		Ok(items)
	}
	
	fn parse_array_lit(&mut self) -> ParseResult<Vec<(::ast::Node, ::ast::Initialiser)>>
	{
		let mut items = Vec::new();
		loop
		{
			if peek_token_nc!(self.lex, Token::BraceClose) {
				break;
			}
			syntax_assert!(self.lex => Token::SquareOpen);
			let idx_expr = self.parse_expr()?;
			syntax_assert!(self.lex => Token::SquareClose);
			syntax_assert!(self.lex => Token::Assign);
			let val = self.parse_lit_inner()?;
			
			items.push( (idx_expr,val) );
			
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);
		
		Ok(items)
	}
	
	fn parse_struct_lit(&mut self) -> ParseResult<Vec<(String,::ast::Initialiser)>>
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
			let val = self.parse_lit_inner()?;
			
			items.push( (name,val) );
			
			if ! peek_token!(self.lex, Token::Comma) {
				break;
			}
		}
		syntax_assert!(self.lex => Token::BraceClose);
		
		Ok(items)
	}
}

// vim: ft=rust ts=4 sw=4
