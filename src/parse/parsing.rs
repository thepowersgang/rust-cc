/*
 * Top-level parser
 */
use parse::Token;
use parse::ParseResult;

/// Parse a file into the passed AST program representation
pub fn parse(ast: &mut ::ast::Program, filename: &::std::path::Path, include_paths: Vec<::std::path::PathBuf>, defines: &[String]) -> ParseResult<()>
{
	let pp_opts = {
		let mut pp_opts = super::preproc::Options::default();
		pp_opts.include_paths = include_paths;
		pp_opts
		};

	let mut self_ = super::ParseState {
		ast: ast,
		lex: ::parse::preproc::Preproc::new( Some(filename), pp_opts )?,
		};
	
	for d in defines
	{
		self_.lex.parse_define_str(d);
	}
	
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
impl<'ast> super::ParseState<'ast>
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
		// - if the ident is non-empty, parse the variable definition.
		if ident != ""
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
	
	
	fn parse_function(&mut self, typeid: ::types::TypeRef, ident: String) -> ParseResult<()>
	{
		let code = try!(self.parse_block());
		debug!("parse_function: code = {:?}", code);
		self.ast.define_function(typeid, ident, Some(code));
		Ok( () )
	}
	
	fn parse_opt_block(&mut self) -> ParseResult<::ast::Block>
	{
		let exprs = try!(self.parse_block_line());
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
			statements.push( self.parse_block_line()? );
		}
		
		Ok( statements )
	}
	
	fn try_parse_local_var(&mut self) -> ParseResult<Option<Vec<::ast::VariableDefinition>>>
	{
		Ok(match try!(self.get_base_type_opt())
		{
		Some(basetype) => {
			debug!("try_parse_local_var - basetype={:?}", basetype);
			// Definition!
			let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
			let rv = if ident != ""
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
								Some( self.parse_expr()? )
							}
							else {
								None
							};
						let mut rv = vec![ ::ast::VariableDefinition { ty: typeid, name: ident, value: init } ];
						while peek_token!(self.lex, Token::Comma)
						{
							let (typeid, ident) = try!(self.get_full_type(basetype.clone()));
							let init = if peek_token!(self.lex, Token::Assign) {
									Some( self.parse_expr()? )
								}
								else {
									None
								};
							
							rv.push( ::ast::VariableDefinition { ty: typeid, name: ident, value: init } );
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
	fn parse_block_line(&mut self) -> ParseResult<::ast::Statement>
	{
		// Attempt to get a type, returns None if no type was present
		Ok(match try!(self.try_parse_local_var())
		{
		Some(n) => {
			syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
			::ast::Statement::VarDef(n)
			},
		None => match try!(self.lex.get_token())
			{
			Token::Semicolon => ::ast::Statement::Empty,
			Token::BraceOpen => ::ast::Statement::Block( try!(self.parse_block()) ),
			Token::Rword_return => if peek_token!(self.lex, Token::Semicolon) {
					::ast::Statement::Return( None )
				} else {
					let rv = ::ast::Statement::Return( Some(self.parse_expr_list()?) );
					syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
					rv
				},
			Token::Rword_break => {
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Statement::Break
				},
			Token::Rword_continue => {
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Statement::Continue
				},
			Token::Rword_goto => {
				let dest = syntax_assert!(self.lex => Token::Ident(s) @ s);
				syntax_assert!(self.lex => Token::Semicolon);
				::ast::Statement::Goto(dest)
				},
			Token::Rword_while => {
				let cnd = self.parse_expr_list()?;
				let code = self.parse_opt_block()?;
				::ast::Statement::WhileLoop {
					cond: ::ast::ExprOrDef::Expr(cnd),
					body: code,
					}
				},
			Token::Rword_do => {
				let code = self.parse_opt_block()?;
				syntax_assert!(self.lex => Token::Rword_while);
				let cnd = self.parse_expr_list()?;
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
				::ast::Statement::DoWhileLoop {
					body: code,
					cond: cnd,
					}
				},
			Token::Rword_for => try!(self.parse_for_loop()),
			Token::Rword_if => {
				let cnd = self.parse_expr()?;
				let tcode = self.parse_opt_block()?;
				let fcode = if peek_token!(self.lex, Token::Rword_else) {
						Some( self.parse_opt_block()? )
					} else {
						None
					};
				::ast::Statement::IfStatement {
					cond: ::ast::ExprOrDef::Expr(cnd),
					true_arm: tcode,
					else_arm: fcode,
					}
				},
			Token::Rword_switch => try!(self.parse_switch_statement()),
			
			t @ Token::Ident(_) => {
				self.lex.put_back(t);
				let rv = try!(self.parse_expr_list());
				if let ::ast::Node::Identifier(i) = rv
				{
					if peek_token!(self.lex, Token::Colon) {
						::ast::Statement::Label(i)
					}
					else {
						syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
						::ast::Statement::Expr( ::ast::Node::Identifier(i) )
					}
				}
				else {
					syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
					::ast::Statement::Expr(rv)
				}
				},
			
			// Expression
			t @ _ => {
				self.lex.put_back(t);
				let rv = try!(self.parse_expr_list());
				syntax_assert!(try!(self.lex.get_token()), Token::Semicolon);
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
				match try!(self.try_parse_local_var())
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
				Some(try!(self.parse_expr_list()))
			};
		syntax_assert!(self.lex => Token::Semicolon);
		debug!("parse_for_loop: cnd = {:?}", cnd);
		let inc = if peek_token_nc!(self.lex, Token::ParenClose) {
				None
			} else {
				Some(try!(self.parse_expr_list()))
			};
		syntax_assert!(self.lex => Token::ParenClose);
		debug!("parse_for_loop: inc = {:?}", inc);
		let code = try!(self.parse_opt_block());
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
			match try!(self.lex.get_token())
			{
			Token::BraceClose => break,
			Token::Rword_default => {
				code.push( ::ast::Statement::CaseDefault );
				syntax_assert!(self.lex => Token::Colon);
				},
			Token::Rword_case => {
				// TODO: Allow named constants (from #define)?
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

impl<'ast> super::ParseState<'ast>
{
	// ----------------------------------------------------------------
	// Expressions!
	// ----------------------------------------------------------------
	pub fn parse_expr(&mut self) -> ParseResult<::ast::Node>
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
	parse_left_assoc!{self, parse_expr_member, parse_expr_p, rv, {
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
	fn parse_expr_p(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		// - Either a cast, or a grouped expression
		Token::ParenOpen => match try!(self.get_base_type_opt())
			{
			Some(basetype) => {
				let (fulltype, ident) = try!(self.get_full_type(basetype));
				if ident != "" {
					syntax_error!("Unexpected identifier in cast");
				}
				syntax_assert!(self.lex => Token::ParenClose);
				::ast::Node::Cast(fulltype, box try!(self.parse_expr_p()))
				},
			None => {
					let rv = try!(self.parse_expr());
					syntax_assert!(try!(self.lex.get_token()), Token::ParenClose);
					rv
					},
			},
		t @ _ => {
			self.lex.put_back(t);
			try!(self.parse_expr_z())
			}
		})
	}
	/// Expression - Leaf nodes
	fn parse_expr_z(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match try!(self.lex.get_token())
		{
		Token::Ident(id) => ::ast::Node::Identifier(id),
		Token::String(s) => {
			let mut val = s;
			loop
			{
				match try!(self.lex.get_token()) {
				Token::String(s) => { val.push_str(&s); },
				t @ _ => { self.lex.put_back(t); break; }
				}
			}
			::ast::Node::String(val)
			},
		Token::Integer(v,_) => ::ast::Node::Integer(v),
		Token::Float(v,_) => ::ast::Node::Float(v),
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
					let val = if expect_paren { box try!(self.parse_expr_0()) } else { box try!(self.parse_expr_p()) };
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
